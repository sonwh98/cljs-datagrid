(ns com.kaicode.datagrid
  (:require [com.kaicode.tily :as tily]
            [com.kaicode.teleport :as t]
            [goog.dom :as gdom]
            [reagent.core :as r]
            [cljsjs.hammer]))

(defonce left-corner-block-width 60)
(defonce common-column-style {:display :table-cell
                              :padding 0
                              :border  "1px solid #d9d9d9"})
(defn get-column-config [grid-state column-kw]
  (->> @grid-state :columns-config (filter #(= (first %) column-kw)) first second))

(defn get-window-dimension [grid-state]
  (-> @grid-state :window-dimension))

(defn get-content-height [grid-state]
  (let [padding-bottom 20
        header-button-height 40
        title-bar-height 60
        search-box-height 67]
    (-> grid-state get-window-dimension :height (- title-bar-height search-box-height padding-bottom header-button-height))))

(defn get-invisible-columns [grid-state]
  (->> @grid-state :columns-config
       (filter #(false? (-> % second :visible?)))))

(defn get-visible-columns [grid-state]
  (->> @grid-state :columns-config
       (filter #(true? (-> % second :visible?)))))

(defn get-content-width [grid-state]
  (-> grid-state get-window-dimension :width))

(defn get-width-for-columns [grid-state]
  (- (get-content-width grid-state) left-corner-block-width 
     (-> grid-state get-visible-columns count (+ 5))))

(defn extra-width-per-visible-column [grid-state]
  (let [width-for-columns        (get-width-for-columns grid-state)
        invisible-columns-config (get-invisible-columns grid-state)
        extra-width              (->> invisible-columns-config
                                      (map #(-> % second :width-weight (* width-for-columns)))
                                      (reduce +))]
    (-> extra-width
        (/ (-> grid-state get-visible-columns count))
        js/Math.floor)))

(defn get-column-width [column-kw grid-state]
  (let [width-for-columns (get-width-for-columns grid-state)
        width-weight        (:width-weight (get-column-config grid-state column-kw))
        width               (+ (* width-weight width-for-columns)
                               (extra-width-per-visible-column grid-state))]
    (js/Math.floor width)))

(defn get-default-column-style [column-kw spreadsheet-state]
  (let [column-width (get-column-width column-kw spreadsheet-state)
        style (merge {:width          column-width
                      :min-width      column-width
                      :max-width      column-width
                      :text-align     :center
                      :vertical-align :middle}
                     common-column-style)]
    style))

(defn- mark-column-as-sticky [grid-state column-kw]
  (swap! grid-state update :sticky-columns conj column-kw)
  (sticky-columns-refresh grid-state this-component))

(defn- mark-column-as-non-sticky [grid-state column-kw]
  (swap! grid-state update :sticky-columns disj column-kw)
  (sticky-columns-refresh grid-state this-component) )

(defn- get-sticky-columns [grid-state]
  (:sticky-columns @grid-state))

(defn- sticky-column? [grid-state column-kw]
  (tily/is-contained? column-kw :in (:sticky-columns @grid-state)))

(defn- get-left-column-kws
  "Returns a set of column keywords that are on the left side of column-kw"
  [grid-state column-kw]
  (set (take-while #(not= column-kw %) (map first (:columns-config @grid-state)))))

(defn- get-total-columns-width
  "Calculates the total width of specified columns"
  [grid-state column-kws]
  (apply + (map #(get-column-width % grid-state) column-kws)))

(defn- can-mark-column-as-sticky? [grid-state column-kw]
  (and (not (sticky-column? grid-state column-kw))
       (every? (partial sticky-column? grid-state)
               (get-left-column-kws grid-state column-kw))))

(defn- can-mark-column-as-non-sticky? [grid-state column-kw]
  (and (sticky-column? grid-state column-kw)
       (= (get-left-column-kws grid-state column-kw)
          (disj (get-sticky-columns grid-state) column-kw))))

(defn- get-non-sticky-columns [grid-state]
  (drop-while (partial sticky-column? grid-state)
              (map first (:columns-config @grid-state))))

(defn assoc-in-columns-config [grid-state column-kw ks v]
  (swap! grid-state assoc :columns-config
         (->> (:columns-config @grid-state)
              (map (fn [[ckw _ :as column]]
                     (if (= ckw column-kw)
                       (assoc-in column (concat [1] ks) v)
                       column)))
              vec)))

(defn get-in-columns-config [grid-state column-kw ks]
  (some (fn [[ckw cconfig]]
          (when (= ckw column-kw)
            (get-in cconfig ks)))
        (:columns-config @grid-state)))

(defn get-column-idx [grid-state column-kw]
  (first
    (keep-indexed (fn [i [ckw _]]
                    (when (= ckw column-kw)
                      i))
                    (:columns-config @grid-state))))

(defn calculate-column-header-z-index [grid-state column-kw]
  (- (if (sticky-column? grid-state column-kw)
       9900
       5500)
     (get-column-idx grid-state column-kw)))

(defn calculate-record-z-index [grid-state column-kw]
  (- (if (sticky-column? grid-state column-kw)
       9800
       5400)
     (get-column-idx grid-state column-kw)))

(defn- column-header-style [grid-state column-kw column-config]
  (let [column-width (get-column-width column-kw grid-state)]
    (merge
      common-column-style
      {:display       :table-cell
       :overflow      :hidden
       :white-space   :nowrap
       :text-overflow :ellipsis
       :width         column-width
       :min-width     column-width
       :max-width     column-width
       :z-index       (calculate-column-header-z-index grid-state column-kw)
       :position      :relative}
      (when (sticky-column? grid-state column-kw)
        {:background-color :gray})
      (when (not= 0 (:left-margin column-config))
        {:left (:left-margin column-config)}))))

(defn- record-style [grid-state column-kw column-config]
  (let [column-width (get-column-width column-kw grid-state)]
    (merge
      common-column-style
      {:display          :table-cell
       :overflow         :hidden
       :white-space      :nowrap
       :text-overflow    :ellipsis
       :width            column-width
       :min-width        column-width
       :max-width        column-width
       :position         :relative
       :background-color "#fff"
       :z-index          (calculate-record-z-index grid-state column-kw)}
      (when (sticky-column? grid-state column-kw)
        {:background-color "#f6f6f6"})
      (when (not= 0 (:left-margin column-config))
        {:left (:left-margin column-config)}))))

(defn- calc [n s k]
  (apply conj (repeat (- n s) (- k)) (repeat s 0)))

(defn calculate-left-margins [grid-state scroll-left]
  (calc (count (remove (partial sticky-column? grid-state)
                (get-visible-columns grid-state)))
         (count (get-sticky-columns grid-state))
         scroll-left))

(defn update-left-margins [grid-state scroll-left]
  (swap! grid-state assoc :columns-config
         (vec
           (map (fn [[column-kw column-config :as column] left-margin]
                  (assoc-in column [1 :left-margin] left-margin))
                (:columns-config @grid-state)
                (calculate-left-margins grid-state scroll-left)))))

(defn- sticky-column-headers-foundation
  "Creates a div that is placed underneath sticky column headers"
  [grid-state]
  [:div {:style {:position         :fixed
                 :top              0
                 :left             left-corner-block-width
                 :background-color "#fff"
                 :width            (+ (get-total-columns-width
                                       grid-state
                                       (filter (partial sticky-column? grid-state)
                                               (map first (get-visible-columns grid-state)))))
                 :height           40}}])

(defn- number-buttons-foundation
  "Creates a div that is placed underneath number buttons"
  [grid-state]
  [:div {:style {:position         :fixed
                 :top              0
                 :left             0
                 :z-index          7000
                 :background-color "#fff"
                 :width            left-corner-block-width
                 :height           "100%"}}])

(defn- data-column-headers [grid-state]
  (doall (for [column-config (-> @grid-state :columns-config)
               :let [[column-kw config] column-config
                     column-width   (get-column-width column-kw grid-state)
                     header-txt     (-> config :render-header-fn (apply nil))
                     sort-indicator (let [sort-column (-> @grid-state :sort-column)
                                          column      (-> sort-column keys first)
                                          descending? (column-kw sort-column)]
                                      (when (= column-kw column)
                                        (if descending?
                                          [:i {:class "material-icons"} "arrow_drop_down"]
                                          [:i {:class "material-icons"} "arrow_drop_up"])))
                     sort-column    (fn [evt]
                                      (let [sort-column (:sort-column @grid-state)
                                            rows        (:rows @grid-state)]
                                        (swap! grid-state (fn [grid-state]
                                                            (let [comparator (cond
                                                                               (nil? sort-column) <
                                                                               :else (if (column-kw sort-column)
                                                                                       >
                                                                                       <))]
                                                              (-> grid-state
                                                                  (assoc :selected-rows #{})
                                                                  (update-in [:sort-column] (fn [sc]
                                                                                              (let [val (column-kw sc)]
                                                                                                {column-kw (not val)})))
                                                                  (assoc :rows (vec (sort-by
                                                                                     #(-> % column-kw
                                                                                          (or "") str
                                                                                          clojure.string/lower-case)
                                                                                     comparator
                                                                                     rows)))))))))]
               :when (:visible? config)]
           [:div {:key      (tily/format "grid-%s-%s-header" (:id @grid-state) column-kw)
                  :class    "table-header-col mdl-button mdl-js-button mdl-js-button mdl-button--raised"
                  :style    (column-header-style grid-state column-kw config)
                  :on-click sort-column
                  :on-context-menu (fn [evt]
                                     (let [rect   (.. evt -target -parentNode -parentNode -parentNode -parentNode getBoundingClientRect)
                                           x      (- (. evt -clientX) 10)
                                           y      (+ (. evt -clientY) 5)
                                           x      (- x (. rect -left))
                                           y      (- y (. rect -top))
                                           mark-as-sticky     [:a {:href     "#"
                                                                   :on-click (fn [_] (mark-column-as-sticky grid-state column-kw))}
                                                               "sticky"]
                                           mark-as-non-sticky [:a {:href     "#"
                                                                   :on-click (fn [_] (mark-column-as-non-sticky grid-state column-kw))}
                                                               "non-sticky"]]
                                       (tily/set-atom! grid-state [:context-menu :content]
                                         (cond
                                           (can-mark-column-as-sticky? grid-state column-kw)
                                           mark-as-sticky
                                           
                                           (can-mark-column-as-non-sticky? grid-state column-kw)
                                           mark-as-non-sticky))
                                       (tily/set-atom! grid-state [:context-menu :coordinate] [x y]))
                                     (. evt preventDefault))}
               header-txt
               sort-indicator])))

(defn- column-headers [grid-state]
  (let [left-corner-block-style {:display   :table-cell
                                 :width     left-corner-block-width
                                 :min-width left-corner-block-width
                                 :max-width left-corner-block-width
                                 :position  :relative
                                 :z-index   9999
                                 :padding   0}
        left-corner-block       (or (:left-corner-block @grid-state)
                                    (fn [grid-state style]
                                      [:div {:class "mdl-button mdl-js-button mdl-button--raised"
                                             :style style}]))]
    [:div#table-header {:style {:display :table-row}}
     (left-corner-block grid-state left-corner-block-style)
     (data-column-headers grid-state)]))

(defn- default-column-render [column-kw row grid-state state]
  (let [id           (-> @grid-state :id)
        column-width (get-column-width column-kw grid-state)
        style        (merge {:width     column-width
                             :min-width column-width
                             :max-width column-width}
                            common-column-style
                            state)
        value        (str (column-kw @row))
        unique       (-> (get-column-config grid-state column-kw) :unique)
        local-save (fn [evt]
                     (let [div     (. evt -target)
                           content (. div -textContent)
                           local-save-fn (:local-save-fn (get-column-config grid-state column-kw))]
                       (when-not (nil? local-save-fn)
                         (local-save-fn content row column-kw))))
        remote-save (fn [evt]
                      (let [div     (. evt -target)
                            content (. div -textContent)
                            remote-save-fn (:remote-save-fn (get-column-config grid-state column-kw))]
                        (when remote-save-fn
                          (remote-save-fn content row column-kw))))
        property     {:key                               (tily/format "grid-%s-default-column-render-%s" id column-kw)
                      :content-editable                  (let [col-config (get-column-config grid-state column-kw)]
                                                           (if (contains? col-config :editable)
                                                             (:editable col-config)
                                                             true))
                      :suppress-content-editable-warning true
                      :style                             style
                      :on-blur (fn [evt]
                                 (local-save evt)
                                 (remote-save evt))}
        property     (if unique
                       property
                       (assoc property :on-input remote-save))]
    [:div property
     value]))

(defn- number-button [i grid-state]
  (let [selected-rows   (r/cursor grid-state [:selected-rows])
        expanded-rows   (r/cursor grid-state [:expanded-rows])
        hovered-nb-row  (r/cursor grid-state [:hovered-number-button-row])
        select-row      #(swap! selected-rows conj i)
        unselect-row    (fn [] (swap! selected-rows (fn [selected-rows]
                                                      (set (filter #(not= i %) selected-rows)))))
        expand-row      #(swap! expanded-rows conj i)
        collapse-row    #(swap! expanded-rows disj i)
        row (get-in @grid-state [:rows i])
        hoverable?      (some? (:on-expand row))
        hover-style     (when (= i @hovered-nb-row)
                          {:background-color "#d9d9d9"})
        hover-indicator (fn []
                          (when (= i @hovered-nb-row)

                            [:i {:class (str "number-button-indicator material-icons"
                                             (when hoverable? " number-button-indicator-hoverable"))
                                 :style {:margin-left    5
                                         :margin-right -10}
                                 :on-click (fn [evt]
                                             (if (tily/is-contained? i :in @expanded-rows)
                                               (collapse-row)
                                               (expand-row))
                                             (.. evt stopPropagation)
                                             (.. evt -nativeEvent stopImmediatePropagation))}
                             (if (tily/is-contained? i :in @expanded-rows)
                               "arrow_drop_up"
                               "arrow_drop_down")]))]
    (r/create-class {:component-did-mount (fn [this-component]
                                            (let [this-element (r/dom-node this-component)
                                                  mc (js/Hammer. this-element)]
                                              (.. mc (on "press" (fn [evt]
                                                                   (let [rect (.. evt -target -parentNode -parentNode -parentNode -parentNode getBoundingClientRect)
                                                                         pointer (-> evt .-pointers tily/to-seq first)
                                                                         x      (+ (. pointer -clientX) 20)
                                                                         y      (+ (. pointer -clientY) 20)
                                                                         y      (- y (. rect -top))]
                                                                     (select-row)
                                                                     (if (:on-delete-rows @grid-state)
                                                                       (let [delete [:a {:href     "#"
                                                                                         :on-click (fn [evt]
                                                                                                     (let [current-rows   (:rows @grid-state)
                                                                                                           index-row      (tily/with-index current-rows)
                                                                                                           new-rows       (->> index-row
                                                                                                                               (remove (fn [[index row]]
                                                                                                                                         (tily/is-contained? index :in @selected-rows)))
                                                                                                                               (map #(second %))
                                                                                                                               vec)
                                                                                                           rows-to-delete (->> index-row
                                                                                                                               (filter (fn [[index row]]
                                                                                                                                         (tily/is-contained? index :in @selected-rows)))
                                                                                                                               (map #(-> % second)))
                                                                                                           on-delete-rows (or (:on-delete-rows @grid-state)
                                                                                                                              constantly)]
                                                                                                       (on-delete-rows rows-to-delete)

                                                                                                       (swap! expanded-rows clojure.set/difference @selected-rows)
                                                                                                       (reset! selected-rows #{})
                                                                                                       (tily/set-atom! grid-state [:rows] new-rows)))} "Delete"]]
                                                                         (swap! grid-state  (fn [grid-state]
                                                                                              (-> grid-state
                                                                                                  (assoc-in [:context-menu :content] delete)
                                                                                                  (assoc-in [:context-menu :coordinate] [x y]))))))))))))
                     :reagent-render (fn [i grid-state]
                                       [:div {:id i
                                              :draggable       true
                                              :class "mdl-button mdl-js-button mdl-js-button mdl-button--raised"

                                              :style           (merge
                                                                 {:display   :table-cell
                                                                  :width     left-corner-block-width
                                                                  :min-width left-corner-block-width
                                                                  :max-width left-corner-block-width
                                                                  :z-index   999999
                                                                  :position :relative
                                                                  :padding   0
                                                                  :user-drag :element}
                                                                 ; why is it necessary?
                                                                 (when hoverable?
                                                                   hover-style))
                                              :on-click        #(if (tily/is-contained? i :in @selected-rows)
                                                                  (unselect-row)
                                                                  (select-row))
                                              :on-mouse-enter  (fn [_] (when hoverable? (reset! hovered-nb-row i)))
                                              :on-mouse-leave  (fn [_] (when hoverable? (reset! hovered-nb-row nil)))
                                              :on-drag-start   (let [on-row-drag-start (:on-row-drag-start @grid-state)]
                                                                 (when on-row-drag-start
                                                                   #(on-row-drag-start i grid-state)))
                                              :on-drag-end (let [on-row-drag-end (:on-row-drag-end @grid-state)]
                                                             (when on-row-drag-end
                                                               #(on-row-drag-end i grid-state)))
                                              :on-context-menu (fn [evt]
                                                                 (let [rect   (.. evt -target -parentNode -parentNode -parentNode -parentNode getBoundingClientRect)
                                                                       x      (- (. evt -clientX) 10)
                                                                       y      (+ (. evt -clientY) 5)
                                                                       x      (- x (. rect -left))
                                                                       y      (- y (. rect -top))]
                                                                   (select-row)
                                                                   (if (:on-delete-rows @grid-state)
                                                                     (let [delete [:a {:href     "#"
                                                                                       :on-click (fn [evt]
                                                                                                   (let [current-rows   (:rows @grid-state)
                                                                                                         index-row      (tily/with-index current-rows)
                                                                                                         new-rows       (->> index-row
                                                                                                                             (remove (fn [[index row]]
                                                                                                                                       (tily/is-contained? index :in @selected-rows)))
                                                                                                                             (map #(second %))
                                                                                                                             vec)
                                                                                                         rows-to-delete (->> index-row
                                                                                                                             (filter (fn [[index row]]
                                                                                                                                       (tily/is-contained? index :in @selected-rows)))
                                                                                                                             (map #(-> % second)))
                                                                                                         on-delete-rows (or (:on-delete-rows @grid-state)
                                                                                                                            constantly)]
                                                                                                     (on-delete-rows rows-to-delete)

                                                                                                     (swap! expanded-rows clojure.set/difference @selected-rows)
                                                                                                     (reset! selected-rows #{})
                                                                                                     (tily/set-atom! grid-state [:rows] new-rows)))} "Delete"]]
                                                                       (tily/set-atom! grid-state [:context-menu :content] delete)
                                                                       (tily/set-atom! grid-state [:context-menu :coordinate] [x y])))

                                                                   (. evt preventDefault)))}
                                        [:div
                                         (inc i)
                                         (when hoverable?
                                           [hover-indicator])]])})))

(defn- rows [grid-state]
  (let [id              (-> @grid-state :id)
        total-width     (get-content-width grid-state)
        total-height    (get-content-height grid-state)
        columns-config  (-> @grid-state :columns-config)
        selected-rows   (r/cursor grid-state [:selected-rows])
        expanded-rows   (r/cursor grid-state [:expanded-rows])
        row-data        (fn [row]
                          (doall (for [[column-kw config] columns-config
                                       :when (:visible? config)
                                       :let [render-column-fn (:render-column-fn config)
                                             k                (tily/format "grid-%s-%s-%s" id (:system/id @row) column-kw)]]
                                   (if render-column-fn
                                     ^{:key k} [render-column-fn column-kw row grid-state (record-style grid-state column-kw config)]
                                     ^{:key k} [default-column-render column-kw row grid-state (record-style grid-state column-kw config)]))))
        row-div         (fn [i row]
                          (let [style {:display :table-row}
                                style (if (tily/is-contained? i :in @selected-rows)
                                        (assoc style :background-color "#e6faff")
                                        style)]
                            [:div {:style style}
                             [number-button i grid-state]
                             (row-data row)]))]
    [:div {:id    (tily/format "grid-%s-rows" id)
           :class "grid-rows"
           :style {:display    :block
                   :height     total-height
                   :width      (- total-width (:scroll-left @grid-state))
                   :overflow-y :auto
                   :overflow-x :hidden}}
     (doall (for [i (range (-> @grid-state :rows count))
                  :let [row (r/cursor grid-state [:rows i])
                        on-expand (:on-expand @row)
                        k   (tily/format "grid-%s-%s-extra" id i)]]
              ^{:key k} [:div
                         (row-div i row) ;;NOTE: this must be a function call and not a reagent component otherwise it would reactive which causes UI quirks
                         (when on-expand 
                           [:div {:style (when-not (tily/is-contained? i :in @expanded-rows)
                                           {:display :none})} 
                            (on-expand row)])
                         ]))]))

(defn- context-menu [grid-state]
  (let [content    (-> @grid-state :context-menu :content)
        coordinate (-> @grid-state :context-menu :coordinate)]
    (when content
      [:div {:id    "context-menu"
             :style {:background-color :white
                     :border           "1px solid grey"
                     :padding          5
                     :position         :absolute
                     :z-index          10000
                     :left             (first coordinate)
                     :top              (second coordinate)}}
       content])))

(defn cog [grid-state]
  (let [id (:id @grid-state)
        setting-visible? (r/atom false)]
    (fn [grid-state]
      (let [columns-config (-> @grid-state :columns-config)
            title-bar-height 60
            search-box-height 77
            header-height 40
            cog-top (+ 48 35)
            drop-down-top (+ cog-top 15)]
        [:div
         [:i {:class "material-icons"
              :style {:position :absolute
                      :right 2
                      :top cog-top}
              :on-click #(swap! setting-visible? (fn [old-val] (not old-val)))} "settings"]
         (when @setting-visible?
           [:div {:style {:position :absolute
                          :z-index 11000
                          :top drop-down-top
                          :right 0
                          :padding 0
                          :margin 0
                          :opacity 1.0
                          :border-left "1px solid grey"
                          :background-color :white}
                  :on-mouse-leave #(reset! setting-visible? false)}
            [:ul {:style {:padding-left 5
                          :padding-right 5
                          :padding-top 0
                          :padding-bottom  0
                          :margin 0
                          :list-style-type :none}}
             (for [[i [column-kw config]] (tily/with-index columns-config)
                   :let [k (tily/format "spreadsheet-%s-cog-%s" id column-kw)
                         visible? (:visible? config)
                         ch (-> config :render-header-fn (apply nil))]]
               [:li {:key k}
                [:label {:class "mdl-checkbox mdl-js-checkbox mdl-js-ripple-effect" :for k}
                 [:input {:type "checkbox" :id k :class "mdl-checkbox__input" :defaultChecked visible?
                          :on-change #(swap! grid-state update-in [:columns-config i 1 :visible?] not)}]
                 [:span {:class "mdl-checkbox__label"} ch]]])]])]))))

(defn sticky-columns-refresh [grid-state this-component]
  (let [element (r/dom-node this-component)
        scroll-left (.-x (gdom/getDocumentScroll))]
    (tily/set-atom! grid-state [:scroll-left] scroll-left)
    (update-left-margins grid-state scroll-left)))


(defn render [grid-state]
  (r/create-class {:component-will-mount   (fn [this-component]
                                             (.addEventListener js/window "scroll" (fn [_]
                                                                                     (sticky-columns-refresh grid-state this-component)))
                                             (tily/set-atom! grid-state [:selected-rows] #{})
                                             (tily/set-atom! grid-state [:expanded-rows] #{})
                                             (tily/set-atom! grid-state [:sticky-columns] #{})
                                             (tily/set-atom! grid-state [:id] (str (rand-int 1000))))
                   :reagent-render         (fn [grid-state]
                                             [:div {:style {:width (get-content-width grid-state)}}
                                              [:div#datagrid-table {:style    {:margin-left (:scroll-left @grid-state)}
                                                                    :on-click #(when (-> @grid-state :context-menu :content)
                                                                                 (tily/set-atom! grid-state [:context-menu :content] nil))}
                                               ;[sticky-column-headers-foundation grid-state]
                                               [context-menu grid-state]
                                               [column-headers grid-state]
                                               [number-buttons-foundation grid-state]
                                               [rows grid-state]]])}))
