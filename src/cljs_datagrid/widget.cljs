(ns cljs-datagrid.widget
  (:require [com.kaicode.tily :as tily]
            [com.kaicode.teleport :as t]
            [com.kaicode.mercury :as m]
            [reagent.core :as r]
            [domina :as domina]))

(defonce title-bar-height 60)
(defonce plus-button-width 40)
(defonce plus-button-height 40)
(defonce cog-button-wdith 15)
(defonce search-box-height 67)

(defonce common-column-style {:display :table-cell
                              :padding 0
                              :border  "1px solid #d9d9d9"})

(defn get-column-config [spreadsheet-state column-kw]
  (->> @spreadsheet-state :columns-config (filter #(= (first %) column-kw)) first second))

(defn get-window-dimension [spreadsheet-state]
  (-> @spreadsheet-state :window-dimension))

(defn get-content-height [spreadsheet-state]
  (let [padding-bottom 35
        fudge-factor   30]
    (-> spreadsheet-state get-window-dimension :height (- title-bar-height padding-bottom search-box-height fudge-factor))))

(defn get-invisible-columns [spreadsheet-state]
  (->> @spreadsheet-state :columns-config
       (filter #(false? (-> % second :visible)))))

(defn get-visible-columns [spreadsheet-state]
  (->> @spreadsheet-state :columns-config
       (filter #(true? (-> % second :visible)))))

(defn get-content-width [spreadsheet-state]
  (-> spreadsheet-state get-window-dimension :width (- 6
                                                       (-> spreadsheet-state get-visible-columns count))))
(defn extra-width-per-visible-column [spreadsheet-state]
  (let [total-content-width      (- (get-content-width spreadsheet-state) plus-button-width cog-button-wdith)
        invisible-columns-config (get-invisible-columns spreadsheet-state)
        extra-width              (->> invisible-columns-config
                                      (map #(-> % second :width-weight (* total-content-width)))
                                      (reduce +))]
    (-> extra-width
        (/ (-> spreadsheet-state get-visible-columns count))
        js/Math.floor)))

(defn get-column-width [column-kw spreadsheet-state]
  (let [total-content-width (- (get-content-width spreadsheet-state) plus-button-width cog-button-wdith)
        width-weight        (:width-weight (get-column-config spreadsheet-state column-kw))
        width               (+ (* width-weight total-content-width)
                               (extra-width-per-visible-column spreadsheet-state))]
    (js/Math.floor width)))

(defn get-default-column-style [column-kw spreadsheet-state]
  (let [column-width (get-column-width column-kw spreadsheet-state)
        style        (merge {:width          column-width
                             :min-width      column-width
                             :max-width      column-width
                             :text-align     :center
                             :vertical-align :middle}
                            common-column-style)]
    style))


(defn- cog [spreadsheet-state]
  (let [id               (:id @spreadsheet-state)
        setting-visible? (r/atom false)]
    (fn [spreadsheet-state]
      (let [columns-config (-> @spreadsheet-state :columns-config)]
        [:div
         [:i {:class    "material-icons"
              :style    {:position :absolute
                         :right    0}
              :on-click #(swap! setting-visible? (fn [old-val] (not old-val)))} "settings"]
         (when @setting-visible?
           [:div {:style          {:position         :absolute
                                   :z-index          1
                                   :top              15
                                   :right            0
                                   :padding          0
                                   :margin           0
                                   :opacity          1.0
                                   :border-left      "1px solid grey"
                                   :background-color :white}
                  :on-mouse-leave #(reset! setting-visible? false)}
            [:ul {:style {:padding-left    5
                          :padding-right   5
                          :padding-top     0
                          :padding-bottom  0
                          :margin          0
                          :list-style-type :none}}
             (for [[i [column-kw config]] (tily/with-index columns-config)
                   :let [k        (tily/format "spreadsheet-%s-cog-%s" id column-kw)
                         visible? (:visible config)
                         ch       (-> config :render-header-fn (apply nil))]]
               [:li {:key k}
                [:label {:class "mdl-checkbox mdl-js-checkbox mdl-js-ripple-effect" :for k}
                 [:input {:type      "checkbox" :id k :class "mdl-checkbox__input" :defaultChecked visible?
                          :on-change #(swap! spreadsheet-state update-in [:columns-config i 1 :visible] not)}]
                 [:span {:class "mdl-checkbox__label"} ch]]])]])]))))

(defn- scroll-to-bottom [element]
  ;;postpone execution 16ms to give enough time for ui to render after a state change otherwise the new UI element will not appear
  (m/postpone #(set! element -scrollTop (. element -scrollHeight))
              16))

(defn- plus-button [spreadsheet-state]
  [:div {:class "mdl-button mdl-js-button mdl-button--fab mdl-button--mini-fab mdl-button--colored"}
   [:i {:class    "material-icons"
        :on-click #(let [id       (:id @spreadsheet-state)
                         div-rows (domina/by-id (tily/format "spreadsheet-%s-rows" id))]
                    (swap! spreadsheet-state update-in [:rows] conj {})
                    (scroll-to-bottom div-rows))} "add"]])

(defn- data-column-headers [spreadsheet-state]
  (doall (for [column-config (-> @spreadsheet-state :columns-config)
               :let [[column-kw config] column-config
                     column-width   (get-column-width column-kw spreadsheet-state)
                     header-txt     (-> config :render-header-fn (apply nil))
                     sort-indicator (let [sort-column (-> @spreadsheet-state :sort-column)
                                          column      (-> sort-column keys first)
                                          descending? (column-kw sort-column)]
                                      (when (= column-kw column)
                                        (if descending?
                                          [:i {:class "material-icons"} "arrow_drop_down"]
                                          [:i {:class "material-icons"} "arrow_drop_up"])))
                     sort-column    (fn [evt]
                                      (let [sort-column (:sort-column @spreadsheet-state)
                                            rows        (:rows @spreadsheet-state)]
                                        (swap! spreadsheet-state (fn [spreadsheet-state]
                                                                   (let [comparator (cond
                                                                                      (nil? sort-column) <
                                                                                      :else (if (column-kw sort-column)
                                                                                              >
                                                                                              <))]
                                                                     (-> spreadsheet-state
                                                                         (update-in [:selected-rows] (constantly #{}))
                                                                         (update-in [:sort-column] (fn [sc]
                                                                                                     (let [val (column-kw sc)]
                                                                                                       {column-kw (not val)})))
                                                                         (update-in [:rows] (constantly
                                                                                              (vec (sort-by
                                                                                                     #(-> % column-kw
                                                                                                          (or "") str
                                                                                                          clojure.string/lower-case)
                                                                                                     comparator
                                                                                                     rows))))))))))]
               :when (:visible config)]
           [:div {:key      (tily/format "spreadsheet-%s-%s" (:id @spreadsheet-state) column-kw)
                  :class    "mdl-button mdl-js-button mdl-js-button mdl-button--raised"
                  :style    (merge {:display   :table-cell
                                    :width     column-width
                                    :min-width column-width
                                    :max-width column-width}
                                   common-column-style)
                  :on-click sort-column}
            header-txt
            sort-indicator])))

(defn- column-headers [spreadsheet-state]
  [:div {:style {:display :table-row}}
   (plus-button spreadsheet-state)
   (data-column-headers spreadsheet-state)
   [cog spreadsheet-state]])

(defn- default-column-render [column-kw row spreadsheet-state]
  (let [id           (-> @spreadsheet-state :id)
        column-width (get-column-width column-kw spreadsheet-state)
        style        (merge {:width     column-width
                             :min-width column-width
                             :max-width column-width}
                            common-column-style)
        value        (str (column-kw @row))
        unique       (-> (get-column-config spreadsheet-state column-kw) :unique)
        property     {:key                               (tily/format "spreadsheet-%s-default-column-render-%s" id column-kw)
                      :content-editable                  true
                      :suppress-content-editable-warning true
                      :style                             style}
        save-fn      (or (:save-fn (get-column-config spreadsheet-state column-kw))
                         #(swap! row update-in [column-kw] (constantly %)))
        save         (fn [evt]
                       (let [div     (. evt -target)
                             content (. div -textContent)]
                         (save-fn content @row column-kw)))
        property     (if unique
                       (assoc property :on-blur save)
                       (assoc property :on-input save))]
    [:div property
     value]))

(defn- number-button [i spreadsheet-state]
  (let [selected-rows (r/cursor spreadsheet-state [:selected-rows])
        select-row    #(swap! selected-rows conj i)
        unselect-row  (fn [] (swap! selected-rows (fn [selected-rows]
                                                    (set (filter #(not= i %) selected-rows)))))]
    [:div {:draggable       true
           :class           "mdl-button mdl-js-button mdl-js-button mdl-button--raised"
           :style           {:display   :table-cell
                             :width     plus-button-width
                             :min-width plus-button-width
                             :max-width plus-button-width
                             :padding   0}
           :on-click        #(if (tily/is-contained? i :in @selected-rows)
                              (unselect-row)
                              (select-row))
           :on-drag-start   (fn [evt]
                              (let [selected-row-indexes  (-> @spreadsheet-state (get-in [:selected-rows]))
                                    selected-entities     (-> @spreadsheet-state :rows
                                                              (select-keys selected-row-indexes)
                                                              vals)
                                    entity-ids            (map #(:system/id %) selected-entities)
                                    serialized-entity-ids (t/serialize entity-ids)]
                                (.. evt -dataTransfer (setData "data/transit" serialized-entity-ids))))

           :on-context-menu (fn [evt]
                              (let [rect   (.. evt -target -parentNode -parentNode -parentNode -parentNode getBoundingClientRect)
                                    x      (- (. evt -clientX) 10)
                                    y      (+ (. evt -clientY) 5)
                                    x      (- x (. rect -left))
                                    y      (- y (. rect -top))
                                    delete [:a {:href     "#"
                                                :on-click (fn [evt]
                                                            (let [current-rows   (:rows @spreadsheet-state)
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
                                                                  on-delete-rows (or (:on-delete-rows @spreadsheet-state)
                                                                                     constantly)]
                                                              (on-delete-rows rows-to-delete)

                                                              (reset! selected-rows #{})
                                                              (tily/set-atom! spreadsheet-state [:rows] new-rows)))} "Delete"]]
                                (select-row)
                                (tily/set-atom! spreadsheet-state [:context-menu :content] delete)
                                (tily/set-atom! spreadsheet-state [:context-menu :coordinate] [x y])
                                (. evt preventDefault)))}
     (inc i)]))

(defn- rows [spreadsheet-state]
  (let [id             (-> @spreadsheet-state :id)
        total-width    (get-content-width spreadsheet-state)
        total-height   (get-content-height spreadsheet-state)
        columns-config (-> @spreadsheet-state :columns-config)
        selected-rows  (r/cursor spreadsheet-state [:selected-rows])
        row-data       (fn [row]
                         (doall (for [[column-kw config] columns-config
                                      :let [render-column-fn (:render-column-fn config)
                                            k                (tily/format "spreadsheet-%s-%s-%s" id (:system/id @row) column-kw)]
                                      :when (:visible config)]
                                  (if render-column-fn
                                    ^{:key k} [render-column-fn column-kw row spreadsheet-state]
                                    ^{:key k} [default-column-render column-kw row spreadsheet-state]))))
        row-div        (fn [i row]
                         (let [style {:display :table-row}
                               style (if (tily/is-contained? i :in @selected-rows)
                                       (assoc style :background-color "#e6faff")
                                       style)]
                           [:div {:key   (tily/format "spreadsheet-%s-%s" id i)
                                  :style style}
                            (number-button i spreadsheet-state)
                            (row-data row)]))]
    [:div {:id    (tily/format "spreadsheet-%s-rows" id)
           :style {:display    :block
                   :height     total-height
                   :width      total-width
                   :overflow-y :auto
                   :overflow-x :hidden}}
     (doall (for [i (range (-> @spreadsheet-state :rows count))
                  :let [row (r/cursor spreadsheet-state [:rows i])]]
              (row-div i row)))]))

(defn- context-menu [spreadsheet-state]
  (let [content    (-> @spreadsheet-state :context-menu :content)
        coordinate (-> @spreadsheet-state :context-menu :coordinate)]
    (when content
      [:div {:id    "context-menu"
             :style {:background-color :white
                     :border           "1px solid grey"
                     :padding          5
                     :position         :absolute
                     :z-index          1
                     :left             (first coordinate)
                     :top              (second coordinate)}}
       content])))

(defn render [spreadsheet-state]
  (r/create-class {:component-will-mount (fn [this-component]
                                           (tily/set-atom! spreadsheet-state [:id] (str (rand-int 1000))))
                   :reagent-render       (fn [spreadsheet-state]
                                           [:div {:on-click #(when (-> @spreadsheet-state :context-menu :content)
                                                              (tily/set-atom! spreadsheet-state [:context-menu :content] nil))}
                                            ;[search-box spreadsheet-state]
                                            [context-menu spreadsheet-state]
                                            [column-headers spreadsheet-state]
                                            [rows spreadsheet-state]])}))
