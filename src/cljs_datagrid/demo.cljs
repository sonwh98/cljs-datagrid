(ns cljs-datagrid.demo
  (:require [reagent.core :as reagent]
            [com.kaicode.datagrid :as datagrid]))

(enable-console-print!)

(defn init-db []
  (let [weight        (/ 1 5)
        common-config {:visible?      true
                       :width-weight weight}
        people        [{:person/first-name "Sonny"
                        :person/last-name  "Su"
                        :person/email      "sonny.su@foobar.com"
                        :person/telephone  "123"
                        :person/gender     "M"
                        :on-expand (fn [this-row]
                                     [:div
                                      [:select
                                       [:option {:value 1} 1]
                                       [:option {:value 2} 2]
                                       [:option {:value 3} 3]]
                                      [:img {:src "https://encrypted-tbn2.gstatic.com/images?q=tbn:ANd9GcSvkX2GAcg7E-ssPgcBStSck01nL0PvfDGEmbzRdl5t7ieZYK26"}]])}
                       {:person/first-name "John"
                        :person/last-name  "Smith"
                        :person/email      "john.smith@foobar.com"
                        :person/telephone  "1234"
                        :person/gender     "M"}
                       {:person/first-name "Jane"
                        :person/last-name  "Doe"
                        :person/email      "jane.doe@foobar.com"
                        :person/telephone  "12345"
                        :person/gender     "F"}
                       {:person/first-name "Jane"
                        :person/last-name  "Austen"
                        :person/email      "jane.austen@foobar.com"
                        :person/telephone  "123456"
                        :person/gender     "F"}]
        app-state     (reagent/atom {:window-dimension {:width  (. js/window -innerWidth)
                                                        :height (. js/window -innerHeight)}
                                     :left-corner-block (fn [grid-state style]
                                                          ;; This fn should be able to merge provided
                                                          ;; styles with it's top-level node, so we
                                                          ;; are able to set it's width for example.
                                                          [:div {:style (merge {:display :table-cell
                                                                                :vertical-align :middle
                                                                                :text-align :center}
                                                                               style)}
                                                           [:div {:class "mdl-button mdl-js-button mdl-button--fab mdl-button--mini-fab mdl-button--colored"}
                                                            [:i {:class "material-icons"
                                                                 :on-click #(let [id (:id @grid-state)
                                                                                  div-rows (js/document.getElementById (str "grid-" id "rows"))]
                                                                              (swap! grid-state update-in [:rows] conj {}))} "add"]]])
                                     :rows             people
                                     :on-delete-rows   (fn [rows]
                                                         (println "deleting" rows))
                                     :columns-config   [[:person/first-name (merge common-config {:unique           true
                                                                                                  :render-header-fn (constantly "First Name")})]
                                                        [:person/last-name  (merge common-config {:render-header-fn (constantly "Last Name")})]
                                                        [:person/email      (merge common-config {:render-header-fn (constantly "Email")})]
                                                        [:person/telephone  (merge common-config {:render-header-fn (constantly "Telephone")})]
                                                        [:person/gender     (merge common-config {:render-header-fn (constantly "Gender")})]]})]
    app-state))

(defonce grid-state (init-db))

(defn hello-world []
  [datagrid/render grid-state])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
