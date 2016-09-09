(ns cljs-datagrid.demo
  (:require [reagent.core :as reagent]
            [com.kaicode.datagrid :as data-grid]))

(enable-console-print!)

(defn init-db []
  (let [weight        (/ 1 3)
        common-config {:visible      true
                       :width-weight weight}
        people        [{:person/first-name "Sonny"
                        :person/last-name  "Su"
                        :person/email      "sonny.su@foobar.com"}
                       {:person/first-name "John"
                        :person/last-name  "Smith"
                        :person/email      "john.smith@foobar.com"}
                       {:person/first-name "Jane"
                        :person/last-name "Doe"
                        :person/email "jane.doe@foobar.com"}]
        app-state     (reagent/atom {:window-dimension {:width  (. js/window -innerWidth)
                                                        :height (. js/window -innerHeight)}
                                     :rows             people
                                     :on-delete-rows   (fn [rows]
                                                         (println "deleting" rows))
                                     :columns-config   [[:person/first-name (merge common-config {:unique           true
                                                                                                  :render-header-fn (constantly "First Name")})]
                                                        [:person/last-name (merge common-config {:render-header-fn (constantly "Last Name")})]
                                                        [:person/email (merge common-config {:render-header-fn (constantly "Email")})]]})]
    app-state))

(defonce grid-state (init-db))

(defn hello-world []
  [data-grid/render grid-state])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
