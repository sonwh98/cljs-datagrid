(ns cljs-datagrid.util
  (:require [cljs-datagrid.constants :as const]))

(defn get-column-config [spreadsheet-state column-kw]
  (->> @spreadsheet-state :columns-config (filter #(= (first %) column-kw)) first second))

(defn get-window-dimension [spreadsheet-state]
  (-> @spreadsheet-state :window-dimension))

(defn get-content-height [spreadsheet-state]
  (let [padding-bottom 35
        fudge-factor 30]
    (-> spreadsheet-state get-window-dimension :height (- const/title-bar-height padding-bottom const/search-box-height fudge-factor))))

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
  (let [total-content-width (- (get-content-width spreadsheet-state) const/plus-button-width const/cog-button-wdith)
        invisible-columns-config (get-invisible-columns spreadsheet-state)
        extra-width (->> invisible-columns-config
                         (map #(-> % second :width-weight (* total-content-width)))
                         (reduce +))]
    (-> extra-width
        (/ (-> spreadsheet-state get-visible-columns count))
        js/Math.floor)))

(defn get-column-width [column-kw spreadsheet-state]
  (let [total-content-width (- (get-content-width spreadsheet-state) const/plus-button-width const/cog-button-wdith)
        width-weight        (:width-weight (get-column-config spreadsheet-state column-kw))
        width               (+ (* width-weight total-content-width)
                               (extra-width-per-visible-column spreadsheet-state))]
    (js/Math.floor width)))

(defn get-default-column-style [column-kw spreadsheet-state]
  (let [column-width (get-column-width column-kw spreadsheet-state)
        style (merge {:width         column-width
                      :min-width     column-width
                      :max-width     column-width
                      :text-align :center
                      :vertical-align :middle}
                     const/common-column-style)]
    style))

