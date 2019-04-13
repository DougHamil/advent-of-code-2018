(ns aoc.ui
  (:require [reagent.core :as r]
            [aoc.kart :refer [add-kart!]]
            [aoc.state :refer [state postfx-state]]))


(defn kart-selector [kart]
  [:div
   [:input {:type "radio"
            :name "follow-kart"
            :checked (= (:id kart)
                        @(r/cursor state [:follow-kart-id]))
            :on-change #(do
                          (swap! postfx-state assoc :focus 0.85)
                          (swap! state assoc :follow-kart-id (:id kart)))}]
   [:span
    [:span (str (:coords kart))]
    (when (= :crashed (:state kart))
      [:img {:src "fire.png"
             :style {:width "20px"}}])]])

(defn karts []
  [:div
   [:div "Karts"]
   [:input {:type "radio"
            :name "follow-kart"
            :checked (nil? @(r/cursor state [:follow-kart-id]))
            :on-change #(do
                          (swap! postfx-state assoc :focus 1.0)
                          (swap! state assoc :follow-kart-id nil))}]
   [:span "None"]
   (when-let [karts @(r/cursor state [:karts])]
     (for [[key kart] karts]
       ^{:key (:id kart)}
       [kart-selector kart]))])

(defn input []
  [:div
   [:div
    [:label "Focus:"]
    [:input {:type "range"
             :min "0.0"
             :max "1.0"
             :step "0.005"
             :value @(r/cursor postfx-state [:focus])
             :on-change #(swap! postfx-state assoc :focus (-> % .-target .-value js/parseFloat))}]]
   [:div
    [:label "Sim Speed:"]
    [:input {:type "range"
             :min "0"
             :max "500"
             :step "1"
             :value @(r/cursor state [:simulation-speed-scale])
             :on-change #(swap! state assoc :simulation-speed-scale (js/parseInt (.-value (.-target %))))}]]
   [:button {:on-click add-kart!}
            "Add Kart"]])
    
    

(defn info []
  [:div
   [:label (str "Current Tick: " (int @(r/cursor state [:sim-time])))]])

(defn root []
  [:div {:style {:background-color "rgba(255, 255, 255, 0.9)"}}
   [info]
   [:hr]
   [input]
   [:hr]
   [karts]])

(defn init! []
  (r/render root (.getElementById js/document "ui-root")))


(defn reload []
  (r/render root (.getElementById js/document "ui-root")))

(comment
  (:focus @state))

