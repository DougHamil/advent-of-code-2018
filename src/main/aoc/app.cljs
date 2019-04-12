(ns aoc.app
  (:require [threeagent.alpha.core :as th]
            [cljs.core.async :refer [chan put! >! <!]]
            [aoc.state :refer [state]]
            [aoc.track :as track]
            [aoc.kart :as kart]
            [aoc.camera :as camera]
            [aoc.models :as models]
            [aoc.instanced-model :as instanced-model]
            ["three" :as three])
  (:require-macros [cljs.core.async :refer [go]]))

(defn- log [d]
  (.log js/console d))

(defn- root []
  [:object
   [camera/render]
   [:hemisphere-light {:position [0 10 0]
                       :sky-color 0xFFFFFF
                       :intensity 0.8}]
   [:object {:rotation [(/ js/Math.PI -2.0) 0 0]}
     [track/render]]
   [kart/render]])

(defn- tick! [delta-time]
  (kart/tick! delta-time)
  (camera/tick! delta-time))

(defn- setup-scene [ctx]
  (let [renderer (.-renderer ctx)]
    (set! (.-enabled (.-shadowMap renderer)) true)
    (.setClearColor renderer 0xDDDDDD)))

(defn init []
  (go (do
        (<! (instanced-model/init!))
        (<! (models/init!))
        (setup-scene
         (th/render root
                    (.getElementById js/document "root")
                    {:on-before-render tick!}))))
  (camera/init!)
  (track/init!))

(defn ^:dev/after-load reload []
  (setup-scene
   (th/render root
              (.getElementById js/document "root")
              {:on-before-render tick!})))

(comment
  (log (count (:track @state)))
  (log (count (first (:track @state)))))


