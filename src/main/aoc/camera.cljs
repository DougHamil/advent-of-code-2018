(ns aoc.camera
  (:require [aoc.state :refer [state]]
            ["three" :as three]
            ["three-orbitcontrols" :as orbitcontrols]
            [threeagent.alpha.core :as th]))
(def directional-light (new three/DirectionalLight 0xFFFFFF 1.0))

;; Setup shadow
(set! (.-castShadow directional-light) true)
(.set (.-position (.-target directional-light)) -20 -10 1)
(let [shadow (.-shadow directional-light)]
  (set! (.-x (.-mapSize shadow)) 1024)
  (set! (.-y (.-mapSize shadow)) 1024)
  (set! (.-enabled shadow) true)
  (set! (.-bias shadow) 0.00001))
(let [shadow-cam (.-camera (.-shadow directional-light))]
  (set! (.-near shadow-cam) 1)
  (set! (.-left shadow-cam) -10)
  (set! (.-bottom shadow-cam) -10)
  (set! (.-right shadow-cam) 10)
  (set! (.-top shadow-cam) 10)
  (set! (.-far shadow-cam) 200))


(defn- setup-light [l]
  (set! (.-castShadow l) true)
  (.set (.-position (.-target l)) 0 0 0))

(defn render []
  [:object
   ^{:on-added #(.add directional-light (.-target directional-light))}
   [:instance {:object directional-light}]
   ^{:on-added #(do (swap! state assoc :camera %)
                    (swap! state assoc :orbit-controls (three/OrbitControls. %)))}
   [:camera {:fov 90
             :aspectRatio (/ 16 9)
             :near 0.1
             :far 1000
             :position [0 10 0]
             :rotation [0 0 0]}]])

(defn tick! [delta-time]
  (when-let [cam (:camera @state)]
    (.set (.-position directional-light)
          (.-x (.-position cam))
          (.-y (.-position directional-light))
          (.-z (.-position cam))))
  (when-let [follow-kart-id @(th/cursor state [:follow-kart-id])]
    (let [kart (get @(th/cursor state [:karts]) follow-kart-id)
          [x y z] (:position kart)
          cam (:camera @state)
          orbit (:orbit-controls @state)]
      (comment
        (set! (.-x (.-position cam)) x)
        (set! (.-z (.-position cam)) z))
      (set! (.-target orbit) (three/Vector3. x y z))
      (.update orbit)
      #_(.lookAt cam (three/Vector3. x y z)))))

(defn init! [])

(comment
  (do
    (swap! state assoc :follow-kart-id (:id (first (filter #(= :driving (:state %))
                                                           (vals (:karts @state))))))
         
    nil))

