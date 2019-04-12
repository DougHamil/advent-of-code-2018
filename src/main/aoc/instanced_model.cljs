(ns aoc.instanced-model
  (:require ["three" :as three]
            ["three-instanced-mesh" :as instanced-mesh]
            ["three-gltf-loader" :as gltfloader]
            [cljs.core.async :refer [chan put! >! <!]]
            [threeagent.alpha.core :as th])
  (:require-macros [threeagent.alpha.macros :refer [defcomponent]]
                   [cljs.core.async :refer [go]]))

(instanced-mesh three) ;; Path three.js with instanced-mesh library

(defonce ^:private models-to-load {"roadEnd" {:filepath "models/roadEnd.glb"
                                              :offset [0 -0.5 0]}
                                   "roadStraight" {:filepath "models/roadStraight.glb"
                                                   :offset [0 0 0]}
                                   "roadCrossing" {:filepath "models/roadCrossingSmall.glb"
                                                   :offset [0 0 0]}
                                   "roadCorner" {:filepath "models/roadCornerSmall2.glb"
                                                 :offset [0 0 0]}
                                   "tree" {:filepath "models/treeSmall.glb"
                                           :offset [0 0 0]}})
(defonce ^:private models (js/Map.))
        
(defcomponent :instanced-models [{:keys [type instances]}]
  (let [source-model (.get models type)
        source-meshes (.-children source-model)
        [ox oy oz] (:offset (get models-to-load type))
        instance-count (count instances)
        meshes (map (fn [m]
                      (three/InstancedMesh. (.-geometry m)
                                            (.-material m)
                                            instance-count
                                            false false true))
                    source-meshes)
        parent (three/Object3D.)
        offset (three/Vector3. ox oy oz)]
    (doseq [[idx i] (map-indexed vector instances)]
      (let [[px py pz] (:position i)
            [rx ry rz] (:rotation i)
            euler (three/Euler. rx ry rz "XYZ")
            position (three/Vector3. px py pz)
            rotation (three/Quaternion.)]
        (.add position offset)
        (.setFromEuler rotation euler)
        (doseq [m meshes]
          (set! (.-castShadow m) true)
          (set! (.-receiveShadow m) true)
          (.setPositionAt m idx position)
          (.setQuaternionAt m idx rotation)
          (.setScaleAt m idx (three/Vector3. 1 1 1)))))
    (doseq [m meshes]
      (.add parent m))
    parent))

(defn- store-model! [type scene]
  (let [model (aget (.-children (.-scene scene)) 0)]
    (.set models type model)))

(defn init! []
  (let [loader (new gltfloader)
        c (chan)
        load-chans (map (fn [[model-key {:keys [filepath]}]]
                          (let [c (chan)]
                            (.load loader filepath #(do (store-model! model-key %)
                                                        (put! c model-key)))
                            c))
                        models-to-load)]
    (go
      (doseq [c load-chans]
        (let [model-key (<! c)]
          (println "Loaded model" model-key)))
      (put! c true))
    c))
