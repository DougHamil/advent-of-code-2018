(ns aoc.kart
  (:require [aoc.state :refer [state]]
            [threeagent.alpha.core :as th]
            [aoc.util :as util]
            [aoc.models :as models]))
(defonce pi-over-2 (/ js/Math.PI 2.0))

(def direction->velocity {:down [0 0 1]
                          :up [0 0 -1]
                          :right [1 0 0]
                          :left [-1 0 0]})

(def direction->rotation {:right [0 pi-over-2 0]
                          :left [0 (* -1.0 pi-over-2) 0]
                          :up [0 (* 2.0 pi-over-2) 0]
                          :down [0 0 0]})

(def token->direction {"v" :down
                       "^" :up
                       "<" :left
                       ">" :right})

(def turn-order [:left :straight :right])

(def turn->direction {[:left :left] :down
                      [:right :left] :up

                      [:left :right] :up
                      [:right :right] :down

                      [:left :up] :left
                      [:right :up] :right

                      [:left :down] :right
                      [:right :down] :left})

(defn render []
  (if-let [karts @(th/cursor state [:karts])]
    [:object
     (for [[id k] karts]
       [:model {:type "carRed"
                :rotation (:rotation k)
                :position (:position k)}])]
    [:object]))

(defn- index-by-id [karts]
  (into {} (map #(vector (:id %) %)) karts))

(defn- sample-track [track [x y]]
  (util/aget2d track x y))

(defn- map-coords->pos [[x y]]
  [x 0 y])

(defn- apply-turn [turn dir]
  (if (= :straight turn)
    dir
    (turn->direction [turn dir])))

(defn- get-kart-direction [dir tile-type]
  (case [dir tile-type]
    [:right :left-to-down-corner] :down
    [:up :left-to-down-corner] :left

    [:down :down-to-right-corner] :right
    [:left :down-to-right-corner] :up

    [:up :up-to-right-corner] :right
    [:left :up-to-right-corner] :down

    [:right :left-to-up-corner] :up
    [:down :left-to-up-corner] :left
    dir))

(defn- step-coords [[x y] d]
  (let [x (int x)
        y (int y)
        [nx ny]
        (case d
          :left [(dec x) y]
          :right [(inc x) y]
          :up [x (dec y)]
          :down [x (inc y)])]
    [(int nx) (int ny)]))

(defn- move-kart [k]
  (let [new-coords (step-coords (:coords k) (:direction k))]
    (-> k
        (assoc :last-coords (:coords k))
        (assoc :coords new-coords))))

(defn- position-kart [sim-delta-time k]
  (let [p (map-coords->pos (:coords k))
        v (map (partial * sim-delta-time)
               (direction->velocity (:direction k)))]
    (if (= :crashed (:state k))
      (assoc k :position p)
      (assoc k :position (map + v p)))))


(defn- handle-intersection [k]
  (let [turn-idx (:turn-idx k) 
        turn (nth turn-order turn-idx)
        new-dir (apply-turn turn (:direction k))]
    (-> k
        (assoc :direction new-dir)
        (assoc :rotation (direction->rotation new-dir))
        (assoc :turn-idx (mod (int (inc turn-idx))
                              (count turn-order))))))

(defn- steer-kart [track k]
  (if (not= (:coords k) (:last-coords k))
    (let [tile-type (sample-track track (:coords k))]
      (if (= :intersection tile-type)
        (handle-intersection k)
        (let [new-dir (get-kart-direction (:direction k) tile-type)]
          (-> k
              (assoc :rotation (direction->rotation new-dir))
              (assoc :direction new-dir)))))
    k))

(defn- tick-kart [track k]
  (if (= :crashed (:state k))
    k
    (->> k
         (move-kart)
         (steer-kart track))))

(defn- apply-collisions [k all-karts colliding-karts]
  (let [colliding (set colliding-karts)
        uncolliding (filter #(not (colliding %)) all-karts)
        colliding (if (empty? colliding)
                    colliding
                    (conj colliding k))]
    (concat uncolliding (map #(assoc % :state :crashed) colliding))))

(defn- find-colliding-kart [k orig-k karts]
  (let [coords (:coords k)]
    (->> karts
     (filter #(and (not= orig-k %)
                   (= (:coords %) coords)))
     (first))))

(defn- kart-sort [a b]
  (let [[ax ay] (:coords a)
        [bx by] (:coords b)]
    (if (= ay by)
      (- ax bx)
      (if (> by ay)
        -1
        1))))
(comment
  (println (group-by :state (vals (:karts @state)))))

(defn- simulation-tick! []
  (let [all-karts (vals (:karts @state))
        track-map (:track-map @state)
        grouped-karts (group-by :state all-karts)
        sorted-karts (sort-by kart-sort (:driving grouped-karts))
        ticked-karts
          (reduce
            (fn [{:keys [driving crashed collided]} k]
              (if (collided k)
                {:driving driving :crashed crashed :collided collided}
                (let [ticked (tick-kart track-map k)
                      colliding (find-colliding-kart ticked k driving)]
                  (if colliding
                    (let [as-set #{colliding k}]
                      {:driving (remove (partial contains? as-set) driving)
                       :collided (set (concat as-set collided))
                       :crashed (concat crashed [(assoc colliding :state :crashed)
                                                 (assoc ticked :state :crashed)])})
                    {:driving (conj (remove #{k} driving)
                                    ticked)
                     :collided collided
                     :crashed crashed}))))
            (assoc grouped-karts :collided #{})
            sorted-karts)]
    (swap! state assoc :karts (index-by-id (concat (:driving ticked-karts) (:crashed ticked-karts))))
    (let [uncrashed (filter #(not= :crashed (:state %)) (vals (:karts @state)))]
      (when (= 1 (count uncrashed))
        (swap! state assoc :sim-state :paused)
        (println "LAST KART" (:coords (first uncrashed)))))))

(defn tick! [delta-time]
  (when (and (:karts @state)
             (:track-map @state)
             (not= :paused (:sim-state @state)))
    (let [sim-speed-scale 2.0
          sim-time (or (:sim-time @state) 0)
          sim-tick (int sim-time)
          new-sim-time (+ sim-time (* sim-speed-scale delta-time))
          new-sim-tick (int new-sim-time)
          sim-delta (- new-sim-time new-sim-tick)]
      (doseq [t (range (- new-sim-tick sim-tick))]
        (when (not= :paused (:sim-state @state))
          (simulation-tick!)))
      (swap! state assoc :karts (index-by-id (map (partial position-kart sim-delta) (vals (:karts @state)))))
      (swap! state assoc :sim-time new-sim-time))))

(defn init! [track-text-array]
  (let [height (count track-text-array)
        width (count (first track-text-array))
        karts (array)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [c (util/aget2d track-text-array x y)]
          (when-let [d (token->direction c)]
            (.push karts {:position [x (- y) 0]
                          :state :driving
                          :id (str x "," y)
                          :coords [x y]
                          :last-coords [x y]
                          :rotation (direction->rotation d)
                          :turn-idx 0
                          :direction d})))))
    (swap! state assoc :karts (index-by-id karts))))

(comment
  (:track-map @state))

