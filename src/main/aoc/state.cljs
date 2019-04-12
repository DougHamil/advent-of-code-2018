(ns aoc.state
  (:require [threeagent.alpha.core :as th]))

(defonce state (th/atom {:track []}))

