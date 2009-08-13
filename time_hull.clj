(ns time-hull
  (:use convex-hull))

(set! *warn-on-reflection* 1)

;; 2s - 2.5s
(def my-points (points 400000 rand-point))
(let [hull-points (time (hull my-points))]
  (printf "Points: %d\n" (count hull-points))
  (doseq [x hull-points] (println x)))
  
