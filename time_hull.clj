(ns time-hull
  (:use convex-hull))

;; 2s to 2.5s
(def my-points (points 400000 rand-point))
(let [hull-points (time (hull my-points))]
  (printf "Points: %d\n" (count hull-points))
  (doseq [x hull-points] (println x)))
  
