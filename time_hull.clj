(ns time-hull
  (:require [convex-hull :as convex])
  (:import [javax.vecmath Vector2d])
  (:use clojure.contrib.math))

(def r (java.util.Random.))
(defn rands [] (repeatedly #(.nextGaussian r)))
(defn points [] (vec (take 400000 (partition 2 (rands)))))
(let [hull-points (time (hull (points)))]
  (printf "Points: %d\n" (count hull-points))
  (doseq [x hull-points] (println x)))

(defmacro quadrant-one-pseudo-angle [p]
  `(float
    (let [#^Vector2d p# ~p
	  dx# (float (.x p#))
	  dy# (float (.y p#))]
      (/ dx# (+ dy# dx#)))))

(defmacro pseudo-angle [p]
  `(float
    (let [#^Vector2d p# ~p
	  dx#           (float (.x p#))
	  dy#           (float (.y p#))
	  zero#         (float 0)]
      (cond
	(and (zero? dx#) (zero? dy#))      zero#
	(and (>= dx# zero#) (> dy# zero#)) (quadrant-one-pseudo-angle p#)
	(and (> dx# zero#) (<= dy# zero#)) (+ (float 1) (float (quadrant-one-pseudo-angle (point (clojure.contrib.math/abs dy#) dx#))))
	(and (<= dx# zero#) (< dy# zero#)) (+ (float 2) (float (quadrant-one-pseudo-angle (point (clojure.contrib.math/abs dx#) (~'abs dy#)))))
	(and (< dx# 0) (>= dy# zero#))     (+ (float 3) (float (quadrant-one-pseudo-angle (point dy# (clojure.contrib.math/abs dx#)))))
	:else nil))))

(defmacro point-min [p1 p2]
  `(let [#^Vector2d p1# ~p1
	 #^Vector2d p2# ~p2
	 x1#        (.x p1#)
	 y1#        (.y p1#)
	 x2#        (.x p2#)
	 y2#        (.y p2#)]
     (cond
       (< x1# x2#) p1#
       (= x1# x2#) (if (< y1# y2#) p1# p2#)
       :else       p2#)))

(defmacro delta-point [p1 p2]
  `(let [#^Vector2d p1# ~p1
	 #^Vector2d p2# ~p2
	 x1#        (float (.x p1#))
	 y1#        (float (.y p1#))
	 x2#        (float (.x p2#) )
	 y2#        (float (.y p2#))]
     (point (- x1# x2#) (- y1# y2#))))

(defmacro angle-and-point [point base]
  `[(pseudo-angle (delta-point ~point ~base)) ~point])

(defn points [n]
  (make-array Vector2d n))

(defmacro point [x y]
  `(new Vector2d (float ~x) (float ~y)))

(comment 
  (defmacro point-min [p1 p2]
   `(let [v1# ~v1
	  v2# ~v2]
      (cond 
	(< (.x v1#)))))
)

(defmacro sub [v1 v2]
   `(let [#^Vector2d v1# ~v1
	  #^Vector2d v2# ~v2]
      (.sub v1# v2#)
      v1#))

(defmacro add [v1 v2]
   `(let [#^Vector2d v1# ~v1
	  #^Vector2d v2# ~v2]
      (.add v1# v2#)
      v1#))

(defmacro div [v1 v2]
   `(let [#^Vector2d v1# ~v1
	  #^Vector2d v2# ~v2]
      (.div v1# v2#)
      v1#))

(comment
  (def my-points (points 400000))

  ;; 100ms since we have create the points here
  (do
    (set! *warn-on-reflection* 1)
    (time
     (dotimes [x 1000000]
       (sub (point 5 3) (point 1 1)))))

  (def p1 (point 5 3))
  (def p2 (point 1 1))

  (quadrant-one-pseudo-angle (point 5 5))

  ;; ~20ms
  (do
    (set! *warn-on-reflection* 1)
    (time
     (dotimes [x 1000000]
       (add p1 p2))))

  (do
    (set! *warn-on-reflection* 1)
    (time
     (let [p (point 5 5)]
      (dotimes [x 1000000]
	(quadrant-one-pseudo-angle p)))))
  
  ;; right
  (quadrant-one-pseudo-angle (point 5 5))

  ;; right
  (point-min (point 5 5) (point 10 10))

  (do
    (set! *warn-on-reflection* 1)
    (time
     (let [p1 (point 5 5)
	   p2 (point 10 10)]
       (dotimes [x 1000000]
	 (delta-point p1 p2)))))
  
  ;; right
  (delta-point (point 5 5) (point 10 10))

  (angle-and-point (point 5 5) (point 1 1))
)
