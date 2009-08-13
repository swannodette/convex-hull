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

(defn rand-point []
  (point (.nextGaussian r) (.nextGaussian r)))

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

(defn find-min-point [points]
  (let [#^"[Ljavax.vecmath.Vector2d;" points points]
    (areduce points i result (aget points (int 0))
	     (point-min result (aget points i)))))

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

(defmacro min-angle-and-point [ap1 ap2]
  `(let [#^Vector2d ap1# ~ap1
	 #^Vector2d ap2# ~ap2]
    (if (< (float (.x ap1#)) (float (.x ap2#))) ap1# ap2#)))

(defmacro find-point-with-least-angle-from [base angle points]
  `(let [angle (float ~angle)]
    (reduce min-angle-and-point
	    (remove
	     #(< (first %) angle)
	     (map #(angle-and-point % base)
		  (remove
		   (fn [p] (= base p))
		   points))))))

(defmacro find-point-with-least-angle-from [base angle points]
  `(let [#^Vector2d base#                      ~base
	 angle#                                (float ~angle)
	 #^"[Ljavax.vecmath.Vector2d;" points# ~points]
    (areduce points# i# result# (aget points# (int 0))
	    (let [#^Vector2d next# (aget points# i#)]
	      (if (and (not= base# next#)
		       (>= (pseudo-angle next#) angle#))
		(min-angle-and-point result# next#)
		next#)))))

(comment
  ;; ignore p that eq base
  ;; ignore p with angle < than angle
  ;; 
  )

(defn point-array [n]
  (make-array Vector2d n))

(defn points
  ([n] 
     (let [#^"[Ljavax.vecmath.Vector2d;" ary (point-array n)]
       ary))
  ([n fn] 
     (let [#^"[Ljavax.vecmath.Vector2d;" ary (point-array n)]
       (amap ary i result 
	     (let [#^Vector2d p (fn)]
	       (aset ary i p))))))

(defmacro point [x y]
  `(new Vector2d (float ~x) (float ~y)))

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
  (def my-points (points 400000 rand-point))

  ;; right
  (quadrant-one-pseudo-angle (point 5 5))

  ;; < 10ms
  (do
    (set! *warn-on-reflection* 1)
    (let [p1 (point 5 5)
	  p2 (point 2 3)]
     (time
      (dotimes [x 1000000]
	(add p1 p2)))))

  ;; a little slower than original
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

  ;; 5x faster than original
  (do
    (set! *warn-on-reflection* 1)
    (time
     (let [p1 (point 5 5)
	   p2 (point 10 10)]
       (dotimes [x 1000000]
	 (delta-point p1 p2)))))
  
  ;; right
  (delta-point (point 5 5) (point 10 10))

  ;; right
  (angle-and-point (point 5 5) (point 1 1))

  ;; 2x faster than original
  (do
    (set! *warn-on-reflection* 1)
    (time
     (let [p1 (point 5 5)
	   p2 (point 10 10)]
       (dotimes [x 1000000]
	 (angle-and-point p1 p2)))))

  ;; takes 3.5s
  (do
   (set! *warn-on-reflection* 1)
    (time
     (points 400000 rand-point)))

  ;; 3ms
  (do
    (time
     (find-min-point my-points)))

  ;; right
  (min-angle-and-point (point 5 5) (point 10 10))

  (defn fill [v]
    (let [idx (atom 0)]
      (fn []
	(let [result (nth v @idx)]
	  (swap! idx inc)
	  result))))

  (let [p     (point 5 5)
	angle (pseudo-angle p)
	vs    (points 10 (fill [(point 1 1) (point 2 0.5) (point 3 3.5) (point -5 -0.5) (point 10 20) 
			       (point 0.3 0.3) (point -4 3) (point 2 -5) (point -9 8) (point -2.3 3.333)]))]
   (find-point-with-least-angle-from p angle vs))

  ;; wrong
  (vec (points 10 (fill [(point 1 1) (point 2 0.5) (point 3 3.5) (point -5 -0.5) (point 10 20) 
		     (point 0.3 0.3) (point -4 3) (point 2 -5) (point -9 8) (point -2.3 3.333)])))
)
