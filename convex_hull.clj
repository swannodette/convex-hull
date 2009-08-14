(ns convex-hull
  (:import [javax.vecmath Vector2d])
  (:import Points)
  (:use clojure.contrib.math))

(set! *warn-on-reflection* 1)

(definline points
  "Casts to Vector2d[]"
  [xs] `(. Points points ~xs))

(def #^java.util.Random r (java.util.Random.))

(defmacro point [x y]
  `(new Vector2d (float ~x) (float ~y)))

(comment
  (defmacro sub [v1 v2]
    `(let [#^Vector2d temp# (.clone ~v1)]
       (.sub temp# ~v2)
       temp#))
)

(def #^Vector2d mutable-point (point 0 0))
(defmacro sub [v1 v2]
  `(do
     (.set mutable-point (.x ~v1) (.y ~v1))
     (let [#^Vector2d mutable-point# mutable-point
	   #^Vector2d v#             ~v2]
       (.sub mutable-point# ~v2)
       mutable-point#)))

(comment
  (def #^Vector2d mutable-point (point 0 0))
  (defmacro sub [v1 v2]
    `(do
       (.set mutable-point (.x ~v1) (.y ~v1))
       (let [#^Vector2d mutable-point# mutable-point
	     #^Vector2d v#             ~v2]
	 (.sub mutable-point# ~v2)
	 mutable-point#)))
  )

(defn #^"[Ljavax.vecmath.Vector2d;" point-array [n]
  (make-array Vector2d n))

(defn #^"[Ljavax.vecmath.Vector2d;" make-points
  ([n] 
     (let [ary (point-array n)]
       ary))
  ([n fn] 
     (let [ary (point-array n)]
       (amap ary i result 
         (let [#^Vector2d p (fn)]
           (aset ary i p))))))

(defn #^Vector2d rand-point []
  (point (.nextGaussian r) (.nextGaussian r)))

(defmacro quadrant-one-pseudo-angle [x y]
  `(/ ~x (+ ~y ~x)))

(defmacro pseudo-angle [p]
  `(let [#^Vector2d p# ~p
	 dx#        (float (.x p#))
	 dy#        (float (.y p#))
	 zero#      (float 0)]
     (cond
       (and (zero? dx#) (zero? dy#))      zero#
       (and (>= dx# zero#) (> dy# zero#)) (quadrant-one-pseudo-angle dx# dy#)
       (and (> dx# zero#) (<= dy# zero#)) (let [ady# (java.lang.Math/abs dy#)]
					    (+ (float 1) (float (quadrant-one-pseudo-angle ady# dx#))))
       (and (<= dx# zero#) (< dy# zero#)) (let [adx# (java.lang.Math/abs dx#)
						ady# (java.lang.Math/abs dy#)]
					    (+ (float 2) (float (quadrant-one-pseudo-angle adx# ady#))))
       (and (< dx# 0) (>= dy# zero#))     (let [adx# (java.lang.Math/abs dx#)]
					    (+ (float 3) (float (quadrant-one-pseudo-angle dy# adx#))))
       :else nil)))

(defn #^Vector2d point-min [#^Vector2d p1 #^Vector2d p2]
  (let [x1 (.x p1)
	y1 (.y p1)
	x2 (.x p2)
	y2 (.y p2)]
     (cond
       (< x1 x2) p1
       (= x1 x2) (if (< y1 y2) p1 p2)
       :else     p2)))

(defn #^Vector2d find-min-point [#^"[Ljavax.vecmath.Vector2d;" vs]
  (areduce vs i result (aget vs (int 0))
	   (point-min result (aget vs i))))

(defn angle-and-point [#^Vector2d point #^Vector2d base]
  [(pseudo-angle (sub point base)) point])

(defn find-point-with-least-angle-from [#^Vector2d base angle #^"[Ljavax.vecmath.Vector2d;" vs]
  (let [angle      (float angle)]
    (loop [i (int 0) #^Vector2d result nil result-angle (float 0)]
      (if (< i (int (alength vs)))
	(let [#^Vector2d next (aget vs i)]
	  (if (not (.equals base next))
	    (let [next-angle (float (pseudo-angle (sub next base)))]
	      (if (>= (float next-angle) (float angle))
		(if (nil? result)
		  (recur (unchecked-inc i) next next-angle)
		  (if (= (float (java.lang.Math/min result-angle next-angle)) (float next-angle))
		    (recur (unchecked-inc i) next next-angle)
		    (recur (unchecked-inc i) result result-angle)))
		(recur (unchecked-inc i) result result-angle)))
	    (recur (unchecked-inc i) result result-angle)))
	[result-angle result]))))

(defn hull [#^"[Ljavax.vecmath.Vector2d;" vs]
  (println "Start")
  (let [#^Vector2d starting-point (find-min-point vs)]
    (println starting-point)
    (loop [hull-list [starting-point] angle (float 0) #^Vector2d last-point starting-point]
      (let [[angle #^Vector2d next-point] (find-point-with-least-angle-from last-point angle (points vs))
	    #^Vector2d first-point        (first hull-list)]
        (if (.equals next-point first-point)
	  hull-list
	  (recur (conj hull-list next-point) (float angle) next-point))))))
