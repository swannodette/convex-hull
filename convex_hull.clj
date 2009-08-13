(ns convex-hull
  (:import [javax.vecmath Vector2d])
  (:import Points)
  (:use clojure.contrib.math))

(set! *warn-on-reflection* 1)

(definline points
  "Casts to Vector2d[]"
  [xs] `(. Points points ~xs))

(def #^java.util.Random r (java.util.Random.))

(defmacro sub [v1 v2]
  `(let [v1# ~v1
         v2# ~v2
         result# (.clone v1#)]
     (.sub result# v2#)
     result#))

(defmacro add [v1 v2]
  `(let [v1# ~v1
         v2# ~v2
         result# (.clone v1#)]
     (.add result# v2#)
     result#))

(defmacro point [x y]
  `(new Vector2d (float ~x) (float ~y)))

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

(defn quadrant-one-pseudo-angle [#^Vector2d p]
  (let [dx (.x p)
	dy (.y p)]
     (/ dx (+ dy dx))))

(defn pseudo-angle [#^Vector2d p]
  (let [dx   (.x p)
	dy   (.y p)
	zero (float 0)]
     (cond
       (and (zero? dx) (zero? dy))    zero
       (and (>= dx zero) (> dy zero)) (quadrant-one-pseudo-angle p)
       (and (> dx zero) (<= dy zero)) (+ (float 1) (float (quadrant-one-pseudo-angle (point (abs dy) dx))))
       (and (<= dx zero) (< dy zero)) (+ (float 2) (float (quadrant-one-pseudo-angle (point (abs dx) (abs dy)))))
       (and (< dx 0) (>= dy zero))    (+ (float 3) (float (quadrant-one-pseudo-angle (point dy (abs dx)))))
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

(defn min-angle-and-point [ap1 ap2]
  (let [angle1 (float (first ap1))
	angle2 (float (first ap2))]
    (if (< angle1 angle2) ap1 ap2)))

;; this could be made parallel
;; should verify that this is running as fast as possible
(defmacro find-point-with-least-angle-from [base angle vs]
  `(areduce ~vs i# result# nil
	    (let [next# (aget ~vs i#)]
	      (if (not= ~base next#)
		(let [next-angle# (pseudo-angle (sub next# ~base))]
		  (if (>= next-angle# ~angle)
		    (if (not result#)
		      [next-angle# next#]
		      (min-angle-and-point result# [next-angle# next#]))
		    result#))
		result#))))

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
