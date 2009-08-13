(ns convex-hull
  (:import [javax.vecmath Vector2d])
  (:use clojure.contrib.math))

(set! *warn-on-reflection* 1)

(def #^java.util.Random r (java.util.Random.))

(defmacro sub [v1 v2]
   `(let [#^Vector2d v1# ~v1
	  #^Vector2d v2# ~v2
	  #^Vector2d result# (.clone v1#)]
      (.sub result# v2#)
      result#))

(defmacro point [x y]
  `(new Vector2d (float ~x) (float ~y)))

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

(defmacro angle-and-point [point base]
  `[(pseudo-angle (sub ~point ~base)) ~point])

(defmacro min-angle-and-point [ap1 ap2]
  `(let [angle1# (float (first ~ap1))
	 angle2# (float (first ~ap2))]
     (if (< angle1# angle2#) ~ap1 ~ap2)))

(defmacro find-point-with-least-angle-from [base angle points]
  `(let [angle (float ~angle)]
    (reduce min-angle-and-point
	    (remove
	     #(< (first %) angle)
	     (map #(angle-and-point % base)
		  (remove
		   (fn [p] (= base p))
		   points))))))

;; this could be made parallel
(defmacro find-point-with-least-angle-from [base angle points]
  `(let [#^Vector2d base#                      ~base
	 angle#                                (float ~angle)
	 #^"[Ljavax.vecmath.Vector2d;" points# ~points]
    (areduce points# 
	     i#
	     result# nil
	     (let [#^Vector2d next# (aget points# i#)]
	       (if (not= base# next#)
		 (let [next-angle# (float (pseudo-angle (sub next# base#)))]
		   (if (>= next-angle# angle#)
		     (if (not result#)
		       [next-angle# next#]
		       (min-angle-and-point result# [next-angle# next#]))
		     result#))
		 result#)))))

(defn hull [#^"[Ljavax.vecmath.Vector2d;" points]
  (println "Start")
  (let [#^Vector2d starting-point (find-min-point points)]
    (println starting-point)
    (loop [hull-list [starting-point] angle (float 0) last-point starting-point]
      (let [[angle next-point] (find-point-with-least-angle-from last-point angle points)]
        (if (= next-point (first hull-list))
          hull-list
          (recur (conj hull-list next-point) (float angle) next-point))))))
