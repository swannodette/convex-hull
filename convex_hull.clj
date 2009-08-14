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

(defmacro sub [v1 v2]
  `(let [#^Vector2d temp# (.clone ~v1)]
     (.sub temp# ~v2)
     mutable-point))

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

(defmacro pseudo-angle [p]
  `(let [dx#   (.x ~p)
	 dy#   (.y ~p)
	 zero# (float 0)]
     (cond
       (and (zero? dx#) (zero? dy#))    zero#
       (and (>= dx# zero#) (> dy# zero#)) (quadrant-one-pseudo-angle ~p)
       (and (> dx# zero#) (<= dy# zero#)) (+ (float 1) (float (quadrant-one-pseudo-angle (point (abs dy#) dx#))))
       (and (<= dx# zero#) (< dy# zero#)) (+ (float 2) (float (quadrant-one-pseudo-angle (point (abs dx#) (abs dy#)))))
       (and (< dx# 0) (>= dy# zero#))     (+ (float 3) (float (quadrant-one-pseudo-angle (point dy# (abs dx#)))))
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

(def #^Vector2d p1 (point 5 5))
(def #^Vector2d p2 (point 1 1))
(defn find-point-with-least-angle-from [#^Vector2d base angle vs]
  (let [angle      (float angle)
	vs         (points vs)]
    (loop [i (int 0) #^Vector2d result nil result-angle (float 0)]
      (if (< i (alength vs))
;; 	(let [#^Vector2d next (aget vs i)]
;; 	  (if (not (.equals base next))
;; 	    (let [next-angle (float (pseudo-angle (sub next base)))]
;; 	      (if (>= next-angle angle)
;; 		(if (nil? result)
;; 		  (recur (unchecked-inc i) next next-angle)
;; 		  (if (= (float (min result-angle next-angle)) next-angle)
;; 		    (recur (unchecked-inc i) next next-angle)
;; 		    (recur (unchecked-inc i) result result-angle)))
;; 		(recur (unchecked-inc i) result result-angle)))
;; 	    (recur (unchecked-inc i) result result-angle)))
;;	[result-angle result]
	(let [#^Vector2d next (aget vs i)
	      #^Vector2d p1   p1
	      #^Vector2d p2   p2]
	  (do
	    (not (.equals next p1))
	    (pseudo-angle (sub p1 p2))
;;	    (if result
;;		(sub result next))
;;	    (.sub mutable-point next)
	    (recur (unchecked-inc i) (aget vs i) result-angle)))
	[0 (.clone (point 0 0))]
	))))

(comment
  ;; 80ms for 8000000 iterations
  (defn find-point-with-least-angle-from [#^Vector2d base angle vs]
    (let [angle      (float angle)
	  vs         (points vs)]
      (loop [i (int 0) #^Vector2d result nil result-angle (float 0)]
	(if (< i (alength vs))
	  [result-angle result]
	  (let [#^Vector2d next (aget vs i)]
	    (do
	      (not (.equals next p))
	      (recur (unchecked-inc i) (aget vs i) result-angle)))
	  [0 (point 0 0)]
	  ))))
)

(defn hull [#^"[Ljavax.vecmath.Vector2d;" vs]
  (println "Start")
  (let [#^Vector2d starting-point (find-min-point vs)]
    (println starting-point)
    (loop [hull-list [starting-point] angle (float 0) #^Vector2d last-point starting-point]
      (println "looping")
      (let [[angle #^Vector2d next-point] (find-point-with-least-angle-from last-point angle (points vs))
	    #^Vector2d first-point        (first hull-list)]
        (if (.equals next-point first-point)
	  hull-list
	  (recur (conj hull-list next-point) (float angle) next-point))))))
