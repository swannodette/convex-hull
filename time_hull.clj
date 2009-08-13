(ns time-hull
  (:use convex-hull)
  (:import [javax.vecmath Vector2d]))

(def r (java.util.Random.))
(defn rands [] (repeatedly #(.nextGaussian r)))
(defn points [] (vec (take 400000 (partition 2 (rands)))))
(let [hull-points (time (hull (points)))]
  (printf "Points: %d\n" (count hull-points))
  (doseq [x hull-points] (println x)))

(defn points [n]
  (make-array Vector2d n))

(defmacro point [x y]
  `(new Vector2d (float ~x) (float ~y)))

(defmacro point-min [p1 p2]
  `(let [v1# ~v1
	 v2# ~v2]
     (cond 
       (< (.x v1#)))))

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

(defn get-point [points n]
  (let [n (int n)]
    [(aget (floats (:x points)) n) (aget (floats (:y points)) n)]))

(defmacro get-point [points idx]
  `[(aget (~'floats (:x ~points)) (int ~idx))
    (aget (~'floats (:y ~points)) (int ~idx))])

(comment
  (def my-points (points 400000))

  ;; will be very slow of course
  (time (vec (:x my-points)))

  ;; 100ms since we have create the points here
  (do
    (set! *warn-on-reflection* 1)
    (time
     (dotimes [x 1000000]
       (sub (point 5 3) (point 1 1)))))

  (def p1 (point 5 3))
  (def p2 (point 1 1))

  ;; ~20ms
  (do
    (set! *warn-on-reflection* 1)
    (time
     (dotimes [x 1000000]
       (add p1 p2))))

  ;; weird macro version takes ~60ms
  (dotimes [x 1000000] (sub (vec2d 5 3) (vec2d 2 1)))

  ;; ~20ms
  (let [xs (floats (:x my-points))
	ys (floats (:y my-points))]
    (time
     (dotimes [x 400000]
       (aget xs (int 0))
       (aget ys (int 0)))))
)
