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

(type (new Vector2d 0 0))

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

  ;; using get-point
  ;; ~50 ms
  (time
   (dotimes [x 400000]
     (get-point my-points 0)))

  ;; weird macro version takes ~60ms

  ;; ~20ms
  (let [xs (floats (:x my-points))
	ys (floats (:y my-points))]
    (time
     (dotimes [x 400000]
       (aget xs (int 0))
       (aget ys (int 0)))))
)
