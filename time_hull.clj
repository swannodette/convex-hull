(ns time-hull
  (:use convex-hull)
  (:import [javax.vecmath Vector2d])
  (:import TimeJarvisMarch))

(set! *warn-on-reflection* 1)

;; 2s - 2.5s
(def my-points (make-points 400000 rand-point))
(let [hull-points (time (hull my-points))]
  (printf "Points: %d\n" (count hull-points))
  (doseq [x hull-points] (println x)))

;; tho why bother when you can just do this? ;)
(.test (new TimeJarvisMarch))

(comment
  ;; 1s?
  (time
   (do
     (set! *warn-on-reflection* 1)
     (dotimes [x 20]
       (areduce #^"[Ljavax.vecmath.Vector2d;" my-points i #^Vector2d result (point 0 0)
		(let [#^Vector2d p (aget my-points i)]
		 (add result p))))))

  (def my-floats (make-array (. Float TYPE) 400000))

  ;; 2000ms, type hint in macro doesn't not work
  (time
   (do
     (set! *warn-on-reflection* 1)
     (dotimes [x 20]
       (areduce #^floats my-floats i result (float 1)
		(+ result (float (aget my-floats i)))))))

  ;; 52ms
  (time
   (do
     (set! *warn-on-reflection* 1)
     (dotimes [x 20]
       (let [#^floats my-floats my-floats]
	 (loop [i (int 0) result (float 1)]
	   (if (< i (alength my-floats))
	     (recur (unchecked-inc i) (+ result (float (aget my-floats i))))
	     result))))))

  ;; 53ms
  (time
   (do
     (set! *warn-on-reflection* 1)
     (let [#^floats my-floats my-floats]
       (dotimes [x 20]
	 (areduce my-floats i result (float 1)
		  (+ result (float (aget my-floats i))))))))

  ;; 56ms
  (time
   (do
     (set! *warn-on-reflection* 1)
     (let [my-floats (floats my-floats)]
       (dotimes [x 20]
	 (areduce my-floats i result (float 1)
		  (+ result (float (aget my-floats i))))))))

  ;; 2000ms
  ;; this is because in the expansion my-floats directly referenced
  ;; instead of the type hinted let binding
  (time
   (do
     (set! *warn-on-reflection* 1)
     (dotimes [x 20]
       (areduce (floats my-floats) i result (float 1)
		(+ result (float (aget my-floats i)))))))

  ;; 66ms
  ;; a tiny bit of a hit for type hinting at the array access
  (time
   (do
     (set! *warn-on-reflection* 1)
     (dotimes [x 20]
       (areduce (floats my-floats) i result (float 1)
		(+ result (float (aget (floats my-floats) i)))))))

)