(ns time-hull
  (:use convex-hull)
  (:import [javax.vecmath Vector2d])
  (:import TimeJarvisMarch)
  (:import Points))

(set! *warn-on-reflection* 1)

;; 2s - 2.5s
(def #^"[Ljavax.vecmath.Vector2d;" my-points (make-points 400000 rand-point))
(let [hull-points (time (hull my-points))]
  (printf "Points: %d\n" (count hull-points))
  (doseq [x hull-points] (println x)))

;; tho why bother when you can just do this? ;)
(.test (new TimeJarvisMarch))

(comment
  ;; CORRECT - 230ms
  ;; note we don't need to type hint (point 0 0)
  ;; this is not a function it's a inlined Java form
  ; my-points symbol is already typed at creation time
  (time
   (do
     (set! *warn-on-reflection* 1)
     (dotimes [x 20]
       (areduce my-points i result (point 0 0)
		(let [p (aget my-points i)]
		  (add result p))))))

  (def my-floats (make-array (. Float TYPE) 400000))

  ;; 2000ms, type hint in macro doesn't not work
  (time
   (do
     (set! *warn-on-reflection* 1)
     (dotimes [x 20]
       (areduce my-floats i result (float 1)
		(+ result (float (aget my-floats i)))))))

  ;; 45ms, the CORRECT way to do things
  (time
   (do
     (set! *warn-on-reflection* 1)
     (let [my-floats (floats my-floats)
	   init      (float 1)]
       (dotimes [x 20]
	 (areduce my-floats i result init
		  (+ result (aget my-floats i)))))))

  ;; wrong, will expand to (aget my-floats i) instead of the dynamic
  ;; loop-recur binding
  ;; 2000ms
  (time
   (do
     (set! *warn-on-reflection* 1)
     (dotimes [x 20]
       (areduce (floats my-floats) i result (float 1)
		(+ result (float (aget my-floats i)))))))

  ;; wrong fixes the above issue but we're taking a hit from the cast
  ;; 66ms
  (time
   (do
     (set! *warn-on-reflection* 1)
     (dotimes [x 20]
       (areduce (floats my-floats) i result (float 1)
		(+ result (float (aget (floats my-floats) i)))))))

)