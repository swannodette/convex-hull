(ns time-hull
  (:use convex-hull)
  (:import [javax.vecmath Vector2d])
  (:import TimeJarvisMarch)
  (:import Points))

(set! *warn-on-reflection* 1)

(def #^"[Ljavax.vecmath.Vector2d;" my-points (make-points 400000 rand-point))
(let [hull-points (time (hull my-points))]
  (printf "Points: %d\n" (count hull-points))
  (doseq [x hull-points] (println x)))

(comment
  ;; takes 0.438 ms for 8000000 iterations, that's more like it
  ;; 356ms if we call .equals and create a point in each loop

  ;; 80ms if we don't create the vector
  ;; we should have a mutable vector we use to hold results
  ;; ~43ms if nothing happens
  ;; ~110ms with Vector2d .equals
  ;; ~150ms with Vector2d .equals and .sub
  ;; ~270ms with fn call to sub instead of .sub
  ;; ~300ms if using Vector2d .set and .sub
  ;; ~260ms with sub macro
  ;; ~500ms with call to pseudo-angle
  ;; ~450ms with pseudo-angle as macro
  (time
   (do
     (set! *warn-on-reflection* 1)
     (dotimes [x 20]
       (find-point-with-least-angle-from (point 0 0) 0 my-points))))

  ;; 20 ms
  (do
   (set! *warn-on-reflection* 1)
   (let [p1 (point 5 5)
	 p2 (point 1 1)]
     (time
      (do
	(set! *warn-on-reflection* 1)
	(dotimes [x 8000000]
	  (.sub p1 p2))))))

  ;; 9 ms
  (do
   (set! *warn-on-reflection* 1)
   (let [p (point 5 5)]
     (time
      (do
	(set! *warn-on-reflection* 1)
	(dotimes [x 8000000]
	  (.set p 0 0))))))

  ;; 13ms
  (do
    (set! *warn-on-reflection* 1)
    (let [p1 (point 5 5)
	  p2 (point 1 1)]
      (time
       (do
	 (set! *warn-on-reflection* 1)
	 (dotimes [x 8000000]
	   (.set p1 (.x p2) (.y p2)))))))

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
		  (sub result p))))))

  ;; wow you pay for vectors
  ;; 400ms vs. 70ms
  (time
   (do
     (set! *warn-on-reflection* 1)
     (let [n (atom 0)]
	 (dotimes [x 20]
	   (areduce my-points i result (point 0 0)
		    (let [p (aget my-points i)]
		      ))))))

  ;; 72ms to iterate 8 million times
  (time
   (do
     (set! *warn-on-reflection* 1)
     (dotimes [x 20]
       (areduce my-points i result (point 0 0)
		(aget my-points i)))))

  ;; BAD - make-points is a function, Clojure can't
  ;; know the return type
  (do
    (set! *warn-on-reflection* 1)
    (let [bad-points (make-points 400000 rand-point)]
     (time
      (dotimes [x 20]
	(areduce bad-points i result (point 0 0)
		 (let [p (aget bad-points i)]
		   (add result p)))))))

  (def my-floats (make-array (. Float TYPE) 400000))

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