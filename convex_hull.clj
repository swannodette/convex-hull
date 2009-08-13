(ns convex-hull
  (:use clojure.contrib.math))

(defn quadrant-one-pseudo-angle [dx dy]
  (let [dx (float dx)
	dy (float dy)]
   (/ dx (+ dy dx))))

(defn pseudo-angle [[dx dy]]
  (let [dx (float dx)
	dy (float dy)]
      (cond
	(and (= dx 0) (= dy 0))	  0
	(and (>= dx 0) (> dy 0))  (quadrant-one-pseudo-angle dx dy)
	(and (> dx 0) (<= dy 0))  (+ 1 (quadrant-one-pseudo-angle (abs dy) dx))
	(and (<= dx 0) (< dy 0))  (+ 2 (quadrant-one-pseudo-angle (abs dx) (abs dy)))
	(and (< dx 0) (>= dy 0))  (+ 3 (quadrant-one-pseudo-angle dy (abs dx)))
	:else nil)))

(comment
  (time
   (dotimes [x 1000000]
     (pseudo-angle [5 5])))

  (time
   (dotimes [x 1000000]
     (quadrant-one-pseudo-angle 5 5)))

  (time
   (let [p1 [5 5]
	 p2 [10 10]]
    (dotimes [x 1000000]
      (delta-point p1 p2))))

  (delta-point [5 5] [10 10])

  (angle-and-point [5 5] [1 1])

  (time
   (let [p1 [5 5]
	 p2 [10 10]]
    (dotimes [x 1000000]
      (angle-and-point p1 p2))))
 )

(defn point-min [[x1 y1 :as p1] [x2 y2 :as p2]]
  (let [x1 (float x1)
	y1 (float y1)
	x2 (float x2)
	y2 (float y2)]
    (cond
      (< x1 x2) p1
      (= x1 x2) (if (< y1 y2) p1 p2)
      :else     p2)))

(defn find-min-point [points]
  (reduce point-min points))

(defn delta-point [[x1 y1] [x2 y2]]
  (let [x1 (float x1)
	y1 (float y1)
	x2 (float x2)
	y2 (float y2)]
    [(- x1 x2) (- y1 y2)]))

(defn angle-and-point [point base]
  [(pseudo-angle (delta-point point base)) point])

(defn min-angle-and-point [ap1 ap2]
  (if (< (float (first ap1)) (float (first ap2))) ap1 ap2))

(defn find-point-with-least-angle-from [base angle points]
  (let [angle (float angle)]
    (reduce min-angle-and-point
	    (remove
	     #(< (first %) angle)
	     (map #(angle-and-point % base)
		  (remove
		   (fn [p] (= base p))
		   points))))))

(defn hull [points]
  (println "Start")
  (let [starting-point (find-min-point points)]
    (println starting-point)
    (loop [hull-list [starting-point] angle (float 0) last-point starting-point]
      (let [[angle next-point] (find-point-with-least-angle-from last-point angle points)]
        (if (= next-point (first hull-list))
          hull-list
          (recur (conj hull-list next-point) (float angle) next-point))))))