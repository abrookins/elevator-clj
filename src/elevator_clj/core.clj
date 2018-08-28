(ns elevator-clj.core
  (:require clojure.data.priority-map clojure.set))

;; TODO: spec!

;; [floor, direction] => time
(def calls (atom (clojure.data.priority-map/priority-map)))

;; car ID => car
(def cars (atom {}))

;; #{integer}
(def floors #{})

;; [floor, direction] => car
(def commitments (atom {}))

;; [[floor direction car]]
(def movements (atom []))

(defn abs
  "Absolute value of a number."
  [n]
  (max n (- n)))

(defn uncommitted-calls
  "Calls that are uncommitted."
  []
  (clojure.set/difference
    (set (keys commitments))
    (set (keys calls))))

(defn filter-cars
  "Filter cars by a key-value comparison."
  [field value]
  (into {} (filter (fn [[_ car]] (= (get car field) value)) @cars)))

(defn idle-cars
  "Cars currently idle."
  []
  (filter-cars :status :idle))

(defn busy-cars
  "Cars currently busy."
  []
  (filter-cars :status :busy))

(defn move 
  "Move a car in a direction."
  [car direction]
  (let [[last-floor _ _] (last movements)
        floor (+ last-floor 1)]
    (if (contains? floors floor)
      (if (contains? (uncommitted-calls) [floor direction])
        (hash-set commitments [floor direction] car))
        (conj movements [floor direction (java.time.LocalDateTime/now)]))))

(defn stop
  "Stop a car at a floor."
  [car floor]
  (let [car (get @cars car)]
    (do
      (hash-set car :status :stopped)
      (hash-set car :floor floor))))
      
(defn movements-processor
  "Respond to car movements."
  [key movements old-movements new-movements]
  (let [[floor direction car] (last new-movements)]
    (if (= (get commitments [floor direction]) car)
      (stop car floor))))

(defn distance-to-floor
  "The distance from a car to a floor."
  [floor1 floor2]
    (abs (- floor1 floor2)))

(defn candidate-distances
  "The distance from candidate cars to a target floor."
  [candidates floor]
  (map
    (fn [[idx car]]
      [(distance-to-floor (:floor car) floor) idx])
    (seq candidates)))

(defn commitment-distances
  "The distance from the floors of commitments to a target floor."
  [floor]
  (map
    (fn [[commitment-floor car-idx]]
      [(distance-to-floor commitment-floor floor) car-idx])
    (seq @commitments)))

(defn closest-car
  "The closest car to a floor."
  [candidates floor]
  (let [distances (into {} (candidate-distances candidates floor))
        closest (first (sort (keys distances)))]
    (get distances closest)))

(defn closest-commitment
  "The closest commitment to a floor."
  [floor]
  (let [distances (into {} (commitment-distances floor))
        closest (first (sort (keys distances)))]
    (get distances closest)))

(defn busy-cars-in-path
  "Busy cars in whose path a floor lies."
  [floor direction]
  (filter some?
    (map
      (fn [idx car]
        (let [current-floor (:floor car)
              commitment-floor (get (clojure.set/map-invert @commitments) car)
              car-direction (:direction car)]
          (if
            (and (= direction car-direction)
                 (if (= direction :down)
                   (and (>= floor commitment-floor) (<= floor current-floor))
                   (and (<= floor commitment-floor) (>= floor current-floor))))
            idx))))))

(defn commit-car
  "Commit a car to a call."
  [car floor]
  (hash-set commitments floor car))

(defn call-car
  "Call a car."
  [floor direction]
  (hash-set calls [floor direction] (java.time.LocalDateTime/now)))

(defn call-processor
  "Turn calls into commitments."
  [key watched old-state new-state]
  (let [latest (last new-state)
        floor (get latest :floor)
        direction (get latest :direction)
        idlers (idle-cars)
        num-idlers (count idlers)
        busy-cars (busy-cars-in-path floor direction)
        num-busy-cars-in-path (count busy-cars)]
    (if-not (contains? commitments [floor direction])
      (cond
        (= num-idlers 1) (commit-car (first idlers) [direction floor])
        (> num-idlers 1) (commit-car (closest-car idlers floor) [direction floor])
        (and (= num-idlers 0) (> num-busy-cars-in-path 1)) (commit-car (first busy-cars-in-path) floor)
        (and (= num-idlers 0) (= num-busy-cars-in-path 0)) (commit-car (closest-commitment floor) floor)))))

(add-watch calls :call-processor call-processor)

(defn commitment-processor
  "Turn commitments into movements." 
  [key watched old-state new-state]
  (println key (last new-state)))

(defn movement-processor
  "Turn movements into stops." 
  [key watched old-state new-state]
  (println key (last new-state)))

(defn commitment-reporter
  "Report car commitments to standard out."
  [key watched old-state new-state]
  (println key (last new-state)))

(defn movement-reporter
  "Report car movements to standard out."
  [key watched old-state new-state]
  (println key (last new-state)))

(defn call-reporter
  "Report calls to standard out."
  [key watched old-state new-state]
  (println key (last new-state)))
