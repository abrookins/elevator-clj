(ns elevator-clj.core
  (:require [clojure.data.priority-map :as priority-map :refer [priority-map]]
            [clojure.set :as cset :refer [difference]]))

(def calls (atom (priority-map)))

(def cars (atom {}))

(def floors #{})

(def commitments (atom {}))

(def movements (atom []))

(defn abs
  "Absolute value of a number."
  [n]
  (max n (- n)))

(defn uncommitted-calls
  "Calls that are uncommitted."
  []
  (difference
    (set (keys commitments))
    (set (keys calls))))

(defn idle-cars
  "Cars currently idle."
  []
  (filter
    (fn [car] (= (get car :status) :idle))))

(defn move 
  "Move a car in a direction."
  [car direction]
  (let [floor (+ (get (last movements) :floor) 1)]
    (if (contains? floors floor)
      (if (contains? (uncommitted-calls) floor)
        (hash-set commitments '(floor direction) car))
        (conj movements '(floor direction (java.time.LocalDateTime/now))))))

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
    (if (= (get commitments (floor direction)) car)
      (stop car floor))))

(defn distance-to-floor
  "The distance from a car to a floor."
  [floor1 floor2]
    (abs (- floor1 floor2)))

(defn candidate-distances
  "The distance from car candidates to a target floor."
  [candidates floor direction]
  (let [distances
        (map
          (fn [[idx car]]
            (let [status (:status car)
                  current-floor (:floor car)
                  current-direction (:direction car)]
              (if (= status :idle)
                [(distance-to-floor current-floor floor) idx]
                (if (= direction current-direction)
                  [(distance-to-floor current-floor floor) idx]))))
        (seq candidates))]
    (into {} distances)))

(defn closest-car
  "The closest car to a floor and direction."
  [candidates floor direction]
  (let [distances (candidate-distances candidates floor direction)
        closest (first (sort (keys distances)))]
    (get distances closest)))

(defn commit-car
  "Commit a car to a call."
  [car floor direction]
  (hash-set commitments '(floor direction) car))

; TODO: Floor and direction appear together a lot; are they a value?
(defn call-processor
  "Turn calls into commitments."
  [key watched old-state new-state]
  (let [floor (get new-state :floor)
        direction (get new-state :direction)
        call-key #{floor direction}
        idlers (idle-cars)
        num-idlers (count idlers)]
    (if-not (contains? commitments call-key)
      (when (= num-idlers 1)
        (commit-car (first idlers) floor direction)
      (when (> num-idlers 1)
        (commit-car (closest-car idlers floor direction) floor direction)
      (when (= num-idlers 0)
        (commit-car (closest-car @cars floor direction) floor direction)))))))

(defn commitment-processor
  "Turn commitments into movements." 
  [key watched old-state new-state]
  (println (last new-state)))

(defn movement-reporter
  "Report car movements to standard out."
  [key watched old-state new-state]
  (println (last new-state)))

(defn call-reporter
  "Report calls to standard out."
  [key watched old-state new-state]
  (println (last new-state)))
