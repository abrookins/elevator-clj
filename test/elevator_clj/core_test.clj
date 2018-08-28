(ns elevator-clj.core-test
  (:require [clojure.test :refer :all]
            [elevator-clj.core :refer :all]))

(deftest test-call-processor
  (testing "call-processor commits first idle car")
    (do
      (reset! cars {1 {:floor 1 :status :idle}
                    2 {:floor 3 :status :idle}})
      (hash-set calls [4 :down] (java.time.LocalDateTime/now)))
    (let [commits @commitments]
      (is (= (get @commitments [4 :down]) (get @cars 2)))))

(deftest test-closest-car
  (testing "closest-car finds the closest idle car"
    (do
      (reset! cars {1 {:floor 1 :status :idle}
                    2 {:floor 3 :status :idle}}))
    (let [cars @cars]
      (is (= (closest-car cars 1) 1)))))
