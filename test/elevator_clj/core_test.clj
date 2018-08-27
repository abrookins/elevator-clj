(ns elevator-clj.core-test
  (:require [clojure.test :refer :all]
            [elevator-clj.core :refer :all]))

(deftest test-call-processor
  (testing "call-processor commits first idle car")
    (do
      (reset! cars {1 {:floor 1 :status :idle}
                    2 {:floor 3 :status :idle}}))
      (let [commits @commitments]
        (is (= (closest-car cars 1) 1))))

(deftest test-closest-car
  (testing "closest-car finds the closest idle car"
    (do
      (reset! cars {1 {:floor 1 :status :idle}
                    2 {:floor 3 :status :idle}}))
      (let [cars @cars]
        (is (= (closest-car cars 1) 1)))))
