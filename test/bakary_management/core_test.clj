(ns bakary-management.core-test
  (:require [clojure.test :refer :all]
            [bakary-management.core :refer :all]))




(deftest can-product-breakdown-equally-succes
  (testing "Testing if prouduct can be breakdown successfully"
    (is (= (can-product-breakdown-equally 10 [5 2]) [{:pack-type 5, :num-of-packs 2}] )))
  )

(deftest can-product-breakdown-equally-failure
  (testing "Testing if prouduct can not be breakdown "
    (is (= (can-product-breakdown-equally 11 [5 2]) {:err "Can't devide equally"} )))
  )


(deftest test-product-in-inventory
  (testing "Testing product is in inventory"
    (is (= (check-product-in-inventory "CF" inventory) "CF" )))
  )
