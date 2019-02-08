(ns bakary-management.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(def inventory [{:code "VS5"
                 :name "Vegemite Scroll"
                 :pack-types [{:pack-type 3
                               :price 6.99
                               }
                              {:pack-type 5
                               :price 8.99
                               }
                              ]
                 }
                {:code "MB11"
                 :name "Blueberry Muffin"
                 :pack-types [{:pack-type 2
                               :price 9.95
                               }
                              {:pack-type 5
                               :price 16.95
                               }
                              {:pack-type 8
                               :price 24.95
                               }
                              ]
                 }
                {:code "CF"
                 :name "Croissant"
                 :pack-types [{:pack-type 3
                               :price 5.95
                               }
                              {:pack-type 5
                               :price 9.95
                               }
                              {:pack-type 9
                               :price 16.99
                               }
                              ]
                 }])


;;; Recursive function that checks if you can divide packets into parts
;;; Its takes number of items and pack types vector
;;; e.g to check 13 CF, get pack types associated with CF those are 9 5 3
;;; call this function with 13 [ 9 5 3] if it can devide that equally then it returns vector ortheswise map
(defn can-product-breakdown-equally 
  ([num-of-items pack-types]
   (can-product-breakdown-equally  num-of-items pack-types []))
  ([num-of-items pack-types result]
   (cond (empty? pack-types)
     {:err "Can't devide equally"}
     :else
     (do
       (let [pack-type (first pack-types)
             num-of-packs (quot num-of-items pack-type)
             num-of-remining-packs (rem num-of-items pack-type)
             ]
         (cond
           (= num-of-remining-packs 0)
           (conj result {:pack-type pack-type :num-of-packs num-of-packs})
           (< num-of-items pack-type)
           {:err "Can't devide equally"}
           :else
           (can-product-breakdown-equally  
            num-of-remining-packs 
            (rest pack-types) 
            (conj result {:pack-type pack-type :num-of-packs num-of-packs}))
           ))))))


;; Function checks if product code in is in the iventory if its exists then it returns product code 
;; otherwise returns nil
(defn check-product-in-inventory [product-code inventory]
  (if (some (fn [product] (= (:code product) product-code)) inventory)
    product-code
    nil
    ) )

(defn product-types-from-inventory [product-code inventory]
   ;; (map #(:pack-type %) 
   ;;     (:pack-types  
   ;;      (reduce #()   
   ;;              (filter 
   ;;               (fn [product] (= (:code product) product-code)) inventory))))
  (->>
   (filter (fn [product] (= (:code product) product-code)) inventory)
   (reduce #())
   (:pack-types)
   (map #(:pack-type %))
   (into [])
   )
  )

(defn product-type-permutations [product-types]
  (->>
   (combo/permutations product-types)
   (map #(into [] %))
   (into [])
   (sort)
   (reverse)
   ) 
  )
  
(defn product-type-price-list [product-code inventory]
  (->>
   (filter (fn [product] (= (:code product) product-code)) inventory)
   (reduce #())
   (:pack-types)
   )
  )

(defn price-for-each-product-type [product-type pack-type-price-list]
  (->>
   (filter (fn [{pack-type :pack-type price :price }] (= pack-type product-type)) pack-type-price-list)
   (into {})
   (:price)
   )
  )

(defn calculate-price-for-items [pack-type-price-list can-product-brekdown-equally-result]
  (map 
   (fn [{pack-type :pack-type num-of-packs :num-of-packs}] {:pack-type pack-type
                                                            :num-of-packs num-of-packs
                                                            :individual-price (price-for-each-product-type pack-type pack-type-price-list)
                                                            :total-price (* num-of-packs (price-for-each-product-type pack-type pack-type-price-list))
                                                            } ) 
   can-product-brekdown-equally-result)
  
  )

;;;; Inpure functions 

(declare start)
(defn user-input
  "User input from command line"
  ([] (user-input ""))
  ([default]
     (let [input (clojure.string/trim (read-line))]
       (if (empty? input)
         default
         (clojure.string/upper-case input)))))


(defn prompt
  []
  (println "Place a number of items followed by the product")
  (let [command (user-input)]
    command))

(defn convert-str-to-int
  [number-string]
  (try (Integer/parseInt number-string)
    (catch Exception e nil)))

(defn valid-command 
  [command inventory] 
  (let [[num-of-items product-code] (clojure.string/split command #" ")]
    [(convert-str-to-int num-of-items) (check-product-in-inventory product-code inventory)]
    ))


(defn print-result-on-screen [pack-type-total-price-result inventory]
  (println "*********************************")
  (doseq [{pack-type :pack-type  num-of-packs :num-of-packs total-price :total-price individual-price :individual-price } pack-type-total-price-result]
    (println num-of-packs,  "*",  pack-type, "(", individual-price, "$)", "=", total-price)
    )
  (start inventory)
  )

(defn price-for-products 
  [product-code can-product-brekdown-equally-result inventory]
  (let [pack-type-price-list (product-type-price-list product-code inventory)
        pack-type-total-price-result (calculate-price-for-items pack-type-price-list can-product-brekdown-equally-result)
        ]
    (print-result-on-screen pack-type-total-price-result inventory) 
    )
  )

;;; 
;;; Algo how this program works
;;; Get user Input, from which get pack types associated with it
;;; Calculate permutations of pack types and sort 
;;; For each pack type see you can breakdown num of items
;;; When you are able to breakdown items, stop print the result
;;; Continue to start


(defn process-command 
  [inventory num-of-items product-code]
  (let [product-types (product-types-from-inventory product-code inventory)
        sorted-product-type-permutations (product-type-permutations product-types)
        ]
    ;; (doseq [pack-types sorted-product-type-permutations]
    ;;   (when (vector? (can-product-breakdown-equally num-of-items pack-types))
    ;;     (println "now calculate")
    ;;     )
    ;;   )
    
    (loop [permutation  sorted-product-type-permutations]
      (if permutation
        (let [pack-types (first permutation)
              can-product-brekdown-equally-result (can-product-breakdown-equally num-of-items pack-types)
              ]
          (if (vector? can-product-brekdown-equally-result)
            (price-for-products product-code can-product-brekdown-equally-result inventory)
            (recur (next permutation) )
            )
          )
        ))
   )
  (println "Unfortunetly we can't process your request at this point of time")
  (start inventory)
  )

(defn start
  [inventory]
  (let [[num-of-items product-code]  (valid-command (prompt) inventory)]
    (cond
      (or (nil? num-of-items) (nil?  product-code))
      (do
        (println "Please enter valid command")
        (start inventory)
        )
      :else
      (process-command inventory  num-of-items product-code)
      )
    ))


(defn -main
  [& args]
  (println "Press CTRL-C to exit")
  (start inventory)
  )
