;;
;; zenjure.games.clojnum
;;
(ns zenjure.games.clojnum
  (:use zenjure.core)
  (:use clojure.test)
  (:use net.licenser.sandbox))

(defn parse-koan [s]
  (let [mat (re-find #"\[([0-9])[, ]+([0-9])[, ]+([0-9])\]" s)]
    (if mat (map int (rest mat)))))

(defn parse-rule [s] 
  (try 
    ((stringify-sandbox (new-sandbox-compiler)) s 'k)
    (catch Exception e nil)))

(defn init []
  {:welcome-msg (str "You are playing the clojnum game. Koans are triples "
                     "of the integers 0-9 and are represented by a string, "
                     "e.g. [1 3 4]. A rule is described by Clojure code "
                     "with the koan bound to the name 'k'. For example, "
                     "(= (first k)) holds for the above koan.")
   :parse-koan parse-koan
   :parse-rule parse-rule
   :true? (fn [r k] (r {} k))
   :generate-koans #(for [x (range 10) y (range 10) z (range 10)] [x y z])
   :library (shuffle (seq [
                           [#"^a"      {"abc" true, "cba" false}]
                           [#"^b"      {"bcb" true, "aba" false}]
                           [#"^c"      {"ccb" true, "bba" false}]
                           [#"aaa"     {"aaa" true, "aba" false}]
                           [#"[ab]{3}" {"aba" true, "cba" false}]
                           [#"c.c"     {"ccc" true, "abc" false}]
                           ]))
   })

;(defn valid-koan? [k]
;  (and (vector? k) 
;       (= (count k) 3) 
;       (every? #(and (integer? %) (< 0 %) (>= 9 %)) k)))
;
;(defn valid-rule? [r]
;  true)
;
;(defn is-true? [r k] (r {} k))

;(deftest rule-equalities 
;         (let [z (init)]
;           (is (test-rule-equality z "true" "(= 1 1)"))
;           ))

(deftest koan-truth
         (let [z (init)]
           (is (test-koan-true z "/^a/" "abc"))
           (is (test-koan-true z "/a/" "bba"))
           (is (test-koan-true z "/[^c]$/" "cba"))
           ))

(deftest koan-parsing
         (is (= (parse-koan "[1 2 3]") [1 2 3]))
         (is (= (parse-koan "[3,2, 1]") [3 2 1]))
         (is (nil? (parse-koan "[11 2 3]")))
         (is (nil? (parse-koan "[1 2 3")))
         (is (nil? (parse-koan "(vec '(1 2 3))")))
         (is (nil? (parse-koan "[1 2 (str \"3\")]")))
         (is (nil? (parse-koan "[1 2]")))
         (is (nil? (parse-koan "")))
         )

;(deftest rules
;         (is (nil? (parse-rule "(")))
;         (is (nil? (parse-rule "")))
;         (is (nil? (parse-rule "#=(ls -al)")))
;         (is (is-true? (parse-rule "(< 2 (first k))") 
;                       [3 3 3]))
;         (is (not (is-true? (parse-rule "(< 2 (first k))") 
;                            [1 1 1])))
;         (is (is-true? (parse-rule "(every? #(< 5 %) k)") [6 7 8]))
;         (is (is-true? (parse-rule "(= 9 (reduce + k))") [2 3 4]))
;          )

