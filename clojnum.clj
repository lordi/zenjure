;; user=> (map class (read-string "[ 1 2 3]") )
;; (java.lang.Integer java.lang.Integer java.lang.Integer)
;; user=> (map class (read-string "(vec ( 1 2 3))") )
;; (clojure.lang.Symbol clojure.lang.PersistentList)

(ns zenjure.games.clojnum
  (:use clojure.test))

(defn valid-koan? [k]
  (and (vector? k) 
       (= (count k) 3) 
       (every? #(and (integer? %) (< 0 %) (>= 9 %)) k)))

(defn valid-rule? [r]
  true)

(defn parse-koan [s] 
  (try 
    (binding [*read-eval* false]
      (let [k (read-string s)] (cond (valid-koan? k) k)))
    (catch Exception e nil)))

(defn is-true? [r k]
  (println r)
  (println (eval r))
  ((eval r) k))
;  (r k))

(defn parse-rule [s] 
  (try 
    (binding [*read-eval* false]
      (let [r (read-string s)] (cond (valid-rule? r) r)))
    (catch Exception e nil)))

(deftest koan-parsing
         (is (= (parse-koan "[1 2 3]") [1 2 3]))
         (is (nil? (parse-koan "[11 2 3]")))
         (is (nil? (parse-koan "[1 2 3")))
         (is (nil? (parse-koan "(vec '(1 2 3))")))
         (is (nil? (parse-koan "[1 2 (str \"3\")]")))
         (is (nil? (parse-koan "[1 2]")))
         (is (nil? (parse-koan "")))
         )

(deftest rule-parsing
         (is (= (parse-rule "(> 1 2)") '(> 1 2)))
         (is (= (parse-rule "(> 1 k)") '(> 1 k)))
         (is (nil? (parse-rule "(")))
         (is (nil? (parse-rule "")))
         (is (nil? (parse-rule "#=(ls -al)")))
         (is (is-true? (parse-rule "#(< 1 (last %))") [1 2 3]))
         )

(run-tests 'zenjure.games.clojnum)
