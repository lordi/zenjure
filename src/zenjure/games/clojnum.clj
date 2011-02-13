;;
;; zenjure.games.clojnum
;;
(ns zenjure.games.clojnum
  (:use zenjure.core)
  (:use clojure.test))

(defn init []
  {:welcome-msg (str "You are playing the clojnum game. Koans are vectors of "
                     "the numbers 0-9 and are represented by a string, e.g. [1 3 4].")
   :parse-koan #(re-matches #"^[abc]{3}$" %)
   :parse-rule #(let [mat (re-find #"/(.{0,60})/" %)]
                  (if mat (re-pattern (second mat))))
   :true? (fn [rule koan] (not (empty? (re-find rule koan))))
   :generate-koans #(for [x "abc" y "abc" z "abc"] (str x y z))
   :library (shuffle (seq [
                           [#"^a"      {"abc" true, "cba" false}]
                           [#"^b"      {"bcb" true, "aba" false}]
                           [#"^c"      {"ccb" true, "bba" false}]
                           [#"aaa"     {"aaa" true, "aba" false}]
                           [#"[ab]{3}" {"aba" true, "cba" false}]
                           [#"c.c"     {"ccc" true, "abc" false}]
                           ]))
   })



(use 'clojure.test)
(use 'net.licenser.sandbox)

;;(def my-sandbox (stringify-sandbox (new-sandbox-compiler)))
;;(def my-call (my-sandbox "(rest [1 a 2])" 'a 'b))
;;(print (my-call {} 2 3))

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

(defn is-true? [r k] (r {} k))

(defn parse-rule [s] 
  (try 
    ((stringify-sandbox (new-sandbox-compiler)) s 'k)
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

(deftest rules
         (is (nil? (parse-rule "(")))
         (is (nil? (parse-rule "")))
         (is (nil? (parse-rule "#=(ls -al)")))
         (is (is-true? (parse-rule "(< 2 (first k))") 
                       [3 3 3]))
         (is (not (is-true? (parse-rule "(< 2 (first k))") 
                            [1 1 1])))
         (is (is-true? (parse-rule "(every? #(< 5 %) k)") [6 7 8]))
         (is (is-true? (parse-rule "(= 9 (reduce + k))") [2 3 4]))
         )

