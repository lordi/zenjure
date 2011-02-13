;;
;; zenjure.games.regex3
;;
(ns zenjure.games.regex3
  (:use zenjure.core)
  (:use clojure.test))

(defn init []
  {:welcome-msg (str "You are playing the regex3 game. Koans are simple 3-"
                     "letter strings which may only consist of a combination "
                     "of the letters a, b and c. Please state the rule as a "
                     "regular expressions in the form /rule/.")
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

(deftest test-regex3 
   (let [z (zenjure.games.regex3/init)]
     (is (test-rule-equality z "/^a/" "  /a../stuff"))
     (is (test-rule-equality z "/aaa/" "/[^bc]{3}/  "))
     (is (test-koan-true z "/^a/" "abc"))
     (is (test-koan-true z "/a/" "bba"))
     (is (test-koan-true z "/^a/" "abc"))
    ))

