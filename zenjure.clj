;;
;; zenjure.games
;;
(ns zenjure.games)

(defn regex3 []
  {:welcome-msg (str "You are playing the regex3 game. Koans are simple 3-"
                     "letter strings which may only consist of a combination "
                     "of the letters a, b and c. Please state the rule as a "
                     "regular expressions in the form /rule/.")
   :parse-koan #(re-matches #"^[abc]{3}$" %)
   :parse-rule #(let [mat (re-matches #"^/(.{0,60})/$" %)]
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

;;
;; zenjure.core
;;
(ns zenjure.core)

(defn equal-rule? [z r1 r2]
  (let [t (:true? z) k ((:generate-koans z))]
    (every? #(= (t r1 %) (t r2 %)) k)))



;;
;; zenjure.console
;;
(ns zenjure.console (:refer zenjure.core))

(defn print-cache [cache]
  (println "True:\t" (for [[k s] cache :when (true? s)] k))
  (println "False:\t" (for [[k s] cache :when (false? s)] k)))

(defn print-add-koan [z cache rule koan]
  (let [ncache (assoc cache koan ((:true? z) rule koan))]
    (println (ncache koan)) ncache))

(defn console-line [z r cache]
  (let [input (do (print ">>> ") (flush) (read-line))
        koan ((:parse-koan z) input)
        rule ((:parse-rule z) input)]
    (cond
      (= input "cache") (do (print-cache cache) (recur z r cache))
      (not(nil? koan)) (recur z r (print-add-koan z cache r koan))
      (not(nil? rule))
      (let [contra (filter (fn [[k v]] (not (= v ((:true? z) rule k)))) cache)]
        (if (empty? contra)
          (if (equal-rule? z r rule)
            (println "Congrats, you reached enlightment! The rule is" r)
            (do (println "Yea, might be the rule, except it isn't.") (recur z r cache)))
          (do (println "Can't be it, because" rule "contradicts" contra) (recur z r cache))))
      true (recur z r cache))))

(defn console-game [z]
  (println (str "Zenjure single-player console game\n\n" (:welcome-msg z)))
  (loop [lib (:library z)]
    (let [[rule cache] (first lib)]
      (println "\nGuess the rule from the following examples.")
      (print-cache cache)
      (console-line z rule cache))
    (recur (rest lib))))

(console-game (zenjure.games/regex3))

