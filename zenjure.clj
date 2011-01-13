;; regex part
(defn parse-koan [s] (re-matches #"^[abc]{3}$" s))
(defn parse-rule [s]
  (let [mat (re-matches #"^/(.{0,60})/$" s)]
    (if mat (re-pattern (second mat)))))
(defn true-koan? [rule koan] (< 0 (count (re-find rule koan))))
(defn generate-koans [] (for [x "abc" y "abc" z "abc"] (str x y z)))
(def library [
              [#"^a" {"abc" true, "cba" false}]
              [#"^b" {"bcb" true, "aba" false}]
              [#"^c" {"ccb" true, "bba" false}]
              [#"aaa" {"aaa" true, "aba" false}]
              [#"[ab]{3}" {"aba" true, "cba" false}]
              [#"c.c" {"ccc" true, "abc" false}]
              ]
  )

;; zenjure part
(defn validate-library [] 
  (let [invalid-entries (filter 
                          (fn [[r, ex]] (not (empty? (filter (fn [[k v]] (not (= v (true-koan? r k)))) ex))))
                          library)]
    (if (not (empty? invalid-entries))
      (println "Warning: Invalid examples in the library:" invalid-entries))))

(defn console-line [r cache]
  (let [input (do (print ">>> ") (flush) (read-line))
        koan (parse-koan input)
        rule (parse-rule input)]
    (if koan
      (let [ncache (assoc cache koan (true-koan? r koan))]
        (println (ncache koan))
        (recur r ncache))
      (if rule
        (let [contradict (filter (fn [[k v]] (not (= v (true-koan? rule k)))) cache)]
          (if (empty? contradict)
            (if (empty? (filter #(not (= (true-koan? r %) (true-koan? rule %))) (generate-koans)))
              (println "Congrats, you reached enlightment! The rule is" r)
              (do (println "Yea, might be the rule, except it isn't.") (recur r cache)))
            (do (println "This can't be the rule, because it contradicts" contradict) (recur r cache))))
        (recur r cache)))))

(defn start-game [r cache]
  (println "Guess the rule from the examples" cache)
  (console-line r cache))

(validate-library)
(apply start-game (second library))
