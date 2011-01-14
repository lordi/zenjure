;; regex part
(defn regex3-game []
  {:welcome-msg (str "You are playing the regex3 game. Koans are simple 3-"
                     "letter strings which may only consist of a combination "
                     "of the letters a, b and c. Please state the rule as a "
                     "regular expressions in the form /rule/.")
   :parse-koan #(re-matches #"^[abc]{3}$" %)
   :parse-rule #(let [mat (re-matches #"^/(.{0,60})/$" %)]
                  (if mat (re-pattern (second mat))))
   :true? (fn [rule koan] (not (empty? (re-find rule koan))))
   :generate-koans #(for [x "abc" y "abc" z "abc"] (str x y z))
   :library [
             [#"^a"      {"abc" true, "cba" false}]
             [#"^b"      {"bcb" true, "aba" false}]
             [#"^c"      {"ccb" true, "bba" false}]
             [#"aaa"     {"aaa" true, "aba" false}]
             [#"[ab]{3}" {"aba" true, "cba" false}]
             [#"c.c"     {"ccc" true, "abc" false}]
             ]
   })

;; zenjure part
(defn validate-library [zj] 
  (let [invalid-entries (filter 
                          (fn [[r, ex]] (not (empty? (filter (fn [[k v]] (not (= v ((zj :true?) r k)))) ex))))
                          (zj :library))]
    (if (not (empty? invalid-entries))
      (println "Warning: Invalid examples in the library:" invalid-entries))))

;; console part
(defn console-line [zj r cache]
  (let [input (do (print ">>> ") (flush) (read-line))
        koan ((zj :parse-koan) input)
        rule ((zj :parse-rule) input)]
    (if koan
      (let [ncache (assoc cache koan ((zj :true?) r koan))]
        (println (ncache koan))
        (recur zj r ncache))
      (if rule
        (let [contradict (filter (fn [[k v]] (not (= v ((zj :true?) rule k)))) cache)]
          (if (empty? contradict)
            (if (empty? (filter #(not (= ((zj :true?) r %) ((zj :true?) rule %))) ((zj :generate-koans))))
              (println "Congrats, you reached enlightment! The rule is" r)
              (do (println "Yea, might be the rule, except it isn't.") (recur zj r cache)))
            (do (println "This can't be the rule, because it contradicts" contradict) (recur zj r cache))))
        (recur zj r cache)))))

(defn console-game [zj]
  (println (str "Zenjure single-player console game\n\n" (zj :welcome-msg) "\n"))
  (validate-library zj)
  (let [[rule cache] (second (zj :library))] ; future: (rand-nth (zj :library))
    (println "Guess the rule from the examples" cache)
    (console-line zj rule cache)))

;; main part
(console-game (regex3-game))
