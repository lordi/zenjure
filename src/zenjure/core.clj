;;
;; zenjure.core
;;
(ns zenjure.core)

(defn equal-rule? [z r1 r2]
  (let [t (:true? z) k ((:generate-koans z))]
    (every? #(= (t r1 %) (t r2 %)) k)))

(defn test-rule-equality [z rs1 rs2]
  (equal-rule? z ((:parse-rule z) rs1) ((:parse-rule z) rs2)))

(defn test-koan-true [z r k]
  ((:true? z) ((:parse-rule z) r) ((:parse-koan z) k)))

;
