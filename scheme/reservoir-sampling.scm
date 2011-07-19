PLT

(require srfi/27)
(require srfi/41)

(define (random-sample size input)
  (let ((first-part (stream-take size input))
        (second-part (stream-drop size input))
        (pool (make-vector size)))
    (stream-for-each (match-lambda ((list i val)
                                    (vector-set! pool i val)))
                     (stream-zip (stream-from 0) first-part))
    (stream-for-each (match-lambda ((list i val)
                                    (let ((random-index (random-integer i)))
                                      (when (< random-index size)
                                        (vector-set! pool random-index val)))))
                     (stream-zip (stream-from size) second-part))
    (vector->list pool)))


CLOJURE

(defn random-sample [sample-size items]
  (if (= sample-size 0) []
      (let [[sample remaining-items] (split-at sample-size items)]
        (second (reduce (fn [[num sample] item]
                          (let [index (rand-int num)]
                            (if (< index sample-size)
                              [(inc num) (assoc sample index item)]
                              [(inc num) sample])))
                        [sample-size (vec sample)]
                        remaining-items)))))
