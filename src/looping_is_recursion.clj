(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp acc]
                 (if (zero? exp)
                   acc
                   (recur base (dec exp) (* acc base))
                   ))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq elem]
                 (if (empty? a-seq)
                   elem
                   (recur (rest a-seq) (first a-seq))
                   ))]
    (helper a-seq nil)))

(defn seq= [seq1 seq2]
  (let [helper
        (fn [seq1 seq2 check]
          (if (not check)
            false
            (if (empty? seq1)
              true
              (recur (rest seq1) (rest seq2) (== (first seq1) (first seq2))))
            )
          )]
    (helper seq1 seq2 (== (count seq1) (count seq2)))))

(defn find-first-index [pred a-seq]
  (loop [s a-seq
         idx -1
         found false]
    (if found
      idx
      (if (empty? s)
        nil
        (recur (rest s) (inc idx) (pred (first s)))
        )
      )
    ))

(defn avg [a-seq]
  (loop [s a-seq
         acc 0]
    (if (empty? s)
      (/ acc (count a-seq))
      (recur (rest s) (+ acc (first s))))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [s a-seq
           par #{}
           ]
      (if (empty? s)
        par
        (recur (rest s) (toggle par (first s)))))))

(defn fast-fibo [n]
  (if (or (zero? n) (== 1 n))
    n
    (loop [x n
           f0 1
           f1 0
           ]
      (if (== 1 x)
        f0
        (recur (dec x) (+ f0 f1) f0)))))

(defn cut-at-repetition [a-seq]
  ;; todo
  (loop [s a-seq
         r []
         t #{}
         ]
    (if (or (empty? s) (contains? t (first s)))
      r
      (recur (rest s) (conj r (first s)) (conj t (first s))))))
