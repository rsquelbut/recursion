(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(def not-nil? (complement nil?))

(defn singleton? [coll]
  (let [head (first coll)
        tail (rest coll)]
    (and
      (not-nil? head)
      (empty? tail))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (let [_ (first coll)
                tail (rest coll)]
            (my-last tail))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (let [head (first a-seq)
                tail (rest a-seq)]
            (max head (max-element tail)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [head (first a-seq)
          tail (rest a-seq)]
      (seq-max head (longest-sequence tail)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [head (first a-seq)
          tail (rest a-seq)]
      (if (pred? head)
        (cons head (my-filter pred? tail))
        (my-filter pred? tail)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (if (empty? a-seq)
      '()
      (if (pred? head)
        (cons head (my-take-while pred? tail))
        '()))))

(defn my-drop-while [pred? a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    (if (empty? a-seq)
      '()
      (if (pred? head)
        (my-drop-while pred? tail)
        (seq a-seq)))))

(defn seq= [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)
        a-tail (rest a-seq)
        b-tail (rest b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) true
      (not (= a b)) false
      :else (seq= a-tail b-tail))
    ))

(defn my-map [f seq-1 seq-2]
  (let [a (first seq-1)
        b (first seq-2)
        a-tail (rest seq-1)
        b-tail (rest seq-2)]
    (if (or (empty? seq-1) (empty? seq-2))
      '()
      (cons (f a b) (my-map f a-tail b-tail)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (let [n-1 (dec n)
        n-2 (dec n-1)]
    (if (< n 2)
      n
      (+ (fib n-1) (fib n-2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (or (zero? how-many-times) (neg-int? how-many-times)) '()
    (= 1 how-many-times) [what-to-repeat]
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [n (dec up-to)]
    (if (neg? n)
      '()
      (cons n (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (inits (drop-last a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [val (first a-seq)
          updated (assoc freqs val (if (contains? freqs val)
                                     (inc (freqs val))
                                     1))]
      (my-frequencies-helper updated (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [result a-map]
  (if (empty? a-map)
    result
    (let [[value freq] (first a-map)]
      (concat (repeat freq value)
              (un-frequencies-helper result (rest a-map))))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  (let [stop-it-now (or (empty? coll)
                        (not (pos? n)))]
    (if stop-it-now
      '()
      (cons (first coll) (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (let [stop-it-now (or (empty? coll)
                        (not (pos? n)))]
    (if stop-it-now
      coll
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [threshold (int (/ (count a-seq) 2))]
    [(my-take threshold a-seq) (my-drop threshold a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [[a-head & a-tail] a-seq
        [b-head & b-tail] b-seq]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< a-head b-head) (cons a-head (seq-merge a-tail b-seq))
      :else (cons b-head (seq-merge a-seq b-tail)))))


(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) '()
    (= 1 (count a-seq)) (seq a-seq)
    :else (apply seq-merge (map merge-sort (halve a-seq))))
  )

(merge-sort [])                                             ;=> ()
(merge-sort [1])                                            ;=> ()
(merge-sort [1 2])                                          ;=> ()
(merge-sort [1 2 3])                                        ;=> (1 2 3)
(merge-sort [5 3 4 17 2 100 1])                             ;=> (1 2 3 4 5 17 100)

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])