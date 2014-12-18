(ns looping-is-recursion)

(defn power [base exp]
  (if (= base 0)
    0
    (loop [k exp acc 1]
      (if (= k 0)
        acc
        (recur (dec k) (* acc base))))))

(defn last-element [a-seq]
  (loop [xs a-seq]
    (if (empty? (rest xs))
      (first xs)
      (recur (rest xs)))))

(defn seq= [seq1 seq2]
  (loop [s1 seq1 s2 seq2]
    (cond
     (and (empty? s1) (empty? s2)) true
     (or (empty? s1) (empty? s2)) false
     :else (if (= (first s1) (first s2))
             (recur (rest s1) (rest s2))
             false))))

(defn find-first-index [pred a-seq]
  (loop [xs a-seq i 0]
    (when-not (empty? xs)
      (if (pred (first xs))
        i
        (recur (rest xs) (inc i))))))

(defn avg [a-seq]
  (loop [xs a-seq sum 0 n 0]
    (if (empty? xs)
      (/ sum n)
      (recur (rest xs) (+ sum (first xs)) (inc n)))))

(defn- toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [xs a-seq acc #{}]
    (if (empty? xs)
      acc
      (recur (rest xs) (toggle acc (first xs))))))

(defn fast-fibo [n]
  (loop [a 0 b 1 k 0]
    (if (= k n)
      a
      (recur b (+ a b) (inc k)))))

(defn cut-at-repetition [a-seq]
  (loop [xs a-seq v [] s #{}]
    (if (empty? xs)
      v
      (let [x (first xs)
            [v' s'] (if (contains? s x)
                      [v s]
                      [(conj v x) (conj s x)])]
        (recur (rest xs) v' s')))))

