(ns four-clj.core
  (:require [clojure.set]))

(def my-last
  #(-> %
       reverse
       first))

(def my-second-to-last
  #(-> %
       reverse
       second))

(def my-nth
  (fn [elems n]
    (loop [elems elems
           i 0]
      (if (= i n)
        (first elems)
        (recur (rest elems) (inc i))))))

(def my-count
  (fn [elems]
    (loop [elems elems
           counter 0]
      (if (empty? elems)
        counter
        (recur (rest elems) (inc counter))))))

(def my-reverse
  (fn [elems]
    (loop [elems elems
           reversed (empty elems)]
      (if (empty? elems)
        reversed
        (recur (rest elems) (cons (first elems) reversed))))))

(def my-sum
  #(reduce + %))

(def my-odd-only
  #(filter odd? %))

(def my-fibonacci
  #(take % (map first (iterate (fn [[a b]] [b (+ b a)]) [1 1]))))

(def my-palindrome?
  (fn [elems]
    (or (empty? elems)
        (and (= (first elems) (last elems))
             (recur (-> elems rest butlast))))))

(def my-flatten
  (fn flatten-me [elems]
    (if (empty? elems)
      elems
      (let [first-elem (first elems)
            other-elems (rest elems)]
        (if (coll? first-elem)
          (concat (flatten-me first-elem) (flatten-me other-elems))
          (cons first-elem (flatten-me other-elems)))))))

(def my-cap-only
  #(apply str (re-seq #"[A-Z]" %)))

(def my-dedupe
  (fn [elems]
    (loop [elems elems
           deduped []]
      (if (empty? elems)
        deduped
        (let [first-elem (first elems)
              other-elems (rest elems)]
          (recur other-elems (if (= first-elem (last deduped))
                               deduped
                               (conj deduped first-elem))))))))

(def my-pack
  #(partition-by identity %))

(def my-duplicate
  #(reduce concat (map (partial repeat 2) %)))

(def my-replicate
  #(reduce concat (map (partial repeat %2) %1)))

(def my-range
  (fn [start end]
    (take (java.lang.Math/abs (- end start)) (iterate inc start))))

(def my-max
  (fn [& elems]
    (reduce (fn [x y] (if (>= x y) x y)) elems)))

(def my-interleave
  #(reduce concat (map vector %1 %2)))

(def my-interpose
  (fn [sep elems]
    (butlast (reduce concat (map vector elems (repeat sep))))))

(def my-drop-every-nth
  (fn [elems n]
    (map second
         (filter (fn [[idx elem]] (not= (mod idx n) 0))
                 (map-indexed (fn [idx elem] [(inc idx) elem]) elems)))))

(def my-factorial
  (fn [n]
    (reduce * (take n (iterate inc 1)))))

(def my-reverse-interleave
  (fn [elems n]
    (apply map vector (partition n elems))))

(def my-rotate
  (fn [n elems]
    (let [len (count elems)]
      (take len (drop
                 (if (>= n 0) n (- len (rem (- n) len)))
                 (cycle elems))))))

(def my-flip-args
  (fn [f]
    (fn [& params] (apply f (reverse params)))))

(def my-split-at
  (fn [n elems]
    [(take n elems) (drop n elems)]))

(def my-split-by-type
  (fn [elems]
    (vals (group-by type elems))))

(def my-longest-increasing-sub-seq
  (fn [elems]
    (let [increasing (filter (fn [elems] (every? (fn [[x y]] (> y x)) elems))
                             (partition-by (fn [[x y]] (> y x)) (partition 2 1 elems)))
          longest (reduce (fn [x y] (if (> (count y) (count x)) y x)) [] increasing)]
      (concat (map first (butlast longest))
              (last longest)))))

(def my-partition
  (fn [n elems]
    (let [partitions (quot (count elems) n)]
      (map (fn [x] (->> elems (drop (* x n)) (take n))) (range partitions)))))

(def my-frequencies
  (fn [elems]
    (into {} (for [[k v] (group-by identity elems)] [k (count v)]))))

(def my-distinct
  (fn [elems]
    (map first
         (sort-by second
                  (map (fn [elem] [elem (.indexOf elems elem)])
                       (set elems))))))

(def my-comp
  (fn [& fs]
    (fn [& args]
      (let [[func & funcs] (reverse fs)]
        (reduce (fn [v f] (f v))
                (apply func args)
                funcs)))))

(def my-juxt
  (fn [& fs]
    (fn [& args]
      (map (fn [f] (apply f args)) fs))))

(def my-reductions
  (fn reductions-me
    ([f coll]
     (if (empty? coll)
       coll
       (reductions-me f (first coll) (rest coll))))
    ([f init coll]
     (cons init (lazy-seq (if (empty? coll)
                            nil
                            (reductions-me f (f init (first coll)) (rest coll))))))))

(def my-zipmap
  (fn [elems1 elems2]
    (into {} (map vector elems1 elems2))))

(def my-iterate
  (fn iterate-me [f x]
    (cons x (lazy-seq (iterate-me f (f x))))))

(def my-group-by
  (fn [f coll]
    (into {} (map (fn [[[k _] & pairs :as all-pairs]] [k (map second all-pairs)])
                  (partition-by first (sort (map #(vector (f %) %) coll)))))))

(def my-seq-type
  (fn [s]
    (let [seq-map {{} :map #{} :set}]
      (or (get seq-map (empty s))
          (if (= (first (conj (empty s) :a :b)) :a)
            :vector
            :list)))))

;; NOTE: Brute force
(def my-gcd
  (fn [a b]
    (let [get-cd (fn [n] (clojure.set/union #{1 n}
                                            (set (filter #(= (mod n %) 0) (range 2 n)))))
          a-cd (get-cd a)
          b-cd (get-cd b)]
      (apply max (clojure.set/intersection a-cd b-cd)))))

;; NOTE: Sieve implementation
(def my-prime
  (fn [n]
    (take n ((fn primes [s]
               (let [prime (first s)]
                 (cons prime
                       (lazy-seq (primes (filter #(not= (mod % prime) 0)
                                                 (rest s)))))))
             (iterate inc 2)))))

(def my-merge-with
  (fn [f & hash-maps]
    (let [by-key (group-by first
                           (mapcat #(into [] %) hash-maps))]
      (into {} (map (fn [[k vals]] [k (reduce f (map second vals))])
                    by-key)))))

(def my-sort-words
  (fn [s]
    (sort-by #(.toLowerCase %) (re-seq #"[a-zA-Z]+" s))))

(def my-tic-tac-toe
  (fn [board]
    (let [[[a b c]
           [d e f]
           [g h i]] board]
      (let [winners (map first (filter (fn [[x y z]] (= x y z)) [[a b c] [d e f] [g h i]
                                                                 [a d g] [b e h] [c f i]
                                                                 [a e i] [c e g]]))]
        (first (filter #(or (= % :x) (= % :o)) winners))))))

(def my-perfect-square
  (fn [s]
    (let [ns (map #(Integer/parseInt %) (clojure.string/split s #","))
          square-roots (map #(Math/sqrt %) ns)]
      (clojure.string/join (interpose ","
                                      (map #(-> (* % %) int .toString)
                                           (filter #(= % (-> % int double))
                                                   square-roots)))))))

(def my-totient
  (fn [n]
    (let [divisible (filter #(= (mod n %) 0) (range 2 n))]
      (count (cons 1 (filter (fn [x] (not (some (fn [y] (= (mod x y) 0)) divisible)))
                             (range 2 n)))))))

(def my-anagrams
  (fn [words]
    (let [letters-sorted (group-by sort words)
          anagrams (map set (vals (filter (fn [[k v]] (> (count v) 1)) letters-sorted)))]
      (set anagrams))))

(def my-trampoline
  (fn [f & args]
    (loop [v (apply f args)]
      (if (not (fn? v))
        v
        (recur (v))))))

(def my-triangle-minimal-path
  (fn [triangle]
    (letfn [(gen-paths [paths next-nodes]
              (let [paths-padded (concat [[]] paths [[]])
                    paths-paired (partition 2 1 paths-padded)]
                (map (fn [[node all-inner-paths]]
                       (for [inner-paths all-inner-paths
                             inner-path inner-paths]
                         (cons node inner-path)))
                     (map vector next-nodes paths-paired))))]
      (let [all-paths (reduce gen-paths [[(first triangle)]] (rest triangle))]
        (apply min (map #(apply + %) (reduce concat all-paths)))))))

(def my-perfect-number?
  (fn [n]
    (= (apply + (filter #(= (mod n %) 0) (range 1 n))) n)))

(def my-intersection
  (fn [s1 s2]
    (set (filter #(contains? s2 %) s1))))

(def my-word-chain
  (fn [words]
    (letfn [(an-update? [w1 w2]
              (and (= (count w1) (count w2))
                   (= (count (filter (fn [[x y]] (not= x y)) (map vector w1 w2))) 1)))
            (an-insert-or-delete? [w1 w2]
              (and (not= (count w1) (count w2))
                   (let [[shorter longer] (sort-by count [w1 w2])
                         zipped (map vector (concat shorter (repeat nil)) longer)
                         first-diff-at (count (take-while (fn [[x y]] (= x y)) zipped))]
                     (= (drop first-diff-at shorter)
                        (drop (inc first-diff-at) longer)))))
            (drop-nth [n coll]
              (concat (take n coll)
                      (drop (inc n) coll)))
            (perms [coll]
              (if (empty? coll)
                '(())
                (apply concat (map-indexed (fn [n item] (map #(cons item %)
                                                             (lazy-seq (perms (drop-nth n coll)))))
                                           coll))))]
      (not (nil? (some (fn [word-chain] (every? (fn [[w1 w2]] (or (an-update? w1 w2) (an-insert-or-delete? w1 w2)))
                                                (partition 2 1 word-chain)))
                       (perms words)))))))

(def my-half-truth
  (fn [& bools]
    (and (not (nil? (some identity bools)))
         (not (every? identity bools)))))

(def my-power-set
  (fn [s]
    (letfn [(zero-pad-left [n s]
              (str (apply str (repeat n "0")) s))]
      (let [v (into [] s)
            decimals (range (Math/pow 2 (count s)))
            binaries (map #(Integer/toString % 2) decimals)
            zero-padded (map (fn [b] (zero-pad-left (- (count s) (count b)) b)) binaries)
            indexed-pair-set (map #(map vector (iterate inc 0) %) zero-padded)
            indexed-pair-set-non-zeroes (for [indexed-pairs indexed-pair-set]
                                          (for [non-zero-indexed-pairs (filter (fn [[_ b]] (= b \1)) indexed-pairs)]
                                            non-zero-indexed-pairs))
            index-set (map #(map first %) indexed-pair-set-non-zeroes)]
        (set (map (fn [indices] (set (map #(nth v %) indices))) index-set))))))

(def my-happy-number?
  (fn [x]
    (letfn [(square [x]
              (* x x))
            (sum-of-squares [x]
              (reduce + (map (fn [x] (-> x str Integer/parseInt square))
                             (str x))))]
      (let [with-duplicates (drop-while #(apply distinct? %)
                                        (iterate #(cons (sum-of-squares (first %)) %) (list x)))]
        (= (-> with-duplicates first first) 1)))))

(def my-symmetric-difference
  (fn [set1 set2]
    (clojure.set/difference
     (clojure.set/union set1 set2)
     (clojure.set/intersection set1 set2))))

(def my-cartesian-product
  (fn [set1 set2]
    (set (for [item1 set1
               item2 set2]
           (vector item1 item2)))))

(def my-read-roman-numerals
  (fn [numerals]
    (let [numeral-map {\I 1
                       \V 5
                       \X 10
                       \L 50
                       \C 100
                       \D 500
                       \M 1000}]
      (letfn [(subtrahend? [[a b c d]]
                (let [a-val (get numeral-map a)
                      b-val (get numeral-map b)
                      c-val (get numeral-map c)
                      d-val (get numeral-map d)]
                  ;; b is subtrahend to c (minuend) if...
                  (and
                   ;; 0. Handle padding (i.e. last item)
                   (not (and (= c \_) (= d \_)))
                   ;; 1. "Only to a numeral (the subtrahend) that is a power of ten (I, X or C)"
                   (not (nil? (some #(= % b) [\I \X \C])))
                   ;; 2. "Only when the subtrahend precedes a minuend no more than ten times larger"
                   (and (> c-val b-val)
                        (not (> c-val (* b-val 10))))
                   ;; 3. "Only if any numeral preceding the subtrahend is at least ten times larger"
                   (or (= a \_)
                       (>= a-val (* b-val 10)))
                   ;; 4. "Only if any numeral following the minuend is smaller than the subtrahend"
                   (or (= d \_)
                       (< d-val b-val)))))]
        (reduce + (map (fn [numerals]
                         (let [val (get numeral-map (second numerals))]
                           (if (subtrahend? numerals)
                             (- val)
                             val)))
                       (partition 4 1 (str "_" numerals "__"))))))))

(def my-partially-flatten-sequence
  (fn [coll]
    (letfn [(partially-flatten [coll]
              (if (empty? coll)
                nil
                (if (coll? (-> coll first first))
                  (concat (partially-flatten (first coll)) (partially-flatten (rest coll)))
                  (cons (first coll) (partially-flatten (rest coll))))))]
      (partially-flatten coll))))

(def my-binary-tree?
  (fn [tree]
    (letfn [(binary-tree? [tree]
              (or (nil? tree)
                  (and (coll? tree)
                       (= (count tree) 3)
                       (let [[root left right] tree]
                         (and (not (nil? root))
                              (binary-tree? left)
                              (binary-tree? right))))))]
      (binary-tree? tree))))

(def my-binary-tree-symmetric?
  (fn [tree]
    (letfn [(mirror [tree]
              (if (nil? tree)
                nil
                (let [[root left right] tree]
                  (list root
                        (mirror right)
                        (mirror left)))))
            (symmetric? [tree]
              (let [[root left right] tree]
                (= left (mirror right))))]
      (symmetric? tree))))

(def my-pascals-triangle
  (fn [n]
    (let [seqs (iterate (fn [coll] (cons 1 (map #(apply + %) (partition 2 1 [0] coll))))
                        [1])]
      (nth seqs (dec n)))))

(def my-equivalence-classes
  (fn [f s]
    (set (map set (vals (group-by f s))))))

(def my-product-digits
  (fn [x y]
    (map #(Integer/parseInt %) (map str (str (* x y))))))

(def my-least-common-multiple
  (fn [& xs]
    (letfn [(divisible-by? [x]
              #(= (mod % x) 0))
            (divisible-by-all? [xs]
              #(every? identity ((apply juxt (map (fn [x] (divisible-by? x)) xs)) %)))]
      (let [sorted (sort xs)
            biggest (last sorted)
            others (butlast sorted)]
        (first (filter (divisible-by-all? others) (map #(* biggest %) (iterate inc 1))))))))

(def my-into-camel-case
  (fn [s]
    (letfn [(title-case [s]
              (let [ss (map str s)]
                (apply str (cons (clojure.string/capitalize (first ss))
                                 (rest ss)))))]
      (let [tokens (clojure.string/split s #"-")]
        (apply str (cons (first tokens) (map title-case (rest tokens))))))))

(def my-k-combinations
  (fn [n coll]
    (letfn [(get-combos [combos nums]
              (when (not-empty nums)
                (let [head (first nums)
                      tail (rest nums)]
                  (lazy-seq (cons [(conj combos head) (set tail)]
                                  (get-combos combos (set tail)))))))]
      (let [combo-pairs (iterate (fn [pairs] (mapcat #(apply get-combos %) pairs))
                                 (get-combos #{} coll))]
        (set (map first
                  (->> combo-pairs
                       (take n)
                       last)))))))

(def my-roman-numerals
  (fn [n]
    (let [to-numerals {1 "I" 2 "II" 3 "III" 4 "IV" 5 "V" 6 "VI" 7 "VII" 8 "VIII" 9 "IX"
                       10 "X" 20 "XX" 30 "XXX" 40 "XL" 50 "L" 60 "LX" 70 "LXX" 80 "LXXX" 90 "XC"
                       100 "C" 200 "CC" 300 "CCC" 400 "CD" 500 "D" 600 "DC" 700 "DCC" 800 "DCCC" 900 "CM"
                       1000 "M" 2000 "MM" 3000 "MMM"}]
      (let [sn (str n)
            ns (filter #(not (zero? %))
                       (map (fn [[x y]] (int (* x (Math/pow 10 y))))
                            (map vector
                                 (map #(-> % str Integer/parseInt) sn)
                                 (range (-> sn count dec) -1 -1))))]
        (apply str (map #(get to-numerals %) ns))))))

(def my-identify-keys-and-values
  (fn [coll]
    (let [paired (reduce (fn [x y]
                           (if (keyword? y)
                             (concat x [y []])
                             (conj (vec (butlast x))
                                   (conj (last x) y))))
                         []
                         coll)]
      (into {} (map vec (partition 2 paired))))))

(def my-simple-closure
  (fn [n]
    #(int (Math/pow % n))))

(def my-lazy-searching
  (fn [& colls]
    (letfn [(search [colls]
              (let [firsts (map first colls)
                    max-val (apply max firsts)]
                (lazy-seq
                 (cons firsts
                       (search (concat (filter #(= (first %) max-val) colls)
                                       (map rest (filter #(not= (first %) max-val) colls))))))))]
      (first (some #(when (apply = %) %) (search colls))))))

(def my-sequence-of-pronounciations
  (fn [coll]
    (letfn [(look-and-say [coll]
              (mapcat #((juxt count first) %)
                      (partition-by identity coll)))]
      (drop 1 (iterate look-and-say coll)))))
