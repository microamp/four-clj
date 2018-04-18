(ns four-clj.core-test
  (:require [clojure.test :refer :all]
            [four-clj.core :refer :all]
            [clojure.set :refer :all]))

(deftest test-001
  (testing "#001"
    (let [answer true]
      (is (= answer true)))))

(deftest test-002
  (testing "#002"
    (let [answer 4]
      (is (= (- 10 (* 2 3)) answer)))))

(deftest test-003
  (testing "#003"
    (let [answer "HELLO WORLD"]
      (is (= answer (.toUpperCase "hello world"))))))

(deftest test-004
  (testing "#004"
    (let [answer (list :a :b :c)]
      (is (= answer '(:a :b :c))))))

(deftest test-005
  (testing "#005"
    (let [answer '(1 2 3 4)]
      (are [x y] (= x y)
        answer (conj '(2 3 4) 1)
        answer (conj '(3 4) 2 1)))))

(deftest test-006
  (testing "#006"
    (let [answer [:a :b :c]]
      (is (= answer (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))))))

(deftest test-007
  (testing "#007"
    (let [answer [1 2 3 4]]
      (are [x y] (= x y)
        answer (conj [1 2 3] 4)
        answer (conj [1 2] 3 4)))))

(deftest test-008
  (testing "#008"
    (let [answer #{:a :b :c :d}]
      (are [x y] (= x y)
        answer (set '(:a :a :b :c :c :c :c :d :d))
        answer (clojure.set/union #{:a :b :c} #{:b :c :d})))))

(deftest test-009
  (testing "#009"
    (let [answer 2]
      (is (= #{1 2 3 4} (conj #{1 4 3} answer))))))

(deftest test-010
  (testing "#010"
    (let [answer 20]
      (are [x y] (= x y)
        answer ((hash-map :a 10, :b 20, :c 30) :b)
        answer (:b {:a 10, :b 20, :c 30})))))

(deftest test-011
  (testing "#011"
    (let [answer [:b 2]]
      (is (= {:a 1, :b 2, :c 3} (conj {:a 1} answer [:c 3]))))))

(deftest test-012
  (testing "#012"
    (let [answer 3]
      (are [x y] (= x y)
        answer (first '(3 2 1))
        answer (second [2 3 4])
        answer (last (list 1 2 3))))))

(deftest test-013
  (testing "#013"
    (let [answer [20 30 40]]
      (is (= answer (rest [10 20 30 40]))))))

(deftest test-014
  (testing "#014"
    (let [answer 8]
      (are [x y] (= x y)
        answer ((fn add-five [x] (+ x 5)) 3)
        answer ((fn [x] (+ x 5)) 3)
        answer (#(+ % 5) 3)
        answer ((partial + 5) 3)))))

(deftest test-015
  (testing "#015"
    (let [answer #(* % 2)]
      (is (= (answer 2) 4))
      (is (= (answer 3) 6))
      (is (= (answer 11) 22))
      (is (= (answer 7) 14)))))

(deftest test-016
  (testing "#016"
    (let [answer #(format "Hello, %s!" %)]
      (is (= (answer "Dave") "Hello, Dave!"))
      (is (= (answer "Jenn") "Hello, Jenn!"))
      (is (= (answer "Rhea") "Hello, Rhea!")))))

(deftest test-017
  (testing "#017"
    (let [answer '(6 7 8)]
      (is (= answer (map #(+ % 5) '(1 2 3)))))))

(deftest test-018
  (testing "#018"
    (let [answer '(6 7)]
      (is (= answer (filter #(> % 5) '(3 4 5 6 7)))))))

(deftest test-019
  (testing "#019"
    (let [answer my-last]
      (are [x y] (= x y)
        (answer [1 2 3 4 5]) 5
        (answer '(5 4 3)) 3
        (answer ["b" "c" "d"]) "d"))))

(deftest test-020
  (testing "#020"
    (let [answer my-second-to-last]
      (are [x y] (= x y)
        (answer (list 1 2 3 4 5)) 4
        (answer ["a" "b" "c"]) "b"
        (answer [[1 2] [3 4]]) [1 2]))))

(deftest test-021
  (testing "#021"
    (let [answer my-nth]
      (are [x y] (= x y)
        (answer '(4 5 6 7) 2) 6
        (answer [:a :b :c] 0) :a
        (answer [1 2 3 4] 1) 2
        (answer '([1 2] [3 4] [5 6]) 2) [5 6]))))

(deftest test-022
  (testing "#022"
    (let [answer my-count]
      (are [x y] (= x y)
        (answer '(1 2 3 3 1)) 5
        (answer "Hello World") 11
        (answer [[1 2] [3 4] [5 6]]) 3
        (answer '(13)) 1
        (answer '(:a :b :c)) 3))))

(deftest test-023
  (testing "#023"
    (let [answer my-reverse]
      (are [x y] (= x y)
        (answer [1 2 3 4 5]) [5 4 3 2 1]
        (answer (sorted-set 5 7 2 7)) '(7 5 2)
        (answer [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]]))))

(deftest test-024
  (testing "#024"
    (let [answer my-sum]
      (are [x y] (= x y)
        (answer [1 2 3]) 6
        (answer (list 0 -2 5 5)) 8
        (answer #{4 2 1}) 7
        (answer '(0 0 -1)) -1
        (answer '(1 10 3)) 14))))

(deftest test-025
  (testing "#025"
    (let [answer my-odd-only]
      (are [x y] (= x y)
        (answer #{1 2 3 4 5}) '(1 3 5)
        (answer [4 2 1 6]) '(1)
        (answer [2 2 4 6]) '()
        (answer [1 1 1 3]) '(1 1 1 3)))))

(deftest test-026
  (testing "#026"
    (let [answer my-fibonacci]
      (are [x y] (= x y)
        (answer 3) '(1 1 2)
        (answer 6) '(1 1 2 3 5 8)
        (answer 8) '(1 1 2 3 5 8 13 21)))))

(deftest test-027
  (testing "#027"
    (let [answer my-palindrome?]
      (is (false? (answer '(1 2 3 4 5))))
      (is (true? (answer "racecar")))
      (is (true? (answer [:foo :bar :foo])))
      (is (true? (answer '(1 1 3 3 1 1))))
      (is (false? (answer '(:a :b :c)))))))

(deftest test-028
  (testing "#028"
    (let [answer my-flatten]
      (are [x y] (= x y)
        (answer '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)
        (answer ["a" ["b"] "c"]) '("a" "b" "c")
        (answer '((((:a))))) '(:a)))))

(deftest test-029
  (testing "#029"
    (let [answer my-cap-only]
      (is (= (answer "HeLlO, WoRlD!") "HLOWRD"))
      (is (empty? (answer "nothing")))
      (is (= (answer "$#A(*&987Zf") "AZ")))))

(deftest test-030
  (testing "#030"
    (let [answer my-dedupe]
      (are [x y] (= x y)
        (apply str (answer "Leeeeeerrroyyy")) "Leroy"
        (answer [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)
        (answer [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))))

(deftest test-031
  (testing "#031"
    (let [answer my-pack]
      (are [x y] (= x y)
        (answer [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))
        (answer [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))
        (answer [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))))

(deftest test-032
  (testing "#032"
    (let [answer my-duplicate]
      (are [x y] (= x y)
        (answer [1 2 3]) '(1 1 2 2 3 3)
        (answer [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)
        (answer [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])
        (answer [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))))

(deftest test-033
  (testing "#033"
    (let [answer my-replicate]
      (are [x y] (= x y)
        (answer [1 2 3] 2) '(1 1 2 2 3 3)
        (answer [:a :b] 4) '(:a :a :a :a :b :b :b :b)
        (answer [4 5 6] 1) '(4 5 6)
        (answer [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])
        (answer [44 33] 2) [44 44 33 33]))))

(deftest test-034
  (testing "#034"
    (let [answer my-range]
      (are [x y] (= x y)
        (answer 1 4) '(1 2 3)
        (answer -2 2) '(-2 -1 0 1)
        (answer 5 8) '(5 6 7)))))

(deftest test-035
  (testing "#035"
    (let [answer 7]
      (are [x y] (= x y)
        answer (let [x 5] (+ 2 x))
        answer (let [x 3, y 10] (- y x))
        answer (let [x 21] (let [y 3] (/ x y)))))))

(deftest test-036
  (testing "#036"
    (let [x 7
          y 3
          z 1]
      (are [x y] (= x y)
        (+ x y) 10
        (+ y z) 4
        z 1))))

(deftest test-037
  (testing "#037"
    (is (= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))))

(deftest test-038
  (testing "#038"
    (let [answer my-max]
      (are [x y] (= x y)
        (answer 1 8 3 4) 8
        (answer 30 20) 30
        (answer 45 67 11) 67))))

(deftest test-039
  (testing "#039"
    (let [answer my-interleave]
      (are [x y] (= x y)
        (answer [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)
        (answer [1 2] [3 4 5 6]) '(1 3 2 4)
        (answer [1 2 3 4] [5]) [1 5]
        (answer [30 20] [25 15]) [30 25 20 15]))))

(deftest test-040
  (testing "#040"
    (let [answer my-interpose]
      (are [x y] (= x y)
        (answer 0 [1 2 3]) [1 0 2 0 3]
        (apply str (answer ", " ["one" "two" "three"])) "one, two, three"
        (answer :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))))

(deftest test-041
  (testing "#041"
    (let [answer my-drop-every-nth]
      (are [x y] (= x y)
        (answer [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]
        (answer [:a :b :c :d :e :f] 2) [:a :c :e]
        (answer [1 2 3 4 5 6] 4) [1 2 3 5 6]))))

(deftest test-042
  (testing "#042"
    (let [answer my-factorial]
      (are [x y] (= x y)
        (answer 1) 1
        (answer 3) 6
        (answer 5) 120
        (answer 8) 40320))))

(deftest test-043
  (testing "#043"
    (let [answer my-reverse-interleave]
      (are [x y] (= x y)
        (answer [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))
        (answer (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))
        (answer (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))))

(deftest test-044
  (testing "#044"
    (let [answer my-rotate]
      (are [x y] (= x y)
        (answer 2 [1 2 3 4 5]) '(3 4 5 1 2)
        (answer -2 [1 2 3 4 5]) '(4 5 1 2 3)
        (answer 6 [1 2 3 4 5]) '(2 3 4 5 1)
        (answer 1 '(:a :b :c)) '(:b :c :a)
        (answer -4 '(:a :b :c)) '(:c :a :b)))))

(deftest test-045
  (testing "#045"
    (let [answer [1 4 7 10 13]]
      (is (= answer (take 5 (iterate #(+ 3 %) 1)))))))

(deftest test-046
  (testing "#046"
    (let [answer my-flip-args]
      (are [x y] (= x y)
        3 ((answer nth) 2 [1 2 3 4 5])
        true ((answer >) 7 8)
        4 ((answer quot) 2 8)
        [1 2 3] ((answer take) [1 2 3 4 5] 3)))))

(deftest test-047
  (testing "#047"
    (let [answer 4]
      (is (contains? #{4 5 6} answer))
      (is (contains? [1 1 1 1 1] answer))
      (is (contains? {4 :a 2 :b} answer))
      (is (not (contains? [1 2 4] answer))))))

(deftest test-048
  (testing "#048"
    (let [answer 6]
      (are [x y] (= x y)
        answer (some #{2 7 6} [5 6 7 8])
        answer (some #(when (even? %) %) [5 6 7 8])))))

(deftest test-049
  (testing "#049"
    (let [answer my-split-at]
      (are [x y] (= x y)
        (answer 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]
        (answer 1 [:a :b :c :d]) [[:a] [:b :c :d]]
        (answer 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))))

(deftest test-050
  (testing "#050"
    (let [answer my-split-by-type]
      (are [x y] (= x y)
        (set (answer [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}
        (set (answer [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}
        (set (answer [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))))

(deftest test-051
  (testing "#051"
    (let [answer [1 2 3 4 5]]
      (is (= [1 2 [3 4 5] [1 2 3 4 5]]
             (let [[a b & c :as d] answer]
               [a b c d]))))))

(deftest test-052
  (testing "#052"
    (is (= [2 4]
           (let [[a b c d e] [0 1 2 3 4]]
             [c e])))))

(deftest test-053
  (testing "#053"
    (let [answer my-longest-increasing-sub-seq]
      (are [x y] (= x y)
        (answer [1 0 1 2 3 0 4 5]) [0 1 2 3]
        (answer [5 6 1 3 2 7]) [5 6]
        (answer [2 3 3 4 5]) [3 4 5]
        (answer [7 6 5 4]) []))))

(deftest test-054
  (testing "#054"
    (let [answer my-partition]
      (are [x y] (= x y)
        (answer 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))
        (answer 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))
        (answer 3 (range 8)) '((0 1 2) (3 4 5))))))

(deftest test-055
  (testing "#055"
    (let [answer my-frequencies]
      (are [x y] (= x y)
        (answer [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}
        (answer [:b :a :b :a :b]) {:a 2, :b 3}
        (answer '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))))

(deftest test-056
  (testing "#056"
    (let [answer my-distinct]
      (are [x y] (= x y)
        (answer [1 2 1 3 1 2 4]) [1 2 3 4]
        (answer [:a :a :b :b :c :c]) [:a :b :c]
        (answer '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])
        (answer (range 50)) (range 50)
        (answer (range 50)) (range 50)))))

(deftest test-057
  (testing "#057"
    (let [answer [5 4 3 2 1]]
      (is (= answer
             ((fn foo [x]
                (when (> x 0)
                  (conj (foo (dec x)) x))) 5))))))

(deftest test-058
  (testing "#058"
    (let [answer my-comp]
      (are [x y] (= x y)
        [3 2 1] ((answer rest reverse) [1 2 3 4])
        5 ((answer (partial + 3) second) [1 2 3 4])
        true ((answer zero? #(mod % 8) +) 3 5 7 9)
        "HELLO" ((answer #(.toUpperCase %) #(apply str %) take) 5 "hello world")))))

(deftest test-059
  (testing "#059"
    (let [answer my-juxt]
      (are [x y] (= x y)
        [21 6 1] ((answer + max min) 2 3 5 1 6 4)
        ["HELLO" 5] ((answer #(.toUpperCase %) count) "hello")
        [2 6 4] ((answer :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))))

(deftest test-060
  (testing "#060"
    (let [answer my-reductions]
      (is (= (take 5 (answer + (range))) [0 1 3 6 10]))
      (is (= (answer conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
      (is (= (last (answer * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)))))

(deftest test-061
  (testing "#061"
    (let [answer my-zipmap]
      (are [x y] (= x y)
        (answer [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}
        (answer [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}
        (answer [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))))

(deftest test-062
  (testing "#062"
    (let [answer my-iterate]
      (are [x y] (= x y)
        (take 5 (answer #(* 2 %) 1)) [1 2 4 8 16]
        (take 100 (answer inc 0)) (take 100 (range))
        (take 9 (answer #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))))

(deftest test-063
  (testing "#063"
    (let [answer my-group-by]
      (are [x y] (= x y)
        (answer #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}
        (answer #(apply / %) [[1 2] [2 4] [4 6] [3 6]]) {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}
        (answer count [[1] [1 2] [3] [1 2 3] [2 3]]) {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))))

(deftest test-064
  (testing "#064"
    (let [answer +]
      (are [x y] (= x y)
        15 (reduce answer [1 2 3 4 5])
        0 (reduce answer [])
        6 (reduce answer 1 [2 3])))))

(deftest test-065
  (testing "#065"
    (let [answer my-seq-type]
      (are [x y] (= x y)
        :map (answer {:a 1, :b 2})
        :list (answer (range (rand-int 20)))
        :vector (answer [1 2 3 4 5 6])
        :set (answer #{10 (rand-int 5)})
        [:map :set :vector :list] (map answer [{} #{} [] ()])))))

(deftest test-066
  (testing "#066"
    (let [answer my-gcd]
      (are [x y] (= x y)
        (answer 2 4) 2
        (answer 10 5) 5
        (answer 5 7) 1
        (answer 1023 858) 33))))

(deftest test-067
  (testing "#067"
    (let [answer my-prime]
      (are [x y] (= x y)
        (answer 2) [2 3]
        (answer 5) [2 3 5 7 11]
        (last (answer 100)) 541))))

(deftest test-068
  (testing "#068"
    (let [answer [7 6 5 4 3]]
      (is (= answer
             (loop [x 5
                    result []]
               (if (> x 0)
                 (recur (dec x) (conj result (+ 2 x)))
                 result)))))))

(deftest test-069
  (testing "#069"
    (let [answer my-merge-with]
      (are [x y] (= x y)
        (answer * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
        {:a 4, :b 6, :c 20}
        (answer - {1 10, 2 20} {1 3, 2 10, 3 15})
        {1 7, 2 10, 3 15}
        (answer concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
        {:a [3 4 5], :b [6 7], :c [8 9]}))))

(deftest test-070
  (testing "#070"
    (let [answer my-sort-words]
      (are [x y] (= x y)
        (answer "Have a nice day.")
        ["a" "day" "Have" "nice"]
        (answer "Clojure is a fun language!")
        ["a" "Clojure" "fun" "is" "language"]
        (answer "Fools fall for foolish follies.")
        ["fall" "follies" "foolish" "Fools" "for"]))))

(deftest test-071
  (testing "#071"
    (let [answer last]
      (is (= (answer (sort (rest (reverse [2 5 4 1 3 6]))))
             (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (answer))
             5)))))

(deftest test-072
  (testing "#072"
    (is (= (apply + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
           (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (apply +))
           11))))

(deftest test-073
  (testing "#073"
    (let [answer my-tic-tac-toe]
      (is (= nil (answer [[:e :e :e]
                          [:e :e :e]
                          [:e :e :e]])))
      (is (= :x (answer [[:x :e :o]
                         [:x :e :e]
                         [:x :e :o]])))
      (is (= :o (answer [[:e :x :e]
                         [:o :o :o]
                         [:x :e :x]])))
      (is (= nil (answer [[:x :e :o]
                          [:x :x :e]
                          [:o :x :o]])))
      (is (= :x (answer [[:x :e :e]
                         [:o :x :e]
                         [:o :e :x]])))
      (is (= :o (answer [[:x :e :o]
                         [:x :o :e]
                         [:o :e :x]])))
      (is (= nil (answer [[:x :o :x]
                          [:x :o :x]
                          [:o :x :o]]))))))

(deftest test-074
  (testing "#074"
    (let [answer my-perfect-square]
      (are [x y] (= x y)
        (answer "4,5,6,7,8,9") "4,9"
        (answer "15,16,25,36,37") "16,25,36"))))

(deftest test-075
  (testing "#075"
    (let [answer my-totient]
      (is (= (answer 1) 1))
      (is (= (answer 10) (count '(1 3 7 9)) 4))
      (is (= (answer 40) 16))
      (is (= (answer 99) 60)))))

(deftest test-076
  (testing "#076"
    (let [answer [1 3 5 7 9 11]]
      (is (= answer
             (letfn
              [(foo [x y] #(bar (conj x y) y))
               (bar [x y] (if (> (last x) 10)
                            x
                            #(foo x (+ 2 y))))]
               (trampoline foo [] 1)))))))

(deftest test-077
  (testing "#077"
    (let [answer my-anagrams]
      (is (= (answer ["meat" "mat" "team" "mate" "eat"])
             #{#{"meat" "team" "mate"}}))
      (is (= (answer ["veer" "lake" "item" "kale" "mite" "ever"])
             #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))))

(deftest test-078
  (testing "#078"
    (let [answer my-trampoline]
      (is (= (letfn [(triple [x] #(sub-two (* 3 x)))
                     (sub-two [x] #(stop? (- x 2)))
                     (stop? [x] (if (> x 50) x #(triple x)))]
               (answer triple 2))
             82))
      (is (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                     (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
               (map (partial answer my-even?) (range 6)))
             [true false true false true false])))))

(deftest test-079
  (testing "#079"
    (let [answer my-triangle-minimal-path]
      (are [x y] (= x y)
        7 (answer '([1]
                    [2 4]
                    [5 1 4]
                    [2 3 4 5]))
        20 (answer '([3]
                     [2 4]
                     [1 9 3]
                     [9 9 2 4]
                     [4 6 6 7 8]
                     [5 7 3 5 1 4]))))))

(deftest test-080
  (testing "#080"
    (let [answer my-perfect-number?]
      (are [x y] (= x y)
        (answer 6) true
        (answer 7) false
        (answer 496) true
        (answer 500) false
        (answer 8128) true))))

(deftest test-081
  (testing "#081"
    (let [answer my-intersection]
      (are [x y] (= x y)
        (answer #{0 1 2 3} #{2 3 4 5}) #{2 3}
        (answer #{0 1 2} #{3 4 5}) #{}
        (answer #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))))

(deftest test-082
  (testing "#082"
    (let [answer my-word-chain]
      (are [x y] (= x y)
        true (my-word-chain #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})
        false (my-word-chain #{"cot" "hot" "bat" "fat"})
        false (my-word-chain #{"to" "top" "stop" "tops" "toss"})
        true (my-word-chain #{"spout" "do" "pot" "pout" "spot" "dot"})
        true (my-word-chain #{"share" "hares" "shares" "hare" "are"})
        false (my-word-chain #{"share" "hares" "hare" "are"})))))

(deftest test-083
  (testing "#083"
    (let [answer my-half-truth]
      (are [x y] (= x y)
        false (answer false false)
        true (answer true false)
        false (answer true)
        true (answer false true false)
        false (answer true true true)
        true (answer true true true false)))))

(deftest test-084
  (testing "#084"))

(deftest test-085
  (testing "#085"
    (let [answer my-power-set]
      (are [x y] (= x y)
        (answer #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}
        (answer #{}) #{#{}}
        (answer #{1 2 3}) #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}
        (count (answer (into #{} (range 10)))) 1024))))

(deftest test-086
  (testing "#086"
    (let [answer my-happy-number?]
      (are [x y] (= x y)
        (answer 7) true
        (answer 986543210) true
        (answer 2) false
        (answer 3) false))))

(deftest test-087
  (testing "#087"))

(deftest test-088
  (testing "#088"
    (let [answer my-symmetric-difference]
      (are [x y] (= x y)
        (answer #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}
        (answer #{:a :b :c} #{}) #{:a :b :c}
        (answer #{} #{4 5 6}) #{4 5 6}
        (answer #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]}))))

(deftest test-089
  (testing "#089"))

(deftest test-090
  (testing "#090"
    (let [answer my-cartesian-product]
      (are [x y] (= x y)
        (answer #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
        #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
          ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
          ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}

        (answer #{1 2 3} #{4 5})
        #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}

        300
        (count (answer (into #{} (range 10))
                       (into #{} (range 30))))))))

(deftest test-091
  (testing "#091"))

(deftest test-092
  (testing "#092"
    (let [answer my-read-roman-numerals]
      (are [x y] (= x y)
        14 (answer "XIV")
        827 (answer "DCCCXXVII")
        3999 (answer "MMMCMXCIX")
        48 (answer "XLVIII")))))

(deftest test-093
  (testing "#093"
    (let [answer my-partially-flatten-sequence]
      (are [x y] (= x y)
        (answer [["Do"] ["Nothing"]])
        [["Do"] ["Nothing"]]

        (answer [[[[:a :b]]] [[:c :d]] [:e :f]])
        [[:a :b] [:c :d] [:e :f]]

        (answer '((1 2) ((3 4) ((((5 6)))))))
        '((1 2) (3 4) (5 6))))))

(deftest test-094
  (testing "#094"))

(deftest test-095
  (testing "#095"
    (let [answer my-binary-tree?]
      (are [x y] (= x y)
        (answer '(:a (:b nil nil) nil)) true
        (answer '(:a (:b nil nil))) false
        (answer [1 nil [2 [3 nil nil] [4 nil nil]]]) true
        (answer [1 [2 nil nil] [3 nil nil] [4 nil nil]]) false
        (answer [1 [2 [3 [4 nil nil] nil] nil] nil]) true
        (answer [1 [2 [3 [4 false nil] nil] nil] nil]) false
        (answer '(:a nil ())) false))))

(deftest test-096
  (testing "#096"
    (let [answer my-binary-tree-symmetric?]
      (is (= (answer '(:a (:b nil nil) (:b nil nil))) true))
      (is (= (answer '(:a (:b nil nil) nil)) false))
      (is (= (answer '(:a (:b nil nil) (:c nil nil))) false))
      (is (= (answer [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                      [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
             true))
      (is (= (answer [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                      [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
             false))
      (is (= (answer [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                      [2 [3 nil [4 [6 nil nil] nil]] nil]])
             false)))))

(deftest test-097
  (testing "#097"
    (let [answer my-pascals-triangle]
      (is (= (answer 1) [1]))
      (is (= (map answer (range 1 6))
             [[1]
              [1 1]
              [1 2 1]
              [1 3 3 1]
              [1 4 6 4 1]]))
      (is (= (answer 11)
             [1 10 45 120 210 252 210 120 45 10 1])))))

(deftest test-098
  (testing "#098"
    (let [answer my-equivalence-classes]
      (are [x y] (= x y)
        (answer #(* % %) #{-2 -1 0 1 2}) #{#{0} #{1 -1} #{2 -2}}
        (answer #(rem % 3) #{0 1 2 3 4 5}) #{#{0 3} #{1 4} #{2 5}}
        (answer identity #{0 1 2 3 4}) #{#{0} #{1} #{2} #{3} #{4}}
        (answer (constantly true) #{0 1 2 3 4}) #{#{0 1 2 3 4}}))))

(deftest test-099
  (testing "#099"
    (let [answer my-product-digits]
      (are [x y] (= x y)
        (answer 1 1) [1]
        (answer 99 9) [8 9 1]
        (answer 999 99) [9 8 9 0 1]))))

(deftest test-100
  (testing "#100"
    (let [answer my-least-common-multiple]
      (are [x y] (== x y)
        (answer 2 3) 6
        (answer 5 3 7) 105
        (answer 1/3 2/5) 2
        (answer 3/4 1/6) 3/2
        (answer 7 5/7 2 3/5) 210))))

(deftest test-101
  (testing "#101"))

(deftest test-102
  (testing "#102"
    (let [answer my-into-camel-case]
      (are [x y] (= x y)
        (answer "something") "something"
        (answer "multi-word-key") "multiWordKey"
        (answer "leaveMeAlone") "leaveMeAlone"))))

(deftest test-103
  (testing "#103"
    (let [answer my-k-combinations]
      (is (= (answer 1 #{4 5 6}) #{#{4} #{5} #{6}}))
      (is (= (answer 10 #{4 5 6}) #{}))
      (is (= (answer 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
      (is (= (answer 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                       #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
      (is (= (answer 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
      (is (= (answer 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                    #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})))))

(deftest test-104
  (testing "#104"
    (let [answer my-roman-numerals]
      (are [x y] (= x y)
        "I" (answer 1)
        "XXX" (answer 30)
        "IV" (answer 4)
        "CXL" (answer 140)
        "DCCCXXVII" (answer 827)
        "MMMCMXCIX" (answer 3999)
        "XLVIII" (answer 48)))))

(deftest test-105
  (testing "#105"
    (let [answer my-identify-keys-and-values]
      (are [x y] (= x y)
        {} (answer [])
        {:a [1]} (answer [:a 1])
        {:a [1], :b [2]} (answer [:a 1, :b 2])
        {:a [1 2 3], :b [], :c [4]} (answer [:a 1 2 3 :b :c 4])))))

(deftest test-106
  (testing "#106"))

(deftest test-107
  (testing "#107"
    (let [answer my-simple-closure]
      (is (= 256 ((answer 2) 16) ((answer 8) 2)))
      (is (= [1 8 27 64] (map (answer 3) [1 2 3 4])))
      (is (= [1 2 4 8 16] (map #((answer %) 2) [0 1 2 3 4]))))))

(deftest test-108
  (testing "#108"
    (let [answer my-lazy-searching]
      (are [x y] (= x y)
        3 (answer [3 4 5])
        4 (answer [1 2 3 4 5 6 7] [0.5 3/2 4 19])
        7 (answer (range) (range 0 100 7/6) [2 3 5 7 11 13])
        64 (answer (map #(* % % %) (range)) ;; perfect cubes
                   (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                   (iterate inc 20))))))

(deftest test-109
  (testing "#109"))

(deftest test-110
  (testing "#110"
    (let [answer my-sequence-of-pronounciations]
      (are [x y] (= x y)
        [[1 1] [2 1] [1 2 1 1]] (take 3 (answer [1]))
        [3 1 2 4] (first (answer [1 1 1 4 4]))
        [1 1 1 3 2 1 3 2 1 1] (nth (answer [1]) 6)
        338 (count (nth (answer [3 2]) 15))))))
