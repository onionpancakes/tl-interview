(ns twolions-interview.core)

(def example-board
  [["t" "h" "i" "s" "i" "s" "a"]
   ["s" "i" "m" "p" "l" "e" "x"]
   ["b" "x" "x" "x" "x" "e" "b"]
   ["x" "o" "g" "g" "l" "x" "o"]
   ["x" "x" "x" "D" "T" "r" "a"]
   ["R" "E" "P" "E" "A" "d" "x"]
   ["x" "x" "x" "x" "x" "x" "x"]
   ["N" "O" "T" "R" "E" "-" "P"]
   ["x" "x" "D" "E" "T" "A" "E"]])

#_(defn north
  [start _]
  (let [[x y] start]
    (for [i (range x (dec 0) -1)]
      [i y])))

#_(defn south
  [start dim]
  (let [[x y]   start
        [dx dy] dim]
    (for [i (range x (dx))])))

(defn neighbors
  "Clockwise neighbors"
  [[di dj] [i j]]
  (->> [[(dec i) j] ; north 1
        [(dec i) (inc j)] ; north-east 2
        [i (inc j)] ; east 3
        [(inc i) (inc j)] ; south-east 4
        [(inc i) j] ; south 5
        [(inc i) (dec j)] ; south-west 6
        [i (dec j)] ; west 7
        [(dec i) (dec j)]]; north-west 8
       (filter #(and (<= 0 (first %) (dec di))
                     (<= 0 (second %) (dec dj))))))

(defn search
  [board word]
  (let [dx       (count board)
        dy       (count (first board))
        dim      [dx dy]
        chstr    (map str word)
        ;; Initialize stack value to coord matching the first letter of word.
        init-st  (for [i     (range dx)
                       j     (range dy)
                       :when (= (get-in board [i j]) (first chstr))]
                   {:coord   [i j]
                    :chars   (next chstr)
                    :visited (set [[i j]])})]
    (loop [st (vec init-st)]
      (if (peek st)
        (let [{:keys [coord chars visited]} (peek st)]
          (if (seq chars)
            (recur (->> (neighbors dim coord)
                        (remove visited)
                        (filter #(= (get-in board %) (first chars)))
                        (map #(array-map :coord %
                                         :chars (next chars)
                                         :visited (conj visited %)))
                        (into (pop st))))
            true))
        false))))

;; Tests

(def basic-boggle search)

(def tests
  [{
    :name "test 1"
    :input {:board [["t" "h" "i" "s" "i" "s" "a"]
                    ["s" "i" "m" "p" "l" "e" "x"]
                    ["b" "x" "x" "x" "x" "e" "b"]
                    ["x" "o" "g" "g" "l" "x" "o"]
                    ["x" "x" "x" "D" "T" "r" "a"]
                    ["R" "E" "P" "E" "A" "d" "x"]
                    ["x" "x" "x" "x" "x" "x" "x"]
                    ["N" "O" "T" "R" "E" "-" "P"]
                    ["x" "x" "D" "E" "T" "A" "E"]]
            :word "this"}
    :output true}
   {:name "test 2"
    :input {:board [["t" "h" "i" "s" "i" "s" "a"]
                    ["s" "i" "m" "p" "l" "e" "x"]
                    ["b" "x" "x" "x" "x" "e" "b"]
                    ["x" "o" "g" "g" "l" "x" "o"]
                    ["x" "x" "x" "D" "T" "r" "a"]
                    ["R" "E" "P" "E" "A" "d" "x"]
                    ["x" "x" "x" "x" "x" "x" "x"]
                    ["N" "O" "T" "R" "E" "-" "P"]
                    ["x" "x" "D" "E" "T" "A" "E"]]
            :word "not"}
    :output false}
   {:name "test 3"
    :input {:board [["t" "h" "i" "s" "i" "s" "a"]
                    ["s" "i" "m" "p" "l" "e" "x"]
                    ["b" "x" "x" "x" "x" "e" "b"]
                    ["x" "o" "g" "g" "l" "x" "o"]
                    ["x" "x" "x" "D" "T" "r" "a"]
                    ["R" "E" "P" "E" "A" "d" "x"]
                    ["x" "x" "x" "x" "x" "x" "x"]
                    ["N" "O" "T" "R" "E" "-" "P"]
                    ["x" "x" "D" "E" "T" "A" "E"]]
            :word "board"}
    :output true}
   {:name "test 4"
    :input {:board [["t" "h" "i" "s" "i" "s" "a"]
                    ["s" "i" "m" "p" "l" "e" "x"]
                    ["b" "x" "x" "x" "x" "e" "b"]
                    ["x" "o" "g" "g" "l" "x" "o"]
                    ["x" "x" "x" "D" "T" "r" "a"]
                    ["R" "E" "P" "E" "A" "d" "x"]
                    ["x" "x" "x" "x" "x" "x" "x"]
                    ["N" "O" "T" "R" "E" "-" "P"]
                    ["x" "x" "D" "E" "T" "A" "E"]]
            :word "simple"}
    :output true}
   {:name "test 5"
    :input {:board [["t" "h" "i" "s" "i" "s" "a"]
                    ["s" "i" "m" "p" "l" "e" "x"]
                    ["b" "x" "x" "x" "x" "e" "b"]
                    ["x" "o" "g" "g" "l" "x" "o"]
                    ["x" "x" "x" "D" "T" "r" "a"]
                    ["R" "E" "P" "E" "A" "d" "x"]
                    ["x" "x" "x" "x" "x" "x" "x"]
                    ["N" "O" "T" "R" "E" "-" "P"]
                    ["x" "x" "D" "E" "T" "A" "E"]]
            :word "REPEATED"}
    :output false}
   {:name "test 6"
    :input {:board [["t" "h" "i" "s" "i" "s" "a"]
                    ["s" "i" "m" "p" "l" "e" "x"]
                    ["b" "x" "x" "x" "x" "e" "b"]
                    ["x" "o" "g" "g" "l" "x" "o"]
                    ["x" "x" "x" "D" "T" "r" "a"]
                    ["R" "E" "P" "E" "A" "d" "x"]
                    ["x" "x" "x" "x" "x" "x" "x"]
                    ["N" "O" "T" "R" "E" "-" "P"]
                    ["x" "x" "D" "E" "T" "A" "E"]]
            :word "NOTRE-PEATED"}
    :output true}
   {:name "test 7"
    :input {:board [["y" "g" "f" "y" "e" "i"]
                    ["c" "o" "r" "p" "o" "u"]
                    ["j" "u" "z" "s" "e" "l"]
                    ["s" "y" "u" "r" "h" "p"]
                    ["e" "a" "e" "g" "n" "d"]
                    ["h" "e" "l" "s" "a" "t"]]
            :word "yours"}
    :output true}
   {:name "test 8"
    :input {:board [["y" "g" "f" "y" "e" "i"]
                    ["c" "o" "r" "p" "o" "u"]
                    ["j" "u" "z" "s" "e" "l"]
                    ["s" "y" "u" "r" "h" "p"]
                    ["e" "a" "e" "g" "n" "d"]
                    ["h" "e" "l" "s" "a" "t"]]
            :word "sana"}
    :output false}
   {:name "test 9"
    :input {:board [["y" "g" "f" "y" "e" "i"]
                    ["c" "o" "r" "p" "o" "u"]
                    ["j" "u" "z" "s" "e" "l"]
                    ["s" "y" "u" "r" "h" "p"]
                    ["e" "a" "e" "g" "n" "d"]
                    ["h" "e" "l" "s" "a" "t"]]
            :word "san"}
    :output true}
   {:name "test 10"
    :input {:board [["y" "g" "f" "y" "e" "i"]
                    ["c" "o" "r" "p" "o" "u"]
                    ["j" "u" "z" "s" "e" "l"]
                    ["s" "y" "u" "r" "h" "p"]
                    ["e" "a" "e" "g" "n" "d"]
                    ["h" "e" "l" "s" "a" "t"]]
            :word "danger"}
    :output true}
   {:name "test 11"
    :input {:board [["y" "g" "f" "y" "e" "i"]
                    ["c" "o" "r" "p" "o" "u"]
                    ["j" "u" "z" "s" "e" "l"]
                    ["s" "y" "u" "r" "h" "p"]
                    ["e" "a" "e" "g" "n" "d"]
                    ["h" "e" "l" "s" "a" "t"]]
            :word "help"}
    :output true}
   {:name "test 12"
    :input {:board [["y" "g" "f" "y" "e" "i"]
                    ["c" "o" "r" "p" "o" "u"]
                    ["j" "u" "z" "s" "e" "l"]
                    ["s" "y" "u" "r" "h" "p"]
                    ["e" "a" "e" "g" "n" "d"]
                    ["h" "e" "l" "s" "a" "t"]]
            :word "vomit"}
    :output false}])

(defn run-test
  [n]
  (basic-boggle (:board (:input (nth tests (dec n))))
                (:word (:input (nth tests (dec n))))))

(defn run-tests []
  (doseq [{name :name
           {board :board
            word :word} :input
           output :output} tests]
    (let [got (basic-boggle board word)
          exp output]
      (when (not= got exp)
        (println (str "Test Name: " name "\n"
                      "Expected: " exp "\n"
                      "Got: " got "\n"))))))

(defn -main []
  (println "Running tests.")
  (run-tests)
  (println "Finished."))
