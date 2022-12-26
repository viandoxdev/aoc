(ns main
  "AOC 2022 #11")

(require '[clojure.string :as str])

(defn session [] 
  (str/replace (slurp "../../session") #"\n" ""))

(def url "https://adventofcode.com/2022/day/11/input")

(defrecord Monkey [items op div then else inspected])
(defrecord Operation [op lhs rhs])
(defrecord Report [item monkey])
(defrecord State [worry-fac monkeys lcm])
(defrecord Turn [inspected reports])

(defn parseint [arg] (long (Integer/parseInt arg)))

(defn get-input [session] 
  (.. (java.net.http.HttpClient/newHttpClient)
    (send
      (.. (java.net.http.HttpRequest/newBuilder)
        (header "Cookie", (str "session=" session))
        (GET)
        (uri (java.net.URI. url))
        (build))
      (java.net.http.HttpResponse$BodyHandlers/ofString))
    (body)))

(defn parse-op [op]
  (let [ar (str/split op #" ")
        ope #(if (re-matches #"old" %) :old (parseint %))]
      (Operation.
        (case (get ar 1)
          "*" :Mul
          "+" :Add)
        (ope (get ar 0))
        (ope (get ar 2)))))

(defn eval-op [old op]
  (let [value #(if (= % :old) old %)]
    (case (:op op)
      :Mul (* (value (:lhs op)) (value (:rhs op)))
      :Add (+ (value (:lhs op)) (value (:rhs op))))))

(defn parse-monkey [monkey]
  (let [ items (get (re-find #"Starting items: ([0-9 ,]+?)\n" monkey) 1)
         op    (get (re-find #"Operation: new = ([[0-9a-z *+]]+?)\n" monkey) 1)
         div   (get (re-find #"Test: divisible by ([0-9]+?)\n" monkey) 1)
         then  (get (re-find #"If true: throw to monkey ([0-9]+?)\n" monkey) 1)
         else  (get (re-find #"If false: throw to monkey ([0-9]+)" monkey) 1)]
    (Monkey. 
      (apply vector (map parseint (str/split items #", ")))
      (parse-op op)
      (parseint div)
      (parseint then)
      (parseint else)
      0
      )))

(defn parse-input [input]
  (let [monkeys (apply vector (map parse-monkey (str/split input #"\n\n")))]
  (State.
    1
    monkeys
    (reduce #(* %1 (:div %2)) 1 monkeys))))

(defn monkey-turn [monkey worry-fac lcm]
  (Turn.
    (count (:items monkey))
    (map 
      (fn [item]
        (let [new_item (-> item
                     (eval-op (:op monkey))
                     (* worry-fac)
                     (Math/floor)
                     (long)
                     )]
            (Report.
              (if (== worry-fac 1) (mod new_item lcm) new_item)
              (if (= (mod new_item (:div monkey)) 0)
                (:then monkey)
                (:else monkey)))))
      (:items monkey))))

(defn apply-report [state report]
  (update-in state [:monkeys (:monkey report) :items] #(conj % (:item report))))

(defn update-state [state monkey turn]
  (reduce apply-report 
    (update-in 
      (assoc-in state [:monkeys monkey :items] [])
      [:monkeys monkey :inspected]
      #(+ % (:inspected turn)))
    (:reports turn)))

(defn round-state [state]
  (reduce 
    (fn [state monkey]
        (update-state 
          state
          monkey 
          (monkey-turn (get-in state [:monkeys monkey]) (:worry-fac state) (:lcm state))))
    state
    (range (count (:monkeys state)))))

(defn monkey-business [state]
  (apply * 
    (take 2
      (sort >
        (map 
          (fn [monkey] (:inspected monkey)) 
          (:monkeys state))))))

(defn round-state-n [state n]
  (nth (iterate round-state state) n))

(def init-state
  (-> (session)
    (get-input)
    (parse-input)))


(def part1 (-> init-state
    (assoc :worry-fac (/ 1 3))
    (round-state-n 20)
    (monkey-business)))

(def part2 (-> init-state
    (assoc :worry-fac 1)
    (round-state-n 10000)
    (monkey-business)))

(do
  (println "Part1" part1)
  (println "Part2" part2))
