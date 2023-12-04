#!/usr/bin/env janet

(import spork/misc :as utils)

(def input
  (->> (slurp "day1.txt")
       (string/split "\n")
       (filter |(not (= (length $) 0)))))


(def example-1 @["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"])
(def example-2 @["two1nine" "eightwothree" "abcone2threexyz" "xtwone3four" "4nineeightseven2" "zoneight234" "7pqrstsixteen"])

(def grammar-1 (peg/compile '(some (+ (<- :d) :a))))

(def grammar-2 (peg/compile
		~(some (+
			(/ "one" "1")
			(/ "two" "2")
			(/ "three" "3")
			(/ "four" "4")
			(/ "five" "5")
			(/ "six" "6")
			(/ "seven" "7")
			(/ "eight" "8")
			(/ "nine" "9")
			(<- :d)
			:a))))

(defn calibrate [arr]
  (if (= (length arr) 1)
    (array/push arr (first arr)))
  (utils/string->int (string/join (array (first arr) (last arr)))))

# part 1
(pp (sum (map |(->> (peg/match grammar-1 $) (calibrate)) input)))

# part 2
(each line example-2
  (def parsed (peg/match grammar-2 line))
  (print(string/format "%m -> %m -> %m" line parsed (calibrate parsed))))
  
(pp (sum (map |(->> (peg/match grammar-2 $) (calibrate)) example-2)))
