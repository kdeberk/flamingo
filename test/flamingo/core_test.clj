(ns flamingo.core-test
  (:require [flamingo.core :as sut]
            [clojure.test :as t]))

(t/deftest test-replace-symbols
  (let [m {'a 2 'b 3}]
    (t/is (= (sut/replace-symbols m '(+ 1 2 (/ 5 a)))
             '(+ 1 2 (/ 5 2))))
    (t/is (= (sut/replace-symbols m '([1 2 3 a] a))
             '([1 2 3 2] 2)))
    (t/is (= (sut/replace-symbols m '({a 2 :b 1 :c (+ a 1)} a))
             '({a 2 :b 1 :c (+ 2 1)} 2)))
    (t/is (= (sut/replace-symbols m '(let [c (+ a 1) a (+ a 2)]
                                       (+ a b c)))
             '(clojure.core/let [c (+ 2 1) a (+ 2 2)]
                (+ a 3 c))))))
