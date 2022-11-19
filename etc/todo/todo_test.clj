(require '[clojure.test :refer [deftest testing is]]
         '[todo])

(deftest simple-parsing
  (testing "with when"
    (is (= {:todo "cool stuff"
            :when "tomorrow"
            :deadline nil}
           (todo/parse "cool stuff @tomorrow"))))

  (testing "just todo"
    (is (= {:todo "ah ok"
            :when nil
            :deadline nil}

           (todo/parse "    ah ok    "))))
  (testing "with deadline only"
    (is (= {:todo "cool stuff"
            :when nil
            :deadline "tomorrow"}
           (todo/parse "cool stuff @!tomorrow")))))

(deftest when-expressions
  (is (= {:todo "foo bar"
          :when "next wednesday"
          :deadline nil}
         (todo/parse "foo bar @'next wednesday'"))))

(deftest deadline-expressions
  (is (= {:todo "foo bar"
          :when nil
          :deadline "next wednesday"}
         (todo/parse "foo bar @!'next wednesday'"))))

(clojure.test/run-tests)
