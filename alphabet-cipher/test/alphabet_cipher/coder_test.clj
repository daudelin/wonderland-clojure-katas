(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]))

(defn equality-test
  "Builds a simple equality test from the provided test function"
  [test-fn]
  (fn [expected-val test-fn-args]
    (is (= expected-val (apply test-fn test-fn-args)))))

(deftest test-char-to-int
  (let [test-equal (equality-test char-to-int)]
    (testing "converts charaters to correct integer"
      (test-equal 0 [\a])
      (test-equal 1 [\b])
      (test-equal 2 [\c])
      (test-equal 25 [\z]))))

(deftest test-int-to-char
  (let [test-equal (equality-test int-to-char)]
    (testing "converts integers to correct characters"
      (test-equal \a [0])
      (test-equal \b [1])
      (test-equal \c [2])
      (test-equal \z [25])
      ;; ints > 25 should wrap back to \a
      (test-equal \f [31])
      (test-equal \o [40])
      (test-equal \w [100]))))

(deftest test-encode-char
  (let [test-equal (equality-test encode-char)]
    (testing "correctly encodes chars given a key"
      (test-equal \a [\a \a])
      (test-equal \b [\a \b])
      (test-equal \s [\g \m])
      (test-equal \o [\z \p]))))

(deftest test-decode-char
  (let [test-equal (equality-test decode-char)]
    (testing "correctly decodes chars given a key"
      (test-equal \a [\a \a])
      (test-equal \a [\b \b])
      (test-equal \s [\l \t])
      (test-equal \j [\p \g]))))

(deftest test-encode
  (testing "can encode given a secret keyword"
    (is (= "hmkbxebpxpmyllyrxiiqtoltfgzzv"
           (encode "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "egsgqwtahuiljgs"
           (encode "scones" "meetmebythetree")))))

(deftest test-decode
  (testing "can decode an cyrpted message given a secret keyword"
    (is (= "meetmeontuesdayeveningatseven"
           (decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv")))
    (is (= "meetmebythetree"
           (decode "scones" "egsgqwtahuiljgs")))))

(deftest test-decipher
  (testing "can extract the secret keyword given an encrypted message and the original message"
    (is (= "vigilance"
           (decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")))
    (is (= "scones"
           (decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs")))))
