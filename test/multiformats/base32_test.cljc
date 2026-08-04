(ns multiformats.base32-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])
            [multiformats.base32 :as base32]))

(deftest lightweight-codec-round-trips-without-hash-namespace
  (doseq [bytes [[] [0] [0 1 2 127 128 255]]]
    (is (= bytes
           (mapv #(bit-and % 0xff)
                 (seq (base32/decode (base32/encode bytes))))))))

(deftest lightweight-codec-rejects-non-alphabet-input
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (base32/decode "abc!"))))
