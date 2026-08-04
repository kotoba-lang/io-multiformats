(ns multiformats.base32
  "Lightweight RFC 4648 base32 codec used by CID wire adapters.

  This namespace deliberately has no hashing or npm dependency. Consumers that
  only parse or re-emit an existing CID must not load the SHA implementation."
  (:refer-clojure :exclude [decode]))

(def ^:private alphabet "abcdefghijklmnopqrstuvwxyz234567")
(def ^:private alphabet-index
  (into {} (map-indexed (fn [i c] [c i]) alphabet)))

(defn- alphabet-char [i]
  #?(:clj (.charAt ^String alphabet (int i))
     :cljs (.charAt alphabet i)))

(defn encode
  "Bytes to lowercase RFC 4648 base32 without padding."
  [bytes]
  (let [bits (mapcat (fn [byte]
                       (let [v (bit-and (int byte) 0xff)]
                         (map #(bit-and (bit-shift-right v %) 1)
                              [7 6 5 4 3 2 1 0])))
                     (seq bytes))]
    (->> bits
         (partition 5 5 nil)
         (map (fn [chunk]
                (let [padded (concat chunk (repeat (- 5 (count chunk)) 0))]
                  (alphabet-char
                   (reduce (fn [acc bit] (+ (* acc 2) bit)) 0 padded)))))
         (apply str))))

(defn decode
  "Lowercase RFC 4648 base32 without padding to bytes; reject bad input."
  [text]
  (let [out (loop [chars (seq text) buffer 0 bit-count 0 acc []]
              (if (empty? chars)
                acc
                (let [ch (first chars)
                      idx (or (alphabet-index ch)
                              (throw (ex-info "multiformats: invalid base32 character"
                                              {:char ch})))
                      buffer (bit-or (bit-shift-left buffer 5) idx)
                      bit-count (+ bit-count 5)]
                  (if (>= bit-count 8)
                    (recur (rest chars) buffer (- bit-count 8)
                           (conj acc
                                 (bit-and
                                  (unsigned-bit-shift-right buffer (- bit-count 8))
                                  0xff)))
                    (recur (rest chars) buffer bit-count acc)))))]
    #?(:clj (byte-array out) :cljs (vec out))))
