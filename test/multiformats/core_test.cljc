(ns multiformats.core-test
  "Correctness pinned to the canonical `ipfs add --cid-version=1 --raw-leaves`
   output (these vectors were minted by real go-ipfs/kubo) plus encode/decode
   round-trips. No network, no ipfs CLI at test time."
  (:require [clojure.string :as str]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])
            [multiformats.core :as mf]))

(defn- utf8-bytes [s]
  #?(:clj (.getBytes ^String s "UTF-8")
     :cljs (.encode (js/TextEncoder.) s)))

(defn- bytes-of [ints]
  #?(:clj (byte-array (map unchecked-byte ints))
     :cljs (js/Uint8Array. (clj->js ints))))

(defn- empty-bytes []
  #?(:clj (byte-array 0) :cljs (js/Uint8Array. 0)))

;; ── CIDv1-raw vs go-ipfs (`ipfs add -Q --cid-version=1 --raw-leaves`) ─────────
(deftest cidv1-raw-matches-ipfs
  (is (= "bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku"
         (mf/cidv1-raw (empty-bytes))) "empty input")
  (is (= "bafkreicysg23kiwv34eg2d7qweipxwosdo2py4ldv42nbauguluen5v6am"
         (mf/cidv1-raw (utf8-bytes "hello\n"))))
  (is (= "bafkreifsjhh4xb4ct3q652hbcbayexs57mu46imyrjxua4r6ofgft3qmv4"
         (mf/cidv1-raw (utf8-bytes "multiformats-clj")))))

;; ── multihash framing ─────────────────────────────────────────────────────────
(deftest multihash-sha256-frames-0x12-0x20
  (let [mh (mf/multihash-sha256 (empty-bytes))]
    ;; `alength`, not `count` -- `count` has no ICounted impl for a raw
    ;; Uint8Array on :cljs (works fine for byte-array on :clj, and for
    ;; :cljs's own JS Array, just not TypedArray); `alength` works on both.
    (is (= 34 (alength mh)))
    (is (= 0x12 (bit-and (aget mh 0) 0xff)) "sha2-256 code")
    (is (= 0x20 (bit-and (aget mh 1) 0xff)) "32-byte length")
    ;; sha256("") is the well-known e3b0c442…
    (is (= "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
           (mf/hexify (bytes-of (drop 2 (seq mh))))))))

;; ── SHA-384 ───────────────────────────────────────────────────────────────────
;; Vectors are the published FIPS 180-4 ones, verified against an independent
;; implementation before being written here rather than recalled.

(deftest sha384-matches-the-fips-180-4-vectors
  (is (= 48 (alength (mf/sha384 (empty-bytes)))) "384 bits")
  (is (= (str "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da"
              "274edebfe76f65fbd51ad2f14898b95b")
         (mf/hexify (mf/sha384 (empty-bytes))))
      "the empty string")
  (is (= (str "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed"
              "8086072ba1e7cc2358baeca134c825a7")
         (mf/hexify (mf/sha384 (utf8-bytes "abc"))))
      "\"abc\""))

(deftest sha384-is-not-a-truncated-sha512
  ;; SHA-384 is SHA-512 with DIFFERENT initial state, so truncating SHA-512 gives
  ;; a different digest. Asserting the two disagree keeps anyone from "optimising"
  ;; this into (take 48 (sha512 x)) later.
  (is (not= (subs "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd" 0 96)
            (mf/hexify (mf/sha384 (utf8-bytes "abc"))))
      "truncated SHA-512(\"abc\") must NOT equal SHA-384(\"abc\")"))

(deftest sha384-and-sha256-are-different-functions
  (is (not= (mf/hexify (mf/sha256 (utf8-bytes "abc")))
            (mf/hexify (mf/sha384 (utf8-bytes "abc")))))
  (is (= 32 (alength (mf/sha256 (empty-bytes))))))

;; ── varint ────────────────────────────────────────────────────────────────────
(deftest varint-unsigned
  (is (= [0x00] (map #(bit-and % 0xff) (mf/varint 0))))
  (is (= [0x55] (map #(bit-and % 0xff) (mf/varint 0x55))))       ; raw codec, single byte
  (is (= [0x71] (map #(bit-and % 0xff) (mf/varint 0x71))))       ; dag-cbor codec
  (is (= [0x80 0x01] (map #(bit-and % 0xff) (mf/varint 128))))   ; multi-byte boundary
  (is (= [0xac 0x02] (map #(bit-and % 0xff) (mf/varint 300)))))

;; ── base58btc round-trip + leading-zero preservation ──────────────────────────
;; `vec`, not `seq` -- comparing raw seqs over byte-array/Uint8Array isn't
;; reliably `=`-comparable on every platform; `vec` always is.
(deftest base58-roundtrip
  (doseq [s ["" "Satoshi" "the quick brown fox"]]
    (let [b (utf8-bytes s)]
      (is (= (vec b) (vec (mf/base58btc-decode (mf/base58btc b)))))))
  ;; leading zero bytes → leading '1's (33 → alphabet index 33 = 'a')
  (is (= "11a" (mf/base58btc (bytes-of [0 0 33]))))
  (is (= (vec (bytes-of [0 0 33]))
         (vec (mf/base58btc-decode "11a"))))
  (is (= [0] (vec (mf/base58btc-decode "1"))) "'1' decodes to a single zero byte"))

;; ── base32 round-trip ─────────────────────────────────────────────────────────
(deftest base32-roundtrip
  (doseq [s ["" "f" "fo" "foo" "foob" "multiformats"]]
    (let [b (utf8-bytes s)]
      (is (= (vec b) (vec (mf/base32-decode (mf/base32 b))))))))

;; ── hex round-trip + odd-length rejection ─────────────────────────────────────
(deftest hex-roundtrip
  (doseq [ints [[] [0] [0xab] [0 1 2 250 255]]]
    (let [b (bytes-of ints)]
      (is (= ints (map #(bit-and % 0xff) (mf/unhex (mf/hexify b))))))))

(deftest unhex-rejects-odd-length-hex-strings
  ;; An odd number of hex digits is never valid encoded byte data;
  ;; `(partition 2 s)` would otherwise silently drop the trailing nibble
  ;; instead of erroring -- must fail loudly, not quietly decode a
  ;; shorter-than-intended byte array.
  (is (thrown? #?(:clj Exception :cljs js/Error) (mf/unhex "1")))
  (is (thrown? #?(:clj Exception :cljs js/Error) (mf/unhex "abc"))))

(deftest base58btc-decode-rejects-invalid-characters
  ;; b58-idx returns nil for a character outside the base58btc alphabet, and
  ;; that nil used to flow silently into `+`/`pos?` arithmetic: on :clj this
  ;; throws (fails closed, but only by accident of JVM unboxing -- NPE, not
  ;; a deliberate guard); on :cljs (confirmed via a real compiled build)
  ;; `+`/`pos?` silently coerce nil to 0, so an invalid FIRST character
  ;; silently decoded to an EMPTY byte vector, and an invalid character
  ;; later in the string silently decoded to the WRONG bytes -- neither
  ;; case raised anything. "0", "O", "I", "l" are excluded from base58btc's
  ;; alphabet specifically to avoid visual ambiguity with "o"/"0"/"1"/"I".
  (doseq [bad ["0" "O" "I" "l" "+" "/"]]
    (is (thrown? #?(:clj Exception :cljs js/Error) (mf/base58btc-decode bad))
        (str "must reject invalid base58btc character as sole char: " bad)))
  (is (thrown? #?(:clj Exception :cljs js/Error) (mf/base58btc-decode "zz0zz"))
      "must reject invalid base58btc character mid-string, not silently
       decode wrong bytes"))

(deftest base32-decode-rejects-invalid-characters
  ;; b32-idx returns nil for a character outside the base32 alphabet, and
  ;; (int nil) throws on :clj (fails closed) but silently returns 0 on
  ;; :cljs (confirmed via a real compiled build) -- an invalid character
  ;; used to silently decode as if it were 'a' (alphabet index 0) on
  ;; :cljs instead of raising. "1", "0", "8", "9", and uppercase letters
  ;; are all outside this lowercase-only, no-padding RFC 4648 alphabet.
  (doseq [bad ["1" "0" "8" "9" "A" "="]]
    (is (thrown? #?(:clj Exception :cljs js/Error) (mf/base32-decode bad))
        (str "must reject invalid base32 character: " bad))))

;; ── CID decode round-trips its bytes ──────────────────────────────────────────
(deftest cid-bytes-roundtrip
  (let [c (mf/cidv1-raw (utf8-bytes "round-trip"))]
    (is (= c (str "b" (mf/base32 (mf/cid->bytes c)))))
    ;; the decoded prefix is version=1, codec=raw(0x55), mh=sha2-256(0x12),len=32(0x20)
    (let [bs (mf/cid->bytes c)]
      (is (= [0x01 0x55 0x12 0x20] (map #(bit-and % 0xff) (take 4 (seq bs))))))))

;; ── dag-cbor / kotoba-cid produce a bafyrei… (dag-cbor) CID ────────────────────
(deftest kotoba-cid-is-dag-cbor
  (let [c (mf/kotoba-cid "ibuki")]
    (is (str/starts-with? c "bafyrei"))
    (is (= [0x01 0x71 0x12 0x20] (map #(bit-and % 0xff) (take 4 (seq (mf/cid->bytes c))))))))

;; ── base64url, no padding (RFC 4648 §5) ───────────────────────────────────────
;; The RFC 4648 §10 test vectors, which pin the no-padding decision: standard
;; base64 would render "f" as "Zg==".
(deftest base64url-rfc4648-vectors
  (doseq [[input expected] [["" ""]
                            ["f" "Zg"]
                            ["fo" "Zm8"]
                            ["foo" "Zm9v"]
                            ["foob" "Zm9vYg"]
                            ["fooba" "Zm9vYmE"]
                            ["foobar" "Zm9vYmFy"]]]
    (is (= expected (mf/base64url (utf8-bytes input))) (str "encode " (pr-str input)))
    (is (= (vec (map #(bit-and % 0xff) (seq (utf8-bytes input))))
           (vec (map #(bit-and % 0xff) (seq (mf/base64url-decode expected)))))
        (str "decode " (pr-str expected)))))

(deftest base64url-uses-the-url-alphabet
  ;; Indices 62 and 63 are '-' and '_' here, where standard base64 has '+' and '/'.
  ;; Getting this wrong yields a value that is not URL-safe and not multibase `u`.
  (is (= "-___" (mf/base64url (bytes-of [0xfb 0xff 0xff]))))
  (is (= [0xfb 0xff 0xff]
         (vec (map #(bit-and % 0xff) (seq (mf/base64url-decode "-___"))))))
  (is (not (str/includes? (mf/base64url (bytes-of [0xfb 0xff 0xff])) "+")))
  (is (not (str/includes? (mf/base64url (bytes-of [0xfb 0xff 0xff])) "/"))))

(deftest base64url-round-trips-every-byte-value
  (let [all (bytes-of (range 256))]
    (is (= (vec (range 256))
           (vec (map #(bit-and % 0xff) (seq (mf/base64url-decode (mf/base64url all)))))))))

(deftest base64url-tolerates-padding-on-input
  ;; Older private encoders in this ecosystem emit padding; decoding must still
  ;; work so their output remains readable.
  (is (= [102] (vec (map #(bit-and % 0xff) (seq (mf/base64url-decode "Zg==")))))))

(deftest base64url-rejects-out-of-alphabet-characters
  ;; Must throw on BOTH hosts: on :cljs a nil alphabet index fed to bit-or is
  ;; silently 0, which would decode an invalid character as 'A'.
  (is (thrown? #?(:clj Exception :cljs :default) (mf/base64url-decode "Zg*v")))
  (is (thrown? #?(:clj Exception :cljs :default) (mf/base64url-decode "Zm9+")))
  (is (thrown? #?(:clj Exception :cljs :default) (mf/base64url-decode "Zm9/"))))
