(ns lorem-ipsum.core)

(require '[clojure.java.io :as io])

(declare att-cons att-vow att-start)

(def default-lang {:name "default"
                   :cons '("b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "z" "th")
                   :rare-cons '("bd" "bc" "bf" "bh" "bj" "bl" "br" "bs" "bv" "bz"
                                     "ch" "cj" "ck" "cl" "cm" "cn" "cr" "cs" "ct" "cv" "cx" "cz"
                                     "dd" "dg" "dh" "dj" "dk" "dl" "dm" "dn" "dq" "dr" "ds" "dv" "dw" "dz"
                                     "fb" "fc" "fd" "fh" "fk" "fl" "fn" "fp" "fr" "fs" "ft" "fz"
                                     "gh" "gl" "gm" "gn" "gr" "gs" "gz"
                                     "hr"
                                     "jh" "jl" "jn" "jr"
                                     "kh" "kl" "km" "kn" "kr" "ks" "kt" "kz"
                                     "lb" "ld" "lh" "lm" "ln" "lr" "ls" "lt" "lz"
                                     "mh" "ml" "mm" "mr" "mz" "mj" "mn"
                                     "nh" "nk" "nr" "nz" "nj" "nv"
                                     "pd" "pf" "ph" "pl" "pr" "ps" "pt" "pz" "pj"
                                     "rb" "rc" "rd" "rh" "rk" "rl" "rm" "rn" "rp" "rs" "rt" "rv" "rx" "rz"
                                     "sb" "sc" "sd" "sf" "sg" "sh" "sk" "sl" "sm" "sn" "sp" "sr" "ss" "st" "sv" "sz"
                                     "tc" "tf" "tj" "tk" "tl" "tm" "tn" "tr" "ts" "tv" "tz"
                                     "vh" "vg" "vj" "vk" "vl" "vm" "vn" "vq" "vr" "vs" "vt" "vz"
                                     "zb" "zc" "zd" "zg" "zh" "zk" "zl" "zm" "zn" "zp" "zr" "zt" "zv")
                   :vow '("a" "e" "i" "o" "u")
                   :rare-vow '("ay" "ae" "ai" "ao" "ah" "au"
                                    "ea" "ey" "ee" "ei" "eh"
                                    "io" "ia" "ie" "ih" "iu"
                                    "oa" "oe" "oo" "ou" "oh"
                                    "ua" "uo" "uu"
                                    "ya" "yu" "yo" "ye" "yi")

                   :cons-rarity 5
                   :vow-rarity 6})

(defn att-start [length language]
  (if (zero? (rand-int 2))
    (trampoline att-cons	(vec nil) length language)
    (trampoline att-vow		(vec nil) length language)))

(defn att-cons [coll length language]
  (if-not (or (neg? length) (zero? length))
    (let [c-rarity (language :cons-rarity)
          n (if (pos? c-rarity) (rand-int c-rarity) 1)]
      (cond
        (zero? n)	#(att-vow (conj coll (rand-nth (language :rare-cons)))
                            (dec (dec length))
                            language)
        :else		#(att-vow (conj coll (rand-nth (language :cons)))
                         (dec length)
                         language)))
    coll))

(defn att-vow [coll length language]
  (if-not (or (neg? length) (zero? length))
    (let [v-rarity (language :vow-rarity)
          n (if (pos? v-rarity) (rand-int v-rarity) 1)]
      (cond
        (zero? n) 	#(att-cons (conj coll (rand-nth (language :rare-vow)))
                              (dec (dec length))
                              language)
        :else		#(att-cons (conj coll (rand-nth (language :vow)))
                          (dec length)
                          language)))
    coll))

(defn gen-word [length language]
  (clojure.string/capitalize
   (apply str
          (take length
                (seq
                 (apply str
                        (att-start length language)))))))

(defn gen-word-list [n language mini maxi]
  (loop [n n
         lst ()
         w-length (+ mini (rand-int (- maxi mini)))]
    (cond
      (pos? n)	(recur
                (dec n)
                (cons (gen-word w-length language) lst)
                (+ mini (rand-int (- maxi mini))))
      :else lst)))

(defn genphrases [n language min-words max-words w-min-length w-max-length comma-freq semi-column-freq question-freq exclamation-freq new-par-freq]
  (loop [l (+ min-words (rand-int (- max-words min-words)))
         n n
         q-mark 	(if (pos? question-freq) 		(rand-int question-freq) 		1)
         e-mark 	(if (pos? exclamation-freq) 	(rand-int exclamation-freq) 	1)
         comma  	(if (pos? comma-freq) 			(rand-int comma-freq) 			1)
         semi-col	(if (pos? semi-column-freq) 	(rand-int semi-column-freq) 	1)
         new-par 	(if (pos? new-par-freq)			(rand-int new-par-freq)			1)
         after-comma? false
         § ["\t"]]
    (cond
      (pos? n)	(recur (+ min-words (rand-int (- max-words min-words)))
                      (dec n)
                      (if (pos? question-freq) 		(rand-int question-freq) 		1)
                      (if (pos? exclamation-freq) 		(rand-int exclamation-freq) 	1)
                      (if (pos? comma-freq) 			(rand-int comma-freq) 			1)
                      (if (pos? semi-column-freq) 		(rand-int semi-column-freq) 	1)
                      (if (pos? new-par-freq)			(rand-int new-par-freq)			1)
                      (or (zero? comma) (zero? semi-col))
                      (conj § (str ((if-not after-comma? #(clojure.string/capitalize %) #(clojure.string/lower-case %))
                                    (apply str
                                           (interpose " "
                                                      (map clojure.string/lower-case
                                                           (gen-word-list l language w-min-length w-max-length))))) (cond
                                                                                                                      (and
                                                                                                                       (zero? comma)
                                                                                                                       (not= 1 n))		", "
                                                                                                                      (and
                                                                                                                       (zero? semi-col)
                                                                                                                       (not= 1 n))		"; "
                                                                                                                      (zero? q-mark)	"? "
                                                                                                                      (zero? e-mark)	"! "
                                                                                                                      :else 			". ")
                                   (if (and (not (zero? comma)) (not (zero? semi-col)) (zero? new-par)) "\n\r\t" ""))))

      :else (apply str §))))

(defn -main
  [& _]
  (spit "./out/loremipsum.txt" 	(genphrases 600 default-lang 2 15 3 11 8 20 10 15 1000)))




































