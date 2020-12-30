(ns fctorial.utils
  (:require [clojure.core.async :as async :refer [<! >! <!! >!! go-loop chan close!]]
            [clojure.set :refer :all]))

(defn zip-colls [& cs]
  (partition (count cs)
             (apply interleave cs)))

(defn iter-n [coll n]
  (apply zip-colls
         (map
           #(drop % coll)
           (range n))))

(defn rand-range [n]
  (map
    (fn [_] (rand-int 100))
    (range n)))

(defn minimax [nums]
  (reduce
    (fn [[mx mn] num]
      [(max mx num) (min mn num)])
    [Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY]
    nums))

(defn manhatten [a b]
  (->> (map - a b)
       (map #(Math/abs %))
       (apply +)))

(defn -< [ip ops]
  (go-loop []
    (let [val (<! ip)]
      (when val
        (doseq [op ops]
          (>! op val))
        (recur)))))

(defn >- [ips op]
  (let [ip (async/merge ips)]
    (go-loop []
      (let [val (<! ip)]
        (when val
          (>! op val)
          (recur))))))

(defn closed-chan []
  (let [ch (chan)]
    (close! ch)
    ch))

(defn chan->seq [ch]
  (let [val (<!! ch)]
    (if val
      (lazy-seq (cons val (chan->seq ch))))))

(defn primes-below [n]
  (let [lim (Math/sqrt n)]
    (loop [curr 3
           seive (transient (vec (concat [false false true]
                                         (take (- n 2) (cycle [true false])))))]
      (let [nxt (+ 2 curr)]
        (if (> curr lim)
          (filter
            #(seive %)
            (range (inc n)))
          (if (seive curr)
            (recur nxt
                   (reduce
                     #(assoc! %1 (* %2 curr) false)
                     seive
                     (range 2 (/ (inc n) curr))))
            (recur nxt
                   seive)))))))

(def factorize
  (memoize
    (fn [n primes]
      (if (< n 2)
        {}
        (let [pr (first
                   (drop-while #(not= 0 (mod n %)) primes))
              [pr_pow left] (loop [pr_pow 0
                                   curr n]
                              (if (not= 0 (mod curr pr))
                                [pr_pow curr]
                                (recur (inc pr_pow) (/ curr pr))))]
          (assoc (factorize left primes) pr pr_pow))))))

(defn lcm [& nums]
  (let [prs (primes-below (apply max nums))
        fs (map #(factorize % prs) nums)
        lcm (reduce #(merge-with max %1 %2) fs)]
    (apply *
           (map
             (fn [[p c]]
               (apply * (repeat c p)))
             lcm))))

(defn hcf [& nums]
  (let [prs (primes-below (apply max nums))
        fs (map #(factorize % prs) nums)
        common (apply intersection (->> fs
                                        (map keys)
                                        (map set)))
        hcf (select-keys
              (reduce #(merge-with min %1 %2) fs)
              common)]
    (apply *
           (map
             (fn [[p c]]
               (apply * (repeat c p)))
             hcf))))

(defn lmap [f & colls]
  (fn [i]
    (apply f (map #(% i) colls))))

(defn split-n [n coll]
  (loop [fst (transient [])
         scnd coll
         left n]
    (if (zero? left)
      [(persistent! fst) scnd]
      (if (empty? scnd)
        (throw (new IndexOutOfBoundsException))
        (recur (conj! fst (first scnd))
               (rest scnd)
               (dec left))))))

(defn split-n-all [n coll]
  (let [[a b] (split-n n coll)]
    (if (empty? b)
      (list a)
      (lazy-seq (cons a (split-n-all n b))))))

(defn subvec-n [v s n]
  (subvec v s (+ s n)))

(defn take-exactly [n coll]
  (if (pos-int? n)
    (if (empty? coll)
      (throw (new Exception))
      (lazy-seq (cons (first coll)
                      (take-exactly (dec n)
                                    (rest coll)))))))

(defn to-byte [n]
  (if (> n 127)
    (byte (- n 256))
    (byte n)))

(defn pow [a b]
  (loop [res 1
         left b]
    (if (zero? left)
      res
      (recur (* res a) (dec left)))))

(defn to-flags [n]
  (for [i (range 63)
        :when (not= 0 (bit-and n (pow 2 i)))]
    i))

(defn geta [m k]
  (second (first (filter
            (fn [[K _]]
              (= K k))
            m))))

(defn assoca [m k v]
  (let [idx (first (filter (range #(= (first %) k) (count m))))]
    (if (nil? idx)
      (conj m [k v])
      (assoc-in m [idx k] v))))

(defn disja
  ([m k] (vec (filter #(not= (first %) k) m)))
  ([m k & ks] (reduce (fn [res k] (disja res k)) m (cons k ks))))

(defn map-vals [f m]
  (into {}
        (map (fn [[k v]] [k (f v)]) m)))
