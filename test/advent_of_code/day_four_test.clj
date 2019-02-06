(ns advent-of-code.day-four-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-file [path]
  (with-open [rdr (io/reader path)]
    (doall (line-seq rdr))))

(defn parse-line [line]
  (let [date (re-find (re-matcher #"\d{4}-\d{2}-\d{2}" line))
        time (re-find (re-matcher #"\d{2}:\d{2}" line))
        activity (re-find (re-matcher #"[a-zA-Z]+.*" line))]
    {:date date :time time :activity activity}))

(def partitioned-records
  (->> "resources/day4.txt"
       read-file
       (map parse-line)
       (sort-by (juxt :date :time))
       (partition-by #(s/includes? % "Guard"))))

(defn group-2 [coll result]
  (if (empty? coll)
    result
    (recur (drop 2 coll) (conj result (concat (take 2 coll))))))

(defn get-time [{time :time}]
  (Integer/parseInt (last (s/split time #":"))))

(defn calculate-duration [details]
  (let [all-pos-result (map get-time details)
        even-indexed (take-nth 2 all-pos-result)
        odd-indexed (take-nth 2 (rest all-pos-result))
        interleaved-result (interleave (map #(* -1 %) even-indexed) odd-indexed)]
    (reduce + interleaved-result)))

(defn get-duration [[guard-entry details-entry]]
  (let [guard (re-find (re-matcher #"\d+" (:activity (last guard-entry))))
        duration (calculate-duration details-entry)]
    {:guard guard :duration duration}))

(def grouped-partitioned-records (group-2 partitioned-records []))

(def chosen-guard (->> grouped-partitioned-records
                       (map get-duration)
                       (group-by :guard)
                       (map val)
                       (map #(let [total-duration (reduce + (map :duration %))]
                               {:guard (:guard (first %))
                                :duration total-duration}))
                       (sort-by :duration)
                       last
                       :guard))

(def chosen-guard-activities (filter
                               #(= (str "Guard #" chosen-guard " begins shift")
                                   (:activity (first (first %))))
                               grouped-partitioned-records))

(defn get-status-timeline [activities]
  (->> activities
       (map rest)
       flatten))

(def result
  (let [timeline (group-2 (get-status-timeline chosen-guard-activities) [])]
    (->> timeline
         (map #(range (get-time (first %)) (get-time (last %))))
         flatten
         frequencies
         (sort-by val)
         last
         first
         (* (Integer/parseInt chosen-guard)))))

