(ns india-covid-extractor.core
  (:gen-class))

(require '[cheshire.core :as cc])

(def statedata 
    (cc/parse-string 
	    (slurp "https://api.covid19india.org/data.json") true))
		
(def statedata 
	(statedata :statewise))
	
(def districtdata 
    (vec (cc/parse-string 
	    (slurp "https://api.covid19india.org/v2/state_district_wise.json") true)))

		
(defn mergestate [data state]
    (loop [data data
	       index 0
		   found false]
		(if (or found (= index (count data)))
		    data
			(let [districtval (nth data index)
			      statematch (= (districtval :state) (state :state))
				  data 
			        (if statematch
				        (assoc data index (merge districtval state))
					    data)]
				(recur data (inc index) statematch)))))


(defn merge-data [statedata districtdata]
    (loop [data districtdata
	       statedata statedata]
	    (if (= 0 (count statedata))
		    data
			(let [state (first statedata)]
                (recur 
				    (mergestate data state)
					(rest statedata))))))
					
(def mergeddata (merge-data districtdata statedata))



(defn generaterecord 
	([data] (apply str (interpose "\t" [(data :state) (data :confirmed) (data :recovered) (data :deaths) "" ""])))
	([line data]
		(let [district (nth (data :districtData) line)]
			(if (= 0 line)
				(apply str (interpose "\t" [(data :state) (data :confirmed) (data :recovered) (data :deaths) (district :district) (district :confirmed)]))
				(apply str (interpose "\t" [(data :state) "" "" "" (district :district) (district :confirmed)]))))))
			
(defn getsinglerecord [data]
	(if  ((complement =) "Total" (data :state))
		(if (nil? (data :districtData))
			(concat "\r\n" (generaterecord data))
			(loop [line 0
				 csv ""]
				(if (= line (count (data :districtData)))
					 csv
					 (recur (inc line) (concat csv "\r\n" (generaterecord line data))))))))
			
(defn transformmap [data header]
    (loop [data data
	       csv header]
	      (if (= 0 (count data))
		      (concat csv "\r\n")
			  (recur (rest data) (concat csv (getsinglerecord (first data)))))))
			  
(def csvdata (transformmap mergeddata "sep=\r\nState\tCases\tCured\tDeaths\tDistrict\tCount"))

(use 'clojure.java.io)
(def covidfilecsv (str (System/getProperty "java.io.tmpdir") "covid19india.csv"))
(with-open [w (clojure.java.io/writer covidfilecsv)]
  (.write w (apply str csvdata)))
  
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "File generated at" covidfilecsv))
