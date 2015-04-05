(ns lt.plugins.upsource.util)

(defn comps [path]
  (filter (comp not empty?) (.split path "/")))

(defn file-in-revision [path]
  (let [cs (comps path)]
    {"projectId" (second cs)
     "revisionId" (nth cs 2)
     "fileName" (apply str (cons "/" (interpose "/" (drop 3 cs))))}))

(defn file-in-revisoin->path [fir]
  (str "!upsource/" (fir "projectId") "/" (fir "revisionId") (fir "fileName")))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))


