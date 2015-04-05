(ns lt.plugins.upsource.filesystem
  (:require [lt.object :as object]
            [lt.objs.tabs :as tabs]
            [lt.objs.command :as cmd]
            [lt.util.load :as load]
            [lt.objs.workspace :as workspace]
            [lt.objs.sidebar.workspace :as sidebar.workspace]
            [lt.objs.files :as files]
            [lt.objs.document :as document]
            [lt.objs.editor :as editor]
            [lt.objs.editor.pool :as pool]
            [lt.objs.opener :as opener]
            [lt.plugins.upsource.util :as util]
            [lt.plugins.upsource.rpc :as rpc])
  (:require-macros [lt.macros :refer [defui behavior]]))


(def upsource-fs-root "!upsource")

(behavior ::init
          :triggers #{:init}
          :reaction (fn [this]
                      (object/merge! this {:fs-type :upsource
                                           :filesystem (atom {upsource-fs-root {:dir? true
                                                                                :file? false}})
                                           :accept (fn [this path] (.startsWith path upsource-fs-root))})
                      ))

(defn init-path [path]
  (fs-merge path {:dir? true
                    :stat {}
                    :file? false}))

(comment
  (files/fs-for-path* "!upsource/graphs/d992c15a2fcc3318982cc52e7a62a31377da8e4b")
  )



(defn upsource-fs [path]
  (files/fs-for-path* path))

(defn fs-path [path]
  (interpose :files (util/comps path)))

(defn fs-get [path]
  (get-in @(upsource-fs path) (fs-path path)))

(defn fs-merge [path file]
  (let [merged (merge (fs-get path) file)]
    (swap! (upsource-fs path) assoc-in (fs-path path) merged)))

(defn fs-remove [path k]
  (swap! (upsource-fs path) util/dissoc-in (conj (fs-path path) k)))

(defn fs-fire! [path]
  (let [alert (:alert (fs-get path))]
    (when alert (alert (files/stat path) nil))))


(defmethod files/bomless-read-async :upsource [path cb]
  (let [content (:content (fs-get path))]
    (if content
      (cb content)
      (load-file-async path (fn [text]
                              (cb text)
                              (fs-fire! path))))))

(defmethod files/bomless-read :upsource [path]
  (let [content (:content (fs-get path))]
    (if content
      content
      (do
        (load-file-async path (fn [text]
                                (fs-fire! path)))
        "Loading..."))))

(defn load-file-async [path cb]
  (rpc/req :getFileContent {"file" (util/file-in-revision path)
                        "requestMarkup" false
                        "requestFolding" false}
           {:fileContent (fn [content]
                           (fs-merge path {:content (content "text")
                                           :mtime (js/Date.)})
                           (cb (content "text")))}))

(defn fs-append-item! [file]
  (fs-merge (:path file) (:file file)))

(defn fs-append-subtree! [result fir]
  (doseq [item (result "items")]
    (let [dir? (item "isDirectory")
          fir (assoc fir "fileName" (item "fileId"))]
      (fs-append-item! {:path (util/file-in-revisoin->path fir)
                        :file {:dir? dir?
                               :file? (not dir?)
                               :stat #js{}
                               :mtime (js/Date. 0)}}))))

(defmethod files/read-dir :upsource [path]
  (let [files (keys (:files (fs-get path)))]
    (if files
      files
      (do
        (let [fir (util/file-in-revision path)]
          (rpc/req :getProjectSubtree fir
                   (fn
                     ([result]
                      (fs-append-subtree! result fir)
                      (fs-fire! path))
                     ([_ error] (println error)))))
        []))))

(defmethod files/exists? :upsource [path]
  (fs-get path))


(defmethod files/stats :upsource [path]
  (files/stat path))

(defmethod files/dir? :upsource [path]
  (let [file (fs-get path)]
    (or (:dir? file) (:files file))))

(defmethod files/file? :upsource [path]
  (not (:dir? (fs-get path))))


(defmethod files/writable? :upsource [path]
  false)

(defmethod files/resolve :upsource [base cur]
  cur)


(defmethod files/real-path :upsource [c]
  c)


(defmethod files/write-file :upsource [path content]
  )


(defmethod files/append-file :upsource [path content]
  )


(defmethod files/delete! :upsource [path]
  )


(defmethod files/move! :upsource [from to]
  )


(defmethod files/copy :upsource [from to]
  )


(defmethod files/mkdir :upsource [path]
  )


(defmethod files/unwatch :upsource [path alert]
  (fs-remove path :alert))


(defmethod files/watch :upsource [path options alert]
  (fs-merge path {:alert alert}))


(defmethod files/stat :upsource [path]
  {"mtime" (:mtime (fs-get path))})


(defmethod files/write-stream :upsource [path]
  )


(defmethod files/read-stream :upsource [path]
  )
