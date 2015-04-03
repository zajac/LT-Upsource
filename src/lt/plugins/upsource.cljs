(ns lt.plugins.upsource
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
            [lt.objs.opener :as opener])
  (:require-macros [lt.macros :refer [defui behavior]]))

(def io (load/node-module "socket.io/node_modules/socket.io-client"))

(def upsource-fs-root "!upsource")

(defui hello-panel [this]
  [:h1 "Hello World from upsource!"]
)

(defui project-item [project]
  [:div [:button (project "projectName")]]
  :click (fn [evt] (open-project (project "projectId"))))

(defn open-project [projectId]
  (req :getRevisionsList {"projectId" projectId
                          "limit" 100}
       (fn [result]
         (let [revision-list (object/create ::revision-list result projectId)]
           (tabs/add-or-focus! revision-list)))))

(defui project-list-view [this]
  [:div
   (map project-item
        (@this :projects))])

(defui add-to-workspace-button [revision projectId]
  [:button "Add to workspace"]
  :click (fn [evt] (add-to-workspace revision projectId)))

(defn add-to-workspace [revision projectId]
  (let [path (str upsource-fs-root "/" projectId "/" (revision "revisionId"))]
    (fs-merge path {:dir? true
                    :stat {}
                    :file? false})
    (object/raise workspace/current-ws :add.folder! path)))

(defui revision-item [revision projectId]
  [:div (str "commit: " (revision "revisionIdShort")
             " date: " (revision "revisionDate")
             " message: " (revision "revisionCommitMessage"))
   (add-to-workspace-button revision projectId)])

(defui revision-list-view [this]
  [:div
   (map (fn [revision] (revision-item revision (:projectId @this)))
        (:revisions @this))])

(object/object* ::upsource
                :tags [:upsource :filesystem]
                :fs-type :upsource
                :behaviors []
                :callbacks (atom {})
                :request-id (atom 0)
                :filesystem (atom {upsource-fs-root {:dir? true
                                                     :file? false}})
                :accept (fn [this path] (.startsWith path upsource-fs-root)))

(behavior ::on-close-destroy
          :triggers #{:close}
          :reaction (fn [this]
                      (object/raise this :destroy)))


(object/object* ::project-list
                :tags [:project-list]
                :behaviors [::on-close-destroy]
                :projects []
                :init (fn [this projects]
                        (object/merge! this {:projects (projects "project")})
                        (project-list-view this)))


(object/object* ::revision-list
                :tags [:revision-list]
                :behaviors [::on-close-destroy]
                :revisions []
                :projectId nil
                :init (fn [this revisions projectId]
                        (object/merge! this {:revisions (revisions "revision")
                                             :projectId projectId})
                        (revision-list-view this)))

(def upsource (object/create ::upsource))



(defn token [user passwd]
  (str "Basic " (js/btoa (str user ":" passwd))))

(behavior ::connect
          :triggers #{:upsource-connect}
          :desc "Upsource: connect to server"
          :params [{:label "URL"}
                   {:label "User"}
                   {:label "Password"}]
          :exclusive true
          :fs-type :upsource
          :reaction (fn [this url user passwd]
                      (let [socket (.connect io url, #js  {"reconnection" "false"
                                                           "forceNew" "true"
                                                           "path" "/~socket.io"})]
                        (object/merge! this {:socket socket
                                             :token (token user passwd)})
                        (.on socket "message" (fn [msg] (process-message-str this msg))))))


(defn process-message-str [upsource message-str]
  (let [message (js->clj (.parse js/JSON message-str))
        callbacks (@upsource :callbacks)
        request-id (message "X-Request-ID")
        callback (@callbacks request-id)]
    (if callback
      (if (process-message message callback)
        (swap! callbacks dissoc request-id)))))

(comment
  (def fuck (atom {1 2}))
  (swap! fuck dissoc 1)
  )

(defn process-message [message callback]
  (dispatch message
            (message "data")
            callback))


(defmulti dispatch (fn [message data callback] (keyword (message "event"))))

(defmethod dispatch :RpcResult [message data callback]
   (let [{result "result", error "error"} data]
     (cond
      result (callback result)
      error (callback :error error)))
  true)



(defmethod dispatch :RpcYield [message data callback]
  (let [{result "result", error "error"} data
        k-str (first (keys result))
        k (keyword k-str)
        handler (when k (k callback))]
     (cond
      result (when handler (handler (result k-str)))
      error (let [eh (:error callback)]
              (when eh (eh error)))))
  false)

(defmethod dispatch :RpcYieldFinished [message data callback]
  (let [res-h (:finished! callback)]
    (when res-h (res-h)))
  true)

(comment
  (req :getProjectSubtree (file-in-revision "!upsource/graphs/d992c15a2fcc3318982cc52e7a62a31377da8e4b/src/graph")
       (fn [results] (print (fs-from-project-subtree results))))


  (req :getFileContent {"file" (file-in-revision "!upsource/graphs/d992c15a2fcc3318982cc52e7a62a31377da8e4b/src/graph/Edge.java")
                        "requestMarkup" true
                        "requestFolding" false}
       {:finished! (fn [] (println "finished!"))
        :fileContent (fn [content]
                       )
        :referenceMarkup println})
  )

(defn get-upsource []
  (if (not (@upsource :socket))
    (object/raise upsource :upsource-connect))
  upsource)



(cmd/command {:command :upsource-project-list
              :desc "Upsource: Projects List"
              :exec (fn []
                      (req :getAllProjects (fn [projects]
                                             (let [project-list (object/create ::project-list projects)]
                                               (tabs/add-or-focus! project-list)))))})



(defn req
  ([method callback]
   (req method {} callback))
  ([method data callback]
   (let [up (get-upsource)
         request-id (swap! (@up :request-id) inc)
         callbacks (@up :callbacks)
         socket (@up :socket)]


     (swap! callbacks assoc request-id callback)
     (.send socket (create-request-str method data (@up :token) request-id)))))

(defn create-request-str [method data token request-id]
  (.stringify js/JSON #js {"type" "rpc"
                           "method" (name method)
                           "X-Request-ID" request-id
                           "data" (clj->js data)
                           "Authorization" token}))


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

(defn comps [path]
  (filter (comp not empty?) (.split path "/")))

(defn fs-path [path]
  (interpose :files (comps path)))

(defn fs-get [path]
  (get-in @(@upsource :filesystem) (fs-path path)))

(defn fs-merge [path file]
  (let [merged (merge (fs-get path) file)]
    (swap! (@upsource :filesystem) assoc-in (fs-path path) merged)))

(defn fs-remove [path k]
  (swap! (@upsource :filesystem) dissoc-in (conj (fs-path path) k)))

(defn fs-fire! [path]
  (let [alert (:alert (fs-get path))]
    (when alert (alert (files/stat path) nil))))

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

(reduce (fn [m p] (assoc m (first p) (second p))) {} [["123" "1"] ["2" "4"]])

(defn file-in-revision [path]
  (let [cs (comps path)]
    {"projectId" (second cs)
     "revisionId" (nth cs 2)
     "fileName" (apply str (cons "/" (interpose "/" (drop 3 cs))))}))

(defn fs-append-item! [file]
  (fs-merge (:path file) (:file file)))

(defn file-in-revisoin->path [fir]
  (str "!upsource/" (fir "projectId") "/" (fir "revisionId") "/" (fir "fileName")))

(defn fs-append-subtree! [result fir]
  (doseq [item (result "items")]
    (let [dir? (item "isDirectory")
          fir (assoc fir "fileId" (item "fileId"))]
      (fs-append-item! {:path (file-in-revisoin->path fir)
                        :file {:dir? dir?
                               :file? (not dir?)
                               :stat #js{}
                               :mtime (js/Date. 0)}}))))

(comment

  (.-mtime (clj->js {"mtime" "asd"}))

  (aget (clj->js {"mtime" "asd"}) "mtime")

  (.showDevTools (.get (.-Window (js/require "nw.gui"))))
  )

(defmethod files/read-dir :upsource [path]
  (let [files (keys (:files (fs-get path)))]
    (if files
      files
      (do
        (let [fir (file-in-revision path)]
          (req :getProjectSubtree fir
             (fn
               ([result]
                (fs-append-subtree! result fir)
                (fs-fire! path))
               ([_ error] (println error)))))
        []))))

(defn nil-if--1 [x] (if (= -1 x) nil x))

(defn update-reference-markup [markup path]
  (when markup
    (when-let [doc-obj (document/path->doc path)]
      (update-document-reference-markup doc-obj markup))))

(defn update-document-reference-markup [doc-obj markup]
  (when-let [cm-doc (:doc @doc-obj)]
    (doseq [m (.getAllMarks cm-doc)] (.clear m))
    (let [navigation-table (markup "navigationPointsTable")
          file-table (markup "fileNameTable")
          target-by-id (fn [id]
                         (first (filter (fn [nav] (= (nav "targetId") id)) navigation-table)))]
      (when-let [e (.getEditor cm-doc)]
        (doseq [r (markup "markup")]
          (let [startPos (editor/index->pos e (r "startOffset"))
                endPos (editor/index->pos e (r "endOffset"))
                mark (.markText cm-doc startPos endPos)
                target (target-by-id (r "targetId"))]
            (aset mark "navigationInfo" {:capability-flags (r "capabilityFlags")
                                         :hash (r "hash")
                                         :target (when target {:file (file-in-revisoin->path (nth file-table (target "fileId")))
                                                               :start-offset (nil-if--1 (target "startOffset"))
                                                               :end-offset (nil-if--1 (target "endOffset"))
                                                               :stub-index (nil-if--1 (target "stubIndex"))})})))))))


(behavior ::update-markup
          :triggers #{:init}
          :desc "Upsource: update document markup on open"
          :reaction (fn [this]
                      (when-let [path (:path @this)]
                        (req :getFileContent {"file" (file-in-revision path)
                                              "requestMarkup" true
                                              "requestFolding" false}
                             {:referenceMarkup (fn [markup]
                                                 (update-document-reference-markup this markup))}))))

(defn navigate [path index]
  (object/raise opener/opener :open! path)
  (when-let [doc-obj (document/path->doc path)]
    (when-let [cm-doc (:doc @doc-obj)]
      (let [e (.getEditor cm-doc)
            pos (editor/index->pos e index)]
        (editor/move-cursor e pos)))))

(cmd/command {:command :go-to-declaration
              :desc "Upsource: GoTo Declaration"
              :exec (fn []
                      (when-let [cur (pool/last-active)]
                        (let [cursor (editor/->cursor cur)
                              marks  (editor/find-marks cur cursor)]
                          (when-let [target (first (filter identity (map (fn[mark] (:target (aget mark "navigationInfo"))) marks)))]
                            (when-let [index (:start-offset target)]
                              (navigate (:file target) index))
                            (when-let [stub-index (:stub-index target)]
                              (req :getStubNavigationRange {"fileId" (file-in-revision (:file target))
                                                            "elementId" stub-index}
                                   (fn [result]
                                     (let [index (result "startOffset")]
                                       (load-file-async (:file target) (fn [text]
                                                                         (navigate (:file target) index)
                                                                         ))))))))))})

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
  (req :getFileContent {"file" (file-in-revision path)
                        "requestMarkup" false
                        "requestFolding" false}
       {:fileContent (fn [content]
                       (fs-merge path {:content (content "text")
                                       :mtime (js/Date.)})
                       (cb (content "text")))}))
