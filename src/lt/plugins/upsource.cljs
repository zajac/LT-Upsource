(ns lt.plugins.upsource
  (:require [lt.object :as object]
            [lt.objs.tabs :as tabs]
            [lt.objs.command :as cmd]
            [lt.util.load :as load]
            [lt.objs.workspace :as workspace]
            [lt.objs.sidebar.workspace :as sidebar.workspace]
            [lt.objs.files :as files])
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
                        (println projects)
                        (object/merge! this {:projects (projects "project")})
                        (project-list-view this)))


(object/object* ::revision-list
                :tags [:revision-list]
                :behaviors [::on-close-destroy]
                :revisions []
                :projectId nil
                :init (fn [this revisions projectId]
                        (println revisions)
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
          :type :user
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
        callback (@(@upsource :callbacks) (message "X-Request-ID"))]
    (if callback
      (process-message message callback))))

(defn process-message [message callback]
  (dispatch message
            (message "data")
            callback))


(defmulti dispatch (fn [message data callback] (keyword (message "event"))))

(defmethod dispatch :RpcResult
  ([message data callback]
   (let [{result "result", error "error"} data]
     (cond
      result (callback result)
      error (callback :error error)))))


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



(comment
  (get-upsource)

  (def test_proj (atom nil))

  (req :getAllProjects println)

  (comps "!upsource/graphs/d992c15a2fcc3318982cc52e7a62a31377da8e4b/")

  (files/read-dir "!upsource/graphs/13d90488f0f382f6b5308662f75a4555d369ba4d/")

  (fs-get "!upsource/graphs/13d90488f0f382f6b5308662f75a4555d369ba4d/")
;;   (object/raise workspace/current-ws :add.folder! "upsource#/Graph")

  (files/dir? "!upsource/light-table/0aa35c493a83fba8087a4c94233dd8044e3c001a/osx_deps.sh")

  (files/read-dir "!upsource/hub-project/9ec713a98e0d2cb743268d1a600768fe6c8f5538/solutions")
  (:alert (fs-get "!upsource/hub-project/5dda7865dde0b1341d328e5a2a5f519eeb40a73b/solutions"))
  )

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
  (when (exists? path)
    (fs-get path)))
(comment
  (files/dir? "!upsource/graphs/5f018505b9f6021e0aa8451c852f172e3a35620b")
  (fs-get "!upsource/graphs/5f018505b9f6021e0aa8451c852f172e3a35620b")
)

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
  (println "watch " path "alert " (nil? alert))
  (fs-merge path {:alert alert}))


(comment
  (fs-fire! "!upsource/Epigenom/b69608784d575fca01e442c645da2a9084c568e5")
  (keys (:files (fs-get "!upsource/Epigenom/b69608784d575fca01e442c645da2a9084c568e5")))
  (:alert (fs-get "!upsource/hub-project/5dda7865dde0b1341d328e5a2a5f519eeb40a73b/solutions"))

  (files/watch "!upsource/hub-project/5dda7865dde0b1341d328e5a2a5f519eeb40a73b/tests" {} (fn [x y]
                                                                                           (println "fire!")))
  (files/unwatch "!upsource/hub-project/5dda7865dde0b1341d328e5a2a5f519eeb40a73b/tests" nil)
  (def a (atom {:qwe :qweqwe}))
  (swap! a assoc-in [:a :b :c] :d)

  (:alert (fs-get "!upsource/hub-project/bd15c7bcd7a091c41f96c55fd519a15dd1dbc118/gradle"))
  (count (files/read-dir "!upsource/hub-project/cf88899a1bba262b32ee5c4c87e75cd4f3bd0298/"))
)


(defmethod files/stat :upsource [path]
  (:stat (fs-get path)))


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

(comment
  (req :getProjectSubtree (file-in-revision "!upsource/graphs/d992c15a2fcc3318982cc52e7a62a31377da8e4b/src/graph")
       (fn [results] (print (fs-from-project-subtree results))))

  )

(defn fs-append-item! [file]
  (fs-merge (:path file) (:file file)))

(defn fs-append-subtree! [result projectId revisionId]
  (doseq [item (result "items")]
    (let [dir? (item "isDirectory")]
      (fs-append-item! {:path (str "!upsource/" projectId "/" revisionId "/" (item "fileId"))
                        :file {:dir? dir?
                               :file? (not dir?)
                               :stat #js{}}}))))

(comment

  (:alert (fs-get "!upsource/Epigenom/773ca3bd5955550ce4b1e60a877b69f91fe6b0d2/util/src/main/java/org/"))
  (files/read-dir "!upsource/Epigenom/2de045fb33c9236fc7b357aa7f8485d6f104e9ee/gradle")
  (:alert (fs-get
           "!upsource/Epigenom/281ba563b0df8ea0a2351c2d8b72b229b525f6f3 "))


  (object/raise workspace/current-ws :refresh (:path @this))

  (count (:folders @(first (object/by-tag :workspace.root))))
  (:open-dirs @sidebar.workspace/tree)
  (fs-fire! "!upsource/Epigenom/281ba563b0df8ea0a2351c2d8b72b229b525f6f3")
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
                (fs-append-subtree! result (fir "projectId") (fir "revisionId"))
                (fs-fire! path))
               ([_ error] (println error)))))
        []))))

(defmethod files/bomless-read :upsource [path]
  (let [content (:content (fs-get path))]
    (if content
      content
      (do

        ""))))


