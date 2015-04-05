(ns lt.plugins.upsource
  (:require [lt.object :as object]
            [lt.objs.tabs :as tabs]
            [lt.objs.command :as cmd]
            [lt.objs.workspace :as workspace]
            [lt.objs.sidebar.clients :as scl]
            [lt.objs.clients :as clients]
            [lt.plugins.upsource.rpc :as rpc]
            [lt.plugins.upsource.filesystem :as filesystem])
  (:require-macros [lt.macros :refer [defui behavior]]))

(comment
  (.showDevTools (.get (.-Window (js/require "nw.gui"))))
  )

(defui project-item [project]
  [:div [:button (project "projectName")]]
  :click (fn [evt] (open-project (project "projectId"))))

(defui project-list-view [this]
  [:div
   (map project-item
        (@this :projects))])

(defui add-to-workspace-button [revision projectId]
  [:button "Add to workspace"]
  :click (fn [evt] (add-to-workspace revision projectId)))


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
                :behaviors [])

(def instance (atom nil))

(def client-name "Upsource")

(scl/add-connector {:name "Upsource"
                    :desc "Connect to Upsource server."
                    :connect (fn []
                               (let [up (object/create ::upsource)]
                                 (object/raise up :upsource-connect)
                                 (clients/handle-connection! {:name client-name
                                                              :tags [:upsource-connector]
                                                              :root-relative "!upsource"
                                                              :commands #{}
                                                              :type "UP"})
                                 (reset! instance up)))})

(defmulti on-message identity)

(defmethod on-message :client.close [_ _ _]
  (clients/rem! (clients/by-name client-name))
  (object/raise @instance :destroy))


(defmethod on-message :default [_ _ _])

(behavior ::send!
          :triggers #{:send!}
          :reaction (fn[connector data]
                      (on-message (keyword (:command data)) (:data data) (:cb data))))

(defn open-project [projectId]
  (rpc/req :getRevisionsList {"projectId" projectId
                              "limit" 100}
           (fn [result]
             (let [revision-list (object/create ::revision-list result projectId)]
               (tabs/add-or-focus! revision-list)))))

(defn add-to-workspace [revision projectId]
  (let [path (str filesystem/upsource-fs-root "/" projectId "/" (revision "revisionId"))]
    (filesystem/init-path path)
    (object/raise workspace/current-ws :add.folder! path)))

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


(cmd/command {:command :upsource-project-list
              :desc "Upsource: Projects List"
              :exec (fn []
                      (rpc/req :getAllProjects (fn [projects]
                                             (let [project-list (object/create ::project-list projects)]
                                               (tabs/add-or-focus! project-list)))))})
