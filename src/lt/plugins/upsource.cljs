(ns lt.plugins.upsource
  (:require [lt.object :as object]
            [lt.objs.tabs :as tabs]
            [lt.objs.command :as cmd]
            [lt.util.load :as load])
  (:require-macros [lt.macros :refer [defui behavior]]))

(def io (load/node_module "socket.io/node_modules/socket.io-client"))


(defui hello-panel [this]
  [:h1 "Hello World from upsource!"]
)

(object/object* ::upsource
                :tags [:upsource]
                :behaviors []
                :init (fn [this]
                        (hello-panel this)))

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
          :reaction (fn [this url user passwd]
                      (object/merge! this {:socket (.connect io url, #js  {"reconnection" "false"
                                                                           "forceNew" "true"
					                                                                 "path" "/~socket.io"})
                                           :token (token user passwd)})))




(cmd/command {:command :upsource-connect
              :desc "Upsource: open"
              :exec (fn []
                      (if (not (@upsource :socket))
                        (object/raise upsource :upsource-connect))
                      (tabs/add-or-focus! upsource))})


(comment
  upsource



  )
