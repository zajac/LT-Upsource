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
                :callbacks (atom {})
                :request-id (atom 0)
                :init (fn [this]
                        (hello-panel this )))

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

(cmd/command {:command :upsource-connect
              :desc "Upsource: open"
              :exec (fn []
                      (tabs/add-or-focus! (get-upsource)))})



(defn req [method data callback]
  (let [up (get-upsource)
        request-id (swap! (@up :request-id) inc)
        callbacks (@up :callbacks)
        socket (@up :socket)]


    (swap! callbacks assoc request-id callback)
    (.send socket (create-request-str method data (@up :token) request-id))))

(defn create-request-str [method data token request-id]
  (.stringify js/JSON #js {"type" "rpc"
                           "method" (name method)
                           "X-Request-ID" request-id
                           "data" (clj->js data)
                           "Authorization" token}))



(comment
  (req :getAllProjects {} println)
  )
