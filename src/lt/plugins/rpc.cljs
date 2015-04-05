(ns lt.plugins.upsource.rpc
  (:require [lt.object :as object]
            [lt.util.load :as load]
            [lt.objs.files :as files])
  (:require-macros [lt.macros :refer [defui behavior]]))

(def io (load/node-module "socket.io/node_modules/socket.io-client"))

(defn token [user passwd]
  (str "Basic " (js/btoa (str user ":" passwd))))


(behavior ::connect
          :triggers #{:upsource-connect}
          :desc "Upsource: connect to server"
          :params [{:label "URL"}
                   {:label "User"}
                   {:label "Password"}]
          :exclusive true
          :reaction (fn [this url user passwd]
                      (println "upsource-connect")
                      (let [socket (.connect io url, #js  {"reconnection" "false"
                                                           "forceNew" "true"
                                                           "path" "/~socket.io"})]
                        (object/merge! this {:socket socket
                                             :token (token user passwd)
                                             :callbacks (atom {})
                                             :request-id (atom 0)})
                        (.on socket "message" (fn [msg] (process-message-str this msg))))))


(defn process-message-str [upsource message-str]
  (let [message (js->clj (.parse js/JSON message-str))
        callbacks (@upsource :callbacks)
        request-id (message "X-Request-ID")
        callback (@callbacks request-id)]
    (if callback
      (if (process-message message callback)
        (swap! callbacks dissoc request-id)))))

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

(defn create-request-str [method data token request-id]
  (.stringify js/JSON #js {"type" "rpc"
                           "method" (name method)
                           "X-Request-ID" request-id
                           "data" (clj->js data)
                           "Authorization" token}))

(defn get-upsource []
  (first (object/by-tag :upsource)))

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



