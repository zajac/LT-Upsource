(ns lt.plugins.upsource
  (:require [lt.object :as object]
            [lt.objs.tabs :as tabs]
            [lt.objs.command :as cmd]
            [lt.util.load :as load])
  (:require-macros [lt.macros :refer [defui behavior]]))


(comment
  (def io (load/node_module "socket.io/node_modules/socket.io-client"))

  )


;; UI to be associated with an object
(defui hello-panel [this]
  [:h1 "Hello World from upsource!"])

;; Define an object prototype
(object/object* ::upsource.hello
                :tags [:upsource.hello]
                :behaviors [::on-close-destroy]
                :init (fn [this]
                        (hello-panel this)))

;; Currently used by :user.hello but could be reused by any
;; object with a declaration in user.behaviors.
(behavior ::on-close-destroy
          :triggers #{:close}
          :reaction (fn [this]
                      (object/raise this :destroy)))

(def hello (object/create ::upsource.hello))

;; Create a user command. Commands can call any function
;; and be bound to any keystroke.
(cmd/command {:command :upsource.say-hello
              :desc "Upsource: Say Hello"
              :exec (fn []
                      (tabs/add-or-focus! hello))})
