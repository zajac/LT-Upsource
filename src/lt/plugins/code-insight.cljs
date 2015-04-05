(ns lt.plugins.upsource.code-insight
  (:require [lt.object :as object]
            [lt.objs.command :as cmd]
            [lt.objs.document :as document]
            [lt.objs.editor :as editor]
            [lt.objs.editor.pool :as pool]
            [lt.objs.opener :as opener]
            [lt.plugins.upsource.filesystem :as filesystem]
            [lt.plugins.upsource.util :as util]
            [lt.plugins.upsource.rpc :as rpc])
  (:require-macros [lt.macros :refer [behavior]]))




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
                                         :target (when target {:file (util/file-in-revisoin->path (nth file-table (target "fileId")))
                                                               :start-offset (nil-if--1 (target "startOffset"))
                                                               :end-offset (nil-if--1 (target "endOffset"))
                                                               :stub-index (nil-if--1 (target "stubIndex"))})})))))))


(behavior ::update-markup
          :triggers #{:init}
          :desc "Upsource: update document markup on open"
          :reaction (fn [this]
                      (when-let [path (:path @this)]
                        (if (.startsWith path filesystem/upsource-fs-root)
                          (rpc/req :getFileContent {"file" (util/file-in-revision path)
                                                "requestMarkup" true
                                                "requestFolding" false}
                               {:referenceMarkup (fn [markup]
                                                   (update-document-reference-markup this markup))})))))

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
                              (rpc/req :getStubNavigationRange {"fileId" (util/file-in-revision (:file target))
                                                                "elementId" stub-index}
                                   (fn [result]
                                     (let [index (result "startOffset")]
                                       (filesystem/load-file-async (:file target) (fn [text]
                                                                         (navigate (:file target) index)
                                                                         ))))))))))})
