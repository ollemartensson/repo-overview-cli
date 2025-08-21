(ns repo.overview.tool-test
  (:require
    [clojure.test :refer [deftest is]]
    [repo.overview.tool :as tool])
  (:import
    (java.nio.charset
      StandardCharsets)
    (java.nio.file
      Files)))


(defn tmpdir
  []
  (-> (Files/createTempDirectory "repo-overview-test" (make-array java.nio.file.attribute.FileAttribute 0))
      (.toFile)))


(defn write!
  [^java.io.File root rel s]
  (let [p (.toPath (java.io.File. root rel))]
    (Files/createDirectories (.getParent p) (make-array java.nio.file.attribute.FileAttribute 0))
    (Files/write p (.getBytes s StandardCharsets/UTF_8) (make-array java.nio.file.OpenOption 0))
    (.toFile p)))


(deftest overview-basic
  (let [root (tmpdir)]
    (write! root "README.md" "# Sample Repo\n\nExample.")
    (write! root "src/sample/core.clj" "(ns sample.core)")
    (write! root "node_modules/leftpad/index.js" "module.exports = 1;")
    (with-redefs [tool/git-root (fn [_] (.getAbsolutePath root))
                  tool/tracked-files (fn [_]
                                       [(.toPath (java.io.File. root "README.md"))
                                        (.toPath (java.io.File. root "src/sample/core.clj"))])]
      (let [ov (tool/overview {:dir (.getAbsolutePath root)})]
        (is (map? ov))
        (is (> (:file-count ov) 0))
        (is (some #(= % "README.md") (:keyfiles ov)) "README should be in keyfiles")
        ;; node_modules should be ignored in fallback walk
        (is (not (re-find #"node_modules" (:tree ov)))
            "collapsed dirs should not appear in tree")
        ;; languages should include Clojure and Markdown
        (let [langs (into {} (:languages ov))]
          (is (contains? langs "Clojure"))
          (is (contains? langs "Markdown")))))))


(deftest print-to-file
  (let [root (tmpdir)
        _ (write! root "README.md" "# Title")
        out (java.io.File/createTempFile "overview" ".md")]
    (with-redefs [tool/git-root (fn [_] (.getAbsolutePath root))
                  tool/tracked-files (fn [_]
                                       [(.toPath (java.io.File. root "README.md"))])]
      (tool/print-overview {:dir (.getAbsolutePath root) :out (.getAbsolutePath out)})
      (let [s (slurp out)]
        (is (re-find #"# .* — Repository Overview" s))
        (is (re-find #"## Quick Stats" s))))))


(deftest depth-cap
  (let [root (tmpdir)]
    ;; create a deep structure: a/b/c/d/e.txt
    (write! root "a/b/c/d/e.txt" "x")
    (with-redefs [tool/git-root (fn [_] (.getAbsolutePath root))
                  tool/tracked-files (fn [_]
                                       [(.toPath (java.io.File. root "a/b/c/d/e.txt"))])]
      (let [ov (tool/overview {:dir (.getAbsolutePath root) :max-depth 2})
            tree (:tree ov)]
        ;; the leaf shouldn't be shown when depth cap is 2
        (is (not (re-find #"e.txt" tree)))
        ;; but some prefix should appear
        (is (re-find #"b" tree))))))
