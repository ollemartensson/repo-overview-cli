(ns repo.overview.tool
  (:require [babashka.fs :as fs]
            [clojure.string :as str])
  (:import (java.lang ProcessBuilder)))

;; ---------- internals ----------
(def ^:const MAX-TREE-DEPTH 3)
(def ^:const MAX-NODES 400)
(def ^:const MAX-KEYFILES 20)

(def collapse-dirs
  #{"node_modules" "target" ".git" ".idea" ".vscode" "dist" "build" ".next" ".venv" "__pycache__" ".gradle" ".cache"})

(def lang-by-ext
  {".clj" "Clojure" ".cljc" "Clojure" ".edn" "EDN" ".bb" "Clojure" ".cljs" "ClojureScript"
   ".java" "Java" ".kt" "Kotlin" ".rs" "Rust" ".toml" "TOML"
   ".js" "JavaScript" ".ts" "TypeScript" ".tsx" "TypeScript" ".jsx" "JavaScript"
   ".py" "Python" ".go" "Go" ".rb" "Ruby" ".swift" "Swift"
   ".yaml" "YAML" ".yml" "YAML" ".json" "JSON" ".md" "Markdown" ".txt" "Text"})

(defn- ext->lang [p]
  (some-> p fs/extension str/lower-case (get lang-by-ext "Other")))

(defn- run-sh [dir & cmd]
  (-> (doto (ProcessBuilder. ^java.util.List cmd)
        (.directory (io/file dir))
        (.redirectErrorStream true))
      (.start)
      (doto (.waitFor))
      (.getInputStream)
      (slurp)))

(defn- git-root [dir]
  (try
    (str/trim (run-sh dir "git" "rev-parse" "--show-toplevel"))
    (catch Exception _ (fs/absolutize dir))))

(defn- tracked-files [root]
  (try
    (->> (run-sh root "git" "ls-files")
         (str/split-lines)
         (map #(fs/absolutize (fs/path root %)))
         (filter fs/regular-file?))
    (catch Exception _
      (->> (fs/walk root)
           (filter fs/regular-file?)
           (remove (fn [p]
                     (some #(str/includes? (str p)
                                           (str fs/file-separator % fs/file-separator))
                           collapse-dirs)))))))

(defn- rel [root p] (str (fs/relativize root p)))

(defn- lang-stats [files]
  (->> files (map ext->lang) (frequencies)
       (sort-by (fn [[_ n]] (- n)))))

(defn- safe-slurp [p nbytes]
  (try
    (with-open [r (clojure.java.io/reader (str p))]
      (let [buf (char-array (int nbytes))
            n (.read r buf 0 (alength buf))]
        (String. buf 0 (max 0 n))))
    (catch Exception _ "")))

(defn- readme-headline [root]
  (let [cand (some #(when (fs/exists? (fs/path root %)) (fs/path root %))
                   ["README.md" "Readme.md" "readme.md" "README"])]
    (when cand
      (let [s (safe-slurp cand 4096)]
        (or (->> (str/split-lines s)
                 (filter #(re-find #"^#+" %))
                 first str/trim)
            (first (str/split-lines s)))))))

(defn- limited-tree [root files max-depth max-nodes]
  (let [sep (str fs/file-separator)
        rels (->> files (map #(rel root %)) sort)]
    (loop [xs rels, used 0, out []]
      (if (or (empty? xs) (>= used max-nodes))
        (if (and (seq xs) (>= used max-nodes))
          (conj out (format "(+%d more files not shown)" (count xs)))
          out)
        (let [x (first xs)
              depth (dec (count (str/split x (re-pattern (java.util.regex.Pattern/quote sep)))))]
          (if (> depth max-depth)
            (recur (rest xs) used out)
            (recur (rest xs) (inc used) (conj out x))))))))

(defn- indent-tree [lines]
  (let [sep (str fs/file-separator)]
    (->> lines
         (map (fn [p]
                (if (str/starts-with? p "(+")
                  p
                  (let [parts (str/split p (re-pattern (java.util.regex.Pattern/quote sep)))
                        depth (dec (count parts))
                        name  (last parts)]
                    (str (apply str (repeat depth "  "))
                         (if (str/includes? name ".") "├─ " "└─ ")
                         name)))))
         (str/join "\n"))))

(defn- keyfiles [root files]
  (let [names #{"README.md" "README" "LICENSE" "LICENSE.md"
                "deps.edn" "bb.edn" "project.clj" "pom.xml" "build.gradle" "settings.gradle" "gradlew"
                "package.json" "pnpm-lock.yaml" "yarn.lock" "Cargo.toml"
                "Dockerfile" "docker-compose.yml" "docker-compose.yaml"
                ".github/workflows" ".gitlab-ci.yml" ".circleci" "Makefile" "justfile"}]
    (->> files
         (map #(rel root %))
         (filter (fn [r]
                   (or (contains? names r)
                       (some #(str/starts-with? r (str % fs/file-separator))
                             [".github/workflows"]))))
         sort (take MAX-KEYFILES))))

(defn- summarize* [{:keys [dir max-depth max-nodes]}]
  (let [root (git-root dir)
        files (tracked-files root)]
    {:name (fs/file-name root)
     :root root
     :file-count (count files)
     :languages (lang-stats files)
     :readme-headline (readme-headline root)
     :tree (->> (limited-tree root files (or max-depth MAX-TREE-DEPTH)
                              (or max-nodes MAX-NODES))
                indent-tree)
     :keyfiles (keyfiles root files)}))

(defn- print-md [{:keys [name file-count languages readme-headline tree keyfiles]}]
  (str
   (format "# %s — Repository Overview\n" name)
   (when readme-headline (str "\n_Readme:_ " readme-headline "\n"))
   "\n## Quick Stats\n"
   (format "- Files tracked (approx): %d\n" file-count)
   (format "- Top languages: %s\n"
           (->> languages (take 5)
                (map (fn [[lang n]] (str lang " (" n ")")))
                (str/join ", ")))
   "\n## Key Files & Directories\n"
   (->> keyfiles (map #(str "- " % "\n")) (apply str))
   "\n## Structure (max depth capped)\n```text\n" tree "\n```\n"
   "\n## Notes for LLMs\n"
   "- Tree is truncated for token efficiency (depth & node caps).\n"
   "- Prefer README+build files to infer run/build/test.\n"
   "- For Polylith: look for `components/`, `bases/`, `projects/`, `development/`.\n"))

;; ---------- public tool API (functions must take a single map) ----------
(defn overview
  "Return a data summary {:name :root :file-count :languages :tree :keyfiles}.
   Args: {:dir \".\" :max-depth 3 :max-nodes 400}"
  [{:keys [dir] :as opts}]
  (summarize* (merge {:dir (or dir ".")} opts)))

(defn print
  "Print Markdown overview to stdout or file.
   Args: {:dir \".\" :out nil :max-depth 3 :max-nodes 400}"
  [{:keys [out] :as opts}]
  (let [md (print-md (summarize* opts))]
    (if out
      (spit out md)
      (print md)))
  ;; return status map for -T programmatic callers
  {:status :ok})