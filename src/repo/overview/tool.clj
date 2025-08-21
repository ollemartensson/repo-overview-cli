(ns repo.overview.tool
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str])
  (:import
    (java.lang
      ProcessBuilder)
    (java.nio.file
      FileSystems
      Files
      Path
      Paths)
    (java.util.stream
      Stream)))


;; ---------- config ----------
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


;; ---------- tiny stdlib FS helpers ----------
(def ^String file-sep (.. FileSystems getDefault getSeparator))


(defn path
  ^Path [& segs]
  (Paths/get (first segs) (into-array String (rest segs))))


(defn absolutize
  ^Path [^Path p]
  (.toAbsolutePath p))


(defn relativize
  [^Path root ^Path p]
  (str (.relativize root p)))


(defn file-name
  [^Path p]
  (str (.getFileName p)))


(defn extension
  [^Path p]
  (let [n (str (.getFileName p))
        i (.lastIndexOf n ".")] (when (pos? i) (subs n i))))


(defn exists?
  [^Path p]
  (Files/exists p (into-array java.nio.file.LinkOption [])))


(defn directory?
  [^Path p]
  (Files/isDirectory p (into-array java.nio.file.LinkOption [])))


(defn regular-file?
  [^Path p]
  (Files/isRegularFile p (into-array java.nio.file.LinkOption [])))


(defn list-dir
  [^Path p]
  (with-open [ds (Files/newDirectoryStream p)]
    (doall (map identity ds))))


(defn walk
  [^Path start]
  (let [^Stream s (Files/walk start (into-array java.nio.file.FileVisitOption []))]
    (try (doall (map identity (.iterator s)))
         (finally (.close s)))))


(defn path-contains-collapsed?
  [^Path p]
  (some collapse-dirs (map str (iterator-seq (.iterator p)))))


;; ---------- internals ----------
(defn- ext->lang
  [^Path p]
  (let [e (some-> (extension p) str/lower-case)]
    (get lang-by-ext e "Other")))


(defn- run-sh
  [dir & cmd]
  (-> (doto (ProcessBuilder. ^java.util.List cmd)
        (.directory (io/file dir))
        (.redirectErrorStream true))
      (.start)
      (doto (.waitFor))
      (.getInputStream)
      (slurp)))


(defn- git-root
  [dir]
  (try
    (str/trim (run-sh dir "git" "rev-parse" "--show-toplevel"))
    (catch Exception _ (str (absolutize (path dir))))))


(defn- tracked-files
  [root-str]
  (let [root (absolutize (path root-str))]
    (try
      (->> (run-sh root-str "git" "ls-files")
           (str/split-lines)
           (map #(absolutize (path root-str %)))
           (filter regular-file?))
      (catch Exception _
        (->> (walk root)
             (filter regular-file?)
             (remove path-contains-collapsed?))))))


(defn- lang-stats
  [files]
  (->> files (map ext->lang) (frequencies)
       (sort-by (fn [[_ n]] (- n)))))


(defn- safe-slurp
  [^Path p nbytes]
  (try
    (with-open [r (clojure.java.io/reader (str p))]
      (let [buf (char-array (int nbytes))
            n (.read r buf 0 (alength buf))]
        (String. buf 0 (max 0 (or n 0)))))
    (catch Exception _ "")))


(defn- readme-headline
  [root-str]
  (let [root (absolutize (path root-str))
        cand (some #(when (exists? (path (str root) %)) (path (str root) %))
                   ["README.md" "Readme.md" "readme.md" "README"])]
    (when cand
      (let [s (safe-slurp cand 4096)]
        (or (some->> (str/split-lines s)
                     (filter #(re-find #"^#+" %))
                     first str/trim)
            (first (str/split-lines s)))))))


(defn- limited-tree
  [root-str files max-depth max-nodes]
  (let [root (absolutize (path root-str))
        rels (->> files
                  (map #(relativize root %))
                  (mapcat (fn [path]
                            (let [parts (str/split path (re-pattern (java.util.regex.Pattern/quote file-sep)))]
                              (for [i (range 1 (inc (count parts)))]
                                (str/join file-sep (take i parts))))))
                  distinct
                  sort)]
    (loop [xs rels, used 0, out []]
      (if (or (empty? xs) (>= used max-nodes))
        (if (and (seq xs) (>= used max-nodes))
          (conj out (format "(+%d more files not shown)" (count xs)))
          out)
        (let [x (first xs)
              depth (dec (count (str/split x (re-pattern (java.util.regex.Pattern/quote file-sep)))))]
          (if (> depth max-depth)
            (recur (rest xs) used out)
            (recur (rest xs) (inc used) (conj out x))))))))


(defn- indent-tree
  [lines]
  (->> lines
       (map (fn [p]
              (if (str/starts-with? p "(+")
                p
                (let [parts (str/split p (re-pattern (java.util.regex.Pattern/quote file-sep)))
                      depth (dec (count parts))
                      name  (last parts)]
                  (str (apply str (repeat depth "  "))
                       (if (str/includes? name ".") "├─ " "└─ ")
                       name)))))
       (str/join "\n")))


(defn- keyfiles
  [root-str files]
  (let [root (absolutize (path root-str))
        names #{"README.md" "README" "LICENSE" "LICENSE.md"
                "deps.edn" "bb.edn" "project.clj" "pom.xml" "build.gradle" "settings.gradle" "gradlew"
                "package.json" "pnpm-lock.yaml" "yarn.lock" "Cargo.toml"
                "Dockerfile" "docker-compose.yml" "docker-compose.yaml"
                ".github/workflows" ".gitlab-ci.yml" ".circleci" "Makefile" "justfile"}]
    (->> files
         (map #(relativize root %))
         (filter (fn [^String r]
                   (or (contains? names r)
                       (str/starts-with? r ".github/workflows"))))
         sort (take MAX-KEYFILES))))


(defn- summarize*
  [{:keys [dir max-depth max-nodes]}]
  (let [dir (or dir ".")
        root-str (git-root dir)
        files (tracked-files root-str)]
    {:name (file-name (absolutize (path root-str)))
     :root root-str
     :file-count (count files)
     :languages (lang-stats files)
     :readme-headline (readme-headline root-str)
     :tree (->> (limited-tree root-str files (or max-depth MAX-TREE-DEPTH)
                              (or max-nodes MAX-NODES))
                indent-tree)
     :keyfiles (keyfiles root-str files)}))


(defn- print-md
  [{:keys [name file-count languages readme-headline tree keyfiles]}]
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


;; ---------- public tool API ----------
(defn edn
  "Return a data summary {:name :root :file-count :languages :tree :keyfiles}.
   Args: {:dir ' . ' :max-depth 3 :max-nodes 400}"
  [{:keys [dir] :as opts}]
  (summarize* (merge {:dir (or dir ".")} opts)))


(defn markdown
  "Print Markdown overview to stdout or file.
   Args: {:dir ' . ' :out nil :max-depth 3 :max-nodes 400}"
  [{:keys [out] :as opts}]
  (let [md (print-md (summarize* opts))]
    (if out
      (spit out md)
      (println md)))
  {:status :ok})


;; ---------- CLI entrypoint ----------
(defn- parse-args
  "Turn a flat CLI vector like [\"--dir\" \".\" \"--out\" \"OVERVIEW.md\"] into a map."
  [args]
  (let [pairs (partition 2 2 nil args)]
    (->> pairs
         (reduce (fn [m [k v]]
                   (cond
                     (= k "--dir") (assoc m :dir (or v "."))
                     (= k "--out") (assoc m :out v)
                     (= k "--max-depth") (assoc m :max-depth (some-> v Integer/parseInt))
                     (= k "--max-nodes") (assoc m :max-nodes (some-> v Integer/parseInt))
                     :else m))
                 {}))))


(defn -main
  "CLI entrypoint. Examples:
   clojure -M -m repo.overview.tool --dir .
   clojure -M -m repo.overview.tool --dir . --out OVERVIEW.md --max-depth 3 --max-nodes 400"
  [& args]
  (let [opts (parse-args args)]
    (print opts)
    (shutdown-agents)))
