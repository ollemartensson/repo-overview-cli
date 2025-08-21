# repo-overview-cli

A small Clojure CLI **tool** for printing LLM‑friendly summaries of repo structures.

## Install (once per user)
```bash
clojure -Ttools install io.github.ollemartensson/repo-overview-cli '{:git/tag "v0.1.0"}' :as repo-overview
```

## Usage
```bash
# Print to console
clojure -Trepo-overview print :dir "."
# Write to file
clojure -Trepo-overview print :dir "." :out "OVERVIEW.md"
# Get structured data map (for scripts)
clojure -Trepo-overview overview :dir "."
```
