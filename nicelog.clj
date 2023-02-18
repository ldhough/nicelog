#!/opt/homebrew/bin/bb

;; *** USAGE ***
;; ./nicelog.clj [--default] [--replace [[regex replacement] ...] [--colorize [[regex color] ...]]
;; Where regexes are PCRE, replacements will replace regexes after replace flag, color dictates
;; what color substrings matching regexes after colorize flag will be presented as. Be careful
;; when using characters in the ansi codes in regexes because after each color replacement pass
;; they'll be inserted into the string.

;; TODO: nested coloring
;; TODO: verify escaping works fine

(def ansi-colors
  {"red" "\033[1;31m"
   "cyan" "\033[1;36m"
   "dark blue" "\033[1;34m"
   "green" "\033[1;32m"
   "yellow" "\033[1;33m"
   "magenta" "\033[1;35m"
   "reset" "\033[0m"})

(def log-levels ["DEBUG" "INFO" "WARN" "WARNING" "ERROR"])

(def default-replacements
  (let [ll-joined (str "((" (str/join "|" log-levels) ").*)")]
    [[(re-pattern (str "^.*?" ll-joined)) "$1"]]))

(def default-colorizers
  [[#"(DEBUG)" "dark blue"]
   [#"(INFO)" "green"]
   [#"(WARN|WARNING)" "yellow"]
   [#"(ERROR)" "red"]
   [#"(\[[^\[]+?\..+?:\d+\])" "cyan"]])

(defn escape [s] (str/replace s #"\\" "\\\\"))

(defn color-pass
  [s regexes-colors]
  (if (not-empty regexes-colors)
    (let [[regex color] (first regexes-colors)
          ansi-color (ansi-colors color)
          ansi-reset (ansi-colors "reset")]
      (recur
        (str/replace s regex (str ansi-color "$1" ansi-reset))
        (rest regexes-colors))) s))

(defn remove-patterns
  [s replacements]
  (if (not-empty replacements)
    (let [[regex replacement-str] (first replacements)]
      (recur
        (str/replace s regex replacement-str)
        (rest replacements))) s))

(let [arg-parts (partition-by #(re-matches #"--\w+" %) *command-line-args*)
      argc (count arg-parts)
      run-default? (not-empty (filter #(= % "--default") *command-line-args*))
      colorize-pairs
      (->> (loop [idx 0
                  pairs []]
             (if (< idx argc) 
               (let [colorize-flag? (= "--colorize" (first (nth arg-parts idx)))
                     pattern-color-pairs
                     (if colorize-flag?
                       (if (< (inc idx) argc)
                        (let [pairs? (nth arg-parts (inc idx))]
                          (if (odd? (count pairs?))
                            (butlast pairs?)
                            pairs?)))
                       [])]
                 (recur (if colorize-flag? (+ 2 idx) (inc idx))
                        (into pairs pattern-color-pairs)))
               pairs))
           (partition 2)
           (reduce (fn [acc [re-str color]]
                     (if (ansi-colors color)
                       (conj acc [(re-pattern
                                    (str "(" (escape re-str) ")")) color])
                       acc))
                   []))
      replace-patterns
      (->> (loop [idx 0
                  replacements []]
             (if (< idx argc)
               (let [replace-flag? (= "--replace" (first (nth arg-parts idx)))
                     repls (if replace-flag?
                             (if (< (inc idx) argc)
                               (let [pairs? (filter
                                              #(and (not= % "--colorize")
                                                    (not= % "--replace"))
                                              (nth arg-parts (inc idx)))]
                                 (if (odd? (count pairs?))
                                   (butlast pairs?)
                                   pairs?)))
                             [])]
                 (recur (if replace-flag? (+ 2 idx) (inc idx))
                        (into replacements repls)))
               replacements))
           (partition 2)
           (reduce (fn [acc [regex replace-str]]
                     (conj acc [(re-pattern (escape regex)) replace-str]))
                   []))
      _ (println "colorize-pairs: ")
      _ (pprint colorize-pairs)
      _ (println "replace-patterns")
      _ (pprint replace-patterns)]
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (println (-> line
                 (remove-patterns
                   (into (if run-default? default-replacements []) replace-patterns))
                 (color-pass
                   (into (if run-default? default-colorizers []) colorize-pairs))))))
