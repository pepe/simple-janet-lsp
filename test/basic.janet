(import ../simple-janet-lsp/parser)
(import ../simple-janet-lsp/eval)

(setdyn *pretty-format* "%P")

(def source `(defn new-diagnostic [{:location [line char] :message message} text]
  (def line (dec line))
  (def start @{:line line :character (dec char)})
  (def end @{:line line :character char})
  (def error-line (get (string/split "\n" text) line))
foo
  (when-let [word (eval/word-at error-line char)]
    (put end :character (+ char (length word))))

  (def char-at-error (string/slice error-line (dec char) char))

  (when (or (= char-at-error "(") (= char-at-error "["))
    (update start :character inc))

  (defn word-range [word]
    (unless (or (= char-at-error "(") (= char-at-error "[")) (break))
    (when-let [tup (eval/tuple-at text {"character" (dec char) "line" line})]
      (def {:character char-pos :len len} (parser/sym-loc word tup))
      (put start :character (+ (dec char) char-pos))
      (put end :character (+ (dec char) char-pos len))))

  (cond
    (string/has-prefix? "unknown symbol" message)
    (word-range (string/slice message 15))

    (string/has-prefix? "could not find module" message)
    (word-range (->> (string/slice message 22) (string/split ":") (first))))

  [{:range {:start start :end end} :message message :severity 1}])`)
(def loc {"character" 0 "line" 0})
(pp (parser/sym-loc "foo" (parser/tuple-at loc source)))
