(import ../simple-janet-lsp/parser)

(setdyn *pretty-format* "%P")

(def source `(def- private-foo nil)(def public-bar nil)(var public-baz nil)(var- private-boo nil) (defn- new-diagnostic [{:location [line char] :message message} text]
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
(def unused-def nil)
  (loop [items :in [local-items global-items]
           item :in items
           :let [label (get item :label)]
           :unless (get seen label)]
      (put seen label true)
      (array/push all-items item))
  [{:range {:start start :end end} :message message :severity 1}])`)
(def source (slurp "simple-janet-lsp/eval.janet"))

# (def source `(def top-level nil) (def- top-level-pri nil)(defn pub [[mmm {:foo fbar}]]) (defn- not-used [mmm])(defn- unused-private-fn [foo] (print mmm))(let [[foo1 foo2] nil] nil)(def [foo3 foo4])`)
# (def source `(def foo nil) (print foo)(defn my-func [used unused] (print used)) (my-func nil nil)`)
# (def source `(defn- used-func [unsed-par] (let [used nil unused nil] (let [ununused nil] (print used))))(used-func)`)
# (def source `(def used nil) (print used)`)
(def source `(defn my-func [my-var]
  (def my-var my-var)
  (print my-var))`)
(def loc {"character" 0 "line" 0})

(def tree (parser/make-tree source))

(defn file-warning-check [tree]
  (def declared-symbols @[])
  (def used-symbols @[])
  (def scope-stack @[@[]])

  (defn new-scope []
    (array/push scope-stack @[]))

  (defn exit-scope []
    (array/pop scope-stack))

  (defn declare-symbol [sym]
    (array/push (last scope-stack) sym)
    (array/push declared-symbols sym))

  (defn use-symbol [name]
    (var i (dec (length scope-stack)))
    (while (>= i 0)
      (when-let [scope (get scope-stack i)
                 sym-index (find-index |(= ($ :value) name) scope)]
        (array/push used-symbols (get scope sym-index))
        (array/remove scope sym-index)
        (break))
      (-- i)))

  (defn traverse [node &opt top?]
    (if (struct? node)
      (let [tag (get node :tag)]
        (cond
          (or (= tag :def) (= tag :def-))
          (do
            (when (or (= tag :def-) (and (not top?) (= tag :def)))
              (declare-symbol (get-in node [:value 0 :value 0])))
            (traverse (get-in node [:value 1])))

          (or (= tag :defn) (= tag :defn-))
          (do
            (when (or (= tag :defn-) (and (not top?) (= tag :defn)))
              (declare-symbol (get-in node [:value 0 :value 0])))
            (new-scope)
            (each param (get-in node [:value 1 :value] @[])
              (if-not (some |(= (param :value) $) ["&opt" "&keys" "&named"])
                (declare-symbol param)))
            (each expr (slice (get node :value) 2)
              (traverse expr))
            (exit-scope))

          (= tag :let)
          (do
            (new-scope)
            (each param (get-in node [:value 0 :value])
              (declare-symbol param))
            (each expr (slice (get node :value) 1)
              (traverse expr))
            (exit-scope))

          (let [node (get node :value)]
            (if (string? node)
              (use-symbol node)
              (each val node
                (traverse val))))))))

  (each node (get tree :value)
    (traverse node true))

  (seq [sym :in declared-symbols
        :unless (find |(= $ sym) used-symbols)]
    {:character (dec (get sym :col))
     :line (dec (get sym :line))
     :value (get sym :value)}))

(pp (file-warning-check tree))
