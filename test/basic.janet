(import ../simple-janet-lsp/parser)

(setdyn *pretty-format* "%P")


(def source (slurp "simple-janet-lsp/eval.janet"))

# (def source `(def top-level nil) (def- top-level-pri nil)(defn pub [[mmm {:foo fbar}]]) (defn- not-used [mmm])(defn- unused-private-fn [foo] (print mmm))(let [[foo1 foo2] nil] nil)(def [foo3 foo4])`)
# (def source `(def foo nil) (print foo)(defn my-func [used unused] (print used)) (my-func nil nil)`)
# (def source `(defn- used-func [unsed-par] (let [used nil unused nil] (let [ununused nil] (print used))))(used-func)`)
(def source ````(defn foo []
  (let [[label & rest] (string/split "\n" doc)
        rest (string/join rest "\n")
        doc (string "```janet\n" label "\n```" rest)]
    (print doc)
    (pp rest)))````)
# (def source `(let [[x y] nil
#       z nil
#       [m n] nil]
#   (print x y z))`)
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
      (when-let [sym (find |(= ($ :value) name) (reverse (get scope-stack i)))]
        (array/push used-symbols sym)
        (break))
      (-- i)))

  (defn traverse [node &opt top?]
    (if (struct? node)
      (let [tag (get node :tag)]
        (cond
          (or (= tag :def) (= tag :def-))
          (do
            (traverse (get-in node [:value 1]))
            (when (or (= tag :def-) (and (not top?) (= tag :def)))
              (declare-symbol (get-in node [:value 0 :value 0]))))

          (or (= tag :defn) (= tag :defn-))
          (do
            (new-scope)
            (each param (get-in node [:value 1 :value] @[])
              (if-not (some |(= (param :value) $) ["&opt" "&keys" "&named"])
                (declare-symbol param)))
            (each expr (array/slice (get node :value) 2)
              (traverse expr))
            (exit-scope)
            (when (or (= tag :defn-) (and (not top?) (= tag :defn)))
              (declare-symbol (get-in node [:value 0 :value 0]))))

          (= tag :let)
          (let [node (get node :value)
                raw-pairs (get (first (array/slice node 2 3)) :value)
                params (parser/even-slots raw-pairs)
                exprs (parser/odd-slots raw-pairs)
                expr-param-pairs (partition 2 (interleave exprs params))]
            (new-scope)
            (each [exprs params] expr-param-pairs
              (each e exprs (traverse e))
              (each p params (declare-symbol p)))
            (each expr (array/slice node 3)
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
# (pp tree)

