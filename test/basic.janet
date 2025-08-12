# (import ../simple-janet-lsp/parser)

# (setdyn *pretty-format* "%P")

# (def source `(if-let [[_ word] (try (peg/match *word-at-peg* str (inc pos)) ([_] nil))]
#     word)`)

# (pp (parser/sym-loc "word" source))

(import ../simple-janet-lsp/eval)

(def source `(foo-bar)

  (baz)`)

(print (eval/tuple-at source {"character" 2 "line" 2}))
