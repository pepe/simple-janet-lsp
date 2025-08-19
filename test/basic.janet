(import ../simple-janet-lsp/parser)

(setdyn *pretty-format* "%P")

# (def source `(loop [x :in y [key val] :iterate my-iter] (print x))`)
# (def source `(loop [:repeat x :after (break) :until 1] nil)`)
# (def source `(loop [[x y] :in foo :before (my-func) i :range [0 10] :let [myx nil myy x]] nil)`)
# (def source `(let [x nil y 12])`)
# (def source `(loop [:let [x nil y 10]] x)`)
(def source `(loop [:let [my-x nil my-y nil]] (print my))`)
# (def source `(let [my-x nil my-y nil] (print my))`)
(pp (parser/get-syms-at-loc {"character" 41 "line" 0} source))
# (def tree (parser/make-tree source))
# (pp tree)

