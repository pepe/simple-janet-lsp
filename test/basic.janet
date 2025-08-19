(import ../simple-janet-lsp/parser)
(import ../simple-janet-lsp/eval)

(setdyn *pretty-format* "%P")

# (def source `(loop [x :in y [key val] :iterate my-iter] (print x))`)
# (def source `(loop [:repeat x :after (break) :until 1] nil)`)
# (def source `(loop [[x y] :in foo :before (my-func woo) i :range [0 10] :after y :repeat 1 :let [myx nil myy x]] nil)`)
# (def source `(let [x nil y 12])`)
# (def source `(loop [:let [x nil y 10]] x)`)
# (def source `(loop [:let [my-x nil my-y nil]] (print my))`)
(def source `(let [my-x nil my-y nil] (print my))`)
# (pp (eval/file-warning-check source))
# (pp (parser/get-syms-at-loc {"character" 42 "line" 0} source))
# (def source `(defn foo [x y] 1)`)

(def tree (parser/tuple-at {"character" 14 "line" 48} source))
(pp tree)
(pp (parser/sym-loc "*eof-peg*" tree))
# (pp tree)

