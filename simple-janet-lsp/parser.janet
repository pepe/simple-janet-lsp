# From: https://github.com/CFiggers/janet-lsp/blob/ef025e2bdd948e7d616721f2c9b9de14fb4b6e10/src/parser.janet

(import ./utils)

(defn- tagged-value [tag]
  (fn [x]
    {:tag tag :value x}))

(defn- identifier-node []
  (fn [line col index value end]
    (let [len (- end index)]
      {:value value :index index :len len :line line :col col})))

(defn- tagged-node [tag]
  (fn [line col index value end]
    (let [len (- end index)]
      {:tag tag :value value :index index :len len :line line :col col})))

(defn- concat-tagged-node [tag]
  (fn [line col index value end]
    (let [len (- end index)]
      {:tag tag :value (apply array/concat value) :index index :len len :line line :col col})))

(defn even-slots [ind]
  (map |(get $ 0) (partition 2 ind)))

(defn odd-slots [ind]
  (filter |(not (nil? $)) (map |(get $ 1) (partition 2 ind))))

(defn- let-parsing []
  (fn [x]
    @[{:tag :parameters :value (even-slots x)}
      {:tag :expr :value (odd-slots x)}]))

(defn- loop-let-parsing []
  (fn [x]
    {:tag :loop-let
     :value @[{:tag :parameters :value (even-slots x)}
              {:tag :expr :value (odd-slots x)}]}))

(defn- loop-prefix-parsing []
  (fn [x]
    {:tag :loop-modifiers :value x}))

(defn- wrap-position-capture [inner]
  ~(* (/ (line) ,dec) (/ (column) ,dec) ($)
      ,inner
      ($)))

(def- parse-peg
  "PEG to extract identifier symbols and locations"
  (peg/compile
    ~{:ws (+ :comment (set " \t\r\f\0\v\n"))
      :readermac (set "';~,|")
      :symchars (+ :w "\x80\xFF" (set "!$%&*+-./:<?=>@^_"))
      :token (some :symchars)
      :escape (* "\\" (+ (set "ntrzfev0ab'?\"\\")
                         (* "x" :h :h)
                         (* "u" :h :h :h :h)
                         (* "U" :h :h :h :h :h :h)))
      :comment (* "#" (any (if-not (+ "\n" -1) 1)) (+ "\n" -1))
      :span (/ ,(wrap-position-capture
                  ~(<- :token))
               ,(identifier-node))
      :bytes '(* "\"" (any (+ :escape (if-not "\"" 1))) "\"")
      :string (drop :bytes)
      :numeric (+ :d "_")
      :number (* (? (set "+-"))
                 (+ (* :d (? (* :numeric)) (? (* "." :numeric)))
                    (* "." (* :numeric)))
                 (? (* (+ "e" "E") (? (set "-+")) (some :d))))
      :buffer (drop (* "@" :bytes))
      :long-bytes '{:delim (some "`")
                    :open (capture :delim :n)
                    :close (cmt (* (not (> -1 "`")) (-> :n) ':delim) ,=)
                    :main (drop (* :open (any (if-not :close 1)) :close))}
      :long-string (drop :long-bytes)
      :long-buffer (drop (* "@" :long-bytes))

      :terminal (+ :ws (set ")}]"))
      :non-identifier (+ (* "_" (look 0 :terminal)) (* "&" (look 0 :terminal)) :number)
      :identifier (/ ,(wrap-position-capture
                        ~(<- (if-not :non-identifier :token)))
                     ,(identifier-node))
      :symbol-parameter (+ :identifier :non-identifier)

      :bdestruct (* "[" (any :ws) (any (* :parameter (any :ws))) "]")
      :pdestruct (* "(" (any :ws) (any (* :parameter (any :ws))) ")")

      :table-binding (* (+ :token :string) (any :ws) :parameter (any :ws))
      :tdestruct (* "{" (any :ws) (any :table-binding) "}")

      :parameter (+ :symbol-parameter :bdestruct :pdestruct :tdestruct)
      :parameters (any (* :parameter (any :ws)))

      # it helps to allow the expression we are assigning to the parameter
      # to be optional to allow for prior parameters to be available
      :let-binding (* (group :parameter) (? (* (some :ws) (group :form))) (any :ws))

      :let (/ ,(wrap-position-capture
                 ~(group (* "(" (any :ws) (+ "let" "if-let" "when-let") (some :ws)
                            "[" (any :ws)
                            (/ (group (any :let-binding)) ,(let-parsing))
                            "]" (any :input)
                            ")")))
              ,(concat-tagged-node :let))

      :defn- (/ ,(wrap-position-capture
                   ~(group (* "(" (any :ws)
                              (+ "defn-" "defmacro-") (some :ws)
                              (/ (group :identifier) ,(tagged-value :fn-)) (some :ws)
                              (? (* (+ :string :long-string) (some :ws)))
                              "[" (any :ws)
                              (/ (group :parameters) ,(tagged-value :parameters))
                              "]" (any :input)
                              ")")))
                ,(tagged-node :defn-))

      :defn (/ ,(wrap-position-capture
                  ~(group (* "(" (any :ws)
                             (+ "defn" "defmacro") (some :ws)
                             (/ (group :identifier) ,(tagged-value :fn)) (some :ws)
                             (? (* (+ :string :long-string) (some :ws)))
                             "[" (any :ws)
                             (/ (group :parameters) ,(tagged-value :parameters))
                             "]" (any :input)
                             ")")))
               ,(tagged-node :defn))

      :lambda (/ ,(wrap-position-capture
                    ~(group (* "(" (any :ws)
                               "fn" (some :ws)
                               "[" (any :ws)
                               (/ (group :parameters) ,(tagged-value :parameters))
                               "]"
                               (any :input)
                               ")")))
                 ,(tagged-node :lambda))

      :def- (/ ,(wrap-position-capture
                  ~(group (* "(" (any :ws)
                             (+ "def-" "var-") (some :ws)
                             (/ (group :parameter) ,(tagged-value :variables)) (any :input)
                             ")")))
               ,(tagged-node :def-))

      :def (/ ,(wrap-position-capture
                 ~(group (* "(" (any :ws)
                            (+ "def" "var") (some :ws)
                            (/ (group :parameter) ,(tagged-value :variables)) (any :input)
                            ")")))
              ,(tagged-node :def))

      :for-each (/ ,(wrap-position-capture
                      ~(group (* "(" (any :ws)
                                 (+ "for" "each") (some :ws)
                                 (/ (group :parameter) ,(tagged-value :parameters)) (any :input)
                                 ")")))
                   ,(tagged-node :for-each))

      :loop-verbs (+ ":in" ":keys" ":pairs" ":range" ":range-to" ":down" ":down-to" ":iterate")
      :loop-infix (* (group :parameter) (some :ws) :loop-verbs (some :ws) (group :form) (any :ws))

      :loop-modifiers (+ ":while" ":until" ":before" ":after" ":repeat" ":when" ":unless")
      :loop-prefix (* :loop-modifiers (some :ws) (group :form) (any :ws))

      :loop-let (* ":let" (some :ws)
                   "[" (any :ws)
                   (/ (group (any :let-binding)) ,(loop-let-parsing))
                   "]" (any :ws))

      :loop-head (* (any (+ :loop-let
                            (/ (group :loop-prefix) ,(loop-prefix-parsing))
                            (/ (group :loop-infix) ,(loop-let-parsing))))
                    (any :ws))

      :loop (/ ,(wrap-position-capture
                  ~(group (* "(" (any :ws)
                             (+ "loop" "seq" "catseq" "tabseq" "generate") (some :ws)
                             "[" (any :ws)
                             (group :loop-head) (any :input)
                             "]"
                             (any :input)
                             ")")))
               ,(concat-tagged-node :loop))

      :ptuple (/ ,(wrap-position-capture
                    ~(group (* "(" (any :input) ")")))
                 ,(tagged-node :ptuple))
      :btuple (/ ,(wrap-position-capture
                    ~(group (* "[" (any :input) "]")))
                 ,(tagged-node :btuple))
      :struct (/ ,(wrap-position-capture
                    ~(group (* "{" (any :input) "}")))
                 ,(tagged-node :struct))
      :parray (/ ,(wrap-position-capture
                    ~(group (* "@(" (any :input) ")")))
                 ,(tagged-node :parray))
      :barray (/ ,(wrap-position-capture
                    ~(group (* "@[" (any :input) "]")))
                 ,(tagged-node :barray))
      :table (/ ,(wrap-position-capture
                   ~(group (* "@[" (any :input) "]")))
                ,(tagged-node :table))
      :rmform (/ ,(wrap-position-capture
                    ~(group (* ':readermac
                               (group (any :non-form))
                               :form)))
                 ,(tagged-node :rmform))

      :form (choice :let :defn- :defn :lambda :def- :def
                    :for-each :loop :rmform
                    :parray :barray :ptuple :btuple :table :struct
                    :buffer :string :long-buffer :long-string
                    :span)
      :non-form (choice :ws :comment)
      :input (choice :non-form :form)
      :main (* (any :input))}))

(defn make-tree
  "Turn a string of source code into an AST"
  [source]
  {:tag :top :value (peg/match parse-peg source)})

(defn- get-value-for-tag [tag node]
  (if (= tag (get node :tag))
    (get node :value)
    @[]))

(defn- get-defined-for-tag [tag node]
  (let [value (get node :value)]
    (if (indexed? value)
      (catseq [v :in value] (get-value-for-tag tag v))
      @[])))

(defn- get-fn-names [node]
  (array/concat (get-value-for-tag :fn node) (get-defined-for-tag :fn node)))

(defn- collect-symbols [heads]
  (let [parameters (catseq [head :in heads] (flatten (get-value-for-tag :parameters head)))
        variables (catseq [head :in heads] (get-defined-for-tag :variables head))
        fn-names (catseq [head :in heads] (get-fn-names head))
        pars-and-vars (array/concat parameters variables)
        lsp-symbols (seq [p-or-v :in pars-and-vars] {:kind 12 :label (symbol (p-or-v :value))})
        lsp-fn-names (seq [f :in fn-names] {:kind 3 :label (symbol (f :value))})]
    (array/concat lsp-symbols lsp-fn-names)))

(varfn find-symbols-in-node [])

(defn- before-node? [pos node]
  (when-let [index (get node :index)]
    (< pos index)))

(defn- find-symbols-in-nodes [head nodes pos]
  (if-let [node (first nodes)]
    (or
      (when (before-node? pos node)
        (tuple head @[]))
      (if-let [syms (find-symbols-in-node node pos)]
        (tuple head syms))
      (if-let [rest (array/slice nodes 1)
               result (find-symbols-in-nodes (tuple node) rest pos)
               (heads syms) result]
        (tuple (tuple/join head heads) syms)))
    (tuple head @[])))

(defn- get-syms-from-tree [tree pos]
  (let [value (get tree :value)]
    (when (indexed? value)
      (if-let [result (find-symbols-in-nodes '() value pos)
               [heads syms] result]
        (array/join syms (collect-symbols heads))))))

(defn- in-node? [pos node]
  (if-let [start (get node :index)
           end (+ start (get node :len))]
    (and (<= start pos)
         (< pos end))))

(varfn find-symbols-in-node [node pos]
  (when (in-node? pos node)
    (if (indexed? (get node :value))
      (if (= (get node :tag) :loop)
        (let [node (->> (get node :value)
                        (filter |(= ($ :tag) :loop-let))
                        (mapcat |($ :value)))]
          (get-syms-from-tree {:value node} pos))
        (get-syms-from-tree node pos))
      @[])))

(defn- blank-source [source start end]
  (string
    (string/slice source 0 start)
    (string/repeat " " (- end start))
    (string/slice source end)))

(defn- get-blanked-source [{"character" char-pos "line" line-pos} source]
  (def line (get (string/split "\n" source) line-pos))
  (if-let [word (utils/word-at line char-pos)]
    (let [word-end (utils/get-index {"character" char-pos "line" line-pos} source)
          word-start (- word-end (length word))]
      (blank-source source word-start word-end))
    source))

(defn get-syms-at-loc [loc source]
  (let [blanked-source (get-blanked-source loc source)
        index (utils/get-index loc source)
        tree (make-tree blanked-source)]
    (get-syms-from-tree tree index)))

(defn tuple-at [loc source]
  (defn recur [nodes index]
    (if (empty? nodes)
      nil
      (let [tree (first nodes)
            value (get tree :value)]
        (if (= (get tree :index) index)
          tree
          (let [inner (if (indexed? value) (flatten value) @[])
                rest (array/slice nodes 1)
                nodes (array/concat inner rest)]
            (recur nodes index))))))
  (recur @[(make-tree source)] (utils/get-index loc source)))

(defn sym-loc [sym tree]
  (defn recur [nodes sym]
    (if (empty? nodes)
      nil
      (let [tree (first nodes)
            value (get tree :value)]
        (if (and (not (indexed? value)) (= value sym))
          {:character (get tree :col)
           :line (get tree :line)}
          (let [inner (if (indexed? value) (flatten value) @[])
                rest (array/slice nodes 1)
                nodes (array/concat inner rest)]
            (recur nodes sym))))))
  (recur @[tree] sym))
