(import ./flycheck)
(import ./parser)
(import ./utils)

(def- *eof-peg*
  (peg/compile
    ~{:digit (/ (<- :d+) ,(comp dec scan-number))
      :main (some (+ (* "opened at line " :digit ", column " :digit) 1))}))

(defn- eval-error-pat [pat]
  (peg/compile ~(some (+ (* (line) (column) ,pat) 1))))

(defn- eval-error-loc [message text source]
  (if (string/has-prefix? "could not find module" message)
    (let [loc (-> (utils/tuple->string source) (eval-error-pat) (peg/match text))]
      (map dec loc))
    [0 0]))

(defn file-error-check [filepath text]
  (var err nil)
  (def file-env (make-env root-env))
  (def filepath
    (if (string/has-prefix? "file:///" filepath)
      (string/slice filepath 8)
      filepath))

  (var str text)

  (defn chunks [buf _]
    (when str
      (buffer/push-string buf str)
      (buffer/push-string buf "\n")
      (set str nil)))

  (defn evaluator [thunk source env where]
    (unless err
      (match (protect (flycheck/flycheck-evaluator thunk source env where))
        [false e] (set err {:type :eval
                            :diagnostic {:message e
                                         :location (eval-error-loc e text source)}}))))

  (defn on-compile-error [msg _ _ line col]
    (unless err (set err {:type :compile
                          :diagnostic {:message msg
                                       :location [(dec line) (dec col)]}})))

  (defn on-parse-error [p _]
    (let [message (parser/error p)
          eof (peg/match *eof-peg* message)
          location (if-not (empty? eof) eof (map dec (parser/where p)))]
      (unless (= (get err :type) :parse)
        (set err {:type :parse
                  :diagnostic {:message message
                               :location location}}))))

  (def old-modcache (table/clone module/cache))
  (table/clear module/cache)

  (run-context {:chunks chunks
                :env file-env
                :evaluator evaluator
                :on-compile-error on-compile-error
                :on-compile-warning on-compile-error
                :on-parse-error on-parse-error
                :source filepath})

  (table/clear module/cache)
  (merge-into module/cache old-modcache)

  [(get err :diagnostic) file-env])

(defn file-warning-check [source]
  (def tree (parser/make-tree source))

  (def declared-symbols @[])
  (def used-symbols @{})
  (def scope-stack @[@[]])

  (defn new-scope []
    (array/push scope-stack @[]))

  (defn exit-scope []
    (array/pop scope-stack))

  (defn declare-symbol [sym]
    (array/push (last scope-stack) sym)
    (array/push declared-symbols sym))

  (defn use-symbol [name]
    (defn riter [ind]
      (generate [i :down-to [(dec (length ind)) 0]] (get ind i)))

    (prompt :return
      (loop [scope :in (riter scope-stack)
             sym :in (riter scope)
             :when (= (get sym :value) name)
             :after (return :return)]
        (put used-symbols sym true))))

  (defn traverse [node &opt top?]
    (unless (struct? node) (break))

    (match (get node :tag)
      (tag (or (= tag :def) (= tag :def-)))
      (do
        (traverse (get-in node [:value 1]))
        (when (or (= tag :def-) (and (not top?) (= tag :def)))
          (declare-symbol (get-in node [:value 0 :value 0]))))

      (tag (or (= tag :defn) (= tag :defn-)))
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

      :let
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

      _
      (let [node (get node :value)]
        (if (string? node)
          (use-symbol node)
          (each val node
            (traverse val))))))

  (each node (get tree :value) (traverse node true))

  (seq [sym :in declared-symbols :unless (get used-symbols sym)]
    {:character (get sym :col)
     :line (get sym :line)
     :value (get sym :value)}))

# (def [err env] (eval-file "simple-janet-lsp/init.janet" (slurp "simple-janet-lsp/init.janet")))
# (def [err env] (eval-file "test/basic.janet" (slurp "test/basic.janet")))
# (print "Errors:")
# (printf "%Q" err)
# (print "Env:")
# (printf "%Q" env)

# (defn get-doc [sym env]
#   (get-in env [sym :doc]))

# (print (get-doc 'json/encode env))

