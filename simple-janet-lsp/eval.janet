(import ./flycheck)
(import ./parser)
(import ./utils)

(def- *eof-peg*
  (peg/compile
    ~{:digit (/ (<- :d+) ,scan-number)
      :main (some (+ (* "opened at line " :digit ", column " :digit) 1))}))

(defn- eval-error-pat [pat]
  (peg/compile ~(some (+ (* (line) (column) ,pat) 1))))

(defn- eval-error-loc [message text source]
  (if (string/has-prefix? "could not find module" message)
    (-> (utils/tuple->string source) (eval-error-pat) (peg/match text))
    [1 1]))

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
        [false e] (set err {:type :eval :diagnostic {:message e :location (eval-error-loc e text source)}}))))

  (defn on-compile-error [msg _ _ line col]
    (unless err (set err {:type :compile :diagnostic {:message msg :location [line col]}})))

  (defn on-parse-error [p _]
    (let [message (parser/error p)
          eof (peg/match *eof-peg* message)
          location (if-not (empty? eof) eof (parser/where p))]
      (unless (= (get err :type) :parse)
        (set err {:type :parse :diagnostic {:message message :location location}}))))

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

(defn- all-variables [tree]
  (def variables @[])

  (defn not-private-variable? [node]
    (if (= (type node) :struct)
      (not= (get node :tag) :def)
      false))

  (def nodes @[{:value (filter not-private-variable? (get tree :value))}])

  (while (not (empty? nodes))
    (def tree (array/pop nodes))
    (def tag (get tree :tag))
    (def value (get tree :value))

    (when (or (= tag :variables) (= tag :parameters))
      (array/push variables value))

    (when (indexed? value)
      (each val value (array/push nodes val))))

  (defn special-param? [str]
    (some |(= $ str) ["&opt" "&named" "&keys"]))

  (->> (flatten variables)
       (filter |(not (special-param? ($ :value))))
       (map |[($ :value)
              {:index ($ :index)
               :line (dec ($ :line))
               :character (dec ($ :col))}])))

(defn- all-values [tree]
  (def vals @{})
  (def nodes @[tree])

  (while (not (empty? nodes))
    (def tree (array/pop nodes))
    (def value (get tree :value))
    (def index (get tree :index))

    (unless (indexed? value)
      (if-let [val (get vals value)]
        (update vals value |(array/push $ index))
        (put vals value @[index])))

    (when (indexed? value)
      (each val value (array/push nodes val))))

  vals)

(defn file-warning-check [source]
  (def tree (parser/make-tree source))
  (def variables (all-variables tree))
  (def vals (all-values tree))

  (defn used-variable? [[variable {:index index}]]
    (let [indexes (get vals variable)]
      (when-let [index (find-index |(= $ index) indexes)]
        (array/remove indexes index))
      (not (>= (length indexes) 1))))

  (filter used-variable? variables))

# (def [err env] (eval-file "simple-janet-lsp/init.janet" (slurp "simple-janet-lsp/init.janet")))
# (def [err env] (eval-file "test/basic.janet" (slurp "test/basic.janet")))
# (print "Errors:")
# (printf "%Q" err)
# (print "Env:")
# (printf "%Q" env)

# (defn get-doc [sym env]
#   (get-in env [sym :doc]))

# (print (get-doc 'json/encode env))

