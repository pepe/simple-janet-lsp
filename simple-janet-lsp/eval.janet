(import ./flycheck)
(import ./utils)

# From: https://github.com/janet-lang/spork/blob/eb8ba6bd042f6beb66cbf5194ac171cfda98424e/spork/getline.janet#L14C1-L20C29
(def- *word-at-peg*
  (peg/compile
    ~{:symchar (+ (range "\x80\xff" "AZ" "az" "09") (set "!$%&*+-./:<?=>@^_"))
      :anchor (drop (cmt ($) ,|(= $ 0)))
      :cap (* (+ (> -1 (not :symchar)) :anchor) (* ($) '(some :symchar)))
      :recur (+ :cap (> -1 :recur))
      :main (> -1 :recur)}))

(def- *eof-peg*
  (peg/compile
    ~{:digit (/ (<- :d+) ,scan-number)
      :main (some (+ (* "opened at line " :digit ", column " :digit) 1))}))

(defn word-at [str pos]
  (if-let [[_ word] (try (peg/match *word-at-peg* str (inc pos)) ([_] nil))]
    word))

(defn form-at [str pos]
  (var result nil)
  (var par 0)

  (def rstr (try (reverse (slice (pairs str) 0 pos)) ([_] (break))))

  (each [pos char] rstr
    (case (string/from-bytes char)
      ")" (++ par)
      "(" (-- par))

    (when (neg? par)
      (set result (word-at str (inc pos)))
      (break)))

  result)

(defn tuple-at
  "Expects the `loc` to be on either `(` or `[`. Meant to be used for diagnostics."
  [source loc]
  (def index (utils/get-index loc source))

  (var par 0)
  (var end index)

  (def [open close]
    (case (string/slice source index (inc index))
      "(" ["(" ")"]
      "[" ["[" "]"]
      (break)))

  (each char (string/slice source index)
    (case (string/from-bytes char)
      open (++ par)
      close (-- par))
    (when (zero? par) (break))
    (++ end))

  (string/slice source index (inc end)))

(defn- eval-error-pat [pat]
  (peg/compile ~(some (+ (* (line) (column) ,pat) 1))))

(defn- eval-error-loc [message text source]
  (if (string/has-prefix? "could not find module" message)
    (-> (utils/tuple->string source)
        (eval-error-pat)
        (peg/match text))
    [1 1]))

(defn eval-file [filepath text]
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

# (def [err env] (eval-file "simple-janet-lsp/init.janet" (slurp "simple-janet-lsp/init.janet")))
# (def [err env] (eval-file "test/basic.janet" (slurp "test/basic.janet")))
# (print "Errors:")
# (printf "%Q" err)
# (print "Env:")
# (printf "%Q" env)

# (defn get-doc [sym env]
#   (get-in env [sym :doc]))

# (print (get-doc 'json/encode env))

