(import ../simple-janet-lsp/parser)

(setdyn *pretty-format* "%P")

(def source `(def- private-foo nil)(def public-bar nil)(var public-baz nil)(var- private-boo nil) (defn new-diagnostic [{:location [line char] :message message} text]
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
# (def source (slurp "simple-janet-lsp/eval.janet"))

(def loc {"character" 0 "line" 0})

(def tree (parser/make-tree source))

(defn special-param? [str]
  (some |(= $ str) ["&opt" "&named" "&keys"]))

(defn all-variables [tree]
  (def variables @[])

  (defn public-variable? [node]
    (if (= (type node) :struct)
      (not= (get node :tag) :def)
      false))

  (def nodes @[{:value (filter public-variable? (get tree :value))}])

  (while (not (empty? nodes))
    (def tree (array/pop nodes))
    (def tag (get tree :tag))
    (def value (get tree :value))

    (when (or (= tag :variables) (= tag :parameters))
      (array/push variables value))

    (when (indexed? value)
      (each val value (array/push nodes val))))

  (->> (flatten variables)
       (filter |(not (special-param? ($ :value))))
       (map |[($ :value)
              {:index ($ :index)
               :line (dec ($ :line))
               :character (dec ($ :col))}])))

(defn all-values [tree]
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

(def variables (all-variables tree))
(def vals (all-values tree))

(defn used-variable? [[variable {:index index}]]
  (let [indexes (get vals variable)]
    (when-let [index (find-index |(= $ index) indexes)]
      (array/remove indexes index))
    (not (>= (length indexes) 1))))
# (pp variables)
(pp tree)
# (pp (filter used-variable? variables))

