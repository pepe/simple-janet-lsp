# From: https://github.com/janet-lang/spork/blob/eb8ba6bd042f6beb66cbf5194ac171cfda98424e/spork/getline.janet#L14C1-L20C29
(def- *word-at-peg*
  (peg/compile
    ~{:symchar (+ (range "\x80\xff" "AZ" "az" "09") (set "!$%&*+-./:<?=>@^_\"`"))
      :anchor (drop (cmt ($) ,|(= $ 0)))
      :cap (* (+ (> -1 (not :symchar)) :anchor) (* ($) '(some :symchar)))
      :recur (+ :cap (> -1 :recur))
      :main (> -1 :recur)}))

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

(defn tuple->string [tup]
  (def buf @"")
  (with-dyns [:out buf] (prinf "%q" tup))
  buf)

(defn get-index [{"character" char-pos "line" line-pos} source]
  (let [lines (string/split "\n" source)
        pre-lines (array/slice lines 0 line-pos)
        pre-index (sum (map (comp inc length) pre-lines))]
    (+ pre-index char-pos)))
