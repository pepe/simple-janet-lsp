(defn tuple->string [tup]
  (def buf @"")
  (with-dyns [:out buf] (prinf "%q" tup))
  buf)

(defn get-index [{"character" char-pos "line" line-pos} source]
  (let [lines (string/split "\n" source)
        pre-lines (array/slice lines 0 line-pos)
        pre-index (sum (map (comp inc length) pre-lines))]
    (+ pre-index char-pos)))
