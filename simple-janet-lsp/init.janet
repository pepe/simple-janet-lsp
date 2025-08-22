(import spork/argparse)
(import spork/json)

(import ./eval)
(import ./log)
(import ./parser)
(import ./utils)

(def *files* @{})
(def *msgs* (ev/thread-chan 1000))
(def *new-lines* (if (= (os/which) :windows) "\n\n" "\r\n\r\n"))

(defn get-message []
  (def content (file/read stdin :line))
  (unless (buffer? content) (break))
  (def content-length (scan-number (string/trim content "Content-Length: \r\n")))
  (file/read stdin :line)
  (json/decode (file/read stdin content-length)))

(defn create-result [message]
  (string "Content-Length: " (length message) *new-lines* message))

(defn write-response [id result]
  (let [message (json/encode {:jsonrpc "2.0" :id id :result result})
        result (create-result message)]
    (prin result)
    (flush)))

(defn write-notification [method params]
  (let [message (json/encode {:jsonrpc "2.0" :method method :params params})
        result (create-result message)]
    (prin result)
    (flush)))

(defn new-error-diagnostic [{:location [line char] :message message} text]
  (var start @{:line line :character char})
  (var end @{:line line :character (inc char)})
  (def error-line (get (string/split "\n" text) line))

  (when-let [word (utils/word-at error-line char)]
    (put end :character (+ (inc char) (length word))))

  (def char-at-error (string/slice error-line char (inc char)))

  (defn word-range [word]
    (unless (some |(= $ char-at-error) ["(" "[" "'" ";" "~" "," "|"]) (break))

    (def tup (parser/tuple-at {"character" char "line" line} text))
    (def {:character char-pos :line line-pos} (parser/sym-loc word tup))

    (set start {:character char-pos :line line-pos})
    (set end {:character (+ char-pos (length word)) :line line-pos}))

  (cond
    (string/has-prefix? "unknown symbol" message)
    (word-range (string/slice message 15))

    (string/has-prefix? "could not find module" message)
    (word-range (->> (string/slice message 22) (string/split ":") (first))))

  @[{:range {:start start :end end} :message message :severity 1}])

(defn new-warning-diagnostic [text]
  (catseq [{:value var :character char :line line} :in (eval/file-warning-check text)
           :let [message (string "unused variable " var)
                 start {:character char :line line}
                 end {:character (+ char (length var)) :line line}]]
    {:range {:start start :end end} :message message :severity 2}))

(defn update-files [uri text]
  (let [[err env] (eval/file-error-check uri text)
        error-diagnostic (if err (new-error-diagnostic err text) @[])
        warning-diagnostics (new-warning-diagnostic text)
        diagnostics (array/concat error-diagnostic warning-diagnostics)]
    (put *files* uri @{:text text :env env})
    (write-notification "textDocument/publishDiagnostics"
                        {:uri uri :diagnostics diagnostics})))

(defn update-diagnostics [msg]
  (let [uri (get-in msg ["params" "textDocument" "uri"])
        text (get-in msg ["params" "contentChanges" 0 "text"])]
    (update-files uri text)))

(defn did-open [msg]
  (let [{"text" text "uri" uri} (get-in msg ["params" "textDocument"])]
    (update-files uri text)))

(def *timer-sv* (ev/chan))
(var *timer* nil)

(defn did-change-debounce [msg]
  (defn sleep []
    (ev/sleep 0.3)
    (set *timer* nil)
    (ev/give *msgs* [:update-diagnostics msg]))

  (defn start-timer []
    (set *timer* (ev/go sleep nil *timer-sv*)))

  (if-not *timer*
    (start-timer)
    (do
      (ev/cancel *timer* "")
      (ev/take *timer-sv*)
      (start-timer))))

(defn did-change [msg]
  (did-change-debounce msg)
  (let [uri (get-in msg ["params" "textDocument" "uri"])
        text (get-in msg ["params" "contentChanges" 0 "text"])]
    (when-let [file (get *files* uri)]
      (put file :text text))))

(defn completion [id msg]
  (def uri (get-in msg ["params" "textDocument" "uri"]))
  (def file (get *files* uri))
  (def env (get file :env))

  (def {"character" pos "line" line} (get-in msg ["params" "position"]))

  (def text (get file :text))
  (def line (get (string/split "\n" text) line))
  (def word (utils/word-at line pos))

  (when (and word (some |(string/has-prefix? $ word) ["\"" ":" "`"]))
    (write-response id :null)
    (break))

  (when-let [form (utils/form-at line pos)]
    (when (or (= form "import") (= form "use"))
      (write-response id :null)
      (break)))

  (def break-char-pos
    (if word (max-of (mapcat |(string/find-all $ word) ["/" "-" "*"]))))

  (def prefix
    (if break-char-pos (string/slice word 0 (inc break-char-pos))))

  # From: https://github.com/CFiggers/janet-lsp/blob/b3fa52bf66ac8f305e8bd53eca651b1948a70212/src/main.janet#L159-L167
  (defn item-kind [binding]
    (case (type (get-in env [binding :value]))
      :symbol 12 :boolean 6 :core/file 17 :core/peg 6
      :function 3 :cfunction 3 :struct 6 :table 6
      :string 6 :buffer 6 :tuple 6 :array 6
      :number 6 :keyword 6 :fiber 6 :nil 6))

  (defn to-item [binding &opt kind]
    (default kind (item-kind binding))
    (def insert-text
      (if prefix (string/slice binding (inc break-char-pos))))
    {:label binding :insertText insert-text :kind kind :data uri})

  (defn has-prefix? [binding]
    (if prefix (string/has-prefix? prefix binding)))

  (def local-syms (parser/get-syms-at-loc (get-in msg ["params" "position"]) text))

  (def global-bindings
    (if break-char-pos
      (filter has-prefix? (all-bindings env))
      (all-bindings env)))

  (def local-bindings
    (if break-char-pos
      (filter |(has-prefix? ($ :label)) local-syms)
      local-syms))

  (def global-items (map to-item global-bindings))
  (def local-items (map |(to-item ($ :label) ($ :kind)) local-bindings))

  (def all-items @[])
  (def seen @{})

  (loop [items :in [local-items global-items]
         item :in items
         :let [label (get item :label)]
         :unless (get seen label)]
    (put seen label true)
    (array/push all-items item))

  (write-response id {:isIncomplete false :items all-items}))

(defn get-doc [binding env]
  (get-in env [(symbol binding) :doc]))

(defn completion-resolve [id msg]
  (def file (get *files* (get-in msg ["params" "data"])))
  (def env (get file :env))

  (def item (get-in msg ["params"]))
  (def label (get item "label"))
  (def doc (or (get-doc label env) label))

  (put item :documentation {:kind "markdown" :value doc})

  (write-response id item))

(defn hover [id msg]
  (def file (get *files* (get-in msg ["params" "textDocument" "uri"])))
  (def env (get file :env))
  (def text (get file :text))

  (def {"character" pos "line" line} (get-in msg ["params" "position"]))

  (def line (get (string/split "\n" text) line))
  (def word (utils/word-at line (inc pos)))
  (def doc (get-doc word env))

  (def result
    (if doc
      (let [[label & rest] (string/split "\n" doc)
            rest (string/join rest "\n")
            doc (string "```janet\n" label "\n```" rest)]
        {:contents {:kind "markdown" :value doc}})
      :null))

  (write-response id result))

(defn signature-help [id msg]
  (def file (get *files* (get-in msg ["params" "textDocument" "uri"])))
  (def env (get file :env))
  (def text (get file :text))

  (def {"character" pos "line" line} (get-in msg ["params" "position"]))

  (def line (get (string/split "\n" text) line))
  (def word (utils/form-at line pos))
  (def doc (get-doc word env))

  (def result
    (if doc
      (let [[label & rest] (string/split "\n" doc)
            rest (string/triml (string/join rest "\n"))
            documentation (if-not (empty? rest) {:kind "markdown" :value (string rest "\n.")})]
        {:signatures [{:label label :documentation documentation}]})
      :null))

  (write-response id result))

(def initialize-result
  {:capabilities {:textDocumentSync 1
                  :diagnosticProvider {:interFileDependencies false
                                       :workspaceDiagnostics false}
                  :completionProvider {:triggerCharacters ["/" "(" "-" "*"]
                                       :resolveProvider true}
                  :signatureHelpProvider {:triggerCharacters [" "]}
                  :hoverProvider true}
   :serverInfo {:name "simple-janet-lsp"
                :version "0.1.0"}})

(defn main [&]
  (def args
    (argparse/argparse
      "A simple janet language server"
      "debug-server" {:kind :flag
                      :short "d"
                      :help "Starts the debug RPC server"}
      "log" {:kind :flag
             :short "l"
             :help "Enables log printing to the debug RPC server"}))

  (when (get args "debug-server")
    (log/start-debug-server)
    (break))

  (when (get args "log")
    (log/enable-logging)
    (log/info "Server started"))

  (ev/spawn-thread (forever (ev/give *msgs* [:lsp-message (get-message)])))

  (loop [message :iterate (ev/take *msgs*)]
    (match message
      [:lsp-message msg]
      (let [id (get msg "id")]
        (case (get msg "method")
          "initialize" (write-response id initialize-result)
          "textDocument/didOpen" (did-open msg)
          "textDocument/didChange" (did-change msg)
          "textDocument/completion" (completion id msg)
          "textDocument/hover" (hover id msg)
          "textDocument/signatureHelp" (signature-help id msg)
          "completionItem/resolve" (completion-resolve id msg)
          "shutdown" (write-response id :null)
          "exit" (os/exit 0)
          (log/info (string "Got uncaught method: " (string/format "%P" msg)))))
      [:update-diagnostics msg] (update-diagnostics msg)
      msg (log/info "Go uncaught message: " (string/format "%P" msg)))))
