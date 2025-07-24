(import spork/argparse)
(import spork/json)

(import ./eval)
(import ./log)
(import ./parser)

(def *files* @{})
(def *msgs* (ev/thread-chan 1000))

(defn get-message []
  (def content (file/read stdin :line))
  (unless (buffer? content) (break))
  (def content-length
    (-> content
        (string/trim "Content-Length: \r\n")
        (scan-number)))
  (file/read stdin :line)
  (json/decode (file/read stdin content-length)))

(defn create-result [message]
  (def new-lines (if (= (os/which) :windows) "\n\n" "\r\n\r\n"))
  (string "Content-Length: " (length message) new-lines message))

(defn write-response [id result]
  (let [message (json/encode {:jsonrpc "2.0" :id id :result result})
        result (create-result message)]
    # (log/info "Sending response: " result)
    (prin result)
    (flush)))

(defn write-notification [method params]
  (let [message (json/encode {:jsonrpc "2.0" :method method :params params})
        result (create-result message)]
    # (log/info "Sending notification: " result)
    (prin result)
    (flush)))

(defn new-diagnostic [{:location [line char] :message message} text]
  (def line (dec line))
  (def start @{:line line :character (dec char)})
  (def end @{:line line :character char})
  (def error-line (get (string/split "\n" text) line))

  (when-let [word (eval/word-at error-line char)]
    (put end :character (+ char (length word))))

  (when (= (string/slice error-line (dec char) char) "(")
    (update start :character inc))

  [{:range {:start start :end end} :message message :severity 1}])

(defn update-files [uri text]
  (let [[err env] (eval/eval-file uri text)
        diagnostic (if err (new-diagnostic err text) [])]
    (put *files* uri @{:text text :env env})
    (write-notification "textDocument/publishDiagnostics"
                        {:uri uri :diagnostics diagnostic})))

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
  (def char (string/slice line (dec pos) pos))
  (def word (eval/word-at line pos))

  (when-let [form (eval/form-at line pos)]
    (when (or (= form "import") (= form "use"))
      (write-response id :null)
      (break)))

  (def break-char-pos
    (if word (max ;(mapcat |(string/find-all $ word) ["/" "-" "*"]))))

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
      (filter (fn [{:label l}] (has-prefix? l)) local-syms)
      local-syms))

  (def global-items (map to-item global-bindings))
  (def local-items (map (fn [{:label l :kind k}] (to-item l k)) local-bindings))

  (def all-items @[])
  (def seen @{})

  (loop [items :in [local-items global-items]
         item :in items
         :let [label (get item :label)]
         :unless (get seen label)]
    (put seen label true)
    (array/push all-items item))

  (write-response id {:isIncomplete true :items all-items}))

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
  (def word (eval/word-at line (inc pos)))
  (def doc (get-doc word env))

  (def result
    (if doc
      {:contents {:kind "markdown" :value doc}}
      :null))

  (write-response id result))

(defn signature-help [id msg]
  (def file (get *files* (get-in msg ["params" "textDocument" "uri"])))
  (def env (get file :env))
  (def text (get file :text))

  (def {"character" pos "line" line} (get-in msg ["params" "position"]))

  (def line (get (string/split "\n" text) line))
  (def word (eval/form-at line pos))
  (def doc (get-doc word env))

  (def result
    (if doc
      {:signatures [{:label word :documentation {:kind "markdown" :value doc}}]}
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
    # (log/pp message)
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
