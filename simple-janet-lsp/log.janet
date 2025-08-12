(import spork/rpc)

(import ./utils)

(var- *log* nil)

(defn enable-logging []
  (set *log* (try (rpc/client) ([_] nil))))

(defn start-debug-server []
  (rpc/server {:print (fn [self x] (print x))}))

(defn- header [level]
  (def {:hours h :minutes m :seconds s} (os/date))
  (string/format "%02d:%02d:%02d [%s]" h m s level))

(defn info [& strs]
  (when *log*
    (:print *log* (string (header "INFO") " " ;strs))))

(defn- _pp [sym x]
  (when *log*
    (:print *log* (string/format "%s %s = %P" (header "INFO") sym x))))

(defmacro pp [x]
  ~(,_pp ,(utils/tuple->string x) ,x))
