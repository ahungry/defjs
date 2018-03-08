;; defjs - Common Lisp REPL fun in Javascript
;; Copyright (C) 2014 Matthew Carter
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; defjs.lisp

(in-package #:defjs)

;;; "defjs" goes here. Hacks and glory await!

(defconstant +toot-port+ 39998)
(defparameter *websocket-port* 39999)
(defparameter *clients* (make-hash-table :test 'equal))
(defparameter *websocket-url* "ws://localhost:39999")
(defparameter *toot* nil)
(defparameter *js* (make-hash-table :test 'equal))

(setf *js-string-delimiter* #\")
(setf *ps-print-pretty* t)

(defmacro defjs (name args &rest fn)
  "Set up a parenscript block for later rendering"
  `(progn
     (setf (gethash (string ',name) defjs:*js*)
           (ps (defun ,name ,args ,@fn)))
     (broadcast "eval" (list
                        (cons "code" (format nil "window.~a=~a"
                                             (string-downcase (string ',name))
                                             (gethash (string ',name) defjs:*js*)))))))

(defmacro dojs (&rest fn)
  `(broadcast "eval" (list
                      (cons "code" (ps ,@fn)))))

(defun start-websocket-server (&key (port *websocket-port*)
                                 (server-name "localhost"))
  "Begin the websocket server in a thread, assuming thread does not exist"
  (setf *websocket-port* port)
  (setf *websocket-url* (format nil "ws://~a:~a" server-name port))
  (unless (member (format nil "DEFJS Websocket Server (port: ~a)" port)
                  (mapcar #'thread-name
                          (all-threads)) :test #'string=)
    (make-thread (λ α → (run-server port))
                 :name (format nil "DEFJS Websocket Server (port: ~a)" port))))

(defclass ws-controller (ws-resource)
  ())

(defmethod resource-client-connected ((res ws-controller) client)
  "Save our client in connection pool"
  (setf (gethash (client-host client) *clients*) client)
  (format t "Got connection from ~s: ~s~%"
          (client-host client)
          (client-port client))
  (write-to-client-text client "{\"event\":\"Welcome\"}")
  t)

(defmethod resource-client-disconnected ((resource ws-controller) client)
  "TODO Put some message here"
  (format t "Client disconnected from resource ~s: ~s~%" resource client)
  t)

(defun ws-clients ()
  "Get the list of the clients"
  (loop for v being the hash-values in *clients*
     collect v))

(defmethod resource-received-text ((res ws-controller) client message)
  "This should more or less be a drop in replacement for the existing
way things are piped into the general socket listener and spit back
out at the client."
  (let* ((json (cl-json:decode-json-from-string message))
         (event (format nil "pseudo::~a" (cdr (assoc :event json))))
         (data-in (cdr (assoc :data json))))
    (funcall (read-from-string event) data-in client)))

(defmethod resource-received-binary ((res ws-controller) client message)
  (format t "Client ~s sent binary frame ~s~%" client message)
  (write-to-client-binary client message))

(register-global-resource "/defjs-controller/"
                          (make-instance 'ws-controller)
                          (origin-prefix
                           "http://127.0.0.1"
                           "http://defjs.ahungry.com"
                           "http://ahungry.com"
                           "http://lisp.ahungry.com"
                           "http://pro"
                           "http://localhost"))

(make-thread (λ α → (run-resource-listener
                     (find-global-resource "/defjs-controller/")))
             :name "Resource listener for /defjs-controller/")

(ƒ emit-broadcast-data-formatter
   (and (listp α) (eq :obj (car α))) → (cdr α)
   (and (listp α)
        (listp (car α))
        (eq :obj (car (car α)))) → (mapcar #'cdr α)
        (stringp α) → (decode-json-from-string α)
        α → α)

(defun broadcast (event data)
  "Receive a list of data items and send to all connected clients"
  (let ((json-string (cl-json:encode-json-to-string
                      (list (cons :event event)
                            (cons :data (emit-broadcast-data-formatter data))))))
    (write-to-clients-text (ws-clients) json-string)))

(defun emit (event data client)
  "Send data out to just one individual client"
  (let ((json-string (cl-json:encode-json-to-string
                      (list (cons :event event)
                            (cons :data (emit-broadcast-data-formatter data))))))
    (when client
      (write-to-client-text client json-string))
    json-string))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro+ps class-extends (child parent)
    `(progn
       (defun ,child (props state)
         (chain this super (call this props state))
         )
       (setf (@ ,child prototype)
             (chain -Object (create (@ ,parent prototype))))
       (setf (@ ,child prototype constructor) ,child)
       (setf (@ ,child prototype super) ,parent)
       ))

  (defmacro+ps c (&rest data)
    `(chain console (log ,@data))))

(defun class-extends-fn (child parent)
  (ps
    (defun child ()
      (chain this super (call this)))
    (setf (@ child prototype)
          (chain -Object (create (@ parent prototype))))
    (setf (@ child prototype constructor)
          child)
    (setf (@ child prototype super)
          parent)))

(defun react-js ()
  (ps
    (defvar rc (@ -react -component))
    (class-extends Greeting rc)
    (setf (@ Greeting prototype state)
          (create click-count 0))
    (setf (@ Greeting prototype render)
          (lambda ()
            (let* ((self this)
                  (click-count (1+ (@ self state click-count))))
              (chain -react
                     (create-element
                      "div"             ; tag
                      (create
                       class-name "greeting"
                       on-click (lambda ()
                                  (chain self (set-state (create click-count click-count)))
                                  ))          ; properties
                      (+ "Hello " (@ this props name) " - clicked - "
                         click-count))))))    ; inner content

    (chain -react-d-o-m
           (render
            (chain -react
                   (create-element
                    Greeting
                    (create
                     name "User"
                     class-name "root"
                     on-click (lambda () (alert 3)))
                    nil))
            (chain document (get-element-by-id "react-stuff"))
            ))
    ))

(defun page-js ()
  (ps
    ;;(defvar *socket-url* (lisp *node-socket-url*))
    (defvar *defjs-websocket-url* (+ (lisp *websocket-url*) "/defjs-controller/"))
    (defvar defjs-socket-clws)
    (defvar *defjs-sock-responses* [])

    (defun defjs-sock-on (event fn)
      "Bind a function to the sock-responses"
      (setf (aref *defjs-sock-responses* event) fn))

    (defun defjs-sock-emit (event data)
      "Safely send data into the websocket, relink if fail"
      (if (and defjs-socket-clws
               (eq (@ defjs-socket-clws ready-state) 1))
          (progn
            (let ((json (chain -j-s-o-n (stringify
                                         (create "event" event
                                                 "data" data)))))
              (c event json)
              (chain defjs-socket-clws (send json))))
          (progn
            (defjs-socket-start)
            (set-timeout (λλ α → (defjs-sock-emit event data)) 1000))))

    (defun defjs-socket-start ()
      ;;(setf socket (chain io (connect *socket-url*)))
      (setf defjs-socket-clws (new (-Web-Socket *defjs-websocket-url*)))
      ;; Response mappings on open
      (setf (@ defjs-socket-clws onopen)
            (lambda (event)
              (setf (@ defjs-socket-clws onmessage)
                    (λλ α → (progn
                              (let* ((data (chain -j-s-o-n (parse (@ α data))))
                                     (event (aref *defjs-sock-responses* (@ data event))))
                                (when event (chain event (call nil (@ data data))))))))))

      ;; Essentially a Firefox fix for not gracefully closing sockets
      (setf (@ window onbeforeunload)
            (λλ α → (progn (setf (@ defjs-socket-clws onclose)
                                 (lambda () nil))
                           (chain defjs-socket-clws (close))))))

    (defjs-sock-on "eval"
        (λλ α → (progn
                  (c "Got something from socket")
                  (c α)
                  (c (eval (@ α code))))))

    (set-timeout (λλ α → (defjs-socket-start)) 1000)

    nil))

(defmacro loader ()
  `(maphash (lambda (k v)
              (declare (ignore k))
              (str v)) defjs:*js*))

(defun get-loader ()
  "Pull out the cached JS content"
  (with-html-output-to-string (s)
    (:script (loader))))

(defun page-index ()
  "Render the index page"
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "defjs")
      ;; (:script
      ;;  :src "https://cdnjs.cloudflare.com/ajax/libs/react/0.13.3/react.js")
      )
     (:body
      (:h1 "Welcome to defjs")
      (:p "To get started, in SLIME try out some fun things like:")
      (:pre "(defjs:defjs hello-name (name) (alert (+ \"Hello \" name)))")
      (:pre "(defjs:dojs (hello-name \"Matt\"))")
      (:div :id "react-stuff")
      (:p "At that point, you should see some results pop up here in the browser.  To
find out how to integrate with your own page, "
          (:a :href "https://github.com/ahungry/defjs/" "read the full usage guide on Github"))
      (:p :style "font-size:8px;" "&copy; http://ahungry.com 2014 Licensed with GPLv3")
      (:script :src "https://unpkg.com/react@16/umd/react.development.js"
               :crossorigin)
      (:script :src "https://unpkg.com/react-dom@16/umd/react-dom.development.js"
               :crossorigin)
      (:script :src "/defjs.js")
      (:script :src "/reactjs.js")
      (:script (loader))))))

(defun main ()
  "Start webserver, websocket server"
  (unless *toot*
    ;; Start the web server
    (setf *toot* (make-instance 'hunchentoot:easy-acceptor
                                :port +toot-port+))
    ;;:message-log-destination nil
    ;;:access-log-destination nil))
    (setf (hunchentoot:acceptor-document-root *toot*) #P"www/")
    (hunchentoot:start *toot*))

  (hunchentoot:define-easy-handler (uri-home :uri "/") ()
    (setf (hunchentoot:content-type*) "text/html")
    (page-index))

  (hunchentoot:define-easy-handler (uri-js :uri "/defjs.js") ()
    (setf (hunchentoot:content-type*) "text/javascript")
    (page-js))

  (hunchentoot:define-easy-handler (uri-reactjs :uri "/reactjs.js") ()
    (setf (hunchentoot:content-type*) "text/javascript")
    (react-js))

  nil)
