; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Truledger server web interface
;;;
;;; (let ((server (make-server "/users/billstclair/testserverdb" "passphrase")))
;;;   (truledger-web-server server))
;;;

(in-package :truledger)

(defvar *ports-to-servers*
  (make-hash-table :test 'eql))

(defvar *ports-to-acceptors*
  (make-hash-table :test 'eql))

(defvar *acceptors-to-ports*
  (make-hash-table :test 'eq))

(defvar *ports-to-www-dirs*
  (make-hash-table :test 'eql))

(defvar *port-forwarding-hash*
  (make-hash-table :test 'eql))

(defvar *port-pathname-defaults*
  (make-hash-table :test 'eql))

(defun port-server (port)
  (gethash port *ports-to-servers*))

(defun (setf port-server) (server port)
  (if server
      (setf (gethash port *ports-to-servers*) server)
      (remhash port *ports-to-servers*))
  (let ((forwarded-from (port-forwarded-from port)))
    (when forwarded-from
      (setf (gethash forwarded-from *ports-to-servers*) server)))
  server)

(defun map-port-servers (f)
  "Call f with two args for each port server: port and server"
  (maphash f *ports-to-servers*))

(defmacro do-port-servers ((port server) &body body)
  `(map-port-servers
    (lambda (,port ,server) ,@body)))

(defun port-acceptor (port)
  (gethash port *ports-to-acceptors*))

(defun (setf port-acceptor) (acceptor port)
  (cond (acceptor
         (setf (gethash port *ports-to-acceptors*) acceptor
               (gethash acceptor *acceptors-to-ports*) port))
        (t (let ((acceptor (port-acceptor port)))
             (remhash port *ports-to-acceptors*)
             (when acceptor
               (remhash acceptor *acceptors-to-ports*)))))
  acceptor)

(defun acceptor-port (acceptor)
  (gethash acceptor *acceptors-to-ports*))

(defun port-www-dir (port)
  (gethash port *ports-to-www-dirs*))

(defun (setf port-www-dir) (www-dir port)
  (if www-dir
      (setf (gethash port *ports-to-www-dirs*) www-dir)
      (remhash port *ports-to-www-dirs*))
  www-dir)

(defun port-pathname-defaults (port)
  (gethash port *port-pathname-defaults*))

(defun (setf port-pathname-defaults) (defaults port)
  (if defaults
      (setf (gethash port *port-pathname-defaults*) (pathname defaults))
      (remhash port *port-pathname-defaults*))
  defaults)

(defun port-forwarded-to (port)
  (cdr (gethash port *port-forwarding-hash*)))

(defun port-forwarded-from (port)
  (car (gethash port *port-forwarding-hash*)))

(defun (setf port-forwarded-to) (forwarded-to port)
  (cond ((null forwarded-to)
         (let ((forwarded-to (port-forwarded-to port)))
           (when forwarded-to (remhash forwarded-to *port-forwarding-hash*)))
         (remhash port *port-forwarding-hash*))
        (t (let ((cell (or (gethash forwarded-to *port-forwarding-hash*)
                           (setf (gethash forwarded-to *port-forwarding-hash*)
                                 (cons nil nil)))))
             (setf (car cell) port))
           (let ((cell (or (gethash port *port-forwarding-hash*)
                           (setf (gethash port *port-forwarding-hash*)
                                 (cons nil nil)))))
             (setf (cdr cell) forwarded-to)))))

(defun web-server-active-p ()
  (> (hash-table-count *ports-to-acceptors*) 0))

(defmacro bind-parameters ((&rest params) &body body)
  `(let ,(mapcar (lambda (param)
                   `(,param (hunchentoot:parameter
                             ,(string-downcase (string param)))))
                 params)
     ,@body))

(defvar *debug-stream* t)

(defmacro with-debug-stream ((&optional (only-if-p t)) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-debug-stream #',thunk ,only-if-p))))

(defun call-with-debug-stream (thunk only-if-p)
  (let ((*debug-stream* (and only-if-p (make-string-output-stream))))
    (funcall thunk)))

(defun get-debug-stream-string ()
  (and *debug-stream*
       (get-output-stream-string *debug-stream*)))

(defun enable-debug-stream (&optional (enable t))
  (when (eq *debug-stream* t)
    (error "Not inside with-debug-stream"))
  (when (xor enable *debug-stream*)
    (setq *debug-stream* (and enable (make-string-output-stream))))
  enable)

(defun debug-stream-p ()
  (and *debug-stream* (not (eq *debug-stream* t))))

(defun debugmsg (format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (when (and *debug-stream* (stringp format-string))
    (apply #'format *debug-stream* format-string format-args)))

(defun do-truledger-web-server ()
  (let* ((acceptor hunchentoot:*acceptor*)
         (port (hunchentoot:acceptor-port acceptor))
         (server (port-server port))
         (*default-pathname-defaults* (or (port-pathname-defaults port)
                                          *default-pathname-defaults*)))
    (bind-parameters (msg debug debugmsgs)
      (setf (hunchentoot:content-type*) "text/html")
      (cond ((and msg server)
             (with-server-crypto-session-context (msg)
               (let* ((debugstr nil)
                      (res (with-debug-stream (debugmsgs)
                             (unwind-protect
                                  (process server msg)
                               (setq debugstr (get-debug-stream-string))))))
                 (cond (debug
                        (setq res
                              (format
                               nil
                               "msg: <pre>~a</pre>~%response: <pre>~a</pre>~%"
                               msg res))
                        (unless (blankp debugstr)
                          (setq res (format nil "~a<pre>~a</pre>" res debugstr)))
                        res)
                       ((blankp debugstr) res)
                       (t (format nil "<<~a>>~%~a" debugstr res))))))
            ((not server)
             (if msg
                 (with-server-crypto-session-context (msg)
                   (declare (ignore msg))
                   (let ((downmsg (db-get (make-fsdb (server-db-dir)) $SHUTDOWNMSG)))
                     (or downmsg "Server is down")))
                 (redirect "/client/")))
            (t (do-static-file))))))

(defvar *last-uri* nil)

(defun do-truledger-web-client ()
  (let* ((port (hunchentoot:acceptor-port hunchentoot:*acceptor*))
         (*default-pathname-defaults* (or (port-pathname-defaults port)
                                          *default-pathname-defaults*)))
    (truledger-client-web:web-server)))
  
(defun do-loom-web-client ()
  (let* ((port (hunchentoot:acceptor-port hunchentoot:*acceptor*))
         (*default-pathname-defaults* (or (port-pathname-defaults port)
                                          *default-pathname-defaults*)))
    (truledger-client-web:loom-web-server)))
  
;; Maybe this should be per-server, but it's rare to run more than
;; one in a single lisp image.
(defvar *url-prefix*
  (asdf:getenv "TRULEDGER_URL_PREFIX"))

(defun prepend-url-prefix (path)
  (if *url-prefix*
      (strcat *url-prefix* path)
      path))

(defun remove-url-prefix (path)
  (if *url-prefix*
      (and (eql 0 (search *url-prefix* path :test #'string=))
           (subseq path (length *url-prefix*)))
      path))

(defun redirect (path)
  (hunchentoot:redirect (prepend-url-prefix path)))

(defvar *web-script-handlers*
  (make-hash-table :test 'equal))

;; note that we expect the prefixed script-name when getting,
;; but we append the prefix when setting.
(defun get-web-script-handler (port script-name)
  (gethash (list port script-name)
           *web-script-handlers*))

(defun (setf get-web-script-handler) (handler port script-name)
  (setf (gethash (list port (prepend-url-prefix script-name))
                 *web-script-handlers*)
        handler))

(defun remove-web-script-handlers (port)
  (loop
     for key being the hash-key of *web-script-handlers*
     do
       (when (eql port (car key))
         (remhash key *web-script-handlers*))))

(hunchentoot:define-easy-handler (truledger-server :uri 'identity) ()
  (let* ((script (hunchentoot:script-name*))
         (acceptor hunchentoot:*acceptor*)
         (port (hunchentoot:acceptor-port acceptor))
         (handler (get-web-script-handler port script)))
    (cond (handler (funcall handler))
          ((search "/.." script)
           (abort-request))
          (t (do-static-file)))))

(defun abort-request ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (hunchentoot:abort-request-handler))

(defparameter *little-lambda-base64*
"iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+g
 vaeTAAAACXBIWXMAAAsSAAALEgHS3X78AAAAB3RJTUUH1AITDy0QS9ZSPgAAAKxJ
 REFUeNrNk8ENwyAQBNdR2qAPUwj0AfRx7sMUAn1AH5uXUeQQ28SfzOs4pBG3JyaS
 xA0euElXEEJACOE3wbIsrY4xnhu4Q0RYSiFJeu95Bo4u32XDglIKAXBd10PB1y1s
 WeScDyN49po5Z4gIRGQ8xG12pRRJ0hgzPkKMEc45AIBSCrVWAIC19toatdbtnFIi
 AGqtmVL6eEE3A2NMq+d5hve+1Xum//xMI7wAbKVIbzySjiIAAAAASUVORK5CYII=")

(defparameter *truledger-logo-base64*
"R0lGODlhMgAxAPcAAAAAAA0AABwAACoAACsVADgVADkAAEccAEgnDEolAFQsCVkm
 AFxZUl04DmRiYGYzAGZCFmZNGmtjUmtnXW1DFm5fR3Bwa3NFD3RJEXVbOHY7AHZO
 G3hID3lNEnlWKHl1anpoS3t5c3xyWn1RFH1SGIFUFoJWG4KAdINcI4RoQYVZFoVi
 MoZZGod8Y4hpO4pcG4tdIIxiLI1hHY1iI42DaY6HdY6JfI9zTJJjHZJkIZJwOpKO
 h5RqKpVpI5ZaHpZ8UZlmGplvNppsI5psKZqKbJuFYpxxK5x0M52SeJ5wJp6Wg56a
 jZ93OKJ0K6KMYqKRbKKahKNzJqSWdaSafqV5M6Z3MKZ4Lah4J6mUaqmbdqp7LKqe
 gat9Mat/OayDPayii62BNK2IP62KSq2UXa6FQK+DLrGBLLKFOLKni7OEM7Oaa7Oh
 dLOjfrSKO7SKQLSvnbWJNbaRSreQQ7iGLLiaYLmLLLmvlbuLNbuNOruOQbuVTbug
 aruwhryTQ72RNr2RPL2jc72pfL6ZRr6aScGlbcKSNcKUPMKVQsOrdcOuf8SaQ8Sb
 S8SeU8SwisWYPMWgS8ajX8ejV8meSsmnaMqbPcqcQ8qoWMqqZMujU8yiS8y1gsy5
 gs2hRc2pT86oU9CeN9CmUdGiRNKjPdKlStKyatK3c9OtW9O6fdSrUtSwY9WpRdWq
 StWyW9XDldawVdaxTtfEidmnSdmsRNmtUdm2YtqsTNu0Wtu6aNyyS9yyU9y7Y9zC
 f925XN6yRd65VN7AZN/Bat/EcOG1U+G2WuHGe+K1S+K8YuO7W+S5TOS6U+TDVuTD
 a+TIbOTJYuXCZOXDW+a/bObPiei9VOm+W+rBXOrFaOvCVOvLY+vLbuzEYuzLXu7S
 be7SfO/JVe/ZiPDGVvDGY/DHW/HKXPLKZPPMavPTcvTRa/XaavXbcvXidfXiefbS
 ZPbaevjSXPjUbPjfgvnVcfnaY/vbdPvic/zdevzje/zqfPzqif3jbP3kgf3nkP3s
 gf7pdf7yhP73igAAACH5BAkAAP8AIf/8SUNDUkdCRzEwMTIAAAPcYXBwbAIAAABt
 bnRyUkdCIFhZWiAH2QABABoACAAlAB5hY3NwQVBQTAAAAAAAAAAAAAAAAAAAAAAA
 AAAAAAAAAAAA9tYAAQAAAADTLWFwcGzTB5qzjwU0VUwr1VfXVASWAAAAAAAAAAAA
 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
 MIIRZjCDY1f72x0JCAA7")

(defparameter *truledger-logo-png-base64*
  "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")

(defparameter *site-icon-base64*
"AAABAAEAPDsAAAEAIABQOQAAFgAAACgAAAA8AAAAdgAAAAEAIAAAAAAAAAAAAAAA
 AAAAAAAAAAAAAAAAAAD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD///8A////AP///wD///8A////AP///wD///8A////AP//
 /wD///8A////AP///wD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD///8A////AP///gD5+PgA5ejq/9DY3//P2uT/sMHN/67C0v+uwM//scDN/87X
 4f/T2N7/6erszPv5+AD///8A////AP///wD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD///8A////AP/9
 /ADj5+r/scPS/4iwyv9rp8n/VaDH/1au1P9ftNz/Va7Y/1e03f9QsNr/SKjV/1Gr
 1f9VqtT/VJrD/2GXuP+OrsX/vcjU/+zs7Wb///8A////AP///wD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A////AP///wDw8PEAvtDe/4O2
 0/9ZrNP/VLbf/1y/5/9dvOL/aMPn/2XC5v9hu+L/Y8Ln/2TB4/9du+D/W7fe/1+7
 4P9jvuL/Xrbe/1S13v9KrNn/Qp3K/1KVvv+CpL7/xMzV//n49wD///8A///+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP///wD///8A8fHzALLK2/9rsNP/UrXf/1q+
 5v9ZvOP/WLrg/0uv2f9KrNX/RbDa/0qy3P9PuOL/Tr7n/0++6P9Swer/VcPs/1TB
 6v9Vwuv/WMLr/1u/5/9ZueH/XLzh/1S54/9Eqdf/P5bF/2GPr//By9X///78AP//
 /wD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A////APv5+QDB1OH/a7HV/0623/9WvOT/UbTc/0mq
 0v88mcb/Lo27/yyIuP8yj73/M5XD/zqezP9JrNj/TLXf/0+65P9Vv+j/VsDp/1jE
 7P9ax+//Yc31/2TO9f9lzPT/Ycrw/17C6v9avuf/Wbzj/0uv3v87lMT/f6jH/9/l
 6/////8A////AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD///8A5+ru/4C10/9Sst3/UrTf/0yu1/87mMP/J4Ox/yB4
 p/8gdaT/JHys/yl/rf8uibf/Loq3/zmZxv9DptH/PZrH/0Wq1v9Go8//ULPe/2DI
 8P9lzPL/Z8/1/2bN8/9sz/X/bNL3/3DT+P9s1Pn/YMrw/1O74/9Vt+D/Sa3b/0iT
 wP+rwNH//Pv7AP///wD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A////AP///wDM2eT/bbfd/1bA6f9Qsdr/Q5fD/yN8q/8ZbZz/HHCg/xhy
 of8ecJ7/I3em/yl/rf8uh7X/LYOy/z2bx/89mMX/R5/L/1Cr1f84k8P/Tq3Z/2HC
 7P9jx+//VK/Z/2XI7/9fwun/Z8nx/2zR9/9v1fr/c9f8/3DZ/P9lz/L/Wbzj/0yt
 2f84lsf/eqTC/+3u8UT///8A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD///8A//78AL3T4v9it9z/Vrzl/0em0P8qgK7/Gm6e/xdrmf8ZbJr/GWua/xds
 nP8sgK7/MISz/zOQvv81jr3/OYy6/0+fyf85lcT/TZ3J/2S54/87j73/Wa/Z/2DA
 6v9cu+T/RpjE/1u44f9WrNX/XLbe/1ey3P9jw+v/ctT4/3XY/P923P3/cd38/1bA
 5/9LrNf/QqPT/2ujxv/n6+//////AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7/
 /wD//f0Aq8jZ/1m64v9TtuH/QZ3H/xtzo/8Xapn/GWyb/xVikv8MVoT/LICu/x9z
 pP8nf6//UJ3I/z2Ww/84ksD/RJrI/0aezP9LqdX/UrHd/1i34v9fwOn/ZMPs/2jD
 7P9kwuv/Wbbh/1634v9Wr9n/VajT/1mjyv9pwuf/VrTd/2LG7f922vz/fd///3rk
 //9hzPD/VrLb/0aq2f9ZnMP/3uTq/////wD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP/+
 /gCuytv/Xbzg/1K03f89krz/E2qZ/xRllf8WZZT/EmWU/yh5p/8abZ3/M4Kw/zSE
 sv8zga7/NYa1/zSJuP9Al8X/R5/N/0Ggzf9Io8//SqXR/0up1f9RsNz/Xrzm/2PB
 6v9ryfH/asjx/2rI8f9ryvD/acbt/2nB5/9TqdP/Yrvh/2HC6/901vn/feD//4Lk
 //9+5f//aNP0/1m24P9DotD/UJjA/+Dk6f////8A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A////AMfa
 6P9nweb/TqzX/zuQu/8NZpb/DmWV/xZnlv8ncqD/KHel/x9vn/9Hkbv/MIKw/yl9
 qv8pe6r/MYOy/zCEtP8sfq3/Lnup/yRwoP8qd6X/QY+8/0CKtv9SpMz/N4Wz/0ye
 x/9LoMz/Uq7a/2TC7P9wz/X/ctL3/3PS9/9ju+P/Zbnh/3DP9P900/f/asjw/23S
 9v995P//hez//2/Y9f9Xs9z/SqvX/2Gbv//t7/FE////AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD///8A3uju/22/
 4f9Kp9T/Nou5/xJllf8SZpX/EWKR/xVfjv8nd6f/OoKu/zaArv8sfav/I3in/yV6
 qf8mean/Mnup/0ybw/9Ln8v/Wq7X/1613v9yyu//gNb0/1685f9otdn/a7fZ/1qq
 0f81hrT/VarU/1qz3f9Hnsv/Xrvk/3HR9/9z0fb/cM70/3HO8/9nxOz/Z7PZ/1Wu
 2f913v3/kPD//47u//9w2fX/Vbnh/z6h0P9+p8T//f38AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP///wD29vgAicfi/1K2
 3f88kr//FGiY/xBllv8QYpL/ElyL/yyBsP8jdqf/MoGu/yh6qP8id6b/HG6d/x1r
 mf85irj/abPX/02Zwf9IoM//WrPd/2e/5/9kut7/acPq/3LJ7f9ntdj/YLTb/1Or
 1f9juuH/ccXo/2XB5/9Np9P/bMDk/1Wv2v9mw+v/dNL3/3HO9P9oxu3/UanV/2G9
 5f9syOv/fOD5/5Hw//+M7///Z9Ty/1Wx2v9EoM//vNDg/////wD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP///wC11ef/W7ne/z6b
 x/8bapn/DV+Q/w9fj/8OY5T/EmKT/0iaxf8pf67/IHim/yN6qf8aZpT/JWuZ/0CT
 vv9aq9T/brzf/zeCsf9Ck8D/Q5bE/0yi0f9kuOD/a8Xs/3PM8f9svuL/bL3h/27D
 5/+C0/D/fdPy/1eq0/85jbz/TqPO/2XB6f9bs93/WbPc/2vM8v9uzfP/asjv/2TA
 5/9swOP/acjs/3/n//+I6///h+z//2DF5v9Hp9P/UJvF/+jt8O7///8A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A////AOLo7v91wd//Rp7I/yl+
 rP8RX5H/FV+Q/w1cjP8WZpb/Knam/y+Arv8mean/Jn2t/xhjk/8odKL/TKXV/1Ss
 2f9SptH/WK/Z/0mn0/9In8v/OYi0/1Km0v9NpNP/YLrl/2vG7v9ryO//d9Dz/3DL
 8P9xyOv/c8/y/zyNu/9UpM3/QZnH/2W/5v+B2fn/bsTp/0+t2P9PqNT/Xrzk/3LS
 9v9nvuP/ccPk/2G/5P985P7/ju3//37n/f9RtNz/NJfG/4mzzf////4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v79AJ/K4f9Xs9j/OIi2/xJn
 l/8RYZL/EVyN/wlaiv8qd6P/L4i1/yB5qP8geqr/GmaW/zSDs/9LqNb/TqjV/0ii
 0P9JptT/SaXR/0+q1v9GoND/SJ7K/0qWwf9fsdj/Uq/c/2C75P9lwer/cMrw/23H
 7/9syfD/b8z0/0yk0P97zur/acHn/1Omzv9/1fb/fdj8/33V9/9au+T/XLDZ/2/J
 7f+C3/r/dc3s/3HS8/9oze//g+r//47u//9x1vH/TqnU/0GcyP/T3+j/////AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD///8A4Ojv/3XB3/9Jnsn/InOj/w1c
 jv8NXo7/EFiJ/ydunv8xeqf/GnKg/yF7rP8eb57/N5C9/1Cu2/9Ws97/UbDc/128
 5P9RrNn/Uq7a/1av2v9Tp9H/OIWy/2ix1f9wxer/R6LQ/0+o1f9TrNb/Yrjf/1yx
 2/9lwOf/a8bs/2bA6v93xeT/gdn4/2a95/97z+//g9z6/4Lc+v9yz/T/Xbvk/37S
 7/+R6///gt76/2S94v9iweb/dN/6/5Hu//+P7///V7jd/0Ol0v+GttL//f39AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD///4Ar9bo/1yy2P8yh7X/C1qL/w1a
 i/8TYI//DmGS/zOArv8td6X/HXOj/xxxoP8rfq3/O5XE/zmQvv87lcT/WrPa/1Os
 1P9AlML/PZbD/zCCsf8ue6r/ZrTY/3HA4/9ZsNr/RqHP/0uk0f9ht+D/UKfR/z6S
 v/9ImcT/TKPP/2K95v9nwur/a8Xr/2jE7P93ze//d8rs/3TM8P90zPD/d8vt/33Z
 9/+Q6f//hN/9/2PA5/9wyev/feL9/47s//+U8P//cNTx/06t1v9DmcT/2+Pp////
 /wD+/v4A/v7+AP7+/gD+/v4A/v7+AP///wDx9PYAiMrj/06jzP8idaP/DFeJ/wxX
 h/8TX4//GmeV/xZomP8XbJz/Hnen/x91pf9Np8//Qp/L/yl+rv9Locz/abzg/z6e
 zP9IoMz/KX2s/z+Ou/9it+H/W7Xf/02n1f9Srdn/R57L/0ebyf9bt+P/SaXT/1Wt
 2f9Opc//XLLb/2a+5/9evOb/Ybrj/1yx3P94yer/ZcHo/2/K7/9yz/P/gdb0/4zj
 +v+U6///jef//4Ti//+C5P//iun//4/s//+T7v//jev+/1a02v8/nsz/mcDX////
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP///wDQ4ez/a7LU/zaNu/8UY5T/DVyO/xVe
 kP8RXY//FGSU/xhnlv8SZZL/FnCg/yR+r/8thrf/Loy7/zCNvf89nMr/N5PD/yV6
 qv83gq//XqzT/12x2/9HpNL/SabU/1Ow3P9Zsd3/WLLf/z+SwP9duuP/U7De/0ql
 1f9BlcX/YbPY/z2Ovf9Fns3/Q6HQ/0qp1f9Nq9f/VrTf/1y65P9vzPL/ieL//53u
 //+X6///j+f//4nm//+H6P//i+v//4rr//+S7f//k+///1/G6P9Kps//W6fL//P0
 9QD///8A/v7+AP7+/gD+/v4A/v7+AP/+/gCizuP/SKXO/zCEsv8JYpT/E2CR/xde
 j/8RX47/E2KQ/xhmlf8baZn/HGuc/yJ1pP8sgbP/Moy8/zOQv/8nfq3/LX+s/2e1
 1/9qu+D/SaXT/zaQv/87kLz/VKvW/1ex3v9Zst7/XLvm/0Wbyf9NptD/bs3x/1Sx
 3/9Pqtj/T6vZ/0mgz/9PqNX/UarW/zyXxv89nMv/SqjV/1q34v9zzvT/juX//5jr
 //+R6P//iuX//4jm//+M6f//j+3//4zt//+O7P//m/H//3LW9P9Kp9L/Q6LO/8vZ
 4f////8A/v7+AP7+/gD+/v4A///+APb2+AB9vtn/PZnF/yh8q/8TYZL/Cl6P/xhi
 kv8YYpL/DV6P/xZjk/8daZj/Hmuc/yRzo/8idqb/H2+e/x1yov84g63/W7Xd/2zD
 5v9IlMD/TZG7/0qUwP9quN3/Vq/a/1Wv3f9ZteD/Xrrl/06l0v9Ck77/X73l/2TC
 6/9Urtv/UKnX/0yj0v9EnMr/Q53M/0Gayf87lsX/QZ/N/1y34v920vj/i+T//5Ho
 //+P5f//iuX//4bl//+M6f//kO3//4/t//+N7P//j+3//3zj/f9JrNb/R6bT/6TJ
 3P////8A/v7+AP7+/gD+/v4A////AOPr8f9kuNn/OpXB/x5woP8KXo//K2qV/2uI
 mv92j6D/VICf/xtqmP8SZ5f/HmmZ/x1pmP8qdqL/Mom5/1u23f9Mo87/QZbF/1a0
 3/9vyu7/YLLb/1ez3v9Pq9r/VbDd/1q35P9cven/XsDr/1Wv3f84ibb/ZsDn/3LP
 8/9at+L/UKva/0ql1P89lsb/MYu8/yuEtv8tfa7/X7Tb/1y44/910PX/i+H//4nj
 //+L4///ieX//4nl//+P6v//jOv//4vq//+K6///i+z//4Hp//9WueL/RKXP/43D
 3f/+/f0A//7+AP7+/gD+/v4A////AMHU4P9WstX/OJPB/xJllP8LXI7/S3qd/3aT
 pf8qZ47/WHmN/2qCkv9GfaH/GW2d/xxsnP8icaH/J36t/0ik0P88l8T/SKHN/0+o
 1f9+2Pb/TrLg/0yv3v9Pr93/Ua3c/0Sk0v9Cnsz/Qp3K/0CXxv82jbz/d8zp/2W8
 4f9owuj/TabT/zuPvf87kb7/RZzH/06n0/9Ln83/fMfp/16+5/9yz/b/h9///4jh
 //+H4v//ieT//4jl//+I5v//iej//4bo//+L6f//h+j//4Dq//9fxuv/QZ7M/3O1
 1v/19fYA////AP7+/gD+/v4A////ALTT4v9LqtH/L4u4/w1djf8RXpD/G2WT/26R
 qv9ylK3/GmOU/zJqj/9jd4T/aIuh/0J+qf8bbJ3/Hm2e/zSJuP9Mr9v/SKjV/1K2
 4v+A3Pn/f8Lj/5O/1/+dwdT/k7PF/2qdvf9joMX/Xp3D/2ahxf9zqsr/YaTK/2Gi
 yv9grtb/PI27/1Omzv9Rr9v/VKjT/3vK6f9wweX/VrHe/1W55f9vzPP/gNv+/4nl
 //+Q5P//ldvz/5bd8/+L5f//iOf//4jn//+C5///fej//33p//9fyu//QpzJ/1+u
 0//p7/PM////AP7+/gD+/v4A////AJvC1v9KqND/Loaz/wdZif8VX4//J3aj/w5Y
 i/9Yf5v/gpyt/0F5oP8XaJn/VnSG/3eBhv9Vg6X/IHWm/yRyof+RvNL/psHO/5/E
 1/+Zxtz/u8fM/5m3yf+Fu9n/ncLV/7K4uv+0v8P/tsDD/7TN2P+mzNv/rcnV/6PH
 1/+kxtb/mLrM/5a/0f+Cyen/dMfq/1Wq1v8+ntD/S63b/1+96f9xz/f/htz//4ix
 wP+Nn6T/j7PC/4uyw/+gwMz/kt/4/3zk//954///dNv6/3Xm//9fzPH/PZvI/0ml
 zv/V4+r/////AP7+/gD+/v4A////AJvL4f9HqNH/LYKv/wpdjv8ncp//O5K+/w5Y
 iv8GW4z/PHCT/4mltf9vk6z/M3up/0l9nP9icXj/Rnye/zuUwv9joMD/iq3D/5i4
 yf+asr3/ore//6vAyP+twcr/qba8/56orP+apKn/o66z/5qttv+hxtf/osXV/6HH
 2P+av9P/rcXS/7K2uP+WnqH/mL7T/3+xzf+Jt87/jrrS/6HD0/+myNn/g6Ct/4aO
 kP+T0er/a7fd/3XF5f+NqLj/j8ve/2rY/P9gyvD/YbPY/3He+v9i0fX/PZvI/02p
 1f/Y6PL/////AP7+/gD+/v4A////AKDQ5f9Pq9H/LH+t/wRZif8pcZz/OY23/xJZ
 iv9FkLn/DmCS/0KTvf9hjKb/hp+w/1KMr/9Hgab/WGt3/1WIp/83jr//R5vJ/0un
 1f9jwuz/bszz/1iv2v9drtn/h7bP/7HGz/+Ooan/naWo/5Stuf+Qxt3/jcLZ/4q/
 1/9vqsr/e6rD/4CuxP+XxdT/kM/m/3m61/9urc//fbva/37I5/+PyOD/e4KE/6nM
 2v+ewdL/kbPD/46tuv+avcv/f937/1a44/9cud//arvd/3jX9v9h0fb/P5jF/0ej
 z//I2+f/////AP7+/gD+/v4A////AJ3N5P9IptD/LYCs/wFThf87d6D/Roqz/xBh
 k/9Ckrz/HGOT/1Sgyf8cb5//OX6l/36esf9/oLb/bpi0/2R7h/9zqcb/ZJOs/5mu
 t/+eu8n/nMba/5e+0v+RvdP/hrXM/8PO0v+ttrn/mJ2e/7rR2/+ZyuD/irTH/5HB
 1/+Nu9X/frLQ/4nK5v+e4PP/j9Hs/4fO5/9Zs9//erfV/3/I5v+I0O3/mK22/4DQ
 9f9kwez/UqLN/4HT8f9etd3/bNL2/2HL8/9Os+D/W7DZ/3Df//9bzPP/OJPC/0aj
 0P++097/////AP7+/gD+/v4A////AJbF3f9Kq9P/L4Ku/w1cjP8iaZf/Jm+d/w9f
 kP8XYZH/KXSg/yt6qP8ba5n/JXmn/yt7qv9Nhaj/f6K1/5mvu/+PlZf/jbLE/5Kh
 p/+bnZ7/nKGj/7G3uf+kqKn/pKal/7DDzP+nw9D/YYCN/4GHiv++xsn/o7a9/6Cl
 p/+eo6T/qq+x/56mqP+hqaz/qrGz/52prP+bsr3/nrW//5ebnf+Zp63/nKes/5nC
 1/9duuf/Vq3a/1244f9WpdD/Z8zx/1/J8f9Nuub/WrTc/2/a/P9Xw+3/N5G//02n
 0v/a6/X/////AP7+/gD+/v4A////AJG60f9LrNT/N4m0/xNejv8TYZD/EV6P/xZk
 lP8aZ5X/HWiX/x9plv8jb53/M4Gw/zJ/r/8ZbZz/J3im/3Ozzv+mtLr/kqiy/73M
 0/+ksbb/p62v/52lqf+0v8T/l6Wr/6Cssf+wzdv/qMzc/4Sbpv+jqKn/oq2y/7rF
 yv+IkJT/oq61/4uSlf+Pl5r/r7i8/6q4u/+VpKz/g4yP/6Krr/+kr7P/mJ2f/4SM
 jv+FyOr/Xb7q/1285/9ewu3/XMPt/1/C7f9dx+//X8zy/2XS+f9Puub/PpTB/0eh
 zP/T4er/////AP7+/gD+/v4A////AK7J2/9IqdT/PZC8/xNdjf8OZJL/FmiW/xZm
 lf8bapj/HmyZ/yFvnP8qeKT/M4Ky/zWJuP8qeKf/GmeV/1Kp0v9onbv/o7/N/6W6
 xP+hzOD/lsTa/6C6w/+sv8f/m9Xw/4rT8v+E2fv/m8jY/6Wyt/+Mmp//o7K4/6i9
 yP+ZzOL/X63V/3Gqyv+Iv9r/mNzy/57V5f9rr9T/gKzD/33B4f93vNv/jMjj/3ej
 t/93f4P/ib/Y/2jB7P9gv+r/XsDr/2HC7P9lx+//ZMnx/2LK8v9Krtv/OpG+/0yh
 yv/e5ev/////AP7+/gD+/v4A////AL3P3f9LrdT/PJbC/xBikv8TYZD/GGaV/x5p
 mP8jcJ//I2+d/yZyoP8yfKr/N4S0/zqOvf83jrz/J3ak/zJ+qv9it93/jbbH/6u2
 vP+tvMP/nbrI/6Pf8v+Sydv/pLO6/5bd9P+F3Pr/e9P2/4qlsv+kp6j/rbe7/6Gw
 t/+atML/Po29/0aayP9pzPf/kOX//4jN6f80l8v/T67b/1e24/9hwu3/asjx/2rF
 7/91r8r/gJii/3zJ6/9jwev/Yr7o/2bE7f9fwu3/Xsbv/1TE7v89n9H/OpLA/2av
 0f/x9PYA////AP7+/gD+/v4A////ANzl6/9Urtb/Q5/I/xxunf8SZZP/HWyY/yBt
 mv8odKH/IXKh/yt4pP8zfqz/OIa1/z2Rv/87ksH/Moe3/yh5qf8vga//Y6vO/3mu
 yf9suuH/fbbR/43P6v+G4v//oNXm/53W6/93z/T/Z8fw/3XJ7P+Wu8v/gcPh/4Gz
 zf9zsdP/NYO1/1is1f942fz/jN30/1Cexv9NqtX/U7Pg/1u85f9qyvH/a8rz/2jD
 7v9wzPX/eM71/2rD7P9kwev/YsPt/2fI8P9fxe3/WsTt/0y96f8zksP/QJvH/326
 1//8+voA////AP7+/gD+/v4A////APPz9QBkqc3/Ua7V/yd2pP8RYpH/FGeV/xlt
 m/8fc6L/Inaj/yd5pv8ueqj/MYS0/zuTwv88lcT/NY68/y6Ht/8yh7f/NIm4/zeS
 wf9Insr/WbTf/1uz3v9lv+f/acjw/2XG8P9mxO3/ZcTt/2LE7f9hw+7/Wbfl/1Ku
 3P9Kp9X/MYK0/2q+4f9zyuv/U6jS/1y23/9/zO3/Vrfk/27O9P972/z/dtT5/3bS
 +P9wzvX/bMfw/2bD7P9lwOr/ZsXu/1/F7f9ew+3/XMTt/0iz4P8zj73/SKbQ/5zE
 2v////4A/v7+AP7+/gD+/v4A/v7+AP///wCHs83/TK7X/zaKt/8UZpX/F22Z/xVu
 nP8kdqL/JHak/yV4pP8meaf/Moa1/z6Uw/88k8H/N5C+/zaPvf8/lMP/OpLA/zya
 x/9Dncr/RZzL/0uj0v9Tr9z/Xrzn/2vH7/9ryPH/acbv/2fD7f9hven/WLTi/1Gr
 2f9Lo9D/NIi3/2O74v9cueT/b870/2rD6v92zO//Y8jx/4Th/v+J5P//f9v9/3TP
 9v9wyfL/a8jw/2zJ8f9nxu//Zsjw/2HI8P9fxO3/XMPt/0Kj0f8xirf/TarR/8XY
 5P////8A/v7+AP7+/gD+/v4A/v7+AP///wCxxdT/TK/X/0CdyP8acZ//G2mW/xxu
 nP8fcJ7/HGya/yJ0ov8pean/NIa2/0OXx/89lcT/OpLA/zeRvv87k8L/QprI/0il
 0f9LrNn/Ua7a/1Sz3/9bvOb/b830/3TR9/9x0fj/cM71/2nG8P9fvej/V7He/0+r
 2f9LodD/OZC//1ew3/970/b/b8Hk/33P7v9z0vT/etr7/4/p//+D3f7/dNP4/3HO
 9f9vzPP/cc71/3DO9f9mx+//Zsjw/2LH7/9exe7/Ubzo/zmVxf8+kr7/bbXW/+3x
 9ET///8A/v7+AP7+/gD+/v4A/v7+AP///wDj5+v/VqjM/1Cw2f8qga//G2iW/xtt
 m/8gb53/HG2c/yRzof82i7r/NYe2/0OVxP9Cmsj/P5bF/zqSwP87lMP/R6LP/02v
 2v9YuOT/YcLr/2bI7/921vn/heD//4Hc/v951vz/d9X6/3PO9f9lw+z/V7bj/1Cw
 3f9JptX/Loe4/1y55P9vwuf/eMfk/5ji9/940/b/jeX//5Dq//951vr/bcry/3HN
 9f9wzvX/bs/2/1m86f9Wuuf/X8Xu/2HI8P9dw+3/S7Pg/ziSwf9FoMn/lMLa//7+
 /QD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v0AhbLM/1O13P88m8b/IXOj/x5w
 n/8Va5n/GnKg/zGFtf8+lMP/M4u6/z+VxP9BmMf/P5XE/0GayP9Dnsr/TarX/1S5
 5P9ixe3/bc71/3nY/P+G4v//i+P//4jh//+E3v//g97//3zY/P9pyPD/Vbbk/0Ok
 0/8yhbX/VKHH/2/L7P9RsN7/md3z/4fU8f+A2Pn/kun//4zl//910/j/a8fw/27K
 8f9ryPH/T6rY/1+02v9euOD/W8Ls/1rF7v9cw+7/QKfW/z+Xwv9br9X/1OTu////
 /wD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD///8AzNbg/1Sw1/9Vsdj/Moe1/xto
 lf8ufar/M4Kv/zqUwv8pfq3/Rp7J/0Gbyv9AmMb/SKHP/0mi0P9Jp9T/TLDb/1m8
 5/9szPT/d9X7/4Hd//+K4v//iuP//4vl//+L5f//huH//3/d//9uzfX/Ua/c/0eQ
 u/94uNr/Zbvh/yeBsv9dtNz/YKzU/5LZ8v+D4f//kOn//4Pg/v910vj/d9D1/3DO
 9P9XteD/aLbZ/27Q9P9ZvOj/a8jq/1fF7f9SueX/PZvK/0mjyv+AvNn/+fn6AP//
 /wD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/fz8AH2yz/9avOL/SaXP/yR2
 pf8nd6X/Q4y1/yx/rP8ve6v/Q4+8/z+Wxv9IodH/TafV/0mn1f9KqNX/UbLe/1zB
 6v9szfX/eNr9/4Hf//+J4///jeb//4zo//+N5///iOP//4Lg/v9t0Pb/R5/L/3G9
 3v9ktd7/LYKw/06Zwf9ww+f/YbLX/5Dc9P+P6P//heL//33a/P9z0vj/dtP3/2vL
 8v9bt+D/eNLx/1/D7f9MsN3/XbDV/1jC7P83nc7/QZrF/1mpzf/I3ur/////AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A////AM/a4/9dttz/Wrje/z6d
 yf8gdqf/YKLH/zSItv87kL//LoS0/0CTv/9Rrdv/T6/e/0ur2f9Ortv/Urbh/13C
 6/9pzvX/fNz+/4bk//+J5v//jej//47o//+N6P//i+X//4Th//9uzvX/VrHb/02o
 1f81hbP/VabM/2vD6P9Ts9//eMXl/3rP6/+M5P//fdn8/3jX+/901Pj/Z8jx/2LC
 6P912/z/bNT4/1zD7f9dsdn/ZcHm/0iy4P8yjr3/UKPL/4G41P/4+PkA////AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP/+/gCSutT/Wrzi/1Kw
 2P84ksH/LX6t/zOCsP8uhbT/Ooq1/1Kv2f9IpdT/VbXj/1O04P9Qsd7/VLfi/17E
 7f9sz/b/eNv8/4Xj//+J5///i+f//4jl//+L5f//h+L//4Th//9y0fb/YMDq/0uX
 wv9dsNn/Uq/b/1q85v9ewOj/fs7p/3bN7v951/n/etj8/3XW+v9szvT/ZMnx/12v
 1/971/X/aNP5/2TO9f9iwOj/WMHr/ziXxf9ClcD/Xa7R/9De6P////8A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP///wDs7/Jmcrna/2TB
 4/9Vr9f/N5G//yl9rP87h7L/UJ7H/0acxv8ug7P/TKPP/1+/6/9avun/Wbvn/2HG
 7/9rz/X/ddf7/4Pg/v+H4v//hOL//4Tj//+H4///hOL//4Pg/v951/r/a8fv/2O0
 2P9UteD/Wrrk/2nJ8P941/j/d8Tj/3vY+v920/j/etn8/3XY+/9ft97/e9bz/2i/
 4f+A0ez/XcLp/1/H8P9Xxe7/Q6XS/z2RvP9Iosr/m7/W//39/QD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD///8A0eLs/2fD
 6f9ft93/T6/Z/zSSwv84irn/MYW2/3W+4f9Krdz/R63b/0Gayf9Uq9X/as30/2nI
 8f9tzfX/c9T4/3vc/f+B4P//heL//4jl//+J5P//ieT//4nk//+C3v7/dNH2/2fB
 6P9owuf/Z8jw/3bV+P9xyer/dMjo/3jY+/911vr/aMjv/2rG5/973Pr/bMzv/2C2
 2v+T4/f/Wbzj/1vB6f9Cq9j/NJG+/0agyv91sdL/8/X3AP///wD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A///+AKTK
 3/9avOT/Ybne/02y3P82m8z/LI+//0Gg0P9GrNv/OJTE/1Cp1P9Sp9D/idXx/3TU
 +v931Pr/edb7/3nY/P9+3f//h+P//43m//+P5v//jeX//4zl//+G4v//e9j7/2nL
 8v9pzPL/etv8/4bg/f96z+7/e9v7/23N8f963P3/ZcPp/4jZ7/+P7f//huj+/2rD
 5P+M3PT/aNL4/0aw3f83k8D/Sp/I/2Coy//e5Oz/////AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A////APj4
 +QCXyOL/Wrzk/1y33f9PtuD/QKvc/z2n1v8wkcD/S57J/1zI8f9TsNz/ecns/3fY
 /P9juuP/XK/X/3/W9/+F3v//iuT//4/m//+P5f//j+X//43k//+G4v//g9///3vb
 /P9/3P3/iuT//5Hq//991vT/b8/x/1u64P9pxef/U6fQ/4fV7P+S8f//kvH//3jc
 9v9u0fX/Wr7o/zKQv/9Eo83/ZLTX/8jV3/////8A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP//
 /wD5+vsAl8rm/1m85v9Ztt3/U7vj/0u75/9CpdL/YLvi/0qx3f9RrdX/aNL5/1y+
 5/9etNv/XbXb/2ax1P+R6P//f9b1/3bK7f9/1vf/iOH+/4bd/P+C2fr/heD//3nS
 8/9/2Pn/ccXn/4ff+P9itNj/e9r3/3rf//900/L/abjb/4bJ5f+W8f//iu7//4Hm
 //9bueT/PpPC/02r1P9esdf/x9rn/////wD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD///8A+fr6AJTF4P9OtOD/W7fe/1e54P9TxO3/Usbw/0emz/9syOv/aNb7/2G8
 4f+H5v//asfp/2263P+Q6///bcXl/2C01/9artL/gdTw/3vS8P94z+v/c8jq/2Gy
 1/962/v/d9Hw/3HF5P91yej/heX8/4Xm//+U8v//kun4/33R7P+N7P//gej+/1a6
 4/9Em8b/VKvU/1ur0v/G1+P/////AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A////APv6+QCoyN3/TarX/1K34f9auN7/YMjt/13R9/9fz/f/ZNT6/2HF
 6f9mv+H/cMnp/4Tg+f+J6///ccjq/3jS7v+D2/T/kOj8/33Z9v92y+j/Z73f/4zl
 /P+B4///ctT1/4Xb8P9ivuP/h9jx/4/x//+W9P//mfj//4/y//911/P/TK3W/02i
 zP9Qq9X/aKnN/9Da4/////8A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP///wD///4AyNnl/2Ot0/9Msd7/XLzj/12+4/9jzfD/Z9n8/2/f
 //9WvOH/Ua7T/33g+/+D5f7/WK/V/1+74P9tyOf/ku3//3vb9/950Ov/aMPj/4TX
 8f+Q7v//gOH4/23C4f9es9X/heD1/5L0//+W9v//hef5/2C94v9Hnsn/UK7a/0+r
 1v+LttH/5+nu/////wD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A////APHy9ACUu9T/Q57L/1G14P9buN//XL7i/2PM
 7v9t3vv/eOb//3rl//+D6P//cM/q/2XC4v9txeX/jez+/4Xi9/9kvN3/XbLU/4Pa
 8P+S8///lfP//43r+v+U9P//jO/+/3rb8v9hweP/Uq7X/1Gt1f9Urdj/bbHU/7PI
 1//7+fkA////AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP///wD//fwAzdbe/3uvzv9Oqdb/TLDf/1i1
 3/9Zu+D/Zcfn/2nR7v9z3PX/f+j9/4nt//+N7v//j/D//4/y//+Q7v//k/P//4/w
 //+K7Pz/heb3/3nb8v9mxub/UrHY/0qo1f9Nr9v/TK/b/1+q0v+mwtf/8fL0AP//
 /wD///8A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD///4A////AP37+wDT3uf/ibTR/1el
 z/9FqNb/TbHf/1u85P9cu+H/YL7i/2O94f9qxuT/asfl/2vH5v9oxeb/ZcTl/1q7
 3/9buN7/U7Ha/1Sv2P9SsNv/QqPT/0Gdyv9hosX/nrrM/+fq7v////8A////AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP///wD///8A///+AOzu
 8Wa2y9r/h7XQ/2uv1P9El8T/Q6PR/0an1v9Krdr/Sa3b/0qp1/9Fp9b/UK/d/0Ok
 0/9EpNL/R6LQ/0maxf9uqMr/m73T/8jU3v/x8fIA////AP///wD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP//
 /wD///8A//79APb29gDc4eb/xdTg/63H2f+Ps8v/jrjS/4650/+PutX/jrXO/5Oz
 yP+vxtf/ytbg/+Ll6f/5+PcA////AP///wD///8A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP///wD///8A////AP///wD///8A////AP///wD///8A////AP//
 /wD///8A////AP///wD///8A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+/gD+/v4A/v7+AP7+
 /gD+/v4A/v7+AP7+/gD/////////8P///4Af///w///4AAP///D//+AAAH//8P//
 gAAAH//w//4AAAAH//D/+AAAAAP/8P/wAAAAAf/w/+AAAAAAf/D/wAAAAAA/8P+A
 AAAAAB/w/wAAAAAAH/D+AAAAAAAP8P4AAAAAAAfw/AAAAAAAA/D4AAAAAAAD8PgA
 AAAAAAHw8AAAAAAAAfDwAAAAAAAA8PAAAAAAAADw4AAAAAAAAPDgAAAAAAAAcOAA
 AAAAAABwwAAAAAAAAHDAAAAAAAAAcMAAAAAAAAAwwAAAAAAAADDAAAAAAAAAMMAA
 AAAAAAAwwAAAAAAAADDAAAAAAAAAMMAAAAAAAAAwwAAAAAAAADDAAAAAAAAAcMAA
 AAAAAABw4AAAAAAAAHDgAAAAAAAAcOAAAAAAAADw4AAAAAAAAPDwAAAAAAAA8PAA
 AAAAAAHw+AAAAAAAAfD4AAAAAAAD8PwAAAAAAAPw/gAAAAAAB/D+AAAAAAAP8P8A
 AAAAAA/w/4AAAAAAH/D/wAAAAAA/8P/gAAAAAH/w//AAAAAA//D/+AAAAAH/8P/+
 AAAAB//w//8AAAAf//D//8AAAD//8P//+AAB///w////AA////D/////////8P//
 ///////w")

(defparameter *tables-css*
  "table.prettytable {
  margin: 0em 0.5em 0.5em 0.5em;
  background: whitesmoke;
  border-collapse: collapse;
}
table.prettytable th, table.prettytable td {
  border: 1px silver solid;
  padding: 0.2em;
}
table.prettytable th {
  background: gainsboro;
  text-align: center;
}
table.prettytable caption {
  margin-left: inherit;
  margin-right: inherit;
}
table.innertable th, table.innertable td {
  border: 0;
}
span.id {
  font-size: 80%;
  font-family: monospace;
}
a.version:link {
  color: #AAAAAA;
}
a.version:visited {
  color: #AAAAAA;
}
a.version:active {
  color: #FF0000;
}
a.version:hover {
  background: #DDDDFF;
}
p.version {
  font-size: 80%;
  color: #AAAAAA;
}
")

(defun from-now-rfc-1123 (delta-seconds)
  (hunchentoot:rfc-1123-date
   (+ (get-universal-time) delta-seconds)))

(defun b642s (base64-string)
  (base64-string-to-string base64-string))

;; This is used for images in the client, so that we don't depend on
;; any external files.
(defun do-coded-png (base64-string &optional (type "image/png"))
  (do-png (b642s base64-string) type))

(defun do-png (string &optional (type "image/png"))
  (setf (hunchentoot:content-type*) type)
  ;; Set the image to expire 10 years from now
  (setf (hunchentoot:header-out "Expires")
        (from-now-rfc-1123 (* 10 365 24 60 60)))
  string)

(defun do-text (string &optional (type "text/html"))
  (setf (hunchentoot:content-type*) type)
  string)

(defparameter *coded-files*
  `(("/little-lambda.png" do-png ,(b642s *little-lambda-base64*))
    ("/truledger-logo-50x49.gif" do-png ,(b642s *truledger-logo-base64*) "image/gif")
    ("/truledger-logo-50x49.png" do-png ,(b642s *truledger-logo-png-base64*))
    ("/site-icon.ico" do-png ,(b642s *site-icon-base64*) "image/x-icon")
    ("/css/tables.css" do-text ,*tables-css* "text/css")))

(defun do-static-file ()
  (let* ((acceptor hunchentoot:*acceptor*)
         (port (hunchentoot:acceptor-port acceptor))
         (dir (port-www-dir port)))
    (cond ((not dir)
           (abort-request))
          (t
           (let* ((uri (remove-url-prefix
                        (hunchentoot:request-uri hunchentoot:*request*)))
                  (coded-file (cdr (assoc uri *coded-files* :test 'string-equal))))
             ;; Change this to look for the coded file in the file system first,
             ;; and use that if it's there.
             (cond ((null uri) nil)
                   (coded-file (apply (car coded-file) (cdr coded-file)))
                   (t (let ((file (merge-pathnames (strcat dir "/." uri))))
                        (hunchentoot:handle-static-file
                         (if (cl-fad:directory-pathname-p file)
                             (merge-pathnames "index.html" file)
                             file)
                         (and (equal "lisp" (pathname-type file))
                              "text/plain"))))))))))

(defparameter *default-server-port* 8785) ; "TRUL" on a phone dialpad
(defparameter *default-server-ssl-port* 8786) ; add one for SSL port

#|| Creating a self-signed certificate
openssl genrsa -out key.pem
openssl req -key key.pem -out req.pem -new 
openssl x509 -req -in req.pem -signkey key.pem -out cert.pem
openssl x509 -in cert.pem -text -noout
||#

#-windows
(progn

(defcfun ("getuid" getuid) :int)
(defcfun ("getgid" getgid) :int)

(defcfun ("setuid" %setuid) :int
  (uid :int))

(defun setuid (uid)
  (unless (eql 0 (%setuid uid))
    (error "Can't set uid to ~s" uid))
  uid)
  
(defcfun ("setgid" %setgid) :int
  (uid :int))

(defun setgid (gid)
  (unless (eql 0 (%setgid gid))
    (error "Can't set gid to ~s" gid))
  gid)
  
) ; #-windows

#+windows
(progn

(defun setuid (uid)
  (declare (ignore uid))
  (error "Can't set uid on Windows"))

(defun setgid (gid)
  (declare (ignore gid))
  (error "Can't set gid on Windows"))

) ; #+windows

(defclass acceptor (limited-thread-taskmaster:limited-thread-acceptor)
  ())

(defclass ssl-acceptor (limited-thread-taskmaster:limited-thread-ssl-acceptor)
  ())

(defun truledger-web-server (server &key
                             (pathname-defaults nil)
                             (www-dir "www")
                             ssl-certificate-file
                             ssl-privatekey-file
                             (port (if ssl-privatekey-file
                                       *default-server-ssl-port*
                                       *default-server-port*))
                             forwarding-port
                             uid
                             gid
                             )
  "Start the client, and, if SERVER is non-NIL, server web servers.
   Use 'dbs/clientdb/' and 'dbs/serverdb/' as the database directories,
   relative to the default directory."
  ;; In the SSL case, this will be a port to auto-forward to the SSL port.
  ;; Not coded yet.
  (when (xor ssl-certificate-file ssl-privatekey-file)
    (error
     "Both or neither required of SSL-CERTIFICATE-FILE and SSL-PRIVATEKEY-FILE"))
  (prog1
      (or (let ((acceptor (port-acceptor port)))
            (cond ((and acceptor
                        (typep acceptor
                               (if ssl-privatekey-file
                                   'ssl-acceptor
                                   'acceptor)))
                   acceptor)
                  (t (stop-web-server port)
                     nil)))
          (let ((acceptor (if ssl-privatekey-file
                              (make-instance
                               'ssl-acceptor
                               :port port
                               :ssl-certificate-file ssl-certificate-file
                               :ssl-privatekey-file ssl-privatekey-file)
                              (make-instance 'acceptor :port port))))
            (setf (port-server port) server
                  (port-www-dir port) www-dir
                  (port-pathname-defaults port) pathname-defaults)
            (setf (get-web-script-handler port "/")
                  'do-truledger-web-server
                  (get-web-script-handler port "/server")
                  #'(lambda ()
                      (if (parm "msg")
                          (do-truledger-web-server)
                          (redirect "/server/")))
                  (get-web-script-handler port "/server/")
                  'do-truledger-web-server
                  (get-web-script-handler port "/client")
                  #'(lambda () (redirect "/client/"))
                  (get-web-script-handler port "/client/")
                  'do-truledger-web-client       
                  (get-web-script-handler port "/server/client")
                  #'(lambda () (redirect "/client/"))
                  (get-web-script-handler port "/server/client/")
                  #'(lambda () (redirect "/client/"))
                  (get-web-script-handler port "/client/")
                  'do-truledger-web-client       
                  (get-web-script-handler port "/client/loom")
                  'do-loom-web-client)
            (when *url-prefix*
              (setf (get-web-script-handler port "")
                    (lambda () (redirect "/"))))
            (setf (port-acceptor port) acceptor)
            (hunchentoot:start acceptor)
            acceptor))
    (when (and ssl-certificate-file forwarding-port)
      ;; 
      (let ((acceptor (make-instance 'acceptor :port forwarding-port)))
        (setf (port-server forwarding-port) server
              (port-www-dir forwarding-port) www-dir
              (port-forwarded-to forwarding-port) port)
        (setf (get-web-script-handler forwarding-port "/")
              (lambda () (maybe-redirect-to-ssl port))
              (get-web-script-handler forwarding-port "/client")
              (lambda () (redirect "/client/"))
              (get-web-script-handler forwarding-port "/client/")
              (lambda () (maybe-redirect-to-ssl port))
              (port-acceptor forwarding-port) acceptor)
        (hunchentoot:start acceptor)))
    
    (handler-bind
        ((error (lambda (c)
                  (declare (ignore c))
                  (stop-web-server port))))
      (when gid (setgid gid))
      (when uid (setuid uid)))))

(defun maybe-redirect-to-ssl (to-port)
  (let ((script (hunchentoot:script-name*)))
    (cond ((member script '("/" "/server/" "/server") :test #'equal)
           (if (parm "msg")
               (redirect-to-ssl to-port)
               (do-truledger-web-server)))
          ((equal script "/client/")
           (redirect-to-ssl to-port))
          (t (do-truledger-web-server)))))

(defun redirect-to-ssl (to-port)
  (hunchentoot:redirect
   (hunchentoot:request-uri*)
   :port (unless (eql to-port 443) to-port)
   :protocol :https
   :code hunchentoot:+http-moved-permanently+))
              
(defun stop-web-server (&optional (port :all))
  "Stop the web server on PORT, or all web servers if PORT is :ALL (the default)."
  (cond ((eq port :all)
         (let ((ports (loop
                         for port being the hash-keys
                         of *ports-to-acceptors*
                         collect port)))
           (mapc 'stop-web-server ports)))
        (t (let ((forwarded-from (port-forwarded-from port)))
             (when forwarded-from
               (setf (port-forwarded-to forwarded-from) nil)
               (stop-web-server forwarded-from)))
           (let ((acceptor (port-acceptor port)))
             (setf (port-server port) nil
                   (port-acceptor port) nil
                   (port-www-dir port) nil)
             (when acceptor
               (remove-web-script-handlers port)
               (let ((started nil))
                 (process-run-function
                  (format nil "Stop port ~d" port)
                  (lambda ()
                    (setq started t)
                    (hunchentoot:stop acceptor)))
                 (process-wait "Server stopper" (lambda () started)))
               ;; Need to make a request in order to get the server to shut down
               (ignore-errors
                 (let ((ssl-p (hunchentoot:ssl-p acceptor)))
                   (dotimes (i 3)
                     (drakma:http-request
                      (format nil "~a://localhost:~d/"
                              (if ssl-p "https" "http") port)
                      :redirect nil)))))))))

(defun send-server-request (uri &optional msg-p)
  (drakma:http-request
   (format nil "http://~a~@[?msg=(bc50c4fd9c228a21f64d34ca644a46c1fe8520e4%2Cserverid%2C-----BEGIN+PUBLIC+KEY-----%0AMFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBAMwfcmkk2coTuYAEbdZ5iXggObNPzbSi%0ADnVtndZFe4%2F4Xg0IQPfpQ04OkhWIftMy1OjFhGlBzzNzdW98KYwKMgsCAwEAAQ%3D%3D%0A-----END+PUBLIC+KEY-----%0A)%3A%0AsLJ9GqFjZ61fq%2FbDFL6rxpY3w2s5dWIAXJCvPKQTPEkrG%2F2I1fwxBfugBmn%2FiPwa%0AjCRtnFDnrn7Mv%2BUY%2BSH4yw%3D%3D~]" uri msg-p)))

(defvar *stop-pounding-flag* nil)
(defvar *pound-lock* (make-lock "Pound lock"))
(defvar *pound-count* 0)

(defun pound (uri &optional msg-p (thread-count 5))
  (setq *stop-pounding-flag* nil
        *pound-count* 0)
  (dotimes (i thread-count)
    (process-run-function (format nil "Pound ~d" i)
      (lambda ()
        (loop
           (when *stop-pounding-flag* (return))
           (ignore-errors
             (send-server-request uri msg-p)
             (with-lock-grabbed (*pound-lock*)
               (incf *pound-count*))))))))

(defun stop-pounding ()
  (setq *stop-pounding-flag* t))

(defun pound-count ()
  *pound-count*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009-2010 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
