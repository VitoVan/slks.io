(defpackage #:slks-io
  (:use #:cl ))
(in-package #:slks-io)
(ql:quickload '(:woo :cl-json :cl-redis :ningle :clack))
;; Connect to redis
(redis:connect)

;; Removed i I l o O 0 for better reading experience
(defvar *alphabet* "abcdefghjkmnpqrstuvwxyzABCDEFGHJKLMNOPQRSTUVWXYZ123456789")

(defun test-timeout ()
  (handler-case (sb-ext:with-timeout 2 (sleep 10))
    (condition () "sb-ext:with-timeout works fine!")
    (:no-error (r) (format nil "no timeout, result ~S" r))))

(defun number->string(num)
  (handler-case
      (if (equal num 0)
          (subseq *alphabet* 0 1)
          (do* ((seq-start 0 (cadr (multiple-value-list (floor number (length *alphabet*)))))
                (seq-end 0 (+ 1 seq-start))
                (seqs "" (concatenate 'string (subseq *alphabet* seq-start seq-end) seqs))
                (number num (car (multiple-value-list (floor (/ number (length *alphabet*)))))))
               ((equal number 0) seqs)))
    (condition () nil)))

(defun string->number(str)
  (handler-case
      (let ((i 0))
        (map 'string
             #'(lambda (c)
                 (setf i (+ (* i (length *alphabet*)) (position c *alphabet*)))
                 c)
             str)
        i)
    (condition () nil)))

(defun red-next-int (name &optional (default 1))
  (red:incrby name default))

(defun red-get-url (id)
  (red:hget 'id->url id))

(defun red-get-id (url)
  (red:hget 'url->id url))

(defun red-set-url (url)
  (or (parse-integer (string (red-get-id url)) :junk-allowed t)
      (let ((id (red-next-int 'counter)))
        (red:hset 'url->id url id)
        (red:hset 'id->url id url)
        id)))

(defvar *params* nil)

(defun api-set-handler(params)
  (setf *params* params)
  (let ((url (cdr (assoc "url" params :test #'string=))))
    (number->string
     (red-set-url url))))

(defun api-get-handler(params)
  (setf *params* params)
  (let ((id (cdr (assoc "id" params :test #'string=))))
    (red-get-url (string->number id))))

(defun file-to-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun page-handler(params)
  (declare (ignore params))
  (file-to-string #p"index.html"))

(defvar *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/")
      (file-to-string #p"default.html"))

(setf (ningle:route *app* "/api" :method :post)
      #'api-set-handler)

(setf (ningle:route *app* "/api" :method :get)
      #'api-get-handler)

(setf (ningle:route *app* "/(?!api)(.*)" :regexp t :method :get)
      #'page-handler)

(clack:clackup *app* :server :woo)

;; -- test --
(ql:quickload 'drakma)

(drakma:http-request "http://localhost:5000/api"
                     :method :post
                     :parameters '(("url" . "http://www.example.com")))
