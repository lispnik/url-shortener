(ql:quickload '("clog" "cl-ksuid" "dbi" "dbd-sqlite3" "alexandria" "cl-ppcre"))

(defpackage #:url-shortener
  (:use #:common-lisp
	#:clog)
  (:local-nicknames (#:a #:alexandria)
		    (#:r #:cl-ppcre)))

(in-package #:url-shortener)

(defvar *connection*
  (dbi:connect :sqlite3
               :database-name "urls.sqlite3"))

(defun initialize-database ()
  (dbi:do-sql *connection* "
create table if not exists urls (
  timestamp datetime default current_timestamp,
  url text not null unique,
  url_shortened_path text not null,
  accessed_count int not null default 0,
  accessed_last datetime
)")
  (dbi:do-sql *connection* "
create unique index if not exists url_shortened_path_index on urls (url_shortened_path)"))

(defun create-shortened (url)
  (let* ((insert (dbi:prepare *connection* "
insert or ignore into urls (url, url_shortened_path) values (?, ?)
returning url_shortened_path as RESULT"))
	 (ksuid (format nil "~A" (make-instance 'ksuid:ksuid)))
	 (insert (dbi:execute insert (list url ksuid))))
    (a:if-let ((url-shortened-path (getf (dbi:fetch insert) :result)))
      url-shortened-path
      (let* ((query (dbi:prepare *connection* "select url_shortened_path as RESULT from urls where url = ?")))
	(getf (dbi:fetch (dbi:execute query (list url))) :result)))))

(defun lookup-shortened (shortened-url)
  (let ((query (dbi:prepare *connection* "
update urls set accessed_last = current_timestamp, accessed_count = accessed_count + 1
where url_shortened_path = ? returning url as RESULT")))
    (getf (dbi:fetch (dbi:execute query (list shortened-url))) :result)))

(defun on-new-window (body)
  (clog-web:clog-web-initialize body)
  (let* ((input (create-div body))
	 (url-input (create-form-element input :text :placeholder "Enter a URL..."))
	 (ok-button (create-button input :content "OK"))
	 (output (create-div body))
	 (url-output (create-span output))
	 (copy-button (create-img output)))
    (set-on-click ok-button
		  (lambda (obj)
		    (setf (text url-output)
			  (format nil "/re?~A" (create-shortened (value url-input))))))
    (set-on-click copy-button
		  'copy-button-handler)))

(defun redirector-middleware (app)
  (lambda (env)
    (if (string= (getf env :path-info) "/re")
	(a:if-let ((url-shortened-path  (getf env :query-string)))
	  (a:if-let ((url (lookup-shortened url-shortened-path)))
	    `(301 (:location ,url
                   ;; FIXME some combination of this crap prevents caching
;;                   :cache-control "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
                   ;; :cache-control "no-cache"
                   ;; :cache-control "no-cache, no-store, must-revalidate"
                   ;; :pragma "no-cache"
                   ;; :expires 0
                   ))
	    (funcall app env))
	  (funcall app env))
	(funcall app env))))


(defun start ()
  (initialize-database)
  (initialize 'on-new-window
	      :lack-middleware-list '(redirector-middleware))
  (open-browser))

(defun stop ()
  (clog:shutdown))
