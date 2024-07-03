(defsystem #:url-shortener
  :components ((:file "url-shortener"))
  :depends-on (#:clog
	       #:cl-ksuid
	       #:dbi
	       #:dbd-sqlite3
	       #:alexandria))
