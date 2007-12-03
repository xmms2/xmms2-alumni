(load "xmmsc.lisp")

(defpackage :xmms2
  (:use :common-lisp :xmmsc :cffi))

(in-package :xmms2)

(defun get-value-from-result (result)
  (if (= 1 (xmmsc-result-iserror result))
    (error (xmmsc-result-get-error result))
    (let ((result-type (foreign-enum-keyword '#.(my-lispify "xmmsc_result_value_type_t" 'enumname) (xmmsc-result-get-type result)))
	  (result-pointer (foreign-alloc :pointer)))
      (cond
	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-NONE+) nil)

	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-UINT-32+) ; UINT
	 (xmmsc-result-get-uint result result-pointer)
	 (mem-ref result-pointer :unsigned-int 0))

	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-INT-32+) ; INT
	 (xmmsc-result-get-int result result-pointer)
	 (mem-ref result-pointer :int 0))

	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-STRING+) ;string
	 (xmmsc-result-get-string result result-pointer)
	 (foreign-string-to-lisp (mem-ref result-pointer :pointer 0)))

	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-DICT+) ;string
	 (let ((result-list (list)))
	   (defcallback dict-foreach :void ((key-pointer :pointer) (result-type :int) (value-pointer :pointer) (user-data :pointer))
	    (declare (ignore user-data))
			(let ((key (foreign-string-to-lisp key-pointer))
			      (value (if (= result-type 3) (foreign-string-to-lisp value-pointer) (pointer-address value-pointer))))
			 (setf result-list (cons (list key value) result-list))))
	   (xmmsc-result-dict-foreach result (callback dict-foreach) (null-pointer))
	 result-list))

	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-PROPDICT+) ;string
	 (let ((result-list (list)))
	   (defcallback propdict-foreach :void ((key-pointer :pointer) (result-type :int) (value-pointer :pointer) (source-pointer :pointer) (user-data :pointer))
			(declare (ignore user-data))
			(let ((key (foreign-string-to-lisp key-pointer))
			      (source (foreign-string-to-lisp source-pointer))
			      (value (if (= result-type 3) (foreign-string-to-lisp value-pointer) (pointer-address value-pointer))))
			 (setf result-list (cons (list source key value) result-list))))
	   (xmmsc-result-propdict-foreach result (callback propdict-foreach) (null-pointer))
	 result-list))

	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-COLL+) ;collection
	 (xmmsc-result-get-collection result result-pointer)
	 (mem-ref result-pointer :pointer 0))
	; (collection-c-to-lisp (mem-ref result-pointer :pointer 0)))
	(t (error (format nil "not yet implementet - result-type = ~a" (foreign-enum-value '#.(my-lispify "xmmsc_result_value_type_t" 'enumname) result-type))))))))

(defun result-c-to-lisp (result)
  (if (= (xmmsc-result-is-list result) 1)
    (loop when (= (xmmsc-result-list-valid result) 0) do (return col)
	  collect (get-value-from-result result) into col
	  do (xmmsc-result-list-next result))
    (get-value-from-result result)))

(defctype result (:wrapper :pointer :from-c result-c-to-lisp))

(defvar *default-connection* (xmmsc-init "xmmsc-lisp"))
(xmmsc-connect *default-connection* (null-pointer))

(defmacro sync-bind (old-name new-name &rest arguments)
  `(defun ,new-name ,arguments
     (let ((result (,old-name *default-connection* ,@arguments)))
       (xmmsc-result-wait result)
       (convert-from-foreign result 'result))))

(defun sync-exec (function &rest arguments)
  (let ((result (apply function *default-connection* arguments)))
    (xmmsc-result-wait result)
    (convert-from-foreign result 'result)))

(defun list-to-string (string-list &optional (string ""))
  (if (equal string-list nil)
    string
    (list-to-string (cdr string-list) (concatenate 'string string (car string-list)))))

(defun get-indent-tabs (level)
  (list-to-string
    (loop for i from 1 upto level
	  collect (string "  "))))

(defun attribute-list-to-string (attribute-list &optional (indent-level 0))
  (if (equal attribute-list nil)
    nil
    (let ((pair (car attribute-list)))
      (concatenate 'string
		   (string #\Newline)
		   (get-indent-tabs indent-level)
		   (car pair) ": " (cdr pair)
		   (attribute-list-to-string (cdr attribute-list) indent-level)))))

(defun collection-c-to-lisp (collection &optional (indent-level 0))
  (let ((coll-type (xmmsc-coll-get-type collection)))
    (xmmsc-coll-operand-list-first collection)
    (xmmsc-coll-attribute-list-first collection)
    (concatenate 'string (string #\Newline) (get-indent-tabs indent-level) "("
		 (cond
		   ((= coll-type 0) ;XMMS_COLLECTION_TYPE_REFERENCE
		    "reference")
		   ((= coll-type 1) ;XMMS_COLLECTION_TYPE_UNION
		    "union")
		   ((= coll-type 2) ;XMMS_COLLECTION_TYPE_INTERSECTION
		    "intersection")
		   ((= coll-type 3) ;XMMS_COLLECTION_TYPE_COMPLEMENT
		    "complement")
		   ((= coll-type 4) ;XMMS_COLLECTION_TYPE_HAS
		    "has")
		   ((= coll-type 5) ;XMMS_COLLECTION_TYPE_EQUALS
		    "equals")
		   ((= coll-type 6) ;XMMS_COLLECTION_TYPE_MATCH
		    "match")
		   ((= coll-type 7) ;XMMS_COLLECTION_TYPE_SMALLER
		    "smaller")
		   ((= coll-type 8) ;XMMS_COLLECTION_TYPE_GREATER
		    "greater")
		   ((= coll-type 9) ;XMMS_COLLECTION_TYPE_IDLIST
		    "idlist")
		   ((= coll-type 10) ;XMMS_COLLECTION_TYPE_QUEUE
		    "queue")
		   ((= coll-type 11) ;XMMS_COLLECTION_TYPE_PARTYSHUFFLE
		    "partyshuffle"))

		 ; retrieving attributes
		 (attribute-list-to-string
		   (loop with key-pointer = (foreign-alloc :pointer)
			 with value-pointer = (foreign-alloc :pointer)
			 while (= (xmmsc-coll-attribute-list-valid collection) 1)
			 do (xmmsc-coll-attribute-list-entry collection key-pointer value-pointer)
			 do (xmmsc-coll-attribute-list-next collection)
			 collect (cons (foreign-string-to-lisp (mem-ref key-pointer :pointer 0))
				       (foreign-string-to-lisp (mem-ref value-pointer :pointer 0))))
		   (1+ indent-level))
		 ; retrieving operands
		 (list-to-string
		   (loop with operand-pointer = (foreign-alloc :pointer)
			 while (= (xmmsc-coll-operand-list-valid collection) 1)
			 do (xmmsc-coll-operand-list-entry collection operand-pointer)
			 do (xmmsc-coll-operand-list-next collection)
			 collect (collection-c-to-lisp (mem-ref operand-pointer :pointer 0) (1+ indent-level))))
		 (string #\Newline)
		 (get-indent-tabs indent-level)
		 ")")))

;;;; Collections
;;; Lowlevel structure generation
(defmacro collection-type (type)
  `(foreign-enum-value 'XMMSC-COLL-TYPE-T ,(intern (concatenate 'string "+XMMS-COLLECTION-TYPE-" (string type) "+") (find-package :keyword))))

(defun coll-union (&rest collections)
  (let ((new-collection (xmmsc-coll-new (collection-type union))))
    (loop for (fun . nil) on collections
	  do (xmmsc-coll-add-operand new-collection (eval fun)))
    (return-from coll-union new-collection)))

(defun coll-intersection (&rest collections)
  (let ((new-collection (xmmsc-coll-new (collection-type intersection))))
    (loop for (fun . nil) on collections
	  do (xmmsc-coll-add-operand new-collection (eval fun)))
    (return-from coll-intersection new-collection)))

(defun coll-complement (&rest collections)
  (let ((new-collection (xmmsc-coll-new (collection-type complement))))
    (xmmsc-coll-add-operand new-collection (apply 'coll-union collections))
    (return-from coll-complement new-collection)))

;(defun coll-reference (collection)
;(let ((new-collection (xmmsc-coll-new (collection-type reference))))
;(xmmsc-coll-add-operand new-collection collection)
;(return-from coll-reference new-collection)))

(defun coll-match (key value &optional reference)
  (let ((new-collection (xmmsc-coll-new (collection-type match))))
    (xmmsc-coll-add-operand new-collection (if reference (eval reference) (xmmsc-coll-universe)))
    (xmmsc-coll-attribute-set new-collection "field" key)
    (xmmsc-coll-attribute-set new-collection "value" value)
    (return-from coll-match new-collection)))

(defun coll-equals (key value &optional reference)
  (let ((new-collection (xmmsc-coll-new (collection-type equals))))
    (xmmsc-coll-add-operand new-collection (if reference (eval reference) (xmmsc-coll-universe)))
    (xmmsc-coll-attribute-set new-collection "field" key)
    (xmmsc-coll-attribute-set new-collection "value" value)
    (return-from coll-equals new-collection)))

(defun coll-smaller (key value &optional reference)
  (let ((new-collection (xmmsc-coll-new (collection-type smaller))))
    (xmmsc-coll-add-operand new-collection (if reference (eval reference) (xmmsc-coll-universe)))
    (xmmsc-coll-attribute-set new-collection "field" key)
    (xmmsc-coll-attribute-set new-collection "value" value)
    (return-from coll-smaller new-collection)))

(defun coll-greater (key value &optional reference)
  (let ((new-collection (xmmsc-coll-new (collection-type greater))))
    (xmmsc-coll-add-operand new-collection (if reference (eval reference) (xmmsc-coll-universe)))
    (xmmsc-coll-attribute-set new-collection "field" key)
    (xmmsc-coll-attribute-set new-collection "value" value)
    (return-from coll-greater new-collection)))

(defun coll-has (key &optional reference)
  (let ((new-collection (xmmsc-coll-new (collection-type has))))
    (xmmsc-coll-add-operand new-collection (if reference (eval reference) (xmmsc-coll-universe)))
    (xmmsc-coll-attribute-set new-collection "field" key)
    (return-from coll-has new-collection)))

;;; Highlevel structure generation
(defmacro with-highlevel-bindings (&body body)
  `(macrolet
     ((album (album &optional reference)
	     (if (typep album 'cons)
	       `(coll-union ,@(loop for name in album
				    collect `(album ,name ,reference) into ret
				    finally (return ret)))
	       `(coll-match "album" ,(concatenate 'string "%" (string album) "%") ,reference)))
      (artist (artist &optional reference)
	      (if (typep artist 'cons)
		`(coll-union ,@(loop for name in artist
				     collect `(artist ,name ,reference) into ret
				     finally (return ret)))
		`(coll-match "artist" ,(concatenate 'string "%" (string artist) "%") ,reference)))
      (song (song &optional reference)
	    (if (typep song 'cons)
	      `(coll-union ,@(loop for name in song
				   collect `(song ,name ,reference) into ret
				   finally (return ret)))
	      `(coll-match "title" ,(concatenate 'string "%" (string song) "%") ,reference)))
      (collection (name &optional (namespace "Collections"))
		  `(sync-exec #'xmmsc-coll-get ,name ,namespace))
      (playlist (name &optional (namespace "Playlists"))
		`(sync-exec #'xmmsc-coll-get ,name ,namespace)))
     ,@body))

(defmacro with-collection (name-structure &body body)
  `(let (,@(loop for (name . structure) in name-structure
		 collect `(,name (with-highlevel-bindings ,(car structure))) into ret
		 finally (return ret)) )
     ,@body))

;;; Collection Operations
(defmacro save-collection (collection-structure name &optional (namespace "Collections"))
  `(with-collection ((nc ,collection-structure))
		    (sync-exec #'xmmsc-coll-save nc ,name ,namespace)))

(defun remove-collection (name &key (namespace "Collections"))
  (sync-exec #'xmmsc-coll-remove name namespace))

(defun remove-playlist (name &key (namespace "Playlists"))
  (remove-collection name :namespace namespace))

(defun rename-collection (oldname newname &key (namespace "Collections"))
  (sync-exec #'xmmsc-coll-rename oldname newname namespace))

(defun rename-playlist (oldname newname &key (namespace "Playlists"))
  (rename-collection oldname newname :namespace namespace))

(defmacro collection-query-ids (collection &key (order-by nil) (start 0) (length 0))
  (let ((order (typecase order-by
		 (cons `(string-array-lisp-to-c ,order-by))
		 (string `(string-array-lisp-to-c '(,order-by)))
		 (t '(null-pointer)))))
    `(with-collection ((nc ,collection))
		      (sync-exec #'xmmsc-coll-query-ids nc ,order ,start ,length))))

(defun list-collections (&key (namespace "Collections") (show-hidden nil))
  (if show-hidden
    (sync-exec #'xmmsc-coll-list namespace)
    (remove-if #'(lambda (name) (char= (elt name 0) #\_)) (list-collections :namespace namespace :show-hidden t))))

(defun list-playlists (&key (namespace "Playlists") (show-hidden nil))
  (list-collections :namespace namespace :show-hidden show-hidden))

;;;; Playlist control
(defun active-playlist ()
  (sync-exec #'xmmsc-playlist-current-active))

(defun playlist-list-entries (&optional (playlist (active-playlist)))
  (sync-exec #'xmmsc-playlist-list-entries playlist))

(defun playlist-set-next (pos &key (relative nil))
  (if relative
    (sync-exec #'xmmsc-playlist-set-next-rel pos)
    (sync-exec #'xmmsc-playlist-set-next pos)))

(defmacro playlist-append-collection (collection-structure &key (order-by nil) (playlist (active-playlist)))
  (let ((order (typecase order-by
		 (cons `(string-array-lisp-to-c ,order-by))
		 (string `(string-array-lisp-to-c '(,order-by)))
		 (t '(null-pointer)))))
    `(with-collection ((nc ,collection-structure))
		      (sync-exec #'xmmsc-playlist-add-collection ,playlist nc ,order))))

(defun shuffle (&optional (playlist (active-playlist)))
  (sync-exec #'xmmsc-playlist-shuffle playlist))

;;;; PlaybackControl
(defun toggle-play ()
  (if (equal :+XMMS-PLAYBACK-STATUS-PLAY+ (foreign-enum-keyword 'xmms-playback-status-t (sync-exec #'xmmsc-playback-status)))
    (sync-exec #'xmmsc-playback-pause)
    (sync-exec #'xmmsc-playback-start)))

(defun pause ()
  (sync-exec #'xmmsc-playback-pause))

(defun tickle ()
  (sync-exec #'xmmsc-playback-tickle))

(defun stop ()
  (sync-exec #'xmmsc-playback-stop))

(defun jump-to (pos &key (relative nil))
  (playlist-set-next pos :relative relative)
  (tickle))

(defun next ()
  (jump-to 1 :relative t))

(defun back ()
  (jump-to -1 :relative t))


;;;; Medialib
(defun remove-entry (id)
  (sync-exec #'xmmsc-medialib-remove-entry id))

(defun mlib-entry-set-property (id key value &key (source nil))
  (typecase value
    (number (if source
	      (sync-exec #'xmmsc-medialib-entry-property-set-int-with-source id source key value)
	      (sync-exec #'xmmsc-medialib-entry-property-set-int id key value)))
    (string (if source
	      (sync-exec #'xmmsc-medialib-entry-property-set-str-with-source id source key value)
	      (sync-exec #'xmmsc-medialib-entry-property-set-str id key value)))))

(defun mlib-entry-remove-property (id key &key (source nil))
  (if source
    (sync-exec #'xmmsc-medialib-entry-property-remove-with-source id source key)
    (sync-exec #'xmmsc-medialib-entry-property-remove id key)))
