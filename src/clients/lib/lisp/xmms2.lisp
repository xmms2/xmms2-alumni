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

	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-UINT32+) ; UINT
	 (xmmsc-result-get-uint result result-pointer)
	 (mem-ref result-pointer :unsigned-int 0))

	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-INT32+) ; INT
	 (xmmsc-result-get-int result result-pointer)
	 (mem-ref result-pointer :int 0))

	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-STRING+) ;string
	 (xmmsc-result-get-string result result-pointer)
	 (foreign-string-to-lisp (mem-ref result-pointer :pointer 0)))

	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-DICT) ;string
	 (xmmsc-result-get-string result result-pointer)
	 (foreign-string-to-lisp (mem-ref result-pointer :pointer 0)))

	((equal result-type :+XMMSC-RESULT-VALUE-TYPE-PROPDICT) ;string
	 (xmmsc-result-get-string result result-pointer)
	 (foreign-string-to-lisp (mem-ref result-pointer :pointer 0)))

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

;; Collections

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

(defun get-collection (name namespace)
  (sync-exec #'xmmsc-coll-get name namespace))

;;; Highlevel Collection

(defmacro album (album &optional reference)
  (if (typep album 'cons)
    `(coll-union ,@(loop for name in album
			 collect `(album ,name ,reference) into ret
			 finally (return ret)))
    `(coll-match "album" ,(concatenate 'string "%" (string album) "%") ,reference)))

(defmacro artist (artist &optional reference)
  (if (typep artist 'cons)
    `(coll-union ,@(loop for name in artist
			 collect `(artist ,name ,reference) into ret
			 finally (return ret)))
    `(coll-match "artist" ,(concatenate 'string "%" (string artist) "%") ,reference)))

(defmacro song (song &optional reference)
  (if (typep song 'cons)
    `(coll-union ,@(loop for name in song
			 collect `(song ,name ,reference) into ret
			 finally (return ret)))
    `(coll-match "title" ,(concatenate 'string "%" (string song) "%") ,reference)))
