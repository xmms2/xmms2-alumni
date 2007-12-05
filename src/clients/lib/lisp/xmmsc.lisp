(require 'asdf)

(asdf:oos 'asdf:load-op :cffi)

(defpackage :xmmsc
  (:use :common-lisp :cffi))

(in-package :xmmsc)

(define-foreign-library libxmmsclient
			(t (:default "libxmmsclient")))

(use-foreign-library libxmmsclient)

(defun my-lispify (name flag &optional (package *package*))
  (labels ((helper (lst last rest &aux (c (car lst)))
		   (cond
		     ((null lst)
		      rest)
		     ((upper-case-p c)
		      (helper (cdr lst) 'upper
			      (case last
				((lower digit) (list* c #\- rest))
				(t (cons c rest)))))
		     ((lower-case-p c)
		      (helper (cdr lst) 'lower (cons (char-upcase c) rest)))
		     ((digit-char-p c)
		      (helper (cdr lst) 'digit
			      (case last
				((upper lower) (list* c #\- rest))
				(t (cons c rest)))))
		     ((char-equal c #\_)
		      (helper (cdr lst) '_ (cons #\- rest)))
		     (t
		       (error "Invalid character: ~A" c)))))
    (let ((fix (case flag
		 ((constant enumvalue) "+")
		 (variable "*")
		 (t ""))))
      (intern
	(concatenate
	  'string
	  fix
	  (nreverse (helper (concatenate 'list name) nil nil))
	  fix)
	package))))


(cffi:defcenum #.(my-lispify "xmmsc_result_value_type_t" 'enumname)
	       #.(my-lispify "XMMSC_RESULT_VALUE_TYPE_NONE" 'enumvalue :keyword)
	       #.(my-lispify "XMMSC_RESULT_VALUE_TYPE_UINT32" 'enumvalue :keyword)
	       #.(my-lispify "XMMSC_RESULT_VALUE_TYPE_INT32" 'enumvalue :keyword)
	       #.(my-lispify "XMMSC_RESULT_VALUE_TYPE_STRING" 'enumvalue :keyword)
	       #.(my-lispify "XMMSC_RESULT_VALUE_TYPE_STRINGLIST" 'enumvalue :keyword)
	       #.(my-lispify "XMMSC_RESULT_VALUE_TYPE_DICT" 'enumvalue :keyword)
	       #.(my-lispify "XMMSC_RESULT_VALUE_TYPE_LIST" 'enumvalue :keyword)
	       #.(my-lispify "XMMSC_RESULT_VALUE_TYPE_PROPDICT" 'enumvalue :keyword)
	       #.(my-lispify "XMMSC_RESULT_VALUE_TYPE_COLL" 'enumvalue :keyword)
	       #.(my-lispify "XMMSC_RESULT_VALUE_TYPE_BIN" 'enumvalue :keyword))

(export 'my-lispify)

(defun string-array-lisp-to-c (content)
  (typecase content
    (string
      (foreign-alloc :string
		     :initial-contents (list content)
		     :null-terminated-p t))
    (list 
      (foreign-alloc :string
		     :initial-contents content
		     :null-terminated-p t))
    (t (null-pointer))))

(export 'string-array-lisp-to-c)

(defun string-array-c-to-lisp (my-pointer)
  (if (not (null-pointer-p (mem-ref my-pointer :pointer)))
    (cons (mem-ref my-pointer :string) (string-array-c-to-lisp (inc-pointer my-pointer (foreign-type-size :string))))))

(export 'string-array-c-to-lisp)
;(cffi:defcenum #.(my-lispify "xmmsc_result_value_type_t" 'enumname)
;(#.(my-lispify "XMMSC_RESULT_VALUE_TYPE_NONE" 'enumvalue :keyword)
;(foreign-enum-value
;'#.(my-lispify "xmms_object_cmd_arg_type_t" 'enumname)
;#.(my-lispify "XMMSC_OBJECT_CMD_ARG_NONE" 'enumvalue :keyword))
;(#.(my-lispify "XMMSC_RESULT_VALUE_TYPE_UINT32" 'enumvalue :keyword)
;(foreign-enum-value '#.(my-lispify "xmms_object_cmd_arg_type_t" 'enumname)
;XMMS_OBJECT_CMD_ARG_INT32)
;(#.(my-lispify "XMMSC_RESULT_VALUE_TYPE_INT32" 'enumvalue :keyword)
;(foreign-enum-value '#.(my-lispify "xmms_object_cmd_arg_type_t" 'enumname)
;XMMS_OBJECT_CMD_ARG_INT32)
;(#.(my-lispify "XMMSC_RESULT_VALUE_TYPE_STRING" 'enumvalue :keyword)
;(foreign-enum-value '#.(my-lispify "xmms_object_cmd_arg_type_t" 'enumname)
;XMMS_OBJECT_CMD_ARG_STRING)
;(#.(my-lispify "XMMSC_RESULT_VALUE_TYPE_DICT" 'enumvalue :keyword)
;(foreign-enum-value '#.(my-lispify "xmms_object_cmd_arg_type_t" 'enumname)
;XMMS_OBJECT_CMD_ARG_DICT)
;(#.(my-lispify "XMMSC_RESULT_VALUE_TYPE_PROPDICT" 'enumvalue :keyword)
;(foreign-enum-value '#.(my-lispify "xmms_object_cmd_arg_type_t" 'enumname)
;XMMS_OBJECT_CMD_ARG_PROPDICT)
;(#.(my-lispify "XMMSC_RESULT_VALUE_TYPE_COLL" 'enumvalue :keyword)
;XMMS_OBJECT_CMD_ARG_CO)
;(#.(my-lispify "XMMSC_RESULT_VALUE_TYPE_BIN" 'enumvalue :keyword)
;XMMS_OBJECT_CMD_ARG_BIN))

(cl:export '#.(my-lispify "xmmsc_result_value_type_t" 'enumname))

(load "xmmsc-swig.lisp")
