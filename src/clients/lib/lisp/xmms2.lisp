(load "xmmsc.lisp")

(defpackage :xmms2
  (:use :common-lisp :xmmsc :cffi)
  (:export
    :save-collection
    :remove-collection
    :remove-playlist
    :rename-collection
    :rename-playlist
    :collection-query-ids
    :list-collections
    :list-playlists
    :active-playlist
    :playlist-list-entries
    :playlist-append-collection
    :shuffle
    :toggle-play
    :pause
    :tickle
    :stop
    :jump-to
    :next
    :back
    :mlib-remove-entry
    :mlib-entry-set-property
    :mlib-entry-remove-property))

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

        ((equal result-type :+XMMSC-RESULT-VALUE-TYPE-DICT+) ;dict
         (let ((result-table (make-hash-table :test 'equal)))
           (defcallback dict-foreach :void ((key-pointer :pointer) (result-type :int) (value-pointer :pointer) (user-data :pointer))
                        (declare (ignore user-data))
                        (let ((key (foreign-string-to-lisp key-pointer))
                              (value (if (= result-type 3) (foreign-string-to-lisp value-pointer) (pointer-address value-pointer))))
                          (setf (gethash key result-table) value)))
           (xmmsc-result-dict-foreach result (callback dict-foreach) (null-pointer))
           result-table))

        ((equal result-type :+XMMSC-RESULT-VALUE-TYPE-PROPDICT+) ;propdict
         (let ((result-table (make-hash-table :test 'equal)))
           (defcallback propdict-foreach :void ((key-pointer :pointer) (result-type :int) (value-pointer :pointer) (source-pointer :pointer) (user-data :pointer))
                        (declare (ignore user-data))
                        (let ((key (foreign-string-to-lisp key-pointer))
                              (source (foreign-string-to-lisp source-pointer))
                              (value (if (= result-type 3) (foreign-string-to-lisp value-pointer) (pointer-address value-pointer))))
                          (if (null (gethash source result-table))
                            (setf (gethash source result-table) (make-hash-table :test 'equal)))

                          (setf (gethash key (gethash source result-table)) value)))
           (xmmsc-result-propdict-foreach result (callback propdict-foreach) (null-pointer))
           result-table))

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
(defun collection-type (type)
  (foreign-enum-value 'XMMSC-COLL-TYPE-T (intern (concatenate 'string "+XMMS-COLLECTION-TYPE-" (string type) "+") (find-package :keyword))))

(defun coll-union (&rest collections)
  (let ((new-collection (xmmsc-coll-new (collection-type 'union))))
    (loop for collection in collections
          do (xmmsc-coll-add-operand new-collection collection))
    new-collection))

(defun coll-intersection (&rest collections)
  (let ((new-collection (xmmsc-coll-new (collection-type 'intersection))))
    (loop for collection in collections
          do (xmmsc-coll-add-operand new-collection collection))
    new-collection))

(defun coll-complement (&rest collections)
  (let ((new-collection (xmmsc-coll-new (collection-type 'complement))))
    (loop for collection in collections
          do (xmmsc-coll-add-operand new-collection collection))
    new-collection))

(defun coll-reference (&optional (collection "All Media"))
  (let ((new-collection (xmmsc-coll-new (collection-type 'reference))))
    (xmmsc-coll-attribute-set new-collection "reference" collection)
    (xmmsc-coll-attribute-set new-collection "namespace" "Collections")
    new-collection))

(defun coll-match (key &rest values)
  (if (and (= 1 (length values)) (not (listp (car values))))
    (let ((new-collection (xmmsc-coll-new (collection-type 'match))))
      (xmmsc-coll-attribute-set new-collection "field" key)
      (xmmsc-coll-attribute-set new-collection "value" (car values))
      (xmmsc-coll-add-operand new-collection (coll-reference))
      new-collection)
    (loop for value in values
          collect (coll-match key value) into collections
          finally (return (apply #'coll-union collections)))))

(defun coll-equals (key &rest values)
  (if (and (= 1 (length values)) (not (listp (car values))))
    (let ((new-collection (xmmsc-coll-new (collection-type 'equals))))
      (xmmsc-coll-attribute-set new-collection "field" key)
      (xmmsc-coll-attribute-set new-collection "value" (car values))
      (xmmsc-coll-add-operand new-collection (coll-reference))
      new-collection)
    (loop for value in values
          collect (coll-equals key value) into collections
          finally (return (apply #'coll-union collections)))))

;;; Collection Operations

(defun get-collection (name &optional (namespace "Collections"))
  (sync-exec #'xmmsc-coll-get name namespace))

(defun save-collection (collection name &optional (namespace "Collections"))
  "Save a the collection-structure on the server under the given name and optional namespace
  Usage:
  (save-collection (artist \"Katie Melua\") \"Katie\")"
  (sync-exec #'xmmsc-coll-save collection name namespace))

(defun remove-collection (name &key (namespace "Collections"))
  "Removes the named collection from the server
  Usage:
  (remove-collection \"Katie\" :namespace \"Collections\")"
  (sync-exec #'xmmsc-coll-remove name namespace))

(defun rename-collection (oldname newname &key (namespace "Collections"))
  "Rename a saved collection.
  Usage:
  (rename-collection \"Katie\" \"NewName\")"
  (sync-exec #'xmmsc-coll-rename oldname newname namespace))

(defun find-collection (id &optional (namespace "Collections"))
  (sync-exec #'xmmsc-coll-find id namespace))

(defmacro collection-query-ids (collection &key (order-by nil) (start 0) (length 0))
  "Returns a list containing IDs matching the given collection structure.
  You can limit the entries by specifying :length and set an offset with :start.
  Ordering is possible through :order-by. To sort by multiple keys provide a list.
  Usage:
  (collection-query-ids (artist \"Katie Melua\") :order-by (\"album\" \"tracknr\") :length 20)"
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

;;;; Playlist control
(defun active-playlist ()
  (sync-exec #'xmmsc-playlist-current-active))

(defun list-playlists (&key (namespace "Playlists") (show-hidden nil))
  (list-collections :namespace namespace :show-hidden show-hidden))

(defun rename-playlist (oldname newname &key (namespace "Playlists"))
  "Same as rename-collection operates in defaul namespace \"Playlists\""
  (rename-collection oldname newname :namespace namespace))

(defun create-playlist (name &optional)
  (sync-exec 'xmmsc-playlist-create name))
;(if (not (equal "list"))
;(type-playlist name type)))

(defun remove-playlist (name &key (namespace "Playlists"))
  "Same as remove-collection but with default namespace \"Playlists\""
  (remove-collection name :namespace namespace))

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

(defun make-url-valid (url)
  (let ((position (search "://" url)))
    (if (or (null position) (= 0 position))
      (concatenate 'string "file://" (namestring (truename url)))
      url)))

(defun add (url &optional (playlist (active-playlist))) ;TODO: fix this to use XMMS_ACTIVE_PLAYLIST-constant, because it's faster
  (let ((valid-url (make-url-valid url)))
    (if (eq #\/ (char valid-url (- (length valid-url) 1)))
      (sync-exec #'xmmsc-playlist-radd    playlist valid-url)
      (sync-exec #'xmmsc-playlist-add-url playlist valid-url))))

(defun add-id (id &optional (playlist (active-playlist))) ;TODO: fix this to use XMMS_ACTIVE_PLAYLIST-constant, because it's faster
  (sync-exec #'xmmsc-playlist-add-id playlist id))

(defun insert (url &optional position (playlist (active-playlist)))
  (sync-exec #'xmmsc-playlist-insert-url playlist position url))

(defun insert-id (id &optional position (playlist (active-playlist)))
  (sync-exec #'xmmsc-playlist-insert-id playlist position id))

(defun move (source destination &optional (playlist (active-playlist)))
  (sync-exec 'xmmsc-playlist-move-entry playlist source destination))

(defun clear (&optional (playlist (active-playlist)))
  (sync-exec #'xmmsc-playlist-clear playlist))

(defun shuffle (&optional (playlist (active-playlist)))
  (sync-exec #'xmmsc-playlist-shuffle playlist))

(defun sort-playlist (&key (playlist (active-playlist)) order-by)
  (if (null order-by)
    (error "You didn't specify an list with properties so sort by")
    (sync-exec #'xmmsc-playlist-sort playlist (string-array-lisp-to-c order-by))))


;;;; PlaybackControl
(defun toggle-play ()
  (if (equal :+XMMS-PLAYBACK-STATUS-PLAY+ (foreign-enum-keyword 'xmms-playback-status-t (sync-exec #'xmmsc-playback-status)))
    (sync-exec #'xmmsc-playback-pause)
    (sync-exec #'xmmsc-playback-start)))

(defun play ()
  (sync-exec #'xmmsc-playback-start))

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

(defun seek (time &key relative)
  (if relative
    (sync-exec #'xmmsc-playback-seek-ms-rel (round (* time 1000)))
    (sync-exec #'xmmsc-playback-seek-ms (round (* time 1000)))))

(defun set-volume (volume &key relative)
  (loop for channel being the hash-keys in (sync-exec 'xmmsc-playback-volume-get) using (hash-value value)
        do (if relative
             (sync-exec 'xmmsc-playback-volume-set channel (+ volume value))
             (sync-exec 'xmmsc-playback-volume-set channel volume))))

(defun current-id ()
  (sync-exec 'xmmsc-playback-current-id))

;;;; Medialib
(defun mlib-add-entry (url)
  (sync-exec #'xmmsc-medialib-add-entry (make-url-valid url)))

(defun mlib-remove-entry (id)
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

(defun mlib-get-info (&optional (id (current-id)))
  (sync-exec 'xmmsc-medialib-get-info id))
;;; config

(defun config-list ()
 (sync-exec #'xmmsc-configval-list))
