

;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here


(cffi:defcfun ("xmmsc_coll_new" #.(my-lispify "xmmsc_coll_new" 'function)) :pointer
  (type :int))

(cl:export '#.(my-lispify "xmmsc_coll_new" 'function))

(cffi:defcfun ("xmmsc_coll_ref" #.(my-lispify "xmmsc_coll_ref" 'function)) :void
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_ref" 'function))

(cffi:defcfun ("xmmsc_coll_unref" #.(my-lispify "xmmsc_coll_unref" 'function)) :void
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_unref" 'function))

(cffi:defcfun ("xmmsc_coll_set_idlist" #.(my-lispify "xmmsc_coll_set_idlist" 'function)) :void
  (coll :pointer)
  (ids :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_set_idlist" 'function))

(cffi:defcfun ("xmmsc_coll_add_operand" #.(my-lispify "xmmsc_coll_add_operand" 'function)) :void
  (coll :pointer)
  (op :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_add_operand" 'function))

(cffi:defcfun ("xmmsc_coll_remove_operand" #.(my-lispify "xmmsc_coll_remove_operand" 'function)) :void
  (coll :pointer)
  (op :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_remove_operand" 'function))

(cffi:defcfun ("xmmsc_coll_idlist_append" #.(my-lispify "xmmsc_coll_idlist_append" 'function)) :int
  (coll :pointer)
  (id :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_coll_idlist_append" 'function))

(cffi:defcfun ("xmmsc_coll_idlist_insert" #.(my-lispify "xmmsc_coll_idlist_insert" 'function)) :int
  (coll :pointer)
  (index :unsigned-int)
  (id :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_coll_idlist_insert" 'function))

(cffi:defcfun ("xmmsc_coll_idlist_move" #.(my-lispify "xmmsc_coll_idlist_move" 'function)) :int
  (coll :pointer)
  (index :unsigned-int)
  (newindex :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_coll_idlist_move" 'function))

(cffi:defcfun ("xmmsc_coll_idlist_remove" #.(my-lispify "xmmsc_coll_idlist_remove" 'function)) :int
  (coll :pointer)
  (index :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_coll_idlist_remove" 'function))

(cffi:defcfun ("xmmsc_coll_idlist_clear" #.(my-lispify "xmmsc_coll_idlist_clear" 'function)) :int
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_idlist_clear" 'function))

(cffi:defcfun ("xmmsc_coll_idlist_get_index" #.(my-lispify "xmmsc_coll_idlist_get_index" 'function)) :int
  (coll :pointer)
  (index :unsigned-int)
  (val :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_idlist_get_index" 'function))

(cffi:defcfun ("xmmsc_coll_idlist_set_index" #.(my-lispify "xmmsc_coll_idlist_set_index" 'function)) :int
  (coll :pointer)
  (index :unsigned-int)
  (val :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_coll_idlist_set_index" 'function))

(cffi:defcfun ("xmmsc_coll_idlist_get_size" #.(my-lispify "xmmsc_coll_idlist_get_size" 'function)) :pointer
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_idlist_get_size" 'function))

(cffi:defcfun ("xmmsc_coll_get_type" #.(my-lispify "xmmsc_coll_get_type" 'function)) :int
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_get_type" 'function))

(cffi:defcfun ("xmmsc_coll_get_idlist" #.(my-lispify "xmmsc_coll_get_idlist" 'function)) :pointer
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_get_idlist" 'function))

(cffi:defcfun ("xmmsc_coll_operand_list_first" #.(my-lispify "xmmsc_coll_operand_list_first" 'function)) :int
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_operand_list_first" 'function))

(cffi:defcfun ("xmmsc_coll_operand_list_valid" #.(my-lispify "xmmsc_coll_operand_list_valid" 'function)) :int
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_operand_list_valid" 'function))

(cffi:defcfun ("xmmsc_coll_operand_list_entry" #.(my-lispify "xmmsc_coll_operand_list_entry" 'function)) :int
  (coll :pointer)
  (operand :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_operand_list_entry" 'function))

(cffi:defcfun ("xmmsc_coll_operand_list_next" #.(my-lispify "xmmsc_coll_operand_list_next" 'function)) :int
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_operand_list_next" 'function))

(cffi:defcfun ("xmmsc_coll_operand_list_save" #.(my-lispify "xmmsc_coll_operand_list_save" 'function)) :int
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_operand_list_save" 'function))

(cffi:defcfun ("xmmsc_coll_operand_list_restore" #.(my-lispify "xmmsc_coll_operand_list_restore" 'function)) :int
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_operand_list_restore" 'function))

(cffi:defcfun ("xmmsc_coll_operand_list_clear" #.(my-lispify "xmmsc_coll_operand_list_clear" 'function)) :void
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_operand_list_clear" 'function))

(cffi:defcfun ("xmmsc_coll_attribute_list_first" #.(my-lispify "xmmsc_coll_attribute_list_first" 'function)) :void
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_attribute_list_first" 'function))

(cffi:defcfun ("xmmsc_coll_attribute_list_valid" #.(my-lispify "xmmsc_coll_attribute_list_valid" 'function)) :int
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_attribute_list_valid" 'function))

(cffi:defcfun ("xmmsc_coll_attribute_list_entry" #.(my-lispify "xmmsc_coll_attribute_list_entry" 'function)) :void
  (coll :pointer)
  (k :pointer)
  (v :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_attribute_list_entry" 'function))

(cffi:defcfun ("xmmsc_coll_attribute_list_next" #.(my-lispify "xmmsc_coll_attribute_list_next" 'function)) :void
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_attribute_list_next" 'function))

(cffi:defcfun ("xmmsc_coll_attribute_set" #.(my-lispify "xmmsc_coll_attribute_set" 'function)) :void
  (coll :pointer)
  (key :string)
  (value :string))

(cl:export '#.(my-lispify "xmmsc_coll_attribute_set" 'function))

(cffi:defcfun ("xmmsc_coll_attribute_remove" #.(my-lispify "xmmsc_coll_attribute_remove" 'function)) :int
  (coll :pointer)
  (key :string))

(cl:export '#.(my-lispify "xmmsc_coll_attribute_remove" 'function))

(cffi:defcfun ("xmmsc_coll_attribute_get" #.(my-lispify "xmmsc_coll_attribute_get" 'function)) :int
  (coll :pointer)
  (key :string)
  (value :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_attribute_get" 'function))

(cffi:defcfun ("xmmsc_coll_attribute_foreach" #.(my-lispify "xmmsc_coll_attribute_foreach" 'function)) :void
  (coll :pointer)
  (func :pointer)
  (user_data :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_attribute_foreach" 'function))

(cffi:defcfun ("xmmsc_coll_universe" #.(my-lispify "xmmsc_coll_universe" 'function)) :pointer)

(cl:export '#.(my-lispify "xmmsc_coll_universe" 'function))

(cl:defconstant #.(my-lispify "XMMS_IPC_PROTOCOL_VERSION" 'constant) 10)

(cl:export '#.(my-lispify "XMMS_IPC_PROTOCOL_VERSION" 'constant))

(cffi:defcenum #.(my-lispify "xmms_object_cmd_arg_type_t" 'enumname)
	#.(my-lispify "XMMS_OBJECT_CMD_ARG_NONE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_OBJECT_CMD_ARG_UINT32" 'enumvalue :keyword)
	#.(my-lispify "XMMS_OBJECT_CMD_ARG_INT32" 'enumvalue :keyword)
	#.(my-lispify "XMMS_OBJECT_CMD_ARG_STRING" 'enumvalue :keyword)
	#.(my-lispify "XMMS_OBJECT_CMD_ARG_STRINGLIST" 'enumvalue :keyword)
	#.(my-lispify "XMMS_OBJECT_CMD_ARG_DICT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_OBJECT_CMD_ARG_LIST" 'enumvalue :keyword)
	#.(my-lispify "XMMS_OBJECT_CMD_ARG_PROPDICT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_OBJECT_CMD_ARG_COLL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_OBJECT_CMD_ARG_BIN" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmms_object_cmd_arg_type_t" 'enumname))

(cffi:defcenum #.(my-lispify "xmms_ipc_objects_t" 'enumname)
	#.(my-lispify "XMMS_IPC_OBJECT_MAIN" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_OBJECT_PLAYLIST" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_OBJECT_CONFIG" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_OBJECT_OUTPUT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_OBJECT_MEDIALIB" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_OBJECT_COLLECTION" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_OBJECT_SIGNAL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_OBJECT_VISUALISATION" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_OBJECT_MEDIAINFO_READER" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_OBJECT_XFORM" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_OBJECT_BINDATA" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_OBJECT_END" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmms_ipc_objects_t" 'enumname))

(cffi:defcenum #.(my-lispify "xmms_ipc_signals_t" 'enumname)
	#.(my-lispify "XMMS_IPC_SIGNAL_OBJECT_DESTROYED" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_PLAYLIST_CHANGED" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_CONFIGVALUE_CHANGED" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_PLAYBACK_STATUS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_OUTPUT_VOLUME_CHANGED" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_OUTPUT_PLAYTIME" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_OUTPUT_CURRENTID" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_OUTPUT_OPEN_FAIL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_PLAYLIST_CURRENT_POS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_PLAYLIST_LOADED" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_MEDIALIB_ENTRY_ADDED" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_MEDIALIB_ENTRY_UPDATE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_COLLECTION_CHANGED" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_TRANSPORT_MIMETYPE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_DECODER_THREAD_EXIT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_VISUALISATION_DATA" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_QUIT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_MEDIAINFO_READER_STATUS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_MEDIAINFO_READER_UNINDEXED" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_SIGNAL_END" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmms_ipc_signals_t" 'enumname))

(cffi:defcenum #.(my-lispify "xmms_ipc_cmds_t" 'enumname)
	#.(my-lispify "XMMS_IPC_CMD_HELLO" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_QUIT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_REPLY" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_ERROR" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_PLUGIN_LIST" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_STATS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_SHUFFLE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_SET_POS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_SET_POS_REL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_ADD_URL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_ADD_ID" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_ADD_COLL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_REMOVE_ENTRY" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_MOVE_ENTRY" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_CLEAR" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_SORT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_LIST" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_CURRENT_POS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_CURRENT_ACTIVE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_INSERT_URL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_INSERT_ID" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_INSERT_COLL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_LOAD" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_RADD" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_GETVALUE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_SETVALUE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_REGVALUE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_LISTVALUES" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_START" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_STOP" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_PAUSE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_DECODER_KILL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_CPLAYTIME" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_SEEKMS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_SEEKMS_REL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_SEEKSAMPLES" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_SEEKSAMPLES_REL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_OUTPUT_STATUS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_CURRENTID" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_VOLUME_SET" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_VOLUME_GET" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_INFO" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_PATH_IMPORT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_REHASH" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_GET_ID" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_REMOVE_ID" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_PROPERTY_SET_STR" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_PROPERTY_SET_INT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_PROPERTY_REMOVE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_MOVE_URL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_COLLECTION_GET" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_COLLECTION_LIST" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_COLLECTION_SAVE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_COLLECTION_REMOVE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_COLLECTION_FIND" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_COLLECTION_RENAME" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_QUERY_IDS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_QUERY_INFOS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_IDLIST_FROM_PLS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_COLLECTION_SYNC" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_SIGNAL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_BROADCAST" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_BROWSE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_GET_DATA" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_ADD_DATA" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_REMOVE_DATA" 'enumvalue :keyword)
	#.(my-lispify "XMMS_IPC_CMD_END" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmms_ipc_cmds_t" 'enumname))

(cffi:defcenum #.(my-lispify "xmms_playlist_changed_actions_t" 'enumname)
	#.(my-lispify "XMMS_PLAYLIST_CHANGED_ADD" 'enumvalue :keyword)
	#.(my-lispify "XMMS_PLAYLIST_CHANGED_INSERT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_PLAYLIST_CHANGED_SHUFFLE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_PLAYLIST_CHANGED_REMOVE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_PLAYLIST_CHANGED_CLEAR" 'enumvalue :keyword)
	#.(my-lispify "XMMS_PLAYLIST_CHANGED_MOVE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_PLAYLIST_CHANGED_SORT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_PLAYLIST_CHANGED_UPDATE" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmms_playlist_changed_actions_t" 'enumname))

(cffi:defcenum #.(my-lispify "xmms_collection_changed_actions_t" 'enumname)
	#.(my-lispify "XMMS_COLLECTION_CHANGED_ADD" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_CHANGED_UPDATE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_CHANGED_RENAME" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_CHANGED_REMOVE" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmms_collection_changed_actions_t" 'enumname))

(cffi:defcenum #.(my-lispify "xmms_playback_status_t" 'enumname)
	#.(my-lispify "XMMS_PLAYBACK_STATUS_STOP" 'enumvalue :keyword)
	#.(my-lispify "XMMS_PLAYBACK_STATUS_PLAY" 'enumvalue :keyword)
	#.(my-lispify "XMMS_PLAYBACK_STATUS_PAUSE" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmms_playback_status_t" 'enumname))

(cffi:defcenum #.(my-lispify "xmms_mediainfo_reader_status_t" 'enumname)
	#.(my-lispify "XMMS_MEDIAINFO_READER_STATUS_IDLE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_MEDIAINFO_READER_STATUS_RUNNING" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmms_mediainfo_reader_status_t" 'enumname))

(cffi:defcenum #.(my-lispify "xmms_plugin_type_t" 'enumname)
	#.(my-lispify "XMMS_PLUGIN_TYPE_ALL" 'enumvalue :keyword)
	#.(my-lispify "XMMS_PLUGIN_TYPE_OUTPUT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_PLUGIN_TYPE_XFORM" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmms_plugin_type_t" 'enumname))

(cffi:defcenum #.(my-lispify "xmmsc_coll_type_t" 'enumname)
	#.(my-lispify "XMMS_COLLECTION_TYPE_REFERENCE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TYPE_UNION" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TYPE_INTERSECTION" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TYPE_COMPLEMENT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TYPE_HAS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TYPE_EQUALS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TYPE_MATCH" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TYPE_SMALLER" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TYPE_GREATER" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TYPE_IDLIST" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TYPE_QUEUE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TYPE_PARTYSHUFFLE" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmmsc_coll_type_t" 'enumname))

(cffi:defcenum #.(my-lispify "xmmsc_medialib_entry_status_t" 'enumname)
	#.(my-lispify "XMMS_MEDIALIB_ENTRY_STATUS_NEW" 'enumvalue :keyword)
	#.(my-lispify "XMMS_MEDIALIB_ENTRY_STATUS_OK" 'enumvalue :keyword)
	#.(my-lispify "XMMS_MEDIALIB_ENTRY_STATUS_RESOLVING" 'enumvalue :keyword)
	#.(my-lispify "XMMS_MEDIALIB_ENTRY_STATUS_NOT_AVAILABLE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_MEDIALIB_ENTRY_STATUS_REHASH" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmmsc_medialib_entry_status_t" 'enumname))

(cl:defconstant #.(my-lispify "XMMS_COLLECTION_NS_ALL" 'constant) "*")

(cl:export '#.(my-lispify "XMMS_COLLECTION_NS_ALL" 'constant))

(cl:defconstant #.(my-lispify "XMMS_COLLECTION_NS_COLLECTIONS" 'constant) "Collections")

(cl:export '#.(my-lispify "XMMS_COLLECTION_NS_COLLECTIONS" 'constant))

(cl:defconstant #.(my-lispify "XMMS_COLLECTION_NS_PLAYLISTS" 'constant) "Playlists")

(cl:export '#.(my-lispify "XMMS_COLLECTION_NS_PLAYLISTS" 'constant))

(cl:defconstant #.(my-lispify "XMMS_ACTIVE_PLAYLIST" 'constant) "_active")

(cl:export '#.(my-lispify "XMMS_ACTIVE_PLAYLIST" 'constant))

(cffi:defcenum #.(my-lispify "xmmsc_result_type_t" 'enumname)
	#.(my-lispify "XMMSC_RESULT_CLASS_DEFAULT" 'enumvalue :keyword)
	#.(my-lispify "XMMSC_RESULT_CLASS_SIGNAL" 'enumvalue :keyword)
	#.(my-lispify "XMMSC_RESULT_CLASS_BROADCAST" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmmsc_result_type_t" 'enumname))

(cffi:defcfun ("xmmsc_init" #.(my-lispify "xmmsc_init" 'function)) :pointer
  (clientname :string))

(cl:export '#.(my-lispify "xmmsc_init" 'function))

(cffi:defcfun ("xmmsc_connect" #.(my-lispify "xmmsc_connect" 'function)) :int
  (arg0 :pointer)
  (arg1 :string))

(cl:export '#.(my-lispify "xmmsc_connect" 'function))

(cffi:defcfun ("xmmsc_ref" #.(my-lispify "xmmsc_ref" 'function)) :void
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_ref" 'function))

(cffi:defcfun ("xmmsc_unref" #.(my-lispify "xmmsc_unref" 'function)) :void
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_unref" 'function))

(cffi:defcfun ("xmmsc_lock_set" #.(my-lispify "xmmsc_lock_set" 'function)) :void
  (conn :pointer)
  (lock :pointer)
  (lockfunc :pointer)
  (unlockfunc :pointer))

(cl:export '#.(my-lispify "xmmsc_lock_set" 'function))

(cffi:defcfun ("xmmsc_disconnect_callback_set" #.(my-lispify "xmmsc_disconnect_callback_set" 'function)) :void
  (c :pointer)
  (callback :pointer)
  (userdata :pointer))

(cl:export '#.(my-lispify "xmmsc_disconnect_callback_set" 'function))

(cffi:defcfun ("xmmsc_disconnect_callback_set_full" #.(my-lispify "xmmsc_disconnect_callback_set_full" 'function)) :void
  (c :pointer)
  (callback :pointer)
  (userdata :pointer)
  (free_func :pointer))

(cl:export '#.(my-lispify "xmmsc_disconnect_callback_set_full" 'function))

(cffi:defcfun ("xmmsc_io_need_out_callback_set" #.(my-lispify "xmmsc_io_need_out_callback_set" 'function)) :void
  (c :pointer)
  (callback :pointer)
  (userdata :pointer))

(cl:export '#.(my-lispify "xmmsc_io_need_out_callback_set" 'function))

(cffi:defcfun ("xmmsc_io_need_out_callback_set_full" #.(my-lispify "xmmsc_io_need_out_callback_set_full" 'function)) :void
  (c :pointer)
  (callback :pointer)
  (userdata :pointer)
  (free_func :pointer))

(cl:export '#.(my-lispify "xmmsc_io_need_out_callback_set_full" 'function))

(cffi:defcfun ("xmmsc_io_disconnect" #.(my-lispify "xmmsc_io_disconnect" 'function)) :void
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_io_disconnect" 'function))

(cffi:defcfun ("xmmsc_io_want_out" #.(my-lispify "xmmsc_io_want_out" 'function)) :int
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_io_want_out" 'function))

(cffi:defcfun ("xmmsc_io_out_handle" #.(my-lispify "xmmsc_io_out_handle" 'function)) :int
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_io_out_handle" 'function))

(cffi:defcfun ("xmmsc_io_in_handle" #.(my-lispify "xmmsc_io_in_handle" 'function)) :int
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_io_in_handle" 'function))

(cffi:defcfun ("xmmsc_io_fd_get" #.(my-lispify "xmmsc_io_fd_get" 'function)) :int
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_io_fd_get" 'function))

(cffi:defcfun ("xmmsc_get_last_error" #.(my-lispify "xmmsc_get_last_error" 'function)) :string
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_get_last_error" 'function))

(cffi:defcfun ("xmmsc_quit" #.(my-lispify "xmmsc_quit" 'function)) :pointer
  (arg0 :pointer))

(cl:export '#.(my-lispify "xmmsc_quit" 'function))

(cffi:defcfun ("xmmsc_broadcast_quit" #.(my-lispify "xmmsc_broadcast_quit" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_quit" 'function))

(cffi:defcfun ("xmmsc_userconfdir_get" #.(my-lispify "xmmsc_userconfdir_get" 'function)) :string
  (buf :string)
  (len :int))

(cl:export '#.(my-lispify "xmmsc_userconfdir_get" 'function))

(cffi:defcfun ("xmmsc_playlist_list" #.(my-lispify "xmmsc_playlist_list" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_playlist_list" 'function))

(cffi:defcfun ("xmmsc_playlist_create" #.(my-lispify "xmmsc_playlist_create" 'function)) :pointer
  (c :pointer)
  (playlist :string))

(cl:export '#.(my-lispify "xmmsc_playlist_create" 'function))

(cffi:defcfun ("xmmsc_playlist_shuffle" #.(my-lispify "xmmsc_playlist_shuffle" 'function)) :pointer
  (c :pointer)
  (playlist :string))

(cl:export '#.(my-lispify "xmmsc_playlist_shuffle" 'function))

(cffi:defcfun ("xmmsc_playlist_add_args" #.(my-lispify "xmmsc_playlist_add_args" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (arg2 :string)
  (arg3 :int)
  (arg4 :pointer))

(cl:export '#.(my-lispify "xmmsc_playlist_add_args" 'function))

(cffi:defcfun ("xmmsc_playlist_add_url" #.(my-lispify "xmmsc_playlist_add_url" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_playlist_add_url" 'function))

(cffi:defcfun ("xmmsc_playlist_add_id" #.(my-lispify "xmmsc_playlist_add_id" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (id :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_playlist_add_id" 'function))

(cffi:defcfun ("xmmsc_playlist_add_encoded" #.(my-lispify "xmmsc_playlist_add_encoded" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_playlist_add_encoded" 'function))

(cffi:defcfun ("xmmsc_playlist_add_collection" #.(my-lispify "xmmsc_playlist_add_collection" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (coll :pointer)
  (order :pointer))

(cl:export '#.(my-lispify "xmmsc_playlist_add_collection" 'function))

(cffi:defcfun ("xmmsc_playlist_remove_entry" #.(my-lispify "xmmsc_playlist_remove_entry" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (arg2 :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_playlist_remove_entry" 'function))

(cffi:defcfun ("xmmsc_playlist_clear" #.(my-lispify "xmmsc_playlist_clear" 'function)) :pointer
  (c :pointer)
  (playlist :string))

(cl:export '#.(my-lispify "xmmsc_playlist_clear" 'function))

(cffi:defcfun ("xmmsc_playlist_remove" #.(my-lispify "xmmsc_playlist_remove" 'function)) :pointer
  (c :pointer)
  (playlist :string))

(cl:export '#.(my-lispify "xmmsc_playlist_remove" 'function))

(cffi:defcfun ("xmmsc_playlist_list_entries" #.(my-lispify "xmmsc_playlist_list_entries" 'function)) :pointer
  (c :pointer)
  (playlist :string))

(cl:export '#.(my-lispify "xmmsc_playlist_list_entries" 'function))

(cffi:defcfun ("xmmsc_playlist_sort" #.(my-lispify "xmmsc_playlist_sort" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (properties :pointer))

(cl:export '#.(my-lispify "xmmsc_playlist_sort" 'function))

(cffi:defcfun ("xmmsc_playlist_set_next" #.(my-lispify "xmmsc_playlist_set_next" 'function)) :pointer
  (c :pointer)
  (arg1 :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_playlist_set_next" 'function))

(cffi:defcfun ("xmmsc_playlist_set_next_rel" #.(my-lispify "xmmsc_playlist_set_next_rel" 'function)) :pointer
  (c :pointer)
  (arg1 :int))

(cl:export '#.(my-lispify "xmmsc_playlist_set_next_rel" 'function))

(cffi:defcfun ("xmmsc_playlist_move_entry" #.(my-lispify "xmmsc_playlist_move_entry" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (arg2 :unsigned-int)
  (arg3 :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_playlist_move_entry" 'function))

(cffi:defcfun ("xmmsc_playlist_current_pos" #.(my-lispify "xmmsc_playlist_current_pos" 'function)) :pointer
  (c :pointer)
  (playlist :string))

(cl:export '#.(my-lispify "xmmsc_playlist_current_pos" 'function))

(cffi:defcfun ("xmmsc_playlist_current_active" #.(my-lispify "xmmsc_playlist_current_active" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_playlist_current_active" 'function))

(cffi:defcfun ("xmmsc_playlist_insert_args" #.(my-lispify "xmmsc_playlist_insert_args" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (pos :int)
  (url :string)
  (numargs :int)
  (args :pointer))

(cl:export '#.(my-lispify "xmmsc_playlist_insert_args" 'function))

(cffi:defcfun ("xmmsc_playlist_insert_url" #.(my-lispify "xmmsc_playlist_insert_url" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (pos :int)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_playlist_insert_url" 'function))

(cffi:defcfun ("xmmsc_playlist_insert_id" #.(my-lispify "xmmsc_playlist_insert_id" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (pos :int)
  (id :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_playlist_insert_id" 'function))

(cffi:defcfun ("xmmsc_playlist_insert_encoded" #.(my-lispify "xmmsc_playlist_insert_encoded" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (pos :int)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_playlist_insert_encoded" 'function))

(cffi:defcfun ("xmmsc_playlist_insert_collection" #.(my-lispify "xmmsc_playlist_insert_collection" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (pos :int)
  (coll :pointer)
  (order :pointer))

(cl:export '#.(my-lispify "xmmsc_playlist_insert_collection" 'function))

(cffi:defcfun ("xmmsc_playlist_load" #.(my-lispify "xmmsc_playlist_load" 'function)) :pointer
  (c :pointer)
  (playlist :string))

(cl:export '#.(my-lispify "xmmsc_playlist_load" 'function))

(cffi:defcfun ("xmmsc_playlist_radd" #.(my-lispify "xmmsc_playlist_radd" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_playlist_radd" 'function))

(cffi:defcfun ("xmmsc_playlist_radd_encoded" #.(my-lispify "xmmsc_playlist_radd_encoded" 'function)) :pointer
  (c :pointer)
  (playlist :string)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_playlist_radd_encoded" 'function))

(cffi:defcfun ("xmmsc_broadcast_playlist_changed" #.(my-lispify "xmmsc_broadcast_playlist_changed" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_playlist_changed" 'function))

(cffi:defcfun ("xmmsc_broadcast_playlist_current_pos" #.(my-lispify "xmmsc_broadcast_playlist_current_pos" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_playlist_current_pos" 'function))

(cffi:defcfun ("xmmsc_broadcast_playlist_loaded" #.(my-lispify "xmmsc_broadcast_playlist_loaded" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_playlist_loaded" 'function))

(cffi:defcfun ("xmmsc_playback_stop" #.(my-lispify "xmmsc_playback_stop" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_playback_stop" 'function))

(cffi:defcfun ("xmmsc_playback_tickle" #.(my-lispify "xmmsc_playback_tickle" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_playback_tickle" 'function))

(cffi:defcfun ("xmmsc_playback_start" #.(my-lispify "xmmsc_playback_start" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_playback_start" 'function))

(cffi:defcfun ("xmmsc_playback_pause" #.(my-lispify "xmmsc_playback_pause" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_playback_pause" 'function))

(cffi:defcfun ("xmmsc_playback_current_id" #.(my-lispify "xmmsc_playback_current_id" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_playback_current_id" 'function))

(cffi:defcfun ("xmmsc_playback_seek_ms" #.(my-lispify "xmmsc_playback_seek_ms" 'function)) :pointer
  (c :pointer)
  (milliseconds :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_playback_seek_ms" 'function))

(cffi:defcfun ("xmmsc_playback_seek_ms_rel" #.(my-lispify "xmmsc_playback_seek_ms_rel" 'function)) :pointer
  (c :pointer)
  (milliseconds :int))

(cl:export '#.(my-lispify "xmmsc_playback_seek_ms_rel" 'function))

(cffi:defcfun ("xmmsc_playback_seek_samples" #.(my-lispify "xmmsc_playback_seek_samples" 'function)) :pointer
  (c :pointer)
  (samples :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_playback_seek_samples" 'function))

(cffi:defcfun ("xmmsc_playback_seek_samples_rel" #.(my-lispify "xmmsc_playback_seek_samples_rel" 'function)) :pointer
  (c :pointer)
  (samples :int))

(cl:export '#.(my-lispify "xmmsc_playback_seek_samples_rel" 'function))

(cffi:defcfun ("xmmsc_playback_playtime" #.(my-lispify "xmmsc_playback_playtime" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_playback_playtime" 'function))

(cffi:defcfun ("xmmsc_playback_status" #.(my-lispify "xmmsc_playback_status" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_playback_status" 'function))

(cffi:defcfun ("xmmsc_playback_volume_set" #.(my-lispify "xmmsc_playback_volume_set" 'function)) :pointer
  (c :pointer)
  (channel :string)
  (volume :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_playback_volume_set" 'function))

(cffi:defcfun ("xmmsc_playback_volume_get" #.(my-lispify "xmmsc_playback_volume_get" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_playback_volume_get" 'function))

(cffi:defcfun ("xmmsc_broadcast_playback_volume_changed" #.(my-lispify "xmmsc_broadcast_playback_volume_changed" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_playback_volume_changed" 'function))

(cffi:defcfun ("xmmsc_broadcast_playback_status" #.(my-lispify "xmmsc_broadcast_playback_status" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_playback_status" 'function))

(cffi:defcfun ("xmmsc_broadcast_playback_current_id" #.(my-lispify "xmmsc_broadcast_playback_current_id" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_playback_current_id" 'function))

(cffi:defcfun ("xmmsc_signal_playback_playtime" #.(my-lispify "xmmsc_signal_playback_playtime" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_signal_playback_playtime" 'function))

(cffi:defcfun ("xmmsc_configval_set" #.(my-lispify "xmmsc_configval_set" 'function)) :pointer
  (c :pointer)
  (key :string)
  (val :string))

(cl:export '#.(my-lispify "xmmsc_configval_set" 'function))

(cffi:defcfun ("xmmsc_configval_list" #.(my-lispify "xmmsc_configval_list" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_configval_list" 'function))

(cffi:defcfun ("xmmsc_configval_get" #.(my-lispify "xmmsc_configval_get" 'function)) :pointer
  (c :pointer)
  (key :string))

(cl:export '#.(my-lispify "xmmsc_configval_get" 'function))

(cffi:defcfun ("xmmsc_configval_register" #.(my-lispify "xmmsc_configval_register" 'function)) :pointer
  (c :pointer)
  (valuename :string)
  (defaultvalue :string))

(cl:export '#.(my-lispify "xmmsc_configval_register" 'function))

(cffi:defcfun ("xmmsc_broadcast_configval_changed" #.(my-lispify "xmmsc_broadcast_configval_changed" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_configval_changed" 'function))

(cffi:defcfun ("xmmsc_plugin_list" #.(my-lispify "xmmsc_plugin_list" 'function)) :pointer
  (c :pointer)
  (type #.(my-lispify "xmms_plugin_type_t" 'enumname)))

(cl:export '#.(my-lispify "xmmsc_plugin_list" 'function))

(cffi:defcfun ("xmmsc_main_stats" #.(my-lispify "xmmsc_main_stats" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_main_stats" 'function))

(cffi:defcfun ("xmmsc_broadcast_mediainfo_reader_status" #.(my-lispify "xmmsc_broadcast_mediainfo_reader_status" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_mediainfo_reader_status" 'function))

(cffi:defcfun ("xmmsc_signal_visualisation_data" #.(my-lispify "xmmsc_signal_visualisation_data" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_signal_visualisation_data" 'function))

(cffi:defcfun ("xmmsc_signal_mediainfo_reader_unindexed" #.(my-lispify "xmmsc_signal_mediainfo_reader_unindexed" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_signal_mediainfo_reader_unindexed" 'function))

(cffi:defcfun ("xmmsc_entry_format" #.(my-lispify "xmmsc_entry_format" 'function)) :int
  (target :string)
  (len :int)
  (fmt :string)
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_entry_format" 'function))

(cffi:defcfun ("xmmsc_medialib_add_entry" #.(my-lispify "xmmsc_medialib_add_entry" 'function)) :pointer
  (conn :pointer)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_medialib_add_entry" 'function))

(cffi:defcfun ("xmmsc_medialib_add_entry_args" #.(my-lispify "xmmsc_medialib_add_entry_args" 'function)) :pointer
  (conn :pointer)
  (url :string)
  (numargs :int)
  (args :pointer))

(cl:export '#.(my-lispify "xmmsc_medialib_add_entry_args" 'function))

(cffi:defcfun ("xmmsc_medialib_add_entry_encoded" #.(my-lispify "xmmsc_medialib_add_entry_encoded" 'function)) :pointer
  (conn :pointer)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_medialib_add_entry_encoded" 'function))

(cffi:defcfun ("xmmsc_medialib_get_info" #.(my-lispify "xmmsc_medialib_get_info" 'function)) :pointer
  (arg0 :pointer)
  (arg1 :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_medialib_get_info" 'function))

(cffi:defcfun ("xmmsc_medialib_path_import" #.(my-lispify "xmmsc_medialib_path_import" 'function)) :pointer
  (conn :pointer)
  (path :string))

(cl:export '#.(my-lispify "xmmsc_medialib_path_import" 'function))

(cffi:defcfun ("xmmsc_medialib_path_import_encoded" #.(my-lispify "xmmsc_medialib_path_import_encoded" 'function)) :pointer
  (conn :pointer)
  (path :string))

(cl:export '#.(my-lispify "xmmsc_medialib_path_import_encoded" 'function))

(cffi:defcfun ("xmmsc_medialib_rehash" #.(my-lispify "xmmsc_medialib_rehash" 'function)) :pointer
  (conn :pointer)
  (id :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_medialib_rehash" 'function))

(cffi:defcfun ("xmmsc_medialib_get_id" #.(my-lispify "xmmsc_medialib_get_id" 'function)) :pointer
  (conn :pointer)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_medialib_get_id" 'function))

(cffi:defcfun ("xmmsc_medialib_remove_entry" #.(my-lispify "xmmsc_medialib_remove_entry" 'function)) :pointer
  (conn :pointer)
  (entry :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_medialib_remove_entry" 'function))

(cffi:defcfun ("xmmsc_medialib_move_entry" #.(my-lispify "xmmsc_medialib_move_entry" 'function)) :pointer
  (conn :pointer)
  (entry :unsigned-int)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_medialib_move_entry" 'function))

(cffi:defcfun ("xmmsc_medialib_entry_property_set_int" #.(my-lispify "xmmsc_medialib_entry_property_set_int" 'function)) :pointer
  (c :pointer)
  (id :unsigned-int)
  (key :string)
  (value :int))

(cl:export '#.(my-lispify "xmmsc_medialib_entry_property_set_int" 'function))

(cffi:defcfun ("xmmsc_medialib_entry_property_set_int_with_source" #.(my-lispify "xmmsc_medialib_entry_property_set_int_with_source" 'function)) :pointer
  (c :pointer)
  (id :unsigned-int)
  (source :string)
  (key :string)
  (value :int))

(cl:export '#.(my-lispify "xmmsc_medialib_entry_property_set_int_with_source" 'function))

(cffi:defcfun ("xmmsc_medialib_entry_property_set_str" #.(my-lispify "xmmsc_medialib_entry_property_set_str" 'function)) :pointer
  (c :pointer)
  (id :unsigned-int)
  (key :string)
  (value :string))

(cl:export '#.(my-lispify "xmmsc_medialib_entry_property_set_str" 'function))

(cffi:defcfun ("xmmsc_medialib_entry_property_set_str_with_source" #.(my-lispify "xmmsc_medialib_entry_property_set_str_with_source" 'function)) :pointer
  (c :pointer)
  (id :unsigned-int)
  (source :string)
  (key :string)
  (value :string))

(cl:export '#.(my-lispify "xmmsc_medialib_entry_property_set_str_with_source" 'function))

(cffi:defcfun ("xmmsc_medialib_entry_property_remove" #.(my-lispify "xmmsc_medialib_entry_property_remove" 'function)) :pointer
  (c :pointer)
  (id :unsigned-int)
  (key :string))

(cl:export '#.(my-lispify "xmmsc_medialib_entry_property_remove" 'function))

(cffi:defcfun ("xmmsc_medialib_entry_property_remove_with_source" #.(my-lispify "xmmsc_medialib_entry_property_remove_with_source" 'function)) :pointer
  (c :pointer)
  (id :unsigned-int)
  (source :string)
  (key :string))

(cl:export '#.(my-lispify "xmmsc_medialib_entry_property_remove_with_source" 'function))

(cffi:defcfun ("xmmsc_xform_media_browse" #.(my-lispify "xmmsc_xform_media_browse" 'function)) :pointer
  (c :pointer)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_xform_media_browse" 'function))

(cffi:defcfun ("xmmsc_xform_media_browse_encoded" #.(my-lispify "xmmsc_xform_media_browse_encoded" 'function)) :pointer
  (c :pointer)
  (url :string))

(cl:export '#.(my-lispify "xmmsc_xform_media_browse_encoded" 'function))

(cffi:defcfun ("xmmsc_bindata_add" #.(my-lispify "xmmsc_bindata_add" 'function)) :pointer
  (c :pointer)
  (data :pointer)
  (len :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_bindata_add" 'function))

(cffi:defcfun ("xmmsc_bindata_retrieve" #.(my-lispify "xmmsc_bindata_retrieve" 'function)) :pointer
  (c :pointer)
  (hash :string))

(cl:export '#.(my-lispify "xmmsc_bindata_retrieve" 'function))

(cffi:defcfun ("xmmsc_bindata_remove" #.(my-lispify "xmmsc_bindata_remove" 'function)) :pointer
  (c :pointer)
  (hash :string))

(cl:export '#.(my-lispify "xmmsc_bindata_remove" 'function))

(cffi:defcfun ("xmmsc_broadcast_medialib_entry_changed" #.(my-lispify "xmmsc_broadcast_medialib_entry_changed" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_medialib_entry_changed" 'function))

(cffi:defcfun ("xmmsc_broadcast_medialib_entry_added" #.(my-lispify "xmmsc_broadcast_medialib_entry_added" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_medialib_entry_added" 'function))

(cffi:defcfun ("xmmsc_coll_get" #.(my-lispify "xmmsc_coll_get" 'function)) :pointer
  (conn :pointer)
  (collname :string)
  (ns :string))

(cl:export '#.(my-lispify "xmmsc_coll_get" 'function))

(cffi:defcfun ("xmmsc_coll_list" #.(my-lispify "xmmsc_coll_list" 'function)) :pointer
  (conn :pointer)
  (ns :string))

(cl:export '#.(my-lispify "xmmsc_coll_list" 'function))

(cffi:defcfun ("xmmsc_coll_save" #.(my-lispify "xmmsc_coll_save" 'function)) :pointer
  (conn :pointer)
  (coll :pointer)
  (name :string)
  (ns :string))

(cl:export '#.(my-lispify "xmmsc_coll_save" 'function))

(cffi:defcfun ("xmmsc_coll_remove" #.(my-lispify "xmmsc_coll_remove" 'function)) :pointer
  (conn :pointer)
  (name :string)
  (ns :string))

(cl:export '#.(my-lispify "xmmsc_coll_remove" 'function))

(cffi:defcfun ("xmmsc_coll_find" #.(my-lispify "xmmsc_coll_find" 'function)) :pointer
  (conn :pointer)
  (mediaid :unsigned-int)
  (ns :string))

(cl:export '#.(my-lispify "xmmsc_coll_find" 'function))

(cffi:defcfun ("xmmsc_coll_rename" #.(my-lispify "xmmsc_coll_rename" 'function)) :pointer
  (conn :pointer)
  (from_name :string)
  (to_name :string)
  (ns :string))

(cl:export '#.(my-lispify "xmmsc_coll_rename" 'function))

(cffi:defcfun ("xmmsc_coll_idlist_from_playlist_file" #.(my-lispify "xmmsc_coll_idlist_from_playlist_file" 'function)) :pointer
  (conn :pointer)
  (path :string))

(cl:export '#.(my-lispify "xmmsc_coll_idlist_from_playlist_file" 'function))

(cffi:defcfun ("xmmsc_coll_sync" #.(my-lispify "xmmsc_coll_sync" 'function)) :pointer
  (conn :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_sync" 'function))

(cffi:defcfun ("xmmsc_coll_query_ids" #.(my-lispify "xmmsc_coll_query_ids" 'function)) :pointer
  (conn :pointer)
  (coll :pointer)
  (order :pointer)
  (limit_start :unsigned-int)
  (limit_len :unsigned-int))

(cl:export '#.(my-lispify "xmmsc_coll_query_ids" 'function))

(cffi:defcfun ("xmmsc_coll_query_infos" #.(my-lispify "xmmsc_coll_query_infos" 'function)) :pointer
  (conn :pointer)
  (coll :pointer)
  (order :pointer)
  (limit_start :unsigned-int)
  (limit_len :unsigned-int)
  (fetch :pointer)
  (group :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_query_infos" 'function))

(cffi:defcenum #.(my-lispify "xmmsc_coll_token_type_t" 'enumname)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_INVALID" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_GROUP_OPEN" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_GROUP_CLOSE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_REFERENCE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_SYMBOL_ID" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_STRING" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_PATTERN" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_INTEGER" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_SEQUENCE" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_PROP_LONG" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_PROP_SHORT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_OPSET_UNION" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_OPSET_INTERSECTION" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_OPSET_COMPLEMENT" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_OPFIL_HAS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_OPFIL_EQUALS" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_OPFIL_MATCH" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_OPFIL_SMALLER" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_OPFIL_GREATER" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_OPFIL_SMALLEREQ" 'enumvalue :keyword)
	#.(my-lispify "XMMS_COLLECTION_TOKEN_OPFIL_GREATEREQ" 'enumvalue :keyword))

(cl:export '#.(my-lispify "xmmsc_coll_token_type_t" 'enumname))

(cl:defconstant #.(my-lispify "XMMS_COLLECTION_TOKEN_CUSTOM" 'constant) 32)

(cl:export '#.(my-lispify "XMMS_COLLECTION_TOKEN_CUSTOM" 'constant))

(cffi:defcstruct #.(my-lispify "xmmsc_coll_token_St" 'classname)
	(#.(my-lispify "type" 'slotname) #.(my-lispify "xmmsc_coll_token_type_t" 'enumname))
	(#.(my-lispify "string" 'slotname) :string)
	(#.(my-lispify "next" 'slotname) :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_token_St" 'classname))

(cl:export '#.(my-lispify "type" 'slotname))

(cl:export '#.(my-lispify "string" 'slotname))

(cl:export '#.(my-lispify "next" 'slotname))

(cffi:defcfun ("xmmsc_coll_parse" #.(my-lispify "xmmsc_coll_parse" 'function)) :int
  (pattern :string)
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_parse" 'function))

(cffi:defcfun ("xmmsc_coll_parse_custom" #.(my-lispify "xmmsc_coll_parse_custom" 'function)) :int
  (pattern :string)
  (parse_f :pointer)
  (build_f :pointer)
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_parse_custom" 'function))

(cffi:defcfun ("xmmsc_coll_default_parse_build" #.(my-lispify "xmmsc_coll_default_parse_build" 'function)) :pointer
  (tokens :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_default_parse_build" 'function))

(cffi:defcfun ("xmmsc_coll_default_parse_tokens" #.(my-lispify "xmmsc_coll_default_parse_tokens" 'function)) :pointer
  (str :string)
  (newpos :pointer))

(cl:export '#.(my-lispify "xmmsc_coll_default_parse_tokens" 'function))

(cffi:defcfun ("xmmsc_broadcast_collection_changed" #.(my-lispify "xmmsc_broadcast_collection_changed" 'function)) :pointer
  (c :pointer))

(cl:export '#.(my-lispify "xmmsc_broadcast_collection_changed" 'function))

(cffi:defcfun ("xmmsc_result_restart" #.(my-lispify "xmmsc_result_restart" 'function)) :pointer
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_restart" 'function))

(cffi:defcfun ("xmmsc_result_run" #.(my-lispify "xmmsc_result_run" 'function)) :void
  (res :pointer)
  (msg :pointer))

(cl:export '#.(my-lispify "xmmsc_result_run" 'function))

(cffi:defcfun ("xmmsc_result_get_class" #.(my-lispify "xmmsc_result_get_class" 'function)) #.(my-lispify "xmmsc_result_type_t" 'enumname)
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_class" 'function))

(cffi:defcfun ("xmmsc_result_disconnect" #.(my-lispify "xmmsc_result_disconnect" 'function)) :void
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_disconnect" 'function))

(cffi:defcfun ("xmmsc_result_ref" #.(my-lispify "xmmsc_result_ref" 'function)) :void
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_ref" 'function))

(cffi:defcfun ("xmmsc_result_unref" #.(my-lispify "xmmsc_result_unref" 'function)) :void
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_unref" 'function))

(cffi:defcfun ("xmmsc_result_notifier_set" #.(my-lispify "xmmsc_result_notifier_set" 'function)) :void
  (res :pointer)
  (func :pointer)
  (user_data :pointer))

(cl:export '#.(my-lispify "xmmsc_result_notifier_set" 'function))

(cffi:defcfun ("xmmsc_result_notifier_set_full" #.(my-lispify "xmmsc_result_notifier_set_full" 'function)) :void
  (res :pointer)
  (func :pointer)
  (user_data :pointer)
  (free_func :pointer))

(cl:export '#.(my-lispify "xmmsc_result_notifier_set_full" 'function))

(cffi:defcfun ("xmmsc_result_wait" #.(my-lispify "xmmsc_result_wait" 'function)) :void
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_wait" 'function))

(cffi:defcfun ("xmmsc_result_iserror" #.(my-lispify "xmmsc_result_iserror" 'function)) :int
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_iserror" 'function))

(cffi:defcfun ("xmmsc_result_get_error" #.(my-lispify "xmmsc_result_get_error" 'function)) :string
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_error" 'function))

(cffi:defcfun ("xmmsc_result_get_int" #.(my-lispify "xmmsc_result_get_int" 'function)) :int
  (res :pointer)
  (r :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_int" 'function))

(cffi:defcfun ("xmmsc_result_get_uint" #.(my-lispify "xmmsc_result_get_uint" 'function)) :int
  (res :pointer)
  (r :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_uint" 'function))

(cffi:defcfun ("xmmsc_result_get_string" #.(my-lispify "xmmsc_result_get_string" 'function)) :int
  (res :pointer)
  (r :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_string" 'function))

(cffi:defcfun ("xmmsc_result_get_collection" #.(my-lispify "xmmsc_result_get_collection" 'function)) :int
  (conn :pointer)
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_collection" 'function))

(cffi:defcfun ("xmmsc_result_get_bin" #.(my-lispify "xmmsc_result_get_bin" 'function)) :int
  (res :pointer)
  (r :pointer)
  (rlen :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_bin" 'function))

(cffi:defcfun ("xmmsc_result_get_dict_entry_type" #.(my-lispify "xmmsc_result_get_dict_entry_type" 'function)) :int
  (res :pointer)
  (key :string))

(cl:export '#.(my-lispify "xmmsc_result_get_dict_entry_type" 'function))

(cffi:defcfun ("xmmsc_result_get_dict_entry_string" #.(my-lispify "xmmsc_result_get_dict_entry_string" 'function)) :int
  (res :pointer)
  (key :string)
  (r :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_dict_entry_string" 'function))

(cffi:defcfun ("xmmsc_result_get_dict_entry_int" #.(my-lispify "xmmsc_result_get_dict_entry_int" 'function)) :int
  (res :pointer)
  (key :string)
  (r :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_dict_entry_int" 'function))

(cffi:defcfun ("xmmsc_result_get_dict_entry_uint" #.(my-lispify "xmmsc_result_get_dict_entry_uint" 'function)) :int
  (res :pointer)
  (key :string)
  (r :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_dict_entry_uint" 'function))

(cffi:defcfun ("xmmsc_result_get_dict_entry_collection" #.(my-lispify "xmmsc_result_get_dict_entry_collection" 'function)) :int
  (conn :pointer)
  (key :string)
  (coll :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_dict_entry_collection" 'function))

(cffi:defcfun ("xmmsc_result_dict_foreach" #.(my-lispify "xmmsc_result_dict_foreach" 'function)) :int
  (res :pointer)
  (func :pointer)
  (user_data :pointer))

(cl:export '#.(my-lispify "xmmsc_result_dict_foreach" 'function))

(cffi:defcfun ("xmmsc_result_propdict_foreach" #.(my-lispify "xmmsc_result_propdict_foreach" 'function)) :int
  (res :pointer)
  (func :pointer)
  (user_data :pointer))

(cl:export '#.(my-lispify "xmmsc_result_propdict_foreach" 'function))

(cffi:defcfun ("xmmsc_result_source_preference_set" #.(my-lispify "xmmsc_result_source_preference_set" 'function)) :void
  (res :pointer)
  (preference :pointer))

(cl:export '#.(my-lispify "xmmsc_result_source_preference_set" 'function))

(cffi:defcfun ("xmmsc_result_source_preference_get" #.(my-lispify "xmmsc_result_source_preference_get" 'function)) :pointer
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_source_preference_get" 'function))

(cffi:defcfun ("xmmsc_result_is_list" #.(my-lispify "xmmsc_result_is_list" 'function)) :int
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_is_list" 'function))

(cffi:defcfun ("xmmsc_result_list_next" #.(my-lispify "xmmsc_result_list_next" 'function)) :int
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_list_next" 'function))

(cffi:defcfun ("xmmsc_result_list_first" #.(my-lispify "xmmsc_result_list_first" 'function)) :int
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_list_first" 'function))

(cffi:defcfun ("xmmsc_result_list_valid" #.(my-lispify "xmmsc_result_list_valid" 'function)) :int
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_list_valid" 'function))

(cffi:defcfun ("xmmsc_result_get_type" #.(my-lispify "xmmsc_result_get_type" 'function)) :int
  (res :pointer))

(cl:export '#.(my-lispify "xmmsc_result_get_type" 'function))

(cffi:defcfun ("xmmsc_result_decode_url" #.(my-lispify "xmmsc_result_decode_url" 'function)) :string
  (res :pointer)
  (string :string))

(cl:export '#.(my-lispify "xmmsc_result_decode_url" 'function))


