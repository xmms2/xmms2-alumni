(defsystem xmms2
  :name "xmms2"
  :components
  ((:file "package")
   (:file "xmmsc" :depends-on ("package"))
   (:file "xmmsc-swig" :depends-on ("xmmsc"))
   (:file "xmms2" :depends-on ("xmmsc-swig")))
  :depends-on (:cffi))
