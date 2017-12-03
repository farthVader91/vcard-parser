(asdf:defsystem #:vcard-parser
  :description "vCard parser is meant to parse .vcf format files and supports features like exporting, merging
by criteria, etc. "
  :author "Vishal Gowda"
  :depends-on (
               :cl-ppcre
               :cl-json)
  :serial t
  :components ((:file "package")
               (:file "vcard-parser")))
