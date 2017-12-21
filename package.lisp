(defpackage #:vcard-parser
  (:nicknames #:vcparser)
  (:use :cl :cl-ppcre :cl-json :cl-csv)
  (:export #:parse-vcf :vcard))
