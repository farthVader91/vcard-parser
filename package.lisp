(defpackage #:vcard-parser
  (:nicknames #:vcparser)
  (:use :cl :cl-ppcre :cl-json)
  (:export #:parse-vcf :vcard))
