(in-package :cl-user)

(defpackage #:vcard-parser
  (:nicknames #:vcparser)
  (:use :cl :cl-user)
  (:export #:parse-vcf :vcard))

(in-package :vcard-parser)

(ql:quickload "cl-ppcre")

(defconstant +block-start-pattern+ "BEGIN:VCARD")
(defconstant +block-end-pattern+ "END:VCARD")

(defun vcard-startp (line)
  (string= +block-start-pattern+ line))

(defun vcard-endp (line)
  (string= +block-end-pattern+ line))

(defclass vcard ()
  ((tel
    :initarg tel
    :initform nil
    :accessor tel)
   (fn
    :initarg fn
    :initform ""
    :accessor fn)
   (n
    :initarg n
    :initform ""
    :accessor n)))

(defun extract-fn (line)
  (multiple-value-bind (string matchp) (cl-ppcre:regex-replace "FN:" line "")
    (when matchp
      string)))

(defun tel-p (line)
  (and
   (>= (length line) 4)
   (string= "TEL;" (subseq line 0 4))))

(defun fn-p (line)
  (and
   (>= (length line) 3)
   (string= "FN:" (subseq line 0 3))))

(defun n-p (line)
  (and
   (>= (length line) 2)
   (string= "N:" (subseq line 0 2))))

(defun extract-n (line)
  (multiple-value-bind (string matchp) (cl-ppcre:regex-replace "N:" line "")
    (when matchp
      string)))

(defun extract-tel (line)
  (multiple-value-bind (orig matches) (cl-ppcre:scan-to-strings (cl-ppcre:create-scanner "^TEL;.*:(\\+?\\d+)$") line)
    (when (and matches (> (length matches) 0))
      (aref matches 0))))

(defun process-vcard (lines)
  (let ((vc (make-instance 'vcard)))
    (mapc #'(lambda (line) (cond
                             ((tel-p line) (setf (tel vc) (push (extract-tel line) (tel vc))))
                             ((fn-p line) (setf (fn vc) (extract-fn line)))
                             ((n-p line) (setf (n vc) (extract-n line))))) lines) vc))

(defun parse-vcf (file)
  (with-open-file (in file)
    (loop with vcard = nil
       for line = (read-line in nil)
       until (eq line nil)
       if (vcard-startp line) do (setf vcard nil)
       else if (vcard-endp line) collect (process-vcard vcard) into vcards
       else do (push line vcard)
       finally (return vcards))))

(defmethod print-object ((obj vcard) out)
           (print-unreadable-object (obj out :type t)
             (format out "~s" (n obj))))
