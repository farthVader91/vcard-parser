(in-package :vcard-parser)

(defconstant +block-start-pattern+ "BEGIN:VCARD")
(defconstant +block-end-pattern+ "END:VCARD")
(defconstant +export-formats+ '(json csv))

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
  (multiple-value-bind (orig matches) (cl-ppcre:scan-to-strings
                                       (cl-ppcre:create-scanner "^TEL;.*:(\\+?\\d+)$") line)
    (when (and matches (> (length matches) 0))
      (aref matches 0))))

(defun make-vcard (lines)
  (let ((vc (make-instance 'vcard)))
    (mapc #'(lambda (line) (cond
                             ((tel-p line) (setf (tel vc) (push (extract-tel line) (tel vc))))
                             ((fn-p line) (setf (fn vc) (extract-fn line)))
                             ((n-p line) (setf (n vc) (extract-n line))))) lines) vc))

(defun parse-vcf (file)
  (with-open-file (in file)
    (loop with lines = nil and vcards = nil
       for line = (read-line in nil) until (eq line nil)
       if (vcard-startp line) do (setf lines nil)
       else if (vcard-endp line) do (let ((vcard (handler-case (make-vcard lines)
                                                   (cl-ppcre:ppcre-syntax-error () nil))))
                                      (when vcard (push vcard vcards)))
       else do (push line lines)
       finally (return vcards))))

(define-condition unsupported-export-format (error)
  ((text :initarg :text :reader text)))

(defun export-json (vcards outfile)
  (with-open-file (out outfile :direction :output :if-exists :supersede)
    (encode-json vcards out) ))

(defun export-csv (vcards outfile)
  (with-open-file (out outfile :direction :output :if-exists :supersede)
    (progn
      (format out "n,fn,tel~%")
      (loop for vcard in vcards
         do (write-csv-row (with-slots (n fn tel) vcard
                             (list n fn tel)) :stream out)))))

(defun export (vcards &key fmt (outfile (format nil "exported.~(~a~)" fmt)))
  (cond ((string= (symbol-name fmt) "JSON") (export-json vcards outfile))
        ((string= (symbol-name fmt) "CSV") (export-csv vcards outfile))
        (t (error 'unsupported-export-format :text (format nil "~a is not a supported format" fmt)))))
