;;;; lisp-cond-restarts.lisp

(in-package :lisp-cond-restarts)


(defun gen-data-file (out-file-name &key (num-lines 100) (fail-prob 0.1))
  "Create a temporary data file with three columns of dummy data. With some
failure probability (`fail-prob') the second column will be missing in some
lines."
  (let* ((ascii-code-a  97)
         (alphabet-size 26)
         (num-to-char   (lambda (n) (code-char (+ ascii-code-a n))))
         (fail-pos      (round (* fail-prob alphabet-size))))
    (with-open-file (out out-file-name :direction :output
                                       :if-exists :supersede)
      (loop for i from 1 upto num-lines
            for x = (random alphabet-size) then (random alphabet-size)
            do (format out "~3d ~C ~d~%"
                       i (if (< x fail-pos) #\Space (funcall num-to-char x)) x)))))


(defclass log-entry ()
  ((line-num
    :initarg :line-num
    :initform (error "Must provide a line number.")
    :reader line-num
    :documentation "Line number")
   (status-char
    :initarg :status-char
    :initform (error "Must provide a status character.")
    :reader status-char
    :documentation "Status character")
   (status-num
    :initarg :status-num
    :initform (error "Must provide a status number.")
    :reader status-num
    :documentation "Status code or number")))


(define-condition malformed-log-entry-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "Attempt to parse entry '~a' with wrong number of fields.~&"
                     (text condition)))))


(defun parse-log-entry (line)
  "Parse a given line into three fields."
  (let ((fields (ppcre:split "\\s+" (string-trim '(#\Space #\t) line)))
        (slot-names '(:line-num :status-char :status-num)))
    (if (= (length slot-names) (length fields))
        (apply #'make-instance
               'log-entry
               (reduce #'append
                       (mapcar (lambda (x y) `(,x ,y)) slot-names fields)))
        (restart-case (error 'malformed-log-entry-error :text line)
          (use-value (value) value)
          (reparse-entry (fixed-text) (parse-log-entry fixed-text))))))


(defun parse-log-file (file)
  "Parse log entried in a file."
  (with-open-file (log-input file :direction :input)
    (loop for text = (read-line log-input nil nil) while text
          for entry = (restart-case (parse-log-entry text)
                        (skip-log-entry () nil))
          when entry collect it)))


(defun analyze-entry (entry)
  "Extract status code from entry."
  (status-char entry))


(defun skip-log-entry (c)
  "Restart using `skip-log-entry'."
  (declare (ignore c))
  (let ((restart (find-restart 'skip-log-entry)))
    (when restart (invoke-restart restart))))


(defparameter malformed-log-entry
  (make-instance 'log-entry
                 :line-num 0
                 :status-char #\*
                 :status-num 0))


(defun use-malformed-value (c)
  "Restart using `use-value'."
  (declare (ignore c))
  ;; (use-value (make-instance 'log-entry :line-num 0 :status-char #\* :status-num 0)))
  (let ((restart (find-restart 'use-value)))
    (when restart
        (invoke-restart restart malformed-log-entry))))


(defun analyze-log (file)
  "Parse log file and print status characters."
  (let ((status-chars (make-hash-set)))
    ;; (handler-bind ((malformed-log-entry-error #'skip-log-entry))
    (handler-bind ((malformed-log-entry-error #'use-malformed-value))
      (loop for entry in (parse-log-file file)
            do (hs-ninsert status-chars (analyze-entry entry))))
    (dolist (elt (hs-to-list status-chars))
      (format t "~a " elt))))


(defun log-analyzer ()
  "Generate a dummy log file and analyze it."
    (let ((log-file "/tmp/sample.txt"))
      (gen-data-file log-file)
      (analyze-log log-file)))
