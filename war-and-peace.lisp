;; Step 1: Read files
(defun read-file (filename)
  "Read `FILENAME' and return it as a list of lines."
  (uiop:read-file-lines filename))

(defun read-book ()
  (read-file "war-and-peace"))
(defun peace-terms ()
  (read-file "peace-terms"))
(defun war-terms ()
  (read-file "war-terms"))

;; Step 2: Tokenize text
(defun tokenize-line (line)
  "Splits `LINE' on whitespace and removes punctuation and empty strings."
  (remove-if #'str:blankp (mapcar
			   #'str:remove-punctuation
			   (str:words line))))

(defun tokenize-text (text)
  "Splits `TEXT' into lists of words corresponding to the lines from the
text."
  (remove nil (mapcar #'tokenize-line text)))

(defun tokenized-book ()
  (tokenize-text (read-book)))


;; Step 3: Filter words
(defun filter-words (word-list target-words)
  "Filters words from a list based on another list (case insensitive)."
  (remove-if-not (lambda (word)
                   (member word target-words
                           :test #'string-equal))
                 word-list))

;; Auxiliary functions
(defun write-to-file (content filename)
  "Write `CONTENT' to `FILENAME', overwriting if necessary."
  (with-open-file
      (stream filename
	      :direction :output    ;; Write to disk
	      :if-exists :supersede ;; Overwrite the file
	      :if-does-not-exist :create)
    (format stream "~S" content)))

;; Program entry point
(defun categorize-book ()
  (write-to-file
   (tokenized-book)
   "categorization"))

;; uncomment for interactive use
(defvar *book* (tokenized-book))
