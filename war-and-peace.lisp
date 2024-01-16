;; Step 1: Read files
;; ----------------------------------------------------
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
;; ----------------------------------------------------
(defun tokenize-line (line)
  "Splits `LINE' on whitespace and removes punctuation and empty strings."
  (remove-if #'str:blankp
             (mapcar #'str:remove-punctuation
			  (str:words line))))

(defun tokenize-text (text)
  "Splits `TEXT' into lists of words corresponding to the lines from the
text."
  (remove nil (mapcar #'tokenize-line text)))

(defun tokenized-book ()
  (tokenize-text (read-book)))

(defun trimmed-book ()
  "Remove the parts of the file that don't belong to a chapter."
  (let ((book (tokenized-book)))
    (reverse (set-difference
     (member '("CHAPTER" "1") book :test 'equal)
     (member '("END" "OF" "THE" "PROJECT" "GUTENBERG" "EBOOK" "WAR" "AND" "PEACE") book :test 'equal)))))

(defun is-chapterp (line)
  "A chapter starts with CHAPTER (case sensitive)."
  (equal "CHAPTER" (first line)))

;; Step 3: Split by chapter
;; ----------------------------------------------------
(defun split-chapters (book)
  "Splits `BOOK' into chapters. The return value is a list of lists of
strings; each inner list is the content of a chapter split into words."
  (let ((result ())
        (current-chapter ()))
    (loop for line in book
          do (if (is-chapterp line)
                 (progn
                   (when current-chapter (push current-chapter result))
                   (setf current-chapter line))
                 (setf current-chapter (append current-chapter line))))
    (push current-chapter result)
    (reverse result)))

;; Filter words
(defun filter-words (word-list target-words)
  "Filters words from a list based on another list (case insensitive)."
  (remove-if-not (lambda (word)
                   (member word target-words
                           :test #'string-equal))
                 word-list))

;; File output
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
   (split-chapters (trimmed-book))
   "categorization"))

;; uncomment for interactive use
(defvar *book* (trimmed-book))
