;; Step 1: Read files
;; ----------------------------------------------------
(defun read-file (filename)
  "Read `FILENAME' and return it as a list of lines."
  (uiop:read-file-lines filename))

(defun read-book ()
  (read-file "war-and-peace"))
(defun read-peace-terms ()
  (read-file "peace-terms"))
(defun read-war-terms ()
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
  "Read in the book and return it as a list of lists of strings."
  (tokenize-text (read-book)))

(defun trimmed-book ()
  "Remove the parts of the file that don't belong to a chapter."
  (let ((book (tokenized-book)))
    (reverse (set-difference
     (member '("CHAPTER" "1") book :test 'equal)
     (member '("END" "OF" "THE" "PROJECT" "GUTENBERG" "EBOOK" "WAR" "AND" "PEACE") book :test 'equal)))))

;; Step 3: Split by chapter
;; ----------------------------------------------------
(defun is-chapterp (line)
  "A chapter starts with CHAPTER (case sensitive)."
  (equal "CHAPTER" (first line)))

(defun split-chapters (book)
  "Splits `BOOK' into chapters. The return value is a list of lists of
strings; each inner list is the content of a chapter split into words."
  (let ((result ())
        (current-chapter ()))
    (loop for line in book
          do (if (is-chapterp line)
                 (progn
                   (when current-chapter
                     (push current-chapter result))
                   (setf current-chapter line))
                 (setf current-chapter (append current-chapter line))))
    (push current-chapter result)
    (reverse result)))

;; Step 4: Categorize Chapters
;; ----------------------------------------------------
;; Filter words
(defun filter-words (terms chapter)
  "Filters words from a list based on another list (case insensitive)."
  (remove-if-not (lambda (word)
                   (member word terms
                           :test #'string-equal))
                 chapter))

(defun categorize-chapter (chapter war peace)
  "Categorize a chapter as war related or peace related."
  (cond ((> (length (filter-words war chapter))
            (length (filter-words peace chapter)))
         'war)
        (t 'peace)))

(defun categorize-book ()
  "Reads in the book, the war terms, and the peace tearm. Then maps the
categorization function over each chapter."
  (let ((book (split-chapters (trimmed-book)))
        (war (read-war-terms))
        (peace (read-peace-terms)))
    ;; TODO multithreaded mapcar
    (mapcar (lambda (chapter) (categorize-chapter chapter war peace)) book)))

;; Step 5: Output and Utility Functions
;; ----------------------------------------------------
;; File output
(defun write-to-file (content filename)
  "Write `CONTENT' to `FILENAME', overwriting if necessary."
  (with-open-file
      (stream filename
	      :direction :output    ;; Write to disk
	      :if-exists :supersede ;; Overwrite the file
	      :if-does-not-exist :create)
    (format stream "~S" content)))

(defun related-string (item)
  "Return war-related if `item' is war, else peace-related."
  (if (equal item 'war)
      "war-related"
      "peace-related"))

;; Program entry point
(defun output-categorization ()
  "Categorize the book and output the categorization to standard out."
  (let ((categorization (categorize-book)))
    (loop for index from 1 to (length categorization)
          do (format t
              "Chapter ~S: ~A~%"
              index (related-string (nth index categorization))))))

;; uncomment for interactive use
(defvar *book* (trimmed-book))
