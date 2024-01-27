;; The way this library works, this thread pool must be globally
;; available. Throughout the program, instances of `mapcar' can be
;; seamlessly replaced with a call to `lparallel:pmap' with `list' as
;; the return type.
(setf lparallel:*kernel* (lparallel:make-kernel 4))

(defun pmapcar (func list)
  "Wrapper function for parallel map."
  (lparallel:pmap 'list func list))

;; Read files
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

;; Tokenize text
;; ----------------------------------------------------
(defun tokenize-line (line)
  "Splits `LINE' on whitespace and removes punctuation and empty strings."
  (remove-if #'str:blankp
             (pmapcar #'str:remove-punctuation
                      (str:words line))))

(defun tokenize-text (text)
  "Splits `TEXT' into lists of words corresponding to the lines from the
text."
  (remove nil (pmapcar #'tokenize-line text)))

(defun tokenized-book ()
  "Read in the book and return it as a list of lists of strings."
  (tokenize-text (read-book)))

(defun trimmed-book ()
  "Remove the parts of the file that don't belong to a chapter."
  (let ((book (tokenized-book)))
    (reverse (set-difference
     (member '("CHAPTER" "1") book :test 'equal)
     (member '("END" "OF" "THE" "PROJECT" "GUTENBERG" "EBOOK" "WAR" "AND" "PEACE") book :test 'equal)))))

;; Split by chapter
;; ----------------------------------------------------
(defun filter-words (terms chapter)
  "Filters words from `chapter' that also occur in `terms' (case
insensitive)."
  (remove-if-not
   (lambda (word) (member word terms :test 'string-equal))
   chapter))

(defun overlap-coefficient (terms chapter)
  (/ (length (filter-words terms chapter))
     (min (length terms) (length chapter))))

;; (defun categorize-chapter (chapter)
;;   "Categorize a chapter as war related or peace related."
;;   (let ((war (read-war-terms))
;;         (peace (read-peace-terms)))
;;     (if (> (length (filter-words war chapter))
;;            (length (filter-words peace chapter)))
;;         'war
;;         'peace)))

(defun indices (search-terms chapter)
  "Return a list of indices from `chapter' where terms from
`search-terms' occur."
  (remove nil
          (pmapcar (lambda (term)
                     (position term chapter
                               :test 'string-equal))
                   search-terms)))

(defun distance (i j)
  "Calculate the distance between i and j."
  (cond ((or (null i) (null j)) 0)
        (t (abs (- i j)))))

(defun rec-distances (indices acc)
  "Given a list of `indices', returns a list of the pairwise distances
between the indices."
  (cond ((null indices) acc)
        (t (rec-distances (rest indices)
                          (push (distance (first indices)
                                          (second indices))
                                acc)))))

(defun calculate-category-density (category-words chapter)
  "Calculate the density of words from `category-words' that occur in `chapter'."
  (let* ((indices (sort (indices category-words chapter) #'<))
         (distances (rec-distances indices '()))
         (total-distance (reduce #'+ distances)))
    (if (zerop total-distance)
        0.0
        (/ total-distance (length indices)))))

(defun categorize-chapter (chapter)
  "Return war if the density of war terms in `chapter' is higher than the density of peace terms in `chapter', else return peace."
  (cond ((> (calculate-category-density (read-war-terms) chapter)
            (calculate-category-density (read-peace-terms) chapter))
         'war)
        (t 'peace)))

(defun is-chapterp (line)
  "A chapter starts with CHAPTER (case sensitive)."
  (equal "CHAPTER" (first line)))

(defun split-recursive (book chapter acc)
  "Recursive function to collect all the chapters from the book."
  (cond
    ;; if the book is nil, we're done
    ((null book) (reverse (push (categorize-chapter chapter) acc)))

    ;; if the line is a chapter, we process the
    ;; chapter we've collected so far and push it
    ;; to the accumulator
    ((is-chapterp (first book))
     (progn
       (when chapter
         (push (categorize-chapter chapter) acc))
       (split-recursive (rest book) (first book) acc)))

    ;; the line was not a new chapter, so append it to the current
    ;; chapter and move on
    (t (split-recursive (rest book) (append chapter (first book)) acc))))

(defun split-chapters (book)
    "Splits `BOOK' into chapters and categorizes them. The return value is
a list of categorizations corresponding to each chapter."
  (split-recursive book '() '()))

;; Output and Utility Functions
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
  (let ((categorization (split-chapters (trimmed-book))))
    (loop for index from 1 to (length categorization)
          do (format t "Chapter ~S: ~A~%"
                     index (related-string (nth index categorization))))))

