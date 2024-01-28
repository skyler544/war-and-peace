(defun indices (search-terms chapter)
  "Return a list of indices from `chapter' where terms from
`search-terms' occur."
  (reverse (remove nil
                   (pmapcar (lambda (term)
                              (position term chapter
                                        :test 'string-equal))
                            search-terms))))


(defun distance (i j)
  "Calculate the distance between i and j. Returns 0 if either argument
is nil."
  (cond ((or (null i) (null j)) 0)
        (t (abs (- i j)))))

(defun rec-distances (indices acc)
  "Given a list of `indices', returns a list of the pairwise distances
between the indices."
  (cond ((or (null indices) 
             (null (second indices)))
         acc)
        (t (rec-distances (rest indices)
                          (push (distance (first indices)
                                          (second indices))
                                acc)))))

(defun calculate-category-density (category-words chapter)
  "Calculate the density of words from `category-words' that occur in `chapter'."
  (let* ((indices (indices category-words chapter))
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
