(fiveam:test can-read-file
  (fiveam:is (read-file "tests.lisp")))

(fiveam:test can-read-book
  (fiveam:is (read-book)))

(fiveam:test can-read-war-terms
  (fiveam:is (read-war-terms)))

(fiveam:test can-read-peace-terms
  (fiveam:is (read-peace-terms)))

(fiveam:test can-split-string-into-words
  (fiveam:is (equal '("foo" "bar") (tokenize-line "foo...bar"))))

(fiveam:test can-split-text-into-lists-of-words
  (fiveam:is (equal '(("foo" "bar")) (tokenize-text '("foo...bar")))))

(fiveam:test can-tokenize-book
  (fiveam:is (trimmed-book)))

(fiveam:test successfully-trimmed-book
  (fiveam:is (not
              (member
               '("END" "OF" "THE" "PROJECT" "GUTENBERG" "EBOOK" "WAR" "AND" "PEACE")
               (trimmed-book)))))

(fiveam:test can-filter-words
  (fiveam:is (not (member "lisp" (read-war-terms)))))

(fiveam:test non-zero-overlap-coefficient
  (fiveam:is (overlap-coefficient '("foo" "bar" "baz") '("cool" "foo" "baz"))))

(fiveam:test categorizes-war-chapter-correctly
  (fiveam:is (equal 'war (categorize-chapter '("sword" "death" "fear")))))

(fiveam:test categorizes-peace-chapter-correctly
  (fiveam:is (equal 'peace (categorize-chapter '("love" "wealth" "food")))))


