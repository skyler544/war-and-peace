(fiveam:test can-read-file
  (fiveam:is (read-file "tests.lisp")))

(fiveam:test can-read-book
  (fiveam:is (read-book)))

(fiveam:test can-read-war-terms
  (fiveam:is (read-war-terms)))

(fiveam:test can-peace-terms
  (fiveam:is (read-peace-terms)))

(fiveam:test can-split-string-into-words
  (fiveam:is (equal '("foo" "bar") (tokenize-line "foo...bar"))))

(fiveam:test can-split-text-into-lists-of-words
  (fiveam:is (equal '(("foo" "bar")) (tokenize-text '("foo...bar")))))


