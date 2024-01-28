(fiveam:test can-get-indices
  (fiveam:is (equal '(0 3) (indices (read-war-terms) '("sword" "peace" "food" "fear")))))

(fiveam:test can-calculate-single-distance
  (fiveam:is (equal 5 (distance 0 5))))

(fiveam:test can-calculate-multiple-distances
  (fiveam:is (equal '(5 5) (rec-distances '(0 5 10) '()))))

