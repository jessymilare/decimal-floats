(defun test (a b)
  (locally ;      (declare (optimize sb-ext:inhibit-warnings))
    (if t
        (+ a b)
        (- a b))))
