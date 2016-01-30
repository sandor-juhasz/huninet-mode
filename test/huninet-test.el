;;;
;;; Test cases for the huninet mode.
;;;

(ert-deftest huninet-convert-region-test ()
  "Tests if a region is poperly converted."
  (let ((test-cases '(("a" . "a")
		      ("árvíztűrő tükörfúrógép" . "a'rvi'ztu\"ro\" tu:ko:rfu'ro'ge'p")
		      ("ÁRVÍZTŰRŐ TÜKÖRFÚRÓGÉP" . "A'RVI'ZTU\"RO\" TU:KO:RFU'RO'GE'P")
		      ("a\nb\n" . "a\nb\n"))))
    (dolist (test-case test-cases)
      (let ((input-text (car test-case))
	    (expected-text (cdr test-case)))
	(message "Test case: \"%s\" -> \"%s\"" input-text expected-text)
	(with-temp-buffer
	  (insert input-text)
	  (huninet-mode-convert-region (point-min) (point-max))
	  (should (equal (buffer-string) expected-text)))))))

(ert-deftest huninet-convert-char-test ()
  "Tests if a character is properly converted to its huninet counterpart."
  (should (equal (huninet-convert-char "á") "a'"))
  (should (equal (huninet-convert-char "Ü") "U:"))
  (should (equal (huninet-convert-char "b") "b")))


(ert-deftest huninet-mode-test ()
  "Tests if the transformer function is registered when the minor
mode is enable.d"
  (with-temp-buffer
     (let ((is-mode-enabled (lambda () (member 'huninet-transform-as-you-type post-command-hook))))
       (should-not (funcall is-mode-enabled))
       (huninet-mode 1)
       (should (funcall is-mode-enabled))
       (huninet-mode 0)
       (should-not (funcall is-mode-enabled)))))


(ert-deftest huninet-mode-convert-char-before-point-test ()
  "Tests that the currently typed in character gets converted."
  (with-temp-buffer
    (insert "á")
    (huninet-mode-convert-char-before-point)
    (should (equal (buffer-string) "a'")))
  (with-temp-buffer
    (insert "a")
    (huninet-mode-convert-char-before-point)
    (should (equal (buffer-string) "a"))))
