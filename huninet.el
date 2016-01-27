;;; huninet.el --- Minor mode to enter text in the 7-bit huninet format.

;; Copyright (C) 2016 Sandor Juhasz

;; Author: Sandor Juhasz <sandor.juhasz.1983@gmail.com>
;; URL: https://github.com/sandor-juhasz
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Minor mode to enter Hungarian text using the 7-bit ASCII character set.
;; This encoding was recommended by the Huninet in the early days of the
;; interet back in the '90s.
;;
;;; Code:

(defvar huninet-mapping '(("á" . "a'")
			  ("é" . "e'")
			  ("í" . "i'")
			  ("ó" . "o'")
			  ("ö" . "o:")
			  ("ő" . "o\"")
			  ("ú" . "u'")
			  ("ü" . "u:")
			  ("ű" . "u\"")
			  ("Á" . "A'")
			  ("É" . "E'")
			  ("Í" . "I'")
			  ("Ó" . "O'")
			  ("Ö" . "O:")
			  ("Ő" . "O\"")
			  ("Ú" . "U'")
			  ("Ü" . "U:")
			  ("Ű" . "U\""))  
  "The mapping to convert Hungarian characters to their pairs based on the
Huninet recommendation.")

(defun huninet-convert-char (char)
  "Converts the character passed as parameter to a huninet string. If the
character is not mapped, it returns the character as is."
  (let ((conversion-pair (assoc char huninet-mapping)))
    (if conversion-pair
	(cdr conversion-pair)
      char)))

(defun huninet-mode-convert-region (start end)
  "Converts the selected region to a huninet string."
  (interactive "*r")
  (save-excursion
    (goto-char start)
    (let ((cnt (- end start))
	  (i   0))
      ;(message "%d" cnt)
      (while (< i cnt)
	;(message "Executing for i=%d" i)
	(let* ((p       (point))
	       (char    (buffer-substring-no-properties p (1+ p)))
	       (newchar (huninet-convert-char char)))
	  (delete-char 1)
	  (insert newchar)
	  (setq i (1+ i)))))))

;;
;; Unit tests
;;

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

(provide 'huninet)

;;; huninet.el ends here
