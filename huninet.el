;; Huninet minor mode (C) 2016 Juhász Sándor <sandor.juhasz.1983@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
  "Hello, World!")

;;
;; Unit tests
;;

(ert-deftest huninet-convert-char-test ()
  "Tests if a character is properly converted to its huninet counterpart."
  (should (equal (huninet-convert-char "á") "a'"))
  (should (equal (huninet-convert-char "Ü") "U:"))
  (should (equal (huninet-convert-char "b") "b")))

(ert-deftest huninet-mode-convert-region-test ()
  "Tests if the conversion is done properly."
  (should (equal (huninet-mode-convert-region 10 20) "Hello, World!")))
