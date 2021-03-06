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

(defun huninet-mode-convert-char-before-point ()
  "Converts the char before the point to a huninet string."
  (let ((ch (buffer-substring-no-properties (1- (point)) (point))))
    (if ch
	(progn (backward-char)
	       (delete-char 1)
	       (insert (huninet-convert-char ch))))))



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
;; Minor mode functions
;;

(define-minor-mode huninet-mode
  "Converts characters to 7-bit notation as you type."
  :lighter "hun"
  (if huninet-mode
      (huninet-enable-mode)
    (huninet-disable-mode)))

(defun huninet-enable-mode ()
  "Enables the huninet mode."
  (add-hook 'post-command-hook 'huninet-transform-as-you-type nil t))

(defun huninet-disable-mode ()
  "Disables the huninet mode."
  (remove-hook 'post-command-hook 'huninet-transform-as-you-type t))

(defun huninet-transform-as-you-type ()
  "This function is called from the post-command-hook. It tests
  if a transformation is necessary and it executes the
  transformation."
  (if (equal "self-insert-command" (symbol-name this-command))
      (huninet-mode-convert-char-before-point)))

(provide 'huninet)

;;; huninet.el ends here
