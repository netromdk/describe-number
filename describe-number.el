;;; describe-number.el --- Describe number at point.

;; Copyright (C) 2015  Morten Slot Kristensen

;; Author: Morten Slot Kristensen <msk AT nullpointer DOT dk>
;; Keywords: describe value help
;; URL: https://github.com/netromdk/describe-number
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify it under the terms of the
;; GNU General Public License as published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;; even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License along with this program.  If
;; not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Describe number value at point/region. If value is a number then
;; binary/octal/decimal/hexadecimal/character values and conversions are shown. For strings each
;; character is processed in the same way.
;;
;; Use `describe-number-at-point' on point/region or `describe-number' to input value manually.
;;
;; Might be preferable to bind `describe-number-at-point' to some key:
;;   (global-set-key (kbd "C-c ?") 'describe-number-at-point)

;;; Code:

(defun describe-number--convert-to-number (value base)
  "Convert VALUE to number in BASE, or nil if not possible."
  (let ((num (string-to-number value base)))
    (if (and (zerop num)
             (not (string-match "\\`[ ]*0[ ]*\\'" value)))
        nil
      num)))

(defun describe-number--get-bin-value (value)
  "Retrieve binary VALUE from string."
  (if (string-match "\\`[ ]*\\([0-1]+\\)[ ]*\\'" value)
      (describe-number--convert-to-number (match-string 1 value) 2)
    nil))

(defun describe-number--get-oct-value (value)
  "Retrieve octal VALUE from string with optional prefixes 'o', '0o', or '#o'."
  (if (string-match "\\`[ ]*\\(?:[0#]?o\\)?\\([0-7]+\\)[ ]*\\'" value)
      (describe-number--convert-to-number (match-string 1 value) 8)
    nil))

(defun describe-number--get-dec-value (value)
  "Retrieve decimal VALUE from string."
  (if (string-match "\\`[ ]*\\([0-9]+\\)[ ]*\\'" value)
      (describe-number--convert-to-number (match-string 1 value) 10)
    nil))

(defun describe-number--get-hex-value (value)
  "Retrieve hexadecimal VALUE from string with optional prefixes 'x', '0x', or '#x'."
  (if (string-match "\\`[ ]*\\(?:[0#]?x\\)?\\([0-9a-f]+\\)[ ]*\\'" value)
      (describe-number--convert-to-number (match-string 1 value) 16)
    nil))

(defun describe-number--is-number-p (value)
  "Check if VALUE is binary, octal, decimal, or hexadecimal."
  (or (describe-number--get-bin-value value)
      (describe-number--get-oct-value value)
      (describe-number--get-dec-value value)
      (describe-number--get-hex-value value)))

(defun describe-number--describe (value)
  "Subroutine to convert VALUE from number or character."
  (let* ((bin (describe-number--get-bin-value value))
         (oct (describe-number--get-oct-value value))
         (dec (describe-number--get-dec-value value))
         (hex (describe-number--get-hex-value value))
         (msg ""))
    (if dec
        (setq msg (format "%s [%d #x%X #o%o '%c']" msg dec dec dec dec)))
    (if bin
        (setq msg (format "%s [b->d=%d #x%X #o%o '%c']" msg bin bin bin bin)))
    (if oct
        (setq msg (format "%s [o->d=%d #x%X '%c']" msg oct oct oct)))
    (if hex
        (setq msg (format "%s [x->d=%d #o%o '%c']" msg hex hex hex)))
    msg))

;;;###autoload
(defun describe-number (value)
  "Discover information about VALUE."
  (interactive (list (read-string "Value: ")))
  (if (not (zerop (length value)))
      (let ((msg ""))
        (if (not (describe-number--is-number-p value)) ;; If not bin, oct, dec, or hex..
            (dolist (val (string-to-list value))
              (setq msg (format "%s\n%s" msg (describe-number--describe (number-to-string val)))))
          (setq msg (concat msg (describe-number--describe value))))
        (if (zerop (length msg))
            (message "No results for '%s'." value)
          (message "'%s':%s" value msg)))
    (message "Must input value!")))

;;;###autoload
(defun describe-number-at-point ()
  "Discover information about value at point or region by using `describe-number'."
  (interactive)
  (if (use-region-p)
      (describe-number
       (buffer-substring-no-properties (region-beginning) (region-end)))
    (let ((thing (thing-at-point 'word)))
      (if thing
          (describe-number
           (substring-no-properties (thing-at-point 'word)))
        (message "Nothing at point!")))))


(provide 'describe-number)
;;; describe-number.el ends here
