;;; discover-point.el --- Discover information about value at point -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Morten Slot Kristensen

;; Author: Morten Slot Kristensen <msk AT nullpointer DOT dk>
;; Keywords: discover value help
;; URL: https://github.com/netromdk/discover-point
;; Version: 0.0.1

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

;; Discover information about value at point.

;;; Code:

(defun bin-string-p (value)
  "Check if VALUE is a binary string of chars in [0-1]s."
  (string-match "\\`[0-1]+\\'" value))

(defun oct-string-p (value)
  "Check if VALUE is a octal string of chars in [0-7]s."
  (string-match "\\`[0-7]+\\'" value))

(defun dec-string-p (value)
  "Check if VALUE is a decimal string of chars in [0-9]s."
  (string-match "\\`[0-9]+\\'" value))

(defun hex-string-p (value)
  "Check if VALUE is a hex string of chars in [0-9a-f]s."
  (string-match "\\`[0-9a-f]+\\'" value))

(defun convert-to-number (value base)
  "Convert VALUE to number in BASE, or nil if not possible."
  (let ((num (string-to-number value base)))
    (if (and (zerop num)
             (string-match "\\`\\s-*0+\\.?0*\\s-*\\'" value))
        nil
      num)))

(defun discover--value (value)
  "Subroutine to convert VALUE from number or character."
  (let* ((dec (if (dec-string-p value)
                  (convert-to-number value 10)
                  nil))
         (bin (if (bin-string-p value)
                  (convert-to-number value 2)
                nil))
         (oct (if (oct-string-p value)
                  (convert-to-number value 8)
                nil))
         (hex (if (hex-string-p value)
                  (convert-to-number value 16)
                nil))
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
(defun discover-value (value)
  "Discover information about VALUE."
  (interactive (list (read-string "Value: ")))
  (let ((msg ""))
    (if (not (hex-string-p value)) ;; If not bin, oct, dec, or hex..
          (dolist (val (string-to-list value))
            (setq msg (format "%s\n%s" msg (discover--value (number-to-string val)))))
      (setq msg (concat msg (discover--value value))))
    (if (= (string-width msg) 0)
        (message "No results for '%s'." value)
      (message "'%s':%s" value msg))))

;;;###autoload
(defun discover-at-point ()
  "Discover information about value/region at point."
  (interactive)
  (if (use-region-p)
      (progn
        (discover-value
         (buffer-substring-no-properties (region-beginning) (region-end))))
    (discover-value
     (substring-no-properties (thing-at-point 'word)))))


(provide 'discover-point)

;;; discover-point.el ends here
