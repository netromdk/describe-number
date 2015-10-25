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

;;;###autoload
(defun discover-value (value)
  "Discover information about VALUE."
  (interactive (list (read-string "Value: ")))
  (let* ((bin (if (bin-string-p value)
                  (convert-to-number value 2)
                nil))
         (oct (if (oct-string-p value)
                  (convert-to-number value 8)
                nil))
         (dec (if (dec-string-p value)
                  (convert-to-number value 10)
                nil))
         (hex (if (hex-string-p value)
                  (convert-to-number value 16)
                nil))
         (msg ""))
    (if bin
        (setq msg (format "%s (bin=%d hex=%X char='%c')" msg bin bin bin)))
    (if oct
        (setq msg (format "%s (oct=%o hex=%X char='%c')" msg dec oct oct)))
    (if dec
        (setq msg (format "%s (dec=%d hex=%X char='%c')" msg dec dec dec)))
    (if hex
        (setq msg (format "%s (hex=%d char='%c')" msg hex hex)))
    (if (= (string-width msg) 0)
        (message "No results for '%s'." value)
      (message "'%s' (len=%d) as%s" value (string-width value) msg))))

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
