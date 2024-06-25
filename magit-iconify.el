;;; magit-iconify.el --- Make you magit status buffer look iconic  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Barclay

;; Author: Justin Barclay <github@justincbarclay.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'magit)
(require 'nerd-icons)
(require 'text-properties)

(defun magit-iconify--insert-icon (file)
  (if (directory-name-p file)
      (insert (format "%s " (nerd-icons-icon-for-dir file)))
    (insert (format "%s " (nerd-icons-icon-for-file file)))))

;; magit-diff-insert-file-section
;; magit-diff-wash-diffstat
;; magit-diff-insert-file-section
(defun magit-iconify--diff-file-heading ()
  (progn
    (forward-word)
    (forward-whitespace 1)
    (insert (propertize
             (format "%s " (nerd-icons-icon-for-file
                            (thing-at-point 'filename t)))
             'font-lock-faces 'magit-diff-file-heading))
    (when (re-search-forward "->\\\s+" (pos-eol) t)
      (magit-iconify--insert-icon (thing-at-point 'filename t)))))

(defun magit-iconify-status-buffer ()
  (require 'text-property-search)
  (require 'nerd-icons)
  (let ((pos (point)))
    (read-only-mode -1)
    (goto-char (point-min))
    (while-let ((prop (text-property-search-forward 'font-lock-face nil
                                                    (lambda (propa propb)
                                                      (memq propb '(magit-filename magit-diff-file-heading)))
                                                    nil)))
      (save-mark-and-excursion
        (goto-char (prop-match-beginning prop))
        (if (eq (get-text-property (point) 'font-lock-face)
                'magit-diff-file-heading)
            ;; Move to beginning of filename
            (magit-iconify--diff-file-heading)
          (magit-iconify--insert-icon (thing-at-point 'filename t)))))
    (goto-char pos)))

(advice-add 'magit-refresh :after 'magit-iconify-status-buffer)

(provide 'magit-iconify)
;;; magit-iconify.el ends here
