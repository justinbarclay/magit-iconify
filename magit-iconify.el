;;; magit-iconify.el --- Make you magit status buffer look iconic  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Justin Barclay

;; Author: Justin Barclay <github@justincbarclay.ca>
;; Package-Requires: ((emacs "29.1") (magit "4.3.0-latest"))
;; Package-Version: 1.0.0
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

(declare-function nerd-icons-icon-for-dir "ext:nerd-icons" t t)
(declare-function nerd-icons-icon-for-file "ext:nerd-icons" t t)

(declare-function all-the-icons-for-dir "ext:all-the-icons" t t)
(declare-function all-the-icons-for-file "ext:all-the-icons" t t)

(defgroup magit-iconify nil
  "Make your magit status buffer look iconic"
  :group 'magit)

(defun magit-iconify-nerd-icon (file-or-dir)
  "Return the nerd-icon for a file or directory"
  (if (directory-name-p file-or-dir)
      (nerd-icons-icon-for-dir file-or-dir)
    (nerd-icons-icon-for-file file-or-dir)))

(defun magit-iconify-all-the-icons (file-or-dir)
  "Return the all-the-icons icon for a file or directory"
  (if (directory-name-p file-or-dir)
      (all-the-icons-icon-for-dir file-or-dir)
    (all-the-icons-icon-for-file file-or-dir)))

(defcustom magit-iconify-fn (cond ((featurep 'nerd-icons) #'magit-iconify-nerd-icon)
                                  ((featurep 'all-the-icons) #'magit-iconify-all-the-icons)
                                  #'identity)
  "Function to convert a File or Directory to an icon."
  :type '(choice (function-item #'identity)
                 (function-item #'magit-iconify-nerd-icon)
                 (function-item #'magit-iconify-all-the-icons)
                 function)
  :group 'magit)

(defun magit-iconify-nerd-icon (file-or-dir)
  "Return the nerd-icon for a file or directory"
  (if (directory-name-p file-or-dir)
      (nerd-icons-icon-for-dir file-or-dir)
    (nerd-icons-icon-for-file file-or-dir)))

(defun magit-iconify-format-file (_kind file face &optional status orig)
  (propertize
   (concat (and status (format "%-11s" status))
           (if orig
               (format "%s %s -> %s %s"
                       (funcall magit-iconify-fn orig) orig
                       (funcall magit-iconify-fn file) file)
             (format "%s %s" (magit-iconify-nerd-icon file) file)))
   'font-lock-face face))
(setopt magit-formate-file-function 'magit-iconify-format-file)


;;;###autoload
(define-minor-mode magit-iconify-mode
  "Make your magit status buffer look iconic"
  :global t
  (if magit-iconify-mode
      (progn
        (setq magit-iconify-old-fomatter 'magit-format-file-function)
        (setq magit-format-file-function 'magit-iconify-format-file))
    (setq magit-iconify-file-function 'magit-iconify-old-formatter))
  :group 'magit-iconify)

(provide 'magit-iconify)
;;; magit-iconify.el ends here
