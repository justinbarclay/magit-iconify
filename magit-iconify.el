;;; magit-iconify.el --- Make you magit status buffer look iconic  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Justin Barclay

;; Author: Justin Barclay <github@justincbarclay.ca>
;; Package-Requires: ((emacs "29.1") (magit))
;; Package-Version: 0.5.0
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

(defcustom magit-iconify-fn nil
  "An optional function that returns an icon given a file name"
  :type '(function)
  :group 'magit)

(defgroup magit-iconify nil
  "Make your magit status buffer look iconic"
  :group 'magit)

(cond ((and (not magit-iconify-fn)
            (featurep 'nerd-icons))
       (require 'nerd-icons)
       (eval-when-compile
         (declare-function nerd-icons-icon-for-dir "ext:nerd-icons" t t)
         (declare-function nerd-icons-icon-for-file "ext:nerd-icons" t t))
       (defun magit-iconify-nerd-icon (file-or-dir)
         "Return the nerd-icon for a file or directory"
         (if (directory-name-p file-or-dir)
             (nerd-icons-icon-for-dir file-or-dir)
           (nerd-icons-icon-for-file file-or-dir)))
       (setopt magit-file-icon-fn 'magit-iconify-nerd-icon))
      ((and (not magit-file-icon-fn)
            (featurep 'all-the-icons))
       (require 'all-the-icons)
       (eval-when-compile
         (declare-function all-the-icons-for-dir "ext:all-the-icons" t t)
         (declare-function all-the-icons-for-file "ext:all-the-icons" t t))

       (defun magit-iconify-all-the-icons (file-or-dir)
         "Return the all-the-icons icon for a file or directory"
         (if (directory-name-p file-or-dir)
             (all-the-icons-icon-for-dir file-or-dir)
           (all-the-icons-icon-for-file file-or-dir)))
       (setopt magit-file-icon-fn 'all-the-icons-icon-for-file)))

(defun magit-iconify-file-section (file status orig)
  (if (boundp 'magit-file-icon-fn)
      (format "%-10s %s %s" status (funcall magit-file-icon-fn (or orig file))
              (if (or (not orig) (equal orig file))
                  file
                (format "%s -> %s %s") orig (funcall magit-file-icon-fn file) file))
    (format "%-10s %s" status
            (if (or (not orig) (equal orig file))
                file
              (format "%s -> %s" orig file)))))

(defun magit-diff-insert-file-section-with-icons
    (file orig status modes rename header binary long-status)
  (magit-insert-section
    ( file file
      (or (equal status "deleted") (derived-mode-p 'magit-status-mode))
      :source (and (not (equal orig file)) orig)
      :header header
      :binary binary)
    (insert (propertize (magit-iconify-file-section file status orig)
                        'font-lock-face 'magit-diff-file-heading))
    (cond ((and binary long-status)
           (insert (format " (%s, binary)" long-status)))
          ((or binary long-status)
           (insert (format " (%s)" (if binary "binary" long-status)))))
    (magit-insert-heading)
    (when modes
      (magit-insert-section (hunk '(chmod))
        (insert modes)
        (magit-insert-heading)))
    (when rename
      (magit-insert-section (hunk '(rename))
        (insert rename)
        (magit-insert-heading)))
    (magit-wash-sequence #'magit-diff-wash-hunk)))

(defun magit-iconify-file (file)
  (if (boundp 'magit-file-icon-fn)
      (format "%s %s" (funcall magit-file-icon-fn file) file)
    file))

(defun magit-insert-files-with-icons (type fn)
  (when-let ((files (funcall fn
                             (and magit-buffer-diff-files
                                  (cons "--" magit-buffer-diff-files)))))
    (magit-insert-section section ((eval type) nil t)
                          (magit-insert-heading (length files)
                            (let ((title (symbol-name type)))
                              (format "%c%s files"
                                      (capitalize (aref title 0))
                                      (substring title 1))))
                          (magit-insert-section-body
                            (let ((magit-section-insert-in-reverse t)
                                  (limit magit-status-file-list-limit))
                              (while (and files (> limit 0))
                                (cl-decf limit)
                                (let ((file (pop files)))
                                  (magit-insert-section (file file)
                                    (insert (propertize (magit-iconify-file file) 'font-lock-face 'magit-filename))
                                    (insert ?\n))))
                              (when files
                                (magit-insert-section (info)
                                  (insert (propertize
                                           (format "%s files not listed\n" (length files))
                                           'face 'warning)))))
                            (insert ?\n)
                            (oset section children (nreverse (oref section children)))))))

(defun magit-diff-wash-diffstat-with-icons ()
  (let (heading (beg (point)))
    (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
      (setq heading (match-string 1))
      (magit-delete-match)
      (goto-char beg)
      (magit-insert-section (diffstat)
        (insert (propertize heading 'font-lock-face 'magit-diff-file-heading))
        (magit-insert-heading)
        (let (files)
          (while (looking-at "^[-0-9]+\t[-0-9]+\t\\(.+\\)$")
            (push (magit-decode-git-path
                   (let ((f (match-string 1)))
                     (cond
                      ((string-match "{.* => \\(.*\\)}" f)
                       (replace-match (match-string 1 f) nil t f))
                      ((string-match " => " f)
                       (substring f (match-end 0)))
                      (t f))))
                  files)
            (magit-delete-line))
          (setq files (nreverse files))
          (while (looking-at magit-diff-statline-re)
            (magit-bind-match-strings (file sep cnt add del) nil
              (magit-delete-line)
              (when (string-match " +$" file)
                (setq sep (concat (match-string 0 file) sep))
                (setq file (substring file 0 (match-beginning 0))))
              (let ((le (length file)) ld)
                (setq file (magit-decode-git-path file))
                (setq ld (length file))
                (when (> le ld)
                  (setq sep (concat (make-string (- le ld) ?\s) sep))))
              (magit-insert-section (file (pop files))
                (insert (propertize (magit-iconify-file file) 'font-lock-face 'magit-filename)
                        sep cnt " ")
                (when add
                  (insert (propertize add 'font-lock-face
                                      'magit-diffstat-added)))
                (when del
                  (insert (propertize del 'font-lock-face
                                      'magit-diffstat-removed)))
                (insert "\n")))))
        (if (looking-at "^$") (forward-line) (insert "\n"))))))

(defun override-magit-diff-insert-file-section (_orig &rest args)
  (apply 'magit-diff-insert-file-section-with-icons args))

(defun override-magit-diff-wash-diffstat (_orig &rest args)
  (apply 'magit-diff-wash-diffstat-with-icons args))

(defun override-magit-insert-files (_orig &rest args)
  (apply 'magit-insert-files-with-icons args))

;;;###autoload
(define-minor-mode magit-iconify-mode
  "Make your magit status buffer look iconic"
  :global t
  (if magit-iconify-mode
      (progn
        (advice-add 'magit-diff-insert-file-section :around #'override-magit-diff-insert-file-section)
        (advice-add 'magit-diff-wash-diffstat :around #'override-magit-diff-wash-diffstat)
        (advice-add 'magit-insert-files :around #'override-magit-insert-files))
    (advice-remove 'magit-diff-insert-file-section #'override-magit-diff-insert-file-section)
    (advice-remove 'magit-diff-wash-diffstat #'override-magit-diff-wash-diffstat)
    (advice-remove 'magit-insert-files #'override-magit-insert-files))
  :group 'magit-iconify)

(provide 'magit-iconify)
;;; magit-iconify.el ends here
