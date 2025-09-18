;;;; significant-other.el --- Helper functions to help you jump to significant other files -*- lexical-binding: t -*-

;; Version: 1.0.0
;; Author: Ovi Stoica <ovidiu.stoica1094@gmail.com>
;; Url: https://github.com/ovistoica/significant-other.el
;; Keywords: convenience, project, testing

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;; Many files come in pairs, like tests and source files, header files and
;; implementations, components and their devcards.
;;
;; This package helps set up functions to jump between
;; significant other files.

;;; Credit:

;; This package was extracted from the original work of Magnars Sven
;; (https://github.com/magnars) as part of emacsd-reboot
;; (https://github.com/magnars/emacsd-reboot/blob/main/settings/significant-other.el). This library is an effort to extract that functionality and make it available to more people.

;;; Code:

(require 'dash)

(defgroup significant-other)

(setq significant-other-find-fn
      (lambda ()
        (message "Significant other not configured for this mode.")
        nil))

(defun significant-other-find-existing ()
  (-first 'file-exists-p (funcall significant-other-find-fn)))

(defun significant-other-find-all-existing ()
  (-filter 'file-exists-p (funcall significant-other-find-fn)))

(defun significant-other-find-tests ()
  "Find existing significant other files that are test files."
  (-filter (lambda (file)
             (and (file-exists-p file)
                  (string-match-p "/test/.+\\.clj" file)))
           (funcall significant-other-find-fn)))

(defun significant-other-jump (arg)
  (interactive "P")
  (let ((existing-files (significant-other-find-all-existing)))
    (cond
     ((= (length existing-files) 1)
      ;; Only one file exists, jump to it
      (find-file (car existing-files)))
     ((> (length existing-files) 1)
      ;; Multiple files exist, let user choose
      (let ((file (completing-read "Choose significant other: "
                                   existing-files nil t)))
        (find-file file)))
     (t
      ;; No existing files, offer to create one
      (when-let (file (car (funcall significant-other-find-fn)))
        (if arg
            (progn (find-file file) (save-buffer))
          (ido-find-file-in-dir (file-name-directory file))))))))

(defmacro with-significant-others (binding &rest mappings)
  (declare (indent 1))
  `(setq-local
    significant-other-find-fn
    (lambda ()
      (let ((,binding (buffer-file-name)))
        (cond
         ,@(--map
            `((string-match-p ,(car it) ,binding)
              ,(cadr it))
            mappings))))))

(global-set-key (kbd "s-j") 'significant-other-jump)

(provide 'significant-other)
