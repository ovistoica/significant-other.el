;;; significant-other.el --- Helper functions to help you jump to significant other files -*- lexical-binding: t -*-

;; Version: 1.0.0
;; Author: Magnars Sven
;; Maintainer: Ovi Stoica <ovidiu.stoica1094@gmail.com>
;; URL: https://github.com/ovistoica/significant-other.el
;; Package-Requires: ((emacs "24.3") (dash "2.12.0"))
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

(defgroup significant-other nil
  "Helper functions to jump to significant other files."
  :group 'convenience
  :prefix "significant-other-")

(defcustom significant-other-test-file-regex "/test/.+\\.clj"
  "Regular expression to identify test files in significant other candidates."
  :type 'string
  :group 'significant-other)

(defcustom significant-other-multiple-file-behavior 'prompt
  "Behavior when multiple significant other files exist.
- 'prompt: Use completing-read to let user choose (default)
- 'first: Jump to the first file in the list automatically"
  :type '(choice (const :tag "Prompt user to choose" prompt)
                 (const :tag "Jump to first file" first))
  :group 'significant-other)

(defvar significant-other-find-fn
  (lambda ()
    (message "Significant other not configured for this mode.")
    nil)
  "Function to find significant other files for the current buffer.
Should return a list of file paths that are considered significant others.")

(defun significant-other-find-existing ()
  "Find the first existing significant other file for the current buffer."
  (-first 'file-exists-p (funcall significant-other-find-fn)))

(defun significant-other-find-all-existing ()
  "Find all existing significant other files for the current buffer."
  (-filter 'file-exists-p (funcall significant-other-find-fn)))

(defun significant-other-find-tests ()
  "Find existing significant other files that are test files."
  (-filter (lambda (file)
             (and (file-exists-p file)
                  (string-match-p significant-other-test-file-regex file)))
           (funcall significant-other-find-fn)))

;;;###autoload
(defun significant-other-jump (arg)
  "Jump to a significant other file for the current buffer.
With prefix ARG, create the file if it doesn't exist.
If multiple significant others exist, behavior depends on
`significant-other-multiple-file-behavior'."
  (interactive "P")
  (let ((existing-files (significant-other-find-all-existing)))
    (cond
     ((= (length existing-files) 1)
      ;; Only one file exists, jump to it
      (find-file (car existing-files)))
     ((> (length existing-files) 1)
      ;; Multiple files exist, behavior depends on customization
      (if (eq significant-other-multiple-file-behavior 'first)
          (find-file (car existing-files))
        (let ((file (completing-read "Choose significant other: "
                                     existing-files nil t)))
          (find-file file))))
     (t
      ;; No existing files, offer to create one
      (when-let (file (car (funcall significant-other-find-fn)))
        (if arg
            (progn (find-file file) (save-buffer))
          (ido-find-file-in-dir (file-name-directory file))))))))

(defmacro with-significant-others (binding &rest mappings)
  "Configure significant other file mappings for the current buffer.
BINDING is the variable name to bind the current file path to.
MAPPINGS is a list of (REGEX PATHS-EXPR) pairs where:
- REGEX matches against the current file path
- PATHS-EXPR evaluates to a list of significant other file paths"
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

(provide 'significant-other)

;;; significant-other.el ends here
