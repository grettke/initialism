;;; initialism.el --- Create an abbreviation formed from initial letters.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Grant Rettke

;; Author: Grant Rettke <grant@wisdomandwonder.com>
;; Keywords: abbrev

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

(require 'cl-lib)

(defgroup initialism
  '((initialism-format-string custom-variable))
  "Create an abbreviation formed from initial letters."
  :group 'abbrev)

(defvar initialism--model nil
  "The internal representations of information.")

(defcustom initialism-format-string "(%s)"
  "Configuration string passed to `format' for the view."
  :type 'string
  :group 'initialism)

(cl-defun initialism-build ()
  "Incrementally builds initialism using character under cursor.

- Q. Initialisms are created using the first letter of a word.
     However, this function includes both letters and non-letters
     (characters). Why?
- A. It creates initialisms from entire sentences. Punctuation is
     essential for understanding a sentence-based initialism."
  (interactive)
  (cl-block here
    (let ((letter (char-to-string (char-after)))
          (forward-type 'undefined))
      (cond ((looking-at "[[:alnum:]]") (setq forward-type 'word))
            ((looking-at "[[:punct:]]") (setq forward-type 'char))
            ((looking-at "[[:blank:]]") (progn (forward-char) (cl-return-from here)))
            ((error
              "(initialism) Sorry, I don't know how to handle LETTER: '%s'" letter)))
      (let* ((uc-letter (capitalize letter))
             (new-model (concat initialism--model uc-letter)))
        (setq initialism--model new-model)
        (initialism-view))
      (cond ((equal forward-type 'word) (forward-word))
            ((equal forward-type 'char) (forward-char))
            ((error
              "(initialism) Sorry, I don't know how to handle TYPE: '%s'"
              forward-type))))))

(defun initialism-view ()
  "Presents information to the user."
  (interactive)
  (if initialism--model (message initialism-format-string initialism--model)
    (error "(initialism) Sorry, I don't have a value to display")))

(defun initialism-insert ()
  "Insert then deletes the view."
  (interactive)
  (insert (initialism-view))
  (message "(initialism) View inserted."))

(defun initialism-delete ()
  "Deletes view."
  (interactive)
  (setq initialism--model nil)
  (message "(initialism) View deleted."))

(defun initialism-dispatch ()
  "Use `C-u' prefix arguments to use the library.

Use this as a shortcut for the most frequently used
functions in order of the typical workflow:

- Building it
- Reviewing it
- Inserting it
- Deleting it

Usage:

- `initialism-dispatch': Calls `initialism-build')
- `C-u' `initialism-dispatch': Calls `initialism-view')
- `C-u' `C-u' `initialism-dispatch': Calls `initialism-insert')
- `C-u' `C-u' `C-u' `initialism-dispatch': Calls `initialism-delete'"
  (interactive)
  (let* ((arg current-prefix-arg)
         (value (if (null arg) nil (first arg))))
    (cond ((null value) (call-interactively 'initialism-build))
          ((= value 4) (call-interactively 'initialism-view))
          ((= value 16) (call-interactively 'initialism-insert))
          ((= value 64) (call-interactively 'initialism-delete))
          (t
           (error "(initialism) Sorry, I can't handle the argument '%s'" value)))))

(provide 'initialism)
;;; initialism.el ends here

