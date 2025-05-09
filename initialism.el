;;; initialism.el --- Create a word formed from initial letters  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Grant Rettke

;; Author: Grant Rettke <grant@wisdomandwonder.com>
;; Maintainer: Grant Rettke <grant@wisdomandwonder.com>
;; Version: 1.1.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: abbrev
;; Homepage: https://github.com/grettke/initialism

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

;; *This tool makes memorizing scripts and speeches easier!*
;;
;; Why struggle with the tedious and error-prone task of creating
;; initialisms for an entire script by hand? This program does it
;; with impeccable precision for you every time, allowing you to
;; format the output to your liking. Experience the ease and
;; convenience for yourself―your brain will love it!
;;
;; - Before using this program
;;  - Watch Lauren Tothero explain how this technique works
;;    - https://www.youtube.com/watch?v=Jbe1-oHnR6k

;;; Code:

(require 'cl-lib)

(defvar initialism-format-string " (%s)"
  "Configuration string passed to `format' for the view.")

(defvar initialism-crlf t
  "Non-nil means after `initialism-complete' perform a CRLF.

A user-convenience to ease processing single-line content.")

(defcustom initialism-auto-complete t
  "If non-nil, automatically call `initialism-complete' when
point reaches the end of a line during `initialism-build'.

This option controls whether initialism completion is triggered
by encountering end-of-line. Set to `nil' if you want to handle
completion manually."
  :type 'boolean
  :group 'initialism)

(defvar initialism--model nil
  "The internal representation of the information.")

(defun initialism--format ()
  "Create a formatted model for use."
  (cond (initialism--model
         (format initialism-format-string initialism--model))
        (t
         (message "(initialism) Sorry, I don't have a value to format.")
         nil)))

(cl-defun initialism-build ()
  "Incrementally build the model using the character under the cursor.

- Q. Initialisms are created using the first letter of a word.
     However, this function includes both letters and non-letters
     (characters). Why?
- A. It creates initialisms from entire sentences. Punctuation is
     essential for understanding a sentence-based initialism."
  (interactive)
  (cl-block here
    (let ((character (char-to-string (char-after)))
          (forward-type 'undefined))
      (cond ((looking-at "[[:alnum:]]") (setq forward-type 'word))
            ((looking-at "[[:punct:]]") (setq forward-type 'char))
            ((looking-at "[[:blank:]]") (progn (forward-char) (cl-return-from here)))
            ((and initialism-auto-complete
                  (looking-at "$"))
             (progn
               (initialism-complete)
               (cl-return-from here)))
            ((error
              "(initialism) Sorry, I don't know how to handle character: '%s'" character)))
      (let* ((uc-letter (capitalize character))
             (new-model (concat initialism--model uc-letter)))
        (setq initialism--model new-model)
        (initialism-view))
      (cond ((equal forward-type 'word) (forward-word))
            ((equal forward-type 'char) (forward-char))
            ((error
              "(initialism) Sorry, I don't know how to handle TYPE: '%s'"
              forward-type))))))

(defun initialism-view ()
  "Present the view to the user.

The message is prefixed with the model length purely to assist
in understanding the model assembly process: it is NOT part of
the underlying model itself."
  (interactive)
  (cond
   (initialism--model
    (let* ((size (length initialism--model))
           (status (format "[%s]: " size))
           (report (concat status (initialism--format))))
      (message report)))
   (t (message "(initialism) Sorry, I don't have a value to display"))))

(defun initialism-insert ()
  "Insert the view.

For convenience, the inserted value is also added to the `kill-ring'."
  (interactive)
  (cond
   (initialism--model
    (let ((it (initialism--format)))
      (kill-new it)
      (insert it))
    (message "(initialism) View inserted."))
   (t (message "(initialism) Sorry, I don't have a value to insert"))))

(defun initialism-delete ()
  "Delete the model."
  (interactive)
  (cond
   (initialism--model
    (setq initialism--model nil)
    (message "(initialism) View deleted."))
   (t
    (message "(initialism) Sorry, I don't have a value to delete."))))

(defun initialism-complete ()
  "Completes build process for this model.

- Insert the view
- Delete the model
- When `initialism-crlf' perform a CRLF operation."
  (interactive)
  (initialism-insert)
  (initialism-delete)
  (when initialism-crlf
    (beginning-of-line)
    (forward-line 1))
  (message "(initialism) Initialism complete."))

(defun initialism-dispatch ()
  "Use `C-u' prefix arguments to use the library.

Use this as a shortcut for the most frequently used
functions in order of the typical workflow.

Usage:

- Building it
  - `initialism-dispatch': Calls `initialism-build')
- Reviewing it
  - `C-u' `initialism-dispatch': Calls `initialism-view')
- Inserting it
  - `C-u' `C-u' `initialism-dispatch': Calls `initialism-insert')
- Deleting it
  - `C-u' `C-u' `C-u' `initialism-dispatch': Calls `initialism-delete'"
  (interactive)
  (let* ((arg current-prefix-arg)
         (value (if (null arg) nil (first arg))))
    (cond
     ((null value) (call-interactively 'initialism-build))
     ((= value 4) (call-interactively 'initialism-view))
     ((= value 16) (call-interactively 'initialism-insert))
     ((= value 64) (call-interactively 'initialism-delete))
     (t
      (error "(initialism) Sorry, I can't handle the argument '%s'" value)))))

(defun initialism-help ()
  "Display documentation for `initialism-dispatch'."
  (interactive)
  (describe-symbol 'initialism-dispatch))

(provide 'initialism)
;;; initialism.el ends here

