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

(defun initialism-view ()
  "Presents information to the user."
  (interactive)
  (if initialism--model (message initialism-format-string initialism--model)
    (error "(initialism) Sorry, I don't have a value to display")))

(defun initialism-delete ()
  "Deletes view."
  (interactive)
  (setq initialism--model nil)
  (message "(initialism) View deleted."))

(defun initialism-insert ()
  "Insert then deletes the view."
  (interactive)
  (insert (initialism-view))
  (message "(initialism) View inserted.")
  (initialism-delete))

(cl-defun initialism-do ()
  "Incrementally builds initialism using content under cursor."
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
              "(initialism) Sorry, I don't know how to handle TYPE: '%s'" forward-type))))))

(provide 'initialism)
;;; initialism.el ends here

