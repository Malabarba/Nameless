;;; nameless.el --- Hide package namespace in your emacs-lisp code  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Keywords: convenience, lisp
;; Version: 0.2
;; Package-Requires: ((emacs "24.2"))

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

;; Usage
;; ─────
;;
;;   To use this package add the following configuration to your Emacs init
;;   file.
;;
;;   ┌────
;;   │ (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
;;   └────
;;
;;   You can configure a string to use instead of `:' by setting the
;;   `nameless-prefix', and the name of the face used is `nameless-face'.
;;
;;   While the mode is active, the `_' key inserts the package
;;   namespace if appropriate.

;;; Code:
(require 'lisp-mnt)

(defgroup nameless nil
  "Customization group for nameless."
  :group 'emacs)

(defcustom nameless-prefix ":"
  "Prefix displayed instead of package namespace."
  :type 'string)

(defcustom nameless-global-aliases '(("fl" . "font-lock"))
  "Alist from aliases to namespaces.
This alist is used everywhere.  It is designed for namespaces you
use commonly.  To apply aliases specific to a file, set the
`nameless-aliases' variable with `add-file-local-variable'.

Each element of this list should have the form (ALIAS . NAMESPACE),
both strings.  For example, if you set this variable to
          ((\"fl\" . \"font-lock\"))
then expressions like `(font-lock-add-keywords nil kwds)' will
displayed as `(fl/add-keywords nil kwds)' instead.

Furthermore typing `fl' followed by `\\[nameless-insert-name]' will
automatically insert `font-lock-'."
  :type '(alist string string))

(defvar nameless-aliases nil
  "Alist from namespaces to aliases.
Samse syntax as `nameless-global-aliases', but designed to be
used as a file-local variable.")

(defface nameless-face
  '((t :inherit font-lock-type-face))
  "Face used on `nameless-prefix'")

(defcustom nameless-affect-indentation-and-filling 'outside-strings
  "If non-nil, code is indented and filled according to what you see.
If nil, code is indented and filled according to its actual content.
If the value is `outside-strings', behave like nil inside strings
and behave like t otherwise.

After changing this variable, you must reenable `nameless-mode'
for it to take effect."
  :type '(choice (const :tag "Always affect indentation" t)
                 (const :tag "Don't affect indentation" nil)
                 (const :tag "Only outside strings" 'outside-strings)))


;;; Font-locking
(defun nameless--make-composition (s)
  "Return a list that composes S if passed to `compose-region'."
  (cdr (apply #'append (mapcar (lambda (x) (list '(Br . Bl) x)) s))))

(defvar nameless-mode)
(defun nameless--compose-as (display)
  "Compose the matched region and return a face spec."
  (when nameless-mode
    (let ((compose (and nameless-affect-indentation-and-filling
                        (or (not (eq nameless-affect-indentation-and-filling 'outside-strings))
                            (not (nth 3 (syntax-ppss))))))
          (dis (concat display nameless-prefix)))
      (when compose
        (compose-region (match-beginning 1)
                        (match-end 1)
                        (nameless--make-composition dis)))
      `(face nameless-face ,@(unless compose (list 'display dis))))))

(defvar-local nameless--font-lock-keywords nil)

(defun nameless--ensure ()
  (save-excursion
    (font-lock-fontify-region (point-min) (point-max))))

(defun nameless--remove-keywords ()
  "Remove font-lock keywords set by `nameless--add-keywords'."
  (font-lock-remove-keywords nil nameless--font-lock-keywords)
  (setq nameless--font-lock-keywords nil)
  (nameless--ensure))

(defun nameless--add-keywords (&rest r)
  "Add font-lock keywords displaying REGEXP as DISPLAY.

\(fn (regexp . display) [(regexp . display) ...])"
  (setq-local font-lock-extra-managed-props
              `(composition display ,@font-lock-extra-managed-props))
  (let ((kws (mapcar (lambda (x) `(,(nameless--name-regexp (cdr x)) 1 (nameless--compose-as ,(car x)) prepend)) r)))
    (setq nameless--font-lock-keywords kws)
    (font-lock-add-keywords nil kws t))
  (nameless--ensure))


;;; Name and regexp
(defvar-local nameless-current-name nil)

(defun nameless--in-arglist-p ()
  "Is point inside an arglist?"
  (save-excursion
    (ignore-errors
      (backward-up-list)
      (or (progn (forward-sexp -1)
                 (looking-at-p "[a-z-]lambda\\_>"))
          (progn (forward-sexp -1)
                 (looking-at-p "\\(cl-\\)?def\\(un\\|macro\\|inline\\)\\*?\\_>"))))))

(defun nameless-insert-name (&optional self-insert)
  "Insert the name of current package, with a hyphen."
  (interactive "P")
  (cond
   ((or self-insert
        (not nameless-current-name)
        (eq (char-before) ?\\)
        (nameless--in-arglist-p))
    (call-interactively #'self-insert-command))
   ((string-match (rx (or (syntax symbol)
                          (syntax word)))
                  (string (char-before)))
    (let* ((r (point))
           (l (save-excursion
                (forward-sexp -1)
                (skip-chars-forward "^[:alnum:]")
                (point)))
           (alias (buffer-substring l r))
           (full-name (when alias
                        (cdr (or (assoc alias nameless-aliases)
                                 (assoc alias nameless-global-aliases))))))
      (if full-name
          (progn (delete-region l r)
                 (insert full-name "-"))
        (call-interactively #'self-insert-command))))
   (t (insert nameless-current-name "-"))))

(defun nameless--name-regexp (name)
  "Return a regexp of the current name."
  (concat "\\_<@?\\(" (regexp-quote name) "-\\)\\(\\s_\\|\\sw\\)"))


;;; Minor mode
;;;###autoload
(define-minor-mode nameless-mode
  nil nil " :" '(("_" . nameless-insert-name))
  (if nameless-mode
      (if (or nameless-current-name
              (ignore-errors (string-match "\\.el\\'" (lm-get-package-name))))
          (progn
            (unless nameless-current-name
              (setq nameless-current-name (replace-regexp-in-string "\\.[^.]*\\'" "" (lm-get-package-name))))
            (apply #'nameless--add-keywords
                   `((nil . ,nameless-current-name)
                     ,@nameless-global-aliases
                     ,@nameless-aliases)))
        (nameless-mode -1))
    (setq nameless-current-name nil)
    (nameless--remove-keywords)))

(provide 'nameless)
;;; nameless.el ends here
