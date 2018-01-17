;;; sme-mode.el --- major mode for editing SME files. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2018, Truls Asheim

;; Author: Truls Asheim ( truls@asheim.dk )
;; Version: 1.0.0
;; Created: 13 Jan 2018
;; Keywords: languages
;; Homepage: https://github.com/truls/language-smeil/

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU
;; General Public License version 3.

;;; Commentary:

;; Simple major mode for editing SME hardware designs

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sme\\'" . sme-mode))

(defvar sme-mode-hook nil
  "Hook for `sme-mode` that is run whenever the mode is entered.")

(defconst sme-keywords
  '("barrier"
    "break"
    "bus"
    "case"
    "default"
    "elif"
    "else"
    "enum"
    "for"
    "func"
    "if"
    "import"
    "instance"
    "network"
    "of"
    "proc"
    "range"
    "return"
    "simulation"
    "switch"
    "to"
    "var"
    "where")
  "SME keywords.")

(defconst sme-types
  (let* ((ws "\\(?:[[:space:]]+\\|;\\|$\\)")
         (ws-opt "\\(?:[[:space:]]*\\|;\\)")
         (array-prefix (concat "\\(?:\\[[[:alnum:][:blank:]+-/*%<>&^|.]*\\]"
                               ws-opt
                               "\\)*")))
    (concat "\\(?:"
            array-prefix ws-opt "bool" ws
            "\\|"
            "in" ws
            "\\|"
            "out" ws
            "\\|"
            "const" ws
            "\\|"
            array-prefix ws-opt "f32" ws
            "\\|"
            array-prefix ws-opt "f64" ws
            "\\|"
            array-prefix ws-opt "i[[:digit:]]+" ws
            "\\|"
            array-prefix ws-opt "u[[:digit:]]+" ws
            "\\)" )
    )
  "Regexp for matching SME types."
  )

(defconst sme-properties
  `("sync"
    "async"
    "exposed")
  "SME definition modifiers."
  )

(defconst sme-constants
  `("true" "false")
  "Constants of SME (boolean values)."
  )

;; TODO: Highlight declarations properly
(defvar sme-font-lock-keywords)
(setq sme-font-lock-keywords
      (let* (
            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt sme-keywords 'words))
            (x-constants-regexp (regexp-opt sme-constants 'words))
            )

        `(
          (,x-keywords-regexp . font-lock-keyword-face)
          (,sme-types . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          ;;(,x-events-regexp . font-lock-builtin-face)
          (,(regexp-opt sme-properties 'words) . font-lock-builtin-face)
          ;(,(regexp-opt sme-properties 'words) . font-lock-function-name-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

(defvar sme-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Define // comments.
    (modify-syntax-entry ?\/ ". 12" st)
    (modify-syntax-entry ?\n ">" st)

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)

    ;; Variable names can include _
    (modify-syntax-entry ?\\ "_" st)
    st)
  "Syntax table used in `sme-mode'.")

;;; Indentation:

(defun sme-indent-line ()
  "Indent a line in SME-code."
  nil
  )

(defvar sme-indent-level 4
  "Indentation level for sme-mode.")

;;;###autoload
(define-derived-mode sme-mode prog-mode "sme mode"
  "Major mode for editing SME (Synchronous Message Exchange) files"
  :syntax-table sme-mode-syntax-table
  :group 'sme-mode
  ;; code for syntax highlighting
  (setq font-lock-defaults '((sme-font-lock-keywords)))
  (setq comment-start "//")
  (setq comment-end "")
  (setq indent-line-function 'sme-indent-line)
  )

(provide 'sme-mode)

;;; sme-mode.el ends here
