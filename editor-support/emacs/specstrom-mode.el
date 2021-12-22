;; a simple major mode for Specstrom

(setq specstrom-font-lock-keywords
      (let* (
             ;; define several category of keywords
             (x-keyword '("fun" "syntax" "action" "timeout" "let" "macro" "check" "with" "when" "import" "left" "right"))
             (x-builtin '(;; builtins and control flow operators
                          "if"
                          "else"
                          "for"
                          "forall"
                          "exists"
                          "in"
                          "match"
                          "case"
                          ;; temporal operators
                          "not"
                          "next"
                          "nextF"
                          "nextT"
                          "always"
                          "eventually"
                          "until"
                          "changed"
                          "unchanged"
                          ))
             (x-operator '("~>"
                           "&&"
                           "||"
                           "+"
                           "-"
                           "*"
                           "/"
                           "~"
                           ))
             (x-constant '("true" "false" "null"))

             ;; generate regex string for each category of keywords
             (x-keyword-regexp (regexp-opt x-keyword 'words))
             (x-builtin-regexp (regexp-opt x-builtin 'words))
             (x-operator-regexp (regexp-opt x-operator 'symbols))
             (x-constant-regexp (regexp-opt x-constant 'words))
             )

        `(
          (,x-keyword-regexp . font-lock-keyword-face)
          (,x-builtin-regexp . font-lock-builtin-face)
          (,x-operator-regexp . font-lock-function-name-face)
          (,x-constant-regexp . font-lock-constant-face)

          ("\\(\\w+?[!?]\\)" . font-lock-type-face)
          ("\\(\\w+?[!?]\\)" . font-lock-type-face)

          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

(defconst specstrom-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?\/ "_ 123" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `specstrom-mode'.")

;;;###autoload
(define-derived-mode specstrom-mode prog-mode "Specstrom"
  "Major mode for editing Specstrom"
  :syntax-table specstrom-mode-syntax-table

  ;; code for syntax highlighting
  (setq comment-start "//")
  (setq font-lock-defaults '(
                             (specstrom-font-lock-keywords)
                             ))
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.strom\\'" . specstrom-mode))
 
(provide 'specstrom-mode)
