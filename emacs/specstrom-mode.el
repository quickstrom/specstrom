;; a simple major mode for Specstrom

(setq specstrom-font-lock-keywords
      (let* (
             ;; define several category of keywords
             (x-keywords '("let" "freeze" "forall" "exists" "check" "import" "with" "when"))
             (x-constants '("true" "false" "null"))
             

             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-constants-regexp (regexp-opt x-constants 'words))
             )

        `(

          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-constants-regexp . font-lock-constant-face)

          ("\\(?:[^ ]+?\\?\\)" . font-lock-builtin-face)
          ("\\(?:[^ ]+?\\!\\)" . font-lock-builtin-face)

          ;;(,x-builtin-regexp . font-lock-function-name-face)

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

(add-to-list 'auto-mode-alist '("\\.strom\\'" . specstrom-mode))
 
(provide 'specstrom-mode)
