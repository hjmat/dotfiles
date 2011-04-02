;;
;; This is my .emacs, there are many like it but this one is mine.
;;

;; Paths
(add-to-list 'load-path "~/.emacs.d/")

;; Backups
(defun make-backup-file-name (file)
(concat "~/.emacs_backups/" (file-name-nondirectory file) "~"))

;; Tabs
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; Navigation
(setq scroll-step 1)
(line-number-mode 1)
(column-number-mode 1)

;; Colouring
(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(set-default-font "-*-courier-*-12-*-mac-roman")
(set-face-background 'region "yellow")
(set-variable 'tool-bar-mode nil)

;; Parenthesis matching
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

;; Commenting
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-v" 'uncomment-region)

;; C
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;; Haskell
(require 'inf-haskell) ;; Inferior Haskell
(setq haskell-font-lock-symbols t) ;; Fancy unicode lambdas
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
