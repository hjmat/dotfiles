;;
;; This is my .emacs, there are many like it but this one is mine.
;;

;; Startup
(add-to-list 'load-path "~/.emacs.d/")
(setq inhibit-startup-message t)

;; Backups
(defun make-backup-file-name (file)
(concat "~/.emacs_backups/" (file-name-nondirectory file) "~"))

;; Tabs
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; Look and feel
(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(set-face-background 'region "yellow")
(set-variable 'tool-bar-mode nil)
(global-hl-line-mode t)
(setq scroll-step 1)
(line-number-mode 1)
(column-number-mode 1)
(setq-default show-trailing-whitespace t)

;; Parenthesis matching
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

;; Comments and other shortcuts
(global-set-key "\C-c\C-a" 'mark-whole-buffer)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-v" 'uncomment-region)

;; C
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;; Haskell
(featurep 'haskell-mode
          (progn
            (require 'haskell-mode)
            (featurep 'inf-haskell (require 'inf-haskell))
            (require 'speedbar)
            (speedbar-add-supported-extension ".hs")
            (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
            ))

;; Auto-complete
(featurep 'auto-complete-config
          (progn
            (require 'auto-complete-config)
            (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
            (ac-config-default))
          ))

