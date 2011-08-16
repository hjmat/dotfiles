;;; .emacs --- This is my .emacs, there are many like it but this one is mine.
;; Dependencies: haskell-mode, inferior haskell, speedbar, auto-complete 1.3.1
;;

;; Startup
(add-to-list 'load-path "~/.emacs.d/")
(setq inhibit-startup-message t)

;; Backups
(defun make-backup-file-name (file)
  (concat "~/.emacs.d/auto-save-list" (file-name-nondirectory file) "~"))

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

;; soft-require
(defun soft-require (f &optional cfg-fun)
  "If the feature f exists, load and configure it with the function cfg-fun"
  (when (require f nil t)
    (progn
      (when cfg-fun (funcall cfg-fun))
      (message (format "'%s' loaded" f)))
    )
  )

(defun electric-pair ()
      (interactive)
      (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))

;; Auto-complete
(defun configure-my-ac-mode ()
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    (ac-config-default)
  )
(soft-require 'auto-complete-config 'configure-my-ac-mode)

;; C
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;; Haskell
(defun haskell-electric-pair ()
  (define-key haskell-mode-map "(" 'electric-pair)
  (define-key haskell-mode-map "[" 'electric-pair)
  (define-key haskell-mode-map "{" 'electric-pair)
  )

(defun configure-my-haskell-mode ()
    (soft-require 'inf-haskell)
    (soft-require 'speedbar (lambda () (speedbar-add-supported-extension ".hs")))
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'haskell-electric-pair)
  )
(soft-require 'haskell-mode 'configure-my-haskell-mode)
