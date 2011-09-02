;;; .emacs --- This is my .emacs, there are many like it but this one is mine.
;; Dependencies: haskell-mode, inferior haskell, speedbar, auto-complete 1.3.1,
;;               erlang-mode

;; Startup
(setq emacs-d "~/.emacs.d")
(setq local-elisp-path (concat emacs-d "/elisp"))
(add-to-list 'load-path emacs-d)
(add-to-list 'load-path local-elisp-path)
(add-to-list 'load-path (concat local-elisp-path "/erlang"))
(setq inhibit-startup-message t)

;; Platform specific config
(defun configure-for-darwin ()
  (setq erlang-home "/opt/local/lib/erlang")
  )

(defun configure-for-linux ()
  (setq erlang-home "/usr/lib/erlang")
  )

(setq platform-configs
      '((darwin . configure-for-darwin)
        (gnu/linux . configure-for-linux))
      )

(setq platform-config (assoc system-type platform-configs))
(when platform-config
  (funcall (cdr platform-config))
  (message (format "Set up for '%s'" (car platform-config)))
)

;; Backups
(defun make-backup-file-name (file)
  (concat emacs-d "auto-save-list/" (file-name-nondirectory file) "~"))

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

;; Soft require
(defun soft-require (f &optional cfg-fun &optional suppress-output)
  "If the feature f exists, load and configure it with the function cfg-fun"
  (when (require f nil t)
    (when cfg-fun (funcall cfg-fun))
    (when (not suppress-output)
      (message "'%s' loaded" f))
    )
  )

;; Electric pair
(defun electric-pair ()
  (interactive)
  (if (eolp)
      (let (parens-require-spaces) (insert-pair)) (self-insert-command 1))
  )

;; Auto-complete
(defun configure-my-ac-mode ()
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  )
(soft-require 'auto-complete-config 'configure-my-ac-mode)
(soft-require 'auto-complete-extension)

;; C
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;; Haskell
(defun haskell-electric-pair ()
  (define-key haskell-mode-map "(" 'electric-pair)
  (define-key haskell-mode-map "[" 'electric-pair)
  (define-key haskell-mode-map "{" 'electric-pair)
  )

(defun configure-haskell-with-ac-mode ()
  (add-to-list 'ac-modes 'haskell-mode)
  (when 'ac-source-haskell
    (add-to-list 'ac-sources 'ac-source-haskell))
  )

(defun configure-my-haskell-mode ()
  (soft-require 'inf-haskell)
  (soft-require 'speedbar (lambda () (speedbar-add-supported-extension ".hs")))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'haskell-electric-pair)
  (soft-require 'auto-complete-config 'configure-haskell-with-ac-mode t)
  )
(soft-require 'haskell-mode 'configure-my-haskell-mode)

;; Erlang
(defun configure-my-erlang-mode ()
  (setq erlang-root-dir erlang-home)
  (setq erlang-bin-dir (concat erlang-root-dir "/bin"))
  (setq exec-path (cons erlang-bin-dir exec-path))
  )
(soft-require 'erlang-start 'configure-my-erlang-mode)
