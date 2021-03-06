;;; .emacs --- This is my .emacs, there are many like it but this one is mine.
;;
;; Dependencies:
;;   - haskell-mode     https://github.com/haskell/haskell-mode
;;   - auto-complete    http://cx4a.org/software/auto-complete/
;;
;; Paths
;;   - ~/.emacs.d is the assumed root for all resources
;;   - el packages are installed in elisp
;;   - auto-complete dictionaries are stored in ac-dict
;;   - backups are stored in auto-save-list
;;

;; Startup
(setq emacs-d "~/.emacs.d")
(setq local-elisp-path (concat emacs-d "/elisp"))
(add-to-list 'load-path emacs-d)
(add-to-list 'load-path local-elisp-path)
(add-to-list 'load-path (concat local-elisp-path "/auto-complete"))
(add-to-list 'load-path (concat local-elisp-path "/haskell-mode"))

;; Backups
(defun make-backup-file-name (file)
  (concat emacs-d "/auto-save-list/" (file-name-nondirectory file) "~"))

;; package.el
(require 'package)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

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

(if (eq system-type 'darwin)
    (set-frame-font "-apple-dejavu sans mono-medium-r-normal--0-0-0-0-m-0-mac-roman")
  (set-frame-font "courier new-12")
  )

;; Parenthesis matching
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

;; Comments and other shortcuts
(global-set-key "\C-c\C-a" 'mark-whole-buffer)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-v" 'uncomment-region)

;; Soft require
(defun soft-require (f &optional cfg-fun)
  "If the feature f exists, load it use cfg-fun for configuration"
  (when (require f nil t)
    (when cfg-fun (funcall cfg-fun))
      (message "'%s' loaded" f))
  )

;; Column indicator
(defun configure-fci ()
  (setq fci-rule-column 80)
  (setq fci-rule-use-dashes t)
  )
(soft-require 'fill-column-indicator 'configure-fci)

;; Electric pair
(defun electric-pair ()
  (interactive)
  (if (eolp)
      (let (parens-require-spaces) (insert-pair)) (self-insert-command 1))
  )

;; Auto-complete
(defun configure-my-ac-mode ()
  (add-to-list 'ac-dictionary-directories (concat emacs-d "/ac-dict"))
  (ac-config-default)
  )
(soft-require 'auto-complete-config 'configure-my-ac-mode)
(soft-require 'auto-complete-extension)

;; Language customizations -----------------------------------------------------

;; C
(defun my-c-mode-hook ()
  (setq tab-width 4)
  )

(add-hook 'c-mode-hook 'my-c-mode-hook)

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
  (custom-set-variables
   '(haskell-mode-hook '(turn-on-haskell-indentation)))
  (soft-require 'speedbar (lambda () (speedbar-add-supported-extension ".hs")))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-electric-pair)
  (soft-require 'auto-complete-config 'configure-haskell-with-ac-mode)
  )
(soft-require 'haskell-mode 'configure-my-haskell-mode)

;; End of language customizations ----------------------------------------------

;; Automagically bytecompile .emacs
(defun last-write (filename)
  (if (file-exists-p filename)
      (nth 5 (file-attributes filename))
    '(0 0)
    )
  )

(defun file-is-newer (file1 file2)
  (setq time1 (last-write file1))
  (setq time2 (last-write file2))
  (if (> (car time1) (car time2))
      't
    (if (eq (car time1) (car time2))
        (> (car (cdr time1)) (car (cdr time2)))
      'nil)
    )
  )

(when (file-is-newer "~/.emacs" "~/.emacs.elc")
  (byte-compile-file "~/.emacs"))
