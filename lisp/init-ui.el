;;(setq inhibit-startup-message t)
;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("Hack" "SF Mono" "Source Code Pro" "Fira Code"
                         "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height (cond (sys/mac-x-p 130)
                                                    (sys/win32p 110)
                                                    (t 100))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Symbola" "Apple Symbols" "Symbol" "icons-in-terminal")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

;; enable line numbers
;;(global-linum-mode t)
;;(setq linum-format "%3d\u2502 ")
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(setenv "PAGER" "/bin/cat")

;; remove the toolbar, menu bar and scroll bars
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; no hard tabs
(setq indent-tabs-mode nil)

;; enable y and n for emacs queries
(defalias 'yes-or-no-p 'y-or-n-p)

;; buffers reflect external file changes
(global-auto-revert-mode t)

;; high light
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;transient-mark-mode
(setq transient-mark-mode t)

;; default to unified diffs
(setq diff-switches "-u")

;; kill the newline (CR) when killing whole line
(setq-default kill-whole-line t)

;;close bell
(setq visible-bell t)

;; mouse is great, especially when using trackpad
;; (unless window-system
;;   (require 'mouse)
;;   (xterm-mouse-mode t)
;;   (global-set-key [mouse-4] (lambda ()
;;                               (interactive)
;;                               (scroll-down 1)))
;;   (global-set-key [mouse-5] (lambda ()
;;                               (interactive)
;;                               (scroll-up 1)))
;;   (defun track-mouse (e))
;;   (setq mouse-sel-mode t)
;;   )

;; windowmove, currently trackpad
;; (global-set-key  (kbd "ESC <left>") 'windmove-left)
;; (global-set-key  (kbd "ESC <right>") 'windmove-right)
;; (global-set-key  (kbd "ESC <up>") 'windmove-up)
;; (global-set-key  (kbd "ESC <down>") 'windmove-down)
;; (global-set-key (kbd "M-o") 'other-window)

;; find related files
(global-set-key (kbd "C-x t") (lambda () (interactive) (ff-find-other-file nil t)))

;; better keep backup files
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/autosaves" t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      auto-save-file-name-transforms  '((".*" "~/.emacs.d/autosaves/\\1" t))
      delete-old-versions -1
      version-control t)

;; locale coding
(setq locale-coding-system'utf-8)
(prefer-coding-system'utf-8)
(set-keyboard-coding-system'utf-8)
(set-terminal-coding-system'utf-8)
(set-selection-coding-system'utf-8)
(set-clipboard-coding-system 'ctext)
(set-buffer-file-coding-system 'utf-8)

;; search prefer one key
(global-set-key [f3] 'isearch-forward)
(define-key isearch-mode-map [f3] 'isearch-repeat-forward)
(global-set-key (kbd "S-<f3>") 'isearch-forward-symbol-at-point)

(global-set-key [f6] 'compile)

(defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))

(when window-system
  ;; (use-package spaceline :ensure t
  ;;   :config
  ;;   (require 'spaceline-config)
  ;;   (spaceline-spacemacs-theme)
  ;;   )
	(use-package material-theme
		:ensure t
		:config
		(load-theme 'material-light t))
	(use-package doom-modeline
		:ensure t
		:hook (after-init . doom-modeline-mode))
	(use-package all-the-icons :ensure t))

(setq winner-boring-buffers
      '("*Completions*"
	"*Compile-Log*"
	"*inferior-lisp*"
	"*Fuzzy Completions*"
	"*Apropos*"
	"*Help*"
	"*cvs*"
	"*Buffer List*"
	"*Ibuffer*"
	"*esh command on file*"))
(winner-mode 1)

(setq-default indent-tabs-mode nil)

(provide 'init-ui)
