;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Increase gc size when startup
(setq gc-cons-threshold 10000000)

;; Restore gc size after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; Package load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; submodules ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package settings, mainly use-package
(require 'init-package)

(require 'init-basic)

;; basic ui settings
(require 'init-ui)

;; function & commands
(require 'init-funcs)

;; ivy and other package from abo
(require 'init-ivy)

;; orgmode
(require 'init-org)

;; all misc
(require 'init-misc)

;; the best editor for ever
;; (require 'init-evil)

(require 'init-magit)

(require 'init-lsp)

(require 'init-prog)

;;; specific custom-file to keep init.el clean
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; init.el ends here
