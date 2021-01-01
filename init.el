;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Package load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
