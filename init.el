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

(require 'init-ui)

;; essential packages
(require 'init-common-packages)

;; function & commands
(require 'init-common-functions)

;; common editor settings
(require 'init-common)

;; the best editor for ever
;; (require 'init-evil)

;; ivy and other package from abo
(require 'init-ivy)

(require 'init-magit)

(require 'init-lsp)

(require 'init-c++)

(require 'init-python)

;;; specific custom-file to keep init.el clean
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; init.el ends here
