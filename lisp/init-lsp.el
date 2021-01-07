;;; init-lsp.el --- LSP configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs LSP configurations.
;;

;;; Code:

(use-package lsp-mode
  :commands lsp
  :hook ((c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ("M-SPC" . lsp-signature-activate))
  :init
  (setq
   lsp-clients-clangd-args
   '("--header-insertion=never")))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; (use-package lsp-ui
;;     :config
;;     (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;     (add-hook 'lsp-mode-hook 'lsp-ui-sideline-mode)
;;     ;(add-hook 'lsp-mode-hook 'lsp-ui-doc-mode)
;;     (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;     (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;     (setq lsp-ui-sideline-enable t
;; 	  lsp-ui-doc-enable nil
;; 	  lsp-ui-flycheck-enable t
;; 	  lsp-ui-imenu-enable t
;; 	  lsp-ui-sideline-ignore-duplicate t)
;;     )

;; (use-package company-lsp
;;   :ensure t
;;   :after company lsp-mode
;;   :init
;;   (setq company-lsp-cache-candidates nil
;; 	company-lsp-cache-candidates 'auto
;; 	company-lsp-async t
;; 	company-lsp-enable-recompletion t)
;;   :config
;;   (push 'company-lsp company-backends)
;;   )

;; (use-package ccls
;;   :ensure t
;;   :config
;;   (setq ccls-executable "ccls"
;; 	ccls-args '("--log-file=/tmp/ccls.log")
;; 	ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
;;   )

;;; dap
;; (use-package dap-mode
;;   :ensure t
;;   :config
;;   (dap-mode 1)
;;   (require 'dap-hydra)
;;   (require 'dap-gdb-lldb)
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1))

;; (defcustom lsp-dir-whitelist
;;   '("/ha3_develop/search_test/temp/source_code/ha3"
;;     "/ha3_develop/search_test/temp/source_code/sap_easy"
;;     "/ha3_develop/search_test/temp/source_code/matchdoc"
;;     "/ha3_develop/search_test/temp/source_code/suez"
;;     "/ha3_develop/search_test/temp/source_code/suez_turing")
;;   "only dir in this whitelist will be prompt to enable lsp"
;;   :type 'list
;;   :group 'lsp)

;; (defun my/lsp ()
;;   (when (and (buffer-file-name)
;; 	     (cl-some (lambda (d) (f-descendant-of-p (buffer-file-name) d))
;; 		      lsp-dir-whitelist))
;;     (lsp)))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
