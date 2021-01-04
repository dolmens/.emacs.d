;;; init-org.el --- Orgmode configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Orgmode configurations.
;;

;;; Code:

(use-package org
  :pin org
  :ensure
  org-plus-contrib)

(use-package org-superstar
  :hook ((org-mode . org-superstar-mode)))

(defun my/org-mode-hook ()
  (set-face-attribute 'org-level-1 nil :height 1.0 :box nil :background nil)
  (set-face-attribute 'org-level-2 nil :height 1.0 :box nil :background nil)
  (set-face-attribute 'org-level-3 nil :height 1.0 :box nil :background nil)
  (set-face-attribute 'org-level-4 nil :height 1.0 :box nil :background nil))
(add-hook 'org-mode-hook 'my/org-mode-hook)


(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
