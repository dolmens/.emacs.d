(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  )
;; (defun call-magit-status()
;;   "open magit status"
;;   (interactive) 
;;   (save-some-buffers t)
;;   (call-interactively 'magit-status)
;;   )
;; (global-set-key (kbd "C-x g") 'call-magit-status)

(setq magit-status-margin '(t age magit-log-margin-width t 18))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "blue" :foreground "white"))))
 '(magit-branch-current ((t (:inherit magit-branch-local :box 1))))
 '(magit-diff-added ((t (:foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:foreground "#22aa22"))))
 '(magit-diff-base ((t (:background "#ffffcc" :foreground "#aaaa11"))))
 '(magit-diff-base-highlight ((t (:background "#eeeebb" :foreground "#aaaa11"))))
 '(magit-diff-conflict-heading ((t nil)))
 '(magit-diff-context ((t nil)))
 '(magit-diff-context-highlight ((t nil)))
 '(magit-diff-file-heading ((t (:weight bold))))
 '(magit-diff-file-heading-highlight ((t (:inherit magit-section-highlight))))
 '(magit-diff-file-heading-selection ((t (:inherit magit-diff-file-heading-highlight :foreground "salmon4"))))
 '(magit-diff-hunk-heading ((t nil)))
 '(magit-diff-hunk-heading-highlight ((t nil)))
 '(magit-diff-hunk-heading-selection ((t (:inherit magit-diff-hunk-heading-highlight :foreground "salmon4"))))
 '(magit-diff-lines-boundary ((t (:inherit magit-diff-lines-heading))))
 '(magit-diff-lines-heading ((t (:inherit magit-diff-hunk-heading-highlight :background "LightSalmon3"))))
 '(magit-diff-our ((t (:inherit magit-diff-removed))))
 '(magit-diff-our-highlight ((t nil)))
 '(magit-diff-removed ((t (:foreground "#aa2222"))))
 '(magit-diff-removed-highlight ((t (:foreground "#aa2222"))))
 '(magit-diff-their ((t (:inherit magit-diff-added))))
 '(magit-diff-their-highlight ((t (:inherit magit-diff-added-highlight))))
 '(magit-dimmed ((t (:foreground "grey50"))))
 '(magit-hash ((t (:foreground "green"))))
 '(magit-item-highlight ((t (:inherit nil))))
 '(magit-section-highlight ((t nil)))
 '(magit-section-secondary-heading ((t (:foreground "green" :weight normal))))
 '(magit-log-date ((t (:foreground "white")))))

;;(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(provide 'init-magit)
