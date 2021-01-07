;;; init-funcs.el --- Common functions configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Common functions configurations.
;;

;;; Code:

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (version< emacs-version "27")
  (require 'eshell)
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun grepcpp(command-args)
  (interactive
   (list (read-from-minibuffer "Run grep *.cpp *.cc *.h: "
                               grep-cpp-command
                               nil
                               nil
                               'grep-cpp-history)))
   (grep command-args))

(defun greppy(command-args)
  (interactive
   (list (read-from-minibuffer "Run grep *.py: "
                               grep-py-command
                               nil
                               nil
                               'grep-py-history)))
   (grep command-args))

(defun grepet(command-args)
  (interactive
   (list (read-from-minibuffer "Run grep ex test *.cpp *.h: "
                               grep-etest-command
                               nil
                               nil
                               'grep-etest-history)))
   (grep command-args))

(setq grep-command "grep -r -nHi ")
(setq grep-cpp-command "grep --include=\"*\\.cpp\" --include=\"*\\.cc\" --include=\"*\.h\" -r -nHi -e ")
(setq grep-etest-command "grep --exclude=\"*Test*\" --exclude=\"*test*\" -r -nHi -e ")
(setq grep-py-command "grep --include=\"*\\.py\" -r -nHi ")

(provide 'init-common-functions)

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
