;;; init-org.el --- Org Mode Configuration -*- lexical-binding: t -*-

(use-package org
  :hook (org-mode . org-indent-mode) ; Indent text according to outline structure
  :config
  (setq org-ellipsis " ▾")           ; Fold symbol
  (setq org-hide-emphasis-markers t) ; Hide *bold* /param/ markers
  (setq org-src-fontify-natively t)  ; Syntax highlighting in code blocks
  (setq org-src-tab-acts-natively t) ; Tab acts as in language buffer
  (setq org-edit-src-content-indentation 0)
  
  ;; Org Babel Languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  
  ;; Auto-save Org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.1))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))

;; ----- Org Modern (Beautification) -----
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "◈" "◇" "✳" "wl" "▾" "▹"))
  (setq org-modern-list '((45 . "➤") (42 . "–") (43 . "•")))
  (setq org-modern-tag t)
  (setq org-modern-priority t)
  (setq org-modern-todo t)
  (setq org-modern-table t))


(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(require 'org-tempo)

;; ----- Org Roam (Optional - Knowledge Management) -----
;; Uncomment if you need Zettelkasten features
;; (use-package org-roam
;;   :custom (org-roam-directory (file-truename "~/Org/roam"))
;;   :config (org-roam-db-autosync-mode))

(provide 'init-org)
