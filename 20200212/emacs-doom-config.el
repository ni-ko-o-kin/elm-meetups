;;;  package -- summary
;;; .doom.d/config.el -*- lexical-binding: t; -*-
;;; commentary:

;;; code:
(setq doom-font (font-spec :family "Source Code Pro" :size 22))

(define-key evil-normal-state-map "H" 'beginning-of-line)
(define-key evil-normal-state-map "L" 'end-of-line)
(define-key evil-visual-state-map "H" 'beginning-of-line)
(define-key evil-visual-state-map "L" 'end-of-line)

(setq treemacs-show-hidden-files nil)
(setq-default frame-title-format '((:eval default-directory)))
(setq-default org-duration-format (quote h:mm))
(setq-default create-lockfiles nil)

(after! org (setq org-startup-indented nil))

;;; multiple cursors
(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)
(define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
(define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-and-next)
(define-key evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)
(define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
(define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-and-prev)


(map!
 (:leader :desc "Toggle Treemacs" :n  "f t" #'treemacs)
 (:leader :desc "Swiper"          :n  "s s" #'swiper)
 (:leader :desc "Search Project"  :n  "s p" #'+default/search-project)
 (:leader :desc "Switch Treemacs Workspace"  :n  "p p" #'treemacs-switch-workspace)
 )

(add-hook 'elm-mode-hook #'lsp)

(provide 'config)
;;; config.el ends here
