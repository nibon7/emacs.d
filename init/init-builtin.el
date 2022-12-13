;;; init-builtin --- builtin settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; default settings
(setq make-backup-files nil ;; don't make backup copies
      fill-column 72 ;; rfc2822 style
      indent-tabs-mode nil ;; don't insert tabs when indenting
      tab-width 4 ;; tab width
      large-file-warning-threshold 10485760 ;; warn large files larger than 10M
      read-file-name-completion-ignore-case t)


;; disable menu bar, toolbar and scroll bar
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; mode line settings
(setq mode-line-compact t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; use short answers
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

;; native compilation
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
	package-native-compile t))

;; auto fill
(add-hook 'after-init-hook #'auto-fill-mode)

;; highlight current line
(add-hook 'after-init-hook #'global-hl-line-mode)

;; revert buffers atomatically when the underlying file changes on the disk
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; comment
(global-set-key (kbd "C-x ;") 'comment-line)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; set default font
(when (window-system)
  (let* ((name "JetBrainsMono Nerd Font Mono")
	 (size 12)
	 (font (format "%s-%d" name size)))
    (add-to-list 'default-frame-alist `(font . ,font))
    (set-face-attribute 'default t :font `,font)))

(provide 'init-builtin)

;;; init-builtin.el ends here
