;;; early-init.el --- early init -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(let ((minver "29.3"))
  (when (version< emacs-version minver)
    (error (format "Emacs %s or later required" minver))))

;; disable package at early stage
(setq package-enable-at-startup nil)

;; don't use precious startup time checking mtime on elisp bytecode
(setq load-prefer-newer noninteractive)

;; Garbage collection is a big contributor to startup times. This fends it
;; off, but will be reset later by `gcmh-mode'. Not resetting it later will
;; cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; don't show startup screen
(setq inhibit-startup-screen t)

;; don't resize the frame at early stage
(setq frame-inhibit-implied-resize t)

;; don't show menu bar, toolbar and scroll bar
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))

(provide 'init-early)

;;; early-init.el ends here
