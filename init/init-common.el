;;; init-common.el --- common settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; garbage collector magic hack
(use-package gcmh
  :hook after-init
  :config
  (nibon7/delight 'gcmh-mode))

;; vim-like key bindings
(use-package evil
  :hook after-init
  :custom
  (evil-want-C-i-jump nil)
  (evil-undo-system (when (>= emacs-major-version 28)
		      'undo-redo)))

;; highlight the cursor whenever the window scrolls
(use-package beacon
  :hook after-init)

;; vertical interactive completion
(use-package vertico
  :hook after-init
  :custom
  (vertico-cycle t))

;; orderless completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; consult
(use-package consult
  :bind
  ([remap apropos] . consult-apropos)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap locate] . consult-locate)
  ([remap load-theme] . consult-theme)
  ([remap man] . consult-man)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap yank-pop] . consult-yank-pop)
  ("C-c f" . consult-flymake)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<"
	consult-line-numbers-widen t
	consult-async-min-input 2
	consult-async-refresh-delay 0.15
	consult-async-input-throttle 0.2
	consult-async-input-debounce 0.1))

;; marginalia in the minibuffer
(use-package marginalia
  :hook after-init)

;; suggest next keys
(use-package which-key
  :hook after-init
  :config
  (nibon7/delight 'which-key-mode))

;; jump to visible text using a char-based decision tree
(use-package avy
  :bind
  ("C-c g c" . avy-goto-char-2)
  ("C-c g t" . avy-goto-char-timer)
  ("C-c g w" . avy-goto-word-0)
  ("C-c g g" . avy-goto-line))

;; switch window
(use-package ace-window
  :bind
  ([remap other-window] . ace-window))

;; split window and switch to it
(use-package emacs
  :ensure nil
  :config
  (defun nibon7/split-window-and-switch (direction &optional root)
    "Split window and switch to the new window.
The parameter DIRECTION is a symbol, possible values are `right'/`horizontally'
or `below'/`vertically' which means to split window horizontally or vertically.
If the optional parameter ROOT is non-nil, split the root window of current
frame, otherwise split the current window."
    (interactive
     (let*  ((completion-list '(right bellow horizontally vertically))
	     (input (completing-read "direction: " completion-list nil t)))
       (list (intern input))))
    (let ((buffer "*scratch*")
	  (switch-fn (intern (if root
				 (format "split-root-window-%s" direction)
			       (format "split-window-%s" direction)))))
      (if (symbol-function switch-fn)
	  (progn
	    (select-window (funcall switch-fn))
	    (unless (string-equal (buffer-name) buffer)
	      (switch-to-buffer buffer)))
	(user-error "%s is not a valid function" switch-fn))))
  (defun nibon7/split-window-right-and-switch ()
    "Split window horizontally and switch to the new window."
    (interactive)
    (nibon7/split-window-and-switch 'right))
  (defun nibon7/split-window-below-and-switch ()
    "Split window vertically and switch to the new window."
    (interactive)
    (nibon7/split-window-and-switch 'below))
  (defun nibon7/split-root-window-right-and-switch ()
    "Split root window horizontally and switch to the new window."
    (interactive)
    (nibon7/split-window-and-switch 'right t))
  (defun nibon7/split-root-window-below-and-switch ()
    "Split root window vertically and switch to the new window."
    (interactive)
    (nibon7/split-window-and-switch 'below t))
  :bind
  ([remap split-window-right] . nibon7/split-window-right-and-switch)
  ([remap split-window-below] . nibon7/split-window-below-and-switch)
  ([remap split-root-window-right] . nibon7/split-root-window-right-and-switch)
  ([remap split-root-window-below] . nibon7/split-root-window-below-and-switch))

;; emoji
(use-package emojify
  :hook
  (after-init . global-emojify-mode))

;; mode line
(use-package doom-modeline
  :hook after-init
  :custom
  (doom-modeline-support-imenu t)
  (doom-modeline-minor-modes t)
  (doom-modeline-hud t))

;; theme
(use-package doom-themes
  :init
  (load-theme 'doom-monokai-machine t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(provide 'init-common)

;;; init-common.el ends here
