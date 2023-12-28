;;; init-package.el --- initialize package system -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)

(defvar  nibon7/elpa-mirror-alist '((tuna . "http://mirrors.tuna.tsinghua.edu.cn")
				    (ustc . "http://mirrors.ustc.edu.cn")))

(defvar  nibon7/elpa-repositories '("gnu" "nongnu" "melpa"))

(defvar  nibon7/elpa-enable-mirror t)

(defun nibon7/elpa-set-mirror (mirror)
  "Use mirror to accelerate package installation.
Replace the official repositories with the given MIRROR."
  (if-let* ((mirror-entry (assq mirror nibon7/elpa-mirror-alist))
	    (baseurl (cdr mirror-entry)))
      (setq package-archives
	    (mapcar (lambda (repo) (cons repo (format "%s/elpa/%s/" baseurl repo)))
		    nibon7/elpa-repositories))))

(if nibon7/elpa-enable-mirror
    (nibon7/elpa-set-mirror 'tuna)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t
      use-package-always-defer t)

(use-package diminish)

(provide 'init-package)

;;; init-package.el ends here
