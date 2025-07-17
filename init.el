;;; init.el --- my config file for emacs
;;; Commentary:
;;; Code:
;;
;; Package settings
;;
(require 'package)
(setq package-user-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package bind-key
  :ensure t)

;;
;; load init file specific to the environment
;;
(defun local-init-file ()
  "Return local init file name for the system where Emacs is running."
  (cond ((eq system-type 'gnu/linux)
         (cond ((string-match-p "Microsoft" (shell-command-to-string "uname -r")) "init-wsl")
               ((window-system) "init-linux-desktop")
               (t "init-linux-server")))
        ((eq system-type 'windows-nt) "init-windows")
        (t "init-local")))

(let ((file (expand-file-name (local-init-file) user-emacs-directory)))
  (if (or (file-readable-p (concat file ".elc"))
          (file-readable-p (concat file ".el")))
      (load file)))

(setq custom-file (expand-file-name ".custom-file.el" user-emacs-directory))
;; (if (file-readable-p custom-file)
;;     (load-file custom-file))

(unless backup-directory-alist
  (setq backup-directory-alist
	`((".*" . ,(expand-file-name "backup" user-emacs-directory)))))

(setq inhibit-startup-message t)

(if (version<= "26.1" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode t))
(line-number-mode t)
(column-number-mode t)

(tool-bar-mode 0)
(menu-bar-mode 0)

(ido-mode 1)

(setq frame-title-format (format "%%b - Emacs %s" emacs-version))

(setq-default line-spacing 0.1)

(delete-selection-mode t)

(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(when window-system
  (global-hl-line-mode t)
  (set-face-background 'hl-line "#ccffff"))
(show-paren-mode t)

(when (not window-system)
  ;; color setup
  (set-face-foreground 'font-lock-builtin-face "brightblue")
  (set-face-foreground 'font-lock-comment-face "color-132")
  (set-face-foreground 'font-lock-function-name-face "color-32")
  (set-face-foreground 'font-lock-string-face "color-197")
  (set-face-foreground 'minibuffer-prompt "brightblue")
  (add-hook 'linum-mode-hook
            (lambda()
              (set-face-foreground 'linum "#999999"))))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

(setq-default show-trailing-whitespace t)
(dolist (hook (list
               'buffer-menu-mode-hook
               'shell-mode-hook
               'term-mode-hook
               'eshell-mode-hook))
  (add-hook hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

(setq backup-by-copying-when-linked t)

(setq scroll-conservatively 1
      scroll-preserve-screen-position t
      scroll-error-top-bottom t)

(defun add-parent-directory-to-buffer-name (&rest args)
  (let ((file (or buffer-file-name dired-directory)))
    (when file
      (when (not (string= file "~/"))
        (setq file (abbreviate-file-name (directory-file-name file))))
      (let* ((parent (directory-file-name
                      (file-name-directory file)))
             (dir (if (string-match "\\`/[^/]*\\'" parent)
                      parent
                    (file-name-nondirectory parent))))
        (rename-buffer (concat
                        (file-name-as-directory dir)
                        (file-name-nondirectory file)))))))
(add-hook 'find-file-hook 'add-parent-directory-to-buffer-name)

(defun switch-buffer-skipping-special-buffer (next)
  (let ((start-buf (buffer-name))
        (func (if next 'next-buffer 'previous-buffer)))
    (funcall func)
    (while (and (string-match "\\`\\*.+\\*\\'" (buffer-name))
                (not (string= start-buf (buffer-name))))
      (funcall func))))
(defun next-buffer-skipping-special-buffer ()
  (interactive)
  (switch-buffer-skipping-special-buffer t))
(defun previous-buffer-skipping-special-buffer ()
  (interactive)
  (switch-buffer-skipping-special-buffer nil))
(bind-key "C-<right>" 'next-buffer-skipping-special-buffer)
(bind-key "C-<left>" 'previous-buffer-skipping-special-buffer)

(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to `revert-buffer'.  Ignoring the auto-save
 file and not requesting for confirmation.  When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer t t t)
    (error "The buffer has been modified")))
(bind-key "<f5>" 'revert-buffer-no-confirm)

(bind-key "C-m" 'reindent-then-newline-and-indent)
(bind-key "C-j" 'newline)
(defun indent-all ()
  (interactive)
  (indent-region (point-min) (point-max)))
(bind-key "C-M-]" 'indent-all)

(defun newline-at-the-end-of-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(bind-key "C-M-j" 'newline-at-the-end-of-line)

(bind-key "C-k" 'kill-whole-line)
(bind-key "M-k" 'kill-line)

(bind-key "C-x k" 'kill-current-buffer)
(defun kill-current-buffer-and-window()
  (interactive)
  (kill-current-buffer)
  (delete-window))
(bind-key "C-x 9" 'kill-current-buffer-and-window)

;;(bind-key "C-x C-r" 'replace-string)
(bind-key "C-x C-r" 'replace-regexp)

(bind-key "C-c <left>"  'windmove-left)
(bind-key "C-c <right>" 'windmove-right)
(bind-key "C-c <up>"    'windmove-up)
(bind-key "C-c <down>"  'windmove-down)

(defun duplicate-current-line-or-region (n up)
  (let* (kill-ring
         kill-ring-yank-pointer
         (deactivate-mark nil)
         (inhibit-message t)
         (region (region-active-p))
         (col (current-column))
         (lines
          (if region (count-lines (region-beginning) (region-end)) 1)))
    (when (region-active-p) (goto-char (region-beginning)))
    (kill-whole-line lines)
    (dotimes (i (1+ n)) (yank))
    (forward-line (if up (- (* (1+ n) lines)) (- lines)))
    (if region
        (progn
          (push-mark nil t)
          (forward-line lines))
      (move-to-column col))))
(defun duplicate-up-current-line-or-region (n)
  "Duplicates current line or region to the top of it.
The line or region is duplicated `N` times."
  (interactive "p")
  (duplicate-current-line-or-region n t))
(defun duplicate-down-current-line-or-region (n)
  "Duplicates current line or region to the bottom of it.
The line or region is duplicated `N` times."
  (interactive "p")
  (duplicate-current-line-or-region n nil))
(bind-key "C-M-<up>" 'duplicate-up-current-line-or-region)
(bind-key "C-M-<down>" 'duplicate-down-current-line-or-region)

(defun move-current-line (up)
  (let (kill-ring
        kill-ring-yank-pointer
        (inhibit-message t)
        (col (current-column)))
    (kill-whole-line)
    (forward-line (if up -1 1))
    (yank)
    (forward-line -1)
    (move-to-column col)))
(defun move-up-current-line ()
  (interactive)
  (move-current-line t))
(defun move-down-current-line ()
  (interactive)
  (move-current-line nil))
(bind-key "M-<up>" 'move-up-current-line)
(bind-key "M-<down>" 'move-down-current-line)

(use-package ruby-mode
  :defer t
  :config
  (add-hook 'ruby-mode-hook
            (lambda ()
              (setq ruby-deep-indent-paren-style nil
                    ruby-insert-encoding-magic-comment nil)
              (global-rbenv-mode 1)
              (flycheck-mode 1))))
(use-package rbenv
  :commands (global-rbenv-mode rbenv-use rbenv-use-system rbenv-use-corresponding)
  :ensure t)

(defun dired-find-file-or-alternate-directory ()
  (interactive)
  (if (file-directory-p (dired-get-file-for-visit))
      (dired-find-alternate-file)
    (dired-find-file)))
(add-hook 'dired-load-hook
          (lambda ()
            (setq dired-listing-switches "-alhF"
                  line-spacing nil)
            (put 'dired-find-alternate-file 'disabled nil)
            (bind-keys :map dired-mode-map
                       ("RET" . dired-find-file-or-alternate-directory)
                       ([mouse-2] . dired-find-file-or-alternate-directory))))
(use-package dired-toggle
  :defines dired-toggle-window-side dired-toggle-window-size
  :bind ("C-x C-d" . dired-toggle)
  :config
  (setq dired-toggle-window-side 'below
        dired-toggle-window-size 8))

(use-package sh-script
  :defer t
  :config
  (sh-electric-here-document-mode -1))

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2)
  :ensure t)

(use-package markdown-mode
  :mode ("\\.markdown\\'" "\\.md\\'")
  :no-require t
  :ensure t)

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :no-require t
  :ensure t)

(use-package sed-mode
  :mode "\\.sed\\'"
  :interpreter "sed"
  :no-require t
  :ensure t)

(use-package slim-mode
  :mode "\\.slim\\'"
  :config
  (bind-key "C-m" 'newline-and-indent slim-mode-map)
  :ensure t)

(use-package php-mode
  :mode "\\.php\\'"
  :config
  (add-hook 'php-mode-hook
            (lambda ()
              (setq c-basic-offset 2))))

(use-package multi-term
  :commands multi-term
  :config
  (add-hook 'term-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)
              (linum-mode 0)
              (hl-line-mode 0)
              (set-face-background 'hl-line (face-background 'default))))
  :no-require t
  :ensure t)

(use-package magit
  :bind ("C-c C-g" . magit-status))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :ensure t)

(use-package flycheck
  :config
  (global-flycheck-mode)
  :ensure t)

(use-package company
  :bind ("C-SPC" . company-complete)
  :config
  (global-company-mode)
  :ensure t)

(use-package ng2-mode
  :mode "\\.ts\\'"
  :defines typescript-indent-level
  :config
  (add-hook 'ng2-typescript-mode-hook
            (lambda ()
              (setq typescript-indent-level 2)))
  :no-require t)

(use-package web-mode
  :mode ("\\.html?\\'" "\\.erb\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2)
  (set-face-foreground 'web-mode-doctype-face "dark cyan")
  (set-face-foreground 'web-mode-html-tag-face "blue")
  :ensure t)

(use-package emmet-mode
  :commands emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode)
  :config
  (puthash "thymeleaf"
           "!!!+doc[lang=ja xmlns:th=http://www.thymeleaf.org]" emmet-tag-aliases-table)
  (puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
                           (puthash "th:action" "" tbl)
                           (puthash "method" "post" tbl) tbl)
           (gethash "form" emmet-tag-settings-table))
  :ensure t)

(use-package dockerfile-mode
  :mode "\\`Dockerfile.*"
  :ensure t)

(defun compile-init-el ()
  "Compile init.el and my local init file."
  (dolist (file (list
                 user-init-file
                 (expand-file-name (concat (local-init-file) ".el") user-emacs-directory)))
    (when (and file (file-readable-p file)
               (file-newer-than-file-p file (concat file "c")))
      (byte-compile-file file))))
(add-hook 'after-init-hook 'compile-init-el)
(add-hook 'kill-emacs-hook 'compile-init-el)

(provide 'init)
;;; init.el ends here
