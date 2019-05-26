;;; init-wsl.el --- additional init file for WSL
;;; Commentary:
;;; Code:

;; (bind-keys* ("C-\\"    . undo)
;; 	    ("C-x C-n" . set-mark-command)
;;             ("M-]"     . indent-all))

(setq initial-frame-alist
      (append (list
	       '(width . 84)
	       '(height . 36)
	       '(top . 50)
	       '(left . 100)
	       )
	      initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

(set-language-environment "Japanese")
(use-package mozc
  :commands mozc-mode
  :bind ([zenkaku-hankaku] . toggle-input-method)
  :init
  (setq default-input-method "japanese-mozc")
  (advice-add 'toggle-input-method :before 'mozc-mode)
  :config
  (advice-remove 'toggle-input-method 'mozc-mode))

(when (window-system)
  (let ((font-list (font-family-list)))
    (cond ((member "Ricty" font-list)
           (set-frame-font "Ricty-11.5"))
          ((member "Ricty Diminished" font-list)
           (set-frame-font "Ricty Diminished-11.5"))))
  (set-face-attribute 'region nil :background "#FFEE99"))



;;; init-wsl.el ends here
