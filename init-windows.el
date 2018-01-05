;; setting for tramp
(eval-when-compile (require 'tramp))
(with-eval-after-load 'tramp
  (setenv "PATH" (concat "C:\\Program Files (x86)\\PuTTY" ";" (getenv "PATH")))
  (setq tramp-default-method "plink")
  ;; tunneling
  (defun get-ip-address ()
    "Win32: get the IP-address of the first network interface"
    (let ((ipconfig (shell-command-to-string "ipconfig | findstr IPv4")))
      (string-match "\\(\\([0-9]+.\\)+[0-9]+\\)" ipconfig)
      (match-string 0 ipconfig)))
  (when (not (string-match "^133\\.103\\.101\\." (get-ip-address)))
    (add-to-list 'tramp-default-proxies-alist
                 '("uv20" nil "/plink:v2452:"))
    (add-to-list 'tramp-default-proxies-alist
                 '("ubuntu" nil "/uv20:"))
    (add-to-list 'tramp-default-method-alist
                 '("uv20\\|ubuntu" nil "ssh"))))

(set-frame-font "Ricty Diminished-12")
(set-default-font "Ricty Diminished-12")

(setq initial-frame-alist
      (append (list '(width . 82))
              initial-frame-alist))

(setq shell-file-name "C:/Windows/System32/bash.exe"
      multi-term-program shell-file-name)
