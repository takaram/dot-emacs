(bind-keys* ("C-\\"    . undo)
	    ("C-x C-n" . set-mark-command)
            ("M-]"     . indent-all))

(when (window-system)
  (set-frame-font "Ricty Diminished-11.5")
  (set-default-font "Ricty Diminished-11.5")
  (set-face-attribute 'region nil :background "#FFEE99"))
