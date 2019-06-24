
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("" . "~/.emacs.d/backups/"))))
 '(bookmark-save-flag 1)
 '(column-number-mode t)
 '(completion-cycle-threshold 6)
 '(custom-safe-themes
   (quote
	("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(delete-trailing-lines nil)
 '(display-line-numbers (quote relative))
 '(display-line-numbers-widen t)
 '(display-raw-bytes-as-hex t)
 '(display-time-24hr-format t)
 '(display-time-mode nil)
 '(frame-background-mode (quote light))
 '(global-hl-line-mode t)
 '(history-length 500)
 '(indicate-buffer-boundaries (quote right))
 '(isearch-allow-scroll nil nil nil "Maybe change this.")
 '(kept-new-versions 6)
 '(kept-old-versions 4)
 '(kill-do-not-save-duplicates t)
 '(package-selected-packages (quote (color-theme-solarized julia-mode)))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(read-quoted-char-radix 16)
 '(recenter-redisplay t nil nil "Change this so we redraw when calling `C-u C-l`.")
 '(register-separator 43)
 '(search-default-mode t)
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(vc-make-backup-files t)
 '(version-control t)
 '(whitespace-style
   (quote
	(face trailing tabs lines-tail empty indentation::space big-indent space-after-tab space-before-tab))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#eee8d5" :foreground "#586e75" :inverse-video t :box nil))))
 '(whitespace-tab ((t (:background "#dc322f" :foreground "#eee8d5")))))


(setq visible-bell 1)

(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-11"))

(let ((basedir "~/.emacs.d/themes/"))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
        (add-to-list 'custom-theme-load-path (concat basedir f)))))

(load-theme 'solarized t)

(put 'scroll-left 'disabled nil)
