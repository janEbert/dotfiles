;; -*- mode: elisp -*-

;; Build with:
;;    ./autogen.sh
;;    ./configure --with-modules [--with-xwidgets] \
;;                [--with-x-toolkit=lucid] [--prefix=...]
;;    [sudo] make install
;; Execute:
;;    emacsclient -c -a '' -F "'(fullscreen . maximized)"

;; Put external plugins into "~/.emacs.d/lisp".
;; Put themes into "~/.emacs.d/themes".
;;
;; External plugins to download:
;;   - [lsp-julia](https://github.com/non-Jedi/lsp-julia)
;;
;; Themes to download:
;;   - none yet
;;
;; [ctags](https://github.com/universal-ctags/ctags) for better tags.
;; [global](https://www.gnu.org/software/global/) for better tag integration.
;;
;; Build Universal Ctags using:
;;    ./autogen.sh
;;    ./configure --program-prefix=u [--prefix=...]
;;    make
;;    [sudo] make install
;;
;; Build GNU Global using:
;;    ./configure --with-universal-ctags=<ctagsbin> [--prefix=...]
;;    make
;;    [sudo] make install
(defconst my-emacs-dir "~/.emacs.d")
(defconst my-backup-dir (concat my-emacs-dir "/backups"))
(defconst my-extended-package-dir (concat my-emacs-dir "/lisp"))
(defconst my-themes-dir (concat my-emacs-dir "/themes"))

(defvar my-default-theme 'solarized-light)

(defconst my-gtags-dir "/usr/local/share/gtags")
(defconst my-julia-bin "~/local/bin/julia")
(defconst my-jupyter-dir "~/anaconda3/bin")

(defconst my-music-dir "~/Music/")

;; For faster initialization
(defvar my-tmp-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
	  gc-cons-percentage 0.6
	  file-name-handler-alist nil)

;; Restore (better) GC defaults afterwards
(add-hook 'after-init-hook ;; or 'emacs-startup-hook
		  '(lambda ()
			 (setq gc-cons-threshold 16777216
				   gc-cons-percentage 0.1
				   file-name-handler-alist my-tmp-file-name-handler-alist)))

;; Windows performance improvements (?)
(if (eq system-type 'windows-nt)
	(setq-default w32-pipe-read-delay 0
				  inhibit-compacting-font-caches t
				  bidi-display-reordering nil))

;; Prefer more recent files when loading.
(setq load-prefer-newer t)

;; This key binding here so it is loaded before errors
;; Edit init.el (C-c e)
(define-key mode-specific-map (kbd "e") 'find-init-file)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Add package lists
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
			 '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

;; Customized variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(backup-by-copying nil)
 '(backup-by-copying-when-linked t)
 '(before-save-hook (quote (time-stamp)))
 '(bookmark-save-flag 1)
 '(c-default-style
   (quote
	((c-mode . "k&r")
	 (c++-mode . "k&r")
	 (java-mode . "java")
	 (awk-mode . "awk")
	 (other . "java"))))
 '(column-number-mode t)
 '(completion-cycle-threshold 6)
 '(current-language-environment "UTF-8")
 '(delete-old-versions t)
 '(delete-trailing-lines nil)
 '(dired-always-read-filesystem t)
 '(display-battery-mode t)
 '(display-line-numbers (quote relative))
 '(display-line-numbers-widen t)
 '(display-raw-bytes-as-hex t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(electric-pair-mode t)
 '(electric-quote-comment nil)
 '(eshell-visual-commands
   (quote
	("vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "vim" "vimdiff" "tmux" "joe" "nano" "cmus" "htop" "ncdu" "mutt" "nethack" "crawl")))
 '(eshell-visual-options
   (quote
	(("git" "--help" "--paginate")
	 ("emacs" "-nw" "--no-window-system")
	 ("emacsclient" "-nw" "--no-window-system"))))
 '(eshell-visual-subcommands (quote (("git" "log" "reflog" "diff" "show"))))
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(grep-scroll-output t)
 '(history-length 500)
 '(hs-isearch-open t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere nil nil nil "Maybe enable? Keey an eye out for when this is useful.")
 '(indicate-buffer-boundaries (quote right))
 '(isearch-allow-scroll nil nil nil "Maybe change this.")
 '(kept-new-versions 6)
 '(kept-old-versions 4)
 '(kill-do-not-save-duplicates t)
 '(mail-signature nil)
 '(menu-bar-mode nil)
 '(message-kill-buffer-on-exit t)
 '(message-signature nil)
 '(org-agenda-files (quote ("~/Uni/smw-levelgen/plan.org")))
 '(package-archive-priorities (quote (("gnu" . 5) ("melpa-stable" . 3) ("melpa" . 2))))
 '(package-check-signature t)
 '(package-menu-hide-low-priority t)
 '(package-selected-packages
   (quote
	(jupyter use-package gotham-theme zenburn-theme toc-org flymake org tramp projectile ivy ggtags pdf-tools yasnippet solarized-theme rainbow-delimiters lsp-mode julia-mode helm gnu-elpa-keyring-update forge evil emms darkroom company)))
 '(prettify-symbols-unprettify-at-point (quote right-edge))
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(read-quoted-char-radix 16)
 '(recenter-redisplay t nil nil "Change this so we redraw when calling `C-u C-l`.")
 '(register-separator 43)
 '(save-place-mode t)
 '(savehist-additional-variables
   (quote
	(command-history search-ring regexp-search-ring kill-ring extended-command-history)))
 '(savehist-mode t)
 '(scroll-bar-mode (quote right))
 '(semantic-mode t)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(shell-prompt-pattern "^[^#$%>
]*[#$%>] *")
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis) nil nil "Maybe try out expression or mixed.")
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(show-trailing-whitespace t)
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(time-stamp-time-zone t)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote top))
 '(tramp-shell-prompt-pattern
   "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
 '(url-cookie-confirmation t)
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(which-function-mode t)
 '(whitespace-style (quote (face trailing lines-tail tab-mark)))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; For external plugins in 'my-extended-package-dir
(let ((default-directory my-extended-package-dir))
  (setq load-path
		(append
		 (let ((load-path  (copy-sequence load-path)))
		   (append
			(copy-sequence (normal-top-level-add-to-load-path '(".")))
			(normal-top-level-add-subdirs-to-load-path)))
		 load-path)))

;; Load themes in 'my-themes-dir
(let ((basedir my-themes-dir))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
        (add-to-list 'custom-theme-load-path (concat basedir f)))))


;; Load private configurations
(load (concat my-emacs-dir "/.private_config") t)

;; Do complete .bin files
(setq completion-ignored-extensions
	  (remove ".bin" completion-ignored-extensions))

(require 'eshell)
;; More Eshell visual commands
;; TODO eshell-visual-commands is not defined before M-x eshell.
;; Even (require 'eshell) does not change that.
;; (maybe with eshell-mode-hook?)
;; TODO end
;; (setq eshell-visual-commands
;; 	  (append eshell-visual-commands '("vim" "vimdiff" "tmux" "joe" "nano"
;; 									   "mg" "cmus" "htop" "ncdu" "nethack"
;; 									   "crawl" "jstar" "jmacs" "rjoe"
;; 									   "jpico")))

;; Set backup directory
(setq backup-directory-alist `(("." . ,my-backup-dir)))

;; Use visible bell instead of tone
(setq visible-bell t)

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Use flyspell for strings and comments by default
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Subword
(subword-mode 1)


;; Ido
;; (ido-mode 1)
(add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)

;; Use EDE everywhere
;; (global-ede-mode t) (conflicts with org-mode binding)
;; Change font
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-11"))

;; TRAMP
;; Load Eshell extensions
;; (add-to-list 'eshell-modules-list 'eshell-tramp) ; or em-tramp
;; Use X11 forwarding (-X)
;; TODO check if this works correctly
;;(add-to-list 'tramp-remote-process-environment
;;			 (format "DISPLAY=%s" (getenv "DISPLAY")))
(defun remote-shell ()
  "Start a remote shell with the correct TERM environment variable."
  (interactive)
  (let ((process-environment (cons "TERM=xterm-256color" process-environment)))
	(shell)))

;; Org
(setq org-directory "~/.emacs.d/org")
(setq org-disputed-keys
	  (quote
	   (;; these are the defaults
		([(shift up)] . [(control shift p)])
		([(shift down)] . [(control shift n)])
		([(shift left)] . [(meta -)])
		([(shift right)] . [(meta +)])
		([(control shift right)] . [(control meta +)])
		([(control shift left)] . [(control meta -)])
		;; these are custom
		([(control meta shift left)] . [(control meta shift b)])
		([(control meta shift right)] . [(control meta shift f)])
		([(control shift up)] . [(control meta shift p)])
		([(control shift down)] . [(control meta shift n)])
		([(meta up)] . [(meta shift p)])
		([(meta down)] . [(meta shift n)])
		([(meta left)] . [(meta shift b)])
		([(meta right)] . [(meta shift f)]))))
(setq org-log-done (quote time))
(setq org-replace-disputed-keys t)
(setq org-use-extra-keys t)
(setq org-use-speed-commands t)

;; (setq org-confirm-babel-evaluate nil)

;; If we ever use ob-async...
;; (setq ob-async-no-async-languages-alist
;; 	  '("jupyter-python" "jupyter-julia"))

(require 'org-install)
(require 'org-habit)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'turn-on-orgstruct)

(global-set-key (kbd "C-c n") 'org-footnote-action)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)

;; Set frame background
(setq frame-background-mode 'light)

;; Load theme
(load-theme my-default-theme t)


;; Package config

;; GNU Global
(add-to-list 'load-path my-gtags-dir)
(autoload 'gtags-mode "gtags" "" t)

(add-hook 'prog-mode-hook '(lambda () (gtags-mode 1)))

;; Ggtags
(add-hook 'c-mode-common-hook
		  (lambda ()
			(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
			  (progn (ggtags-mode 1))
			  (setq-local hippie-expand-try-functions-list
						  (cons 'ggtags-try-complete-tag
								hippie-expand-try-functions-list)))))

;; Magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(define-key magit-file-mode-map (kbd "C-c g") 'magit-file-dispatch)

;; EMMS
(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory my-music-dir)
(setq emms-stream-info-backend 'vlc)

(setq emms-random-playlist t)
(setq emms-repeat-playlist t)

(if (eq system-type 'gnu/linux)
	(setq emms-source-file-directory-tree-function
		  'emms-source-file-directory-tree-find))

(defun update-emms-faces ()
  "Change EMMS faces to be consistent with the rest of Emacs."
  (setq local-default-foreground (face-attribute 'default :foreground))
  (setq local-default-background (face-attribute 'default :background))
	(set-face-attribute 'emms-playlist-track-face nil
						:foreground local-default-foreground)
	(set-face-attribute 'emms-playlist-selected-face nil
						:background local-default-foreground
						:foreground local-default-background))

;; Start EMMS right away
(defun init-emmms ()
  "Load the playlist emms-music in 'my-music-dir, go to a random track and
stop playback."
  (interactive)
  (emms-play-playlist (concat my-music-dir "/emms-music"))
  (emms-random)
  (emms-stop)
  (update-emms-faces))

(update-emms-faces)

;; EMMS key bindings (C-c m)
(define-prefix-command 'my-emms-map)
(define-key mode-specific-map (kbd "m") 'my-emms-map)
(define-key my-emms-map (kbd "SPC") 'emms-pause)
(define-key my-emms-map (kbd "s") 'emms-stop)
(define-key my-emms-map (kbd "p") 'emms-previous)
(define-key my-emms-map (kbd "n") 'emms-next)
(define-key my-emms-map (kbd "r") 'emms-random)
(define-key my-emms-map (kbd "m") 'emms-shuffle)
(define-key my-emms-map (kbd "b") 'emms-seek-backward)
(define-key my-emms-map (kbd "f") 'emms-seek-forward)
(define-key my-emms-map (kbd "l") 'emms)
(define-key my-emms-map (kbd "i") 'init-emmms)
(define-key my-emms-map (kbd "+") 'emms-volume-raise)
(define-key my-emms-map (kbd "-") 'emms-volume-lower)

;; Evil mode
;;(require 'evil)
;; (evil-mode 1)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Ivy
;; (ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; Helm
(require 'helm-config)
;;(global-set-key (kbd "M-x") #'helm-M-x)
;;(global-set-key (kbd "C-x C-f") #'helm-find-files)
;;(global-set-key (kbd "C-x C-b") #'helm-mini) ; or helm-buffers-list
;;(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;;(global-set-key (kbd "C-s") #'helm-swoop) ; external package
;;(global-set-key (kbd "M-s o") #'helm-occur)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
;;(helm-mode 1)
;;(add-hook 'eshell-mode-hook
;;		  (lambda ()
;;			(eshell-cmpl-initialize)
;;			(define-key eshell-mode-map [remap eshell-pcomplete]
;;										'helm-esh-pcomplete)
;;			(define-key eshell-mode-map (kbd "M-p") 'helm-esh-history)))

;; Projectile
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; PDF-Tools
(pdf-tools-install)
;; or (pdf-loader-install)

;; toc-org
(if (require 'toc-org nil t)
    (progn
	  (add-hook 'org-mode-hook 'toc-org-mode)
	  (add-to-list 'org-tag-alist '("TOC" . ?T)))
  (warn "toc-org not found"))

;; Jupyter
(setq exec-path (append exec-path `(,(expand-file-name my-jupyter-dir))))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   ;;(julia . t)
   (python . t)
   (jupyter . t)))


;; Julia mode
(setq julia-program my-julia-bin)
(add-hook 'julia-mode-hook (lambda ()
							 (setq-local whitespace-line-column 92)))

;; ESS Julia

;; YASnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; or (next two)
;; (yas-reload-all)
;; (add-hook 'prog-mode-hook #'yas-minor-mode)

;; lsp-mode TODO maybe eglot?
(require 'lsp-mode)
;; (add-hook 'prog-mode-hook #'lsp)
;; (add-hook 'julia-mode-hook #'lsp) ; or maybe #'lsp-mode


;; Custom commands

(defun find-init-file ()
  "Find init.el in 'my-emacs-dir."
  (interactive)
  (find-file (concat my-emacs-dir "/init.el")))

(defun quit-other-window ()
  "Quit the other (next) window while staying in the selected window."
  (interactive)
  (select-window (next-window))
  (quit-window nil)
  (select-window (previous-window)))

(defun update-frame-background-mode ()
  "Update 'frame-background-mode for all frames."
  (mapc 'frame-set-background-mode (frame-list)))

(defun toggle-background ()
  "Toggle background brightness and reload theme."
  (interactive)
  (if (eq frame-background-mode 'dark)
	  (setq frame-background-mode 'light)
	(setq frame-background-mode 'dark))
  (update-frame-background-mode)
  (load-theme my-default-theme t)
  (update-emms-faces))

(defun toggle-solarized ()
  "Toggle Solarized light and dark."
  (interactive)
  (if (eq frame-background-mode 'dark)
	  (progn
		(setq frame-background-mode 'light)
		(update-frame-background-mode)
		(load-theme 'solarized-light t))
	(progn
	  (setq frame-background-mode 'dark)
	  (update-frame-background-mode)
	  (load-theme 'solarized-dark t)))
  (update-emms-faces))

(defun toggle-solarized-or-background ()
  "Toggle either Solarized light and dark or the background brightness depending
on if a Solarized variant is currently active."
  (interactive)
  (if (or
	   (equal (car custom-enabled-themes) 'solarized-light)
	   (equal (car custom-enabled-themes) 'solarized-dark))
	  (toggle-solarized)
	(toggle-background)))

(defun toggle-indent-tabs-mode ()
  "Toggle 'indent-tabs-mode."
  (interactive)
  (if (eq indent-tabs-mode nil)
	  (progn
		(setq indent-tabs-mode t)
		(tabify))
	(progn
	  (setq indent-tabs-mode nil)
	  (untabify))))


(defun julia-repl ()
  "Start a Julia REPL in a terminal emulator in the selected window."
  (interactive)
  (term (expand-file-name my-julia-bin)))

(defun julia-repl-split-right ()
  "Start a Julia REPL in a window to the side of the selected window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (julia-repl))

(defun julia-repl-split-below ()
  "Start a Julia REPL in a window to the side of the selected window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (julia-repl))


;; Key bindings

;; Find file at point (C-c f)
(define-key mode-specific-map (kbd "f") 'find-file-at-point)

;; Indent using tabs or spaces (C-c i)
(define-key mode-specific-map (kbd "i") 'toggle-indent-tabs-mode)

;; Better expanding
(global-set-key (kbd "M-/") 'hippie-expand)

;; Like dt or df in Vim
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-z") 'zap-to-char)

;; Don't use arrow keys for window/buffer management (C-c w)
(define-prefix-command 'my-window-map)
(define-key mode-specific-map (kbd "w") 'my-window-map)
(define-key my-window-map (kbd "p") 'previous-buffer)
(define-key my-window-map (kbd "n") 'next-buffer)
(define-key my-window-map (kbd "b") 'winner-undo)
(define-key my-window-map (kbd "f") 'winner-redo)
(define-key my-window-map (kbd "u") 'windmove-up)
(define-key my-window-map (kbd "d") 'windmove-down)
(define-key my-window-map (kbd "l") 'windmove-left)
(define-key my-window-map (kbd "r") 'windmove-right)
(define-key my-window-map (kbd "s") 'speedbar)

;; Swap literal and regex isearch
;; (we then don't need (search-default-mode t))
;; Following line commented out due to Ivy/Helm:
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Swap literal and regex query-replace
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Close help without switching buffer (C-c q)
(define-key mode-specific-map (kbd "q") 'quit-other-window)

;; Toggle Solarized or background brightness
(global-set-key (kbd "<f9>") 'toggle-solarized-or-background)
;; Define it twice for good measure (C-c w t)
(define-key my-window-map (kbd "t") 'toggle-solarized-or-background)


;; Enable disabled commands

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

