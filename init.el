;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; Build with:
;;    ./autogen.sh
;;    ./configure --with-modules [--with-xwidgets] \
;;                [--with-x-toolkit=lucid] [--prefix=...]
;;    [sudo] make install
;; Execute:
;;    emacsclient -c -a '' -F "'(fullscreen . maximized)"

;; Put external plugins into "~/.emacs.d/lisp".
;; Put themes into "~/.emacs.d/themes".
;; Put etags configurations into "~/.emacs.d/etags".
;;
;; External plugins to download:
;;   - [lsp-julia](https://github.com/non-Jedi/lsp-julia) (optional)
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
;;    [sh reconf.sh]  # If building from CVS.
;;    ./configure --with-universal-ctags=<ctagsbin> [--prefix=...]
;;    make
;;    [sudo] make install

(defconst my-emacs-dir "~/.emacs.d")
(defconst my-backup-dir (expand-file-name "backups" my-emacs-dir))
(defconst my-autosave-dir (expand-file-name "autosaves" my-emacs-dir))
(defconst my-extended-package-dir (expand-file-name "lisp" my-emacs-dir))
(defconst my-themes-dir (expand-file-name "themes" my-emacs-dir))

(defvar my-graphic-light-theme 'solarized-light)
(defvar my-graphic-dark-theme 'solarized-dark)
(defvar my-graphic-default-theme my-graphic-light-theme)
(defvar my-terminal-light-theme 'leuven)
(defvar my-terminal-dark-theme 'misterioso)
(defvar my-terminal-default-theme my-terminal-light-theme)
;; We assume these to be included in Emacs.
(defvar my-fallback-light-theme 'leuven)
(defvar my-fallback-dark-theme 'misterioso)
(defvar my-fallback-default-theme my-fallback-light-theme)

(defconst my-gtags-dir "/usr/local/share/gtags")
(defconst my-julia-bin "~/local/bin/julia")
(defconst my-julia-default-environment "~/.julia/environments/v1.3")
(defconst my-jupyter-dir "~/anaconda3/bin")

(defconst my-music-dir "~/Music/")

;; For faster initialization
(defvar my-tmp-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
	  gc-cons-percentage 0.6
	  file-name-handler-alist nil)

;; Restore (better) GC defaults afterwards
(add-hook 'after-init-hook ;; or 'emacs-startup-hook
		  (lambda ()
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
 '(global-hl-line-mode t)
 '(global-subword-mode t)
 '(global-whitespace-mode t)
 '(grep-scroll-output t)
 '(history-length 500)
 '(hs-isearch-open t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere nil nil nil "Maybe enable? Keey an eye out for when this is useful.")
 '(image-animate-loop t)
 '(indicate-buffer-boundaries (quote right))
 '(isearch-allow-scroll nil nil nil "Maybe change this.")
 '(kept-new-versions 6)
 '(kept-old-versions 4)
 '(kill-do-not-save-duplicates t)
 '(mail-signature nil)
 '(menu-bar-mode nil)
 '(message-kill-buffer-on-exit t)
 '(message-signature nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-yank-at-point t)
 '(org-agenda-files (quote ("~/Uni/SMWLevelGenerator/plan.org")))
 '(package-archive-priorities (quote (("gnu" . 5) ("melpa-stable" . 3) ("melpa" . 2))))
 '(package-menu-hide-low-priority t)
 '(package-selected-packages
   (quote
	(dired-du dired-git-info purescript-mode js2-mode magit markdown-mode typescript-mode realgud dap-mode cobol-mode csharp-mode fsharp-mode go-mode num3-mode php-mode sed-mode smalltalk-mode stan-mode swift-mode zig-mode elixir-mode erlang clojure-mode cmake-mode haskell-snippets caml sml-mode haskell-mode lsp-julia nasm-mode yaml-mode ada-mode chess csv-mode json-mode vterm lua-mode python nov ein rust-mode slime yasnippet-snippets texfrag eglot undo-propose julia-repl ess form-feed nim-mode evil-collection evil-commentary evil-lion evil-magit evil-matchit evil-snipe evil-surround evil-visualstar counsel-spotify landmark auctex zotxt company-lsp company-quickhelp dumb-jump expand-region jupyter use-package gotham-theme zenburn-theme toc-org flymake org tramp projectile ivy ggtags pdf-tools yasnippet solarized-theme rainbow-delimiters lsp-mode julia-mode helm gnu-elpa-keyring-update forge evil emms darkroom company)))
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
 '(size-indication-mode t)
 '(tab-width 4)
 '(time-stamp-time-zone t)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote top))
 '(tramp-shell-prompt-pattern
   "\\(?:^\\|\\)[^]#$%>
]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
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

(setq package-check-signature t)

;; For external plugins in `my-extended-package-dir'
(let ((default-directory my-extended-package-dir))
  (setq load-path
		(append
		 (let ((load-path  (copy-sequence load-path)))
		   (append
			(copy-sequence (normal-top-level-add-to-load-path '(".")))
			(normal-top-level-add-subdirs-to-load-path)))
		 load-path)))

;; Load themes in `my-themes-dir'
(let ((basedir my-themes-dir))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
        (add-to-list 'custom-theme-load-path (concat basedir f)))))

(defun toggle-show-whitespace ()
  "Toggle showing whitespace in the current buffer."
  (interactive)
  (if (or show-trailing-whitespace global-whitespace-mode whitespace-mode)
	  (progn
		(setq-local show-trailing-whitespace nil)
		(whitespace-mode 0))
	(progn
	  (setq-local show-trailing-whitespace t)
	  (if (not global-whitespace-mode)
		  (whitespace-mode 1)))))


(define-prefix-command 'my-extended-map)
;; Extended custom commands (C-c x)
(define-key mode-specific-map (kbd "x") 'my-extended-map)

;; Do complete .bin files
(setq completion-ignored-extensions
	  (remove ".bin" completion-ignored-extensions))

;; More Eshell visual commands
(with-eval-after-load "em-term"
  (setq eshell-visual-commands
		(append eshell-visual-commands
				'("vim" "vimdiff" "nvim" "tmux" "joe" "nano" "mg" "cmus" "mpsyt"
				  "htop" "ncdu" "nethack" "crawl" "jstar" "jmacs" "rjoe" "jpico"
				  "zile" "zemacs" "zi")))
  (setq eshell-visual-options
		(append eshell-visual-options
				'(("git" "--help" "--paginate")
				  ("emacs" "-nw" "--no-window-system")
				  ("emacsclient" "-nw" "--no-window-system"))))
  (setq eshell-visual-subcommands
		(append eshell-visual-subcommands
				'(("git" "log" "reflog" "diff" "show")))))

;; Set backup directory
(setq backup-directory-alist `(("." . ,my-backup-dir)))

;; Use visible bell instead of tone
(setq visible-bell t)

;; Start maximized (does not work with Emacsclient)
;; Can use `default-frame-alist', however, then _every_ new frame is maximized;
;; this works with Emacsclient.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Use flyspell for strings and comments by default
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Deactivate scroll bars
(add-hook 'emacs-startup-hook (lambda () (scroll-bar-mode 0)))

;; Smooth (mouse) scrolling
(if (require 'pixel-scroll nil t)
	(pixel-scroll-mode 1))

;; Vim-like autoscroll
(setq scroll-conservatively 1)

;; Use tab for completion if line is already indented.
;; Activate if it is dangerous (to go alone (without `company')).
;; (setq tab-always-indent 'complete)

;; Autoclose blocks in LaTeX mode
(add-hook 'latex-mode-hook 'latex-electric-env-pair-mode)
;; Auto-fill in TeX mode
(add-hook 'tex-mode-hook 'auto-fill-mode)

;; Disable whitespace in Term mode
(add-hook 'term-mode-hook 'toggle-show-whitespace)


;; Dired
(add-hook 'dired-after-readin-hook 'toggle-show-whitespace)

;; Ido
(ido-mode 1)
(add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)

;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Use EDE everywhere
;; (global-ede-mode t) (conflicts with org-mode binding)
;; Change font
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-11"))

;; RefTeX
;; (require 'reftex)
(autoload 'turn-on-reftex "reftex")
(add-hook 'latex-mode-hook 'turn-on-reftex)

(with-eval-after-load "reftex"
  (add-hook 'reftex-select-bib-mode-hook 'toggle-show-whitespace)
  (add-to-list 'reftex-include-file-commands "includeonly"))

;; TRAMP
(if (require 'tramp nil t)
	(progn
	  ;; Load Eshell extensions
	  ;; Maybe named "em-tramp"
	  (with-eval-after-load "em-term"
		(add-to-list 'eshell-modules-list 'eshell-tramp))
	  ;; Change a value in `tramp-methods'
	  ;; (with-eval-after-load "tramp"
	  ;;   (setf (cadr (assq 'tramp-login-args (cdr (assoc "plink" tramp-methods))))
	  ;;          '(("-l" "%u") ("-P" "%p") ("-ssh") ("-t") ("%h") ("\"")
	  ;;            ("env 'TERM=dumb' 'PROMPT_COMMAND=' 'PS1=#$ '") ("/bin/sh") ("\""))))
	  ;; Use X11 forwarding (-X)
	  ;; TODO check if this works correctly (nope, not on multi hops)
	  (add-to-list 'tramp-remote-process-environment
				   (format "DISPLAY=localhost%s" (getenv "DISPLAY")))
	  (defun remote-shell ()
		"Start a remote shell with the correct TERM environment variable."
		(interactive)
		(let ((process-environment (cons "TERM=xterm-256color" process-environment)))
		  (shell)))))

;; Flymake
(if (require 'flymake nil t)
	(progn
	  (add-hook 'prog-mode-hook #'flymake-mode)
	  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
	  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

	  (defun toggle-flymake-mode ()
		"Toggle Flymake mode."
		(interactive)
		(if (eq flymake-mode nil)
			(flymake-mode 1)
		  (flymake-mode 0)))
	  (define-key my-extended-map (kbd "f") 'toggle-flymake-mode)))

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

(setq my-org-babel-load-languages '())
(if (and (require 'org-install nil t)
		 (require 'org-habit nil t))
	(progn
	  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
	  (add-hook 'org-mode-hook 'org-display-inline-images)
	  (add-hook 'message-mode-hook 'orgtbl-mode)

	  (define-prefix-command 'my-org-map)
	  (define-key mode-specific-map (kbd "o") 'my-org-map)
	  (define-key my-org-map (kbd "n") 'org-footnote-action)
	  (define-key my-org-map (kbd "l") 'org-store-link)
	  (define-key my-org-map (kbd "a") 'org-agenda)
	  (define-key my-org-map (kbd "o") 'org-switchb)
	  (define-key my-org-map (kbd "c") 'org-capture)
	  (setq my-org-babel-load-languages
			(append my-org-babel-load-languages
					'((emacs-lisp . t)
					  (python . t))))))


;; load-theme "fixes"
;; Correctly switch themes by first `disable-theme`ing
;; the current one, then `load-theme`ing the other.
(advice-add 'load-theme :before
			(lambda (&rest args) (mapc #'disable-theme custom-enabled-themes))
			'((name . "theme-dont-propagate")))

(defun load-theme--restore-scroll-bar-mode (orig-fun &rest args)
  "Restore `scroll-bar-mode' after theme switch."
  (let ((current-scroll-bar-mode (get-scroll-bar-mode)))
	(progn
	  (apply orig-fun args)
	  (set-scroll-bar-mode current-scroll-bar-mode))))
(advice-add 'load-theme :around #'load-theme--restore-scroll-bar-mode)

(defun safe-load-theme (theme default-theme)
  "Load the given theme but if it is not available, load the given default."
  (condition-case nil
	  (load-theme theme t)
	(error (load-theme default-theme t))))

(defun update-frame-background-mode ()
  "Update `frame-background-mode' for all frames."
  (mapc 'frame-set-background-mode (frame-list)))

(defun load-theme-getenv (light-theme dark-theme default-theme
									  envvar dark-pattern)
  "Load either the given light, dark, or default theme depending on if the given
environment variable is equal to the given pattern which activates the dark
theme variant."
  ;; Set frame background and fix defaults if not available.
  (if (not (equal (getenv envvar) ""))
	  (if (equal (getenv envvar) dark-pattern)
		  (progn (setq frame-background-mode 'dark)
				 (update-frame-background-mode)
				 (safe-load-theme dark-theme my-fallback-dark-theme))
		(progn (setq frame-background-mode 'light)
			   (update-frame-background-mode)
			   (safe-load-theme light-theme my-fallback-light-theme)))
	;; Load theme
	(safe-load-theme default-theme my-fallback-default-theme)))

(load-theme-getenv my-graphic-light-theme
				   my-graphic-dark-theme
				   my-graphic-default-theme
				   "SOLARIZED_THEME"
				   "dark")

(if (daemonp)
	(add-hook 'after-make-frame-functions
			  (lambda (frame)
				(select-frame frame)
				(if (not (display-graphic-p frame))
					(load-theme-getenv
					 my-terminal-light-theme
					 my-terminal-dark-theme
					 my-terminal-default-theme
					 "SOLARIZED_THEME"
					 "dark")))))


;; TODO Find out how to automatically get comment strings. And use that instead
;; of the hardcoded regex for _all_ occurrences.
(defun highlight-todos ()
  "Highlight TODO-related keywords."
  (font-lock-add-keywords nil
						  '(("\\<\\(\\(?:TODO\\|FIXME\\)[Ss]?\\>:?\\)" 1
							 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'highlight-todos)
(add-hook 'tex-mode-hook  'highlight-todos)

;; Ripgrep
(if (executable-find "rg")
	(setq grep-command
		  "rg --color always -nH --null --no-heading --smart-case -e "))


;; Package config

;; GNU Global
(add-to-list 'load-path my-gtags-dir)
(autoload 'gtags-mode "gtags" "" t)

(add-hook 'prog-mode-hook (lambda () (gtags-mode 1)))

;; Ggtags
(add-hook 'c-mode-common-hook
		  (lambda ()
			(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
			  (progn
				(ggtags-mode 1)
				(setq-local hippie-expand-try-functions-list
							(cons 'ggtags-try-complete-tag
								  hippie-expand-try-functions-list))))))

;; AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-electric-sub-and-superscript t)
(setq TeX-electric-math '("$" . "$"))
(setq LaTeX-electric-left-right-brace t)
;; TODO necessary? (setq TeX-newline-function 'newline-and-indent)
(add-hook 'TeX-mode-hook (lambda () (prettify-symbols-mode 1)))
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook #'flymake-mode)
(add-hook 'LaTeX-mode-hook (lambda () (electric-pair-local-mode 0)))
(setq reftex-plug-into-AUCTeX t)

;; Use PDF Tools
(add-hook 'TeX-mode-hook
		  (lambda () (setf (nth 1
								(assq 'output-pdf TeX-view-program-selection))
						   "PDF Tools")))
;; If PDF-Tools are used:
(add-hook 'TeX-after-compilation-finished-functions
		  'TeX-revert-document-buffer)
;; TODO LuaLaTeX available instead of pdflatex?
;; TODO TeX-file-line-error useful?

;; Use below lines if LaTeX 3 deprecates $...$ for inline equations.
;; (add-hook 'plain-TeX-mode-hook
;;      	  (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
;; 						  (cons "$" "$"))))
;; ;; Press $ to insert "\(\)" with point in between in LaTeX mode.
;; (add-hook 'LaTeX-mode-hook
;;      	  (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
;; 						  (cons "\\(" "\\)"))))

;; AUCTeX mode hooks
;; (add-hook 'TeX-mode-hook   'highlight-todos)
(add-hook 'TeX-mode-hook   'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'latex-electric-env-pair-mode)

;; Dumb Jump
(if (functionp 'dumb-jump-mode)
	(dumb-jump-mode))

;; Magit
(if (require 'magit nil t)
	(progn
	  (define-prefix-command 'my-magit-map)
	  (define-key mode-specific-map (kbd "g") 'my-magit-map)
	  (define-key my-magit-map (kbd "g") 'magit-status)
	  (define-key my-magit-map (kbd "G") 'magit-dispatch-popup)
	  (define-key magit-file-mode-map (kbd "C-c g") 'magit-file-dispatch)))

;; EMMS
(if (require 'emms-setup nil t)
	(progn
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
	  (defun init-emms ()
		"Load the playlist emms-music in 'my-music-dir, go to a random track and
stop playback."
		(interactive)
		(emms-play-playlist (expand-file-name "emms-music" my-music-dir))
		(emms-random)
		(emms-stop)
		(update-emms-faces))

	  (add-hook 'after-init-hook 'init-emms)

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
	  (define-key my-emms-map (kbd "i") 'init-emms)
	  (define-key my-emms-map (kbd "+") 'emms-volume-raise)
	  (define-key my-emms-map (kbd "-") 'emms-volume-lower))
  (defun update-emms-faces ()
	"No op." ;; TODO remove when hook is added
	()))


;; undo-propose
(define-key mode-specific-map (kbd "u") 'undo-propose)

;; dired-git-info
(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode))

;; Evil mode
(setq evil-flash-delay 20)
(setq evil-want-Y-yank-to-eol t)
(setq evil-want-change-word-to-end nil)

(setq evil-vsplit-window-right t)
(setq evil-shift-round nil)
(setq evil-shift-width 4)

(setq evil-search-module 'evil-search)

;; Required for Evil Collection
;; (setq evil-want-keybinding nil)

(if (require 'evil nil t)
	(progn
	  (evil-mode 1)

	  ;; Disable undo-tree-mode to prevent bugs
	  (global-undo-tree-mode 0)

	  ;; Setup Evil Collection (also uncomment above to use)
	  ;; (when (require 'evil-collection nil t)
	  ;;   (evil-collection-init))
	  ;; evil-magit
	  ;; (require 'evil-magit)

	  ;; Other Evil packages
	  ;; Evil Surround
	  (global-evil-surround-mode 1)
	  ;; evil-commentary
	  (evil-commentary-mode 1)
	  ;; evil-lion
	  (evil-lion-mode)
	  ;; evil-matchit
	  (if (require 'evil-matchit nil t)
		  (global-evil-matchit-mode 1))
	  ;; evil-visualstar
	  (global-evil-visualstar-mode 1)

	  ;; Emacs state by default (must be added to head of list)
	  ;; (add-to-list 'evil-buffer-regexps '("." . emacs))
	  (setq evil-default-state 'emacs)
	  ;; Except in these modes
	  (evil-set-initial-state 'prog-mode 'normal)
	  (evil-set-initial-state 'text-mode 'normal)
	  (evil-set-initial-state 'tex-mode  'normal)
	  ;; But also not in these (possibly inherited) modes
	  (evil-set-initial-state 'help-mode   'emacs)
	  (evil-set-initial-state 'Info-mode   'emacs)
	  (evil-set-initial-state 'comint-mode 'emacs)
	  (evil-set-initial-state 'shell-mode  'emacs)
	  (evil-set-initial-state 'term-mode   'emacs)
	  (evil-set-initial-state 'org-mode    'emacs)

	  ;; Evil mappings

	  ;; C-S-d to delete-forward-char in insert mode
	  (define-key evil-insert-state-map (kbd "C-S-d") 'evil-delete-char)

	  ;; C-l to exit from any state to normal state
	  (define-key evil-insert-state-map   (kbd "C-l") 'evil-normal-state)
	  (define-key evil-operator-state-map (kbd "C-l") 'evil-normal-state)
	  (define-key evil-replace-state-map  (kbd "C-l") 'evil-normal-state)
	  (define-key evil-visual-state-map   (kbd "C-l") 'evil-normal-state)
	  ;; Here we do not necessarily go back to normal state but that's fine.
	  (define-key evil-ex-completion-map  (kbd "C-l") 'abort-recursive-edit)

	  ;; C-S-d in normal or motion state to scroll up (C-S-u fails in Ubuntu)
	  (define-key evil-normal-state-map (kbd "C-S-d") 'evil-scroll-up)
	  (define-key evil-motion-state-map (kbd "C-S-d") 'evil-scroll-up)

	  ;; Ex state (minibufer) mappings
	  ;; C-b moves one char backward
	  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
	  ;; C-a moves to start of line
	  (define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
	  ;; C-d deletes char forward
	  (define-key evil-ex-completion-map (kbd "C-d") 'delete-char)
	  ;; C-k deletes line forward
	  (define-key evil-ex-completion-map (kbd "C-k") 'evil-delete-line)
	  (define-key evil-ex-completion-map (kbd "C-S-k") 'evil-insert-digraph)

	  ;; C-l in normal state to remove highlighting
	  (define-key evil-normal-state-map (kbd "C-l") 'evil-ex-nohighlight)

	  (defun my-maybe-evil-repeat-pop ()
		"Execute `evil-repeat-pop' or `flyspell-auto-correct-word' depending on
the context."
		(interactive)
		(condition-case err
			(evil-repeat-pop)
		  (user-error (if flyspell-mode
						  (flyspell-auto-correct-word)
						(signal (car err) (cdr err))))))

	  (defun my-maybe-evil-repeat-pop-next ()
		"Execute `evil-repeat-pop-next' or `xref-find-definitions' depending on
the context."
		(interactive)
		(condition-case nil
			(evil-repeat-pop-next)
		  (user-error (xref-find-definitions))))

	  ;; C-. executes `flyspell-auto-correct-word' if no prior repetition can
	  ;; be popped (only if `flyspell-mode' is enabled).
	  (define-key evil-normal-state-map (kbd "C-.") 'my-maybe-evil-repeat-pop)
	  ;; M-. executes `xref-find-definitions' if no prior repetition can be popped.
	  (define-key evil-normal-state-map (kbd "M-.") 'my-maybe-evil-repeat-pop-next)

	  ;; C-r invokes undo-propose (since we do not use undo-tree)
	  (define-key evil-normal-state-map (kbd "C-r") 'undo-propose)


	  ;; TODO this is most likely unnecessary
	  ;; ;; Toggle global Evil mode with C-c x v (also toggle undo-tree-mode).
	  ;; ;; Does not disable evil-magit.
	  ;; ;; TODO what about evil minor modes?
	  ;; (defun toggle-global-evil ()
	  ;;   "Toggle global Evil mode. Also toggle undo-tree-mode."
	  ;;   (interactive)
	  ;;   (if (eq evil-mode t)
	  ;; 	  (progn (evil-mode 0)
	  ;; 			 (global-undo-tree-mode 0))
	  ;; 	(progn (evil-mode 1)
	  ;; 		   (global-undo-tree-mode 0))))
	  ;; (define-key my-extended-map (kbd "v") 'toggle-global-evil)

	  ;; Evil-snipe
	  (if (require 'evil-snipe nil t)
		  (progn
			(evil-snipe-mode 1)

			(setq evil-snipe-smart-case nil)

			(setq evil-snipe-scope 'visible)
			(setq evil-snipe-repeat-scope 'visible)
			(setq evil-snipe-spillover-scope 'buffer)))

	  ;; Use C-s to substitute (as "s" is taken by evil-snipe)
	  (define-key evil-normal-state-map (kbd "C-s") 'evil-substitute)
	  (define-key evil-normal-state-map (kbd "C-S-s") 'evil-change-whole-line)))


;; Company
(if (functionp 'global-company-mode)
	(progn
	  (add-hook 'after-init-hook 'global-company-mode)
	  ;; Faster auto completion
	  (setq company-minimum-prefix-length 2)
	  (setq company-idle-delay 0.1)

	  (setq company-dabbrev-downcase nil)

	  (setq company-selection-wrap-around t)
	  ;; Autocomplete with C-c c
	  (define-key mode-specific-map (kbd "c") 'company-complete)

	  ;; Usual completion keybindings
	  ;; (with-eval-after-load "company"
	  ;; 	 (define-key company-active-map (kbd "TAB")
	  ;; 	   'company-complete-common-or-cycle)
	  ;; 	 (define-key company-active-map (kbd "<tab>")
	  ;; 	   'company-complete-common-or-cycle)

	  ;; 	 (define-key company-active-map (kbd "S-TAB")
	  ;; 	   'company-select-previous)
	  ;; 	 (define-key company-active-map (kbd "<backtab>")
	  ;; 	   'company-select-previous))

	  ;; No need to accept completion with RET; use TAB and S-TAB to cycle.
	  ;; However, compatibility problem with YASnippet (resolved later).
	  (company-tng-configure-default)))

;; Company quickhelp
(if (functionp 'company-quickhelp-mode)
	(progn
	  (company-quickhelp-mode)
	  (setq company-quickhelp-delay 0.65)
	  (define-key company-active-map
		(kbd "M-h") #'company-quickhelp-manual-begin)))

;; Company-lsp
;; (if (require 'company-lsp nil t)
;; 	(push 'company-lsp company-backends))


;; Ivy
;; (ivy-mode 1)
(if (functionp 'ivy-mode)
	(progn
	  (setq ivy-use-virtual-buffers t)
	  (setq ivy-count-format "(%d/%d) ")

	  (global-set-key (kbd "C-s")	'swiper)
	  (global-set-key (kbd "M-x")	'counsel-M-x)
	  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
	  (global-set-key (kbd "C-x b")	'ivy-switch-buffer)
	  (global-set-key (kbd "C-x 4 b") 'ivy-switch-buffer-other-window)
	  (global-set-key (kbd "C-x d")	'counsel-dired)
	  (global-set-key (kbd "C-x r b") 'counsel-bookmark)
	  (global-set-key (kbd "<f1> f")	'counsel-describe-function)
	  (global-set-key (kbd "<f1> v")	'counsel-describe-variable)
	  (global-set-key (kbd "<f1> l")	'counsel-find-library)
	  (global-set-key (kbd "<f2> i")	'counsel-info-lookup-symbol)
	  (global-set-key (kbd "<f2> u")	'counsel-unicode-char)

	  (define-key mode-specific-map (kbd "j") 'counsel-semantic-or-imenu)
	  (define-key mode-specific-map (kbd "r") 'ivy-resume)
	  (define-key my-emms-map	  (kbd "o") 'counsel-rhythmbox)))

;; Helm
(if (require 'helm-config nil t)
	(progn
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
	  (with-eval-after-load "eshell"
		(add-hook
		 'eshell-mode-hook
		 (lambda ()
		   (eshell-cmpl-initialize)
		   (define-key eshell-mode-map [remap eshell-pcomplete]
			 'helm-esh-pcomplete)
		   (define-key eshell-mode-map (kbd "M-p") 'helm-esh-history))))))

;; Projectile
(if (functionp 'projectile-mode)
	(progn
	  (projectile-mode 1)
	  ;; (define-key mode-specific-map (kbd "p") 'projectile-command-map)
	  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

;; YASnippet
(if (require 'yasnippet nil t)
	(progn
	  (yas-global-mode 1)
	  ;; or (next two)
	  ;; (yas-reload-all)
	  ;; (add-hook 'prog-mode-hook #'yas-minor-mode)

	  ;; This resolves YASnippet problems with company-tng.
	  (define-key yas-minor-mode-map (kbd "C-j") 'yas-expand)
	  (define-key yas-keymap (kbd "C-j") 'yas-next-field-or-maybe-expand)
	  (dolist (keymap (list yas-minor-mode-map yas-keymap))
		(define-key keymap (kbd "TAB") nil)
		(define-key keymap (kbd "<tab>") nil))))

;; (require 'expand-region)
(autoload 'er/expand-region "expand-region")
(define-key my-extended-map (kbd "x") 'er/expand-region)

;; PDF-Tools
(if (functionp 'pdf-tools-install)
	(pdf-tools-install)
  ;; or (pdf-loader-install)
  )

;; Emacs libvterm
(if (functionp 'vterm-mode)
	;; Disable whitespace in VTerm mode
	(add-hook 'vterm-mode-hook 'toggle-show-whitespace))

;; toc-org
(if (require 'toc-org nil t)
    (progn
	  (add-hook 'org-mode-hook 'toc-org-mode)
	  (add-to-list 'org-tag-alist '("TOC" . ?T)))
  (warn "toc-org not found"))

;; Jupyter
(setq exec-path (append exec-path `(,(expand-file-name my-jupyter-dir))))
(if (functionp 'org-babel-jupyter-scratch-buffer)
	(setq my-org-babel-load-languages
		  (append my-org-babel-load-languages
				  '(
					;; (julia . t)  TODO needs upstream fix; is too old
					(jupyter . t)))))

;; Emacs IPython Notebook
(setq ein:polymode t)

;; form-feed (display  as horizontal line)
(if (functionp 'form-feed-mode)
	(progn
	  (setq form-feed-line-width 72)
	  (add-hook 'Info-mode-hook 'form-feed-mode)
	  (add-hook 'help-mode-hook 'form-feed-mode)
	  (add-hook 'text-mode-hook 'form-feed-mode)
	  (add-hook 'prog-mode-hook 'form-feed-mode)))

;; TeXfrag
;; TODO fix PreviewLaTeX in AuCTeX
;; (texfrag-global-mode)
;; (add-hook 'eww-mode-hook 'texfrag-mode)

(if (functionp 'org-babel-do-load-languages)
	(org-babel-do-load-languages 'org-babel-load-languages
								 my-org-babel-load-languages))


;; Julia mode
;; (require 'julia-mode)
(autoload 'julia-mode-hook "julia-mode")
(setq julia-program my-julia-bin)
(add-hook 'julia-mode-hook (lambda ()
							 (setq-local whitespace-line-column 92)))

;; ess
;; deactivate automatic loading of `ess-julia-mode'
(setq auto-mode-alist
	  (delete (rassoc 'ess-julia-mode auto-mode-alist) auto-mode-alist))
(setq inferior-julia-program my-julia-bin)
(setq inferior-julia-args "--color=yes")

;; julia-repl
(add-hook 'julia-mode-hook
		  (lambda () (if (buffer-file-name) (julia-repl-mode))))
(setq julia-repl-executable-records
	  '((default "julia")
		(new "julian")))

;; lsp-julia
;; (setq lsp-julia-default-environment my-julia-default-environment)
;; ;; If we don't want to use the included Language Server:
;; (setq lsp-julia-package-dir nil)
;; (require 'lsp-julia nil t)


;; Rust mode
;; (require 'rust-mode)
(autoload 'rust-mode-hook "rust-mode")
(add-hook 'rust-mode-hook
		  (lambda () (setq indent-tabs-mode nil)))
;; Run rustfmt on save
;; (setq rust-format-on-save t)

;; lsp-mode
;; (if (require 'lsp-mode nil t)
;; 	(progn
;; 	  (add-hook 'prog-mode-hook #'lsp)
;; 	  (add-hook 'julia-mode-hook #'lsp-mode)
;; 	  (add-hook 'julia-mode-hook #'lsp)))


;; Eglot
;; Use company-capf backend whenever `M-x eglot' connects
;; TODO Maybe redundant as this is always forced
;; (add-hook 'eglot-connect-hook
;; 		  (lambda ()
;; 			(setq-local company-backends
;; 						(cons 'company-capf
;; 							  (remove 'company-capf company-backends)))))
(if (require 'eglot nil t)

	(progn
	  (add-hook 'rust-mode-hook   'eglot-ensure)
	  (add-hook 'python-mode-hook 'eglot-ensure)

	  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
	  (add-hook 'c-mode-hook 'eglot-ensure)
	  (add-hook 'c++-mode-hook 'eglot-ensure)

	  (defun my-julia-get-project-root (dir)
		"Get the Julia project root directory of the given `dir'."
		(expand-file-name (if dir
							  (or (locate-dominating-file dir "Project.toml")
								  (locate-dominating-file
								   dir "JuliaProject.toml")
								  my-julia-default-environment)
							my-julia-default-environment)))

	  (defun my-julia-lsp-command (arg)
		"Command to start the Julia language server. `arg' is ignored."
		(let ((project-root-dir (my-julia-get-project-root (buffer-file-name))))
		  `("julia" "--startup-file=no" "--history-file=no"
			,(concat "--project=" project-root-dir)
			,(concat "-e using LanguageServer; "
					 "using LanguageServer.SymbolServer; "
					 "server = LanguageServerInstance("
					 "stdin, stdout, false, \""
					 project-root-dir
					 "\"); "
					 "run(server)"))))

	  (add-to-list
	   'eglot-server-programs
	   '(julia-mode . my-julia-lsp-command))

	  (add-hook 'julia-mode-hook 'eglot-ensure)
	  ;; Wait longer due to slow compilation
	  (add-hook 'julia-mode-hook
				(lambda () (setq-local eglot-connect-timeout 90)))))


;; Load private configurations
(load (expand-file-name ".private_config.el" my-emacs-dir) t)


;; Custom commands

(defun find-init-file ()
  "Find init.el in `my-emacs-dir'."
  (interactive)
  (find-file (expand-file-name "init.el" my-emacs-dir)))

(defun query-kill-emacs ()
  "Query whether to `kill-emacs' and if yes, `save-some-buffers' and kill."
  (interactive)
  (if (y-or-n-p "Kill Emacs server? ")
	  (progn (save-some-buffers)
			 (kill-emacs))))

(defun insert-arbitrary-pair (beginning ending)
  "Insert a pair of any two characters."
  (if (region-active-p)
	  (let ((beg (region-beginning)))
		(save-excursion
		  (goto-char (region-end))
		  (insert ending)
		  (goto-char beg)
		  (insert beginning)))
	(insert beginning)
	(save-excursion
	  (insert ending))))

(defun insert-char-pair (char)
  "Insert the given `char' before and after point or the region if active."
  (interactive "cSurrounding character: ")
  (insert-arbitrary-pair char char))

(defun insert-same-pair (text)
  "Insert the given `text'  before and after point or the region if active."
  (interactive "sSurrounding text: ")
  (insert-arbitrary-pair text text))

(defun insert-differing-pair (beginning ending)
  "Insert the given `beginning' before point or the region if active and the
given `ending' after."
  (interactive "sBeginning text: \nsEnding text: ")
  (insert-arbitrary-pair beginning ending))

(defun insert-reversed-pair (text)
  "Insert the given `beginning' before point or the region if active and the
given `ending' after, but reversed ('[a' -> 'a[')."
  (interactive "sUnreversed beginning: ")
  (insert-arbitrary-pair text (reverse text)))

(defun insert-tag-pair (tag)
  "Insert the given HTML `tag' before and after point or the region if active."
  (interactive "sTag: ")
  (insert-arbitrary-pair (concat "<" tag ">") (concat "</" tag ">")))

(defun delete-around (count)
  "Delete `count' characters before and after point or the region if active."
  (interactive "p")
  (if (region-active-p)
	  (let ((beg (region-beginning)))
		(save-excursion
		  (goto-char (region-end))
		  (delete-char count)
		  (goto-char beg)
		  (delete-backward-char count)))
	(delete-backward-char count)
	(delete-char count)))

(defun quit-other-window ()
  "Quit the other (next) window while staying in the selected window."
  (interactive)
  (select-window (next-window))
  (quit-window nil)
  (select-window (previous-window)))

(defun toggle-background ()
  "Toggle background brightness and reload theme."
  (interactive)
  (if (eq frame-background-mode 'dark)
	  (setq frame-background-mode 'light)
	(setq frame-background-mode 'dark))
  (update-frame-background-mode)
  (load-theme (car custom-enabled-themes) t)
  (update-emms-faces))

(defun toggle-theme-brightness ()
  "Toggle light and dark theme depending on the current window system."
  (interactive)
  (let ((light-theme (if (not window-system)
						 'my-terminal-light-theme
					   my-graphic-light-theme))
		(dark-theme (if (not window-system)
						'my-terminal-dark-theme
					  my-graphic-dark-theme)))
	(if (eq frame-background-mode 'dark)
		(progn
		  (setq frame-background-mode 'light)
		  (update-frame-background-mode)
		  (safe-load-theme light-theme my-fallback-light-theme))
	  (progn
		(setq frame-background-mode 'dark)
		(update-frame-background-mode)
		(safe-load-theme dark-theme my-fallback-dark-theme)))
	(update-emms-faces)))

(defun toggle-theme-brightness-or-background ()
  "Toggle either Solarized light and dark or the background brightness depending
on if a Solarized variant is currently active."
  (interactive)
  (if (or
	   (eq (car custom-enabled-themes) my-graphic-light-theme)
	   (eq (car custom-enabled-themes) my-graphic-dark-theme)
	   (eq (car custom-enabled-themes) my-terminal-light-theme)
	   (eq (car custom-enabled-themes) my-terminal-dark-theme)
	   (eq (car custom-enabled-themes) my-fallback-light-theme)
	   (eq (car custom-enabled-themes) my-fallback-dark-theme))
	  (toggle-theme-brightness)
	(toggle-background)))

(defun toggle-indent-tabs-mode ()
  "Toggle `indent-tabs-mode'."
  (interactive)
  (if (eq indent-tabs-mode nil)
	  (progn
		(setq indent-tabs-mode t)
		(tabify (point-min) (point-max)))
	(progn
	  (setq indent-tabs-mode nil)
	  (untabify (point-min) (point-max)))))

(if (functionp 'pixel-scroll-mode)
	(defun toggle-pixel-scroll-mode ()
	  "Toggle `pixel-scroll-mode'."
	  (interactive)
	  (if (eq pixel-scroll-mode nil)
		  (pixel-scroll-mode 1)
		(pixel-scroll-mode 0)))
  (defun toggle-pixel-scroll-mode ()
	"No op."
	(interactive)
	()))


(defun my-julia-repl ()
  "Start a Julia REPL in a terminal emulator in the selected window."
  (interactive)
  (term (expand-file-name my-julia-bin)))

(defun my-julia-repl-split-right ()
  "Start a Julia REPL in a window to the side of the selected window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (my-julia-repl))

(defun my-julia-repl-split-below ()
  "Start a Julia REPL in a window to the side of the selected window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (my-julia-repl))


;; Key bindings

;; Do not untabify before backspacing
(global-set-key (kbd "DEL") 'backward-delete-char)

;; Find file at point (C-c f)
(define-key mode-specific-map (kbd "f") 'find-file-at-point)

;; Better expanding
(global-set-key (kbd "M-/") 'hippie-expand)

;; Like dt or df in Vim
(global-set-key (kbd "M-z") 'zap-up-to-char)
(define-key mode-specific-map (kbd "z") 'zap-to-char)

;; Swap literal and regex isearch
;; (we then don't need (search-default-mode t))
;; Following line commented out due to Ivy/Helm:
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Swap literal and regex query-replace
(global-set-key (kbd "M-%")   'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Indent using tabs or spaces (C-c x i)
(define-key my-extended-map (kbd "i") 'toggle-indent-tabs-mode)

;; Toggle pixel-scrolling (C-c x p)
(define-key my-extended-map (kbd "p") 'toggle-pixel-scroll-mode)

;; Compile (C-c x c)
(define-key my-extended-map (kbd "c") 'compile)


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
(define-key my-window-map (kbd "o") 'window-swap-states)
(define-key my-window-map (kbd "w") 'toggle-frame-fullscreen)
(define-key my-window-map (kbd "m") 'toggle-frame-maximized)
(define-key my-window-map (kbd "c") 'switch-to-completions)
(define-key my-window-map (kbd "q") 'quit-other-window)
(define-key my-window-map (kbd "R") 'redraw-display)

;; imenu and ibuffer keybindings
;; (imenu commented out due to Ivy; ibuffer is better standalone)
;; (define-key mode-specific-map (kbd "j") 'imenu)
(define-key mode-specific-map (kbd "b") 'ibuffer)

;; Remap `transpose-sexps' to M-S-t to avoid the Terminal shortcut.
(global-set-key (kbd "M-T") 'transpose-sexps)
;; Remap `kill-whole-line' to M-S-k to avoid Terminal misinterpretation.
(global-set-key (kbd "M-K") 'kill-whole-line)

;; Query whether to kill Emacs server (C-c k)
(define-key mode-specific-map (kbd "k") 'query-kill-emacs)

;; Surround point or region (C-c s)
(define-key mode-specific-map (kbd "s") 'insert-char-pair)

;; Insert other pairs around point or region (C-c a)
(define-prefix-command 'my-pairs-map)
(define-key mode-specific-map (kbd "a") 'my-pairs-map)
(define-key my-pairs-map (kbd "a") 'insert-same-pair)
(define-key my-pairs-map (kbd "s") 'insert-same-pair)
(define-key my-pairs-map (kbd "p") 'insert-differing-pair)
(define-key my-pairs-map (kbd "r") 'insert-reversed-pair)
(define-key my-pairs-map (kbd "t") 'insert-tag-pair)
(define-key my-pairs-map (kbd "d") 'delete-around)

;; Toggle theme or background brightness
(global-set-key (kbd "<f9>") 'toggle-theme-brightness-or-background)
;; Define it twice for good measure (C-c w t)
(define-key my-window-map (kbd "t") 'toggle-theme-brightness-or-background)


;; Enable disabled commands

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; init.el ends here

