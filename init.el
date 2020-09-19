;;; init.el --- Personal Emacs configuration  -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Build with:
;;    ./autogen.sh
;;    ./configure --with-modules [--with-xwidgets] \
;;                [--with-x-toolkit=lucid] [--prefix=...]
;;    [sudo] make install
;; Execute:
;;    emacsclient -c -a ''
;; After upgrade:
;;    \emacs --batch --eval '(byte-recompile-directory <package-user-dir> nil t)'
;;    # or...
;;    \emacs -q
;;    # possibly also M-x package-initialize
;;    M-x eval-expression (M-:)
;;    (byte-recompile-directory package-user-dir nil t)
;;
;; Link target "~/.emacs.d/.gnus.el" to name "~/.gnus.el"
;; (optional) Create "~/.emacs.d/.private_config.el"
;;
;; Put external plugins into "~/.emacs.d/lisp".
;; Put themes into "~/.emacs.d/themes".
;; Put etags configurations into "~/.emacs.d/etags".
;;
;; External plugins to download:
;;   - [constants](https://github.com/cdominik/constants-for-Emacs)
;;   - [ats2-mode](https://github.com/githwxi/ATS-Postiats) (in utils/emacs)
;;   - [flymake-ats2](https://github.com/githwxi/ATS-Postiats) (in utils/emacs)
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

;;; Code:

(defconst my-emacs-dir "~/.emacs.d")
(defconst my-backup-dir (expand-file-name "backups" my-emacs-dir))
(defconst my-autosave-dir (expand-file-name "autosaves" my-emacs-dir))
(defconst my-extended-package-dir (expand-file-name "lisp" my-emacs-dir))
(defconst my-themes-dir (expand-file-name "themes" my-emacs-dir))
(defconst my-desktop-dir (expand-file-name "desktops" my-emacs-dir))

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
;; (defconst my-godot-bin "~/local/bin/godot")

(defconst my-lsp-package 'lsp-mode "Which LSP package to use.
Can be a symbol in `(lsp-mode eglot all)'. If it is `all', only activate the
hooks for eglot.")
;; (defconst my-rls-bin "~/.cargo/bin/rls")

(defconst my-line-number-format 'relative)
(defconst my-music-dir "~/Music/")

;; For faster initialization
(defvar my-tmp-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
	  gc-cons-percentage 0.6
	  file-name-handler-alist nil)

;; Restore (better) GC defaults afterwards
(add-hook 'after-init-hook				; or 'emacs-startup-hook
		  (lambda ()
			(setq gc-cons-threshold 16777216
				  gc-cons-percentage 0.1
				  file-name-handler-alist my-tmp-file-name-handler-alist)))

;; Windows performance improvements TODO (?)
(when (eq system-type 'windows-nt)
	(setq-default w32-pipe-read-delay 0
				  inhibit-compacting-font-caches t
				  bidi-display-reordering nil))

(defun find-init-file ()
  "Find init.el in `my-emacs-dir'."
  (interactive)
  (find-file (expand-file-name "init.el" my-emacs-dir)))

(defun query-kill-emacs ()
  "Query whether to `kill-emacs' and if yes, `save-some-buffers' and kill."
  (interactive)
  (when (y-or-n-p "Kill Emacs server? ")
	(save-some-buffers)
	(kill-emacs)))

;; These key bindings here so they are loaded before errors
;; Edit init.el (C-c i)
(define-key mode-specific-map (kbd "i") 'find-init-file)

;; Query whether to kill Emacs server (C-c k)
(define-key mode-specific-map (kbd "k") 'query-kill-emacs)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(when (< emacs-major-version 27)
  (package-initialize))

;; Add package lists
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
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
 '(before-save-hook '(time-stamp))
 '(bookmark-save-flag 1)
 '(c-basic-offset 4)
 '(c-default-style
   '((c-mode . "k&r")
	 (c++-mode . "k&r")
	 (java-mode . "java")
	 (awk-mode . "awk")
	 (other . "java")))
 '(calendar-date-style 'iso)
 '(column-number-mode t)
 '(completion-cycle-threshold 6)
 '(current-language-environment "UTF-8")
 '(delete-old-versions t)
 '(delete-trailing-lines nil)
 '(desktop-restore-eager 10)
 '(dired-always-read-filesystem t)
 '(dired-dwim-target 'dired-dwim-target-recent)
 '(display-battery-mode t)
 '(display-line-numbers-widen t)
 '(display-raw-bytes-as-hex t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(electric-pair-mode t)
 '(electric-quote-comment nil)
 '(global-cwarn-mode t)
 '(global-hl-line-mode t)
 '(global-subword-mode t)
 '(global-whitespace-mode t)
 '(grep-scroll-output t)
 '(history-length 500)
 '(hs-isearch-open t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere nil nil nil "Maybe enable? Keey an eye out for when this is useful.")
 '(image-animate-loop t)
 '(indicate-buffer-boundaries 'right)
 '(isearch-allow-scroll nil nil nil "Maybe change this.")
 '(kept-new-versions 6)
 '(kept-old-versions 4)
 '(kill-do-not-save-duplicates t)
 '(mail-signature nil)
 '(menu-bar-mode nil)
 '(message-kill-buffer-on-exit t)
 '(message-signature nil)
 '(minibuffer-depth-indicate-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-yank-at-point t)
 '(nnmail-expiry-wait 'never)
 '(org-agenda-files '("~/Documents/life.org"))
 '(package-archive-priorities
   '(("org" . 9)
	 ("gnu" . 7)
	 ("melpa" . 5)
	 ("melpa-stable" . 3)))
 '(package-menu-hide-low-priority t)
 '(package-quickstart t)
 '(package-selected-packages
   '(project so-long xref undohist browse-at-remote mines magit julia-repl counsel swiper projectile rust-mode slime jsonrpc d-mode cider gdscript-mode disk-usage dart-mode gnuplot web-mode ada-ref-man docker dockerfile-mode dired-du dired-git-info purescript-mode js2-mode markdown-mode typescript-mode realgud dap-mode cobol-mode csharp-mode fsharp-mode go-mode num3-mode php-mode sed-mode smalltalk-mode stan-mode swift-mode zig-mode elixir-mode erlang clojure-mode cmake-mode haskell-snippets caml sml-mode haskell-mode lsp-julia nasm-mode yaml-mode ada-mode chess csv-mode json-mode vterm lua-mode python nov ein yasnippet-snippets texfrag eglot undo-propose ess form-feed nim-mode evil-collection evil-commentary evil-lion evil-magit evil-matchit evil-snipe evil-surround evil-visualstar landmark auctex zotxt company-quickhelp dumb-jump expand-region jupyter use-package gotham-theme zenburn-theme toc-org flymake org tramp ivy ggtags pdf-tools yasnippet solarized-theme rainbow-delimiters julia-mode helm gnu-elpa-keyring-update forge evil emms darkroom company))
 '(password-cache-expiry 1200)
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(read-quoted-char-radix 16)
 '(recenter-redisplay t nil nil "Change this so we redraw when calling `C-u C-l`.")
 '(recentf-max-saved-items 200)
 '(recentf-mode t)
 '(register-separator 43)
 '(require-final-newline t)
 '(save-place-mode t)
 '(savehist-additional-variables
   '(tablist-named-filter command-history search-ring regexp-search-ring kill-ring extended-command-history compile-command))
 '(savehist-mode t)
 '(scroll-bar-mode 'right)
 '(semantic-mode t)
 '(send-mail-function 'smtpmail-send-it)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(shell-prompt-pattern "^\\(?:###\\)?[^#$%>
]*[#$%>] *")
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis nil nil "Originally parenthesis. Maybe try out expression or mixed.")
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tab-width 4)
 '(time-stamp-time-zone t)
 '(tool-bar-mode nil)
 '(tool-bar-position 'top)
 '(tramp-shell-prompt-pattern
   "\\(?:^\\|\\)\\(?:###\\)?[^]#$%>
]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(url-automatic-caching t)
 '(url-cookie-confirmation t)
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(wdired-allow-to-change-permissions t)
 '(which-function-mode t)
 '(whitespace-style '(face trailing lines-tail tab-mark))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq package-check-signature t)

(defun add-dir-tree-to-front-of-load-path (dir)
  "Add DIR and all of its subdirectories to the front of `load-path'.
This way, the newly added directories have priority over old ones."
  (let ((default-directory dir))
	(if (file-exists-p dir)
		(setq load-path
			  (append
			   (let ((load-path (copy-sequence load-path)))
				 (append
				  (copy-sequence (normal-top-level-add-to-load-path '(".")))
				  (normal-top-level-add-subdirs-to-load-path)))
			   load-path))
	  (warn "%s does not exist; `load-path' was not modified" dir))))

;; Add external plugins in `my-extended-package-dir' to `load-path'.
(add-dir-tree-to-front-of-load-path my-extended-package-dir)

;; Load themes in `my-themes-dir'
(let ((basedir my-themes-dir))
  (if (file-exists-p my-themes-dir)
	  (dolist (f (directory-files basedir))
		(if (and (not (or (equal f ".") (equal f "..")))
				 (file-directory-p (concat basedir f)))
			(add-to-list 'custom-theme-load-path (concat basedir f))))
	(warn "%s does not exist; no custom themes loaded"
		  my-extended-package-dir)))

(defun dont-show-whitespace ()
  "Disable showing whitespace in the current buffer."
  (setq-local show-trailing-whitespace nil)
  (whitespace-mode 0))

(defun show-whitespace ()
  "Enable showing whitespace in the current buffer."
  (setq-local show-trailing-whitespace t)
  (whitespace-mode 1))

(defun toggle-show-whitespace ()
  "Toggle showing whitespace in the current buffer."
  (interactive)
  (if (or show-trailing-whitespace whitespace-mode)
	  (dont-show-whitespace)
	(show-whitespace)))

(defun disable-string-face ()
  "Disable the string face."
  (setq-local font-lock-string-face nil))


(define-prefix-command 'my-extended-map)
;; Extended custom commands (C-c x)
(define-key mode-specific-map (kbd "x") 'my-extended-map)

(define-prefix-command 'my-toggle-map)
;; Custom commands for toggling (C-c t)
(define-key mode-specific-map (kbd "t") 'my-toggle-map)

(define-prefix-command 'my-music-map)
;; Music commands (C-c m)
(define-key mode-specific-map (kbd "m") 'my-music-map)

(define-prefix-command 'my-mode-map)
;; Actual mode specific commands (C-c y)
(define-key mode-specific-map (kbd "y") 'my-mode-map)

;; Use more flexible completion styles
(setq completion-styles (append completion-styles
								'(substring flex)))

;; Do complete .bin files
(setq completion-ignored-extensions
	  (remove ".bin" completion-ignored-extensions))

;; Ignore .emacs.d in recentf
(setq recentf-exclude
	  (append recentf-exclude
			  (list (concat "^"
							(replace-regexp-in-string
							 "\\." "\\."
							 (expand-file-name my-emacs-dir) t t)
							"/.*"))))

;; Desktop/session saving
(with-eval-after-load "desktop"
  (add-to-list 'desktop-path my-desktop-dir))
;; TODO Configure `frameset-filter-alist' so the stored session is more clean.

;; Set backup directory
(setq backup-directory-alist `(("." . ,my-backup-dir)))

;; Use visible bell instead of tone
(setq visible-bell t)

;; Show tooltips in full
(setq tooltip-resize-echo-area t)

;; Display line numbers
(setq-default display-line-numbers my-line-number-format)

;; Shorten yes/no prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Change font
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-11"))

;; 😀
;; (set-fontset-font t 'symbol "Noto Color Emoji")
;; (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
;; (set-fontset-font t '(#x1f300 . #x1fad0) "Ubuntu Mono Regular")
;; (set-fontset-font t '(#x1f300 . #x1fad0) "Noto Color Emoji")
;; (set-fontset-font t '(#x1f600 . #x1f64f)
;; 				  (font-spec :name "Noto Color Emoji:" :registry "iso10646-1") nil 'prepend)
;; (set-fontset-font "fontset-default" 'symbol "Noto Color Emoji" nil 'prepend)

;; Start maximized (does not work with Emacsclient)
;; Can use `default-frame-alist', however, then _every_ new frame is maximized;
;; that works with Emacsclient.
;; `after-make-frame-functions' and `server-after-make-frame-hook' have
;; the same problem.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; We use `default-frame-alist' and remove the entry in
;; `after-make-frame-functions'.
(when (daemonp)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (defun remove-default-frame-maximized (&optional _frame)
	"Remove the entry (fullscreen . maximized) from `default-frame-alist'.
Afterwards, remove this hook from `after-make-frame-functions'."
	(setq default-frame-alist
		  (delete '(fullscreen . maximized) default-frame-alist))
	(remove-hook 'after-make-frame-functions 'remove-default-frame-maximized))
  (add-hook 'after-make-frame-functions 'remove-default-frame-maximized))

;; Use Flyspell for strings and comments by default
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; Use it for all text when writing e-mails
(add-hook 'message-mode-hook 'flyspell-mode)
;; (add-hook 'mail-mode-hook 'flyspell-mode)

;; Deactivate scroll bars
(add-hook 'emacs-startup-hook (lambda () (scroll-bar-mode 0)))

;; Larger undo limits
(setq undo-limit (* undo-limit 100))
(setq undo-strong-limit (* undo-strong-limit 100))
(setq undo-outer-limit (* undo-outer-limit 10))

;; Smooth (mouse) scrolling
(when (require 'pixel-scroll nil t)
  (pixel-scroll-mode 1))

;; Vim-like autoscroll
(setq scroll-conservatively 1)

;; Use tab for completion if line is already indented.
;; Activate if it is dangerous (to go alone (without `company')).
(unless (functionp 'company-complete)
  (setq tab-always-indent 'complete))

;; Autoclose blocks in LaTeX mode
(add-hook 'latex-mode-hook 'latex-electric-env-pair-mode)
;; Auto-fill in TeX mode
(add-hook 'tex-mode-hook 'auto-fill-mode)

;; Disable whitespace and string face in Eshell, comint, Shell, and Term mode
(dolist (mode-hook
		 '(eshell-mode-hook comint-mode-hook shell-mode-hook term-mode-hook))
  (add-hook mode-hook (lambda ()
						(dont-show-whitespace)
						(disable-string-face))))


;;;; Built-in packages

;;; Eshell

;; Do not suggest aliasing
(setq eshell-bad-command-tolerance 1.0e+INF)

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
				'(("git" "diff" "help" "log" "reflog" "show")))))

;;; Dired
(add-hook 'dired-after-readin-hook 'dont-show-whitespace)

;;; Icomplete
;; (icomplete-mode 1)
;; (fido-mode 1)

;; (add-hook 'icomplete-minibuffer-setup-hook
;; 		  (lambda ()
;; 			;; FIXME find out if ivy is active
;; 			(unless (eq (selected-window) (ivy--get-window ivy-last))
;; 			  (setq icomplete-separator "\n")
;; 			  (setq truncate-lines t)
;; 			  (enlarge-window (1- icomplete-prospects-height)))))
;; (setq icomplete-in-buffer t)
;; (setq icomplete-show-matches-on-no-input nil)
(setq icomplete-hide-common-prefix t)
(setq icomplete-tidy-shadowed-file-names t)
;; (setq icomplete-prospects-height 2)
;; (setq icomplete-prospects-height 10)  ;; like ivy

;;; Ido
;; (ido-mode 1)
;; (add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)
;; TODO ido-everywhere seems to be removed

;;; ERC
(with-eval-after-load "erc"
  (setq erc-default-server "chat.freenode.net")
  (when (gnutls-available-p)
	;; Use TLS by default.
	(setq erc-default-port erc-default-port-tls)
	(setq erc-server-connect-function 'erc-open-tls-stream)))

;;; Rcirc
;; freenode.net default port is 6667; 6697 for TLS connections
(with-eval-after-load "rcirc"
  (if (gnutls-available-p)
	  (add-to-list 'rcirc-server-alist
				   '("chat.freenode.net"
					 :port 6697 :encryption tls))
	(add-to-list 'rcirc-server-alist
				 '("chat.freenode.net"	; :channels ("#rcirc")
				   ;; Don't use the TLS port by default, in case gnutls is not available.
				   :port 6667))))

;;; EWW
;; TODO instead of this, try to request the generated website with a slightly
;; TODO smaller screen size
(add-hook 'eww-mode-hook (lambda () (setq-local display-line-numbers nil)))

;;; xwidgets webkit
(when (functionp 'xwidget-webkit-browse-url)
  (add-hook 'xwidget-webkit-mode-hook
			(lambda ()
			  (local-set-key (kbd "<mouse-4>") 'xwidget-webkit-scroll-down)
			  (local-set-key (kbd "<mouse-5>") 'xwidget-webkit-scroll-up)
			  ))

  ;; Only allow local files to be opened with xwidgets.
  ;; FIXME False sense of security; in xwidget buffers, we can still follow
  ;; non-local links.
  (defun xwidget-webkit-goto-uri--allow-files-only (orig-fun &rest args)
	"Only allow local files when visiting URIs via xwidget webkit.
Local file are filtered using \"^file://\" as the filter pattern.
Advice around ORIG-FUN, called with ARGS."
	(if (eq (string-match "^file://" (nth 1 args)) 0)
		(apply orig-fun args)
	  (message "currently only allowing local files; disable `allow-files-only' advice on `xwidget-webkit-goto-uri'")
	  (apply orig-fun
			 (cons (car args)
				   (cons
					;; Can we do "about:blank"?
					(concat "file://"
							(expand-file-name
							 "only-files-error.html" my-emacs-dir))
					(cddr args))))))
  (advice-add 'xwidget-webkit-goto-uri :around
			  #'xwidget-webkit-goto-uri--allow-files-only
			  '((name . "allow-files-only")))

  ;; FIXME analyze and fix (better to fix below depending on values of `proc' and `arg')
  (defun xwidget-webkit-callback--allow-files-only (orig-fun &rest args)
	"Only allow local files on script callbacks.
Advice around ORIG-FUN, called with ARGS."
	(message (format "xwidget-event-type %s" (nth 1 args)))
	(message (format "proc %s" (nth 3 last-input-event)))
	(message (format "arg %s" (nth 4 last-input-event))))
  (advice-add 'xwidget-webkit-callback :around
			  #'xwidget-webkit-callback--allow-files-only
			  '((name . "allow-files-only")))

  ;; FIXME analyze and fix
;;   (defun xwidget-webkit-execute-script--allow-files-only (orig-fun &rest args)
;; 	"Only allow local files on script callbacks.
;; Advice around ORIG-FUN, called with ARGS."
;; 	(message (format "xwidget-event-type %s" (nth 1 args)))
;; 	(message (format "proc %s" (nth 3 last-input-event)))
;; 	(message (format "arg %s" (nth 4 last-input-event))))
;;   (advice-add 'xwidget-webkit-execute-script :around
;; 			  #'xwidget-webkit-execute-script--allow-files-only
;; 			  '((name . "allow-files-only")))

  (defun xwidget-webkit-open-file (file &optional new-session)
	"Open a local FILE from the xwidget webkit browser.
If NEW-SESSION is non-nil, start a new session."
	(interactive "fxwidget-webkit File: ")
	(xwidget-webkit-browse-url
	 (concat "file://"
			 (and (memq system-type '(windows-nt ms-dos)) "/")
			 (expand-file-name file))
	 new-session)))

;; Avoid performance issues in files with very long lines.
(when (functionp 'global-so-long-mode)
  (global-so-long-mode 1))

;;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Use EDE everywhere
(global-ede-mode 1)					 ; conflicts with org-mode binding

;;; RefTeX
;; (require 'reftex)
(autoload 'turn-on-reftex "reftex")
(add-hook 'latex-mode-hook 'turn-on-reftex)

(with-eval-after-load "reftex"
  (add-hook 'reftex-select-bib-mode-hook 'dont-show-whitespace)
  (add-to-list 'reftex-include-file-commands "includeonly"))

;;; Tramp
(when (require 'tramp nil t)
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
  ;; (add-to-list 'tramp-remote-process-environment
  ;; 			   (format "DISPLAY=localhost%s" (getenv "DISPLAY")))

  ;; Speed ups
  ;; If files are not updated outside of Tramp
  ;; (setq remote-file-name-inhibit-cache nil)

  ;; Speed up completions
  ;; (setq tramp-completion-reread-directory-timeout nil)

  ;; Disable version control
  ;; (setq vc-ignore-dir-regexp
  ;; 		(format "\\(%s\\)\\|\\(%s\\)"
  ;; 				vc-ignore-dir-regexp
  ;; 				tramp-file-name-regexp))
  ;; or
  ;; (setq vc-handled-backends '(Git))

  (defun remote-shell ()
	"Start a remote shell with the correct TERM environment variable."
	(interactive)
	(let ((process-environment (cons "TERM=xterm-256color" process-environment)))
	  (shell)))

  ;; Example `tramp-default-proxies-alist' (passthrough proxies)
  ;; (add-to-list 'tramp-default-proxies-alist
  ;; 			   '("^\\(host1\\|host2\\|host3\\)$"  ; or nil to always match as below
  ;; 				 nil  ; or user regex as above
  ;; 				 "/ssh:%u@my.domain.org:"))

  ;; Docker integration on Linux ("/docker:")
  (when (eq system-type 'gnu/linux)
	(push
	 (cons
	  "docker"
	  '((tramp-login-program "docker")
		(tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
		(tramp-remote-shell "/bin/sh")
		(tramp-remote-shell-args ("-i") ("-c"))))
	 tramp-methods)

	(defun tramp-completion-handle-file-name-all-completions--docker-containers
		(orig-fun &rest args)
	  "Return a list of active Docker container names concatenated with colons.
Activated when
`tramp-completion-handle-file-name-all-completions' is called
with second argument \"/docker:\"."
	  (if (equal (nth 1 args) "/docker:")
		  (let* ((dockernames-raw
				  (shell-command-to-string
				   "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
				 (dockernames
				  (cl-remove-if-not
				   #'(lambda (dockerline) (string-match ":$" dockerline))
				   (split-string dockernames-raw "\n"))))
			dockernames)
		(apply orig-fun args)))
	(advice-add
	 'tramp-completion-handle-file-name-all-completions :around
	 #'tramp-completion-handle-file-name-all-completions--docker-containers))

	;; (defadvice tramp-completion-handle-file-name-all-completions
	;; 	(around dotemacs-completion-docker activate)
	;;   "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
	;; a list of active Docker container names, followed by colons."
	;;   (if (equal (ad-get-arg 1) "/docker:")
	;; 	  (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
	;; 			 (dockernames (cl-remove-if-not
	;; 						   #'(lambda (dockerline) (string-match ":$" dockerline))
	;; 						   (split-string dockernames-raw "\n"))))
	;; 		(setq ad-return-value dockernames))
	;; 	ad-do-it))
  )

;;; Flymake
(when (and (>= emacs-major-version 26)
		   (require 'flymake nil t))
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (add-hook 'prog-mode-hook #'flymake-mode)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

  ;; Do not give byte-compile warnings in Emacs Lisp files when we don't want
  ;; it byte-compiled.
  ;; Does not work for the mode hook because the file-local variables haven't
  ;; been initialized.
  (add-hook 'find-file-hook
			(lambda ()
			  (when (and (derived-mode-p 'emacs-lisp-mode)
						 (cdr
						  (assq 'no-byte-compile file-local-variables-alist)))
				(remove-hook 'flymake-diagnostic-functions
							 'elisp-flymake-byte-compile t))))

  (defun toggle-flymake-mode ()
	"Toggle Flymake mode."
	(interactive)
	(if (eq flymake-mode nil)
		(flymake-mode 1)
	  (flymake-mode 0)))

  ;; Toggle Flymake mode (C-c t f)
  (define-key my-toggle-map (kbd "f") 'toggle-flymake-mode))

;;;; Org mode

(setq org-directory (expand-file-name "org" my-emacs-dir))
(setq org-publish-timestamp-directory
	  (file-name-as-directory
	   (expand-file-name ".org-timestamps" my-emacs-dir)))

;; Keys
(setq org-disputed-keys
	  (quote
	   (;; these are the defaults (changed bindings)
		;; org-shiftup C-S-p
		([(shift up)] . [(control shift p)])
		;; alternatively default M-p
		;; ([(shift up)] . [(meta p)])
		;; org-shiftdown C-S-n
		([(shift down)] . [(control shift n)])
		;; alternatively default M-n
		;; ([(shift down)] . [(meta n)])
		;; org-shiftleft C-S-b
		([(shift left)] . [(control shift b)])
		;; alternatively default M--
		;; ([(shift left)] . [(meta -)])
		;; org-shiftright C-S-f
		([(shift right)] . [(control shift f)])
		;; alternatively default M-+
		;; ([(shift right)] . [(meta +)])
		;; org-shiftcontrolright (C-M-S-f) (default: M-S-+ (M-4))
		([(control shift right)] . [(control meta shift f)])
		;; alternatively C-M-+
		;; ([(control shift right)] . [(control meta +)])
		;; org-shiftcontrolleft (C-M-S-b) (default: M-S-- (M-_))
		([(control shift left)] . [(control meta shift b)])
		;; alternatively C-M--
		;; ([(control shift left)] . [(control meta -)])

		;; these are custom
		;; TODO do these even exist?
		;; ;; TODO C-M-S-b (C-M-B)
		;; ([(control meta shift left)] . [(control meta shift b)])
		;; ;; TODO C-M-S-f (C-M-F)
		;; ([(control meta shift right)] . [(control meta shift f)])

		;; org-shiftcontrolup C-M-S-p (C-M-P)
		([(control shift up)] . [(control meta shift p)])
		;; alternatively M-S-p (M-P)
		;; ([(control shift up)] . [(meta shift p)])
		;; org-shiftcontroldown C-M-S-n (C-M-N)
		([(control shift down)] . [(control meta shift n)])
		;; alternatively M-S-n (M-N)
		;; ([(control shift down)] . [(meta shift n)])

		;; These are already assigned with `org-use-extra-keys'.
		;; Kept only for comfort.
		;; org-metaup M-S-p (M-P)
		([(meta up)] . [(meta shift p)])
		;; org-metadown M-S-n (M-N)
		([(meta down)] . [(meta shift n)])
		;; org-metaleft M-S-b (M-B)
		([(meta left)] . [(meta shift b)])
		;; org-metaright M-S-f (M-F)
		([(meta right)] . [(meta shift f)])
		;; org-shiftmetaup M-S-u (M-U)
		([(meta shift up)] . [(meta shift u)])
		;; org-shiftmetadown M-S-d (M-D)
		([(meta shift down)] . [(meta shift d)])
		;; org-shiftmetaleft M-S-l (M-L)
		([(meta shift left)] . [(meta shift l)])
		;; alternatively C-S-b
		;; ([(meta shift left)] . [(control shift b)])
		;; org-shiftmetaright M-S-r (M-R)
		([(meta shift right)] . [(meta shift r)])
		;; alternatively C-S-f
		;; ([(meta shift right)] . [(control shift f)])
		)))
(setq org-replace-disputed-keys t)
(setq org-use-extra-keys t)
(setq org-use-speed-commands t)
(setq org-goto-auto-isearch nil)

(setq org-catch-invisible-edits 'smart)
;; TODO org-list-demote-modify-bullet maybe
;; TODO org-list-indent-offset 1 or 2 maybe

;; Only remove highlighting by executing C-c C-c
(setq org-remove-highlights-with-change nil)
;; Allow starting lists with letters
(setq org-list-allow-alphabetical t)
(setq org-log-done (quote time))

;; Do not allow DONE when children are not DONE
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

;; Custom TODO keyword faces
(setq org-todo-keyword-faces '(("INFO" . "blue")
							   ("STARTED" . "yellow")
							   ("ISSUE" . org-warning)
							   ("CANCELED" . (:foreground "red" :weight bold))))

;; Persistent clock history
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; Export settings

(setq org-export-with-smart-quotes t)
;; Use HTML5
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)

;;; Org Babel

(setq org-confirm-babel-evaluate nil)
;; Lexical binding by default
(setcdr (assq :lexical org-babel-default-header-args:emacs-lisp) "yes")
(setq org-src-preserve-indentation t)
;; (setq org-edit-src-content-indentation 0)

;; Open source code over current window.
(setq org-src-window-setup 'current-window)
;; Correctly show whitespace
(add-hook 'org-src-mode-hook 'show-whitespace)

;; If we ever use ob-async...
;; (setq ob-async-no-async-languages-alist
;; 	  '("jupyter-python" "jupyter-julia"))

(setq my-org-babel-load-languages '())

;; Other settings and keybindings
;; TODO org-install is obsolete; remove it when backward compatibility
;; is not necessary anymore
(when (and (or (>= emacs-major-version 27) (require 'org-install nil t))
		   (require 'org-habit nil t)
		   (require 'org-protocol nil t)
		   (require 'org-crypt nil t)
		   (require 'org-tempo nil t))
  ;; First agenda file as notes file
  (when (> (length org-agenda-files) 0)
	(setq org-default-notes-file (car org-agenda-files)))

  (defun org-beamer-mode-or-select-beamer-environment ()
	"Call function `org-beamer-mode' or `org-beamer-select-environment'.
The choice is made depending on if variable `org-beamer-mode' is non-nil."
	(interactive)
	(if (eq org-beamer-mode nil)
		(org-beamer-mode)
	  (org-beamer-select-environment)))

  ;; Show agenda when opening Emacs
  (defun open-monthly-agenda-deselected (&optional span)
	"Open the agenda for the next SPAN days (default 30).
Afterwards, switch to the previous window."
	;; (org-agenda-list nil nil (if span span 30))
	(org-agenda-run-series "Agenda and all TODOs" `(((agenda "" ((org-agenda-span (quote ,(if span span 30))))) (alltodo ""))))
	;; Delete other window so we always open it vertically.
	(delete-window (selected-window))
	(switch-to-buffer-other-window "*Org Agenda*" t)
	(select-window (previous-window)))

  (if (not (daemonp))
	  (add-hook 'window-setup-hook 'open-monthly-agenda-deselected)
	(defun open-monthly-agenda-deselected-and-remove (&optional _frame)
	  "Run `open-monthly-agenda-deselected'.
Afterwards, remove it from `after-make-frame-functions'."
	  (open-monthly-agenda-deselected)
	  (remove-hook 'after-make-frame-functions
				   'open-monthly-agenda-deselected-and-remove))
	(add-hook 'after-make-frame-functions
			  'open-monthly-agenda-deselected-and-remove))

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (add-hook 'org-mode-hook 'org-display-inline-images)
  (add-hook 'message-mode-hook 'turn-on-orgtbl)
  ;; (add-hook 'mail-mode-hook 'turn-on-orgtbl)

  ;; Org keybindings (C-c o)
  (define-prefix-command 'my-org-map)
  (define-key mode-specific-map (kbd "o") 'my-org-map)
  (define-key my-org-map (kbd "n") 'org-footnote-action)
  (define-key my-org-map (kbd "l") 'org-store-link)
  (define-key my-org-map (kbd "a") 'org-agenda)
  (define-key my-org-map (kbd "t") 'org-todo-list)
  (define-key my-org-map (kbd "o") 'org-switchb)
  (define-key my-org-map (kbd "c") 'org-capture)

  ;; Activate beamer mode or select beamer environment (C-c o b)
  ;; Use C-c C-b for navigation like always
  (add-hook 'org-beamer-mode-hook
			(lambda () (define-key org-beamer-mode-map (kbd "C-c C-b") nil)))
  (define-key my-org-map (kbd "b")
	'org-beamer-mode-or-select-beamer-environment)

  ;; Calendar minibuffer
  ;; Shift
  (define-key org-read-date-minibuffer-local-map (kbd "M-P")
	(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-N")
	(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-B")
	(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-F")
	(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
  ;; Shiftmeta
  (define-key org-read-date-minibuffer-local-map (kbd "M-L")
	(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-R")
	(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))

  ;; Open PDFs in Emacs
  (setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)


  ;; Enable LaTeX letter class
  (with-eval-after-load 'ox-latex
	(add-to-list 'org-latex-classes
				 '("letter" "\\documentclass[11pt]{letter}"
				   ("\\opening{%s}")
				   ("\\closing{%s}")
				   ("\\ps{}\n%s")
				   ("\\cc{%s}")
				   ("\\encl{%s}"))))

  (defun my-org-plain-text-filter (to-replace
								   mode-replacement-alist
								   text backend info
								   &optional fixedcase literal subexp start)
	"Filter TO-REPLACE in TEXT using replacements in MODE-REPLACEMENT-ALIST.
The replacement for TO-REPLACE is given by the entry in
MODE-REPLACEMENT-ALIST for BACKEND.
MODE-REPLACEMENT-ALIST should contain entries of the form (MODE . REPLACEMENT),
where MODE is a potential parent backend symbol of BACKEND and REPLACEMENT
is a string.
INFO is the export communication channel.

For FIXEDCASE, LITERAL, SUBEXP and START, see `replace-match'."
	(let ((replacement
		   (cdr (seq-some (lambda (elem)
							(org-export-derived-backend-p backend (car elem)))
						  mode-replacement-alist))))
	  (when replacement
		(replace-regexp-in-string to-replace replacement text
								  fixedcase literal subexp start))))

  (defun my-org-plain-text-filter-no-break-space (text backend info)
	"Ensure non-breaking spaces (\" \") are properly handled in Org export.
TEXT is the text to be exported, BACKEND is the export backend
and INFO the export communication channel."
	(my-org-plain-text-filter " " '((latex . "~")
									(html . "&nbsp;"))
							  text backend info t t))

  (defun my-org-plain-text-filter-zero-width-space (text backend info)
	"Ensure zero-width spaces (\"​\") are properly handled in Org export.
TEXT is the text to be exported, BACKEND is the export backend
and INFO the export communication channel."
	(my-org-plain-text-filter "​" '((latex . "\\hspace{0pt}")
								   (html . "&#8203;"))
							  text backend info t t))

  (defun my-org-plain-text-filter-zero-width-no-break-space (text backend info)
	"Ensure zero-width no-break spaces are properly handled in Org export.
Zero-width no-break spaces (\"﻿\") are deprecated and should only be treated as
such when not at the start of a file.
TEXT is the text to be exported, BACKEND is the export backend
and INFO the export communication channel."
	;; FIXME Should only be treated this way if not at the start of the _file_.
	;; We currently ignore the first char of the given text for simplicity.
	(concat (substring text 0 1)
			(my-org-plain-text-filter "﻿" '((latex . "\\nobreak{}")
										   (html . "&#65279;"))
									  text backend info t t nil 1)))

  (defun my-org-plain-text-filter-word-joiner (text backend info)
	"Ensure word joiners (\"⁠\") are properly handled in Org export.
TEXT is the text to be exported, BACKEND is the export backend
and INFO the export communication channel."
	(my-org-plain-text-filter "⁠" '((latex . "\\nolinebreak{}")
								   (html . "&#8288;"))
							  text backend info t t))

  (with-eval-after-load 'ox
	(setq org-export-filter-plain-text-functions
		  (append org-export-filter-plain-text-functions
				  '(my-org-plain-text-filter-no-break-space
					my-org-plain-text-filter-zero-width-space
					my-org-plain-text-filter-word-joiner))))


  ;; TODO only load languages when they're in path; don't load shell on windows
  (setq my-org-babel-load-languages
		(append my-org-babel-load-languages
				'((emacs-lisp . t)
				  (shell . t)
				  (python . t)
				  (C . t)
				  (js . t)))))


;; load-theme "fixes"
;; Correctly switch themes by first `disable-theme'ing
;; the current one, then `load-theme'ing the other.
(advice-add 'load-theme :before
			(lambda (&rest _args) (mapc #'disable-theme custom-enabled-themes))
			'((name . "theme-dont-propagate")))

(defun load-theme--restore-scroll-bar-mode (orig-fun &rest args)
  "Restore variable `scroll-bar-mode' after theme switch.
Adice around ORIG-FUN, called with ARGS."
  (let ((current-scroll-bar-mode (get-scroll-bar-mode)))
	(progn
	  (apply orig-fun args)
	  (set-scroll-bar-mode current-scroll-bar-mode))))
(advice-add 'load-theme :around #'load-theme--restore-scroll-bar-mode)

(defun safe-load-theme (theme default-theme)
  "Load THEME but if it is not available, load DEFAULT-THEME."
  (condition-case nil
	  (load-theme theme t)
	(error (load-theme default-theme t))))

(defun update-frame-background-mode ()
  "Update `frame-background-mode' for all frames."
  (mapc 'frame-set-background-mode (frame-list)))

(defun load-theme-getenv (light-theme dark-theme default-theme
									  envvar dark-pattern)
  "Load either the given light, dark, or default theme.
The themes are respectively defined by LIGHT-THEME, DARK-THEME
and DEFAULT-THEME.
The choice depends on if the given environment variable ENVVAR is
equal to the given pattern DARK-PATTERN for choosing a dark theme
which activates the dark theme variant."
  ;; Set frame background and fix defaults if not available.
  (if (not (equal (getenv envvar) ""))
	  (if (equal (getenv envvar) dark-pattern)
		  (progn (setq frame-background-mode 'dark)
				 (update-frame-background-mode)
				 (safe-load-theme dark-theme my-fallback-dark-theme))
		(setq frame-background-mode 'light)
		(update-frame-background-mode)
		(safe-load-theme light-theme my-fallback-light-theme))
	;; Load theme
	(safe-load-theme default-theme my-fallback-default-theme)))

(load-theme-getenv my-graphic-light-theme
				   my-graphic-dark-theme
				   my-graphic-default-theme
				   "SOLARIZED_THEME"
				   "dark")

(when (daemonp)
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

;; TODO write toggle
;; (setq compilation-scroll-output t)
;; Fix colors in compilation mode
(autoload 'ansi-color-apply-on-region "ansi-color")
(defun ansi-colorize-buffer ()
  "Apply ANSI color codes in the current buffer."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun ansi-colorize-compilation-buffer ()
  "Apply ANSI color codes to the compilation buffer."
  ;; Only do this when we compile; don't do it for grep, for example
  (when (eq major-mode 'compilation-mode)
	(ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'ansi-colorize-compilation-buffer)


;; TODO Find out how to automatically get comment strings. And use that instead
;; of the hardcoded regex for _all_ occurrences.
(defun highlight-todos ()
  "Highlight TODO-related keywords."
  (font-lock-add-keywords nil
						  '(("\\<\\(\\(?:TODO\\|FIXME\\|XXX\\)[Ss]?\\>:?\\)" 1
							 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'highlight-todos)
(add-hook 'tex-mode-hook  'highlight-todos)

;; Fix `list-colors-display' (only works with `global-font-lock' deactivated)
(defun quit-window--list-colors-display-reactivate-font-lock (&rest _args)
  "Reactivate font-lock after quitting the *Colors* buffer."
  (if (equal (buffer-name) "*Colors*")
	  (global-font-lock-mode 1)))
(advice-add 'list-colors-display :before
			(lambda (&rest _args) (global-font-lock-mode 0))
			'((name . "list-colors-deactivate-font-lock")))
(advice-add 'quit-window :before
			#'quit-window--list-colors-display-reactivate-font-lock)

;; TODO write interactive version for one color
(defun preview-hex-colors ()
  "Preview colors as the face color of HTML hex color codes (#fff or #0afff1)."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
	  (0 (put-text-property
		  (match-beginning 0)
		  (match-end 0)
		  'face (list :background
					  (let* (
							 (ms (match-string-no-properties 0))
							 (r (substring ms 1 2))
							 (g (substring ms 2 3))
							 (b (substring ms 3 4)))
						(concat "#" r r g g b b))))))
	 ("#[[:xdigit:]]\\{6\\}"
	  (0 (put-text-property
		  (match-beginning 0)
		  (match-end 0)
		  'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush))

;;; Ripgrep
(if (executable-find "rg")
	(setq grep-command
		  "rg --color always -nH --null --no-heading --smart-case -e "))

;;; Indentation
;; HTML indentation
(setq sgml-basic-offset 4)

;; Built-in modes
(add-hook 'sh-mode-hook
		  (lambda () (setq indent-tabs-mode nil)))
(add-hook 'picture-mode-hook
		  (lambda () (setq indent-tabs-mode nil)))


;;;; Package config

;;; GNU Global
(add-to-list 'load-path my-gtags-dir)
(autoload 'gtags-mode "gtags" "" t)

(add-hook 'prog-mode-hook (lambda () (gtags-mode 1)))

;;; Ggtags
(add-hook 'c-mode-common-hook
		  (lambda ()
			(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
			  (progn
				(ggtags-mode 1)
				(setq-local hippie-expand-try-functions-list
							(cons 'ggtags-try-complete-tag
								  hippie-expand-try-functions-list))))))

;;; Constants
(autoload 'constants-insert "constants" "Insert constants into source." t)
(autoload 'constants-get "constants" "Get the value of a constant." t)
(autoload 'constants-replace "constants" "Replace name of a constant." t)
(define-prefix-command 'my-constants-map)
;; Commands for constants (C-c x C)
(define-key 'my-extended-map "C" 'my-constants-map)
(define-key 'my-constants-map "i" 'constants-insert)
(define-key 'my-constants-map "g" 'constants-get)
(define-key 'my-constants-map "r" 'constants-replace)

;;; AUCTeX
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
(when (require 'flymake nil t)
  (add-hook 'LaTeX-mode-hook #'flymake-mode))
(add-hook 'LaTeX-mode-hook (lambda () (electric-pair-local-mode 0)))
(setq reftex-plug-into-AUCTeX t)
;; Find/visit `TeX-master' (C-c y v)
(with-eval-after-load 'auctex
  (define-key TeX-mode-map (kbd "C-c y v")
	(lambda () (interactive) (find-file (concat (TeX-master-file) ".tex")))))

;; Use PDF-Tools
(when (functionp 'pdf-tools-install)
  (add-hook 'TeX-mode-hook
			(lambda () (setf (nth 1
								  (assq 'output-pdf TeX-view-program-selection))
							 "PDF Tools")))
  ;; If PDF-Tools are used: TODO really only then?
  (add-hook 'TeX-after-compilation-finished-functions
			'TeX-revert-document-buffer))
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

;;; Num3 mode
(global-num3-mode)
;; (setq num3-threshold 4)

;;; Dumb Jump
(when (functionp 'dumb-jump-mode)
	(dumb-jump-mode))

;;; Magit
(when (and (>= emacs-major-version 26)
		   (require 'magit nil t))
  ;; Always wants to save all files whenever we save anything in the repo
  ;; (magit-wip-mode 1)

  ;; Magit keybindings (C-c g)
  (define-prefix-command 'my-magit-map)
  (define-key mode-specific-map (kbd "g") 'my-magit-map)
  (define-key my-magit-map (kbd "g") 'magit-status)
  (define-key my-magit-map (kbd "G") 'magit-dispatch-popup)
  (define-key magit-file-mode-map (kbd "C-c g f") 'magit-file-dispatch))

;;; EMMS
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
		"Prepare playing a random track in playlist \"emms-music\".
The playlist must be in `my-music-dir'."
		(interactive)
		(let ((playlist-file (expand-file-name "emms-music" my-music-dir)))
		  (if (file-exists-p playlist-file)
			  (emms-play-playlist playlist-file)))
		(emms-random)
		(emms-stop)
		(update-emms-faces))

	  (when (>= emacs-major-version 26)
		  (add-hook 'after-init-hook 'init-emms))

	  ;; EMMS key bindings (C-c m)
	  (define-key my-music-map (kbd "SPC") 'emms-pause)
	  (define-key my-music-map (kbd "s") 'emms-stop)
	  (define-key my-music-map (kbd "p") 'emms-previous)
	  (define-key my-music-map (kbd "n") 'emms-next)
	  (define-key my-music-map (kbd "r") 'emms-random)
	  (define-key my-music-map (kbd "m") 'emms-shuffle)
	  (define-key my-music-map (kbd "b") 'emms-seek-backward)
	  (define-key my-music-map (kbd "f") 'emms-seek-forward)
	  (define-key my-music-map (kbd "l") 'emms)
	  (define-key my-music-map (kbd "i") 'init-emms)
	  (define-key my-music-map (kbd "+") 'emms-volume-raise)
	  (define-key my-music-map (kbd "-") 'emms-volume-lower))

  ;; TODO remove when hook is added
  (defun update-emms-faces ()
	"No op."
	()))


;;; dired-git-info
(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode))

;;; Undohist

(require 'undohist)
(undohist-initialize)

;;;; Evil mode
(setq evil-flash-delay 20)
(setq evil-want-Y-yank-to-eol t)
(setq evil-want-change-word-to-end nil)

(setq evil-ex-substitute-global t)

(setq evil-vsplit-window-right t)
(setq evil-shift-round nil)
(setq evil-shift-width 4)

(setq evil-search-module 'evil-search)

;; Change insert state to basically Emacs state (hybrid state)
(setq evil-disable-insert-state-bindings t)

;; Required for Evil Collection
;; (setq evil-want-keybinding nil)

;; TODO add optional viper support
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
	  (when (functionp 'global-evil-surround-mode)
		(global-evil-surround-mode 1))
	  ;; evil-commentary
	  (when (functionp 'evil-commentary-mode)
		(evil-commentary-mode 1))
	  ;; evil-lion
	  (when (functionp 'evil-lion-mode)
		(evil-lion-mode))
	  ;; evil-matchit
	  (when (require 'evil-matchit nil t)
		  (global-evil-matchit-mode 1))
	  ;; evil-visualstar
	  (when (functionp 'global-evil-visualstar-mode)
		(global-evil-visualstar-mode 1))

	  ;; Emacs state by default (must be added to head of list)
	  ;; (add-to-list 'evil-buffer-regexps '("." . emacs))
	  (setq evil-default-state 'emacs)
	  ;; Except in these modes
	  (evil-set-initial-state 'prog-mode 'normal)
	  (evil-set-initial-state 'text-mode 'normal)
	  (evil-set-initial-state 'tex-mode  'normal)
	  (evil-set-initial-state 'conf-mode 'normal)
	  (evil-set-initial-state 'evil-command-window-mode 'normal)
	  ;; But also not in these (possibly inherited) modes
	  (evil-set-initial-state 'help-mode     'emacs)
	  (evil-set-initial-state 'messages-buffer-mode 'emacs)
	  (evil-set-initial-state 'Info-mode     'emacs)
	  (evil-set-initial-state 'Man-mode      'emacs)
	  (evil-set-initial-state 'comint-mode   'emacs)
	  (evil-set-initial-state 'shell-mode    'emacs)
	  (evil-set-initial-state 'term-mode     'emacs)
	  (evil-set-initial-state 'message-mode  'emacs)
	  ;; (evil-set-initial-state 'mail-mode     'emacs)
	  (evil-set-initial-state 'org-mode      'emacs)
	  (evil-set-initial-state 'calendar-mode 'emacs)
	  (evil-set-initial-state 'picture-mode  'emacs)
	  (evil-set-initial-state 'compilation-mode 'emacs)

	  ;; Magit commit message
	  (add-to-list 'evil-buffer-regexps '("COMMIT_EDITMSG" . emacs))

	  ;; Reset *Messages* buffer state
	  (evil-change-to-initial-state (messages-buffer))

	  ;; Picture mode
	  (add-hook 'picture-mode-hook
				(lambda () (evil-emacs-state)))
	  (advice-add 'picture-mode-exit :after
				  (lambda (&rest _args) (evil-change-to-previous-state))
				  '((name . "picture-mode-revert-state")))

	  ;; Evil mappings

	  ;; Leader key on SPC
	  (evil-set-leader 'normal (kbd "SPC"))

	  ;; Use previous substitute flags by default when repeating using &
	  (define-key evil-normal-state-map (kbd "&")
		'evil-ex-repeat-substitute-with-flags)

	  ;; Swap ' and ` in normal state
	  (define-key evil-normal-state-map (kbd "'") 'evil-goto-mark)
	  (define-key evil-normal-state-map (kbd "`") 'evil-goto-mark-line)

	  ;; C-S-d to delete-forward-char in insert state
	  (if (eq evil-disable-insert-state-bindings nil)
		(define-key evil-insert-state-map (kbd "C-S-d") 'evil-delete-char)
		;; Otherwise use bindings to shift line
		(define-key evil-insert-state-map (kbd "C-S-d") 'evil-shift-left-line)
		(define-key evil-insert-state-map (kbd "C-S-t") 'evil-shift-right-line))

	  ;; C-l to exit from any state to normal state
	  (define-key evil-insert-state-map   (kbd "C-l") 'evil-normal-state)
	  (define-key evil-operator-state-map (kbd "C-l") 'evil-normal-state)
	  (define-key evil-replace-state-map  (kbd "C-l") 'evil-normal-state)
	  (define-key evil-visual-state-map   (kbd "C-l") 'evil-normal-state)
	  ;; Here we do not necessarily go back to normal state but that's fine.
	  (define-key evil-ex-completion-map  (kbd "C-l") 'abort-recursive-edit)
	  (when (eq evil-disable-insert-state-bindings t)
		(define-key evil-insert-state-map (kbd "C-S-l") 'recenter-top-bottom))

	  ;; C-S-d in normal or motion state to scroll up (C-S-u fails in Ubuntu)
	  (define-key evil-normal-state-map (kbd "C-S-d") 'evil-scroll-up)
	  (define-key evil-motion-state-map (kbd "C-S-d") 'evil-scroll-up)

	  ;; Ex state (minibuffer) mappings
	  ;; C-b moves one char backward
	  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
	  ;; C-f moves one char forward
	  (define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
	  (define-key evil-ex-completion-map (kbd "C-S-f") 'evil-ex-command-window)
	  (define-key evil-ex-search-keymap (kbd "C-f") 'forward-char)
	  (define-key evil-ex-search-keymap (kbd "C-S-f") 'evil-ex-search-command-window)
	  ;; C-a moves to start of line
	  (define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
	  ;; C-d deletes char forward
	  (define-key evil-ex-completion-map (kbd "C-d") 'delete-char)
	  ;; C-k deletes line forward
	  (define-key evil-ex-completion-map (kbd "C-k") 'evil-delete-line)
	  (define-key evil-ex-completion-map (kbd "C-S-k") 'evil-insert-digraph)
	  ;; M-p gets previous complete history element
	  (define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
	  ;; M-n gets next complete history element
	  (define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

	  ;; C-l in normal state to remove highlighting
	  (define-key evil-normal-state-map (kbd "C-l") 'evil-ex-nohighlight)

	  (defun my-maybe-evil-repeat-pop ()
		"Execute `evil-repeat-pop' or `flyspell-auto-correct-word'.
The choice depends on the whether `evil-repeat-pop' makes sense to call."
		(interactive)
		(condition-case err
			(call-interactively 'evil-repeat-pop)
		  (user-error (if flyspell-mode
						  (call-interactively 'flyspell-auto-correct-word)
						(signal (car err) (cdr err))))))

	  ;; Prepare Xref
	  (setq xref-prompt-for-identifier (append xref-prompt-for-identifier
											   '(my-maybe-evil-repeat-pop-next)))

	  (defun my-maybe-evil-repeat-pop-next ()
		"Execute `evil-repeat-pop-next' or `xref-find-definitions'.
The choice depends on the whether `evil-repeat-pop-next' makes sense to call."
		(interactive)
		(condition-case nil
			(call-interactively 'evil-repeat-pop-next)
		  (user-error (call-interactively 'xref-find-definitions))))

	  ;; C-. executes `flyspell-auto-correct-word' if no prior repetition can
	  ;; be popped (only if `flyspell-mode' is enabled).
	  (define-key evil-normal-state-map (kbd "C-.") 'my-maybe-evil-repeat-pop)
	  ;; M-. executes `xref-find-definitions' if no prior repetition can
	  ;; be popped.
	  (define-key evil-normal-state-map (kbd "M-.")
		'my-maybe-evil-repeat-pop-next)

	  ;; C-r invokes undo-redo (since we do not use undo-tree)
	  (if (< emacs-major-version 28)
		  ;; C-r invokes undo-propose in versions without undo-redo
		  (define-key evil-normal-state-map (kbd "C-r") 'undo-propose)
		(define-key evil-normal-state-map (kbd "C-r") 'undo-redo))


	  ;; TODO this is most likely unnecessary
	  ;; ;; Toggle global Evil mode (C-c t v) (also toggle undo-tree-mode).
	  ;; ;; Does not disable evil-magit.
	  ;; ;; TODO what about evil minor modes?
;; 	  (defun toggle-global-evil ()
;; 	    "Toggle global Evil mode.
;; Also toggle undo-tree-mode."
;; 	    (interactive)
;; 	    (if (eq evil-mode t)
;; 		  (progn (evil-mode 0)
;; 				 (global-undo-tree-mode 0))
;; 		(progn (evil-mode 1)
;; 			   (global-undo-tree-mode 0))))
;; 	  (define-key my-toggle-map (kbd "v") 'toggle-global-evil)

	  ;; Evil-snipe
	  (when (require 'evil-snipe nil t)
		(evil-snipe-mode 1)

		(setq evil-snipe-smart-case nil)

		(setq evil-snipe-scope 'visible)
		(setq evil-snipe-repeat-scope 'visible)
		(setq evil-snipe-spillover-scope 'buffer))

	  ;; Use C-s to substitute (as "s" is taken by evil-snipe)
	  (define-key evil-normal-state-map (kbd "C-s") 'evil-substitute)
	  (define-key evil-normal-state-map (kbd "C-S-s") 'evil-change-whole-line)))


;;; Company
(when (functionp 'global-company-mode)
	  (add-hook 'after-init-hook 'global-company-mode)
	  ;; Faster auto completion
	  (setq company-minimum-prefix-length 2)
	  (setq company-idle-delay 0.1)

	  (setq company-dabbrev-downcase nil)

	  (setq company-selection-wrap-around t)
	  ;; Autocomplete (C-c n)
	  (define-key mode-specific-map (kbd "n") 'company-complete)

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
	  (company-tng-configure-default)

	  ;; Increase idle delay in remote shell (revert our fast completion config)
	  (when (require 'tramp nil t)
		(add-hook 'shell-mode-hook
				  (lambda ()
					(if (file-remote-p default-directory)
						(progn
						  (setq-local company-minimum-prefix-length 3)
						  (setq-local company-idle-delay 0.5)))))))

;;; Company quickhelp
(when (functionp 'company-quickhelp-mode)
  (company-quickhelp-mode)
  (setq company-quickhelp-delay 0.65)
  (define-key company-active-map
	(kbd "M-h") #'company-quickhelp-manual-begin))

;;; Company-lsp
;; (if (require 'company-lsp nil t)
;; 	(push 'company-lsp company-backends))

;;; Ivy
(when (and (functionp 'ivy-mode) t)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")

  ;; Cannot quit `visit-tags-table'; disable it
  (define-key ivy-mode-map (kbd "M-.")
	(lambda () (interactive) (message "not allowed here")))

  (global-set-key (kbd "C-x b")	'ivy-switch-buffer)
  (global-set-key (kbd "C-x 4 b") 'ivy-switch-buffer-other-window)

  ;; Resume Ivy dispatch (C-c r)
  (define-key mode-specific-map (kbd "r") 'ivy-resume)

  (when (functionp 'swiper)
	(global-set-key (kbd "C-s")	'swiper))

  (when (functionp 'counsel-M-x)
	(global-set-key (kbd "M-x")	'counsel-M-x)
	(global-set-key (kbd "C-x C-f") 'counsel-find-file)
	(global-set-key (kbd "C-x d")	'counsel-dired)
	(global-set-key (kbd "C-x r b") 'counsel-bookmark)
	(global-set-key (kbd "<f1> f")	'counsel-describe-function)
	(global-set-key (kbd "<f1> v")	'counsel-describe-variable)
	(global-set-key (kbd "<f1> l")	'counsel-find-library)
	(global-set-key (kbd "<f2> i")	'counsel-info-lookup-symbol)
	(global-set-key (kbd "<f2> u")	'counsel-unicode-char)

	(define-key mode-specific-map (kbd "j") 'counsel-semantic-or-imenu)
	(define-key my-music-map	  (kbd "o") 'counsel-rhythmbox))
  )


;;; Helm
(when (and nil (>= emacs-major-version 26)
		   (require 'helm-config nil t))
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
  ;; (with-eval-after-load "eshell"
  ;; 	(add-hook
  ;; 	 'eshell-mode-hook
  ;; 	 (lambda ()
  ;; 	   (eshell-cmpl-initialize)
  ;; 	   (define-key eshell-mode-map [remap eshell-pcomplete]
  ;; 		 'helm-esh-pcomplete)
  ;; 	   (define-key eshell-mode-map (kbd "M-p") 'helm-esh-history))))
  )

;;; Projectile
(when (functionp 'projectile-mode)
  (projectile-mode 1)
  ;; Projectile keybindings (C-c p)
  ;; (define-key mode-specific-map (kbd "p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;; YASnippet
(when (require 'yasnippet nil t)
  (yas-global-mode 1)
  ;; or (next two)
  ;; (yas-reload-all)
  ;; (add-hook 'prog-mode-hook #'yas-minor-mode)

  ;; This resolves YASnippet problems with company-tng.
  (define-key yas-minor-mode-map (kbd "C-c e") 'yas-expand)
  (define-key yas-keymap (kbd "C-c e") 'yas-next-field-or-maybe-expand)
  (dolist (keymap (list yas-minor-mode-map yas-keymap))
	(define-key keymap (kbd "TAB") nil)
	(define-key keymap (kbd "<tab>") nil)))

;;; Expand-region
;; (require 'expand-region)
(autoload 'er/expand-region "expand-region")
;; Expand region (C-c x e)
(define-key my-extended-map (kbd "e") 'er/expand-region)

;;; PDF-Tools
(when (functionp 'pdf-tools-install)
  (pdf-tools-install)
  ;; or (pdf-loader-install)

  ;; Start with fit page
  (add-hook 'pdf-view-mode-hook 'pdf-view-fit-page-to-window)
  (when (functionp 'swiper)
	(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward-regexp))

  ;; Enable printer
  (setq pdf-misc-print-program "lpr")

  ;; Create better Org mode links
  (require 'ol)

  (org-link-set-parameters "pdfview"
						   :follow 'org-pdfview-open
						   :export 'org-pdfview-export
						   :store 'org-pdfview-store-link)

  (defun org-pdfview-export (link description format)
	"Export a pdfview LINK with DESCRIPTION in FORMAT from Org files."
	(let ((path (if (string-match "\\(.*?\\)\\(?:::\\([0-9]+\\)\\)?$" link)
					(match-string 1 link)
				  link))
		  (page (and (match-beginning 2) (match-string 2 link)))
		  (desc (or description link)))
	  (when (strinp path)
		(setq path (expand-file-name path))
		(cond
		 ((eq format 'html)
		  (format "<a href=\"%s%s\">%s</a>"
				  path
				  (if (and page (not (string= page "1")))
					  (concat "#page=" page) "")
				  desc))
		 ((eq format 'latex) (format "\\href{run:%s}{%s}" path desc))
		 ((eq format 'ascii) (format "%s (%s)" desc path))
		 (t path)))))

  (defun org-pdfview-open (link)
	"Open a pdfview LINK from an Org file."
	(string-match "\\(.*?\\)\\(?:::\\([0-9]+\\)\\)?$" link)
	(let ((path (match-string 1 link))
		  (page (and (match-beginning 2)
					 (string-to-number (match-string 2 link)))))
	  (org-open-file path 1)
	  (when page (pdf-view-goto-page page))))

  (defun org-pdfview-store-link ()
	"Store a link to a pdfview buffer."
	(when (eq major-mode 'pdf-view-mode)
	  (let* ((path buffer-file-name)
			 ;; (pdf-view-current-page) calls the following
			 (page (pdf-view-current-page))
			 (link (concat "pdfview:" path
						   (if (not (eq page 1))
							   (concat "::" (number-to-string page)) "")))
			 (title (cdr (assoc 'title (pdf-info-metadata))))
			 (title (if (string-empty-p title)
						(file-name-nondirectory path)
					  title)))
		(org-link-store-props
		 :type "pdfview"
		 :link link
		 :description title))))

  (defun org-pdfview-complete-link ()
	"Complete file names for pdfview links.
Links to get the file name, then asks the user for the page number
and append it."
	(concat (replace-regexp-in-string "^file:" "docview:"
									  (org-link-complete-file))
			"::"
			(read-from-minibuffer "Page:" "1")))
  )

;;; Emacs libvterm

;; Disable whitespace and string face in VTerm mode
;; TODO these do not work
;; (autoload 'vterm-mode-hook "vterm")
;; (add-hook 'vterm-mode-hook 'toggle-show-whitespace)
(if (functionp 'vterm)
	(progn
	  (advice-add 'vterm :after
				  (lambda (&rest _args)
					(dont-show-whitespace)
					(disable-string-face))
				  '((name . "vterm-disable-special-visuals")))
	  (define-key my-extended-map (kbd "t") 'vterm))
  ;; Enter terminal (C-c x t)
  (define-key my-extended-map (kbd "t") 'term))

;;; toc-org
(when (require 'toc-org nil t)
  (add-hook 'org-mode-hook 'toc-org-mode)
  (add-to-list 'org-tag-alist '("TOC" . ?T)))

;;; Jupyter
(when (>= emacs-major-version 26)
  (setq exec-path (append exec-path (list (expand-file-name my-jupyter-dir))))
  (when (functionp 'org-babel-jupyter-scratch-buffer)
	(setq my-org-babel-load-languages
		  (append my-org-babel-load-languages
				  '(
					;; (julia . t)  TODO needs upstream fix; is too old
					(jupyter . t))))))

;;; Emacs IPython Notebook
(setq ein:polymode t)

;;; form-feed (display  as horizontal line)
(when (functionp 'form-feed-mode)
  (setq form-feed-line-width 72)
  (add-hook 'Info-mode-hook 'form-feed-mode)
  (add-hook 'help-mode-hook 'form-feed-mode)
  (add-hook 'text-mode-hook 'form-feed-mode)
  (add-hook 'prog-mode-hook 'form-feed-mode))

;;; TeXfrag
;; TODO fix PreviewLaTeX in AuCTeX
;; (texfrag-global-mode)
;; (add-hook 'eww-mode-hook 'texfrag-mode)

;; Load Babel languages
(when (functionp 'org-babel-do-load-languages)
  (org-babel-do-load-languages 'org-babel-load-languages
							   my-org-babel-load-languages))


;;;; Programming mode-specific configuration

;;; JavaScript Mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("nodejs" . js2-mode))

;;; PHP mode
(autoload 'php-mode-hook "php-mode")
(add-hook 'php-mode-hook
		  (lambda ()
			(setq indent-tabs-mode t)
			(setq-local whitespace-line-column 120)))

;;; Web mode
(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

  ;; (add-hook 'web-mode-hook (check what file-name ends, then set whitespace-line-column and others accordingly))
  )


;;; Python mode
(add-hook 'python-mode-hook
		  (lambda ()
			(kill-local-variable 'tab-width)
			(kill-local-variable 'python-indent-offset)))


;;; Julia mode
;; (require 'julia-mode)
(autoload 'julia-mode-hook "julia-mode")
(setq julia-program my-julia-bin)
(add-hook 'julia-mode-hook
		  (lambda () (setq-local whitespace-line-column 92)))

;;; ESS
;; deactivate automatic loading of `ess-julia-mode'
(setq auto-mode-alist
	  (delete (rassoc 'ess-julia-mode auto-mode-alist) auto-mode-alist))
(setq inferior-julia-program my-julia-bin)
(setq inferior-julia-args "--color=yes")

;;; julia-repl
(add-hook 'julia-mode-hook
		  (lambda () (if (buffer-file-name) (julia-repl-mode))))
(setq julia-repl-executable-records
	  '((default "julia")
		(new "julian")))

;;; SLIME
(setq inferior-lisp-program "sbcl")


;;; Rust mode
;; (require 'rust-mode)
(autoload 'rust-mode-hook "rust-mode")
(add-hook 'rust-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			(setq-local whitespace-line-column 100)))
;; Run rustfmt on save
;; (setq rust-format-on-save t)

;;; GDScript mode
(autoload 'gdscript-mode-hook "gdscript-mode")
;; (setq gdscript-godot-executable my-godot-bin)
(add-hook 'gdscript-mode-hook
		  (lambda ()
			(setq-local whitespace-line-column 100)))


;;; ATS2 (Postiats)
(require 'ats-mode "ats2-mode" t)
(autoload 'flymake-ats2-load "flymake-ats2")
(add-hook 'ats-mode-hook 'flymake-ats2-load)
(add-hook 'c/ats-mode-hook 'flymake-ats2-load)


;;;; Language Server modes

;;; lsp-mode
;; LSP Prefix (C-c l)
(setq lsp-keymap-prefix "C-c l")

(when (and (or (eq my-lsp-package 'lsp-mode) (eq my-lsp-package 'all))
		   (require 'lsp-mode nil t))
  (unless (eq my-lsp-package 'all)
	(add-hook 'prog-mode-hook #'lsp))	; or #'lsp-deferred

  (setq lsp-before-save-edits nil)
  (setq lsp-completion-enable-additional-text-edit nil)
  (when (< emacs-major-version 27)
	(setq lsp-completion-styles '(basic)))


  (when (executable-find "rust-analyzer")
	(setq lsp-rust-server 'rust-analyzer)
	(autoload 'lsp-rust-analyzer-inlay-hints-mode "lsp-rust")
	(lsp-rust-analyzer-inlay-hints-mode 1))

  ;; Parallel jobs for the LSP and store index in file
  (setq lsp-clients-clangd-args '("-j=4" "-background-index"))

  ;; (setq lsp-pyls-plugins-pylint-enabled t) ; source also disables this
  ;; (setq lsp-pyls-plugins-pycodestyle-max-line-length 100)
  ;; (setq lsp-pyls-plugins-pycodestyle-max-line-length 100)
  ;; (setq lsp-pyls-plugins-flake8-max-line-length 100)
  (setq lsp-pyls-plugins-pydocstyle-enabled t)
  (setq lsp-pyls-plugins-rope-completion-enabled t)
  (setq lsp-pyls-plugins-yapf-enabled t)
  (setq lsp-pyls-plugins-flake8-enabled t)

  ;; lsp-julia
  (setq lsp-julia-default-environment my-julia-default-environment)
  ;; If we don't want to use the included Language Server:
  ;; (setq lsp-julia-package-dir nil)
  (when (require 'lsp-julia nil t)
	nil
	;; (add-hook 'julia-mode-hook #'lsp-mode)
	;; (add-hook 'julia-mode-hook #'lsp)
	)


  ;; TRAMP enabled pyls example
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "/path/to/pyls")
  ;; 					:major-modes '(python-mode)
  ;; 					:remote? t
  ;; 					:server-id 'pyls-remote))

  ;; LSP config using local variables
  ;; (add-hook 'hack-local-variables-hook
  ;; 			(lambda () (when (derived-mode-p 'XXX-mode) (lsp))))
  )


;;; Eglot
;; Use company-capf backend whenever `M-x eglot' connects
;; TODO Maybe redundant as this is always forced
;; (add-hook 'eglot-connect-hook
;; 		  (lambda ()
;; 			(setq-local company-backends
;; 						(cons 'company-capf
;; 							  (remove 'company-capf company-backends)))))
(when (and (or (eq my-lsp-package 'eglot) (eq my-lsp-package 'all))
		   (>= emacs-major-version 26)
		   (require 'eglot nil t))
  ;; Keybindings like for lsp-mode
  (define-prefix-command 'my-eglot-map)

  ;; Eglot Prefix (C-c l)
  (define-key eglot-mode-map (kbd "C-c l") 'my-eglot-map)

  ;; Session commands
  (define-prefix-command 'my-eglot-session-map)
  (define-key my-eglot-map (kbd "s") 'my-eglot-session-map)

  (define-key my-eglot-session-map (kbd "s") 'eglot)
  (define-key my-eglot-session-map (kbd "r") 'eglot-reconnect)
  (define-key my-eglot-session-map (kbd "q") 'eglot-shutdown)
  ;; These do not exist in lsp-mode
  (define-key my-eglot-session-map (kbd "b") 'eglot-events-buffer)
  (define-key my-eglot-session-map (kbd "e") 'eglot-stderr-buffer)
  (define-key my-eglot-session-map (kbd "c") 'eglot-signal-didChangeConfiguration)

  ;; Refactoring commands
  (define-prefix-command 'my-eglot-refactor-map)
  (define-key my-eglot-map (kbd "r") 'my-eglot-refactor-map)

  (define-key my-eglot-refactor-map (kbd "r") 'eglot-rename)

  ;; Formatting commands
  (define-prefix-command 'my-eglot-format-map)
  (define-key my-eglot-map (kbd "=") 'my-eglot-format-map)

  (define-key my-eglot-format-map (kbd "=") 'eglot-format)

  ;; Code action commands
  (define-prefix-command 'my-eglot-action-map)
  (define-key my-eglot-map (kbd "a") 'my-eglot-action-map)

  (define-key my-eglot-action-map (kbd "a") 'eglot-code-actions)

  ;; Help commands
  (define-prefix-command 'my-eglot-help-map)
  (define-key my-eglot-map (kbd "h") 'my-eglot-help-map)

  (define-key my-eglot-help-map (kbd "h") 'eglot-help-at-point)


  (when (executable-find "rls")
	;; (add-to-list 'eglot-server-programs `((rust-mode) eglot-rls ,my-rls-bin))
	(add-hook 'rust-mode-hook 'eglot-ensure))

  (when (executable-find "javascript-typescript-stdio")
	(add-hook 'js-mode-hook 'eglot-ensure)
	(add-hook 'typescript-mode-hook 'eglot-ensure))

  (when (executable-find "pyls")
	(add-hook 'python-mode-hook 'eglot-ensure))

  (when (executable-find "clangd")
	(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
	(add-hook 'c-mode-hook 'eglot-ensure)
	(add-hook 'c++-mode-hook 'eglot-ensure))

  (when (executable-find "digestif")
	(add-hook 'tex-mode-hook 'eglot-ensure))

  (add-to-list 'eglot-server-programs '((gd-script-mode) "localhost"
										6008 "tls")) ; or "starttls" or nil
  (add-hook 'gd-script-mode-hook 'eglot-ensure)

  (defun my-julia-get-project-root (dir)
	"Return the Julia project root directory of DIR."
	(expand-file-name (if dir
						  (or (locate-dominating-file
							   dir "JuliaProject.toml")
							  (locate-dominating-file dir "Project.toml")
							  my-julia-default-environment)
						my-julia-default-environment)))

  (defun my-julia-lsp-command (_arg)
	"Return a shell command to start the Julia language server."
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

  ;; TODO periodical errors
  ;; (when (executable-find "julia")
  ;; 	(add-to-list
  ;; 	 'eglot-server-programs
  ;; 	 '(julia-mode . my-julia-lsp-command))

  ;; 	(add-hook 'julia-mode-hook 'eglot-ensure)
  ;; 	;; Wait longer due to slow compilation
  ;; 	(add-hook 'julia-mode-hook
  ;; 			  (lambda () (setq-local eglot-connect-timeout 90))))

  ;; Example .dir-locals.el for per-project config (place in root folder)
  ;; ((python-mode
  ;; 	. ((eglot-workspace-configuration . (
  ;; 										 (:pyls . (:plugins (:pycodestyle (:maxLineLength 100))))
  ;; 										 (:pyls . (:plugins (:pydocstyle (:enabled t))))
  ;; 										 (:pyls . (:plugins (:pydocstyle (:maxLineLength 100))))
  ;; 										 ))
  ;; 	   (whitespace-line-column . 100))))

  ;; ... or maybe like this:
  ;; ((python-mode
  ;;   . ((eglot-workspace-configuration . ((:pyls . (
  ;; 												 (:plugins (:pycodestyle (:maxLineLength 100)))
  ;; 												 (:plugins (:pydocstyle (:enabled t)))
  ;; 												 (:plugins (:pydocstyle (:maxLineLength 100)))
  ;; 												 ))))
  ;; 	 (whitespace-line-column . 100))))
  )


;;; Load private configurations
(load (expand-file-name ".private_config.el" my-emacs-dir) t)
;; ---
;; ;; Calendar Location and Time
;; (setq calendar-latitude )
;; (setq calendar-longitude )
;; (setq calendar-location-name )
;;
;; ;; (setq calendar-time-zone )
;; ;; (setq calendar-standard-time-zone-name )
;; ;; (setq calendar-daylight-time-zone-name )
;;
;; ;; (setq calendar-daylight-time-offset )
;; ;; (setq calendar-daylight-savings-starts ))
;; ;; (setq calendar-daylight-savings-starts-time )
;; ;; (setq calendar-daylight-savings-ends ))
;; ;; (setq calendar-daylight-savings-ends-time )
;;
;; ;; Workday time
;; ;; (setq timeclock-workday )
;;
;; ;; Mail
;; (setq user-full-name )
;; (setq user-mail-address )
;; ;; (setq smtpmail-mail-address )
;; (setq smtpmail-smtp-user )
;; (setq smtpmail-smtp-server )
;; (setq smtpmail-smtp-service )
;;
;; ;; Tramp
;; ;; Passthrough proxies
;; ;; (see example in Tramp section)
;; ---


;;;; Custom commands

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (when (y-or-n-p "Kill all other buffers? ")
	(mapc 'kill-buffer (delq (current-buffer) (buffer-list)))))

(defun minibuffer-insert (obtain-text-function)
  "Insert text obtained by calling OBTAIN-TEXT-FUNCTION into the minibuffer.
OBTAIN-TEXT-FUNCTION is called with the result of calling function
`current-buffer' as its sole argument."
  (let ((text
		 (with-current-buffer (window-buffer (minibuffer-selected-window))
		   (funcall obtain-text-function (current-buffer)))))
	(when text
	  (insert text))))

(defun minibuffer-insert-abbreviated-buffer-file-name ()
  "Insert the current, abbreviated variable `buffer-file-name' into the minibuffer."
  (interactive)
  (minibuffer-insert
   (lambda (&rest _args) (abbreviate-file-name buffer-file-name))))

(defun minibuffer-insert-buffer-file-name ()
  "Insert the current variable `buffer-file-name' into the minibuffer."
  (interactive)
  (minibuffer-insert
   (lambda (&rest _args) buffer-file-name)))

(defun minibuffer-insert-abbreviated-default-directory ()
  "Insert the current, abbreviated `default-directory' into the minibuffer."
  (interactive)
  (minibuffer-insert
   (lambda (&rest _args) (abbreviate-file-name default-directory))))

(defun minibuffer-insert-default-directory ()
  "Insert the current `default-directory' into the minibuffer."
  (interactive)
  (minibuffer-insert
   (lambda (&rest _args) default-directory)))

(defun quit-other-window ()
  "Quit the other (next) window while staying in the selected window."
  (interactive)
  (select-window (next-window))
  (quit-window nil)
  (select-window (previous-window)))


(defun insert-arbitrary-pair (beginning ending)
  "Insert a pair of any two strings.
The strings BEGINNING and ENDING are respectively inserted before
and after point or the region if active."
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
  "Insert the given CHAR before and after point or the region if active."
  (interactive "cSurrounding character: ")
  (insert-arbitrary-pair char char))

(defun insert-same-pair (text)
  "Insert the given TEXT before and after point or the region if active."
  (interactive "sSurrounding text: ")
  (insert-arbitrary-pair text text))

(defun insert-differing-pair (beginning ending)
  "Insert a pair of any two strings.
The strings BEGINNING and ENDING are respectively inserted before
and after point or the region if active."
  (interactive "sBeginning text: \nsEnding text: ")
  (insert-arbitrary-pair beginning ending))

(defun insert-reversed-pair (text)
  "Insert a pair of TEXT, where the insertion at the end is reversed.
TEXT is inserted before and the reversed version after point or
 the region if active.
TEXT is reversed literally ('[a' -> 'a[')."
  (interactive "sUnreversed beginning: ")
  (insert-arbitrary-pair text (reverse text)))

(defun insert-tag-pair (tag)
  "Insert the given HTML TAG before and after point or the region if active."
  (interactive "sTag: ")
  (insert-arbitrary-pair (concat "<" tag ">") (concat "</" tag ">")))

(defun delete-around (count)
  "Delete COUNT characters before and after point or the region if active."
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
	  (setq frame-background-mode 'dark)
	  (update-frame-background-mode)
	  (safe-load-theme dark-theme my-fallback-dark-theme))
	(update-emms-faces)))

(defun toggle-theme-brightness-or-background ()
  "Toggle either the theme or background brightness.
The choice depends on whether a user-configured theme variant is
currently active."
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
  "Toggle `indent-tabs-mode' and re-tabify."
  (interactive)
  (if (eq indent-tabs-mode nil)
	  (progn
		(setq indent-tabs-mode t)
		(tabify (point-min) (point-max)))
	(setq indent-tabs-mode nil)
	(untabify (point-min) (point-max))))

(defun toggle-font-lock-mode ()
  "Toggle Font-Lock mode."
  (interactive)
  (if (eq global-font-lock-mode nil)
	  (global-font-lock-mode 1)
	(global-font-lock-mode 0)))

(defun toggle-subword-mode ()
  "Toggle Subword mode."
  (interactive)
  (if (eq global-subword-mode nil)
	  (global-subword-mode 1)
	(global-subword-mode 0)))

(if (functionp 'pixel-scroll-mode)
	(defun toggle-pixel-scroll-mode ()
	  "Toggle Pixel-Scroll mode'."
	  (interactive)
	  (if (eq pixel-scroll-mode nil)
		  (pixel-scroll-mode 1)
		(pixel-scroll-mode 0)))
  (defun toggle-pixel-scroll-mode ()
	"No op."
	(interactive)
	()))

(defun toggle-flyspell-mode ()
  "Toggle Flyspell mode."
  (interactive)
  (if (eq flyspell-mode nil)
	  (flyspell-mode 1)
	(flyspell-mode 0)))

(defun toggle-ede-mode ()
  "Toggle EDE mode."
  (interactive)
  (if (eq global-ede-mode nil)
	  (global-ede-mode 1)
	(global-ede-mode 0)))

(if (functionp 'global-num3-mode)
	(defun toggle-global-num3-mode ()
	  "Toggle global Num3 mode."
	  (interactive)
	  (if (eq global-num3-mode nil)
		  (global-num3-mode 1)
		(global-num3-mode 0))))

(when (functionp 'pdf-tools-install)
  ;; TODO turn this into minor mode
  (defun toggle-presentation-mode ()
	"Toggle hiding any visual distractions."
	(interactive)
	(if (eq mode-line-format nil)
		(progn
		  (kill-local-variable 'mode-line-format)
		  (kill-local-variable 'display-line-numbers)
		  (toggle-frame-fullscreen)
		  (when (eq major-mode 'pdf-view-mode)
			(local-unset-key (kbd "<mouse-1>"))
			(local-unset-key (kbd "<mouse-3>"))
			(local-unset-key [down-mouse-1])
			(local-unset-key [down-mouse-3])
			(pdf-misc-context-menu-minor-mode 1)
			(pdf-view-fit-page-to-window))
		  (kill-local-variable 'echo-keystrokes)
		  (kill-local-variable 'inhibit-message)
		  ;; TODO why does `winner-undo' not work here?
		  )
	  (winner-save-unconditionally)
	  (setq-local mode-line-format nil)
	  (setq-local display-line-numbers nil)
	  (delete-other-windows)
	  (toggle-frame-fullscreen)
	  (when (eq major-mode 'pdf-view-mode)
		(local-set-key (kbd "<mouse-1>") 'pdf-view-next-page-command)
		(local-set-key (kbd "<mouse-3>") 'pdf-view-previous-page-command)
		(local-unset-key [down-mouse-1])
		(local-unset-key [down-mouse-3])
		(pdf-misc-context-menu-minor-mode 0)
		(sleep-for 0 200) ; sadly necessary
		(pdf-view-fit-page-to-window))
	  (setq-local echo-keystrokes 0) ; debatable
	  (setq-local inhibit-message t))))


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


;;;; Key bindings

;; Do not untabify before backspacing
(global-set-key (kbd "DEL") 'backward-delete-char)

;; Find file at point (C-c f)
(define-key mode-specific-map (kbd "f") 'find-file-at-point)

;; Repeat complex command (C-c z)
(define-key mode-specific-map (kbd "z") 'repeat-complex-command)

;; undo-only (C-c u)
(define-key mode-specific-map (kbd "u") 'undo-only)

;; Better expanding
(global-set-key (kbd "M-/") 'hippie-expand)

;; Better completion mapping (C-c n)
(unless (functionp 'company-complete)
  (define-key mode-specific-map (kbd "n") 'completion-at-point))

;; Like dt or df in Vim
(global-set-key (kbd "M-z") 'zap-up-to-char)
;; Zap to char (C-c x z)
(define-key mode-specific-map (kbd "z") 'zap-to-char)

;; Swap literal and regex isearch
;; (we then don't need (search-default-mode t))
(unless (functionp 'swiper)
  (global-set-key (kbd "C-s") 'isearch-forward-regexp))
(global-set-key (kbd "C-r")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Swap literal and regex query-replace
(global-set-key (kbd "M-%")   'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Indent using tabs or spaces (C-c t i)
(define-key my-toggle-map (kbd "i") 'toggle-indent-tabs-mode)

;; Toggle showing whitespace (C-c t w)
(define-key my-toggle-map (kbd "w") 'toggle-show-whitespace)

;; Toggle font-lock (C-c t c)
(define-key my-toggle-map (kbd "c") 'toggle-font-lock-mode)

;; Toggle subword movement (C-c t W)
(define-key my-toggle-map (kbd "W") 'toggle-subword-mode)

;; Toggle pixel-scrolling (C-c t p)
(define-key my-toggle-map (kbd "p") 'toggle-pixel-scroll-mode)

;; Toggle auto fill mode (C-c t F)
(define-key my-toggle-map (kbd "F") 'auto-fill-mode)

;; Toggle truncation of long lines (C-c t L)
(define-key my-toggle-map (kbd "L") 'toggle-truncate-lines)

;; Toggle Flyspell mode (C-c t s)
(define-key my-toggle-map (kbd "s") 'toggle-flyspell-mode)

;; Toggle EDE mode (C-c t e)
(define-key my-toggle-map (kbd "e") 'toggle-ede-mode)

;; Toggle Num3 mode (C-c t N)
(define-key my-toggle-map (kbd "N") 'toggle-global-num3-mode)

;; Compile (C-c c)
(define-key mode-specific-map (kbd "c") 'compile)


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
(unless (functionp 'counsel-semantic-or-imenu)
  (define-key mode-specific-map (kbd "j") 'imenu))
;; ibuffer is better standalone
(define-key mode-specific-map (kbd "b") 'ibuffer)

;; Remap `transpose-sexps' to M-S-t to avoid the Terminal shortcut.
(global-set-key (kbd "M-T") 'transpose-sexps)
;; Remap `kill-whole-line' to M-S-k to avoid Terminal misinterpretation.
(global-set-key (kbd "M-K") 'kill-whole-line)


;; Revert buffer (C-c x r)
(define-key my-extended-map (kbd "r") 'revert-buffer)

;; Rename uniquely (C-c x R)
(define-key my-extended-map (kbd "R") 'rename-uniquely)

;; Diff buffers (preferably in split) (C-c x d)
(define-key my-extended-map (kbd "d") 'ediff-buffers)

;; Find file with find (C-c x f)
(define-key my-extended-map (kbd "f") 'find-name-dired)

;; Insert the abbreviated `buffer-file-name' into the minibuffer (C-c x F)
(define-key my-extended-map (kbd "F")
  'minibuffer-insert-abbreviated-buffer-file-name)

;; Insert `buffer-file-name' into the minibuffer (C-c x P)
(define-key my-extended-map (kbd "P") 'minibuffer-insert-buffer-file-name)

;; Insert the abbreviated `default-directory' into the minibuffer (C-c x D)
(define-key my-extended-map (kbd "D")
  'minibuffer-insert-abbreviated-default-directory)

;; Insert `default-directory' into the minibuffer (C-c x L)
(define-key my-extended-map (kbd "L") 'minibuffer-insert-default-directory)

;; Describe char (C-c x c)
(define-key my-extended-map (kbd "c") 'describe-char)

;; Kill other buffers (C-c x k)
(define-key my-extended-map (kbd "k") 'kill-other-buffers)

;; Open Proced (C-c x p)
(define-key my-extended-map (kbd "p") 'proced)

;; Enter shell (C-c x S)
(define-key my-extended-map (kbd "S") 'shell)

;; Enter eshell (C-c x s)
(define-key my-extended-map (kbd "s") 'eshell)

;; Grep (C-c x G)
(define-key my-extended-map (kbd "G") 'grep)

;; Enter EWW (C-c x w)
(define-key my-extended-map (kbd "w") 'eww)

;; Open file with EWW (C-c x o)
(define-key my-extended-map (kbd "o") 'eww-open-file)

(when (functionp 'xwidget-webkit-browse-url)
  ;; Enter xwidget webkit (C-c x x)
  (define-key my-extended-map (kbd "x") 'xwidget-webkit-browse-url)

  ;; Open file with xwidget webkit (C-c x l)
  (define-key my-extended-map (kbd "l") 'xwidget-webkit-open-file))

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
(put 'dired-find-alternate-file 'disabled nil)

;;; init.el ends here
