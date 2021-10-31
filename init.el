;;; init.el --- Personal Emacs configuration  -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Build with:
;;    ./autogen.sh
;;    TODO make native-comp-compiler-options usage portable
;;    ./configure CFLAGS='-O2 -march=native -pipe' \
;;                --with-modules [--with-json] [--with-native-compilation] \
;;                [--with-rsvg] [--with-xwidgets] \
;;                [--with-x-toolkit=lucid] [--prefix=...]
;;    [sed -i -e "s/^BYTE_COMPILE_EXTRA_FLAGS =/BYTE_COMPILE_EXTRA_FLAGS = --eval '(setq native-comp-compiler-options (list \"-O2\" \"-march=$(gcc -Q -march=native --help=target | sed -n 's/^\s*-march=\s*\(.*\)$/\1/p')\"))'/" lisp/Makefile]
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

(defmacro checked-setq (&rest args)
  "Check the symbols in ARGS and return the corresponding `setq' form.

The symbols are checked for being buffer-local or having
`defcustom' options that require the symbol to be set via
`custom-set-variables'. As the required changes are only warned
about, the `checked-setq' statements have to be manually
changed."
  (let ((retval (apply #'list 'setq args)))
	(while args
	  (let ((sym (car args))
			(val (cadr args)))
		(when (local-variable-if-set-p sym)
		  (message "warning! variable `%s' is buffer-local!" sym))
		(let ((csym (indirect-variable sym)))
		  (when (get csym 'custom-set)
			  (message "warning! variable `%s' has `:set` property'!" sym))
		  (when (get csym 'custom-requests)
		    (message "warning! variable `%s' has `:require` property'!" sym))
		  )
	  (setq args (cddr args))))
	retval))

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
(defconst my-docsets-path "~/.local/share/Zeal/Zeal/docsets")
(defconst my-julia-bin "~/local/bin/julia")
(defconst my-julia-default-environment "~/.julia/environments/v1.3")
(defconst my-jupyter-dir "~/anaconda3/bin")
;; (defconst my-godot-bin "~/local/bin/godot")

(defconst my-lsp-package 'eglot "Which LSP package to use.
Can be a symbol in (lsp-mode eglot all). If it is \"all\", only activate the
hooks for `my-autostart-lsp-package'.")
(defconst my-autostart-lsp-package 'eglot
  "Which LSP package to autostart if `my-lsp-package' is \"all\".")
;; (defconst my-rls-bin "~/.cargo/bin/rls")

(defconst my-line-number-format 'relative)
(defconst my-music-dir "~/Music/")
(defconst my-alarms-path (expand-file-name "my-alarms.el" my-emacs-dir))
(defconst my-alarm-ring-path nil)

(defconst my-start-time (current-time))

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
(when (or (< emacs-major-version 27) (and (fboundp 'native-comp-available-p)
                                          (native-comp-available-p)))
    (package-initialize))

;; Add package lists
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
			 '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

;; Customized variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(add-log-mailing-address "janpublicebert@posteo.net")
 '(auth-source-save-behavior nil)
 '(auto-insert t)
 '(auto-insert-mode t)
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
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(completion-cycle-threshold 6)
 '(current-language-environment "UTF-8")
 '(delete-old-versions t)
 '(delete-trailing-lines nil)
 '(desktop-restore-eager 10)
 '(dired-always-read-filesystem t)
 '(dired-dwim-target 'dired-dwim-target-recent)
 '(display-battery-mode nil)
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
   '(("gnu" . 9)
	 ("nongnu" . 7)
	 ("melpa" . 5)
	 ("melpa-stable" . 3)))
 '(package-menu-hide-low-priority t)
 '(package-quickstart t)
 '(package-selected-packages
   '(rmsbolt debbugs markdown-toc org-gcal vlf counsel-dash dash-docs htmlize extempore-mode org lsp-mode project so-long xref undohist browse-at-remote mines magit julia-repl counsel swiper projectile rust-mode slime jsonrpc d-mode cider gdscript-mode disk-usage dart-mode gnuplot web-mode ada-ref-man docker dockerfile-mode dired-du dired-git-info purescript-mode js2-mode markdown-mode typescript-mode realgud dap-mode cobol-mode csharp-mode fsharp-mode go-mode num3-mode php-mode sed-mode smalltalk-mode stan-mode swift-mode zig-mode elixir-mode erlang clojure-mode cmake-mode haskell-snippets caml sml-mode haskell-mode lsp-julia nasm-mode yaml-mode ada-mode chess csv-mode json-mode vterm lua-mode python nov ein yasnippet-snippets texfrag eglot undo-propose ess form-feed nim-mode evil-collection evil-commentary evil-lion evil-magit evil-matchit evil-snipe evil-surround evil-visualstar landmark auctex zotxt company-quickhelp dumb-jump expand-region jupyter use-package gotham-theme zenburn-theme toc-org flymake tramp ivy ggtags pdf-tools yasnippet solarized-theme rainbow-delimiters julia-mode helm gnu-elpa-keyring-update forge evil emms darkroom company))
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
 '(rmail-movemail-flags '("--tls"))
 '(save-interprogram-paste-before-kill t)
 '(save-place-mode t)
 '(savehist-additional-variables
   '(tablist-named-filter command-history search-ring regexp-search-ring kill-ring extended-command-history compile-command))
 '(savehist-mode t)
 '(scroll-bar-mode 'right)
 '(semantic-mode t)
 '(send-mail-function 'smtpmail-send-it)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(shell-command-prompt-show-cwd t)
 '(shell-prompt-pattern "^\\(?:###\\)?[^#$%>
]*[#$%>] *")
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis nil nil "Originally parenthesis. Maybe try out expression or mixed.")
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tab-bar-history-mode t)
 '(tab-bar-show 1)
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
 '(view-read-only t)
 '(wdired-allow-to-change-permissions t)
 '(which-function-mode t)
 '(whitespace-style '(face trailing lines-tail empty tab-mark))
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

(defun disable-line-numbers ()
  "Disable display of line numbers."
  (setq-local display-line-numbers nil))


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


;; Fix `read-passwd' being recorded in lossage.
(defun execute-without-recording (orig-fun &rest args)
  "Execute ORIG-FUN on ARGS without recording input characters."
  ;; Don't allow kills.
  ;; For more low-level control, we could copy the kill ring,
  ;; and afterwards clear-string all its contents and restore it.
  (advice-add 'kill-new :override #'ignore
			  '((name . "disable-recording")))
  (unwind-protect
	  ;; Would be even better to set buffer-locally but whatever.
	  (let ((inhibit--record-char t))
		(apply orig-fun args))
	(advice-remove 'kill-new "disable-recording")))

(advice-add 'read-passwd :around
			#'execute-without-recording)

;; Use more flexible completion styles
(setq completion-styles (append completion-styles
								'(substring initials flex)))

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
;; (setq-default display-line-numbers my-line-number-format)

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

;; Deactive subword mode for transposing words.
(defun dont-transpose-subwords (orig-fun &rest args)
  "Disable function `subword-mode' when transposing words.
Advice around ORIG-FUN, called with ARGS."
  (if (eq subword-mode nil)
	  (apply orig-fun args)
	(subword-mode 0)
	(apply orig-fun args)
	(subword-mode 1)))

(advice-add 'transpose-words :around #'dont-transpose-subwords)

;; Autoclose blocks in LaTeX mode
(add-hook 'latex-mode-hook 'latex-electric-env-pair-mode)
;; Auto-fill in TeX mode
(add-hook 'tex-mode-hook 'auto-fill-mode)

;; Disable whitespace and string face in Eshell, comint, Shell, and Term mode
(dolist (mode-hook
		 '(eshell-mode-hook comint-mode-hook shell-mode-hook term-mode-hook))
  (add-hook mode-hook (lambda ()
						(disable-line-numbers)
						(dont-show-whitespace)
						(disable-string-face))))


;;;; Built-in packages

;;; Xref
(with-eval-after-load "xref"
  ;; Do not prompt for `xref-find-references'
  (push 'xref-find-references (cdr xref-prompt-for-identifier)))

;;; Image-Mode
;; Horizontal scrolling does not work with line numbers enabled
(add-hook 'image-mode-new-window-functions
		  (lambda (&optional _winprops) (disable-line-numbers)))

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

;;; Term

(defun term-watch-for-password-prompt--always (string &rest _args)
  "Prompt in the minibuffer for password and send without echoing.
Checks if STRING contains a password prompt as defined by
`comint-password-prompt-regexp'."
  (when (let ((case-fold-search t))
		  (string-match comint-password-prompt-regexp string))
	(let ((password (read-passwd string)))
	  (term-send-invisible password)
	  (clear-string password))))

(advice-add 'term-watch-for-password-prompt :override
			#'term-watch-for-password-prompt--always)

(defun term-ssh (program host)
  "Start `term' and instantly SSH to HOST.
PROGRAM is the terminal program to start."
  (term program)
  (term-send-string (get-buffer-process (current-buffer))
					(concat "ssh " host "\n")))

(defun term-tramp (program)
  "Optionally SSH to the current remote when starting `term'.
PROGRAM is the terminal program to start."
  (interactive
   (list (read-from-minibuffer
		  (concat "Run "
				  (and (file-remote-p default-directory) "remote ")
				  "program: ")
		  (or explicit-shell-file-name
			  (getenv "ESHELL")
			  shell-file-name))))
  (let ((host (file-remote-p default-directory 'host))
		(user (file-remote-p default-directory 'user)))
	(if host
		(let ((full-host (concat (when user (concat user "@")) host)))
		  (term-ssh program full-host)
		  (term-send-string
		   (get-buffer-process (current-buffer))
		   (concat "cd "
				   (file-remote-p default-directory 'localname)
				   "\n")))
	  (term program))))

(with-eval-after-load "term"
  ;; We'd rather keep `ctrl-x-map' than be able to send raw C-x.
  (define-key term-raw-map (kbd "C-x") nil)
  ;; We'd rather keep `help-map' than be able to send raw C-h.
  (define-key term-raw-map (kbd "C-h") nil)
  ;; We'd rather keep `execute-extended-command' than be able to send raw M-x.
  (define-key term-raw-map (kbd "M-x") nil))

;;; Dired
(defun dired-relist-human-readable ()
  "Relist the dired buffer with the human-readable switch appended."
  (interactive)
  (dired default-directory (concat dired-actual-switches "h")))

(defun dired-open-externally ()
  "Open marked files with the OS default."
  (interactive)
  (let* ((files (dired-get-marked-files))
		 (files (mapc
				 (lambda (file) (replace-regexp-in-string "'" "'\"'\"'" file))
				 files)))
	(dolist (file files)
	  ;; FIXME use different command for different OS, make customizable
	  (shell-command (format "xdg-open '%s'" file)))))

(defun dired-browse-externally ()
  "Browse current directory with the OS default."
  (interactive)
  (let* ((dir (dired-current-directory))
		 (dir (replace-regexp-in-string "'" "'\"'\"'" dir)))
	;; FIXME use different browser for different OS, make customizable
	(shell-command (format "nautilus '%s'" dir))))

(add-hook 'dired-after-readin-hook 'dont-show-whitespace)
(add-hook 'dired-load-hook
		  (lambda ()
			(require 'dired-x)
			;; Set human readability (C-c y h)
			(define-key dired-mode-map (kbd "C-c y h")
			  'dired-relist-human-readable)
			;; Open externally (C-c y f)
			(define-key dired-mode-map (kbd "C-c y f")
			  'dired-open-externally)
			;; Open externally (C-c y d)
			(define-key dired-mode-map (kbd "C-c y d")
			  'dired-browse-externally)))

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
  (setq erc-default-server "irc.libera.chat")
  (when (gnutls-available-p)
	;; Use TLS by default.
	(setq erc-default-port erc-default-port-tls)
	(setq erc-server-connect-function #'erc-open-tls-stream)))

;;; Rcirc
;; libera.chat default port is 6667; 6697 for TLS connections
(with-eval-after-load "rcirc"
  (if (gnutls-available-p)
	  (add-to-list 'rcirc-server-alist
				   '("irc.libera.chat"
					 :port 6697 :encryption tls))
	(add-to-list 'rcirc-server-alist
				 '("irc.libera.chat"	; :channels ("#rcirc")
				   ;; Don't use the TLS port by default, in case
				   ;; gnutls is not available.
				   :port 6667))))

;;; EWW
;; TODO instead of this, try to request the generated website with a slightly
;; TODO smaller screen size
(add-hook 'eww-mode-hook
		  (lambda ()
			(disable-line-numbers)
			(dont-show-whitespace)))

;;; xwidgets webkit
(when (and (featurep 'xwidget-internal)
		   (functionp 'xwidget-webkit-browse-url))
  (add-hook 'xwidget-webkit-mode-hook
			(lambda ()
			  (disable-line-numbers)
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
	  (message "currently only allowing local files; \
disable `allow-files-only' advice on `xwidget-webkit-goto-uri'")
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
	(interactive "fxwidget-webkit file: ")
	(xwidget-webkit-browse-url
	 (concat "file://"
			 (and (memq system-type '(windows-nt ms-dos)) "/")
			 (expand-file-name file))
	 new-session)))

;;; Lynx wrapper
(defun lynx-browse-url (url)
  "Browse URL using the Lynx browser."
  (interactive
   (let* ((uris (eww-suggested-uris))
		  (prompt (concat "lynx URL or keywords"
						  (if uris (format " (default %s)" (car uris)) "")
						  ": ")))
	 (list (read-string prompt nil 'eww-prompt-history uris))))
  (if (not (executable-find "lynx"))
	  (user-error "The lynx executable could not be found")
	(set-buffer (make-term "lynx" "lynx" nil url))
	(term-mode)
	(term-char-mode)
	(switch-to-buffer "*lynx*")))

(defun lynx-open-file (file)
  "Open FILE using the Lynx browser."
  (interactive "flynx file: ")
  (lynx-browse-url
   (concat "file://"
		   (and (memq system-type '(windows-nt ms-dos))
				"/")
		   (expand-file-name file))))

;; Avoid performance issues in files with very long lines.
(when (functionp 'global-so-long-mode)
  (global-so-long-mode 1))

;;; Ediff
(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(setq ediff-split-window-function #'split-window-horizontally)

;; Use EDE everywhere
;; (global-ede-mode 1)					 ; conflicts with org-mode binding

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
  (setq vc-ignore-dir-regexp
		(format "\\(%s\\)\\|\\(%s\\)"
				vc-ignore-dir-regexp
				tramp-file-name-regexp))
  ;; or
  ;; (setq vc-handled-backends '(Git))

  ;; TODO Not sure if sftp works as default method
  (let ((method (seq-find (lambda (x) (executable-find x)) '("rsync" "sftp"))))
	(when method
	  (setq tramp-default-method method)))

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
  (when (and (not (package-installed-p 'docker)) (eq system-type 'gnu/linux))
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
				   (lambda (dockerline) (string-match ":$" dockerline))
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
	;; 						   (lambda (dockerline) (string-match ":$" dockerline))
	;; 						   (split-string dockernames-raw "\n"))))
	;; 		(setq ad-return-value dockernames))
	;; 	ad-do-it))
  )

;;; Flymake
(when (and (>= emacs-major-version 26)
		   (require 'flymake nil t))
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (add-hook 'prog-mode-hook 'flymake-mode)
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
	   (;; these are the defaults (but with changed bindings)
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

		;; TODO these do not work; we manually bind them later
		;; org-backward-paragraph M-p
		;; ([(control up)] . [(meta p)])
		;; org-forward-paragraph M-n
		;; ([(control down)] . [(meta n)])

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

;; Pretty symbols
(setq org-pretty-entities t)
;; No subscript/superscript formatting
(setq org-pretty-entities-include-sub-superscripts nil)

;; Export settings

(setq org-export-with-smart-quotes t)
;; Use HTML5
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)

;;; Org Babel

(setq org-confirm-babel-evaluate nil)
;; Lexical binding by default
(with-eval-after-load 'ob-emacs-lisp
  (setcdr (assq :lexical org-babel-default-header-args:emacs-lisp) "yes"))
(setq org-src-preserve-indentation t)
;; (setq org-edit-src-content-indentation 0)

;; Open source code over current window.
(setq org-src-window-setup 'current-window)
;; Correctly show whitespace
(add-hook 'org-src-mode-hook 'show-whitespace)

;; If we ever use ob-async...
;; (setq ob-async-no-async-languages-alist
;; 	  '("jupyter-python" "jupyter-julia"))

(defvar my-org-babel-load-languages '())

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
	(if (or (not (boundp 'org-beamer-mode)) (eq org-beamer-mode nil))
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
  (define-key my-org-map (kbd "g") 'org-mark-ring-goto)

  ;; Non-disputed keybindings
  (with-eval-after-load 'org
	(define-key org-mode-map (kbd "M-p") 'org-backward-paragraph)
	(define-key org-mode-map (kbd "M-n") 'org-forward-paragraph))

  ;; Use C-c C-b for navigation like always
  (with-eval-after-load 'ox-beamer
			(define-key org-beamer-mode-map (kbd "C-c C-b") nil))

  ;; Activate beamer mode or select beamer environment (C-c o b)
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

  ;; Include babel or polyglossia for better LaTeX language settings.
  (add-to-list 'org-latex-packages-alist
			   '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist
			   '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

  ;; TODO automatically select dictionary depending on org language setting
  ;; TODO automatically translate beamer toc frame/slide

  (with-eval-after-load 'ox-latex
	;; Fix German babel language
	(setcdr (assoc "de" org-latex-babel-language-alist) "ngerman")

	;; Enable LaTeX letter class
	(add-to-list 'org-latex-classes
				 '("letter" "\\documentclass[11pt]{letter}"
				   ("\\opening{%s}")
				   ("\\closing{%s}")
				   ("\\ps{}\n%s")
				   ("\\cc{%s}")
				   ("\\encl{%s}"))))

  ;; TODO implement latex-derived letter backend
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
		   (cdr (seq-find (lambda (elem)
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

  (defun my-org-plain-text-filter-thin-no-break-space (text backend info)
	"Ensure narrow no-break spaces (\" \") are properly handled in Org export.
TEXT is the text to be exported, BACKEND is the export backend
and INFO the export communication channel."
	(my-org-plain-text-filter " " '((latex . "\\,")
									(html . "&#8239;"))
							  text backend info t t))

  (defun my-org-plain-text-filter-cpp (text backend info)
	"Ensure the word 'C++' looks good.
TEXT is the text to be exported, BACKEND is the export backend
and INFO the export communication channel."
	(my-org-plain-text-filter "C\\+\\+" '((latex . "C\\texttt{++}"))
							  text backend info t t))

  (defun my-org-plain-text-filter-big-o (text backend info)
	"Ensure 𝒪 is properly exported.
TEXT is the text to be exported, BACKEND is the export backend
and INFO the export communication channel."
	(my-org-plain-text-filter "𝒪" '((latex . "\\mathcal{O}"))
							  text backend info t t))

  (with-eval-after-load 'ox
	;; Additional export backends
	(require 'ox-md)

	(setq org-export-filter-plain-text-functions
		  (append org-export-filter-plain-text-functions
				  (list #'my-org-plain-text-filter-no-break-space
						#'my-org-plain-text-filter-zero-width-space
						#'my-org-plain-text-filter-word-joiner
						#'my-org-plain-text-filter-thin-no-break-space
						#'my-org-plain-text-filter-cpp
						#'my-org-plain-text-filter-big-o))))


  ;; TODO only load languages when they're in path; don't load shell on windows
  (setq my-org-babel-load-languages
		(append my-org-babel-load-languages
				'(
				  ;; Emacs
				  (emacs-lisp . t)
				  (calc . t)
				  (org . t)

				  ;; Unix
				  (shell . t)
				  (awk . t)
				  (sed . t)

				  ;; Scripting
				  (python . t)
				  (lua . t)
				  (lisp . t)

				  ;; Common languages
				  (C . t)
				  (makefile . t)
				  (js . t)

				  ;; Database languages
				  (sql . t)
				  (sqlite . t)

				  ;; Misc
				  (gnuplot . t)
				  (latex . t)
				  ))))


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
  (mapc #'frame-set-background-mode (frame-list)))

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
(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "M-p") 'backward-paragraph)
  (define-key compilation-mode-map (kbd "M-n") 'forward-paragraph))


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

;;; HTML
(setcdr (assoc 'html-mode auto-insert-alist)
		'(lambda ()
		   (insert (concat "<!DOCTYPE html>\n"
				   "<html lang=\"en\">\n"
				   "<head>\n"
				   "<meta charset=\"utf-8\">\n"
				   "<meta name=\"viewport\" "
				   "content=\"width=device-width, initial-scale=1\">\n"
				   "<meta http-equiv=\"Content-Security-Policy\" "
				   "content=\"default-src 'none'; font-src 'self'; "
				   "img-src 'self'; object-src 'none'; "
				   "script-src 'self'; style-src 'self'\">\n"
				   "<title>" (setq str (read-string "Title: ")) "</title>\n"
				   "<link href=\"css/css.css\" rel=\"stylesheet\">\n"
				   "<script src=\"js/js.js\" defer></script>\n"
				   "</head>\n"
				   "<body>\n<h1>" str "</h1>\n"
				   "\n<address>\n<a href=\"mailto:"
				   user-mail-address
				   "\">" (user-full-name) "</a>\n</address>\n"
				   "</body>\n"
				   "</html>"))
		   (indent-region (point-min) (point-max))))

;;; Indentation
;; HTML indentation
(setq sgml-basic-offset 4)

;; Built-in modes
(add-hook 'sh-mode-hook
		  (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'picture-mode-hook
		  (lambda () (setq-local indent-tabs-mode nil)))


;;;; Package config

;;; GNU Global
(when (file-exists-p my-gtags-dir)
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
									hippie-expand-try-functions-list)))))))

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
(when (functionp 'global-num3-mode)
  (global-num3-mode)
  ;; (setq num3-threshold 4)
  ;; TODO set num3-face-even to '((t :underline t :weight bold ; :slant italic
  ;;))
  (face-spec-set 'num3-face-even '((t :underline t
									  :weight bold
									  ;; :slant italic
									  ))
				 'face-defface-spec))

;;; Dumb Jump
(when (functionp 'dumb-jump-xref-activate)
	(add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;; Magit
;; Do not set global bindings
;; (setq magit-define-global-key-bindings nil)

(when (and (>= emacs-major-version 26)
		   (require 'magit nil t))
  ;; Always wants to save all files whenever we save anything in the repo
  ;; (magit-wip-mode 1)

  ;; Magit keybindings (C-c g)
  (define-prefix-command 'my-magit-map)
  (define-key mode-specific-map (kbd "g") 'my-magit-map)
  (define-key my-magit-map (kbd "g") 'magit-status)
  (define-key my-magit-map (kbd "G") 'magit-dispatch)
  (define-key my-magit-map (kbd "M-g") 'magit-dispatch)
  (define-key my-magit-map (kbd "f") 'magit-file-dispatch)
  (define-key my-magit-map (kbd "M-G") 'magit-file-dispatch)

  ;; Setup fill column
  (add-hook 'git-commit-setup-hook
			(lambda ()
			  (setq-local fill-column 72)
			  (setq-local whitespace-line-column 72))))

;;; Slurm
(require 'slurm-mode nil t)
(when (require 'slurm-script-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.sbatch\\'" . slurm-script-mode)))

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
				#'emms-source-file-directory-tree-find))

	  (defun update-emms-faces ()
		"Change EMMS faces to be consistent with the rest of Emacs."
		(setq local-default-foreground (face-attribute 'default :foreground))
		(setq local-default-background (face-attribute 'default :background))
		(set-face-attribute 'emms-playlist-track-face nil
							:foreground local-default-foreground)
		(set-face-attribute 'emms-playlist-selected-face nil
							:background local-default-foreground
							:foreground local-default-background))

	  (defun emms-playlist-select-random--check-empty ()
		"Select a random track in the current buffer.
If there are no tracks, do not select anything."
		;; Same function as in EMMS but with empty check.
		(emms-playlist-ensure-playlist-buffer)
		(save-excursion
		  (let ((track-indices nil))
			(goto-char (point-min))
			(emms-walk-tracks
			  (setq track-indices (cons (point)
										track-indices)))
			(setq track-indices (vconcat track-indices))
			(unless (seq-empty-p track-indices)
			  (emms-playlist-select (aref track-indices
										  (random (length track-indices))))))))
	  (advice-add 'emms-playlist-select-random :override
				  #'emms-playlist-select-random--check-empty)

	  (defun emms-start--check-empty ()
		"Start playing the current track in the EMMS playlist if there is one."
		(interactive)
		(unless (or emms-player-playing-p
					(condition-case nil (emms-playlist-next)
					  (error t)))
		  (emms-player-start (emms-playlist-current-selected-track))))
	  (advice-add 'emms-start :override
				  #'emms-start--check-empty)

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
	  (define-key my-music-map (kbd "S") 'emms-streams)
	  (define-key my-music-map (kbd "R") 'emms-play-streamlist)
	  (define-key my-music-map (kbd "U") 'emms-play-url)
	  (define-key my-music-map (kbd "+") 'emms-volume-raise)
	  (define-key my-music-map (kbd "-") 'emms-volume-lower)

	  ;; Add some more radio stations
	  (defconst my-emms-streams
		'((*track* (type . url)
				   (name . "http://stream.techno.fm/radio1-320k.mp3")
				   (metadata
					"TechnoFM: Radio-1 320K MP3"
					"http://stream.techno.fm/radio1-320k.mp3"
					1 url))
		  (*track* (type . url)
				   (name . "http://stream.techno.fm/radio1-192k.mp3")
				   (metadata
					"TechnoFM: Radio-1 192K MP3"
					"http://stream.techno.fm/radio1-192k.mp3"
					1 url))))

	  (setq emms-streams-built-in-list
			(append emms-streams-built-in-list my-emms-streams)))

  ;; TODO remove when hook is added
  (defun update-emms-faces ()
	"No op."
	()))


;;; dired-git-info
(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode)
  (setq dgi-commit-message-format "%cr\t%s"))

;;; Undohist

(if (require 'undohist nil t)
  (undohist-initialize))

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

	  ;; TODO Only for backwards compatibility
	  (when (package-installed-p 'undo-tree)
		;; Disable undo-tree-mode to prevent bugs
		(global-undo-tree-mode 0))

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
	  (evil-set-initial-state 'Buffer-menu-mode 'emacs)
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

	  ;; Magit message editing
	  (add-to-list 'evil-buffer-regexps '("^[^_]*_EDITMSG$" . emacs))

	  ;; Jupyter notebooks
	  (add-to-list 'evil-buffer-regexps
				   '("^ ?\\*ein: .*\\*\\(?:\\[.*\\]\\)?$" . emacs))

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
	  (define-key evil-ex-search-keymap (kbd "C-b") 'backward-char)
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

	  (defun my-maybe-evil-repeat-pop-next ()
		"Execute `evil-repeat-pop-next' or `xref-find-definitions'.
The choice depends on the whether `evil-repeat-pop-next' makes sense to call."
		(interactive)
		(condition-case nil
			(call-interactively 'evil-repeat-pop-next)
		  (user-error (call-interactively 'xref-find-definitions))))

	  ;; Prepare Xref
	  (with-eval-after-load "xref"
		(push 'my-maybe-evil-repeat-pop-next (cdr xref-prompt-for-identifier)))

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

	  ;; Disable company-mode in remote shells
	  (when (require 'tramp nil t)
		(dolist (mode-hook '(eshell-mode-hook shell-mode-hook))
		  (add-hook mode-hook
					(lambda ()
					  (if (file-remote-p default-directory)
						  (company-mode 0)))))))

;;; Company quickhelp
(when (functionp 'company-quickhelp-mode)
  (company-quickhelp-mode)
  (setq company-quickhelp-delay 0.65)
  (define-key company-active-map
	(kbd "M-h") 'company-quickhelp-manual-begin))

;;; Company-lsp
;; TODO not required anymore; delete?
;; (if (require 'company-lsp nil t)
;; 	(push 'company-lsp company-backends))

;;; Ivy
(when (and (functionp 'ivy-mode) t)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")

  ;; Cannot quit `visit-tags-table'; disable it
  (define-key ivy-minibuffer-map (kbd "M-.")
	(lambda () (interactive) (message "not allowed here")))

  ;; default switch-to-buffer
  (global-set-key (kbd "C-x b")   'ivy-switch-buffer)
  ;; default switch-to-buffer-other-window
  (global-set-key (kbd "C-x 4 b") 'ivy-switch-buffer-other-window)

  ;; Resume Ivy dispatch (C-c r)
  (define-key mode-specific-map (kbd "r") 'ivy-resume)

  (when (functionp 'swiper)
	;; default isearch-forward (becomes isearch-forward-regexp on our setup)
	(global-set-key (kbd "C-s")	'swiper))

  (when (functionp 'counsel-M-x)
	;; default execute-extended-command
	(global-set-key (kbd "M-x")	'counsel-M-x)
	;; default find-file
	(global-set-key (kbd "C-x C-f") 'counsel-find-file)
	;; default dired
	(global-set-key (kbd "C-x d")	'counsel-dired)
	;; default bookmark-jump
	(global-set-key (kbd "C-x r b") 'counsel-bookmark)
	;; default describe-function
	(global-set-key (kbd "<f1> f")	'counsel-describe-function)
	;; default describe-variable
	(global-set-key (kbd "<f1> v")	'counsel-describe-variable)
	;; default view-lossage
	(global-set-key (kbd "<f1> l")	'counsel-find-library)
	;; default (on <f1>) info
	(global-set-key (kbd "<f2> i")	'counsel-info-lookup-symbol)
	(global-set-key (kbd "<f2> u")	'counsel-unicode-char)

	(define-key mode-specific-map (kbd "j") 'counsel-semantic-or-imenu)
	(define-key my-music-map	  (kbd "o") 'counsel-rhythmbox))
  )


;;; Helm
(when (and nil (>= emacs-major-version 26)
		   (require 'helm-config nil t))
  ;;(global-set-key (kbd "M-x") 'helm-M-x)
  ;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;;(global-set-key (kbd "C-x C-b") 'helm-mini) ; or helm-buffers-list
  ;;(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  ;;(global-set-key (kbd "C-s") 'helm-swoop) ; external package
  ;;(global-set-key (kbd "M-s o") 'helm-occur)
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

;;; Dash-docs
(when (functionp 'dash-docs-activate-docset)
  (setq dash-docs-docsets-path (expand-file-name my-docsets-path))
  (setq dash-docs-browser-func 'eww)

  ;; Configure buffer-local `dash-docs-docsets' per mode.
  ;; It's undocumented but inspected from `dash-docs-buffer-local-docsets'.
  (add-hook 'c-mode-hook (setq-local dash-docs-docsets '("C")))
  (add-hook 'c++-mode-hook (setq-local dash-docs-docsets '("C++")))
  (when (functionp 'cmake-mode)
	(add-hook 'cmake-mode-hook (setq-local dash-docs-docsets '("CMake"))))

  ;; Search docs (C-c d)
  (define-key mode-specific-map (kbd "d") 'counsel-dash-at-point)
  ;; Activate docset (C-c x D)
  (define-key my-extended-map (kbd "D") 'dash-docs-activate-docset))

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
  ;; (add-hook 'prog-mode-hook 'yas-minor-mode)

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
  ;; (pdf-tools-install)
  (pdf-loader-install)					; deferred

  ;; Start with fit page
  (setq-default pdf-view-display-size 'fit-page)

  (when (functionp 'swiper)
	(with-eval-after-load "pdf-view"
	  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward-regexp)))

  ;; Enable printer
  (setq pdf-misc-print-program "lpr")

  (with-eval-after-load "pdf-view"
	;; Add some color filters
	;; TODO add selector menu

	;; Dark filters
	;; (setq pdf-view-midnight-colors '("white" . "black"))
	;; solarized-dark-high-contrast (base0 and base03)
	(setq pdf-view-midnight-colors '("#8d9fa1" . "#002732"))
	;; solarized-light-high-contrast (base0 and base03)
	;; (setq pdf-view-midnight-colors '("#88999b" . "#00212b"))

	;; Light filters
	;; (setq pdf-view-midnight-colors '("black" . "white"))
	;; solarized-light (base00 and base3)
	;; (setq pdf-view-midnight-colors '("#657b83" . "#fdf6e3"))
	;; solarized-light-high-contrast (base00 and base3)
	;; (setq pdf-view-midnight-colors '("#596e76" . "#fffce9"))
	;; solarized-dark-high-contrast (base00 and base3)
	;; (setq pdf-view-midnight-colors '("#60767e" . "#ffffee"))
	)

  ;; Ability to reset the cursor so it does not bother us.
  ;; Otherwise, the cursor may dangle behind the picture which is a
  ;; visual distraction.
  (defun pdfview-reset-cursor ()
	"Set point in the selected window to the result of `point-min'."
	(interactive)
	(set-window-point (selected-window) (point-min)))

  (with-eval-after-load "pdf-view"
	;; Ignore mouse
	(define-key pdf-view-mode-map (kbd "<mouse-1>") 'ignore)
	(define-key pdf-view-mode-map (kbd "<mouse-2>") 'ignore)

	;; Reset cursor (C-c y r)
	(define-key pdf-view-mode-map (kbd "C-c y r") 'pdfview-reset-cursor))

  ;; Create better Org mode links
  (require 'ol)

  (org-link-set-parameters "pdfview"
						   :follow #'org-pdfview-open
						   :export #'org-pdfview-export
						   :store #'org-pdfview-store-link)

  (defun org-pdfview-export (link description format)
	"Export a pdfview LINK with DESCRIPTION in FORMAT from Org files."
	(let ((path (if (string-match "\\(.*?\\)\\(?:::\\([0-9]+\\)\\)?$" link)
					(match-string 1 link)
				  link))
		  (page (and (match-beginning 2) (match-string 2 link)))
		  (desc (or description link)))
	  (when (stringp path)
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
	  (advice-add 'vterm--internal :after
				  (lambda (&rest _args)
					(dont-show-whitespace)
					(disable-string-face))
				  '((name . "disable-special-visuals")))
	  (define-key my-extended-map (kbd "t") 'vterm)
	  (define-key my-extended-map (kbd "4 t") 'vterm-other-window)

	  (defun vterm-ssh (host)
		"Start `vterm' and instantly SSH to HOST."
		(vterm t)
		(vterm-send-string (concat "ssh " host "\n")))

	  ;; TODO support user + host
	  ;; TODO merge this and term-tramp (DRY)
	  (defun vterm-tramp (&optional arg)
		"Optionally SSH to the current remote when starting `vterm'.
If ARG is nil and we are not in a TRAMP buffer, reuse an existing vterm."
		(interactive "P")
		(let ((host (file-remote-p default-directory 'host))
			  (user (file-remote-p default-directory 'user)))
		  (if host
			  (let ((full-host (concat (when user (concat user "@")) host)))
				(vterm-ssh full-host)
				(vterm-send-string
				 (concat "cd "
						 (file-remote-p default-directory 'localname)
						 "\n")))
			(vterm arg))))

	  (with-eval-after-load "vterm"
		;; Allow to send C-z easily (C-c C-z)
		(define-key vterm-mode-map (kbd "C-c C-z") 'vterm-send-C-z)
		;; Allow to send C-z easily (C-c y z)
		(define-key vterm-mode-map (kbd "C-c y z") 'vterm-send-C-z)
		;; Allow to send C-x easily (C-c y x)
		(define-key vterm-mode-map (kbd "C-c y x") 'vterm-send-C-x))

	  (setq vterm-max-scrollback 10000)

	  (defun vterm--watch-for-password-prompt (process input &rest _args)
		"Prompt for password and send to PROCESS without echoing.
Checks if INPUT contains a password prompt as defined by
`comint-password-prompt-regexp'."
		(when (let ((case-fold-search t))
				(string-match comint-password-prompt-regexp input))
		  (let* ((prompt (match-string 0 input))
				 (password (read-passwd prompt)))
			(vterm-send-string password)
			(clear-string password)
			(vterm-send-return))))

	  (advice-add 'vterm--filter :after
				  #'vterm--watch-for-password-prompt))
  ;; Enter terminal (C-c x t)
  (define-key my-extended-map (kbd "t") 'term))

;;; toc-org
(when (require 'toc-org nil t)
  (add-hook 'org-mode-hook 'toc-org-mode)
  (add-to-list 'org-tag-alist '("TOC" . ?T)))

;;; org-gcal
(setq org-gcal-remove-cancelled-events t)
(setq org-gcal-remove-events-with-cancelled-todo t)

;;; Jupyter
(when (>= emacs-major-version 26)
  (setq exec-path (append exec-path (list (expand-file-name my-jupyter-dir))))
  (when (functionp 'org-babel-jupyter-scratch-buffer)
	(setq my-org-babel-load-languages
		  (append my-org-babel-load-languages
				  '(
					;; (julia . t)  TODO needs upstream fix; is too old
					;; (jupyter . t)
					)))))

;;; Emacs IPython Notebook
;; Polymode for highlighting and editing
;; TODO deprecated: Is always enabled, so not necessary anymore.
(setq ein:polymode t)
;; Inline images
(setq ein:output-area-inlined-images t)

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

;;; JavaScript mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("nodejs" . js2-mode))

;;; PHP mode
(autoload 'php-mode-hook "php-mode")
(add-hook 'php-mode-hook
		  (lambda ()
			(setq-local indent-tabs-mode t)
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
			(setq-local whitespace-line-column 79)
			(setq-local fill-column 72)
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

;;; Extempore
(setq extempore-path "~/Downloads/extempore")


;;; Rust mode
;; (require 'rust-mode)
(autoload 'rust-mode-hook "rust-mode")
(add-hook 'rust-mode-hook
		  (lambda ()
			(setq-local indent-tabs-mode nil)
			;; For comments; not perfect though since indentation
			;; should be ignored.
			(setq-local fill-column 80)
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
  (defun my-lsp-mode-autostart-hook ()
	"Hook to autostart lsp-mode in relevant buffers."
	(unless (eq major-mode 'ein:ipynb-mode)
	  (lsp-deferred)))					; lsp or lsp-deferred

  (defvar my-lsp-mode-hooks '(prog-mode-hook))

  (setq lsp-before-save-edits nil)
  (setq lsp-completion-enable-additional-text-edit nil)
  (when (< emacs-major-version 27)
	(setq lsp-completion-styles '(basic)))

  ;; Disable forced automatic installation
  (defun lsp--completing-read--disable-forced-installation (orig-fun &rest args)
	"Return nil to `lsp--completing-read' when the query is a certain statement.
Advice around ORIG-FUN, called with ARGS."
	(unless
		(string= (car args)
				 (concat "Unable to find installed server supporting this "
						 "file. The following servers could be installed "
						 "automatically: "))
	  (apply orig-fun args)))
  (advice-add 'lsp--completing-read :around
			  #'lsp--completing-read--disable-forced-installation
			  '((name . "disable-forced-installation")))


  (when (executable-find "rust-analyzer")
	(setq lsp-rust-server 'rust-analyzer)
	(autoload 'lsp-rust-analyzer-inlay-hints-mode "lsp-rust")
	(lsp-rust-analyzer-inlay-hints-mode 1)
	;; Config for rustc development (maybe use as .dir-locals.el)
	;; (setq lsp-rust-analyzer-cargo-override-command
	;; 	  ["./x.py" "check" "--json-output"])
	;; (setq lsp-rust-analyzer-rustfmt-override-command
	;; 	  ["./build/TARGET_TRIPLE/stage0/bin/rustfmt"])
	)

  ;; Parallel jobs for the LSP and store index in file
  (setq lsp-clients-clangd-args '("-j=4" "-background-index"))

  ;; Source also disables this due to config file being required for usefulness.
  ;; (setq lsp-pylsp-plugins-pylint-enabled t)
  ;; (setq lsp-pylsp-plugins-pycodestyle-max-line-length 100)
  ;; (setq lsp-pylsp-plugins-flake8-max-line-length 100)
  (setq lsp-pylsp-plugins-pydocstyle-enabled t)
  (setq lsp-pylsp-plugins-rope-completion-enabled t)
  (setq lsp-pylsp-plugins-yapf-enabled t)
  (setq lsp-pylsp-plugins-flake8-enabled t)

  ;; lsp-julia
  (setq lsp-julia-default-environment my-julia-default-environment)
  ;; If we don't want to use the included Language Server:
  ;; (setq lsp-julia-package-dir nil)
  (when (require 'lsp-julia nil t)
	nil
	;; (add-hook 'julia-mode-hook 'lsp-mode)
	;; (add-hook 'julia-mode-hook 'lsp)
	)


  ;; TRAMP enabled pylsp example
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "/path/to/pylsp")
  ;; 					:major-modes '(python-mode)
  ;; 					:remote? t
  ;; 					:server-id 'pylsp-remote))

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


  (defun my-eglot-autostart-hook ()
	"Hook to autostart eglot in relevant buffers."
	(eglot-ensure))

  (defvar my-eglot-hooks ())

  (when (executable-find "rls")
	;; (add-to-list 'eglot-server-programs `((rust-mode) eglot-rls ,my-rls-bin))
	(push 'rust-mode-hook my-eglot-hooks))

  ;; TODO replace with tsserver when it's ready
  (when (executable-find "typescript-language-server")
	(push 'js-mode-hook my-eglot-hooks)
	(push 'typescript-mode-hook my-eglot-hooks))

  (when (executable-find "pylsp")
	(setcdr (assoc 'python-mode eglot-server-programs) '("pylsp"))
	(push 'python-mode-hook my-eglot-hooks))

  (when (executable-find "clangd")
	(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
	(push 'c-mode-hook my-eglot-hooks)
	(push 'c++-mode-hook my-eglot-hooks))

  (when (executable-find "digestif")
	(push 'tex-mode-hook my-eglot-hooks))

  (add-to-list 'eglot-server-programs '((gd-script-mode) "localhost"
										6008 "tls")) ; or "starttls" or nil
  (push 'gd-script-mode-hook my-eglot-hooks)

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

  ;; 	(push 'julia-mode-hook my-eglot-hooks)
  ;; 	;; Wait longer due to slow compilation
  ;; 	(add-hook 'julia-mode-hook
  ;; 			  (lambda () (setq-local eglot-connect-timeout 90))))

  ;; Example .dir-locals.el for per-project config (place in root folder)
  ;; ((python-mode
  ;; 	. ((eglot-workspace-configuration . (
  ;; 										 (:pylsp . (:plugins (:pycodestyle (:maxLineLength 100))))
  ;; 										 (:pylsp . (:plugins (:pydocstyle (:enabled t))))
  ;; 										 (:pylsp . (:plugins (:pydocstyle (:maxLineLength 100))))
  ;; 										 ))
  ;; 	   (whitespace-line-column . 100))))

  ;; ... or maybe like this:
  ;; ((python-mode
  ;; 	. ((eglot-workspace-configuration . ((:pylsp . (
  ;; 													(:plugins (:pycodestyle (:maxLineLength 100)))
  ;; 													(:plugins (:pydocstyle (:enabled t)))
  ;; 													(:plugins (:pydocstyle (:maxLineLength 100)))
  ;; 													))))
  ;; 	   (whitespace-line-column . 100))))
  )


;;; All LSP modes

(defvar my-lsp-package-active t)

(defun my-modify-lsp-package-hooks (add-hooks-p)
  "Add or remove the hooks for `my-lsp-package', depending on ADD-HOOKS-P.
If ADD-HOOKS-P is non-nil, activate the hooks, otherwise
deactivate them."
  (let* ((symbol-string
		  (symbol-name (if (eq my-lsp-package 'all)
						   my-autostart-lsp-package
						 my-lsp-package)))
		 (package-hooks (symbol-value
						 (intern (concat "my-" symbol-string "-hooks"))))
		 (autostart-hook
		  (intern (concat "my-" symbol-string "-autostart-hook")))
		 (modify-hook-function (if add-hooks-p #'add-hook #'remove-hook)))
	(mapc (lambda (elem)
			(funcall modify-hook-function elem autostart-hook))
		  package-hooks)
	(setq my-lsp-package-active add-hooks-p)))

;; Autostart LSP package by default
(when (require (if (eq my-lsp-package 'all)
				   my-autostart-lsp-package
				 my-lsp-package)
			   nil t)
  (my-modify-lsp-package-hooks t))

(defun my-toggle-lsp-package ()
  "Toggle whether `my-lsp-package' is activated for each new buffer."
  (interactive)
  (my-modify-lsp-package-hooks (not my-lsp-package-active)))

;; Toggle automatic lsp-package activation (C-c t L)
(define-key my-toggle-map (kbd "L") 'my-toggle-lsp-package)


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

;;; Extended standard commands

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (when (y-or-n-p "Kill all other buffers? ")
	(mapc #'kill-buffer (delq (current-buffer) (buffer-list)))))

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
  "Insert the abbreviated variable `buffer-file-name' into the minibuffer."
  (interactive)
  (minibuffer-insert
   (lambda (&rest _args) (abbreviate-file-name buffer-file-name))))

(defun minibuffer-insert-buffer-file-name ()
  "Insert the variable `buffer-file-name' into the minibuffer."
  (interactive)
  (minibuffer-insert
   (lambda (&rest _args) buffer-file-name)))

(defun minibuffer-insert-abbreviated-default-directory ()
  "Insert the abbreviated `default-directory' into the minibuffer."
  (interactive)
  (minibuffer-insert
   (lambda (&rest _args) (abbreviate-file-name default-directory))))

(defun minibuffer-insert-default-directory ()
  "Insert the `default-directory' into the minibuffer."
  (interactive)
  (minibuffer-insert
   (lambda (&rest _args) default-directory)))

(defun clear-kill-ring ()
  "Clear all entries from the kill ring."
  (interactive)
  (setq kill-ring nil))

(defun save-file-name ()
  "Append the current variable `buffer-file-name' to the kill ring.
If variable `buffer-file-name' is nil, use `default-directory'."
  (interactive)
  (kill-new (or buffer-file-name default-directory)))

(defun quit-other-window ()
  "Quit the other (next) window while staying in the selected window."
  (interactive)
  (let ((current-window (selected-window)))
	(select-window (next-window))
	(quit-window nil)
	(select-window current-window)))

(defun swap-window-buffers ()
  "Swap the selected window's buffer with the other (next) one's."
  (interactive)
  (let* ((curr-buffer (current-buffer))
		 (other-window (next-window))
		 (other-buffer (window-buffer other-window)))
	(set-window-buffer other-window curr-buffer)
	(switch-to-buffer other-buffer)))

(defun has-final-newline ()
  "Return whether the current buffer has a final newline."
  (interactive)
  ;; Adapted from `files.el`.
  (message "%s" (not (and (> (point-max) (point-min))
						  (/= (char-after (1- (point-max))) ?\n)
						  (not (and (eq selective-display t)
									(= (char-after (1- (point-max))) ?\r)))))))


;;; WAV

(require 'bindat)

;; Writing WAV data

(defun wav-pcm-bindat-spec (num-channels num-blocks sample-width)
  "Return a bindat spec for the WAV PCM format based on the given parameters.
NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-WIDTH the number of bytes per sample."
  `((ckID str 4)
	(cksize u32r)
	(WAVEID str 4)
	(ckID2 str 4)
	(cksize2 u32r)
	(wFormatTag u16r)
	(nChannels u16r)
	(nSamplesPerSec u32r)
	(nAvgBytesPerSec u32r)
	(nBlockAlign u16r)
	(wBitsPerSample u16r)
	(ckID3 str 4)
	(cksize3 u32r)
	;; (samples repeat ,(* num-channels num-blocks)
	;; 		 ())
	;; (samples vec ,(* sample-width num-channels num-blocks) str)
	(samples vec ,(* num-channels num-blocks)
			 ,(alist-get sample-width '((1 . byte) (2 . u16r)
										(3 . u24r) (4 . u32r))))
	(align 2)))

(defun wav-pcm-create-struct (num-channels
							  num-blocks sample-rate sample-width samples)
  "Create a WAV struct containing the given SAMPLES.
NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  `((ckID . "RIFF")
	(cksize . ,(+ 4 24 8 (* sample-width num-channels num-blocks)
				  (if (cl-oddp (* sample-width num-channels num-blocks)) 1 0)))
	(WAVEID . "WAVE")
	(ckID2 . "fmt ")
	(cksize2 . 16)
	(wFormatTag . #x0001)
	(nChannels . ,num-channels)
	(nSamplesPerSec . ,sample-rate)
	(nAvgBytesPerSec . ,(* sample-rate sample-width num-channels))
	(nBlockAlign . ,(* sample-width num-channels))
	(wBitsPerSample . ,(* 8 sample-width))
	(ckID3 . "data")
	(cksize3 . ,(* sample-width num-channels num-blocks))
	;; `(* num-channels num-blocks)' channel-interleaved
	;; `sample-width'-byte samples
	(samples . ,(vconcat (mapcar #'identity samples)))))

(defun wav-data-to-string (num-channels
						   num-blocks sample-rate sample-width samples)
  "Return the WAV string for the given SAMPLES.
NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (let* ((spec (wav-pcm-bindat-spec num-channels num-blocks sample-width))
		 (struct (wav-pcm-create-struct num-channels num-blocks sample-rate
										sample-width samples))
		 (packed (bindat-pack spec struct)))
	packed))

(defun wav-pack-samples (num-channels
						 num-blocks sample-rate sample-width samples)
  "Return SAMPLES as a WAV string, discretized and packed.
NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (let* ((samples (wav-normalize-samples samples))
		 (samples (wav-discretize-samples samples sample-width))
		 (packed (wav-data-to-string num-channels num-blocks sample-rate
									 sample-width samples)))
	packed))

;;; Sound

;; Parameter handling utilities

(defun wav-max (sample-width)
  "The maximum value representable for SAMPLE-WIDTH."
  (if (<= sample-width 1)
	  (1- (expt 2 (* 8 sample-width)))
	(1- (expt 2 (1- (* 8 sample-width))))))

(defun wav-min (sample-width)
  "The maximum value representable for SAMPLE-WIDTH."
  (if (<= sample-width 1)
	  0
	(* -1 (wav-max sample-width))))

(defun wav-map-range (func num-channels num-blocks sample-width)
  "Return a vector of samples created by mapping FUNC over a range.
The range goes from 0 to NUM-CHANNELS * NUM-BLOCKS.
SAMPLE-WIDTH controls the number of bytes per sample."
  (let* ((num-samples (* num-channels num-blocks))
		 (samples (make-vector num-samples 0.0)))
	(dotimes (i num-samples)
	  (aset samples i (funcall func i num-samples sample-width)))
	samples))

(defun wav-map-range-rescaled (func
							   num-channels num-blocks sample-rate sample-width)
  "Return a vector of samples by mapping FUNC over a range.
The range goes from 0 to NUM-CHANNELS * NUM-BLOCKS.
The x values are mapped to the time elapsed in seconds.

NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (let ((factor (wav-rescale-factor sample-rate)))
	(wav-map-range (lambda (x &optional _num-samples _sample-width)
					 (funcall func (* x factor)))
				   num-channels num-blocks sample-width)))

(defun wav-map-range-rescaled-hz (func num-channels num-blocks sample-rate
									   sample-width hertz &optional freq)
  "Return a vector of samples by mapping FUNC with the given HERTZ over a range.
The range goes from 0 to NUM-CHANNELS * NUM-BLOCKS.
The x values are mapped to the time elapsed in seconds.

NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample.
FREQ is the base frequency (2 pi by default)."
  (let ((factor (wav-rescale-factor-hz sample-rate hertz freq)))
	(wav-map-range (lambda (x &optional _num-samples _sample-width)
					 (funcall func (* x factor)))
				   num-channels num-blocks sample-width)))

;; Rescaling x values according to time and hertz

;; FIXME do not do this! instead scale the obtained values so
;; we can combine different frequencies!

(defun wav-rescale-factor (sample-rate)
  "Return a factor to scale an X value to the time at X seconds.
SAMPLE-RATE is the number of samples per second."
  (/ 1 (float sample-rate)))
;; (* (or base-freq (* 2 float-pi)) hertz)

(defun wav-rescale-x (x sample-rate)
  "Return X rescaled to the time at X seconds.
SAMPLE-RATE is the number of samples per second.
BASE-FREQ is the base frequency of the wave we want to rescale to (2 pi if nil)."
  (* x (wav-rescale-factor sample-rate)))

(defun wav-rescale-factor-hz (sample-rate hertz &optional base-freq)
  "Return a factor to scale an X value to the time at X seconds.
SAMPLE-RATE is the number of samples per second.
HERTZ is the number of wave frequencies per second to target.
BASE-FREQ is the base frequency of the wave we want to rescale to (2 pi if nil)."
  (/ (* (or base-freq (* 2 float-pi)) hertz) (float sample-rate)))

(defun wav-rescale-x-hz (x sample-rate hertz &optional base-freq)
  "Return X rescaled to the time at X seconds.
SAMPLE-RATE is the number of samples per second.
HERTZ is the number of wave frequencies per second to target.
BASE-FREQ is the base frequency of the wave we want to rescale to (2 pi if nil)."
  (* x (wav-rescale-factor sample-rate hertz base-freq)))

;; Normalization

(defun wav-normalize-samples (samples &optional abs-maximum)
  "Return SAMPLES mapped to [-1, 1].
ABS-MAXIMUM, the absolute maximum value, may be given or will be found if nil."
  (let ((abs-maximum (or abs-maximum
						 (cl-reduce (lambda (a b) (max (abs a) (abs b)))
									samples))))
	(dotimes (i (length samples))
	  (aset samples i (/ (aref samples i) abs-maximum)))
	samples))

;; Discretization

(defun wav-discretize (sample sample-width)
  "Return SAMPLE mapped from [-1, 1] to the value range given by SAMPLE-WIDTH.
The obtained range is [(`wav-min' SAMPLE-WIDTH), (`wav-max' SAMPLE-WIDTH)]."
  (let ((discretize-max (wav-max sample-width)))
	(if (<= sample-width 1)
		(wav-discretize-unsigned-precalc sample discretize-max)
	  (wav-discretize-signed-precalc sample discretize-max))))

(defun wav-discretize-unsigned-precalc (sample discretize-max)
  "Discretize SAMPLE according to DISCRETIZE-MAX, assuming unsigned values.
DISCRETIZE-MAX contains the desired maximum value to discretize to."
  (round (* (1+ sample) discretize-max 0.5)))

(defun wav-discretize-signed-precalc (sample discretize-max)
  "Discretize SAMPLE according to DISCRETIZE-MAX, assuming signed values.
DISCRETIZE-MAX contains the desired maximum value to discretize to."
  (round (* sample discretize-max)))

(defun wav-discretize-samples (samples sample-width)
  "Return continuous SAMPLES mapped to the discrete range of SAMPLE-WIDTH.
SAMPLES are expected to be in [-1, 1].
The obtained integer range is
[(`wav-min' SAMPLE-WIDTH), (`wav-max' SAMPLE-WIDTH)]."
  (let* ((discretize-max (wav-max sample-width))
		 (discretize-func (if (<= sample-width 1)
							  'wav-discretize-unsigned-precalc
							'wav-discretize-signed-precalc)))
	(dotimes (i (length samples))
	  (aset samples i (funcall discretize-func (aref samples i)
							   discretize-max)))
	samples))

;; Base waveforms

(defun wav-noise-raw (_i _num-samples sample-width)
  "Return a uniform random number between `wav-min' and `wav-max'.
Discretized to the given SAMPLE-WIDTH."
  (* (if (or (<= sample-width 1) (eq (random 1) 1)) 1 -1)
	 (random (wav-max sample-width))))

(defun wav-create-noise-raw (num-channels num-blocks sample-width &rest _args)
  "Return a vector of discrete uniform random noise.
NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-WIDTH the number of bytes per sample."
  (wav-map-range #'wav-noise-raw num-channels num-blocks sample-width))

(defun wav-noise (_i _num-samples sample-width)
  "Return a uniform random number between `wav-min' and `wav-max'.
The range is given by SAMPLE-WIDTH."
  (* (if (or (<= sample-width 1) (eq (random 1) 1)) 1 -1)
	 (cl-random 1.0)))

(defun wav-create-noise (num-channels num-blocks sample-width &rest _args)
  "Return a vector of normalized uniform random noise.
NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-WIDTH the number of bytes per sample."
  (wav-map-range #'wav-noise
				 num-channels num-blocks sample-width))

(defun wav-sin (x)
  "Return a sin at X."
  (sin x))

(defun wav-create-sin (num-channels num-blocks sample-rate sample-width
									hertz &optional freq)
  "Return a vector containing a sin wave with the given amount of HERTZ.
The x values are mapped to the time elapsed in seconds.

NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (wav-map-range-rescaled #'wav-sin
						  num-channels num-blocks sample-rate sample-width
						  hertz freq))

(defun wav-saw (x &optional freq)
  "Return a sawtooth wave function at X with frequency FREQ or 2 pi."
  (let ((freq (or freq (* 2 float-pi))))
	(1- (* 2 (/ (mod x freq) freq)))))

(defun wav-inv-saw (x &optional freq)
  "Return an inverted sawtooth wave function at X with frequency FREQ or 2 pi."
  (- (wav-saw x (or freq (* 2 float-pi)))))

(defun wav-create-saw (num-channels num-blocks sample-rate sample-width
									hertz &optional freq)
  "Return a vector containing a sawtooth wave with the given amount of HERTZ.
The x values are mapped to the time elapsed in seconds.
FREQ is the base frequency of the wave (2 pi if nil).

NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (wav-map-range-rescaled #'wav-saw
						  num-channels num-blocks sample-rate sample-width
						  hertz freq))

(defun wav-create-inv-saw (num-channels num-blocks sample-rate sample-width
										hertz &optional freq)
  "Return a vector containing an inverse sawtooth wave with the given HERTZ.
The x values are mapped to the time elapsed in seconds.
FREQ is the base frequency of the wave (2 pi if nil).

NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (wav-map-range-rescaled #'wav-inv-saw
						  num-channels num-blocks sample-rate sample-width
						  hertz freq))

(defun wav-square (x &optional freq)
  "Return a square wave function at X with frequency FREQ or 2 pi."
  (wav-pulse x 0.5 freq))

(defun wav-create-square (num-channels num-blocks sample-rate sample-width
									   hertz &optional freq)
  "Return a vector containing a square wave with the given amount of HERTZ.
The x values are mapped to the time elapsed in seconds.
FREQ is the base frequency of the wave (2 pi if nil).

NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (wav-map-range-rescaled #'wav-square
						  num-channels num-blocks sample-rate sample-width
						  hertz freq))

(defun wav-pulse (x &optional ratio freq)
  "Return a pulse wave function at X with frequency FREQ or 2 pi.
RATIO controls the point in the wave at which the pulse is located (0.5 by
default, giving a square wave)."
  (let ((freq (or freq (* 2 float-pi))))
	(if (< (mod x freq) (* freq (or ratio 0.5))) -1.0 1.0)))

(defun wav-create-pulse (num-channels num-blocks sample-rate sample-width
									  hertz &optional freq)
  "Return a vector containing a pulse wave with the given amount of HERTZ.
The x values are mapped to the time elapsed in seconds.
FREQ is the base frequency of the wave (2 pi if nil).

NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (wav-map-range-rescaled #'wav-pulse
						  num-channels num-blocks sample-rate sample-width
						  hertz freq))

(defun wav-triangle (x &optional ratio freq)
  "Return a triangle wave function at X with frequency FREQ or 2 pi.
RATIO controls the point in the wave at which the peak is
located (0.5 by default)."
  (let* ((freq (or freq (* 2 float-pi)))
		 (ratio (or ratio 0.5))
		 (pos (mod x freq))
		 (peak-pos (* freq ratio)))
	(if (< pos peak-pos)
		(wav-saw pos peak-pos)
	  (let ((inv-saw-freq (- freq peak-pos)))
		(wav-inv-saw (- pos peak-pos) inv-saw-freq)))))

(defun wav-create-triangle (num-channels num-blocks sample-rate sample-width
									   hertz &optional freq)
  "Return a vector containing a triangle wave with the given amount of HERTZ.
The x values are mapped to the time elapsed in seconds.
FREQ is the base frequency of the wave (2 pi if nil).

NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (wav-map-range-rescaled #'wav-triangle
						  num-channels num-blocks sample-rate sample-width
						  hertz freq))

;; Playing sound

(defun wav-play-sound (sound-string)
  "Play the sound contained in SOUND-STRING."
  (play-sound (list 'sound :data sound-string)))

(defun wav-play-test (&optional num-channels
								num-blocks sample-rate sample-width wave hertz)
  "Play an example sound.
NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample,
WAVE the waveform to use for the sound,
HERTZ the amount of hertz in the example sound."
  (let* ((num-channels (or num-channels 1))
		 (sample-rate (or sample-rate 41000))
		 (num-blocks (or num-blocks (* num-channels sample-rate)))
		 (sample-width (or sample-width 2))
		 (hertz (or hertz 2000))
		 (wave-func (intern (concat "wav-create-" (symbol-name wave))))
		 (samples (funcall
				   wave-func
				   num-channels num-blocks sample-rate sample-width hertz))
		 (packed (wav-pack-samples num-channels num-blocks sample-rate
								   sample-width samples)))
	(write-region packed nil (expand-file-name "~/test-elisp-wave.wav"))
	(wav-play-sound packed)))

(defun wav-play-samples (num-channels
						 num-blocks sample-rate sample-width samples)
  "Play the given SAMPLES.
NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (let ((packed (wav-pack-samples num-channels num-blocks sample-rate
								  sample-width samples)))
	(wav-play-sound packed)))

(defun wav-create-and-play (func
							num-channels num-blocks sample-rate sample-width)
  "Play the sound created by applying FUNC to a range of values.

NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (let* ((samples (wav-map-range-rescaled func num-channels num-blocks
										  sample-rate sample-width))
		 (packed (wav-pack-samples num-channels num-blocks sample-rate
								   sample-width samples)))
	(wav-play-sound packed)))

;; Envelopes

;; (defun wav-held-adsr (x attack decay sustain release hold-length
;; 						sample-rate)
;;   ""
;;   ())

(defun wav-held-adsr-intensities (attack decay sustain release hold-length
										 num-channels num-blocks sample-rate)
  "Return values for an ADSR envelope that is held for HOLD-LENGTH seconds.
ATTACK, DECAY, SUSTAIN and RELEASE control the values with the same name in
the envelope, where durations are measure in seconds and the intensity is given
normalized (in [0, 1]).
HOLD-LENGTH corresponds to the duration of pressing a button on a keyboard.

NUM-CHANNELS controls the number of channels (1 for mono, 2 for stereo),
NUM-BLOCKS the number of samples per channel,
SAMPLE-RATE the number of samples per second,
SAMPLE-WIDTH the number of bytes per sample."
  (let* ((num-samples (* num-channels num-blocks))
		 (intensities (make-vector num-samples 0.0)))
	(dotimes (i num-samples)
	  (aset intensities i (wav-held-adsr i num-samples sample-width)))
	intensities))


;;; Alarms

(defvar my-alarm-ring nil)

(defun my-alarm-ring-formula (x)
  "Formula to calculate alarm ringing sound for each X in time."
  (let* ((xx (* x 2 float-pi))
		 (xf (* 880 xx))
		 (harmonics '(1 2 3 4)))
	(* (1+ (wav-square (* xx 7)))
	   (cl-reduce #'+ (mapcar (lambda (harmonic) (wav-sin (/ xf harmonic)))
							  harmonics)))))

(defun my-alarm-ring-packed ()
  "Return a sound string for an alarm ring."
  (let* ((num-channels 1)
		 (num-blocks 30750)
		 (sample-rate 41000)
		 (sample-width 2) 			; 1 or 2 – quality and size mostly same
		 (samples (wav-map-range-rescaled #'my-alarm-ring-formula
										  num-channels num-blocks
										  sample-rate sample-width))
		 (packed (wav-pack-samples num-channels num-blocks sample-rate
								   sample-width samples)))
	packed))

(defun setup-alarm-ring ()
  "Compile the alarm ringing sound and assign it to `my-alarm-ring'.
Will only happen if `my-alarm-ring' is nil."
  (unless my-alarm-ring
	(setq my-alarm-ring (my-alarm-ring-packed)))
  my-alarm-ring)

(defun alarm-ding (&optional alarm-message)
  "Ding, play a sound, and display ALARM-MESSAGE."
  (unless (string-empty-p (or alarm-message ""))
	(message alarm-message))
  (ding-with-sound))

(defun ding-with-sound ()
  "Ring the bell and try to play a sound."
  (if (or (not (fboundp 'play-sound-internal))
		  (not my-alarm-ring))
	  (let ((visible-bell nil))
		(ding t))
	(ding t)
	(make-thread
	 (lambda () (play-sound (list 'sound :data my-alarm-ring))))))

(defun ding-with-sound-from-path (path)
  "Ring the bell and try to play the sound located at PATH.
If playing sound is not available or PATH is not readable, try to ring the bell."
  (let ((path (expand-file-name path)))
	(if (or (not (fboundp 'play-sound-internal))
			(not (file-readable-p path)))
		(let ((visible-bell nil))
		  (ding t))
	  (ding t)
	  (make-thread
	   (lambda () (play-sound (list 'sound :file path)))))))

(defvar my-last-alarm nil)
(defvar my-timer-alist nil
  "An alist of alarm lists.
An alarm's main timer is the car of each list, connecting it to
its other timers.")

(defun decoded-time-set-current (time)
  "Set any nil values in `decoded-time' TIME to current time values."
  (let ((default-time (decode-time)))
	(unless (decoded-time-second time)
	  (setf (decoded-time-second time) (decoded-time-second default-time)))
	(unless (decoded-time-minute time)
	  (setf (decoded-time-minute time) (decoded-time-minute default-time)))
	(unless (decoded-time-hour time)
	  (setf (decoded-time-hour time) (decoded-time-hour default-time)))

	(unless (decoded-time-day time)
	  (setf (decoded-time-day time) (decoded-time-day default-time)))
	(unless (decoded-time-month time)
	  (setf (decoded-time-month time) (decoded-time-month default-time)))
	(unless (decoded-time-year time)
	  (setf (decoded-time-year time) (decoded-time-year default-time)))

	(when (eq (decoded-time-dst time) -1)
	  (setf (decoded-time-dst time) (decoded-time-dst default-time)))

	(unless (decoded-time-zone time)
	  (setf (decoded-time-zone time) (decoded-time-zone default-time)))
	time))

(defun my-decode-time-string (time-string)
  "Decode the time in TIME-STRING.
Unknown values are gotten from the current time."
  (when (string-match-p "^[01][0-9]-[0-3][0-9] " time-string)
	(setq time-string (concat (format-time-string "%+4Y-") time-string)))
  (when (string-match-p "^[0-9][0-9]\\(?:-[0-9]?[0-9]\\)\\{2\\}" time-string)
	(user-error "Year needs to be written out"))
  (when (or (string-match-p "^[0-9]?[0-9]-[0-9]?[0-9]" time-string)
			(string-match-p
			 "^[0-9]\\{4\\}-[0-9]\\(?:[0-9]-\\|-[0-9]\\)[0-9] " time-string))
	(user-error "Cannot accept dates without zero-padding"))
  (let ((time (parse-time-string time-string)))
	(decoded-time-set-current time)))

(defun parse-alarm-time-string (time-string)
  "Parse the time in TIME-STRING, preferring interpreting as a duration."
  (let ((duration (timer-duration time-string)))
	(if duration
		duration
	  (encode-time (my-decode-time-string time-string)))))

(defun alarm-preprocess-start-time (start-time)
  "Return a timestamp for the given string or number START-TIME.
The timestamp is in the style of `encode-time'."
  (let* ((start-time (if (stringp start-time)
						 (parse-alarm-time-string start-time)
					   start-time))
		 (start-time (if (numberp start-time)
						 (time-add nil start-time)
					   start-time)))
	start-time))

(defun alarm-preprocess-stop-time (stop-time start-time)
  "Return a timestamp for the given STOP-TIME in relation to START-TIME.
START-TIME is expected to be a timestamp in the style of `encode-time'.
The returned timestamp is also in the style of `encode-time'."
  (let* ((stop-time (if (stringp stop-time)
						(if (string-prefix-p "+" stop-time)
							;; Convert from a single number to a cons timestamp
							(encode-time (decode-time
										  (time-add start-time
													(parse-alarm-time-string
													 (substring stop-time 1)))))
						  (parse-alarm-time-string stop-time))
					  stop-time))
		 (stop-time (if (numberp stop-time)
						(time-add nil stop-time)
					  stop-time)))
	stop-time))

(defun set-alarm (start-time stop-time repeat-interval &optional alarm-message)
  "Set and return an alarm for START-TIME.
STOP-TIME is a string indicating when to stop the timer (following the same
format as START-TIME, except when prefixed by a plus-symbol ('+')).
REPEAT-INTERVAL is the number of seconds between each ding.
ALARM-MESSAGE is a message to display when the alarm rings.

Both START-TIME and STOP-TIME do not follow the `run-at-time'
format for time strings exactly. Instead they use
`parse-alarm-time-string' to handle more time formats but retain
the duration format.
Be careful as STOP-TIME duration is _not_ relative to START-TIME
but also to the current time, unless it is prefix by a +."
  (interactive
   (list
	(read-string
	 (format
	  "Set alarm at (ISO format or duration) (default %s): "
	  "10 mins")
	 nil nil "10 mins")
	(read-string (format
				  (concat "Stop at (ISO format, duration or "
						  "+duration (relative to start)) (default: %s): ")
				  "+10 mins")
				 nil nil "+10 mins")
	(string-to-number (read-string
					   (format
						"Repeat after this many seconds (default %d): "
						3)
					   nil nil "3"))
	(read-string "Alarm message (optional): " nil nil nil)))

  (setup-alarm-ring)
  (let* ((start-time (alarm-preprocess-start-time start-time))
		 (stop-time (alarm-preprocess-stop-time stop-time start-time))
		 (timer (run-at-time start-time repeat-interval #'alarm-ding alarm-message))
		 (setter-timer
		  (run-at-time start-time repeat-interval #'set-last-alarm timer))
		 (stop-timer (run-at-time stop-time nil #'cancel-alarm timer)))
	(push (list timer setter-timer stop-timer) my-timer-alist)
	(save-alarms)
	(set-last-alarm timer)
	(apply #'message "Alarm will ring from %s to %s."
		   (mapcar (lambda (time)
					 (format-time-string "%F %T" time))
				   (list start-time stop-time)))
	timer))

(defun alarm-get-all-timers (timer)
  "Return all timers for the alarm given by TIMER.
Return nil when there is no associated alarm."
  (assoc timer my-timer-alist))

(defun set-last-alarm (timer)
  "Set `my-last-alarm' to TIMER."
  (when (alarm-get-all-timers timer)
	(setq my-last-alarm timer)))

(defun cancel-all-alarms ()
  "Cancel the alarm given by TIMER."
  (interactive)
  (mapc (lambda (entry) (cancel-alarm (car entry))) my-timer-alist))

(defun cancel-alarm (timer)
  "Cancel the alarm given by TIMER.

Also set `my-last-alarm' to the first timer in `my-timer-alist' or nil."
  (when timer
	(mapc #'cancel-timer (alarm-get-all-timers timer))
	(setq my-timer-alist (assoc-delete-all timer my-timer-alist))
	(save-alarms)
	(when (equal my-last-alarm timer)
	  (setq my-last-alarm
			(and (/= 0 (length my-timer-alist))
				 (caar my-timer-alist))))))

(defun cancel-last-alarm ()
  "Cancel the alarm that you either set last or which rung last."
  (interactive)
  (cancel-alarm my-last-alarm))

(defun alarm-get-start-time (timer)
  "Return the start time of the alarm given by TIMER."
  (list (timer--high-seconds timer)
		(timer--low-seconds timer)
		(timer--usecs timer)))

(defun alarm-get-stop-time (timer)
  "Return the stop time of the alarm given by TIMER."
  (let* ((alarm (alarm-get-all-timers timer))
		 (stop-timer (caddr alarm))
		 (stop-time (list (timer--high-seconds stop-timer)
					 (timer--low-seconds stop-timer)
					 (timer--usecs stop-timer))))
	stop-time))

(defun alarm-get-repeat-interval (timer)
  "Return the repeat interval of the alarm given by TIMER.
Usually a number or nil."
  (timer--repeat-delay timer))

(defun alarm-get-message (timer)
  "Return the message of the alarm given by TIMER.
If it does not have a message, return nil."
  (car (timer--args timer)))

(defun save-alarms ()
  "Save all currently set alarms to `my-alarms-path'."
  (if (file-writable-p my-alarms-path)
	  (with-temp-file my-alarms-path
		(insert (format ";; %s --- Alarms saved at %s  -*- %s -*-\n\n"
						(file-name-nondirectory my-alarms-path)
						(format-time-string "%F %T")
						"lexical-binding: t; no-byte-compile: t;"))
		(insert
		 (let ((print-quoted t)
			   (print-circle t))
		   (format "(setq my-timer-alist %s%S)"
				   (if my-timer-alist "'" "")
				   my-timer-alist))))
	(message (concat "cannot write alarms to " my-alarms-path))))

(defun load-alarms ()
  "Add all alarms from `my-alarms-path' to the active ones."
  (if (file-readable-p my-alarms-path)
	  (progn
		(load-file my-alarms-path)
		(when (/= 0 (length my-timer-alist))
		  (unless my-last-alarm
			(setq my-last-alarm (caar my-timer-alist)))
		  (message "Alarms were loaded; compiling ring sound...")
		  (setup-alarm-ring)
		  (message "Sound compiled."))
		(mapc (lambda (alarms)
				(mapc #'timer-activate alarms))
			  my-timer-alist))
	(message (format "cannot read from %s; alarms were not loaded"
					 my-alarms-path))))

(load-alarms)

;; Listing alarms
;; Based on timer-list.el.gz

(defun alarm-list-format (timer)
  "Return a string for the alarm given by TIMER suitable for a list of alarms."
  (format
   "%4s %4s %21s %21s %8s %s"
   ;; Idle.
   (if (aref timer 7) "*" " ")
   ;; Last alarm.
   (if (equal timer my-last-alarm) "*" " ")
   ;; Start time.
   (let ((start-time (alarm-get-start-time timer)))
	 (format-time-string "%F %T" start-time))
   ;; Stop time.
   (let ((stop-time (alarm-get-stop-time timer)))
	 (format-time-string "%F %T" stop-time))
   ;; Repeat.
   (let ((repeat (alarm-get-repeat-interval timer)))
	 (cond
	  ((numberp repeat)
	   (format "%.2f" repeat))
	  ((null repeat)
	   "-")
	  (t
	   (format "%s" repeat))))
   ;; Message.
   (let ((print-escape-newlines t))
	 (alarm-get-message timer))))

(defun list-alarms (&optional _ignore-auto _nonconfirm)
  "List all alarms in a buffer."
  (interactive)
  (pop-to-buffer-same-window (get-buffer-create "*alarm-list*"))
  (let ((inhibit-read-only t))
	(erase-buffer)
	(alarm-list-mode)
	(mapc
	 (lambda (alarm)
	   (let ((timer (car alarm)))
		 (insert (alarm-list-format timer))
		 (put-text-property (line-beginning-position)
							(1+ (line-beginning-position))
							'timer timer))
	   (insert "\n"))
	 my-timer-alist))
  (goto-char (point-min)))

(defvar alarm-list-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map "c" 'alarm-list-cancel)
	(define-key map "n" 'next-line)
	(define-key map "p" 'previous-line)
	(easy-menu-define nil map ""
	  '("Alarms"
		["Cancel" alarm-list-cancel t]))
	map))

(define-derived-mode alarm-list-mode special-mode "Alarm-List"
  "Mode for listing and controlling alarms."
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local truncate-lines t)
  (buffer-disable-undo)
  (setq-local revert-buffer-function #'list-alarms)
  (setq-local buffer-read-only t)
  (setq-local header-line-format
		(concat
		 (propertize " " 'display '(space :align-to 0))
		 (format "%4s %4s %21s %21s %8s %s"
				 "Idle" "Last" "Start Time" "Stop Time" "Repeat" "Message"))))

(defun alarm-at-point ()
  "Return the alarm on the line under point."
  (let ((timer (get-text-property (line-beginning-position) 'timer))
		(inhibit-read-only t))
	(unless timer
	  (error "No timer on the current line"))
	timer))

(defun alarm-list-cancel ()
  "Cancel the alarm on the line under point."
  (interactive)
  (let ((timer (alarm-at-point))
		(inhibit-read-only t))
	(when (y-or-n-p "Really cancel alarm? ")
	  (cancel-alarm timer)
	  (delete-region (line-beginning-position)
					 (line-beginning-position 2)))))

(define-prefix-command 'my-alarms-map)
;; Alarm commands (C-c x a)
(define-key my-extended-map (kbd "a") 'my-alarms-map)

;; Set alarm (C-c x a s)
(define-key my-alarms-map (kbd "s") 'set-alarm)
;; Cancel last alarm (C-c x a c)
(define-key my-alarms-map (kbd "c") 'cancel-last-alarm)
;; List alarms (C-c x a l)
(define-key my-alarms-map (kbd "l") 'list-alarms)

;;; Surround

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
TEXT is reversed literally (\"[a\" -> \"a[\")."
  (interactive "sUnreversed beginning: ")
  (insert-arbitrary-pair text (reverse text)))

(defun insert-tag-pair (tag)
  "Insert the given HTML TAG before and after point or the region if active."
  (interactive "sTag: ")
  (insert-arbitrary-pair (concat "<" tag ">") (concat "</" tag ">")))

(defun delete-around (count)
  "Delete COUNT characters before and after point or the region if active."
  ;; FIXME delete the same no matter if point is at start of region or end
  ;; FIXME same function but delete characters after and before point
  ;;       (inner vs outer (current) deletion)
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


;;; Toggles

(defun toggle-background-brightness ()
  "Toggle background brightness and reload theme."
  (interactive)
  (if (eq frame-background-mode 'dark)
	  (setq frame-background-mode 'light)
	(setq frame-background-mode 'dark))
  (update-frame-background-mode)
  (when custom-enabled-themes
	(load-theme (car custom-enabled-themes) t))
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
	(toggle-background-brightness)))

(defun toggle-indent-tabs-mode ()
  "Toggle variable `indent-tabs-mode' and re-tabify."
  (interactive)
  (if (eq indent-tabs-mode nil)
	  (progn
		(setq-local indent-tabs-mode t)
		(tabify (point-min) (point-max)))
	(setq-local indent-tabs-mode nil)
	(untabify (point-min) (point-max))))

(defun toggle-font-lock-mode ()
  "Toggle Font-Lock mode."
  (interactive)
  (if (eq global-font-lock-mode nil)
	  (global-font-lock-mode 1)
	(global-font-lock-mode 0)))

(defun toggle-line-numbers ()
  "Toggle display of line numbers.

Rotate between no line numbers, relative line numbers, and
absolute line numbers."
  (interactive)
  (cond
   ((eq display-line-numbers nil) (setq-local display-line-numbers 'relative))
   ((eq display-line-numbers 'relative) (setq-local display-line-numbers t))
   (t (setq-local display-line-numbers nil))))

(defun toggle-blink-cursor-mode ()
  "Toggle Blink Cursor mode."
  (interactive)
  (if (eq blink-cursor-mode nil)
	  (blink-cursor-mode 1)
	(blink-cursor-mode 0)))

(defun toggle-subword-mode ()
  "Toggle Subword mode."
  (interactive)
  (if (eq global-subword-mode nil)
	  (global-subword-mode 1)
	(global-subword-mode 0)))

(if (functionp 'pixel-scroll-mode)
	(defun toggle-pixel-scroll-mode ()
	  "Toggle Pixel-Scroll mode."
	  (interactive)
	  (if (eq pixel-scroll-mode nil)
		  (pixel-scroll-mode 1)
		(pixel-scroll-mode 0)))
  (defun toggle-pixel-scroll-mode ()
	"No op."
	(interactive)
	()))

(defun toggle-semantic-mode ()
  "Toggle Semantic mode."
  (interactive)
  (if (eq semantic-mode nil)
	  (semantic-mode 1)
	(semantic-mode 0)))

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
		(global-num3-mode 0)))
  (defun toggle-global-num3-mode ()
	"No op."
	(interactive)
	()))

(when (functionp 'pdf-tools-install)
  ;; TODO turn this into minor mode
  (defun toggle-presentation-mode ()
	"Toggle hiding any visual distractions."
	(interactive)
	(if (eq mode-line-format nil)
		(progn
		  (kill-local-variable 'mode-line-format)
		  ;; (kill-local-variable 'display-line-numbers)
		  (toggle-frame-fullscreen)
		  (when (eq major-mode 'pdf-view-mode)
			(local-unset-key (kbd "<mouse-1>"))
			(local-unset-key (kbd "<mouse-3>"))
			(local-unset-key [down-mouse-1])
			(local-unset-key [down-mouse-3])
			(pdf-misc-context-menu-minor-mode 1)
			(pdf-view-fit-page-to-window))
		  (kill-local-variable 'cursor-type)
		  (kill-local-variable 'echo-keystrokes)
		  (kill-local-variable 'inhibit-message)
		  ;; TODO why does `winner-undo' not work here?
		  )
	  (winner-save-unconditionally)
	  (setq-local mode-line-format nil)
	  ;; (setq-local display-line-numbers nil)
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
	  (set-window-point (selected-window) (point-min))
	  (setq-local cursor-type nil)
	  (setq-local echo-keystrokes 0) ; debatable
	  (setq-local inhibit-message t)))

  ;; Toggle presentation mode (C-c t p)
  (define-key my-toggle-map (kbd "p") 'toggle-presentation-mode))


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

;; Do not untabify before backspacing (default delete-backward-char)
(global-set-key (kbd "DEL") 'backward-delete-char)

;; Find file at point (C-c f)
(define-key mode-specific-map (kbd "f") 'find-file-at-point)

;; Repeat complex command (C-c z)
(define-key mode-specific-map (kbd "z") 'repeat-complex-command)

;; undo-only (C-c u)
(define-key mode-specific-map (kbd "u") 'undo-only)

;; Better expanding (default dabbrev-expand)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Better completion mapping (C-c n)
(unless (functionp 'company-complete)
  (define-key mode-specific-map (kbd "n") 'completion-at-point))

;; Like dt or df in Vim
(global-set-key (kbd "M-z") 'zap-up-to-char)
;; Zap to char (C-c x z)
(define-key mode-specific-map (kbd "z") 'zap-to-char)

;; Swap literal and regex isearch
;; (we then don't need/want (search-default-mode t))
(unless (functionp 'swiper)
  (global-set-key (kbd "C-s") 'isearch-forward-regexp))
(global-set-key (kbd "C-r")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Swap literal and regex query-replace
(global-set-key (kbd "M-%")   'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Add more behavior to M-SPC (default just-one-space)
(global-set-key (kbd "M-SPC") (lambda () (interactive) (cycle-spacing -1)))
;; Same on C-c x SPC for accessibility (M-SPC is an OS command)
(define-key my-extended-map (kbd "SPC")
  (lambda () (interactive) (cycle-spacing -1)))

;; Don't use arrow keys for indent-rigidly
(define-key indent-rigidly-map (kbd "C-b") 'indent-rigidly-left)
(define-key indent-rigidly-map (kbd "C-f") 'indent-rigidly-right)
(define-key indent-rigidly-map (kbd "M-b") 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map (kbd "M-f") 'indent-rigidly-right-to-tab-stop)

;; Indent using tabs or spaces (C-c t i)
(define-key my-toggle-map (kbd "i") 'toggle-indent-tabs-mode)

;; Toggle showing whitespace (C-c t w)
(define-key my-toggle-map (kbd "w") 'toggle-show-whitespace)

;; Toggle font-lock (C-c t c)
(define-key my-toggle-map (kbd "c") 'toggle-font-lock-mode)

;; Toggle line numbers (C-c t n)
(define-key my-toggle-map (kbd "n") 'toggle-line-numbers)

;; Toggle blinking cursor (C-c t B)
(define-key my-toggle-map (kbd "B") 'toggle-blink-cursor-mode)

;; Toggle subword movement (C-c t W)
(define-key my-toggle-map (kbd "W") 'toggle-subword-mode)

;; Toggle pixel-scrolling (C-c t P)
(define-key my-toggle-map (kbd "P") 'toggle-pixel-scroll-mode)

;; Toggle auto fill mode (C-c t F)
(define-key my-toggle-map (kbd "F") 'auto-fill-mode)

;; Toggle truncation of long lines (C-c t l)
(define-key my-toggle-map (kbd "l") 'toggle-truncate-lines)

;; Toggle Semantic mode (C-c t S)
(define-key my-toggle-map (kbd "S") 'toggle-semantic-mode)

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
(define-key my-window-map (kbd "o") 'swap-window-buffers)
(define-key my-window-map (kbd "O") 'window-swap-states)
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

;; View lossage (C-c x K)
(define-key my-extended-map (kbd "K") 'view-lossage)

;; Open Proced (C-c x p)
(define-key my-extended-map (kbd "p") 'proced)

;; Enter shell (C-c x S)
(define-key my-extended-map (kbd "S") 'shell)

;; Enter eshell (C-c x s)
(define-key my-extended-map (kbd "s") 'eshell)

;; Enter terminal/program (C-c x T)
(define-key my-extended-map (kbd "T") 'term)

;; Grep (C-c x G)
(define-key my-extended-map (kbd "G") 'grep)

;; Enter browser (C-c x w)
(define-key my-extended-map (kbd "w") 'eww)
;; Open file with browser (C-c x o)
(define-key my-extended-map (kbd "o") 'eww-open-file)

(when (and (featurep 'xwidget-internal)
		   (functionp 'xwidget-webkit-browse-url))
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
(put 'list-timers 'disabled nil)
(put 'erase-buffer 'disabled nil)

;;; init.el ends here
