;;; early-init.el --- Early initialization options  -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Personal customization that has to be run at the early initialization step.
;; Mostly performance-related improvements.

;;; Code:

;; Less GC during initialization (reset in init.el)
(setq gc-cons-threshold 536870912		; 2^29 or 512 MiB
	  gc-cons-percentage 0.6)

;; Prefer more recent files when loading.
(setq load-prefer-newer t)

;; Improve native compilation options.
(defun get-native-march ()
  "Return the native CPU architecture string.
This is the one chosen by GCC when specifying `march=native`."
  (string-trim-right
   (shell-command-to-string
	(concat "gcc -Q -march=native --help=target "
			"| sed -n 's/^\\s*-march=\\s*\\(.*\\)$/\\1/p'"))))

(when (and (fboundp 'native-comp-available-p)
		   (native-comp-available-p))
  (setq native-comp-compiler-options
		(list
		 "-O2"
		 ;; `-march=native` is not supported by libgccjit.
		 (concat "-march=" (get-native-march))
		 ;; Currently not available in libgccjit.
		 ;; "-pipe"
		 )))

(setq package-quickstart t)
;; (setq package-enable-at-startup nil)
;; (setq package-load-list '(
;; 						  (package-to-disable nil)
;; 						  all))

;;; early-init.el ends here
