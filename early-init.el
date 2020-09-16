;; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-

(setq gc-cons-threshold 402653184
	  gc-cons-percentage 0.6)

;; Prefer more recent files when loading.
(setq load-prefer-newer t)

(setq package-quickstart t)
;; (setq package-enable-at-startup nil)
;; (setq package-load-list '(
;; 						  (package-to-disable nil)
;; 						  all))

;;; early-init.el ends here
