;;; early-init.el --- Early initialization options  -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Personal customization that has to be run at the early initialization step.
;; Mostly performance-related improvements.

;;; Code:

;; Less GC during initialization (reset in init.el)
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
