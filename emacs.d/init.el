;;; Basic

(tool-bar-mode 0)
(setq ring-bell-function 'ignore)

(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;;; Face

(add-to-list 'default-frame-alist '(background-color . "#F2F2F2"))

(set-face-foreground 'font-lock-comment-face "#6C737C")
;; (set-face-foreground 'font-lock-string-face "#102F5F")
;; (set-face-foreground 'font-lock-string-face "#72972C")
(set-face-foreground 'font-lock-string-face "#467F36")
(set-face-foreground 'font-lock-constant-face "#225DBE")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#C7474E")
(set-face-attribute 'font-lock-function-name-face nil :foreground "#225DBE")

;;; For 13" MacBook Pro
(setq default-frame-alist
      '((width . 200)
        (height . 58)))

(show-paren-mode t)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "M-r"))

(global-set-key "\n" 'newline)
(global-set-key "\r" 'newline-and-indent)
(global-set-key [backspace] 'delete-backward-char)
(global-set-key [(control h)] 'delete-backward-char)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

(add-to-list 'load-path "~/.emacs.d/plugins")

(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   (quote
    (go-mode haskell-mode racket-mode projectile paredit cider)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))


(setq default-directory (concat (getenv "HOME") "/Documents/GitHub"))





;;; Packages

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/")
             t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             t)
(package-initialize)

(defvar required-packages '(racket-mode))

(dolist (package required-packages)
  (unless (package-installed-p package)
    (package-install package)))





;;; Lisp

(require 'parenface)
(set-face-foreground 'paren-face "DimGray")

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)


(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond
     ((string-match "[[{(]" next-char) (forward-sexp 1))
     ((string-match "[\]})]" prev-char) (backward-sexp 1))
     (t (self-insert-command (or arg 1))))))

(global-set-key "%" 'match-paren)



;;; Scheme

(add-to-list 'auto-mode-alist '("\\.sld$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sps$" . scheme-mode))


(require 'cmuscheme)
(setq scheme-program-name "/usr/local/bin/scheme")


;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))


(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))


(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))


(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))


(setq scheme-indent-function-1-list
      '(library
        define-library
        foreign-procedure
        module
        when
        unless
        match
        pmatch
        parameterize
        make-parameter
        let-values
        let*-values
        letrec*
        and-let*
        match-let
        let1
        match-let1
        spawn
        guard
        handle-exceptions
        eval-when
        dotimes
        ;; ftype
        make-ftype-pointer

        ;; I/O
        call-with-port
        open-file-input-port
        open-file-output-port

        ;; Personal
        argument-case

        call-with-input-file
        call-with-input-string))

(add-hook 'scheme-mode-hook
  (lambda ()
    (paredit-mode 1)
    (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "s-r") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)
    (mapc (lambda (x) (put x 'scheme-indent-function 1))
          scheme-indent-function-1-list)
    (put 'trace-lambda 'scheme-indent-function 2)
    (put 'syntax-case 'scheme-indent-function 2)
    ;;(put 'dotimes 'scheme-indent-function 2)
    (put 'with-syntax 'scheme-indent-function 3)
    (put 'with-implicit 'scheme-indent-function 3)
    (put 'receive 'scheme-indent-function 3)
    ;;(put 'module 'scheme-indent-function 'scheme-module-indent)
    ))



;;; Geiser

;; (load-file "~/local/geiser/elisp/geiser.el")

;; (setq geiser-implementations-alist
;;       '(((regexp "\\.ss$") chez)
;;         ((regexp "\\.sls$") chez)
;;         ;;((regexp "\\.scm$") chicken)
;;         ((regexp "\\.rkt$") racket)))


;; (eval-after-load "geiser-impl"
;;   '(add-to-list 'geiser-implementations-alist
;;                 '((dir "/usr/local/bin") chez)))


;; (setq geiser-chez-binary "/usr/local/bin/scheme")
;; (setq geiser-guile-binary "/usr/local/bin/guile")
;; (setq geiser-racket-binary "/Applications/Racket v7.0/bin/racket")
;; (setq geiser-chicken-binary "/usr/local/bin/csi")




;;; Racket

(add-hook 'racket-mode-hook
  (lambda ()
    (paredit-mode 1)))



;;; Common Lisp

(add-to-list 'load-path "~/lisp/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(add-hook 'lisp-mode-hook
  (lambda ()
    (paredit-mode 1)))



;;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (paredit-mode 1)
    (put 'add-hook 'lisp-indent-function 1)))





;;; OCaml
(load "/Users/ziv/.opam/default/share/emacs/site-lisp/tuareg-site-file")





;;; C

(setq c-default-style "linux")
(setq c-basic-offset 4)





;; (require 'smooth-scrolling)
;; (smooth-scrolling-mode 1)


(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)





(add-hook 'before-save-hook
  (lambda ()
    (interactive)
    (delete-trailing-whitespace)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
