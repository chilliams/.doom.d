;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-moonlight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; misc tweaks
(set-default 'truncate-lines t)


;; Override how Doom handles buffers
(setq doom-real-buffer-functions '(doom-dired-buffer-p
                                   doom-special-buffer-p
                                   doom-non-file-visiting-buffer-p))
(setq doom-unreal-buffer-functions '(minibufferp))


;; MacOS tweaks
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)
(setq mac-pass-command-to-system nil)


;; shell
(setenv "EDITOR" "emacsclient")
(require 'shell)
(define-key shell-mode-map (kbd "M-r") #'counsel-shell-history)


;; Make async-shell-command do what I want.
(setq async-shell-command-buffer 'confirm-kill-process)
(defun wrap-async-shell-command (args)
  "Execute `async-shell-command' with a better buffer names"
  (let ((command (nth 0 args))
        (output-buffer (nth 1 args))
        (error-buffer (nth 2 args)))
    (list command
          (or output-buffer
              (concat "*" command " in " default-directory "*"))
          (or error-buffer
              (concat "* errors from " command " in " default-directory "*")))))
(advice-add 'async-shell-command :filter-args #'wrap-async-shell-command)


;; Useful functions
(defun curl (url)
  "Create tmp buffer with curl output"
  (interactive "sURL: ")
  (let ((buffer url))
    (with-output-to-temp-buffer buffer
      (shell-command (format "curl -s %s" url) buffer)
      (pop-to-buffer buffer))))

(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

(defun show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun copy-gerrit-link ()
  (interactive)
  (let ((file-name (or (buffer-file-name) list-buffers-directory))
        (alpha (getenv "ALPHA")))
    (when (not file-name)
      (error "Buffer not visiting a file"))
    (when (not (string-prefix-p alpha file-name))
      (error "File not in alpha"))
    (let ((link
           (replace-regexp-in-string
            (regexp-quote alpha)
            "https://gerrit.yext.com/plugins/gitiles/alpha/+/refs/heads/master/"
            file-name
            nil
            'literal))
          (line (number-to-string (line-number-at-pos))))
      (message (kill-new (concat link "#" line))))))


;; Use emacs as pager
(require 'emacs-pager)


;; bazel
(setq bazel-mode-buildifier-before-save t)
(add-hook 'bazel-mode-hook #'flymake-mode)


;; js
(setq js2-basic-offset 2)


;; java
(load "~/.doom.d/google-c-style")
(require 'google-c-style)
(add-hook 'java-mode-hook #'google-set-c-style)
