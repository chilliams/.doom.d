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
(setq doom-font (font-spec :family "Monaco" :size 14 :weight 'normal)
      doom-variable-pitch-font nil)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

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
(require 'dired-x) ;; Make C-x C-j work on startup
(setq-default truncate-lines nil)

;; MacOS tweaks
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)
(setq mac-pass-command-to-system nil)
(global-set-key (kbd "C-`") 'other-frame)

;; shell
;; (setenv "EDITOR" "emacsclient")
(require 'shell)

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
        (progn
          (kill-new file-name)
          (message file-name))
      (error "Buffer not visiting a file"))))

(require 'magit-git)
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
            (concat "https://gerrit.yext.com/plugins/gitiles/alpha/+/"
                    (magit-git-string "merge-base" "origin/master" "HEAD"))
            file-name
            nil
            'literal))
          (line (number-to-string (line-number-at-pos))))
      (message (kill-new (concat link "#" line))))))


;; Use emacs as pager
(require 'emacs-pager)


;; bazel
(setq bazel-mode-buildifier-before-save nil)
(add-hook 'bazel-mode-hook #'flymake-mode)


;; indentation
(setq css-indent-offset 2)
(setq js-indent-level 2)

;; java
(load "~/.doom.d/google-c-style")
(require 'google-c-style)
(add-hook 'java-mode-hook #'google-set-c-style)

(defun jump-to-test-dir ()
  "Jump to the test version of the current directory."
  (interactive)
  (dired (replace-regexp-in-string "/src/" "/test/" default-directory)))

(defun jump-to-src-dir ()
  "Jump to the src version of the current directory."
  (interactive)
  (dired (replace-regexp-in-string "/test/" "/src/" default-directory)))

(defun toggle-src-test-dir ()
  "Jump between src and test dirs."
  (interactive)
  (cond ((string-match "/src/" default-directory) (jump-to-test-dir))
        ((string-match "/test/" default-directory) (jump-to-src-dir))
        (t (print "Not in src or test directory"))))

(setq lsp-java-vmargs '("-XX:HotswapAgent=core"
                        "-XX:GCTimeRatio=4"
                        "-XX:AdaptiveSizePolicyWeight=90"
                        "-Dsun.zip.disableMemoryMapping=true"
                        "-Xmx1G"
                        "-Xms100m"))

;; work
(defun yext-java-format ()
  (interactive)
  (shell-command
   (concat "$ALPHA/tools/java/javafmt/format-java-changes.sh " buffer-file-name))
  (revert-buffer t t))

(defun yext-java-format-after-save ()
  (interactive)
  (when (and  (eq major-mode 'java-mode)
              (getenv "ALPHA"))
    (yext-java-format)))

(add-hook 'after-save-hook 'yext-java-format-after-save)

(defun set-region-writeable (begin end)
  "Removes the read-only text property from the marked region."
  (interactive "r")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bazel-.*\\'"))

(use-package! company-glsl  ; for `glsl-mode'
  :when (featurep! :completion company)
  :after glsl-mode
  :config (set-company-backend! 'glsl-mode 'company-glsl))

;; patch flycheck scss-stylelint checker
;; https://github.com/flycheck/flycheck/issues/1912
(flycheck-define-checker scss-stylelint
  "A SCSS syntax and style checker using stylelint.

See URL `http://stylelint.io/'."
  :command ("stylelint"
            (eval flycheck-stylelint-args)
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelintrc))
  :standard-input t
  :error-parser flycheck-parse-stylelint
  :predicate flycheck-buffer-nonempty-p
  :modes (scss-mode))

(grep-apply-setting 'grep-command "rg --vimgrep --no-column --max-columns 200 --max-filesize 1M --sort-files --ignore-case ")
;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))

(setq +format-on-save-enabled-modes '(go-mode))

;; (load "~/.doom.d/edward2")

(map! :map shell-mode-map "M-r" #'consult-history)

(after! tide
  (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) 'append))

(setq cua-enable-cua-keys nil)  ; enable only CUA's rectangle selections
(cua-mode t)

;; Someone at work decided to use the .lib extension 🤨
;; BTW, `apropos-value' is so cool 😎
(delete ".lib" completion-ignored-extensions)
(delete ".lib" dired-omit-extensions)

(setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
(setq magit-log-margin-show-committer-date t)

(setq auto-mode-alist (delete '("\\.tsx\\'" . typescript-tsx-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))

(keymap-global-set "<remap> <async-shell-command>"
                   #'with-editor-async-shell-command)
(keymap-global-set "<remap> <shell-command>"
                   #'with-editor-shell-command)

(shell-command-with-editor-mode t)

(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)
(add-hook 'vterm-mode-hook  'with-editor-export-editor)

(setq
 gptel-model 'gemini-2.5-pro
 gptel-backend (gptel-make-gemini "Gemini"
                 :key (getenv "GEMINI_API_KEY")
                 :stream t))
