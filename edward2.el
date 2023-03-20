;;; edward2.el --- Replace edward with emacs -*- lexical-binding: t; -*-

;; Author: Chris Williams <cwilliams@yext.com>

;; Requires prodigy.el:
;; https://github.com/rejeep/prodigy.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Replace edward/tilt with Emacs

;; How to use:
;; - Load this file
;; - Call `edward2-initialize'
;; - Manage services with Prodigy
;; - Use `list-processes' when haproxy misbehaves

;;; Code:

(provide 'edward2)

(require 'f)
(require 'prodigy)

(defun edward2--read-config (filename)
  (json-parse-string
   (f-read-text (expand-file-name
                 filename
                 (substitute-in-file-name "$ALPHA")))
   :object-type 'plist))

(defun edward2--load-services (filename)
  (let* ((root-config (edward2--read-config filename))
         (imports (plist-get root-config :imports))
         (services (mapcan #'edward2--load-services imports)))
    (append (plist-get root-config :services) services)))

(defun edward2--construct-command (commands)
  (let* ((build (plist-get commands :build))
         (launch (plist-get commands :launch)))
    (if build
        (concat build " && " launch)
      launch)))

(defun edward2--define-service (prefix plist)
  (let* ((name (plist-get plist :name))
         (backends (plist-get plist :backends))
         (commands (plist-get plist :commands))
         (ready-message (plist-get (plist-get plist :launch_checks) :log_text)))
    (when backends
      (seq-doseq (backend backends)
        (edward2--define-service name backend)))
    (when commands
      (let* ((command (edward2--construct-command commands)))
        (prodigy-define-service
          :name (if prefix (concat prefix ":" name) name)
          :cwd (getenv "ALPHA")
          :command "bash"
          :args (list "-c" command)
          :ready-message ready-message)))))

(defun edward2-initialize ()
  "(re)load services from edward.json and open prodigy"
  (interactive)
  (setq prodigy-services '())
  (let* ((services (edward2--load-services "edward.json")))
    (seq-doseq (service services)
      (edward2--define-service nil service)))
  (prodigy)
  (revert-buffer))

;;; edward2.el ends here
