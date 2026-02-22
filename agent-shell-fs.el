;;; agent-shell-fs.el --- Filesystem request handlers for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Filesystem request handlers for agent-shell.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'acp)
(require 'map)
(require 'subr-x)
(require 'agent-shell-helpers)

;; Declare as special so byte-compilation doesn't turn `let' bindings into
;; lexical bindings (which would not affect `auto-insert' behavior).
(defvar auto-insert)

(defcustom agent-shell-write-inhibit-minor-modes '(aggressive-indent-mode)
  "List of minor mode commands to inhibit during `fs/write_text_file' edits.

Each element is a minor mode command symbol, such as
`aggressive-indent-mode'.

Agent Shell disables any listed modes that are enabled in the target
buffer before applying `fs/write_text_file' edits, and then restores
them.

Modes whose variables are not buffer-local in the target buffer (for
example, globalized minor modes) are ignored."
  :type '(repeat symbol)
  :group 'agent-shell)

(cl-defun agent-shell--extract-buffer-text (&key buffer line limit)
  "Extract text from BUFFER starting from LINE with optional LIMIT.
If the buffer's file has changed, prompt the user to reload it."
  (with-current-buffer buffer
    (when (and (buffer-file-name)
               (not (verify-visited-file-modtime))
               (y-or-n-p (format "%s has changed on file.  Reload? "
                                 (buffer-name))))
      (revert-buffer t nil nil))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (and line (< 1 line))
          ;; Seems odd to use forward-line but
          ;; that's what `goto-line' recommends.
          (forward-line (1- line)))
        (let ((start (point)))
          (if limit
              ;; Seems odd to use forward-line but
              ;; that's what `goto-line' recommends.
              (forward-line limit)
            (goto-char (point-max)))
          (buffer-substring-no-properties start (point)))))))

(cl-defun agent-shell--on-fs-read-text-file-request (&key state request)
  "Handle fs/read_text_file REQUEST with STATE."
  (let-alist request
    (condition-case err
        (let* ((path (agent-shell--resolve-path .params.path))
               (line (or .params.line 1))
               (limit .params.limit)
               (existing-buffer (find-buffer-visiting path))
               (content (if existing-buffer
                            ;; Read from open buffer (includes unsaved changes)
                            (agent-shell--extract-buffer-text :buffer existing-buffer :line line :limit limit)
                          ;; No open buffer, read from file
                          (with-temp-buffer
                            (insert-file-contents path)
                            (agent-shell--extract-buffer-text :buffer (current-buffer) :line line :limit limit)))))
          (acp-send-response
           :client (map-elt state :client)
           :response (acp-make-fs-read-text-file-response
                      :request-id .id
                      :content content)))
      (quit
       ;; Handle C-g interrupts during file read prompts
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-read-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message "Operation cancelled by user"))))
      (file-missing
       ;; File doesn't exist - return RESOURCE_NOT_FOUND (-32002).
       ;; This allows agents to distinguish "file not found" from actual errors.
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-read-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32002
                           :message "Resource not found"
                           :data `((path . ,(nth 3 err)))))))
      (error
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-read-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message (error-message-string err))))))))

(defun agent-shell--call-with-inhibited-minor-modes (modes thunk)
  "Call THUNK with MODES temporarily disabled in the current buffer.

Disable each mode in MODES that is enabled in the current buffer and has
a buffer-local mode variable.  Re-enable any modes disabled by this
function before returning."
  (let (disabled)
    (unwind-protect
        (progn
          (dolist (mode modes)
            (when (and (symbolp mode)
                       (fboundp mode)
                       (boundp mode)
                       (symbol-value mode)
                       (local-variable-p mode))
              (funcall mode -1)
              (push mode disabled)))
          (funcall thunk))
      (dolist (mode disabled)
        (funcall mode 1)))))

(cl-defun agent-shell--on-fs-write-text-file-request (&key state request)
  "Handle fs/write_text_file REQUEST with STATE."
  (let-alist request
    (condition-case err
        (let* ((path (agent-shell--resolve-path .params.path))
               (content .params.content)
               (dir (file-name-directory path))
               (buffer (or (find-buffer-visiting path)
                           ;; Prevent auto-insert-mode
                           ;; See issue #170
                           (let ((auto-insert nil))
                             (find-file-noselect path)))))
          (when (and dir (not (file-exists-p dir)))
            (make-directory dir t))
          (with-temp-buffer
            (insert content)
            (let ((content-buffer (current-buffer))
                  (inhibit-read-only t))
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  ;; Set a time-out to prevent locking up on large files
                  ;; https://github.com/xenodium/agent-shell/issues/168
                  (agent-shell--call-with-inhibited-minor-modes
                   agent-shell-write-inhibit-minor-modes
                   (lambda ()
                     (replace-buffer-contents content-buffer 1.0)))
                  (basic-save-buffer)))))
          (agent-shell--emit-event
           :event 'file-write
           :data (list (cons :path path)
                       (cons :content content)))
          (acp-send-response
           :client (map-elt state :client)
           :response (acp-make-fs-write-text-file-response
                      :request-id .id)))
      (quit
       ;; Handle C-g interrupts during file save prompts
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-write-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message "Operation cancelled by user"))))
      (error
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-write-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message (error-message-string err))))))))

(provide 'agent-shell-fs)

;;; agent-shell-fs.el ends here
