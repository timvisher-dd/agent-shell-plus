;;; agent-shell-experimental.el --- Experimental ACP features -*- lexical-binding: t; -*-

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
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; Experimental ACP features for agent-shell.
;;
;; session/pushPrompt: Server-initiated prompt push.  The server sends
;; a request to the client, followed by session/update notifications,
;; concluded by an end_of_session_push_prompt notification.  The client
;; then responds to the original request.

;;; Code:

(require 'map)
(eval-when-compile
  (require 'cl-lib))

(declare-function acp-send-response "acp")
(declare-function acp-make-error "acp")

(cl-defun agent-shell-experimental--on-push-prompt-request (&key state acp-request)
  "Handle an incoming session/pushPrompt ACP-REQUEST with STATE.

The server pushes a prompt to the client, followed by session/update
notifications.  The client sends the response after receiving an
end_of_session_push_prompt notification."
  (let ((request (agent-shell-experimental--normalize-request acp-request)))
    ;; Track as active so notifications are not treated as stale.
    (unless (assq :active-requests state)
      (nconc state (list (cons :active-requests nil))))
    (map-put! state :active-requests
              (cons request (map-elt state :active-requests))))
  (map-put! state :last-entry-type "session/pushPrompt"))

(cl-defun agent-shell-experimental--on-end-of-push-prompt (&key state on-finished)
  "Handle end_of_session_push_prompt notification with STATE.

Finds the active push prompt request, sends the response, and
removes it from active requests.  Calls ON-FINISHED when done
to allow the caller to finalize (e.g. display a new shell prompt)."
  (when-let ((push-request (seq-find (lambda (r)
                                       (equal (map-elt r :method) "session/pushPrompt"))
                                     (map-elt state :active-requests))))
    (acp-send-response
     :client (map-elt state :client)
     :response (agent-shell-experimental--make-push-prompt-response
                :request-id (map-elt push-request :id)))
    (map-put! state :active-requests
              (seq-remove (lambda (r)
                            (equal (map-elt r :method) "session/pushPrompt"))
                          (map-elt state :active-requests))))
  (map-put! state :last-entry-type "end_of_session_push_prompt")
  (when on-finished
    (funcall on-finished)))

(cl-defun agent-shell-experimental--make-push-prompt-response (&key request-id error)
  "Instantiate a \"session/pushPrompt\" response.

REQUEST-ID is the ID of the incoming server request this responds to.
ERROR is an optional error object if the push prompt was rejected."
  (unless request-id
    (error ":request-id is required"))
  (if error
      `((:request-id . ,request-id)
        (:error . ,error))
    `((:request-id . ,request-id)
      (:result . nil))))

(defun agent-shell-experimental--methods ()
  "Return the list of experimental methods that replay session notifications."
  '("session/pushPrompt"))

(defun agent-shell-experimental--normalize-request (request)
  "Normalize REQUEST from JSON symbol keys to keyword keys.

Incoming JSON-parsed requests use symbol keys (e.g. \\='method),
while internal request objects use keyword keys (e.g. :method).
This function converts the known keys that `acp--request-sender'
manually translates on the way out.

Example:

  \\='((method . \"session/pushPrompt\")
    (id . 3)
    (params . ((prompt . [...]))))

becomes:

  \\='((:method . \"session/pushPrompt\")
    (:id . 3)
    (:params . ((prompt . [...]))))"
  (seq-map (lambda (pair)
             (let ((key (car pair)))
               (cons (pcase key
                       ('method :method)
                       ('params :params)
                       ('id :id)
                       ('jsonrpc :jsonrpc)
                       (_ key))
                     (cdr pair))))
           request))

(provide 'agent-shell-experimental)

;;; agent-shell-experimental.el ends here
