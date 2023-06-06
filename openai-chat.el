;;; openai-chat.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Create chat with OpenAI API.
;;
;; See https://platform.openai.com/docs/api-reference/chat
;;

;;; Code:

(require 'openai)

;;
;;; API

;;;###autoload
(cl-defun openai-chat ( messages callback
                        &key
                        (base-url openai-base-url)
                        (parameters openai-parameters)
                        (content-type "application/json")
                        (key openai-key)
                        org-id
                        (model "gpt-3.5-turbo")
                        temperature
                        top-p
                        n
                        stream
                        stop
                        max-tokens
                        presence-penalty
                        frequency-penalty
                        logit-bias
                        (user openai-user))
  "Send chat request.

Arguments MESSAGES and CALLBACK are required for this type of request.
MESSAGES is the conversation data.  CALLBACK is the execuation after request is
made.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY, ORG-ID and USER are global
options; however, you can overwrite the value by passing it in.

The rest of the arugments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to MODEL,  TEMPERATURE, TOP-P, N,
STREAM, STOP, MAX-TOKENS, PRESENCE-PENALTY, FREQUENCY-PENALTY, and LOGIT-BIAS."
  (openai-request (concat base-url "/chat/completions")
    :type "POST"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :data (openai--json-encode
           `(("model"             . ,model)
             ("messages"          . ,messages)
             ("temperature"       . ,temperature)
             ("top_p"             . ,top-p)
             ("n"                 . ,n)
             ("stream"            . ,stream)
             ("stop"              . ,stop)
             ("max_tokens"        . ,max-tokens)
             ("presence_penalty"  . ,presence-penalty)
             ("frequency_penalty" . ,frequency-penalty)
             ("logit_bias"        . ,logit-bias)
             ("user"              . ,user)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;
;;; Application

(defcustom openai-chat-max-tokens 4000
  "The maximum number of tokens to generate in the completion."
  :type 'integer
  :group 'openai)

(defcustom openai-chat-temperature 1.0
  "What sampling temperature to use."
  :type 'number
  :group 'openai)

;;;###autoload
(defun openai-chat-say ()
  "Start making a conversation to OpenAI.

This is a ping pong message, so you will only get one response."
  (interactive)
  (if-let* ((user (read-string "What is your name? " "user"))
            (say  (read-string "Start the conversation: ")))
      (openai-chat `[(("role"    . ,user)
                      ("content" . ,say))]
                   (lambda (data)
                     (let ((choices (let-alist data .choices)))
                       (mapc (lambda (choice)
                               (let-alist choice
                                 (let-alist .message
                                   (message "%s: %s" .role (string-trim .content)))))
                             choices)))
                   :max-tokens openai-chat-max-tokens
                   :temperature openai-chat-temperature
                   :user (unless (string= user "user") user))
    (user-error "Abort, cancel chat operation")))

(provide 'openai-chat)
;;; openai-chat.el ends here
