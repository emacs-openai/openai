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
                        (key openai-key)
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

Arguments MESSAGES and CALLBACK are required for this type of request.  MESSAGES
is the conversation data.  CALLBACK is the execuation after request is made.

Arguments KEY and USER are global options; however, you can overwrite the value
by passing it in.

The rest of the arugments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to MODEL,  TEMPERATURE, TOP-P, N,
STREAM, STOP, MAX-TOKENS, PRESENCE-PENALTY, FREQUENCY-PENALTY, and LOGIT-BIAS."
  (openai-request "https://api.openai.com/v1/chat/completions"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " key)))
    :data (openai--json-encode
           `(("model"             . ,model)
             ("messages"          . ,messages)
             ("temperature"       . ,temperature)
             ("top-p"             . ,top-p)
             ("n"                 . ,n)
             ("stream"            . ,stream)
             ("stop"              . ,stop)
             ("max_tokens"        . ,max-tokens)
             ("presence_penalty"  . ,presence-penalty)
             ("frequency_penalty" . ,frequency-penalty)
             ("logit_bias"        . ,logit-bias)
             ("user"              . ,user)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(provide 'openai-chat)
;;; openai-chat.el ends here
