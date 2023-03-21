;;; openai-completion.el --- Create completion with OpenAI API  -*- lexical-binding: t; -*-

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
;; Create completion with OpenAI API.
;;
;; See https://beta.openai.com/docs/api-reference/completions
;;

;;; Code:

(require 'openai)

;;
;;; API

;;;###autoload
(cl-defun openai-completion ( prompt callback
                              &key
                              (key openai-key)
                              (model "text-davinci-003")
                              suffix
                              max-tokens
                              temperature
                              top-p
                              n
                              stream
                              logprobs
                              echo
                              stop
                              presence-penalty
                              frequency-penalty
                              best-of
                              logit-bias
                              (user openai-user))
  "Send completion request.

Arguments PROMPT and CALLBACK are required for this type of request.  PROMPT is
either the question or instruction to OpenAI.  CALLBACK is the execuation after
request is made.

Arguments KEY and USER are global options; however, you can overwrite the value
by passing it in.

The rest of the arugments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to MODEL, SUFFIX, MAX-TOKENS,
TEMPERATURE, TOP-P, N, STREAM, LOGPROBS, ECHO, STOP, PRESENCE-PENALTY,
FREQUENCY-PENALTY, BEST-OF, and LOGIT-BIAS."
  (openai-request "https://api.openai.com/v1/completions"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " key)))
    :data (openai--json-encode
           `(("model"             . ,model)
             ("prompt"            . ,prompt)
             ("suffix"            . ,suffix)
             ("max_tokens"        . ,max-tokens)
             ("temperature"       . ,temperature)
             ("top-p"             . ,top-p)
             ("n"                 . ,n)
             ("stream"            . ,stream)
             ("logprobs"          . ,logprobs)
             ("echo"              . ,echo)
             ("stop"              . ,stop)
             ("presence_penalty"  . ,presence-penalty)
             ("frequency_penalty" . ,frequency-penalty)
             ("best_of"           . ,best-of)
             ("logit_bias"        . ,logit-bias)
             ("user"              . ,user)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;
;;; Application

(defcustom openai-completion-max-tokens 4000
  "The maximum number of tokens to generate in the completion."
  :type 'integer
  :group 'openai)

(defcustom openai-completion-temperature 1.0
  "What sampling temperature to use."
  :type 'number
  :group 'openai)

;;;###autoload
(defun openai-completion-select-insert (start end)
  "Send the region to OpenAI and insert the result to the next paragraph.

START and END are selected region boundaries."
  (interactive "r")
  (let ((initial-buffer (current-buffer)))
    (openai-completion
     (string-trim (buffer-substring-no-properties start end))
     (lambda (data)
       (openai--with-buffer initial-buffer
         (openai--pop-to-buffer initial-buffer)  ; make sure to stay in that buffer
         (let* ((choices (openai--data-choices data))
                (result (openai--get-choice choices))
                original-point)
           (when (string-empty-p result)
             (user-error "No response, please try again"))
           (when (= end (point-max))
             (save-excursion
               (goto-char end)
               (insert "\n")))
           (goto-char end)
           (forward-paragraph)
           (setq original-point (point))
           (insert "\n" (string-trim result) "\n")
           (fill-region original-point (point))
           ;; Highlight the region!
           (call-interactively #'set-mark-command)
           (goto-char (1+ original-point)))))
     :max-tokens openai-completion-max-tokens
     :temperature openai-completion-temperature)))

;;;###autoload
(defun openai-completion-buffer-insert ()
  "Send the entire buffer to OpenAI and insert the result to the end of buffer."
  (interactive)
  (openai-completion-select-insert (point-min) (point-max)))

(provide 'openai-completion)
;;; openai-completion.el ends here
