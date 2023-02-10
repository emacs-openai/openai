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

(defcustom openai-completon-model "text-davinci-003"
  "ID of the model to use.

You can use the List models API to see all of your available models."
  :type 'string
  :group 'openai)

(defcustom openai-completon-suffix nil
  "The suffix that comes after a completion of inserted text."
  :type 'string
  :group 'openai)

(defcustom openai-completon-max-tokens 4000
  "The maximum number of tokens to generate in the completion.

The token count of your prompt plus max_tokens cannot exceed the model's context
length.  Most models have a context length of 2048 tokens (except for the newest
models, which support 4096)."
  :type 'integer
  :group 'openai)

(defcustom openai-completon-temperature 1.0
  "What sampling temperature to use.

Higher values means the model will take more risks.  Try 0.9 for more creative
applications, and 0 (argmax sampling) for ones with a well-defined answer."
  :type 'number
  :group 'openai)

(defcustom openai-completon-top-p 1.0
  "An alternative to sampling with temperature, called nucleus sampling, where
the model considers the results of the tokens with top_p probability mass.
So 0.1 means only the tokens comprising the top 10% probability mass are
considered.

We generally recommend altering this or `temperature' but not both."
  :type 'number
  :group 'openai)

(defcustom openai-completon-n 1
  "How many completions to generate for each prompt."
  :type 'integer
  :group 'openai)

(defcustom openai-completon-stream nil
  "Whether to stream back partial progress.

If set, tokens will be sent as data-only server-sent events as they become
available, with the stream terminated by a data: [DONE] message."
  :type 'boolean
  :group 'openai)

(defcustom openai-completon-logprobs nil
  "Include the log probabilities on the logprobs most likely tokens, as well the
chosen tokens.  For example, if logprobs is 5, the API will return a list of the
5 most likely tokens.  The API will always return the logprob of the sampled
token, so there may be up to logprobs+1 elements in the response.

The maximum value for logprobs is 5."
  :type 'integer
  :group 'openai)

(defcustom openai-completon-echo nil
  "Echo back the prompt in addition to the completion."
  :type 'boolean
  :group 'openai)

(defcustom openai-completon-stop nil
  "Up to 4 sequences where the API will stop generating further tokens.
The returned text will not contain the stop sequence."
  :type 'string
  :group 'openai)

(defcustom openai-completon-presence-penalty 0
  "Number between -2.0 and 2.0. Positive values penalize new tokens based on
whether they appear in the text so far, increasing the model's likelihood to
talk about new topics."
  :type 'number
  :group 'openai)

(defcustom openai-completon-frequency-penalty 0
  "Number between -2.0 and 2.0.

Positive values penalize new tokens based on their existing frequency in the
text so far, decreasing the model's likelihood to repeat the same line verbatim."
  :type 'number
  :group 'openai)

(defcustom openai-completon-best-of 1
  "Generates best_of completions server-side and returns the \"best\" (the one
with the highest log probability per token).  Results cannot be streamed.

When used with `n', `best_of' controls the number of candidate completions and
`n' specifies how many to return – `best_of' must be greater than `n'."
  :type 'integer
  :group 'openai)

(defcustom openai-completon-logit-bias nil
  "Modify the likelihood of specified tokens appearing in the completion."
  :type 'list
  :group 'openai)

;;
;;; API

;;;###autoload
(defun openai-completion (query callback)
  "Query OpenAI with QUERY.

Argument CALLBACK is a function received one argument which is the JSON data."
  (openai-request "https://api.openai.com/v1/completions"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :data (json-encode
           `(("model"             . ,openai-completon-model)
             ("prompt"            . ,query)
             ("suffix"            . ,openai-completon-suffix)
             ("max_tokens"        . ,openai-completon-max-tokens)
             ("temperature"       . ,openai-completon-temperature)
             ("top_p"             . ,openai-completon-top-p)
             ("n"                 . ,openai-completon-n)
             ;;("stream"            . ,(if openai-completon-stream "true" "false"))
             ("logprobs"          . ,openai-completon-logprobs)
             ;;("echo"              . ,(if openai-completon-echo "true" "false"))
             ;;("stop"              . ,openai-completon-stop)
             ;;("presence_penalty"  . ,openai-completon-presence-penalty)
             ;;("frequency_penalty" . ,openai-completon-frequency-penalty)
             ;;("best_of"           . ,openai-completon-best-of)
             ;;("logit_bias"        . ,(if (listp openai-completon-logit-bias)
             ;;                            (json-encode openai-completon-logit-bias)
             ;;                          openai-completon-logit-bias))
             ("user"              . ,openai-user)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

;;
;;; Application

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
           (goto-char (1+ original-point))))))))

;;;###autoload
(defun openai-completion-buffer-insert ()
  "Send the entire buffer to OpenAI and insert the result to the end of buffer."
  (interactive)
  (openai-completion-select-insert (point-min) (point-max)))

(provide 'openai-completion)
;;; openai-completion.el ends here
