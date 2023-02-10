;;; openai-edit.el --- Edit presented documents with OpenAI API  -*- lexical-binding: t; -*-

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
;; Given a prompt and an instruction, the model will return an edited version of
;; the prompt.
;;
;; See https://platform.openai.com/docs/api-reference/edits
;;

;;; Code:

(require 'openai)

(defcustom openai-edit-model "text-davinci-edit-001"
  "ID of the model to use.

You can use the `text-davinci-edit-001' or `code-davinci-edit-001' model with
this endpoint."
  :type 'string
  :group 'openai)

(defcustom openai-edit-n 1
  "How many edits to generate for the input and instruction."
  :type 'integer
  :group 'openai)

(defcustom openai-edit-temperature 1
  "What sampling temperature to use, between 0 and 2.  Higher values like 0.8
will make the output more random, while lower values like 0.2 will make it more
focused and deterministic.

We generally recommend altering this or `top_p' but not both."
  :type 'integer
  :group 'openai)

(defcustom openai-edit-top-p 1.0
  "An alternative to sampling with temperature, called nucleus sampling, where
the model considers the results of the tokens with top_p probability mass.
So 0.1 means only the tokens comprising the top 10% probability mass are
considered.

We generally recommend altering this or `temperature' but not both."
  :type 'number
  :group 'openai)

;;
;;; API

(defun openai-edit-create (input instruction callback)
  "Creates a new edit for the provided input, instruction, and parameters.

The INPUT is text to use as a starting point for the edit.  The INSTRUCTION that
tells the model how to edit the prompt.

The argument CALLBACK is execuated after request is made."
  (openai-request "https://api.openai.com/v1/edits"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :data (json-encode
           `(("model"       . ,openai-edit-model)
             ("input"       . ,input)
             ("instruction" . ,instruction)
             ("temperature" . ,openai-edit-temperature)
             ("top_p"       . ,openai-edit-top-p)
             ("n"           . ,openai-edit-n)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

;;
;;; Application

;;;###autoload
(defun openai-edit-prompt ()
  "Prompt to ask for edited version."
  (interactive)
  (let ((input (read-string "Input: "))
        (instruction (read-string "Instruction: ")))
    (openai-edit-create input instruction
                        (lambda (data)
                          (when-let* ((choices (openai--data-choices data))
                                      (result (openai--get-choice choices)))
                            (kill-new result)
                            (message "The result is pasted into kill ring"))))))

(provide 'openai-edit)
;;; openai-edit.el ends here
