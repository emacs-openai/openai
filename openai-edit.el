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

;;
;;; API

(cl-defun openai-edit-create ( input instruction callback
                               &key
                               (base-url openai-base-url)
                               (parameters openai-parameters)
                               (content-type "application/json")
                               (key openai-key)
                               org-id
                               (model "text-davinci-edit-001")
                               temperature
                               top-p
                               n)
  "Create a new edit for the provided input, instruction, and parameters.

The INPUT is text to use as a starting point for the edit. The INSTRUCTION
that tells the model how to edit the prompt.

The argument CALLBACK is executed after request is made.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY and ORG-ID are global
options; however, you can overwrite the value by passing it in.

The rest of the arguments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to MODEL, TEMPERATURE, TOP-P, and
N."
  (openai-request (concat base-url "/edits")
    :type "POST"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :data (openai--json-encode
           `(("model"       . ,model)
             ("input"       . ,input)
             ("instruction" . ,instruction)
             ("temperature" . ,temperature)
             ("top_p"       . ,top-p)
             ("n"           . ,n)))
    :parser 'json-read
    :complete (cl-function
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
