;;; openai-model.el --- List and describe the various models available in the OpenAI API  -*- lexical-binding: t; -*-

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
;; List and describe the various models available in the OpenAI API.
;;

;;; Code:

(require 'openai)

;;
;;; API

(defun openai-models (callback)
  "Return models data and execute the CALLBACK."
  (openai-request "https://api.openai.com/v1/models"
    :type "GET"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(defun openai-model (model callback)
  "Return MODEL data and execute the CALLBACK."
  (openai-request (format "https://api.openai.com/v1/models/%s" model)
    :type "GET"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

;;
;;; Application

(defvar openai-model-entries nil
  "Async models entries.")

;;;###autoload
(defun openai-retrieve-model ()
  "Retrieves a model instance, providing basic information about the model such
as the owner and permissioning."
  (interactive)
  (openai-models
   (lambda (data)
     (let ((options))
       (let-alist data
         (mapc (lambda (model)
                 (let-alist model
                   (push .id options)))
               .data))
       (when-let ((model (completing-read "Select model: " (reverse options) nil t)))
         ;; Nothing to do now, just print it out!
         ;;
         ;; XXX: Is there a way to improve the UX?
         (openai-model model (lambda (data) (message "%s" (pp-to-string data)))))))))

(tblui-define
 openai-model
 (lambda () openai-model-entries)
 [("ID" 30 nil)
  ("Owned By" 6 nil)]
 nil)

;;;###autoload
(defun openai-list-models ()
  "Lists the currently available models, and provides basic information about
each one such as the owner and availability."
  (interactive)
  (setq openai-model-entries nil)  ; reset
  (openai-models (lambda (data)
                   (let ((id 0))
                     (let-alist data
                       (mapc (lambda (model)
                               (let-alist model
                                 (push (list (number-to-string id)
                                             (vector .id
                                                     .owned_by))
                                       openai-model-entries))
                               (cl-incf id))
                             .data)))
                   (openai-model-goto-ui))))

(provide 'openai-model)
;;; openai-model.el ends here
