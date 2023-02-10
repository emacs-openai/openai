;;; openai.el --- Elisp library for the OpenAI API  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/openai
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (request "0.3.0") (tblui "0.1.0"))
;; Keywords: comm openai

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
;; Elisp library for the OpenAI API
;;

;;; Code:

(require 'cl-lib)
(require 'let-alist)
(require 'pcase)
(require 'pp)

(require 'request)
(require 'tblui)

(defgroup openai nil
  "Elisp library for the OpenAI API."
  :prefix "openai-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/openai"))

(defcustom openai-key ""
  "Generated API key."
  :type 'list
  :group 'openai)

(defcustom openai-user ""
  "A unique identifier representing your end-user, which can help OpenAI to
monitor and detect abuse."
  :type 'string
  :group 'openai)

(defun openai--handle-error (response)
  "Handle error status code from the RESPONSE.

See https://beta.openai.com/docs/guides/error-codes/api-errors."
  (let ((status-code (request-response-status-code response)))
    (pcase status-code
      (400 (error "400 - Bad request.  Please check error message and your parameters"))
      (401 (error "401 - Invalid Authentication"))
      (429 (error "429 - Rate limit reached for requests"))
      (500 (error "500 - The server had an error while processing your request"))
      (_   (error "Internal error: %s" status-code)))))

(defmacro openai-request (url &rest body)
  "Wrapper for `request' function.

The URL is the url for `request' function; then BODY is the arguments for rest."
  (declare (indent 1))
  `(if (string-empty-p openai-key)
       (user-error "[INFO] Invalid API key, please set it to the correct value: %s" openai-key)
     (request ,url
       :error (cl-function
               (lambda (&key response &allow-other-keys)
                 (openai--handle-error response)))
       ,@body)))

;;
;;; Util

(defcustom openai-annotation-ratio 2.5
  "Ratio align from the right to display `completin-read' annotation."
  :type 'float
  :group 'openai)

;;
;;; General

(defun openai--2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

(defun openai--seq-str-max (sequence)
  "Return max length in SEQUENCE of strings."
  (let ((result 0))
    (mapc (lambda (elm) (setq result (max result (length (openai--2str elm))))) sequence)
    result))

(defun openai--completing-frame-offset (options)
  "Return frame offset while `completing-read'.

Argument OPTIONS ia an alist use to calculate the frame offset."
  (max (openai--seq-str-max (mapcar #'cdr options))
       (/ (frame-width) openai-annotation-ratio)))

;;
;;; Buffer

(defmacro openai--with-buffer (buffer-or-name &rest body)
  "Execute BODY ensure the BUFFER-OR-NAME is alive."
  (declare (indent 1))
  `(when (buffer-live-p (get-buffer ,buffer-or-name))
     (with-current-buffer ,buffer-or-name ,@body)))

(defun openai--pop-to-buffer (buffer-or-name)
  "Show BUFFER-OR-NAME to display GPT result."
  (pop-to-buffer (get-buffer-create buffer-or-name)
                 `((display-buffer-in-direction)
                   (dedicated . t))))

;;
;;; Choices

(defun openai--data-choices (data)
  "Extract choices from DATA request."
  (let ((choices (let-alist data .choices))  ; choices if vector
        (texts))
    (mapc (lambda (choice)
            (let-alist choice
              (push .text texts)))  ; text is the only important data in there
          choices)
    texts))

(defun openai--get-choice (choices)
  "Return choice from CHOICES."
  (cond ((zerop (length choices))
         (user-error "No response, please try again"))
        ((= 1 (length choices))
         (car choices))
        (t
         (completing-read "Response: " choices nil t))))

(provide 'openai)
;;; openai.el ends here
