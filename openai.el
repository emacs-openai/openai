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

(require 'auth-source)
(require 'cl-lib)
(require 'let-alist)
(require 'pcase)
(require 'pp)
(require 'json)

(require 'request)
(require 'tblui)

(defgroup openai nil
  "Elisp library for the OpenAI API."
  :prefix "openai-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/emacs-openai/openai"))

;;
;;; Logger

(defvar openai--show-log nil
  "Get more information from the program.")

(defun openai--log (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when openai--show-log
    (apply 'message fmt args)))

;;
;;; Request

(defvar openai-key ""
  "Variable storing the openai key or a function name to retrieve it.

The function should take no arguments and return a string containing the key.

A function, `openai-key-auth-source', that retrieves the key from
auth-source is provided for convenience.")

(defvar openai-user ""
  "A unique identifier representing your end-user, which can help OpenAI to
monitor and detect abuse.")

(defcustom openai-base-url "https://api.openai.com/v1"
  "The base URL for OpenAI API requests."
  :type 'string
  :group 'openai)

(defcustom openai-parameters '()
  "The parameters for the OpenAI request."
  :type 'list
  :group 'openai)

;;;###autoload
(defun openai-key-auth-source (&optional base-url)
  "Retrieve the OpenAI API key from auth-source given a BASE-URL.
If BASE-URL is not specified, it defaults to `openai-base-url'."
  (if-let ((auth-info (auth-source-search :max 1
                                          :host (url-host (url-generic-parse-url (or base-url openai-base-url)))
                                          :require '(:user :secret))))
      (funcall (plist-get (car auth-info) :secret))
    (error "OpenAI API key not found in auth-source")))

(defun openai--resolve-key (key)
  "If the given KEY is a function call it and return the result, otherwise
return KEY."
  (cond ((functionp key)                                (funcall key))
        ((and (stringp key) (not (string-empty-p key))) key)
        (t  (user-error "[INFO] Invalid API key, please set it to the correct value: %s" key))))

(defun open--alist-omit-null (alist)
  "Omit null value or empty string in ALIST."
  (cl-remove-if (lambda (pair)
                  (let ((value (cdr pair)))
                    (or (null value)          ; ignore null
                        (and (stringp value)  ; ignore empty string
                             (string-empty-p value)))))
                alist))

(defun openai--headers (content-type key org-id)
  "Construct request headers.

Arguments CONTENT-TYPE, KEY, and ORG-ID are common request headers."
  (setq key (openai--resolve-key key))
  (open--alist-omit-null `(("Content-Type"        . ,content-type)
                           ("Authorization"       . ,(if (or (null key)
                                                             (string-empty-p key))
                                                         ""
                                                       (concat "Bearer " key)))
                           ("OpenAI-Organization" . ,org-id))))

(defun openai--json-encode (object)
  "Wrapper for function `json-encode' but it remove nil value before
constructing JSON data.

The argument OBJECT is an alist that can be construct to JSON data; see function
`json-encode' for the detials."
  (let* ((object (open--alist-omit-null object))
         (encoded (json-encode object)))
    (openai--log "[ENCODED]: %s" encoded)
    encoded))

(defun openai--handle-error (response)
  "Handle error status code from the RESPONSE.

See https://beta.openai.com/docs/guides/error-codes/api-errors."
  (let ((status-code (request-response-status-code response)))
    (openai--log "[ERROR]: %s" response)
    (pcase status-code
      (400 (message "400 - Bad request.  Please check error message and your parameters"))
      (401 (message "401 - Invalid Authentication"))
      (429 (message "429 - Rate limit reached for requests"))
      (500 (message "500 - The server had an error while processing your request"))
      (_   (message "Internal error: %s" status-code)))))

(defvar openai-error nil
  "Records for the last error.")

(defmacro openai-request (url &rest body)
  "Wrapper for `request' function.

The URL is the url for `request' function; then BODY is the arguments for rest."
  (declare (indent 1))
  `(progn
     (setq openai-error nil)
     (request ,url
       :error (cl-function
               (lambda (&key response &allow-other-keys)
                 (setq openai-error response)
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

;;
;;; Testing

;; The module here is for users to test to see some result.

(defun openai-print-json-encode (object)
  "Encode OBJECT to JSON format then print out the result."
  (let ((encoded (openai--json-encode object)))
    (message "%s" encoded)  ; don't pretty it, show the raw!
    encoded))

(provide 'openai)
;;; openai.el ends here
