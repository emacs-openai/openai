;;; openai-image.el --- Create image with OpenAI  -*- lexical-binding: t; -*-

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
;; Create image with OpenAI.
;;
;; See https://beta.openai.com/docs/api-reference/images/create
;;

;;; Code:

(require 'openai)

;;
;;; API

(cl-defun openai-image ( prompt callback
                         &key
                         (base-url openai-base-url)
                         (parameters openai-parameters)
                         (content-type "application/json")
                         (key openai-key)
                         org-id
                         n
                         size
                         response-format
                         (user openai-user))
  "Create an image given a PROMPT.

Arguments PROMPT and CALLBACK are required for this type of request.  PROMPT is
either the question or instruction to OpenAI.  CALLBACK is the execution after
request is made.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY, ORG-ID and USER are global
options; however, you can overwrite the value by passing it in.

The rest of the arugments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to N, SIZE, and RESPONSE-FORMAT."
  (openai-request (concat base-url "/images/generations")
    :type "POST"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :data (openai--json-encode
           `(("prompt"          . ,prompt)
             ("n"               . ,n)
             ("size"            . ,size)
             ("response_format" . ,response-format)
             ("user"            . ,user)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(cl-defun openai-image-edit ( image prompt callback
                              &key
                              (base-url openai-base-url)
                              (parameters openai-parameters)
                              content-type
                              (key openai-key)
                              org-id
                              mask
                              n
                              size
                              response-format
                              (user openai-user))
  "Create an edited or extended image given an original IMAGE and a PROMPT.

Arguments IMAGE, PROMPT and CALLBACK are required for this type of request.
PROMPT is a text description of the desired image(s).  IMAGE is the image file
to edit.  CALLBACK is the execution after request is made.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY, ORG-ID and USER are global
options; however, you can overwrite the value by passing it in.

The rest of the arguments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to MASK, N, SIZE, and
RESPONSE-FORMAT."
  (openai-request (concat base-url "/images/edits")
    :type "POST"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :data (openai--json-encode
           `(("image"           . ,image)
             ("prompt"          . ,prompt)
             ("mask"            . ,mask)
             ("n"               . ,n)
             ("size"            . ,size)
             ("response_format" . ,response-format)
             ("user"            . ,user)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(cl-defun openai-image-variation ( image callback
                                   &key
                                   (base-url openai-base-url)
                                   (parameters openai-parameters)
                                   content-type
                                   (key openai-key)
                                   org-id
                                   mask
                                   n
                                   size
                                   response-format
                                   (user openai-user))
  "Create a variation of a given IMAGE.

Argument CALLBACK is function with data pass in, and the argument IMAGE  must
be a valid PNG file, less than 4MB, and square.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY, ORG-ID and USER are global
options; however, you can overwrite the value by passing it in.

The rest of the arugments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to MASK, N, SIZE, and
RESPONSE-FORMAT."
  (openai-request (concat base-url "/images/variations")
    :type "POST"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :data (openai--json-encode
           `(("image"           . ,image)
             ("mask"            . ,mask)
             ("n"               . ,n)
             ("size"            . ,size)
             ("response_format" . ,response-format)
             ("user"            . ,user)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;
;;; Util

(defun openai--select-png-files (candidate)
  "Return t if CANDIDATE is either directory or a PNG file."
  (or (string-suffix-p ".png" candidate t)  ; only support png
      (file-directory-p candidate)))        ; allow navigation

;;
;;; Application

(defcustom openai-image-n 1
  "The number of images to generate.  Must be between 1 and 10."
  :type 'integer
  :group 'openai)

(defcustom openai-image-size "1024x1024"
  "The size of the generated images.

Must be one of `256x256', `512x512', or `1024x1024'."
  :type 'string
  :group 'openai)

(defcustom openai-image-response-format "url"
  "The format in which the generated images are returned.

Must be one of `url' or `b64_json'."
  :type 'string
  :group 'openai)

(defvar openai-image-entries nil
  "Async images entries.")

(tblui-define
 openai-image
 (lambda () openai-image-entries)
 [("URL" 200 nil)]
 nil)

;;;###autoload
(defun openai-image-prompt (prompt)
  "Use PROMPT to ask for image, and display result in a buffer."
  (interactive (list (read-string "Describe image: ")))
  (setq openai-image-entries nil)
  (openai-image prompt
                (lambda (data)
                  (let ((id 0))
                    (let-alist data
                      (mapc (lambda (images)
                              (dolist (image images)
                                (push (list (number-to-string id)
                                            (vector (cdr image)))
                                      openai-image-entries)
                                (cl-incf id)))
                            .data)))
                  (openai-image-goto-ui)
                  :size openai-image-size
                  :n openai-image-n
                  :response-format openai-image-response-format)))

;;;###autoload
(defun openai-image-edit-prompt ()
  "Use prompt to ask for image, and display result in a buffer."
  (interactive)
  (setq openai-image-entries nil)
  (let ((image (read-file-name "Select image file: " nil nil t nil
                               #'openai--select-png-files))
        (prompt (read-string "Describe image: ")))
    (openai-image-edit image prompt
                       (lambda (data)
                         (let ((id 0))
                           (let-alist data
                             (mapc (lambda (images)
                                     (dolist (image images)
                                       (push (list (number-to-string id)
                                                   (vector (cdr image)))
                                             openai-image-entries)
                                       (cl-incf id)))
                                   .data)))
                         (openai-image-goto-ui))
                       :size openai-image-size
                       :n openai-image-n
                       :response-format openai-image-response-format)))

;;;###autoload
(defun openai-image-variation-prompt (image)
  "Prompt to select an IMAGE file, and display result in a buffer."
  (interactive (list (read-file-name "Select image file: " nil nil t nil
                                     #'openai--select-png-files)))
  (setq openai-image-entries nil)
  (openai-image-variation image
                          (lambda (data)
                            (let ((id 0))
                              (let-alist data
                                (mapc (lambda (images)
                                        (dolist (image images)
                                          (push (list (number-to-string id)
                                                      (vector (cdr image)))
                                                openai-image-entries)
                                          (cl-incf id)))
                                      .data)))
                            (openai-image-goto-ui))
                          :size openai-image-size
                          :n openai-image-n
                          :response-format openai-image-response-format))

(provide 'openai-image)
;;; openai-image.el ends here
