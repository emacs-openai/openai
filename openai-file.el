;;; openai-file.el --- Control files in OpenAI  -*- lexical-binding: t; -*-

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
;; Files are used to upload documents that can be used with features like
;; Fine-tuning.
;;
;; See https://beta.openai.com/docs/api-reference/files
;;

;;; Code:

(require 'openai)

;;
;;; API

(cl-defun openai-file-list ( callback
                             &key
                             (base-url openai-base-url)
                             (parameters openai-parameters)
                             (content-type "application/json")
                             (key openai-key)
                             org-id)
  "Return a list of files that belong to the user's organization.

The argument CALLBACK is execuated after request is made.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY and ORG-ID are global
options; however, you can overwrite the value by passing it in."
  (openai-request (concat base-url "/files")
    :type "GET"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(cl-defun openai-file-upload ( file purpose callback
                               &key
                               (base-url openai-base-url)
                               (parameters openai-parameters)
                               (content-type "application/json")
                               (key openai-key)
                               org-id)
  "Upload a file that contain document(s) to be used across various
endpoints/features.

The argument FILE is the JSON Lines file to be uploaded.

If the PURPOSE is set to \"fine-tune\", each line is a JSON record with
\"prompt\" and \"completion\" fields representing your training examples.

The argument PURPOSE is the intended purpose of the uploaded documents.

Use \"fine-tune\" for Fine-tuning. This allows us to validate the format of the
uploaded file.

Argument CALLBACK is function with data pass in.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY and ORG-ID are global
options; however, you can overwrite the value by passing it in."
  (openai-request (concat base-url "/files")
    :type "POST"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :data (openai--json-encode
           `(("file"    . ,file)
             ("purpose" . ,purpose)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(cl-defun openai-file-delete ( file-id callback
                               &key
                               (base-url openai-base-url)
                               (parameters openai-parameters)
                               (content-type "application/json")
                               (key openai-key)
                               org-id)
  "Delete a file.

The arument FILE-ID is id of the file to use for this request.

Argument CALLBACK is function with data pass in.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY and ORG-ID are global
options; however, you can overwrite the value by passing it in."
  (openai-request (concat base-url "/files")
    :type "DELETE"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :data (openai--json-encode
           `(("file_id" . ,file-id)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(cl-defun openai-file-retrieve ( file-id callback
                                 &key
                                 (base-url openai-base-url)
                                 (parameters openai-parameters)
                                 (content-type "application/json")
                                 (key openai-key)
                                 org-id)
  "Return information about a specific file.

The arument FILE-ID is id of the file to use for this request.

The argument CALLBACK is execuated after request is made.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY and ORG-ID are global
options; however, you can overwrite the value by passing it in."
  (openai-request (format "%s/files/%s" base-url file-id)
    :type "GET"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :data (openai--json-encode
           `(("file_id" . ,file-id)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(cl-defun openai-file-retrieve-content ( file-id callback
                                         &key
                                         (base-url openai-base-url)
                                         (parameters openai-parameters)
                                         (content-type "application/json")
                                         (key openai-key)
                                         org-id)
  "Return the contents of the specified file

The arument FILE-ID is id of the file to use for this request.

The argument CALLBACK is execuated after request is made.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY and ORG-ID are global
options; however, you can overwrite the value by passing it in."
  (openai-request (format "%s/files/%s/content" base-url file-id)
    :type "GET"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :data (openai--json-encode
           `(("file_id" . ,file-id)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;
;;; Util

(defun openai--select-jsonl-files (candidate)
  "Return t if CANDIDATE is either directory or a JSONL file."
  (or (string-suffix-p ".jsonl" candidate t)  ; only support png
      (file-directory-p candidate)))        ; allow navigation

(defun openai-file--select (callback)
  "Select a file then execute CALLBACK."
  (openai-file-list
   (lambda (data)
     (let ((options))
       (let-alist data
         (mapc (lambda (file)
                 (let-alist file
                   (push (cons .filename .id) options)))
               .data))
       (when (zerop (length options))
         (user-error "No response, please try again"))
       (let*
           ((offset (openai--completing-frame-offset options))
            (file (if (= 1 (length options))
                      (car options)
                    (completing-read
                     "Select file: "
                     (lambda (string predicate action)
                       (if (eq action 'metadata)
                           `(metadata
                             (display-sort-function . ,#'identity)
                             (annotation-function
                              . ,(lambda (cand)
                                   (concat (propertize " " 'display `((space :align-to (- right ,offset))))
                                           (cdr (assoc cand options))))))
                         (complete-with-action action options string predicate)))
                     nil t)))
            (file-id (cdr (assoc file options))))
         (funcall callback options file file-id))))))

;;
;;; Application

(defvar openai-file-entries nil
  "Async files entries.")

(tblui-define
 openai-file
 (lambda () openai-file-entries)
 [("Filename" 15 nil)
  ("ID" 30 nil)
  ("Bytes" 6 nil)
  ("Object" 10 nil)
  ("Created at" 10 nil)
  ("Purpose" 40 nil)]
 nil)

;;;###autoload
(defun openai-list-files ()
  "List files that belong to the user's organization."
  (interactive)
  (setq openai-file-entries nil)  ; reset
  (openai-file-list (lambda (data)
                      (let ((id 0))
                        (let-alist data
                          (mapc (lambda (file)
                                  (let-alist file
                                    (push (list (number-to-string id)
                                                (vector .filename
                                                        .id
                                                        .bytes
                                                        .object
                                                        .created_at
                                                        .purpose))
                                          openai-file-entries))
                                  (cl-incf id))
                                .data)))
                      (openai-file-goto-ui))))

;;;###autoload
(defun openai-upload-file ()
  "Prompt to upload the file to OpenAI server for file-tuning."
  (interactive)
  (when-let ((file (read-file-name "Select file: " nil nil t nil
                                   #'openai--select-jsonl-files))
             (purpose (read-string "Purpoe: ")))
    (openai-file-upload file purpose
                        (lambda (data)
                          ;; Nothing to do now, just print it out!
                          ;;
                          ;; XXX: Is there a way to improve the UX?
                          (message "%s" (pp-to-string data))))))

;;;###autoload
(defun openai-delete-file ()
  "Prompt to select the file and delete it."
  (interactive)
  (openai-file--select
   (lambda (_options file file-id)
     (openai-file-delete
      file-id
      (lambda (data)
        (let-alist data
          (if .deleted
              (message "Deleted file %s" file)
            (message "Failed to delete file: %s %s" file file-id))))))))

;;;###autoload
(defun openai-retrieve-file ()
  "Prompt to select the file and print its' information."
  (interactive)
  (openai-file--select
   (lambda (_options _file file-id)
     (openai-file-retrieve
      file-id
      (lambda (data)
        ;; Nothing to do now, just print it out!
        ;;
        ;; XXX: Is there a way to improve the UX?
        (message "%s" (pp-to-string data)))))))

;;;###autoload
(defun openai-retrieve-file-content ()
  "Prompt to select the file and print its' content."
  (interactive)
  (openai-file--select
   (lambda (_options _file file-id)
     (openai-file-retrieve-content
      file-id
      (lambda (data)
        ;; XXX: It seems like we should download it instead print it?
        (message "%s" data))))))

(provide 'openai-file)
;;; openai-file.el ends here
