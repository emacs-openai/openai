;;; openai-audio.el ---   -*- lexical-binding: t; -*-

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
;; Create audio with OpenAI API.
;;
;; See https://platform.openai.com/docs/api-reference/audio
;;

;;; Code:

(require 'openai)

;;
;;; API

;;;###autoload
(cl-defun openai-audio-create-transcription ( file callback
                                              &key
                                              (key openai-key)
                                              (model "whisper-1")
                                              prompt
                                              response-format
                                              temperature
                                              language)
  "Send transcribe audio request.

Argument FILE is audio file to transcribe, in one of these formats: mp3, mp4,
mpeg, mpga, m4a, wav, or webm.  CALLBACK is the execuation after request is
made.

Arguments KEY is global options; however, you can overwrite the value by passing
it in.

The rest of the arugments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to MODEL PROMPT, RESPONSE-FORMAT,
TEMPERATURE, and LANGUAGE."
  (openai-request "https://api.openai.com/v1/audio/transcriptions"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " key)))
    :data (openai--json-encode
           `(("model"           . ,model)
             ("file"            . ,file)
             ("prompt"          . ,prompt)
             ("response_format" . ,response-format)
             ("temperature"     . ,temperature)
             ("language"        . ,language)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;;###autoload
(cl-defun openai-audio-create-translation ( file callback
                                            &key
                                            (key openai-key)
                                            (model "whisper-1")
                                            prompt
                                            response-format
                                            temperature)
  "Send translate audio request.

Argument FILE is the audio file to translate, in one of these formats: mp3, mp4,
mpeg, mpga, m4a, wav, or webm. CALLBACK is the execuation after request is made.

Arguments KEY is global options; however, you can overwrite the value by passing
it in.

The rest of the arugments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to MODEL PROMPT, RESPONSE-FORMAT,
and TEMPERATURE."
  (openai-request "https://api.openai.com/v1/audio/transcriptions"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " key)))
    :data (openai--json-encode
           `(("model"           . ,model)
             ("file"            . ,file)
             ("prompt"          . ,prompt)
             ("response_format" . ,response-format)
             ("temperature"     . ,temperature)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(provide 'openai-audio)
;;; openai-audio.el ends here
