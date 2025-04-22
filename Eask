;; -*- mode: eask; lexical-binding: t -*-

(package "openai"
         "0.1.0"
         "Elisp library for the OpenAI API")

(website-url "https://github.com/emacs-openai/openai")
(keywords "comm" "openai")

(package-file "openai.el")
(files "openai-*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "26.1")
(depends-on "request")
(depends-on "tblui")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
