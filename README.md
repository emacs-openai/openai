[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/openai.svg)](https://jcs-emacs.github.io/jcs-elpa/#/openai)

# OpenAI.el
> Elisp library for the OpenAI API

[![CI](https://github.com/emacs-openai/openai/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-openai/openai/actions/workflows/test.yml)

The OpenAI Elisp library provides convenient access to the OpenAI API from
applications written in the Elips language. 

## Documentation

- [OpenAI API docs](https://beta.openai.com/docs/introduction)

## 🔨 Usage

```elisp
(setq openai-key "[YOUR API KEY]")
```

```elisp
(openai-completion "How are you?"
                   (lambda (data)
                     (message "%s" data)))
```

You can also choose which model you want to use by going to the 
[api](https://api.openai.com/v1/models) website and looking at the id's. 
For code usage you probably want something that starts with `code-` whereas 
with more text related files you'll likely want something starting with `text-`.

```elisp
(setq openai-completion-model "NAME-HERE")
```

## References

- [CodeGPT](https://marketplace.visualstudio.com/items?itemName=timkmecl.codegpt3)
- [aide.el](https://github.com/junjizhi/aide.el)
- [ChatGPT.el](https://github.com/joshcho/ChatGPT.el)

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

