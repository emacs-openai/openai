[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/openai.svg)](https://jcs-emacs.github.io/jcs-elpa/#/openai)

# OpenAI.el
> Elisp library for the OpenAI API

[![CI](https://github.com/emacs-openai/openai/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-openai/openai/actions/workflows/test.yml)

The OpenAI Elisp library provides convenient access to the OpenAI API from
applications written in the Elips language. 

*P.S. This package is expected to be used as a library, so there are only a few
interactable commands you can use, and those are mostly examples.*

## Documentation

- [OpenAI API docs](https://beta.openai.com/docs/introduction)

## 🔨 Usage

You will need to set up your API key before you can use this library.

```elisp
(setq openai-key "[YOUR API KEY]")
```

### The simplest example

Here is the simplest example that teaches you how to use this library. This is 
a function with a `query` and a callback function.

```elisp
(openai-completion "How are you?"
                   (lambda (data)
                     (message "%s" data)))
```

### Customization

Most arguments are extracted (excepts the required one) as global variables.
For example, one variable `openai-completon-n` is defined in `openai-completion.el`
file. That variable is used for the completion request, for more information see
https://beta.openai.com/docs/api-reference/completions. The naming convention is
by the following pattern:

```
[PACKAGE NAME]-[API TYPE]-[NAME OF THE ARGUMENT]
```

For example:

```
openai-edit-temperature
```

- `openai` - is the package name
- `edit` - is the api type, see [OpenAI API reference](https://platform.openai.com/docs/api-reference/introduction)
- `temperature` - is the argument for the [Edit](https://platform.openai.com/docs/api-reference/edits) request.

You can change the model for a single request without changing its global value.

```elisp
(let ((openai-edit-model "text-davinci-edit-001")  ; use another model for this request,
      (openai-edit-n 3))                           ; and i want three outcomes
  (openai-edit-create "What day of the wek is it?"
                      "Fix the spelling mistakes"
                      (lambda (data)
                        (message "%s" data))))
```

### Setting Model

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

