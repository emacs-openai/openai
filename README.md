[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/openai.svg)](https://jcs-emacs.github.io/jcs-elpa/#/openai)

# OpenAI.el
> Elisp library for the OpenAI API

[![CI](https://github.com/emacs-openai/openai/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-openai/openai/actions/workflows/test.yml)

The OpenAI Elisp library provides convenient access to the OpenAI API from
applications written in the Elips language. 

*P.S. This package is expected to be used as a library, so there are only a few
interactable commands you can use, and those are mostly examples.*

## 📚 Documentation

- [OpenAI API docs](https://beta.openai.com/docs/introduction)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [OpenAI.el](#openaiel)
  - [📚 Documentation](#📚-documentation)
  - [🔨 Usage](#🔨-usage)
    - [🔰 The simplest example](#🔰-the-simplest-example)
    - [📨 Sending Request](#📨-sending-request)
    - [📢 API functions](#📢-api-functions)
  - [🔗 References](#🔗-references)
  - [Contribute](#contribute)

<!-- markdown-toc end -->

## 🔨 Usage

You will need to set up your API key before you can use this library.

```elisp
(setq openai-key "[YOUR API KEY]")
```

### 🔰 The simplest example

Here is the simplest example that teaches you how to use this library. This is 
a function with a `query` and a callback function.

```elisp
(openai-completion "How are you?"
                   (lambda (data)
                     (message "%s" data)))
```

### 📨 Sending Request

Most arguments are exposed in the argument list (excepts the required one).

For example, the request function `openai-completion` accepts argument
`max-tokens`. By seeing OpenAI's references page:

> `max_tokens`  integer  Optional  Defaults to 16
>
> The maximum number of tokens to generate in the completion.
>
> The token count of your prompt plus `max_tokens` cannot exceed the model's
> context length. Most models have a context length of 2048 tokens (except for
> the newest models, which support 4096).

```elisp
(openai-completion "How are you?"
                   (lambda (data)
                     ...)
                   :max-tokens 4069)
```

### 📢 API functions

The API functions are followed by this pattern:

```
[PACKAGE NAME]-[API TYPE]-[REQUEST NAME]
```

For example:

```elisp
(openai-file-list ...)
```

- `openai` - is the package name
- `file` - is the api type, see [OpenAI API reference](https://platform.openai.com/docs/api-reference/introduction)
- `list` - is the request name

## 🔗 References

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

