<p align="right">
<picture>
  <source media="(prefers-color-scheme: light)" srcset="./etc/light.png">
  <source media="(prefers-color-scheme: dark)" srcset="./etc/dark.png">
  <img width="25%" src="">
</picture>
</p>

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/openai.svg)](https://jcs-emacs.github.io/jcs-elpa/#/openai)

# OpenAI.el
> Elisp library for the OpenAI API

[![CI](https://github.com/emacs-openai/openai/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-openai/openai/actions/workflows/test.yml)

The OpenAI Elisp library provides convenient access to the OpenAI API from
applications written in the Elisp language. 

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
      - [🔍 Parameters](#🔍-parameters)
    - [🖥 Setting Model](#🖥-setting-model)
  - [🛑 Debugging](#🛑-debugging)
  - [📂 Example projects](#📂-example-projects)
  - [🔗 References](#🔗-references)
  - [🛠️ Contribute](#🛠️-contribute)
    - [🔬 Development](#🔬-development)
  - [⚜️ License](#⚜️-license)

<!-- markdown-toc end -->

## 🔨 Usage

You will need to set up your API key before you can use this library.

```elisp
(setq openai-key "[YOUR API KEY]")
```

Alternatively you can configure a function to retrieve the key from some
external source. A function, `openai-key-auth-source` is provided to retrieve
the key from an auth-source entry under the `:host` key `api.openai.com`

```elisp
(setq openai-key #'openai-key-auth-source)
```

For requests that need your user identifier,

```elisp
(setq openai-user "[YOUR USER UID]")
```

For using another OpenAI endpoint, 

```elisp
(setq openai-base-url "[OPENAI BASE URL]")
```

> 💡 Tip
>
> The two variables `openai-key` and `openai-user` are the default values for
> sending requests! However, you can still overwrite the value by passing the
> keywords `:key` and `:user`!

### 🔰 The simplest example

Here is the simplest example that teaches you how to use this library. This is 
a function with a `query` and a callback function.

```elisp
(openai-completion "How are you?"
                   (lambda (data)
                     (message "%s" data)))
```

### 📨 Sending Request

All arguments are exposed in the argument list, so you can send any request in
any way you want.

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
(openai-completion ...
                   ...
                   :max-tokens 4069)  ; max out tokens!
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

#### 🔍 Parameters

The function's parameters are followed in this order:

1. required - variables are required for this type of request
2. `callback` - execution after the request is made
3. optional - other variables that are not required, but will affect the final output

```elisp
(openai-completion "How are you?"          ; required
                   (lambda (data)          ; callback
                     ...)
                   :max-tokens 4069)       ; optional
```

### 🖥 Setting Model

Every type of request has a default `model`, and we hope this benefits the users
to not worry about what model to use for their request! However, if you want to
use other models, you can use the keyword `:model` to replace them!

```elisp
(openai-completion ...
                   ... 
                   :model "text-davinci-003")  ; replace the default model
```

## 🛑 Debugging

While playing through this library, you might see this error quite often.

```
400 - Bad request.  Please check error message and your parameters
```

Try `M-x openai-debug-mode`, it will show more error messages.

## 📂 Example projects

- [codegpt](https://github.com/emacs-openai/codegpt)
- [chatgpt](https://github.com/emacs-openai/chatgpt)
- [dall-e](https://github.com/emacs-openai/dall-e)

## 🔗 References

- [CodeGPT](https://marketplace.visualstudio.com/items?itemName=timkmecl.codegpt3)
- [aide.el](https://github.com/junjizhi/aide.el)
- [ChatGPT.el](https://github.com/joshcho/ChatGPT.el)

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!


### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
$ eask install-deps --dev
```

To test the package's installation:

```sh
$ eask package
$ eask install
```

To test compilation:

```sh
$ eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
$ eask lint checkdoc
```

The standard `package` linter:

```sh
$ eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE) for details.
