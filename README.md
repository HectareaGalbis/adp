# Add Documentation, Please

Welcome to ADP!

## Introduction

_Add Documentation, Please_ is a library for literate programming and semi-automatic API generation. There are already good projects for literate programming like `Erudite` so, why another project? Well, they work differently and `Erudite` just doesn't adjust to my needs.

**ADP** is simple but practical. To generate documentation you have to use different macros. For example, if you want a header, then you use the [HEADER](docs/user-api.md#macro-header) macro. Or if you want a block of code you use [CODE-BLOCK](docs/user-api.md#macro-code-block). The reason behind using macros to literate programming is that you can make your own macros using the ones exported by **ADP**.

Generating the API documentation is also easy. Suppose that you have the following function definition:

```
(DEFUN FOO () "A function that does nothing." (VALUES))
```

ADP redefines the macro [DEFUN](docs/user-api.md#macro-defun). To generate the documentation for this function you just need to tell Common Lisp that the macro [DEFUN](http://www.lispworks.com/reference/HyperSpec/Body/m_defun.htm) used is the one from the package `adp`:

```
(ADP:DEFUN FOO () "A function that does nothing" (VALUES))
```

That's all! And the same occurs with every Common Lisp macro that defines something, like [DEFPACKAGE](docs/user-api.md#macro-defpackage) or [DEFINE-METHOD-COMBINATION](docs/user-api.md#macro-define-method-combination).

You may be thinking that this will make your code slower because now your code is gathering information for printing documentation. But that is not the case. The documentation generation is controlled by a global variable. This way, when you load your system like always, ADP will do nothing. Literally (try to macroexpand some ADP macro). The generation is activated only when you load your system using the function [LOAD-DOCUMENTATION-SYSTEM](docs/user-api.md#function-load-documentation-system). Even you can create a different system for loading the files you need for documentation.

Finally, you can also choose between several styles. Each style creates different files. For example, the style _markdown_ generates `md` files. In fact, the readme file you are reading right now has been generated by ADP, so if this is a markdown file you are seeing the markdown style. Another style could generate `html` files or `tex` files.

## Installation

For now, you need to install this project downloading the source. But soon it will be available on Quicklisp.

## Dependencies

The ADP project only depends on Alexandria. However, each style can have other dependencies.

## Documentation

* The ADP guide: TODO
* The Style-Maker guide: TODO
* The ADP api: [docs/user-api](docs/user-api.md)
* The Style-Maker api: [docs/style-maker-api](docs/style-maker-api.md)

