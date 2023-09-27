
# Add Documentation, Please

Welcome to ADP!

## Introduction

Simply put, ADP adds scribble files support to ASDF. Thus, you can add scribble files and use the well-known [at-syntax](https://docs.racket-lang.org/scribble/reader.html).

Example:

``` common-lisp
(defsystem "my-project"
  :defsystem-depends-on ("some-adp-exporter")
    :components ((:file "package")
                                    (:file "some-file")
                                    (:scribble "guide")
                                    (:scribble "reference")))
```

As you might expect, using `:scribble` in your `asd` file will make `ASDF` to look for a `scrbl` file. However, using `asdf:load-system` won't do nothing. In order to generate files from `scrbl` files we need new operations. Those operations will be defined by exporters. Each exporter will process the contents of scribble files and will generate the pertinent files.

Each exporter is different, and you should read their own documentation. However, all exporters have something in common. They will define a new `ASDF` operator. Imagine that an exporter named `adp-latex` defines the operator `adp-latex-op`. Then, to generate the latex files from the scribble ones, you should do the following:

``` common-lisp
(asdf:operate 'adp-latex-op "my-project")
```

And that's all.

## Exporters

* [adp-plain](https://github.com/Hectarea1996/adp-plain)

