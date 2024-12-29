
(in-package #:adp-docs)

@output-file["/README.md"]

@title[:toc nil]{Add Documentation, Please...}

Welcome to ADP!

Simply put, ADP adds scribble files support to ASDF. Thus, you can add scribble files and use the well-known @link[:address "https://docs.racket-lang.org/scribble/reader.html"]{at syntax}.

Also, check out @link[:address "https://github.com/HectareaGalbis/cl-scribble-mode"]{cl-scribble-mode} to edit scribble files with Common Lisp symbols.

Examples:

@image[#P"images/cl-scribble-mode-example.gif" :alt-text "cl-scribble-mode example"]

@image[#P"images/example-adp.png" :alt-text "adp example"]


@subtitle{Installation}

This project is available on Quicklisp. But, instead of installing this, you should install an exporter.


@subtitle{Exporters}

@itemize[
        @item{@link[:address "https://github.com/HectareaGalbis/adp-github"]{adp-github}: Generates Github flavoured Markdown files.}
]

@subtitle{Documentation as another system}

Almost every Common Lisp project can have several systems. These systems usually indicate which files of code should compiled and loaded and in which order. There are, sometimes, other systems that does not load the project's code, but does load unit tests to verify the correctness of the project. It seems natural now that documentation should have its own system to document the project.

ADP extends ASDF with Scribble files. These format is used exhaustively by the Racket language and it proves that Scribble is a very good language for writing documentation.

However, Racket and Common Lisp are different. That's why ADP tries to integrate Scribble into Common Lisp making some changes in favour to flexibility and power. For example, enabling the at-syntax requires changing the readtable to use the scribble language. But, the Common Lisp programmer can still select which readtable to use in every Scribble file. The package can also be choosen as normally.

In this documentation system, the programmer can still specify lisp files to write auxiliar functions or macros to use later within Scribble files. Remember that Scribble is just syntactic sugar for function calls.

@code-block[:lang "common lisp"]{
;; Common Lisp
(+ 3 4)

;; Scribble
@+[3 4]
}

@subsubtitle{Enabling scribble files}

ADP cannot be used directly to enable Scribble files. Instead, you need to load an exporter like @link[:address "https://github.com/HectareaGalbis/adp-github"]{adp-github}.

Enabling scribble files in your system requires 2 things:

@itemize[
        @item{Add the exporter's system to the @code{:defsystem-depends-on} list:}
]

@code-block[:lang "common lisp"]{
(defsystem "my-system/docs"
  :defsystem-depends-on ("adp-github")
  ...)
}

@itemize[
        @item{Specify the class of the system:}
]

@code-block[:lang "common lisp"]{
(defsystem "my-system/docs"
  :defsystem-depends-on ("adp-github")
  :class :adp-github
  ...)
}

And now, add as many files as you want. This can be a valid example of a documentation system:

@code-block[:lang "common lisp"]{
(defsystem "my-project/docs"
  :defsystem-depends-on ("adp-github")
  :class :adp-github
  :depends-on ("my-project")
  :components ((:module "scribble"
                :components ((:file "package")
                             (:file "custom-functions")
                             (:scribble "reference")
                             (:scribble "README")))))
}

The system class @code{:adp-github} and the name of Scribble files like @code{:scribble} are defined by the exporter. Each exporter might define one or more types of scribble files.


@subsubtitle{Generating the files}

Once the system is defined, nothing special is required. Just load the system.

@code-block[:lang "common lisp"]{
(asdf:load-system "my-project/docs")
}

Or, equivalently:

@code-block[:lang "common lisp"]{
(asdf:make "my-project/docs")
}

The files will be generated according to the used exporter.
