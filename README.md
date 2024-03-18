
# Add Documentation, Please

Welcome to ADP!


## Introduction

Simply put, ADP adds scribble files support to ASDF. Thus, you can add scribble files and use the well-known [at-syntax](https://docs.racket-lang.org/scribble/reader.html).

Example:

![Scribble file example](/images/example-adp.png "Example")


## Installation

This project is available on Quicklisp. But, instead of installing this, you should install an exporter.


## Exporters

* [adp-github](https://github.com/Hectarea1996/adp-github): Generates Github flavoured Markdown files with crossreferences, tables of contents, etc.


## Getting started

Each exporter defines diferent ways of adding `scribble` files, but usually all them have similarities.

1. Create a subsystem for the scribble files. It must `defsystem-depends-on` the selected exporter and should `depends-on` you project to be able to use everything you've defined there. Let's use `adp-github` in this example. This exporter defines the ASDF file type `scribble` as well as the ASDF operation `adp-github-op`. 

``` common-lisp
;;; my-project.asd

(defsystem "my-project"
  ...)

(defsystem "my-project/docs"
  :defsystem-depends-on ("adp-github")
  :depends-on ("my-project")
  :build-operation "adp-github-op" 
  :components ((:file "doc-package")
               (:scribble "user-guide")))
```

2. Create the files. In this example we need a regular file named `doc-package.lisp` and a scribble one named `user-guide.scrbl`. In the former we will define the package to use while writing scribble text. Those files could have the following contents:

``` common-lisp
;;; doc-package.lisp

(defpackage "my-project-docs"
  (:use #:cl #:adp-github))


;;; user-guide.scrbl

(in-package "my-project-docs")

@header{User guide}

Welcome to my project!
```

3. Generate the documentation. We only need to `make` the new subsystem. In your REPL, evaluate the following expression:

``` common-lisp
(asdf:make "my-project/docs")
```


## How to create an exporter

Creating an exporter is really easy. Let's do an example of an exporter that creates txt files and `princ`s every object that ADP gathers.

An exporter must be its own project. Let's create the `asd` file:

``` common-lisp
(defsystem "adp-princ"
  :depends-on ("adp")
  :components ((:file "package")
               (:file "adp-princ")))
```

Of course, you can use whatever number of files you want. The exporter can use also whatever package you want; suppose we're using the package `ADP-PRINC`. So, let's see now the file `adp-princ`:

``` common-lisp
(in-package #:adp-princ)

(adp:define-adp-operation adp-princ-op) ; <- defining a new operation

(adp:define-adp-file scribble "scrbl")  ; <- defining a new ASDF file type

...
```

First of all, we need to define an ASDF operation and an ASDF file. The operation will be the responsible of gather all the scribble contents from the new defined `scribble` files with extension `scrbl`.

After this, we can now implement a method using that operation:

``` common-lisp
(in-package #:adp-princ)

(adp:define-adp-operation adp-princ-op)

(adp:define-adp-file scribble "scrbl")

(defmethod adp:export-content ((op adp-princ-op) files system)
  ...)
```

The argument `files` is a `vector` containing each `scribble` file used in the subsystem (actually, a `file` is an object representing a file). Each file contains the elements gathered from a `scribble` file. The order in which files are stored in is the same as the order they are loaded.

Each file contains the `ASDF` component it represents as well. Files have then two accessors:

* `adp:file-component`: Retrieves the `ASDF` component of a file.
* `adp:file-elements`: Retrieves the elements of a file. The elements are stored in a `vector`.

In this case, `file-component` will have always the type `scribble`. This type was defined by `adp:define-adp-file`. We could have defined more file types. Finally, we can use generic functions to call different methods depending on the type of these files.

Coming back to `adp:export-content`, the method receives the operation we just defined, the files `ADP` has gathered and the `ASDF` system component. Like `files` is a vector, we only need to iterate over it:

``` common-lisp
(defmethod adp:export-content ((op adp-princ-op) files system)
  (loop for file across files
        do (let ((target-file (make-pathname :directory (pathname-directory 
                                                            (merge-pathnames file-path
                                                                             (asdf:system-source-directory system)))
                                             :name (pathname-name file-path)
                                             :type "txt")))
               (with-open-file (file-str target-path :direction :output :if-exists :supersede
                                                     :if-does-not-exist :create)
                 (loop for element across (adp:file-elements file)
                       do (princ element file-str))))))
```

For every file, we are creating first the target pathname. It is the same as the source file, but with the type "txt". If it doesn't exist, we create the file and `princ` every object in it.

And that's all! 

There are also generic functions that allow you to make some preparation before/after the system or a file is loaded. These are `adp:pre-process-system`, `adp:post-process-system`, `adp:pre-process-file` and `adp:post-process-file`. 


## Writing scribble with Emacs

Scribble has already its own mode: [scribble-mode](https://github.com/emacs-pe/scribble-mode/tree/master). However, Scribble was designed to be used with Scheme-like languages. But we can make some magic to get syntax highlighting and autocompletion with Common Lisp.

The scribble mode already gives us some syntax highlighting, but we need to make some changes. On the other hand, we can get autocompletion with [company-mode](https://company-mode.github.io/) and [slime-company](https://github.com/anwyn/slime-company).

`company` doesn't need any changes, but `scribble` and `slime-company` do. Here is how I configured these modes in my `init.el`:

``` emacs-lisp
;; ------ company ------
(use-package company
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :config
  (global-company-mode))


;; ------ scrbl ------
(use-package scribble-mode
  ;; We modify the syntax to adapt scribble-mode to the Common Lisp language.
  ;; With this it will understand prefix packages.
  :config
  (modify-syntax-entry ?: "_ " scribble-mode-syntax-table)
  (push `(,(rx (or space "(" "[" "{") (group (zero-or-one "#") ":" (+ (not (any space ")" "]" "}")))))
          (1 font-lock-keyword-face))
        scribble-mode-font-lock-keywords))


;; ------ slime-company ------
(use-package slime-company
  :after (slime company scribble-mode)
  :config
  (push 'scribble-mode slime-company-major-modes) ; <-- This is the important line for slime-company
  (setq slime-company-completion 'fuzzy))
```

Happy hacking! :D
