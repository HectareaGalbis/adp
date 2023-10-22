
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

Each exporter is different, and you should read their own documentation. However, all exporters have something in common. They will define a new `ASDF` operator. For example, the exporter named `adp-github` defines the operator `adp-github-op`. Then, to generate the latex files from the scribble ones, you should do the following:

``` common-lisp
(asdf:operate "adp-github-op" "my-project")
```

## Two different modes

The at-syntax is really cenvenient for writing text. But lisp is not a good language for this. That's why we want scribble files. Scribble files are in text-mode while common lisp source files are in lisp-mode:

* Lisp files: Lisp-mode
* Scribble files: Text-mode

### Text mode

Scribbles are in text mode. By default, the text you write in this files are gathered by ADP as strings. Actually, it can gather every type of object, it doesn't ignore anything. It is the job of exporters processing or ignoring this objects.

Scribble is mainly used in Racket, where you can choose the language to use with a line like `#lang scribble/base` or similar. With ADP, instead of selecting a language, you can select a package. In text mode, everything is a string unless you use the at-syntax. However, there is an exception: If the first expression is a call to `in-package`, then it will be processed as normal; then, the rest of the file will be processed in text mode.

This could be an example of `adp-github`:

``` common-lisp
(in-package #:adp-github)

@header{This is the title}

This is an example of text. It is just text and it will be a string when ADP processes this file.

@; This is a comment and will be ignored

This is text with an @italic{italized} word. Newlines are
also 

recognized.

Numbers like @(+ 3 4) or @+[5 7] are also gathered.
```

### Lisp mode

ADP can also gather element from lisp source files. However, we need to tell explicitly that we want to do it. Instead of using the at-syntax (`@`), we need to use the sheat-syntax (`#@`).

This could be an example of `adp-github`:

``` common-lisp
(in-package #:my-package)

#@adpgh:header{This is the title}

#@adpgh:text{
This is an example of text and contains
newlines

and an @adpgh:italic{italized} word.
}

(defun foo ()
  ;; ...
  )

#@adpgh:subheader{This is the section with the number @+[2 3]} ;; ... with the number 5

;; And more stuff
```

Only the top-level expressions must use the sheat-syntax. Once we enter in text mode, we can use again the at-syntax.

## Installation

This project is available on Quicklisp. But, instead of installing this, you should install an exporter.


## Exporters

* [adp-plain](https://github.com/Hectarea1996/adp-plain): Generates files with plain text.
* [adp-github](https://github.com/Hectarea1996/adp-github): Generates Github flavoured Markdown files with crossreferences, tables of contents, etc.


## How data is stored

The data is stored as a vector of files (actually, the file is an object representing a file). Each file contains the elements gathered by a lisp file or a scribble file. The order in which files are loaded is the same as the order they are stored in.

Each file, contains the `ASDF` component it represents as well. Files have then two accessors:

* `file-component`: Retrieves the `ASDF` component of a file.
* `file-elements`: Retrieves the elements of a file.

The elements are stored as a vector and each element is just an object. It can be a string, a number, a list, an instance of a class, etc.

When all the data is gathered, it is passed to the generic function `adp:export-content`. 

## How to create an exporter

Creating an exporter is really easy. Let's do an example of an exporter that creates txt files and `princ`s every object that ADP gathers.

An exporter must be its own project to be accessible via Quicklisp. Let's create the `asd` file:

``` common-lisp
(defsystem "adp-princ"
  :depends-on ("adp")
  :components ((:file "package")
               (:file "adp-princ")))
```

Of course, you can use whatever number of files you want. The exporter can use also whatever package you want; suppose we're using the package `ADP-PRINC`. So, let's see now the file `adp-princ`:

``` common-lisp
(in-package #:adp-princ)

(adp:define-adp-operation adp-princ-op)

...
```

First of all, we need to define an adp operation. It is actually an `ASDF` operation that inherits from another class. 

After this, we can now implement a method using that operation:

``` common-lisp
(in-package #:adp-princ)

(adp:define-adp-operation adp-princ-op)

(defmethod adp:export-content ((op adp-princ-op) files system)
  ...)
```

As you can see, the method receives the operation we just defined, the files `ADP` has gathered and the `ASDF` system component. Like files is a vector, we only need to iterate over it:

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
                 (loop for element across (file-elements file)
                       do (princ element file-str))))))
```

For every file, we are creating first the target pathname. It is the same as the source file, but with the type "txt". If it doesn't exist, we create the file and `princ` every object in it.

And that's all! 

There are also generic functions that allow you to make some preparation before/after the system or a file is loaded. These are `adp:pre-process-system`, `adp:post-process-system`, `adp:pre-process-file` and `adp:post-process-file`. 

Lastly, the symbol adp:*adp* is also exported and indicates if ADP is enabled while loading a file. Macros like `adp-github:defun` from the `adp-github` exporter makes use of it.

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
  ;; We modify the syntax to adapt scribble-mode to the Common Lisp languages.
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
