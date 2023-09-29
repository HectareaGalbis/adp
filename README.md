
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

## Two different modes

The at-syntax is really cenvenient for writing text. But lisp is not a good language for this. That's why we want scribble files. Scribble files are in text-mode while common lisp source files are in lisp-mode:

* Lisp files: Lisp-mode
* Scribble files: Text-mode

The at-syntax can be used in both modes. This is because you can also generate text files from lisp files. Each exporter should (or not) export functions and macros to use in lisp files which will give to ADP information to generate a text file. A text file will be created from a lisp file if that file uses some of those functions or macros; otherwise, the file is not created.


## Installation

Add [Ultralisp](https://ultralisp.org/) to Quicklisp:

```common-lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
```


## Exporters

* [adp-plain](https://github.com/Hectarea1996/adp-plain): Generates files with plain text.


## How data is gathered

The data is gathered differently on lisp files and scribble files.


### Data from scribble files

By default, all text written in scribble files is gathered as a string. This string can be splitted up by other kind of types if you use the at-syntax. 

``` text
All this text is a string.

A string with the number @+[3 4] in the middle.
```

The data gathered by this scribble file should be: `"All this text is a string." "\n" "\n" "A string with the number " 7 " in the middle."`. So, the data gathered is the objects that are present in the file, whether they are literals or objects returned by a function or macro. This data is then processed by an exporter.

Every type of common lisp is supported, nothing is discarded. It is the exporter that can ignore, warn or raise an error if some object is not wanted (by that exporter).


### Data from common lisp files

The data cannot be gathered like in scribble files. The objects that are the result of evaluating any form in a lisp file is just ignored and cannot be gathered. So, in lisp files we need to call a function that tells ADP to gather some piece of data. The end user doesn't need to bother about this because exporters should define functions or macros to be used in lisp mode. 

If you are making an exporter, you should use the function `adp:add-element`. Keep reading to see more details.


## How data is stored

The data is stored as a hash map of pathnames (keys) and files (values) (actually, the file is an object representing a file). Each file contains the elements gathered by a lisp file or a scribble file. The order in which files are loaded is not guaranted to be the same as the order they are stored in. On the other hand, the elements in a file are.

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
  :defsystem-depends-on ("adp")
  :components ((:file "package")
               (:file "adp-princ")))
```

Note that we have used `:defsystem-depends-on` instead of `:depends-on`. This is needed if we want to use scribble files.

Of course, you can use whatever number of files you want. The exporter can use also whatever package you want; suppose we're using the package `ADP-PRINC`. So, let's see now the file `adp-princ`:

``` common-lisp
(in-package #:adp-princ)

(adp:define-adp-operation adp-princ-op)

...
```

First of all, we need to define an adp operation. It is actually an `ASDF` operation that inherits from another class and makes some imports and exports. 

After this, we can now implement a method using that operation:

``` common-lisp
(in-package #:adp-princ)

(adp:define-adp-operation adp-princ-op)

(defmethod adp:export-content ((op adp-princ-op) files system)
  ...)
```

As you can see, the method receives the operation we just defined, the files `ADP` has gathered and the `ASDF` system component. Like files is a hash map, we are using `maphash`:

``` common-lisp
(defmethod adp:export-content ((op adp-princ-op) files system)
  (maphash (lambda (file-path file)
             (let ((target-file (make-pathname :directory (pathname-directory 
                                                            (merge-pathnames file-path
                                                                             (asdf:system-source-directory system)))
                                               :name (pathname-name file-path)
                                               :type "txt")))
               (with-open-file (file-str target-path :direction :output :if-exists :supersede
                                                     :if-does-not-exist :create)
                 (loop for element across (file-elements file)
                       do (princ element file-str)))))
           files))
```

For every file, we are creating first the target pathname. It is the same as the source file, but with the type "txt". If it doesn't exist, we create the file and `princ` every object in it.


## Defining functions for users

Imagine that you want to make an `html` exporter. Maybe you'll want a function named `header` that prints the `h1` directive. And you want users to use like this:

``` common-lisp
@header{The title}
```

Before defining the function (or functions) let's change a bit the `adp:export-content` method we've defined above. We are changing `princ` to a call to a genenric function named `process-element`:

``` common-lisp
(defgeneric process-element (element stream)
  (:method ((element t) stream)
    (princ element stream)))

(defmethod adp:export-content ((op adp-princ-op) files system)
  (maphash (lambda (file-path file)
             (let ((target-file ...))
               (with-open-file (file-str target-path ...)
                 (loop for element across (file-elements file)
                       do (process-element element file-str)))))
           files))
```

By default, let `process-element` `princ` an element. With this generic function we can process the elements differently depending on its type. So, let's create a new type that will represent a header.

``` common-lisp
(defclass header-object ()
  ((title :initarg :title
          :reader header-title
          :type string)))
          
(defmethod process-element ((element header-object) stream)
  (format stream "<h1>~a</h1>" (header-title element)))
```

It is clear that this function needs more job taking care of dangerous characters. But let's keep this easy.

Remember that users will have two different modes where functions can be used: lisp-mode and text-mode. Each mode must use different kind of functions. In text mode it will return the object to be printed but in lisp mode we must use `adp:add-element`.


### Defining a function for lisp-mode

We must use the function `adp:add-element` to add elements to the current lisp file being processed. Let's make a function that adds a header object:

``` common-lisp
(defun header (title)
  (adp:add-element (make-instance 'header-object :title title)))
```

If we export the symbol `header` in our package, then this will be sufficient to the user:

``` common-lisp
(in-package #:user-package)

@adp-html:header{The title}

;; More code
...
```

However, there is a subtle 'error' here. The function `header` must add the element when the user generates the documentation. But the function will be called even if the user loads the system as usually. That's not correct.

To fix that, we can use the symbol `adp:*adp*`. It will be `t` when `ADP` is gathering data and `NIL` otherwise. So, in order to avoid a function call as well, we can make a macro:

``` common-lisp
(defmacro header (title)
  (when adp:*adp*
    `(adp:add-element (make-instance 'header-object :title ,title))))
```

Now, if the user just loads the system, `header` will expand to `NIL`, i.e. a no-op.


### Defining a function for text-mode

Instead of using `adp:add-element`, we just need to return the object. First, let's define the function:

``` common-lisp
(defun header (title)
  (make-instance 'header-object :title title))
```

I'm sure you are seeing here something wrong. We just defined a macro with same name before. But we want the same name in both lisp-mode and text-mode.

To fix this, first we need to know a bit more about scribble files. Wonder this: Remember that in lisp, at any time a package is current, so... which package is current in scribble files? The answer is the package `adp-user`. Every function that could be used in scribble files must be imported into the package `adp-user`.

So, we can define our function like this:

``` common-lisp
(defun adp-user::header (title)
  (make-instance 'header-object :title title))
```

Using `adp-user::header` (note the double colon) the symbol header is imported to `adp-user`.

And that's all, users can now use `@header{The title}` in scribble files as we wanted.
