

<a id="TITLE:ADP-DOCS:TAG1"></a>
# Add Documentation\, Please\.\.\.

Welcome to ADP\!

* [Introduction](//home/hectarea/common-lisp/adp/README.md#TITLE:ADP-DOCS:TAG2)
* [Installation](//home/hectarea/common-lisp/adp/README.md#TITLE:ADP-DOCS:TAG3)
* [Exporters](//home/hectarea/common-lisp/adp/README.md#TITLE:ADP-DOCS:TAG4)
* [Documentation as another system](//home/hectarea/common-lisp/adp/README.md#TITLE:ADP-DOCS:TAG5)
  * [Enabling scribble files](//home/hectarea/common-lisp/adp/README.md#TITLE:ADP-DOCS:TAG6)
  * [Generating the files](//home/hectarea/common-lisp/adp/README.md#TITLE:ADP-DOCS:TAG7)
  * [Defining the main elements](//home/hectarea/common-lisp/adp/README.md#TITLE:ADP-DOCS:TAG9)
  * [The system and the files](//home/hectarea/common-lisp/adp/README.md#TITLE:ADP-DOCS:TAG10)
  * [The interface](//home/hectarea/common-lisp/adp/README.md#TITLE:ADP-DOCS:TAG11)


<a id="TITLE:ADP-DOCS:TAG2"></a>
## Introduction

Simply put\, ADP adds scribble files support to ASDF\. Thus\, you can add scribble files and use the well\-known [at syntax](https://docs.racket-lang.org/scribble/reader.html)\.

Also\, check out [cl\-scribble\-mode](https://github.com/HectareaGalbis/cl-scribble-mode) to edit scribble files with Common Lisp symbols\.

Examples\:

<img src="/images/cl-scribble-mode-example.gif" alt="cl-scribble-mode example" width="100%">

<img src="/images/example-adp.png" alt="adp example" width="100%">


<a id="TITLE:ADP-DOCS:TAG3"></a>
## Installation

This project is available on Quicklisp with the name ```adp```\. But\, instead of installing this\, you should install an exporter\.

This project should be installed only if you want to make an exporter\.

<a id="TITLE:ADP-DOCS:TAG4"></a>
## Exporters

* [adp\-github](https://github.com/HectareaGalbis/adp-github)\: Generates Github flavoured Markdown files\.


<a id="TITLE:ADP-DOCS:TAG5"></a>
## Documentation as another system

Almost every Common Lisp project has several systems\. These systems usually indicate which files of code should be compiled and loaded and in which order\. There are\, sometimes\, other systems that does not load the project\'s code\, but does load unit tests to verify the correctness of the project\. It seems natural now that documentation should have its own system to document the project\.

ADP extends ASDF with Scribble files\. These format is used exhaustively by the Racket language and it proves that Scribble is a very good language for writing documentation\.

However\, Racket and Common Lisp are different\. That\'s why ADP tries to integrate Scribble into Common Lisp making some changes in favour to flexibility and power\. For example\, enabling the at\-syntax requires changing the readtable to use the scribble language\. But\, the Common Lisp programmer can still select which readtable to use in every Scribble file\. The package can also be choosen as normally\.

In the documentation system\, the programmer can still specify lisp files to write auxiliar functions or macros to use later within Scribble files\. Remember that Scribble is just syntactic sugar for function calls\.

`````common-lisp
;; Common Lisp
(+ 3 4)

;; Scribble
@+[3 4]
`````

<a id="TITLE:ADP-DOCS:TAG6"></a>
### Enabling scribble files

ADP cannot be used directly to enable Scribble files\. Instead\, you need to load an exporter like [adp\-github](https://github.com/HectareaGalbis/adp-github)\.

Enabling scribble files in your system requires 2 things\:

* Add the exporter\'s system to the ```:defsystem-depends-on``` list\:


`````common-lisp
(defsystem "my-system/docs"
  :defsystem-depends-on ("adp-github")
  ...)
`````

* Specify the class of the system\:


`````common-lisp
(defsystem "my-system/docs"
  :defsystem-depends-on ("adp-github")
  :class :adp-github
  ...)
`````

And now\, add as many files as you want\. This can be a valid example of a documentation system\:

`````common-lisp
(defsystem "my-project/docs"
  :defsystem-depends-on ("adp-github")
  :class :adp-github
  :depends-on ("my-project")
  :components ((:module "scribble"
                :components ((:file "package")
                             (:file "custom-functions")
                             (:scribble "reference")
                             (:scribble "README")))))
`````

The system class ```:adp-github``` and the name of Scribble files like ```:scribble``` are defined by the exporter\. Each exporter might define one or more types of scribble files\.


<a id="TITLE:ADP-DOCS:TAG7"></a>
### Generating the files

Once the system is defined\, nothing special is required\. Just load the system\.

`````common-lisp
(asdf:load-system "my-project/docs")
`````

Or\, equivalently\:

`````common-lisp
(asdf:make "my-project/docs")
`````

The files will be generated according to the used exporter\.


<a id="TITLE:ADP-DOCS:TAG8"></a>
## Making an exporter

All the exporters must have the next elements\:

1. A system class\.
2. A file class\.
3. An implementation of the method [adp\:export\-content](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:EXPORT-CONTENT)\.


Let\'s do an example of how to start making an exporter\.

<a id="TITLE:ADP-DOCS:TAG9"></a>
### Defining the main elements

Suppose that our exporter is named ```my-exporter```\. Let\'s create a project with this name\. As every project\, we need to define the main system\:

`````common-lisp
(defsystem "my-project"
  :depends-on ("adp")
  :components ((:file "package")
               (:file "exporter")))
`````

Let\'s jump directly into the ```exporter``` file assuming we defined the package named ```exp```\.

As we said\, we need to define a system class\. This is done with the macro [adp\:define\-adp\-system](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFINE-ADP-SYSTEM)\:

`````common-lisp
(in-package #:exp)

(adp:define-adp-system my-exporter)
`````

In this case\, we named the system class ```my-exporter```\.

The second thing to define is at least one file class\. We\'re using [adp\:define\-adp\-file](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFINE-ADP-FILE)\. Let\'s name it ```expo```\, for example\:


`````common-lisp
(in-package #:exp)

(adp:define-adp-system my-exporter)

(adp:define-adp-file expo)
`````

The macro [adp\:define\-adp\-file](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFINE-ADP-FILE) receives additional optional arguments\. See the [\(reference\)](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFINE-ADP-FILE) for details\.

Lastly\, we only need to implement the method [adp\:export\-content](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:EXPORT-CONTENT)\. In this case\, it will receive the following arguments\:

* An object of type ```my-exporter```\.
* A list of objects of type ```expo```\.


Remember that ```my-exporter``` was the system class we defined with [adp\:define\-adp\-system](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFINE-ADP-SYSTEM) and ```expo``` is the file class we defined with [adp\:define\-adp\-file](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFINE-ADP-FILE)\.

`````common-lisp
(in-package #:exp)

(adp:define-adp-system my-exporter)

(adp:define-adp-file expo)

(defmethod adp:export-content ((system my-exporter) files)
  ...)
`````

Note that we must specialize the first argument with our system class\.

<a id="TITLE:ADP-DOCS:TAG10"></a>
### The system and the files

So\, this is the minimum required to make an exporter\. The rest is up to you\. But\, at least\, you must know how to handle the objects ```system``` and ```files```\. Let\'s see how to retrieve information from them\.

* ```system```\: The class we defined named ```my-exporter``` has as direct superclass the class ```asdf:system```\. So\, you have all the ```asdf``` methods to know perfectly how the system is\. Here are some useful functions\:
  * ```asdf:component-name```\: You can retrieve the system\'s name\.
  * ```asdf:system-source-directory```\: Returns the absolute pathname of the system\'s directory\.
* ```files```\: Each file of a type defined with [adp\:define\-adp\-file](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFINE-ADP-FILE) has two attributes whose values can be retrieved with the following getters\:
  * [adp\:file\-component](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:FILE-COMPONENT)\: Returns the object of type ```asdf:component``` that represents the file within the system\. Similarly to the ```asdf:system```\, there are useful functions from ```asdf``` that we can use\:
    * ```asdf:component-pathname```\: Retrieves the absolute pathname of a component\. In this case\, the pathname of the file\.
  * [adp\:file\-elements](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:FILE-ELEMENTS)\: Returns the elements of the file that needs to be printed\. These elements are also objects with two attributes\. We can retrieve their values with the following functions\:
    * [adp\:element\-value](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:ELEMENT-VALUE)\: The actual value to be printed\. It can be an integer\, a string\, a float\, or even objects that needs to be printed in a special way\.
    * [adp\:element\-form](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:ELEMENT-FORM)\: The lisp form that produced the value\. This can be useful for error messages\.


Knowing this\, every exporter could work similarly\. Just loop over files\, loop over the elements of each file\, and print them into a file stream\.

`````common-lisp
(defmethod adp:export-content ((system my-exporter) files)
  (loop for file in files
        for file-component = (adp:file-component file)
        for file-elements = (adp:file-elements file)
        do (with-some-file-stream stream file-component
             (loop for file-element in file-elements
                   for element-value = (adp:element-value file-element)
                   for element-form = (adp:element-form file-element)
                   do (handler-case (print-element element-value)
                        (error (c)
                          (error "Error while printing the value from ~s" element-form)))))))
`````

<a id="TITLE:ADP-DOCS:TAG11"></a>
### The interface

The Scribble language was defined specifically for Racket\. But Common Lisp works differently\. And this is true for functions\. In Racket\, keyword arguments can be placed wherever you want\, but in Common Lisp they can only be used after every required and optional arguments\.

Consider for example the function ```title``` that can receive a ```tag``` keyword argument\.

In Racket\, it could be defined like this\:

`````racket
(define title (:tag tag . text)
  ...)
`````

And it can be used like this\:

`````scribble
@title[:tag "some-tag"]{This is the title.}
`````

However\, in Common Lisp this is impossible with the usual [DEFUN](http://www.lispworks.com/reference/HyperSpec/Body/m_defun.htm)\:

`````common-lisp
(defun title (&key tag &rest elements) ;; <- Error, &key is before &rest
  ...)

(defun title (&rest elements &key tag) ;; <- Only accepts 1 keyword arguments and not a variable quantity.
  ...)
`````

Because of this\, ADP defines the macros [adp\:defun](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFUN) and [adp\:defmacro](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFMACRO)\.

Now the example is pretty easy\:

`````common-lisp
(adp:defun title (:tag tag &rest elements)
  ...)
`````

Or using the racket style\:

`````common-lisp
(adp:defun title (:tag tag . elements)
  ...)
`````

This macros are adapted to Common Lisp\. We\'ve just saw the use of ```&rest```\. But\, we can also specify the supplied symbol to optional arguments\.

`````common-lisp
(adp:defun variable-reference (type :style (style "default-style" stylep) :variable variable &rest text)
  (if stylep
      ...
      ...))

@variable-reference['constant :variable 'some-var :style "another-style"]{the variable you like}
`````

The macro [adp\:defmacro](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFMACRO) works the same way\. In fact\, it doesn\'t accept destructuring\. The only difference is that arguments are not evaluated\. This is useful if we want to avoid the ```'``` character before symbols\.

`````common-lisp
(adp:defmacro variable-ref (type :style (style "default-style" stylep) :variable variable &rest text)
  `(variable-reference ',type :style ,style :variable ',variable ,@text))

@variable-ref[constant :variable some-var :style "another-style"]{the variable you like}
`````

Lastly\, ADP offers the function [adp\:function\-lambda\-list](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:FUNCTION-LAMBDA-LIST) for retrieving the lambda list for functions and macros defined with [adp\:defun](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFUN) and [adp\:defmacro](//home/hectarea/common-lisp/adp/README.md#FUNCTION:ADP:DEFMACRO)\.

`````common-lisp
(adp:function-lambda-list 'variable-ref)

;; Returns
(type :style (style "default-style" stylep) :variable variable &rest text)
`````