

<a id="TITLE:ADP-DOCS:TAG52"></a>
# Add Documentation\, Please\.\.\.

Welcome to ADP\!

Simply put\, ADP adds scribble files support to ASDF\. Thus\, you can add scribble files and use the well\-known [at syntax](https://docs.racket-lang.org/scribble/reader.html)\.

Also\, check out [cl\-scribble\-mode](https://github.com/HectareaGalbis/cl-scribble-mode) to edit scribble files with Common Lisp symbols\.

Examples\:

<img src="/images/cl-scribble-mode-example.gif" alt="cl-scribble-mode example" width="100%">

<img src="/images/example-adp.png" alt="adp example" width="100%">


<a id="TITLE:ADP-DOCS:TAG53"></a>
## Installation

This project is available on Quicklisp\. But\, instead of installing this\, you should install an exporter\.


<a id="TITLE:ADP-DOCS:TAG54"></a>
## Exporters

* [adp\-github](https://github.com/HectareaGalbis/adp-github)\: Generates Github flavoured Markdown files\.


<a id="TITLE:ADP-DOCS:TAG55"></a>
## Documentation as another system

Almost every Common Lisp project can have several systems\. These systems usually indicate which files of code should compiled and loaded and in which order\. There are\, sometimes\, other systems that does not load the project\'s code\, but does load unit tests to verify the correctness of the project\. It seems natural now that documentation should have its own system to document the project\.

ADP extends ASDF with Scribble files\. These format is used exhaustively by the Racket language and it proves that Scribble is a very good language for writing documentation\.

However\, Racket and Common Lisp are different\. That\'s why ADP tries to integrate Scribble into Common Lisp making some changes in favour to flexibility and power\. For example\, enabling the at\-syntax requires changing the readtable to use the scribble language\. But\, the Common Lisp programmer can still select which readtable to use in every Scribble file\. The package can also be choosen as normally\.

In this documentation system\, the programmer can still specify lisp files to write auxiliar functions or macros to use later within Scribble files\. Remember that Scribble is just syntactic sugar for function calls\.

`````common lisp
;; Common Lisp
(+ 3 4)

;; Scribble
@+[3 4]
`````

<a id="TITLE:ADP-DOCS:TAG56"></a>
### Enabling scribble files

ADP cannot be used directly to enable Scribble files\. Instead\, you need to load an exporter like [adp\-github](https://github.com/HectareaGalbis/adp-github)\.

Enabling scribble files in your system requires 2 things\:

* Add the exporter\'s system to the ```:defsystem-depends-on``` list\:


`````common lisp
(defsystem "my-system/docs"
  :defsystem-depends-on ("adp-github")
  ...)
`````

* Specify the class of the system\:


`````common lisp
(defsystem "my-system/docs"
  :defsystem-depends-on ("adp-github")
  :class :adp-github
  ...)
`````

And now\, add as many files as you want\. This can be a valid example of a documentation system\:

`````common lisp
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


<a id="TITLE:ADP-DOCS:TAG57"></a>
### Generating the files

Once the system is defined\, nothing special is required\. Just load the system\.

`````common lisp
(asdf:load-system "my-project/docs")
`````

Or\, equivalently\:

`````common lisp
(asdf:make "my-project/docs")
`````

The files will be generated according to the used exporter\.