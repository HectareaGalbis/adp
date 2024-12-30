
(in-package #:adp-docs)

@output-file["/README.md"]

@title[:toc nil]{Add Documentation, Please...}

Welcome to ADP!

@table-of-contents[]

@subtitle{Introduction}

Simply put, ADP adds scribble files support to ASDF. Thus, you can add scribble files and use the well-known @link[:address "https://docs.racket-lang.org/scribble/reader.html"]{at syntax}.

Also, check out @link[:address "https://github.com/HectareaGalbis/cl-scribble-mode"]{cl-scribble-mode} to edit scribble files with Common Lisp symbols.

Examples:

@image[#P"images/cl-scribble-mode-example.gif" :alt-text "cl-scribble-mode example"]

@image[#P"images/example-adp.png" :alt-text "adp example"]


@subtitle{Installation}

This project is available on Quicklisp with the name @code{adp}. But, instead of installing this, you should install an exporter.

This project should be installed only if you want to make an exporter.

@subtitle{Exporters}

@itemize[
        @item{@link[:address "https://github.com/HectareaGalbis/adp-github"]{adp-github}: Generates Github flavoured Markdown files.}
]

@subtitle{Documentation as another system}

Almost every Common Lisp project has several systems. These systems usually indicate which files of code should be compiled and loaded and in which order. There are, sometimes, other systems that does not load the project's code, but does load unit tests to verify the correctness of the project. It seems natural now that documentation should have its own system to document the project.

ADP extends ASDF with Scribble files. These format is used exhaustively by the Racket language and it proves that Scribble is a very good language for writing documentation.

However, Racket and Common Lisp are different. That's why ADP tries to integrate Scribble into Common Lisp making some changes in favour to flexibility and power. For example, enabling the at-syntax requires changing the readtable to use the scribble language. But, the Common Lisp programmer can still select which readtable to use in every Scribble file. The package can also be choosen as normally.

In the documentation system, the programmer can still specify lisp files to write auxiliar functions or macros to use later within Scribble files. Remember that Scribble is just syntactic sugar for function calls.

@code-block[:lang "common lisp"]|{
;; Common Lisp
(+ 3 4)

;; Scribble
@+[3 4]
}|

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


@subtitle{Making an exporter}

All the exporters must have the next elements:

@enumerate[
        @item{A system class.}
        @item{A file class.}
        @item{An implementation of the method @fref[adp:export-content].}
]

Let's do an example of how to start making an exporter.

@subsubtitle{Defining the main elements}

Suppose that our exporter is named @code{my-exporter}. Let's create a project with this name. As every project, we need to define the main system:

@code-block[:lang "common lisp"]{
(defsystem "my-project"
  :depends-on ("adp")
  :components ((:file "package")
               (:file "exporter")))
}

Let's jump directly into the @code{exporter} file assuming we defined the package named @code{exp}.

As we said, we need to define a system class. This is done with the macro @fref[adp:define-adp-system]:

@code-block[:lang "common lisp"]{
(in-package #:exp)

(adp:define-adp-system my-exporter)
}

In this case, we named the system class @code{my-exporter}.

The second thing to define is at least one file class. We're using @fref[adp:define-adp-file]. Let's name it @code{expo}, for example:


@code-block[:lang "common lisp"]{
(in-package #:exp)

(adp:define-adp-system my-exporter)

(adp:define-adp-file expo)
}

The macro @fref[adp:define-adp-file] receives additional optional arguments. See the @fref[adp:define-adp-file]{reference} for details.

Lastly, we only need to implement the method @fref[adp:export-content]. In this case, it will receive the following arguments:

@itemize[
        @item{An object of type @code{my-exporter}.}
        @item{A list of objects of type @code{expo}.}
]

Remember that @code{my-exporter} was the system class we defined with @fref[adp:define-adp-system] and @code{expo} is the file class we defined with @fref[adp:define-adp-file].

@code-block[:lang "common lisp"]{
(in-package #:exp)

(adp:define-adp-system my-exporter)

(adp:define-adp-file expo)

(defmethod adp:export-content ((system my-exporter) files)
  ...)
}

Note that we must specialize the first argument with our system class.

@subsubtitle{The system and the files}

So, this is the minimum required to make an exporter. The rest is up to you.

But, at least, you must know how to handle the objects @code{system} and @code{files}. Let's see how to retrieve information from them.

@itemize[
        @item{@code{system}: The class we defined named @code{my-exporter} has as direct superclass the class @code{asdf:system}. So, you have all the @code{asdf} methods to know perfectly how the system is. Here are some functions that I found to be useful:}
        @itemize[
                @item{@code{asdf:component-name}: You can retrieve the system's name.}
                @item{@code{asdf:system-source-directory}: Returns the absolute pathname of the system's directory.}
        ]
        @item{@code{files}: Each file of a type defined with @fref[adp:define-adp-file] has two attributes whose values can be retrieved with the following getters:}
        @itemize[
                @item{@fref[adp:file-component]: Returns the object of type @code{asdf:component} that represents the file within the system. Similarly to the @code{asdf:system}, there are useful functions from @code{asdf} that we can use:}
                @itemize[
                        @item{@code{asdf:component-pathname}: Retrieves the absolute pathname of a component. In this case, the pathname of the file.}
                ]
                @item{@fref[adp:file-elements]: Returns the elements of the file that needs to be printed. These elements are also object with two attributes. We can retrieve their values with the following functions:}
                @itemize[
                        @item{@fref[adp:element-value]: The actual value to be printed. It can be an integer, a string, a float, or even objects that needs to be printed in a special way.}
                        @item{@fref[adp:element-form]: The lisp form that produced the value. This can be useful for error messages.}
                ]
        ]
]

Knowing this, every exporter could work similarly. Just loop over files, loop over the elements of each file, and print them into a file stream.

@code-block[:lang "common lisp"]{
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
}

@subsubtitle{The interface}

The Scribble language was defined specifically for Racket. But Common Lisp works differently. And this is true for functions. In Racket, keyword arguments can be placed wherever you want, but in Common Lisp they can only be used after every required and optional arguments.

Consider for example the function @code{title} that can receive a @code{tag} keyword argument.

In Racket, it could be defined like this:

@code-block[:lang "racket"]{
(define title (:tag tag . text)
  ...)
}

And it can be used like this:

@code-block[:lang "racket"]|{
@title[:tag "some-tag"]{This is the title.}
}|

However, in Common Lisp this is impossible with the usual @clref[defun]:

@code-block[:lang "racket"]{
(defun title (&key tag &rest elements) ;; <- Error, &key is before &rest
  ...)

(defun title (&rest elements &key tag) ;; <- Only accepts 1 keyword arguments and not a variable quantity.
  ...)
}

Because of this, ADP defines the macros @fref[adp:defun] and @fref[adp:defmacro].

Now the example is pretty easy:

@code-block[:lang "common lisp"]{
(adp:defun title (:tag tag &rest elements)
  ...)
}

Or using the racket style:

@code-block[:lang "common lisp"]{
(adp:defun title (:tag tag . elements)
  ...)
}

This macros are adapted to Common Lisp. We've just saw the use of @code{&rest}. But, we can also specify the supplied symbol to optional arguments.

@code-block[:lang "common lisp"]|{
(adp:defun variable-reference (type :style (style "default-style" stylep) :variable variable &rest text)
  (if stylep
      ...
      ...))

@variable-reference['constant :variable 'some-var :style "another-style"]{the variable you like}
}|

The macro @fref[adp:defmacro] works the same way. In fact, it doesn't accept destructuring. The only difference is that arguments are not evaluated. This is useful if we want to avoid the @code{'} character before symbols.

@code-block[:lang "common lisp"]|{
(adp:defmacro variable-ref (type :style (style "default-style" stylep) :variable variable &rest text)
  `(variable-reference ',type :style ,style :variable ',variable ,@text))

@variable-ref[constant :variable some-var :style "another-style"]{the variable you like}
}|

Lastly, ADP offers the function @fref[adp:function-lambda-list] for retrieving the lambda list for functions and macros defined with @fref[adp:defun] and @fref[adp:defmacro].

@code-block[:lang "common lisp"]{
(adp:function-lambda-list 'variable-ref)

;; Returns
(type :style (style "default-style" stylep) :variable variable &rest text)
}
