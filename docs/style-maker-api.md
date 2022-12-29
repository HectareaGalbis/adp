<h1 id="header:ADP-STYLE-MAKER:STYLE-MAKER-API-HEADER">Style-maker API</h1>

<h2 id="header:ADP:HEADERTAG2">ADP pprint dispatch</h2>

<h4 id="symbol:ADP-STYLE-MAKER:*ADP-PPRINT-DISPATCH*">Variable: *adp-pprint-dispatch*</h4>

```Lisp
(defvar adpsm:*adp-pprint-dispatch* adppvt:*adp-pprint-dispatch*)
```

````
ADP custom pprint dispatch table to make the code printing look better. The main difference is when the
package extension of a symbol is printed. It will only print the extension package when a symbol is exported.
````

<h2 id="header:ADP:HEADERTAG3">Style parameters</h2>

<h4 id="function:ADP-STYLE-MAKER:DEFINE-STYLE-PARAMETER">Macro: define-style-parameter</h4>

```Lisp
(defmacro adpsm:define-style-parameter (name &key (value nil) (key nil)
                                        (required nil))
  ...)
```

````
Define a style parameter. A new parameter is defined (using defparameter) and also a style parameter is added.
A style parameter is a keyword parameter added to ADP:LOAD-SYSTEM. NAME is the name of the parameter. VALUE is
the default value. KEY is the keyword of the style parameter. If KEY is not specified, the keyword is obtained
using the symbol name of NAME. If REQUIRED is non-NIL, the user must use this parameter when using
ADP:LOAD-SYSTEM.
````

<h2 id="header:ADP:HEADERTAG4">Writers</h2>

<h4 id="function:ADP-STYLE-MAKER:DEFINE-BEGIN-FILE-WRITER">Macro: define-begin-file-writer</h4>

```Lisp
(defmacro define-begin-file-writer ((stream) &body body)
  ...)
```

````
Define a function that will be called when the file is about to be written in. The function receives a stream 
associated with said file.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-END-FILE-WRITER">Macro: define-end-file-writer</h4>

```Lisp
(defmacro define-end-file-writer ((stream) &body body)
  ...)
```

````
Define a function that will be called after a file has finished of being written in. The function receives a
stream associated with said file.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-FILE-EXTENSION">Macro: define-file-extension</h4>

```Lisp
(defmacro adpsm:define-file-extension (nil &body body)
  ...)
```

````
Define a function that must return a string indicating the extension of the files that ADP will create.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-BEGIN-PROJECT-WRITER">Macro: define-begin-project-writer</h4>

```Lisp
(defmacro define-begin-project-writer ((root-directory) &body body)
  ...)
```

````
Define a function that will be called before the documentation of a project begins to generate. The function 
receives the pathname of the project root directory.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-END-PROJECT-WRITER">Macro: define-end-project-writer</h4>

```Lisp
(defmacro define-end-project-writer ((root-directory) &body body)
  ...)
```

````
Define a function that will be called after the documentation of a project has been generated. The function
receives the pathname of the project root directory.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-HEADER-WRITER">Macro: define-header-writer</h4>

```Lisp
(defmacro adpsm:define-header-writer ((stream title tag) &body body)
  ...)
```

````
Define a function that must print a header element. The function receives the stream to be written in, the
header name and the tag associated to it.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-SUBHEADER-WRITER">Macro: define-subheader-writer</h4>

```Lisp
(defmacro adpsm:define-subheader-writer ((stream title tag) &body body)
  ...)
```

````
Same as define-header-writer but it must print a subheader element.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-SUBSUBHEADER-WRITER">Macro: define-subsubheader-writer</h4>

```Lisp
(defmacro adpsm:define-subsubheader-writer ((stream title tag) &body body)
  ...)
```

````
Same as define-header-writer but it must print a subsubheader element.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-TEXT-WRITER">Macro: define-text-writer</h4>

```Lisp
(defmacro adpsm:define-text-writer ((stream text) &body body)
  ...)
```

````
Define a function that must print a text element. It receives the stream to be written in, and a string with
the text.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-ESCAPE-TEXT">Macro: define-escape-text</h4>

```Lisp
(defmacro adpsm:define-escape-text ((text) &body body)
  ...)
```

````
Define a function that receives a string of text and must return another string. You want this to escape
special characters that will be used with bold, italic, header-ref, web-link, etc.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-BOLD-WRITER">Macro: define-bold-writer</h4>

```Lisp
(defmacro adpsm:define-bold-writer ((stream text) &body body)
  ...)
```

````
Define a function to print a bold text element. It receives the stream to be written in, and a string with
the text.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-ITALIC-WRITER">Macro: define-italic-writer</h4>

```Lisp
(defmacro adpsm:define-italic-writer ((stream text) &body body)
  ...)
```

````
Same as define-bold-writer, but with an italic style.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-EMPHASIS-WRITER">Macro: define-emphasis-writer</h4>

```Lisp
(defmacro adpsm:define-emphasis-writer ((stream text) &body body)
  ...)
```

````
Same as define-bold-writer, but with both bold and italic style.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-INLINE-CODE-WRITER">Macro: define-inline-code-writer</h4>

```Lisp
(defmacro adpsm:define-inline-code-writer ((stream text) &body body)
  ...)
```

````
Same as define-bold-writer, but with a code-inline style.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-HEADER-REF-WRITER">Macro: define-header-ref-writer</h4>

```Lisp
(defmacro adpsm:define-header-ref-writer ((stream tag title
                                           source-relative-path)
                                          &body body)
  ...)
```

````
Define a function to print a header reference element. It receives the stream to be written in, the tag
associated to a header element, the text of said header element, and the relative path to the place where the
header element is in.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-SYMBOL-REF-WRITER">Macro: define-symbol-ref-writer</h4>

```Lisp
(defmacro adpsm:define-symbol-ref-writer ((stream tag source-relative-path)
                                          &body body)
  ...)
```

````
Same as define-header-ref-writer, but it prints a symbol reference. Also, it receives the stream, the tag
associated with the symbol, and the relative path to the place where the symbol definition is in.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-FUNCTION-REF-WRITER">Macro: define-function-ref-writer</h4>

```Lisp
(defmacro adpsm:define-function-ref-writer ((stream tag source-relative-path)
                                            &body body)
  ...)
```

````
Same as define-symbol-ref-writer, but it prints a function reference.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-TYPE-REF-WRITER">Macro: define-type-ref-writer</h4>

```Lisp
(defmacro adpsm:define-type-ref-writer ((stream tag source-relative-path) &body
                                        body)
  ...)
```

````
Sama as define-symbol-ref-writer, but it prints a type reference.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-WEB-LINK-WRITER">Macro: define-web-link-writer</h4>

```Lisp
(defmacro adpsm:define-web-link-writer ((stream text address) &body body)
  ...)
```

````
Define a function to print a web link element. It receives the stream, the link text as a string, and the link address as a string.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-IMAGE-WRITER">Macro: define-image-writer</h4>

```Lisp
(defmacro adpsm:define-image-writer ((stream alt-text image-relative-path)
                                     &body body)
  ...)
```

````
Define a function to print an image element. It receives the stream, the alternative text as a string, and a
relative pathname to the image.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-TABLE-WRITER">Macro: define-table-writer</h4>

```Lisp
(defmacro adpsm:define-table-writer ((stream rows) &body body)
  ...)
```

````
Define a function to print a table element. It receives the stream, and a list of lists of strings. Each
inner list is a row of the table, and each string is an element of the table.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-ITEMIZE-WRITER">Macro: define-itemize-writer</h4>

```Lisp
(defmacro adpsm:define-itemize-writer ((stream itemize) &body body)
  ...)
```

````
Define a function to print an itemize element. It receives the stream and a list. The first element of the
list can be :iterate or :enumerate indicating if you should use numbers. Each element of the rest of the list can be an item or a nested list element. The item has the form (:item string). A nested list element is a list starting with :itemize or :enumerate and the rest of elements are again items or nested list elements.
  For example, you could receive something like:
    (:itemize (:item "Animals")
              (:enumerate (:item "Dog")
                          (:item "Cat")))
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-CODE-BLOCK-WRITER">Macro: define-code-block-writer</h4>

```Lisp
(defmacro adpsm:define-code-block-writer ((stream lang text) &body body)
  ...)
```

````
Define a function to print a code block element. It receives the stream, a string with the used programming 
language, and another string with the text to be placed in the block of code.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-CODE-EXAMPLE-WRITER">Macro: define-code-example-writer</h4>

```Lisp
(defmacro adpsm:define-code-example-writer ((stream text output results) &body
                                            body)
  ...)
```

````
Define a function to print an example block. It receives the stream, a string with the code to be placed in 
a block of code, a string with the standard output of the code, and a list of elements returned by the code.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFCLASS-WRITER">Macro: define-defclass-writer</h4>

```Lisp
(defmacro adpsm:define-defclass-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a defclass definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFCONSTANT-WRITER">Macro: define-defconstant-writer</h4>

```Lisp
(defmacro adpsm:define-defconstant-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a defconstant definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFGENERIC-WRITER">Macro: define-defgeneric-writer</h4>

```Lisp
(defmacro adpsm:define-defgeneric-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a defgeneric definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-COMPILER-MACRO-WRITER">Macro: define-define-compiler-macro-writer</h4>

```Lisp
(defmacro adpsm:define-define-compiler-macro-writer ((stream expr) &body body)
  ...)
```

````
Define a function to print a define-compiler-macro definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-CONDITION-WRITER">Macro: define-define-condition-writer</h4>

```Lisp
(defmacro adpsm:define-define-condition-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a define-condition definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-METHOD-COMBINATION-WRITER">Macro: define-define-method-combination-writer</h4>

```Lisp
(defmacro adpsm:define-define-method-combination-writer ((stream expr) &body
                                                         body)
  ...)
```

````
Define a function to print a define-method-combination definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-MODIFY-MACRO-WRITER">Macro: define-define-modify-macro-writer</h4>

```Lisp
(defmacro adpsm:define-define-modify-macro-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a define-modify-macro definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-SETF-EXPANDER-WRITER">Macro: define-define-setf-expander-writer</h4>

```Lisp
(defmacro adpsm:define-define-setf-expander-writer ((stream expr) &body body)
  ...)
```

````
Define a function to print a define-setf-expander definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-SYMBOL-MACRO-WRITER">Macro: define-define-symbol-macro-writer</h4>

```Lisp
(defmacro adpsm:define-define-symbol-macro-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a define-symbol-macro definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFMACRO-WRITER">Macro: define-defmacro-writer</h4>

```Lisp
(defmacro adpsm:define-defmacro-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a defmacro definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFMETHOD-WRITER">Macro: define-defmethod-writer</h4>

```Lisp
(defmacro adpsm:define-defmethod-writer ((stream expr) &body body)
  ...)
```

````
Define a function to print a defmethod definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFPACKAGE-WRITER">Macro: define-defpackage-writer</h4>

```Lisp
(defmacro adpsm:define-defpackage-writer ((stream expr) &body body)
  ...)
```

````
Define a function to print a defpackage definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFPARAMETER-WRITER">Macro: define-defparameter-writer</h4>

```Lisp
(defmacro adpsm:define-defparameter-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a defparameter definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFSETF-WRITER">Macro: define-defsetf-writer</h4>

```Lisp
(defmacro adpsm:define-defsetf-writer ((stream expr) &body body)
  ...)
```

````
Define a function to print a defsetf definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFSTRUCT-WRITER">Macro: define-defstruct-writer</h4>

```Lisp
(defmacro adpsm:define-defstruct-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a defstruct definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFTYPE-WRITER">Macro: define-deftype-writer</h4>

```Lisp
(defmacro adpsm:define-deftype-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a deftype definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFUN-WRITER">Macro: define-defun-writer</h4>

```Lisp
(defmacro adpsm:define-defun-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a defun definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFVAR-WRITER">Macro: define-defvar-writer</h4>

```Lisp
(defmacro adpsm:define-defvar-writer ((stream expr tag) &body body)
  ...)
```

````
Define a function to print a defvar definition. It receives the stream, and the definition expression.
````

<h2 id="header:ADP:HEADERTAG5">API function components</h2>

<h3 id="header:ADP:HEADERTAG6">Defclass components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFCLASS-COMPONENTS">Macro: with-defclass-components</h4>

```Lisp
(defmacro adpsm:with-defclass-components (((&rest component-rest-args)
                                           function-body-arg)
                                          &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG7">Defconstant components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFCONSTANT-COMPONENTS">Macro: with-defconstant-components</h4>

```Lisp
(defmacro adpsm:with-defconstant-components (((&rest component-rest-args)
                                              function-body-arg)
                                             &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG8">Defgeneric components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFGENERIC-COMPONENTS">Macro: with-defgeneric-components</h4>

```Lisp
(defmacro adpsm:with-defgeneric-components (((&rest component-rest-args)
                                             function-body-arg)
                                            &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG9">Define-compiler-macro components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-COMPILER-MACRO-COMPONENTS">Macro: with-define-compiler-macro-components</h4>

```Lisp
(defmacro adpsm:with-define-compiler-macro-components (((&rest
                                                         component-rest-args)
                                                        function-body-arg)
                                                       &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG10">Define-condition components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-CONDITION-COMPONENTS">Macro: with-define-condition-components</h4>

```Lisp
(defmacro adpsm:with-define-condition-components (((&rest component-rest-args)
                                                   function-body-arg)
                                                  &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG11">Define-method-combination components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-METHOD-COMBINATION-COMPONENTS">Macro: with-define-method-combination-components</h4>

```Lisp
(defmacro adpsm:with-define-method-combination-components (((&rest
                                                             component-rest-args)
                                                            function-body-arg)
                                                           &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG12">Define-modify-macro components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-MODIFY-MACRO-COMPONENTS">Macro: with-define-modify-macro-components</h4>

```Lisp
(defmacro adpsm:with-define-modify-macro-components (((&rest
                                                       component-rest-args)
                                                      function-body-arg)
                                                     &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG13">Define-setf-expander components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-SETF-EXPANDER-COMPONENTS">Macro: with-define-setf-expander-components</h4>

```Lisp
(defmacro adpsm:with-define-setf-expander-components (((&rest
                                                        component-rest-args)
                                                       function-body-arg)
                                                      &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG14">Define-symbol-macro components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-SYMBOL-MACRO-COMPONENTS">Macro: with-define-symbol-macro-components</h4>

```Lisp
(defmacro adpsm:with-define-symbol-macro-components (((&rest
                                                       component-rest-args)
                                                      function-body-arg)
                                                     &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG15">Defmacro components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFMACRO-COMPONENTS">Macro: with-defmacro-components</h4>

```Lisp
(defmacro adpsm:with-defmacro-components (((&rest component-rest-args)
                                           function-body-arg)
                                          &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG16">defmethod components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFMETHOD-COMPONENTS">Macro: with-defmethod-components</h4>

```Lisp
(defmacro adpsm:with-defmethod-components (((&rest component-rest-args)
                                            function-body-arg)
                                           &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG17">Defpackage components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFPACKAGE-COMPONENTS">Macro: with-defpackage-components</h4>

```Lisp
(defmacro adpsm:with-defpackage-components (((&rest component-rest-args)
                                             function-body-arg)
                                            &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG18">Defparameter components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFPARAMETER-COMPONENTS">Macro: with-defparameter-components</h4>

```Lisp
(defmacro adpsm:with-defparameter-components (((&rest component-rest-args)
                                               function-body-arg)
                                              &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG19">Defsetf components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFSETF-COMPONENTS">Macro: with-defsetf-components</h4>

```Lisp
(defmacro adpsm:with-defsetf-components (((&rest component-rest-args)
                                          function-body-arg)
                                         &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG20">Defstruct components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFSTRUCT-COMPONENTS">Macro: with-defstruct-components</h4>

```Lisp
(defmacro adpsm:with-defstruct-components (((&rest component-rest-args)
                                            function-body-arg)
                                           &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG21">Deftype components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFTYPE-COMPONENTS">Macro: with-deftype-components</h4>

```Lisp
(defmacro adpsm:with-deftype-components (((&rest component-rest-args)
                                          function-body-arg)
                                         &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG22">Defun components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFUN-COMPONENTS">Macro: with-defun-components</h4>

```Lisp
(defmacro adpsm:with-defun-components (((&rest component-rest-args)
                                        function-body-arg)
                                       &body body-arg)
  ...)
```

<h3 id="header:ADP:HEADERTAG23">Defvar components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFVAR-COMPONENTS">Macro: with-defvar-components</h4>

```Lisp
(defmacro adpsm:with-defvar-components (((&rest component-rest-args)
                                         function-body-arg)
                                        &body body-arg)
  ...)
```

