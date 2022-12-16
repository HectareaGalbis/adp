<h1 id="header:ADP-STYLE-MAKER:STYLE-MAKER-API-HEADER">Style-maker API</h1>

<h2 id="header:ADP:HEADERTAG2">ADP pprint dispatch</h2>

<h4 id="symbol:ADP-STYLE-MAKER:*ADP-PPRINT-DISPATCH*">Variable: *ADP-PPRINT-DISPATCH*</h4>

```Lisp
(defvar ADPSM:*ADP-PPRINT-DISPATCH* ADPPVT:*ADP-PPRINT-DISPATCH*)
```

````
ADP custom pprint dispatch table to make the code printing look better. The main difference is when the
package extension of a symbol is printed. It will only print the extension package when a symbol is exported.
````

<h2 id="header:ADP:HEADERTAG3">Style parameters</h2>

<h4 id="function:ADP-STYLE-MAKER:DEFINE-STYLE-PARAMETER">Macro: DEFINE-STYLE-PARAMETER</h4>

```Lisp
(defmacro ADPSM:DEFINE-STYLE-PARAMETER (NAME &KEY (VALUE NIL) (KEY NIL)
                                        (REQUIRED NIL))
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

<h4 id="function:ADP-STYLE-MAKER:DEFINE-BEGIN-FILE-WRITER">Macro: DEFINE-BEGIN-FILE-WRITER</h4>

```Lisp
(defmacro DEFINE-BEGIN-FILE-WRITER ((STREAM) &BODY BODY)
  ...)
```

````
Define a function that will be called when the file is about to be written in. The function receives a stream 
associated with said file.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-END-FILE-WRITER">Macro: DEFINE-END-FILE-WRITER</h4>

```Lisp
(defmacro DEFINE-END-FILE-WRITER ((STREAM) &BODY BODY)
  ...)
```

````
Define a function that will be called after a file has finished of being written in. The function receives a
stream associated with said file.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-FILE-EXTENSION">Macro: DEFINE-FILE-EXTENSION</h4>

```Lisp
(defmacro ADPSM:DEFINE-FILE-EXTENSION (NIL &BODY BODY)
  ...)
```

````
Define a function that must return a string indicating the extension of the files that ADP will create.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-BEGIN-PROJECT-WRITER">Macro: DEFINE-BEGIN-PROJECT-WRITER</h4>

```Lisp
(defmacro DEFINE-BEGIN-PROJECT-WRITER ((ROOT-DIRECTORY) &BODY BODY)
  ...)
```

````
Define a function that will be called before the documentation of a project begins to generate. The function 
receives the pathname of the project root directory.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-END-PROJECT-WRITER">Macro: DEFINE-END-PROJECT-WRITER</h4>

```Lisp
(defmacro DEFINE-END-PROJECT-WRITER ((ROOT-DIRECTORY) &BODY BODY)
  ...)
```

````
Define a function that will be called after the documentation of a project has been generated. The function
receives the pathname of the project root directory.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-HEADER-WRITER">Macro: DEFINE-HEADER-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-HEADER-WRITER ((STREAM TITLE TAG) &BODY BODY)
  ...)
```

````
Define a function that must print a header element. The function receives the stream to be written in, the
header name and the tag associated to it.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-SUBHEADER-WRITER">Macro: DEFINE-SUBHEADER-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-SUBHEADER-WRITER ((STREAM TITLE TAG) &BODY BODY)
  ...)
```

````
Same as define-header-writer but it must print a subheader element.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-SUBSUBHEADER-WRITER">Macro: DEFINE-SUBSUBHEADER-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-SUBSUBHEADER-WRITER ((STREAM TITLE TAG) &BODY BODY)
  ...)
```

````
Same as define-header-writer but it must print a subsubheader element.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-TEXT-WRITER">Macro: DEFINE-TEXT-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-TEXT-WRITER ((STREAM TEXT) &BODY BODY)
  ...)
```

````
Define a function that must print a text element. It receives the stream to be written in, and a string with
the text.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-ESCAPE-TEXT">Macro: DEFINE-ESCAPE-TEXT</h4>

```Lisp
(defmacro ADPSM:DEFINE-ESCAPE-TEXT ((TEXT) &BODY BODY)
  ...)
```

````
Define a function that receives a string of text and must return another string. You want this to escape
special characters that will be used with bold, italic, header-ref, web-link, etc.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-BOLD-WRITER">Macro: DEFINE-BOLD-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-BOLD-WRITER ((STREAM TEXT) &BODY BODY)
  ...)
```

````
Define a function to print a bold text element. It receives the stream to be written in, and a string with
the text.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-ITALIC-WRITER">Macro: DEFINE-ITALIC-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-ITALIC-WRITER ((STREAM TEXT) &BODY BODY)
  ...)
```

````
Same as define-bold-writer, but with an italic style.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-EMPHASIS-WRITER">Macro: DEFINE-EMPHASIS-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-EMPHASIS-WRITER ((STREAM TEXT) &BODY BODY)
  ...)
```

````
Same as define-bold-writer, but with both bold and italic style.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-INLINE-CODE-WRITER">Macro: DEFINE-INLINE-CODE-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-INLINE-CODE-WRITER ((STREAM TEXT) &BODY BODY)
  ...)
```

````
Same as define-bold-writer, but with a code-inline style.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-HEADER-REF-WRITER">Macro: DEFINE-HEADER-REF-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-HEADER-REF-WRITER ((STREAM TAG TITLE
                                           SOURCE-RELATIVE-PATH)
                                          &BODY BODY)
  ...)
```

````
Define a function to print a header reference element. It receives the stream to be written in, the tag
associated to a header element, the text of said header element, and the relative path to the place where the
header element is in.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-SYMBOL-REF-WRITER">Macro: DEFINE-SYMBOL-REF-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-SYMBOL-REF-WRITER ((STREAM TAG SOURCE-RELATIVE-PATH)
                                          &BODY BODY)
  ...)
```

````
Same as define-header-ref-writer, but it prints a symbol reference. Also, it receives the stream, the tag
associated with the symbol, and the relative path to the place where the symbol definition is in.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-FUNCTION-REF-WRITER">Macro: DEFINE-FUNCTION-REF-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-FUNCTION-REF-WRITER ((STREAM TAG SOURCE-RELATIVE-PATH)
                                            &BODY BODY)
  ...)
```

````
Same as define-symbol-ref-writer, but it prints a function reference.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-TYPE-REF-WRITER">Macro: DEFINE-TYPE-REF-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-TYPE-REF-WRITER ((STREAM TAG SOURCE-RELATIVE-PATH) &BODY
                                        BODY)
  ...)
```

````
Sama as define-symbol-ref-writer, but it prints a type reference.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-WEB-LINK-WRITER">Macro: DEFINE-WEB-LINK-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-WEB-LINK-WRITER ((STREAM TEXT ADDRESS) &BODY BODY)
  ...)
```

````
Define a function to print a web link element. It receives the stream, the link text as a string, and the link address as a string.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-IMAGE-WRITER">Macro: DEFINE-IMAGE-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-IMAGE-WRITER ((STREAM ALT-TEXT IMAGE-RELATIVE-PATH)
                                     &BODY BODY)
  ...)
```

````
Define a function to print an image element. It receives the stream, the alternative text as a string, and a
relative pathname to the image.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-TABLE-WRITER">Macro: DEFINE-TABLE-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-TABLE-WRITER ((STREAM ROWS) &BODY BODY)
  ...)
```

````
Define a function to print a table element. It receives the stream, and a list of lists of strings. Each
inner list is a row of the table, and each string is an element of the table.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-ITEMIZE-WRITER">Macro: DEFINE-ITEMIZE-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-ITEMIZE-WRITER ((STREAM ITEMIZE) &BODY BODY)
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

<h4 id="function:ADP-STYLE-MAKER:DEFINE-CODE-BLOCK-WRITER">Macro: DEFINE-CODE-BLOCK-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-CODE-BLOCK-WRITER ((STREAM LANG TEXT) &BODY BODY)
  ...)
```

````
Define a function to print a code block element. It receives the stream, a string with the used programming 
language, and another string with the text to be placed in the block of code.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-CODE-EXAMPLE-WRITER">Macro: DEFINE-CODE-EXAMPLE-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-CODE-EXAMPLE-WRITER ((STREAM TEXT OUTPUT RESULTS) &BODY
                                            BODY)
  ...)
```

````
Define a function to print an example block. It receives the stream, a string with the code to be placed in 
a block of code, a string with the standard output of the code, and a list of elements returned by the code.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFCLASS-WRITER">Macro: DEFINE-DEFCLASS-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFCLASS-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a defclass definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFCONSTANT-WRITER">Macro: DEFINE-DEFCONSTANT-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFCONSTANT-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a defconstant definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFGENERIC-WRITER">Macro: DEFINE-DEFGENERIC-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFGENERIC-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a defgeneric definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-COMPILER-MACRO-WRITER">Macro: DEFINE-DEFINE-COMPILER-MACRO-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-COMPILER-MACRO-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a define-compiler-macro definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-CONDITION-WRITER">Macro: DEFINE-DEFINE-CONDITION-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-CONDITION-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a define-condition definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-METHOD-COMBINATION-WRITER">Macro: DEFINE-DEFINE-METHOD-COMBINATION-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-METHOD-COMBINATION-WRITER ((STREAM EXPR) &BODY
                                                         BODY)
  ...)
```

````
Define a function to print a define-method-combination definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-MODIFY-MACRO-WRITER">Macro: DEFINE-DEFINE-MODIFY-MACRO-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-MODIFY-MACRO-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a define-modify-macro definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-SETF-EXPANDER-WRITER">Macro: DEFINE-DEFINE-SETF-EXPANDER-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-SETF-EXPANDER-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a define-setf-expander definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFINE-SYMBOL-MACRO-WRITER">Macro: DEFINE-DEFINE-SYMBOL-MACRO-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-SYMBOL-MACRO-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a define-symbol-macro definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFMACRO-WRITER">Macro: DEFINE-DEFMACRO-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFMACRO-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a defmacro definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFMETHOD-WRITER">Macro: DEFINE-DEFMETHOD-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFMETHOD-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defmethod definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFPACKAGE-WRITER">Macro: DEFINE-DEFPACKAGE-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFPACKAGE-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defpackage definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFPARAMETER-WRITER">Macro: DEFINE-DEFPARAMETER-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFPARAMETER-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a defparameter definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFSETF-WRITER">Macro: DEFINE-DEFSETF-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFSETF-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defsetf definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFSTRUCT-WRITER">Macro: DEFINE-DEFSTRUCT-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFSTRUCT-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a defstruct definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFTYPE-WRITER">Macro: DEFINE-DEFTYPE-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFTYPE-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a deftype definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFUN-WRITER">Macro: DEFINE-DEFUN-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFUN-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a defun definition. It receives the stream, and the definition expression.
````

<h4 id="function:ADP-STYLE-MAKER:DEFINE-DEFVAR-WRITER">Macro: DEFINE-DEFVAR-WRITER</h4>

```Lisp
(defmacro ADPSM:DEFINE-DEFVAR-WRITER ((STREAM EXPR TAG) &BODY BODY)
  ...)
```

````
Define a function to print a defvar definition. It receives the stream, and the definition expression.
````

<h2 id="header:ADP:HEADERTAG5">API function components</h2>

<h3 id="header:ADP:HEADERTAG6">Defclass components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFCLASS-COMPONENTS">Macro: WITH-DEFCLASS-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFCLASS-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                           FUNCTION-BODY-ARG)
                                          &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG7">Defconstant components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFCONSTANT-COMPONENTS">Macro: WITH-DEFCONSTANT-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFCONSTANT-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                              FUNCTION-BODY-ARG)
                                             &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG8">Defgeneric components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFGENERIC-COMPONENTS">Macro: WITH-DEFGENERIC-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFGENERIC-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                             FUNCTION-BODY-ARG)
                                            &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG9">Define-compiler-macro components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-COMPILER-MACRO-COMPONENTS">Macro: WITH-DEFINE-COMPILER-MACRO-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFINE-COMPILER-MACRO-COMPONENTS (((&REST
                                                         COMPONENT-REST-ARGS)
                                                        FUNCTION-BODY-ARG)
                                                       &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG10">Define-condition components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-CONDITION-COMPONENTS">Macro: WITH-DEFINE-CONDITION-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFINE-CONDITION-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                                   FUNCTION-BODY-ARG)
                                                  &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG11">Define-method-combination components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-METHOD-COMBINATION-COMPONENTS">Macro: WITH-DEFINE-METHOD-COMBINATION-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFINE-METHOD-COMBINATION-COMPONENTS (((&REST
                                                             COMPONENT-REST-ARGS)
                                                            FUNCTION-BODY-ARG)
                                                           &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG12">Define-modify-macro components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-MODIFY-MACRO-COMPONENTS">Macro: WITH-DEFINE-MODIFY-MACRO-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFINE-MODIFY-MACRO-COMPONENTS (((&REST
                                                       COMPONENT-REST-ARGS)
                                                      FUNCTION-BODY-ARG)
                                                     &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG13">Define-setf-expander components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-SETF-EXPANDER-COMPONENTS">Macro: WITH-DEFINE-SETF-EXPANDER-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFINE-SETF-EXPANDER-COMPONENTS (((&REST
                                                        COMPONENT-REST-ARGS)
                                                       FUNCTION-BODY-ARG)
                                                      &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG14">Define-symbol-macro components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFINE-SYMBOL-MACRO-COMPONENTS">Macro: WITH-DEFINE-SYMBOL-MACRO-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFINE-SYMBOL-MACRO-COMPONENTS (((&REST
                                                       COMPONENT-REST-ARGS)
                                                      FUNCTION-BODY-ARG)
                                                     &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG15">Defmacro components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFMACRO-COMPONENTS">Macro: WITH-DEFMACRO-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFMACRO-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                           FUNCTION-BODY-ARG)
                                          &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG16">defmethod components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFMETHOD-COMPONENTS">Macro: WITH-DEFMETHOD-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFMETHOD-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                            FUNCTION-BODY-ARG)
                                           &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG17">Defpackage components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFPACKAGE-COMPONENTS">Macro: WITH-DEFPACKAGE-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFPACKAGE-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                             FUNCTION-BODY-ARG)
                                            &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG18">Defparameter components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFPARAMETER-COMPONENTS">Macro: WITH-DEFPARAMETER-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFPARAMETER-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                               FUNCTION-BODY-ARG)
                                              &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG19">Defsetf components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFSETF-COMPONENTS">Macro: WITH-DEFSETF-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFSETF-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                          FUNCTION-BODY-ARG)
                                         &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG20">Defstruct components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFSTRUCT-COMPONENTS">Macro: WITH-DEFSTRUCT-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFSTRUCT-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                            FUNCTION-BODY-ARG)
                                           &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG21">Deftype components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFTYPE-COMPONENTS">Macro: WITH-DEFTYPE-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFTYPE-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                          FUNCTION-BODY-ARG)
                                         &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG22">Defun components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFUN-COMPONENTS">Macro: WITH-DEFUN-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFUN-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                        FUNCTION-BODY-ARG)
                                       &BODY BODY-ARG)
  ...)
```

<h3 id="header:ADP:HEADERTAG23">Defvar components</h3>

<h4 id="function:ADP-STYLE-MAKER:WITH-DEFVAR-COMPONENTS">Macro: WITH-DEFVAR-COMPONENTS</h4>

```Lisp
(defmacro ADPSM:WITH-DEFVAR-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                         FUNCTION-BODY-ARG)
                                        &BODY BODY-ARG)
  ...)
```

