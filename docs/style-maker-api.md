<h1 id="STYLE-MAKER-API-HEADER2">Style\-maker API</h1>

## ADP pprint dispatch

#### Variable: \*ADP\-PPRINT\-DISPATCH\*

```Lisp
(defvar ADPSM:*ADP-PPRINT-DISPATCH* ADPPVT:*ADP-PPRINT-DISPATCH*)
```

````
ADP custom pprint dispatch table to make the code printing look better. The main difference is when the
package extension of a symbol is printed. It will only print the extension package when a symbol is exported.
````

## Style parameters

#### Macro: DEFINE\-STYLE\-PARAMETER

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

## Writers

#### Macro: DEFINE\-BEGIN\-FILE\-WRITER

```Lisp
(defmacro DEFINE-BEGIN-FILE-WRITER ((STREAM) &BODY BODY)
  ...)
```

````
Define a function that will be called when the file is about to be written in. The function receives a stream 
associated with said file.
````

#### Macro: DEFINE\-END\-FILE\-WRITER

```Lisp
(defmacro DEFINE-END-FILE-WRITER ((STREAM) &BODY BODY)
  ...)
```

````
Define a function that will be called after a file has finished of being written in. The function receives a
stream associated with said file.
````

#### Macro: DEFINE\-FILE\-EXTENSION

```Lisp
(defmacro ADPSM:DEFINE-FILE-EXTENSION (NIL &BODY BODY)
  ...)
```

````
Define a function that must return a string indicating the extension of the files that ADP will create.
````

#### Macro: DEFINE\-BEGIN\-PROJECT\-WRITER

```Lisp
(defmacro DEFINE-BEGIN-PROJECT-WRITER ((ROOT-DIRECTORY) &BODY BODY)
  ...)
```

````
Define a function that will be called before the documentation of a project begins to generate. The function 
receives the pathname of the project root directory.
````

#### Macro: DEFINE\-END\-PROJECT\-WRITER

```Lisp
(defmacro DEFINE-END-PROJECT-WRITER ((ROOT-DIRECTORY) &BODY BODY)
  ...)
```

````
Define a function that will be called after the documentation of a project has been generated. The function
receives the pathname of the project root directory.
````

#### Macro: DEFINE\-HEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-HEADER-WRITER ((STREAM TITLE TAG) &BODY BODY)
  ...)
```

````
Define a function that must print a header element. The function receives the stream to be written in, the
header name and the tag associated to it.
````

#### Macro: DEFINE\-SUBHEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SUBHEADER-WRITER ((STREAM TITLE TAG) &BODY BODY)
  ...)
```

````
Same as define-header-writer but it must print a subheader element.
````

#### Macro: DEFINE\-SUBSUBHEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SUBSUBHEADER-WRITER ((STREAM TITLE TAG) &BODY BODY)
  ...)
```

````
Same as define-header-writer but it must print a subsubheader element.
````

#### Macro: DEFINE\-TEXT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TEXT-WRITER ((STREAM TEXT) &BODY BODY)
  ...)
```

````
Define a function that must print a text element. It receives the stream to be written in, and a string with
the text.
````

#### Macro: DEFINE\-ESCAPE\-TEXT

```Lisp
(defmacro ADPSM:DEFINE-ESCAPE-TEXT ((TEXT) &BODY BODY)
  ...)
```

````
Define a function that receives a string of text and must return another string. You want this to escape
special characters that will be used with bold, italic, header-ref, web-link, etc.
````

#### Macro: DEFINE\-BOLD\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-BOLD-WRITER ((STREAM TEXT) &BODY BODY)
  ...)
```

````
Define a function to print a bold text element. It receives the stream to be written in, and a string with
the text.
````

#### Macro: DEFINE\-ITALIC\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-ITALIC-WRITER ((STREAM TEXT) &BODY BODY)
  ...)
```

````
Same as define-bold-writer, but with an italic style.
````

#### Macro: DEFINE\-EMPHASIS\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-EMPHASIS-WRITER ((STREAM TEXT) &BODY BODY)
  ...)
```

````
Same as define-bold-writer, but with both bold and italic style.
````

#### Macro: DEFINE\-INLINE\-CODE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-INLINE-CODE-WRITER ((STREAM TEXT) &BODY BODY)
  ...)
```

````
Same as define-bold-writer, but with a code-inline style.
````

#### Macro: DEFINE\-HEADER\-REF\-WRITER

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

#### Macro: DEFINE\-SYMBOL\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SYMBOL-REF-WRITER ((STREAM TAG SOURCE-RELATIVE-PATH)
                                          &BODY BODY)
  ...)
```

````
Same as define-header-ref-writer, but it prints a symbol reference. Also, it receives the stream, the tag
associated with the symbol, and the relative path to the place where the symbol definition is in.
````

#### Macro: DEFINE\-FUNCTION\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-FUNCTION-REF-WRITER ((STREAM TAG SOURCE-RELATIVE-PATH)
                                            &BODY BODY)
  ...)
```

````
Same as define-symbol-ref-writer, but it prints a function reference.
````

#### Macro: DEFINE\-TYPE\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TYPE-REF-WRITER ((STREAM TAG SOURCE-RELATIVE-PATH) &BODY
                                        BODY)
  ...)
```

````
Sama as define-symbol-ref-writer, but it prints a type reference.
````

#### Macro: DEFINE\-WEB\-LINK\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-WEB-LINK-WRITER ((STREAM TEXT ADDRESS) &BODY BODY)
  ...)
```

````
Define a function to print a web link element. It receives the stream, the link text as a string, and the link address as a string.
````

#### Macro: DEFINE\-IMAGE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-IMAGE-WRITER ((STREAM ALT-TEXT IMAGE-RELATIVE-PATH)
                                     &BODY BODY)
  ...)
```

````
Define a function to print an image element. It receives the stream, the alternative text as a string, and a
relative pathname to the image.
````

#### Macro: DEFINE\-TABLE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TABLE-WRITER ((STREAM ROWS) &BODY BODY)
  ...)
```

````
Define a function to print a table element. It receives the stream, and a list of lists of strings. Each
inner list is a row of the table, and each string is an element of the table.
````

#### Macro: DEFINE\-ITEMIZE\-WRITER

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

#### Macro: DEFINE\-CODE\-BLOCK\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-CODE-BLOCK-WRITER ((STREAM LANG TEXT) &BODY BODY)
  ...)
```

````
Define a function to print a code block element. It receives the stream, a string with the used programming 
language, and another string with the text to be placed in the block of code.
````

#### Macro: DEFINE\-CODE\-EXAMPLE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-CODE-EXAMPLE-WRITER ((STREAM TEXT OUTPUT RESULTS) &BODY
                                            BODY)
  ...)
```

````
Define a function to print an example block. It receives the stream, a string with the code to be placed in 
a block of code, a string with the standard output of the code, and a list of elements returned by the code.
````

#### Macro: DEFINE\-DEFCLASS\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFCLASS-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defclass definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFCONSTANT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFCONSTANT-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defconstant definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFGENERIC\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFGENERIC-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defgeneric definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-COMPILER\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-COMPILER-MACRO-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a define-compiler-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-CONDITION\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-CONDITION-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a define-condition definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-METHOD\-COMBINATION\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-METHOD-COMBINATION-WRITER ((STREAM EXPR) &BODY
                                                         BODY)
  ...)
```

````
Define a function to print a define-method-combination definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-MODIFY\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-MODIFY-MACRO-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a define-modify-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-SETF\-EXPANDER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-SETF-EXPANDER-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a define-setf-expander definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-SYMBOL\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-SYMBOL-MACRO-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a define-symbol-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFMACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFMACRO-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defmacro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFMETHOD\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFMETHOD-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defmethod definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFPACKAGE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFPACKAGE-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defpackage definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFPARAMETER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFPARAMETER-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defparameter definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFSETF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFSETF-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defsetf definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFSTRUCT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFSTRUCT-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defstruct definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFTYPE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFTYPE-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a deftype definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFUN\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFUN-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defun definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFVAR\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFVAR-WRITER ((STREAM EXPR) &BODY BODY)
  ...)
```

````
Define a function to print a defvar definition. It receives the stream, and the definition expression.
````

## API function components

### Defclass components

#### Macro: WITH\-DEFCLASS\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFCLASS-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                           FUNCTION-BODY-ARG)
                                          &BODY BODY-ARG)
  ...)
```

### Defconstant components

#### Macro: WITH\-DEFCONSTANT\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFCONSTANT-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                              FUNCTION-BODY-ARG)
                                             &BODY BODY-ARG)
  ...)
```

### Defgeneric components

#### Macro: WITH\-DEFGENERIC\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFGENERIC-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                             FUNCTION-BODY-ARG)
                                            &BODY BODY-ARG)
  ...)
```

### Define\-compiler\-macro components

#### Macro: WITH\-DEFINE\-COMPILER\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-COMPILER-MACRO-COMPONENTS (((&REST
                                                         COMPONENT-REST-ARGS)
                                                        FUNCTION-BODY-ARG)
                                                       &BODY BODY-ARG)
  ...)
```

### Define\-condition components

#### Macro: WITH\-DEFINE\-CONDITION\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-CONDITION-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                                   FUNCTION-BODY-ARG)
                                                  &BODY BODY-ARG)
  ...)
```

### Define\-method\-combination components

#### Macro: WITH\-DEFINE\-METHOD\-COMBINATION\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-METHOD-COMBINATION-COMPONENTS (((&REST
                                                             COMPONENT-REST-ARGS)
                                                            FUNCTION-BODY-ARG)
                                                           &BODY BODY-ARG)
  ...)
```

### Define\-modify\-macro components

#### Macro: WITH\-DEFINE\-MODIFY\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-MODIFY-MACRO-COMPONENTS (((&REST
                                                       COMPONENT-REST-ARGS)
                                                      FUNCTION-BODY-ARG)
                                                     &BODY BODY-ARG)
  ...)
```

### Define\-setf\-expander components

#### Macro: WITH\-DEFINE\-SETF\-EXPANDER\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-SETF-EXPANDER-COMPONENTS (((&REST
                                                        COMPONENT-REST-ARGS)
                                                       FUNCTION-BODY-ARG)
                                                      &BODY BODY-ARG)
  ...)
```

### Define\-symbol\-macro components

#### Macro: WITH\-DEFINE\-SYMBOL\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-SYMBOL-MACRO-COMPONENTS (((&REST
                                                       COMPONENT-REST-ARGS)
                                                      FUNCTION-BODY-ARG)
                                                     &BODY BODY-ARG)
  ...)
```

### Defmacro components

#### Macro: WITH\-DEFMACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFMACRO-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                           FUNCTION-BODY-ARG)
                                          &BODY BODY-ARG)
  ...)
```

### defmethod components

#### Macro: WITH\-DEFMETHOD\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFMETHOD-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                            FUNCTION-BODY-ARG)
                                           &BODY BODY-ARG)
  ...)
```

### Defpackage components

#### Macro: WITH\-DEFPACKAGE\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFPACKAGE-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                             FUNCTION-BODY-ARG)
                                            &BODY BODY-ARG)
  ...)
```

### Defparameter components

#### Macro: WITH\-DEFPARAMETER\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFPARAMETER-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                               FUNCTION-BODY-ARG)
                                              &BODY BODY-ARG)
  ...)
```

### Defsetf components

#### Macro: WITH\-DEFSETF\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFSETF-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                          FUNCTION-BODY-ARG)
                                         &BODY BODY-ARG)
  ...)
```

### Defstruct components

#### Macro: WITH\-DEFSTRUCT\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFSTRUCT-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                            FUNCTION-BODY-ARG)
                                           &BODY BODY-ARG)
  ...)
```

### Deftype components

#### Macro: WITH\-DEFTYPE\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFTYPE-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                          FUNCTION-BODY-ARG)
                                         &BODY BODY-ARG)
  ...)
```

### Defun components

#### Macro: WITH\-DEFUN\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFUN-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                        FUNCTION-BODY-ARG)
                                       &BODY BODY-ARG)
  ...)
```

### Defvar components

#### Macro: WITH\-DEFVAR\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFVAR-COMPONENTS (((&REST COMPONENT-REST-ARGS)
                                         FUNCTION-BODY-ARG)
                                        &BODY BODY-ARG)
  ...)
```

