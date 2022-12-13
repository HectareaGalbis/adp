# Style\-maker API

## ADP pprint dispatch

## Style parameters

## Writers

#### Macro: DEFINE\-BEGIN\-FILE\-WRITER

```Lisp
(defmacro DEFINE-BEGIN-FILE-WRITER ((G3) &BODY BODY4)
  ...)
```

````
Define a function that will be called when the file is about to be written in. The function receives a stream 
associated with said file.
````

#### Macro: DEFINE\-END\-FILE\-WRITER

```Lisp
(defmacro DEFINE-END-FILE-WRITER ((G6) &BODY BODY7)
  ...)
```

````
Define a function that will be called after a file has finished of being written in. The function receives a
stream associated with said file.
````

#### Macro: DEFINE\-FILE\-EXTENSION

```Lisp
(defmacro ADPSM:DEFINE-FILE-EXTENSION (NIL &BODY BODY9)
  ...)
```

````
Define a function that must return a string indicating the extension of the files that ADP will create.
````

#### Macro: DEFINE\-BEGIN\-PROJECT\-WRITER

```Lisp
(defmacro DEFINE-BEGIN-PROJECT-WRITER ((G11) &BODY BODY12)
  ...)
```

````
Define a function that will be called before the documentation of a project begins to generate. The function 
receives the pathname of the project root directory.
````

#### Macro: DEFINE\-END\-PROJECT\-WRITER

```Lisp
(defmacro DEFINE-END-PROJECT-WRITER ((G14) &BODY BODY15)
  ...)
```

````
Define a function that will be called after the documentation of a project has been generated. The function
receives the pathname of the project root directory.
````

#### Macro: DEFINE\-HEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-HEADER-WRITER ((G17 G18 G19) &BODY BODY20)
  ...)
```

````
Define a function that must print a header element. The function receives the stream to be written in, the
header name and the tag associated to it.
````

#### Macro: DEFINE\-SUBHEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SUBHEADER-WRITER ((G22 G23 G24) &BODY BODY25)
  ...)
```

````
Same as define-header-writer but it must print a subheader element.
````

#### Macro: DEFINE\-SUBSUBHEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SUBSUBHEADER-WRITER ((G27 G28 G29) &BODY BODY30)
  ...)
```

````
Same as define-header-writer but it must print a subsubheader element.
````

#### Macro: DEFINE\-TEXT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TEXT-WRITER ((G32 G33) &BODY BODY34)
  ...)
```

````
Define a function that must print a text element. It receives the stream to be written in, and a string with
the text.
````

#### Macro: DEFINE\-ESCAPE\-TEXT

```Lisp
(defmacro ADPSM:DEFINE-ESCAPE-TEXT ((G36) &BODY BODY37)
  ...)
```

````
Define a function that receives a string of text and must return another string. You want this to escape
special characters that will be used with bold, italic, header-ref, web-link, etc.
````

#### Macro: DEFINE\-BOLD\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-BOLD-WRITER ((G39 G40) &BODY BODY41)
  ...)
```

````
Define a function to print a bold text element. It receives the stream to be written in, and a string with
the text.
````

#### Macro: DEFINE\-ITALIC\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-ITALIC-WRITER ((G43 G44) &BODY BODY45)
  ...)
```

````
Same as define-bold-writer, but with an italic style.
````

#### Macro: DEFINE\-EMPHASIS\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-EMPHASIS-WRITER ((G47 G48) &BODY BODY49)
  ...)
```

````
Same as define-bold-writer, but with both bold and italic style.
````

#### Macro: DEFINE\-INLINE\-CODE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-INLINE-CODE-WRITER ((G51 G52) &BODY BODY53)
  ...)
```

````
Same as define-bold-writer, but with a code-inline style.
````

#### Macro: DEFINE\-HEADER\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-HEADER-REF-WRITER ((G55 G56 G57 G58) &BODY BODY59)
  ...)
```

````
Define a function to print a header reference element. It receives the stream to be written in, the tag
associated to a header element, the text of said header element, and the relative path to the place where the
header element is in.
````

#### Macro: DEFINE\-SYMBOL\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SYMBOL-REF-WRITER ((G61 G62 G63) &BODY BODY64)
  ...)
```

````
Same as define-header-ref-writer, but it prints a symbol reference. Also, it receives the stream, the tag
associated with the symbol, and the relative path to the place where the symbol definition is in.
````

#### Macro: DEFINE\-FUNCTION\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-FUNCTION-REF-WRITER ((G66 G67 G68) &BODY BODY69)
  ...)
```

````
Same as define-symbol-ref-writer, but it prints a function reference.
````

#### Macro: DEFINE\-TYPE\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TYPE-REF-WRITER ((G71 G72 G73) &BODY BODY74)
  ...)
```

````
Sama as define-symbol-ref-writer, but it prints a type reference.
````

#### Macro: DEFINE\-WEB\-LINK\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-WEB-LINK-WRITER ((G76 G77 G78) &BODY BODY79)
  ...)
```

````
Define a function to print a web link element. It receives the stream, the link text as a string, and the link address as a string.
````

#### Macro: DEFINE\-IMAGE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-IMAGE-WRITER ((G81 G82 G83) &BODY BODY84)
  ...)
```

````
Define a function to print an image element. It receives the stream, the alternative text as a string, and a
relative pathname to the image.
````

#### Macro: DEFINE\-TABLE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TABLE-WRITER ((G86 G87) &BODY BODY88)
  ...)
```

````
Define a function to print a table element. It receives the stream, and a list of lists of strings. Each
inner list is a row of the table, and each string is an element of the table.
````

#### Macro: DEFINE\-ITEMIZE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-ITEMIZE-WRITER ((G90 G91) &BODY BODY92)
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
(defmacro ADPSM:DEFINE-CODE-BLOCK-WRITER ((G94 G95 G96) &BODY BODY97)
  ...)
```

````
Define a function to print a code block element. It receives the stream, a string with the used programming 
language, and another string with the text to be placed in the block of code.
````

#### Macro: DEFINE\-CODE\-EXAMPLE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-CODE-EXAMPLE-WRITER ((G99 G100 G101 G102) &BODY BODY103)
  ...)
```

````
Define a function to print an example block. It receives the stream, a string with the code to be placed in 
a block of code, a string with the standard output of the code, and a list of elements returned by the code.
````

#### Macro: DEFINE\-DEFCLASS\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFCLASS-WRITER ((G105 G106) &BODY BODY107)
  ...)
```

````
Define a function to print a defclass definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFCONSTANT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFCONSTANT-WRITER ((G109 G110) &BODY BODY111)
  ...)
```

````
Define a function to print a defconstant definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFGENERIC\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFGENERIC-WRITER ((G113 G114) &BODY BODY115)
  ...)
```

````
Define a function to print a defgeneric definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-COMPILER\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-COMPILER-MACRO-WRITER ((G117 G118) &BODY BODY119)
  ...)
```

````
Define a function to print a define-compiler-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-CONDITION\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-CONDITION-WRITER ((G121 G122) &BODY BODY123)
  ...)
```

````
Define a function to print a define-condition definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-METHOD\-COMBINATION\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-METHOD-COMBINATION-WRITER ((G125 G126) &BODY
                                                         BODY127)
  ...)
```

````
Define a function to print a define-method-combination definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-MODIFY\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-MODIFY-MACRO-WRITER ((G129 G130) &BODY BODY131)
  ...)
```

````
Define a function to print a define-modify-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-SETF\-EXPANDER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-SETF-EXPANDER-WRITER ((G133 G134) &BODY BODY135)
  ...)
```

````
Define a function to print a define-setf-expander definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-SYMBOL\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-SYMBOL-MACRO-WRITER ((G137 G138) &BODY BODY139)
  ...)
```

````
Define a function to print a define-symbol-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFMACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFMACRO-WRITER ((G141 G142) &BODY BODY143)
  ...)
```

````
Define a function to print a defmacro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFMETHOD\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFMETHOD-WRITER ((G145 G146) &BODY BODY147)
  ...)
```

````
Define a function to print a defmethod definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFPACKAGE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFPACKAGE-WRITER ((G149 G150) &BODY BODY151)
  ...)
```

````
Define a function to print a defpackage definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFPARAMETER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFPARAMETER-WRITER ((G153 G154) &BODY BODY155)
  ...)
```

````
Define a function to print a defparameter definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFSETF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFSETF-WRITER ((G157 G158) &BODY BODY159)
  ...)
```

````
Define a function to print a defsetf definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFSTRUCT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFSTRUCT-WRITER ((G161 G162) &BODY BODY163)
  ...)
```

````
Define a function to print a defstruct definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFTYPE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFTYPE-WRITER ((G165 G166) &BODY BODY167)
  ...)
```

````
Define a function to print a deftype definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFUN\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFUN-WRITER ((G169 G170) &BODY BODY171)
  ...)
```

````
Define a function to print a defun definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFVAR\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFVAR-WRITER ((G173 G174) &BODY BODY175)
  ...)
```

````
Define a function to print a defvar definition. It receives the stream, and the definition expression.
````

## API function components

### Defclass components

#### Macro: WITH\-DEFCLASS\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFCLASS-COMPONENTS (((&REST COMPONENT-REST-ARGS179)
                                           FUNCTION-BODY-ARG180)
                                          &BODY BODY-ARG181)
  ...)
```

### Defconstant components

#### Macro: WITH\-DEFCONSTANT\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFCONSTANT-COMPONENTS (((&REST COMPONENT-REST-ARGS186)
                                              FUNCTION-BODY-ARG187)
                                             &BODY BODY-ARG188)
  ...)
```

### Defgeneric components

#### Macro: WITH\-DEFGENERIC\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFGENERIC-COMPONENTS (((&REST COMPONENT-REST-ARGS193)
                                             FUNCTION-BODY-ARG194)
                                            &BODY BODY-ARG195)
  ...)
```

### Define\-compiler\-macro components

#### Macro: WITH\-DEFINE\-COMPILER\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-COMPILER-MACRO-COMPONENTS (((&REST
                                                         COMPONENT-REST-ARGS200)
                                                        FUNCTION-BODY-ARG201)
                                                       &BODY BODY-ARG202)
  ...)
```

### Define\-condition components

#### Macro: WITH\-DEFINE\-CONDITION\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-CONDITION-COMPONENTS (((&REST
                                                    COMPONENT-REST-ARGS207)
                                                   FUNCTION-BODY-ARG208)
                                                  &BODY BODY-ARG209)
  ...)
```

### Define\-method\-combination components

#### Macro: WITH\-DEFINE\-METHOD\-COMBINATION\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-METHOD-COMBINATION-COMPONENTS (((&REST
                                                             COMPONENT-REST-ARGS214)
                                                            FUNCTION-BODY-ARG215)
                                                           &BODY BODY-ARG216)
  ...)
```

### Define\-modify\-macro components

#### Macro: WITH\-DEFINE\-MODIFY\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-MODIFY-MACRO-COMPONENTS (((&REST
                                                       COMPONENT-REST-ARGS221)
                                                      FUNCTION-BODY-ARG222)
                                                     &BODY BODY-ARG223)
  ...)
```

### Define\-setf\-expander components

#### Macro: WITH\-DEFINE\-SETF\-EXPANDER\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-SETF-EXPANDER-COMPONENTS (((&REST
                                                        COMPONENT-REST-ARGS228)
                                                       FUNCTION-BODY-ARG229)
                                                      &BODY BODY-ARG230)
  ...)
```

### Define\-symbol\-macro components

#### Macro: WITH\-DEFINE\-SYMBOL\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-SYMBOL-MACRO-COMPONENTS (((&REST
                                                       COMPONENT-REST-ARGS235)
                                                      FUNCTION-BODY-ARG236)
                                                     &BODY BODY-ARG237)
  ...)
```

### Defmacro components

#### Macro: WITH\-DEFMACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFMACRO-COMPONENTS (((&REST COMPONENT-REST-ARGS242)
                                           FUNCTION-BODY-ARG243)
                                          &BODY BODY-ARG244)
  ...)
```

### defmethod components

#### Macro: WITH\-DEFMETHOD\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFMETHOD-COMPONENTS (((&REST COMPONENT-REST-ARGS249)
                                            FUNCTION-BODY-ARG250)
                                           &BODY BODY-ARG251)
  ...)
```

### Defpackage components

#### Macro: WITH\-DEFPACKAGE\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFPACKAGE-COMPONENTS (((&REST COMPONENT-REST-ARGS256)
                                             FUNCTION-BODY-ARG257)
                                            &BODY BODY-ARG258)
  ...)
```

### Defparameter components

#### Macro: WITH\-DEFPARAMETER\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFPARAMETER-COMPONENTS (((&REST COMPONENT-REST-ARGS263)
                                               FUNCTION-BODY-ARG264)
                                              &BODY BODY-ARG265)
  ...)
```

### Defsetf components

#### Macro: WITH\-DEFSETF\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFSETF-COMPONENTS (((&REST COMPONENT-REST-ARGS270)
                                          FUNCTION-BODY-ARG271)
                                         &BODY BODY-ARG272)
  ...)
```

### Defstruct components

#### Macro: WITH\-DEFSTRUCT\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFSTRUCT-COMPONENTS (((&REST COMPONENT-REST-ARGS277)
                                            FUNCTION-BODY-ARG278)
                                           &BODY BODY-ARG279)
  ...)
```

### Deftype components

#### Macro: WITH\-DEFTYPE\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFTYPE-COMPONENTS (((&REST COMPONENT-REST-ARGS284)
                                          FUNCTION-BODY-ARG285)
                                         &BODY BODY-ARG286)
  ...)
```

### Defun components

#### Macro: WITH\-DEFUN\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFUN-COMPONENTS (((&REST COMPONENT-REST-ARGS291)
                                        FUNCTION-BODY-ARG292)
                                       &BODY BODY-ARG293)
  ...)
```

### Defvar components

#### Macro: WITH\-DEFVAR\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFVAR-COMPONENTS (((&REST COMPONENT-REST-ARGS298)
                                         FUNCTION-BODY-ARG299)
                                        &BODY BODY-ARG300)
  ...)
```

