# Style\-maker API

## ADP pprint dispatch

## Style parameters

## Writers

#### Macro: DEFINE\-BEGIN\-FILE\-WRITER

```Lisp
(defmacro ADPSM::DEFINE-BEGIN-FILE-WRITER ((G170) &BODY BODY171)
  ...)
```

````
Define a function that will be called when the file is about to be written in. The function receives a stream 
associated with said file.
````

#### Macro: DEFINE\-END\-FILE\-WRITER

```Lisp
(defmacro ADPSM::DEFINE-END-FILE-WRITER ((G173) &BODY BODY174)
  ...)
```

````
Define a function that will be called after a file has finished of being written in. The function receives a
stream associated with said file.
````

#### Macro: DEFINE\-FILE\-EXTENSION

```Lisp
(defmacro ADPSM:DEFINE-FILE-EXTENSION (NIL &BODY BODY176)
  ...)
```

````
Define a function that must return a string indicating the extension of the files that ADP will create.
````

#### Macro: DEFINE\-BEGIN\-PROJECT\-WRITER

```Lisp
(defmacro ADPSM::DEFINE-BEGIN-PROJECT-WRITER ((G178) &BODY BODY179)
  ...)
```

````
Define a function that will be called before the documentation of a project begins to generate. The function 
receives the pathname of the project root directory.
````

#### Macro: DEFINE\-END\-PROJECT\-WRITER

```Lisp
(defmacro ADPSM::DEFINE-END-PROJECT-WRITER ((G181) &BODY BODY182)
  ...)
```

````
Define a function that will be called after the documentation of a project has been generated. The function
receives the pathname of the project root directory.
````

#### Macro: DEFINE\-HEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-HEADER-WRITER ((G184 G185 G186) &BODY BODY187)
  ...)
```

````
Define a function that must print a header element. The function receives the stream to be written in, the
header name and the tag associated to it.
````

#### Macro: DEFINE\-SUBHEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SUBHEADER-WRITER ((G189 G190 G191) &BODY BODY192)
  ...)
```

````
Same as define-header-writer but it must print a subheader element.
````

#### Macro: DEFINE\-SUBSUBHEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SUBSUBHEADER-WRITER ((G194 G195 G196) &BODY BODY197)
  ...)
```

````
Same as define-header-writer but it must print a subsubheader element.
````

#### Macro: DEFINE\-TEXT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TEXT-WRITER ((G199 G200) &BODY BODY201)
  ...)
```

````
Define a function that must print a text element. It receives the stream to be written in, and a string with
the text.
````

#### Macro: DEFINE\-ESCAPE\-TEXT

```Lisp
(defmacro ADPSM:DEFINE-ESCAPE-TEXT ((G203) &BODY BODY204)
  ...)
```

````
Define a function that receives a string of text and must return another string. You want this to escape
special characters that will be used with bold, italic, header-ref, web-link, etc.
````

#### Macro: DEFINE\-BOLD\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-BOLD-WRITER ((G206 G207) &BODY BODY208)
  ...)
```

````
Define a function to print a bold text element. It receives the stream to be written in, and a string with
the text.
````

#### Macro: DEFINE\-ITALIC\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-ITALIC-WRITER ((G210 G211) &BODY BODY212)
  ...)
```

````
Same as define-bold-writer, but with an italic style.
````

#### Macro: DEFINE\-EMPHASIS\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-EMPHASIS-WRITER ((G214 G215) &BODY BODY216)
  ...)
```

````
Same as define-bold-writer, but with both bold and italic style.
````

#### Macro: DEFINE\-INLINE\-CODE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-INLINE-CODE-WRITER ((G218 G219) &BODY BODY220)
  ...)
```

````
Same as define-bold-writer, but with a code-inline style.
````

#### Macro: DEFINE\-HEADER\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-HEADER-REF-WRITER ((G222 G223 G224 G225) &BODY BODY226)
  ...)
```

````
Define a function to print a header reference element. It receives the stream to be written in, the tag
associated to a header element, the text of said header element, and the relative path to the place where the
header element is in.
````

#### Macro: DEFINE\-SYMBOL\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SYMBOL-REF-WRITER ((G228 G229 G230) &BODY BODY231)
  ...)
```

````
Same as define-header-ref-writer, but it prints a symbol reference. Also, it receives the stream, the tag
associated with the symbol, and the relative path to the place where the symbol definition is in.
````

#### Macro: DEFINE\-FUNCTION\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-FUNCTION-REF-WRITER ((G233 G234 G235) &BODY BODY236)
  ...)
```

````
Same as define-symbol-ref-writer, but it prints a function reference.
````

#### Macro: DEFINE\-TYPE\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TYPE-REF-WRITER ((G238 G239 G240) &BODY BODY241)
  ...)
```

````
Sama as define-symbol-ref-writer, but it prints a type reference.
````

#### Macro: DEFINE\-WEB\-LINK\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-WEB-LINK-WRITER ((G243 G244 G245) &BODY BODY246)
  ...)
```

````
Define a function to print a web link element. It receives the stream, the link text as a string, and the link address as a string.
````

#### Macro: DEFINE\-IMAGE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-IMAGE-WRITER ((G248 G249 G250) &BODY BODY251)
  ...)
```

````
Define a function to print an image element. It receives the stream, the alternative text as a string, and a
relative pathname to the image.
````

#### Macro: DEFINE\-TABLE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TABLE-WRITER ((G253 G254) &BODY BODY255)
  ...)
```

````
Define a function to print a table element. It receives the stream, and a list of lists of strings. Each
inner list is a row of the table, and each string is an element of the table.
````

#### Macro: DEFINE\-ITEMIZE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-ITEMIZE-WRITER ((G257 G258) &BODY BODY259)
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
(defmacro ADPSM:DEFINE-CODE-BLOCK-WRITER ((G261 G262 G263) &BODY BODY264)
  ...)
```

````
Define a function to print a code block element. It receives the stream, a string with the used programming 
language, and another string with the text to be placed in the block of code.
````

#### Macro: DEFINE\-CODE\-EXAMPLE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-CODE-EXAMPLE-WRITER ((G266 G267 G268 G269) &BODY BODY270)
  ...)
```

````
Define a function to print an example block. It receives the stream, a string with the code to be placed in 
a block of code, a string with the standard output of the code, and a list of elements returned by the code.
````

#### Macro: DEFINE\-DEFCLASS\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFCLASS-WRITER ((G272 G273) &BODY BODY274)
  ...)
```

````
Define a function to print a defclass definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFCONSTANT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFCONSTANT-WRITER ((G276 G277) &BODY BODY278)
  ...)
```

````
Define a function to print a defconstant definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFGENERIC\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFGENERIC-WRITER ((G280 G281) &BODY BODY282)
  ...)
```

````
Define a function to print a defgeneric definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-COMPILER\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-COMPILER-MACRO-WRITER ((G284 G285) &BODY BODY286)
  ...)
```

````
Define a function to print a define-compiler-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-CONDITION\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-CONDITION-WRITER ((G288 G289) &BODY BODY290)
  ...)
```

````
Define a function to print a define-condition definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-METHOD\-COMBINATION\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-METHOD-COMBINATION-WRITER ((G292 G293) &BODY
                                                         BODY294)
  ...)
```

````
Define a function to print a define-method-combination definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-MODIFY\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-MODIFY-MACRO-WRITER ((G296 G297) &BODY BODY298)
  ...)
```

````
Define a function to print a define-modify-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-SETF\-EXPANDER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-SETF-EXPANDER-WRITER ((G300 G301) &BODY BODY302)
  ...)
```

````
Define a function to print a define-setf-expander definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-SYMBOL\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-SYMBOL-MACRO-WRITER ((G304 G305) &BODY BODY306)
  ...)
```

````
Define a function to print a define-symbol-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFMACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFMACRO-WRITER ((G308 G309) &BODY BODY310)
  ...)
```

````
Define a function to print a defmacro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFMETHOD\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFMETHOD-WRITER ((G312 G313) &BODY BODY314)
  ...)
```

````
Define a function to print a defmethod definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFPACKAGE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFPACKAGE-WRITER ((G316 G317) &BODY BODY318)
  ...)
```

````
Define a function to print a defpackage definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFPARAMETER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFPARAMETER-WRITER ((G320 G321) &BODY BODY322)
  ...)
```

````
Define a function to print a defparameter definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFSETF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFSETF-WRITER ((G324 G325) &BODY BODY326)
  ...)
```

````
Define a function to print a defsetf definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFSTRUCT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFSTRUCT-WRITER ((G328 G329) &BODY BODY330)
  ...)
```

````
Define a function to print a defstruct definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFTYPE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFTYPE-WRITER ((G332 G333) &BODY BODY334)
  ...)
```

````
Define a function to print a deftype definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFUN\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFUN-WRITER ((G336 G337) &BODY BODY338)
  ...)
```

````
Define a function to print a defun definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFVAR\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFVAR-WRITER ((G340 G341) &BODY BODY342)
  ...)
```

````
Define a function to print a defvar definition. It receives the stream, and the definition expression.
````

## API function components

### Defclass components

#### Macro: WITH\-DEFCLASS\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFCLASS-COMPONENTS (((&REST COMPONENT-REST-ARGS346)
                                           FUNCTION-BODY-ARG347)
                                          &BODY BODY-ARG348)
  ...)
```

### Defconstant components

#### Macro: WITH\-DEFCONSTANT\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFCONSTANT-COMPONENTS (((&REST COMPONENT-REST-ARGS353)
                                              FUNCTION-BODY-ARG354)
                                             &BODY BODY-ARG355)
  ...)
```

### Defgeneric components

#### Macro: WITH\-DEFGENERIC\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFGENERIC-COMPONENTS (((&REST COMPONENT-REST-ARGS360)
                                             FUNCTION-BODY-ARG361)
                                            &BODY BODY-ARG362)
  ...)
```

### Define\-compiler\-macro components

#### Macro: WITH\-DEFINE\-COMPILER\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-COMPILER-MACRO-COMPONENTS (((&REST
                                                         COMPONENT-REST-ARGS367)
                                                        FUNCTION-BODY-ARG368)
                                                       &BODY BODY-ARG369)
  ...)
```

### Define\-condition components

#### Macro: WITH\-DEFINE\-CONDITION\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-CONDITION-COMPONENTS (((&REST
                                                    COMPONENT-REST-ARGS374)
                                                   FUNCTION-BODY-ARG375)
                                                  &BODY BODY-ARG376)
  ...)
```

### Define\-method\-combination components

#### Macro: WITH\-DEFINE\-METHOD\-COMBINATION\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-METHOD-COMBINATION-COMPONENTS (((&REST
                                                             COMPONENT-REST-ARGS381)
                                                            FUNCTION-BODY-ARG382)
                                                           &BODY BODY-ARG383)
  ...)
```

### Define\-modify\-macro components

#### Macro: WITH\-DEFINE\-MODIFY\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-MODIFY-MACRO-COMPONENTS (((&REST
                                                       COMPONENT-REST-ARGS388)
                                                      FUNCTION-BODY-ARG389)
                                                     &BODY BODY-ARG390)
  ...)
```

### Define\-setf\-expander components

#### Macro: WITH\-DEFINE\-SETF\-EXPANDER\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-SETF-EXPANDER-COMPONENTS (((&REST
                                                        COMPONENT-REST-ARGS395)
                                                       FUNCTION-BODY-ARG396)
                                                      &BODY BODY-ARG397)
  ...)
```

### Define\-symbol\-macro components

#### Macro: WITH\-DEFINE\-SYMBOL\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-SYMBOL-MACRO-COMPONENTS (((&REST
                                                       COMPONENT-REST-ARGS402)
                                                      FUNCTION-BODY-ARG403)
                                                     &BODY BODY-ARG404)
  ...)
```

### Defmacro components

#### Macro: WITH\-DEFMACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFMACRO-COMPONENTS (((&REST COMPONENT-REST-ARGS409)
                                           FUNCTION-BODY-ARG410)
                                          &BODY BODY-ARG411)
  ...)
```

### defmethod components

#### Macro: WITH\-DEFMETHOD\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFMETHOD-COMPONENTS (((&REST COMPONENT-REST-ARGS416)
                                            FUNCTION-BODY-ARG417)
                                           &BODY BODY-ARG418)
  ...)
```

### Defpackage components

#### Macro: WITH\-DEFPACKAGE\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFPACKAGE-COMPONENTS (((&REST COMPONENT-REST-ARGS423)
                                             FUNCTION-BODY-ARG424)
                                            &BODY BODY-ARG425)
  ...)
```

### Defparameter components

#### Macro: WITH\-DEFPARAMETER\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFPARAMETER-COMPONENTS (((&REST COMPONENT-REST-ARGS430)
                                               FUNCTION-BODY-ARG431)
                                              &BODY BODY-ARG432)
  ...)
```

### Defsetf components

#### Macro: WITH\-DEFSETF\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFSETF-COMPONENTS (((&REST COMPONENT-REST-ARGS437)
                                          FUNCTION-BODY-ARG438)
                                         &BODY BODY-ARG439)
  ...)
```

### Defstruct components

#### Macro: WITH\-DEFSTRUCT\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFSTRUCT-COMPONENTS (((&REST COMPONENT-REST-ARGS444)
                                            FUNCTION-BODY-ARG445)
                                           &BODY BODY-ARG446)
  ...)
```

### Deftype components

#### Macro: WITH\-DEFTYPE\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFTYPE-COMPONENTS (((&REST COMPONENT-REST-ARGS451)
                                          FUNCTION-BODY-ARG452)
                                         &BODY BODY-ARG453)
  ...)
```

### Defun components

#### Macro: WITH\-DEFUN\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFUN-COMPONENTS (((&REST COMPONENT-REST-ARGS458)
                                        FUNCTION-BODY-ARG459)
                                       &BODY BODY-ARG460)
  ...)
```

### Defvar components

#### Macro: WITH\-DEFVAR\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFVAR-COMPONENTS (((&REST COMPONENT-REST-ARGS465)
                                         FUNCTION-BODY-ARG466)
                                        &BODY BODY-ARG467)
  ...)
```

