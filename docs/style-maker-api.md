# Style\-maker API

## ADP pprint dispatch

## Style parameters

## Writers

#### Macro: DEFINE\-BEGIN\-FILE\-WRITER

```Lisp
(defmacro ADPSM::DEFINE-BEGIN-FILE-WRITER ((G176) &BODY BODY177)
  ...)
```

````
Define a function that will be called when the file is about to be written in. The function receives a stream 
associated with said file.
````

#### Macro: DEFINE\-END\-FILE\-WRITER

```Lisp
(defmacro ADPSM::DEFINE-END-FILE-WRITER ((G179) &BODY BODY180)
  ...)
```

````
Define a function that will be called after a file has finished of being written in. The function receives a
stream associated with said file.
````

#### Macro: DEFINE\-FILE\-EXTENSION

```Lisp
(defmacro ADPSM:DEFINE-FILE-EXTENSION (NIL &BODY BODY182)
  ...)
```

````
Define a function that must return a string indicating the extension of the files that ADP will create.
````

#### Macro: DEFINE\-BEGIN\-PROJECT\-WRITER

```Lisp
(defmacro ADPSM::DEFINE-BEGIN-PROJECT-WRITER ((G184) &BODY BODY185)
  ...)
```

````
Define a function that will be called before the documentation of a project begins to generate. The function 
receives the pathname of the project root directory.
````

#### Macro: DEFINE\-END\-PROJECT\-WRITER

```Lisp
(defmacro ADPSM::DEFINE-END-PROJECT-WRITER ((G187) &BODY BODY188)
  ...)
```

````
Define a function that will be called after the documentation of a project has been generated. The function
receives the pathname of the project root directory.
````

#### Macro: DEFINE\-HEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-HEADER-WRITER ((G190 G191 G192) &BODY BODY193)
  ...)
```

````
Define a function that must print a header element. The function receives the stream to be written in, the
header name and the tag associated to it.
````

#### Macro: DEFINE\-SUBHEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SUBHEADER-WRITER ((G195 G196 G197) &BODY BODY198)
  ...)
```

````
Same as define-header-writer but it must print a subheader element.
````

#### Macro: DEFINE\-SUBSUBHEADER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SUBSUBHEADER-WRITER ((G200 G201 G202) &BODY BODY203)
  ...)
```

````
Same as define-header-writer but it must print a subsubheader element.
````

#### Macro: DEFINE\-TEXT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TEXT-WRITER ((G205 G206) &BODY BODY207)
  ...)
```

````
Define a function that must print a text element. It receives the stream to be written in, and a string with
the text.
````

#### Macro: DEFINE\-ESCAPE\-TEXT

```Lisp
(defmacro ADPSM:DEFINE-ESCAPE-TEXT ((G209) &BODY BODY210)
  ...)
```

````
Define a function that receives a string of text and must return another string. You want this to escape
special characters that will be used with bold, italic, header-ref, web-link, etc.
````

#### Macro: DEFINE\-BOLD\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-BOLD-WRITER ((G212 G213) &BODY BODY214)
  ...)
```

````
Define a function to print a bold text element. It receives the stream to be written in, and a string with
the text.
````

#### Macro: DEFINE\-ITALIC\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-ITALIC-WRITER ((G216 G217) &BODY BODY218)
  ...)
```

````
Same as define-bold-writer, but with an italic style.
````

#### Macro: DEFINE\-EMPHASIS\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-EMPHASIS-WRITER ((G220 G221) &BODY BODY222)
  ...)
```

````
Same as define-bold-writer, but with both bold and italic style.
````

#### Macro: DEFINE\-INLINE\-CODE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-INLINE-CODE-WRITER ((G224 G225) &BODY BODY226)
  ...)
```

````
Same as define-bold-writer, but with a code-inline style.
````

#### Macro: DEFINE\-HEADER\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-HEADER-REF-WRITER ((G228 G229 G230 G231) &BODY BODY232)
  ...)
```

````
Define a function to print a header reference element. It receives the stream to be written in, the tag
associated to a header element, the text of said header element, and the relative path to the place where the
header element is in.
````

#### Macro: DEFINE\-SYMBOL\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-SYMBOL-REF-WRITER ((G234 G235 G236) &BODY BODY237)
  ...)
```

````
Same as define-header-ref-writer, but it prints a symbol reference. Also, it receives the stream, the tag
associated with the symbol, and the relative path to the place where the symbol definition is in.
````

#### Macro: DEFINE\-FUNCTION\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-FUNCTION-REF-WRITER ((G239 G240 G241) &BODY BODY242)
  ...)
```

````
Same as define-symbol-ref-writer, but it prints a function reference.
````

#### Macro: DEFINE\-TYPE\-REF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TYPE-REF-WRITER ((G244 G245 G246) &BODY BODY247)
  ...)
```

````
Sama as define-symbol-ref-writer, but it prints a type reference.
````

#### Macro: DEFINE\-WEB\-LINK\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-WEB-LINK-WRITER ((G249 G250 G251) &BODY BODY252)
  ...)
```

````
Define a function to print a web link element. It receives the stream, the link text as a string, and the link address as a string.
````

#### Macro: DEFINE\-IMAGE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-IMAGE-WRITER ((G254 G255 G256) &BODY BODY257)
  ...)
```

````
Define a function to print an image element. It receives the stream, the alternative text as a string, and a
relative pathname to the image.
````

#### Macro: DEFINE\-TABLE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-TABLE-WRITER ((G259 G260) &BODY BODY261)
  ...)
```

````
Define a function to print a table element. It receives the stream, and a list of lists of strings. Each
inner list is a row of the table, and each string is an element of the table.
````

#### Macro: DEFINE\-ITEMIZE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-ITEMIZE-WRITER ((G263 G264) &BODY BODY265)
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
(defmacro ADPSM:DEFINE-CODE-BLOCK-WRITER ((G267 G268 G269) &BODY BODY270)
  ...)
```

````
Define a function to print a code block element. It receives the stream, a string with the used programming 
language, and another string with the text to be placed in the block of code.
````

#### Macro: DEFINE\-CODE\-EXAMPLE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-CODE-EXAMPLE-WRITER ((G272 G273 G274 G275) &BODY BODY276)
  ...)
```

````
Define a function to print an example block. It receives the stream, a string with the code to be placed in 
a block of code, a string with the standard output of the code, and a list of elements returned by the code.
````

#### Macro: DEFINE\-DEFCLASS\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFCLASS-WRITER ((G278 G279) &BODY BODY280)
  ...)
```

````
Define a function to print a defclass definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFCONSTANT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFCONSTANT-WRITER ((G282 G283) &BODY BODY284)
  ...)
```

````
Define a function to print a defconstant definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFGENERIC\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFGENERIC-WRITER ((G286 G287) &BODY BODY288)
  ...)
```

````
Define a function to print a defgeneric definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-COMPILER\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-COMPILER-MACRO-WRITER ((G290 G291) &BODY BODY292)
  ...)
```

````
Define a function to print a define-compiler-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-CONDITION\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-CONDITION-WRITER ((G294 G295) &BODY BODY296)
  ...)
```

````
Define a function to print a define-condition definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-METHOD\-COMBINATION\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-METHOD-COMBINATION-WRITER ((G298 G299) &BODY
                                                         BODY300)
  ...)
```

````
Define a function to print a define-method-combination definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-MODIFY\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-MODIFY-MACRO-WRITER ((G302 G303) &BODY BODY304)
  ...)
```

````
Define a function to print a define-modify-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-SETF\-EXPANDER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-SETF-EXPANDER-WRITER ((G306 G307) &BODY BODY308)
  ...)
```

````
Define a function to print a define-setf-expander definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFINE\-SYMBOL\-MACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFINE-SYMBOL-MACRO-WRITER ((G310 G311) &BODY BODY312)
  ...)
```

````
Define a function to print a define-symbol-macro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFMACRO\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFMACRO-WRITER ((G314 G315) &BODY BODY316)
  ...)
```

````
Define a function to print a defmacro definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFMETHOD\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFMETHOD-WRITER ((G318 G319) &BODY BODY320)
  ...)
```

````
Define a function to print a defmethod definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFPACKAGE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFPACKAGE-WRITER ((G322 G323) &BODY BODY324)
  ...)
```

````
Define a function to print a defpackage definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFPARAMETER\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFPARAMETER-WRITER ((G326 G327) &BODY BODY328)
  ...)
```

````
Define a function to print a defparameter definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFSETF\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFSETF-WRITER ((G330 G331) &BODY BODY332)
  ...)
```

````
Define a function to print a defsetf definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFSTRUCT\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFSTRUCT-WRITER ((G334 G335) &BODY BODY336)
  ...)
```

````
Define a function to print a defstruct definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFTYPE\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFTYPE-WRITER ((G338 G339) &BODY BODY340)
  ...)
```

````
Define a function to print a deftype definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFUN\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFUN-WRITER ((G342 G343) &BODY BODY344)
  ...)
```

````
Define a function to print a defun definition. It receives the stream, and the definition expression.
````

#### Macro: DEFINE\-DEFVAR\-WRITER

```Lisp
(defmacro ADPSM:DEFINE-DEFVAR-WRITER ((G346 G347) &BODY BODY348)
  ...)
```

````
Define a function to print a defvar definition. It receives the stream, and the definition expression.
````

## API function components

### Defclass components

#### Macro: WITH\-DEFCLASS\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFCLASS-COMPONENTS (((&REST COMPONENT-REST-ARGS352)
                                           FUNCTION-BODY-ARG353)
                                          &BODY BODY-ARG354)
  ...)
```

### Defconstant components

#### Macro: WITH\-DEFCONSTANT\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFCONSTANT-COMPONENTS (((&REST COMPONENT-REST-ARGS359)
                                              FUNCTION-BODY-ARG360)
                                             &BODY BODY-ARG361)
  ...)
```

### Defgeneric components

#### Macro: WITH\-DEFGENERIC\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFGENERIC-COMPONENTS (((&REST COMPONENT-REST-ARGS366)
                                             FUNCTION-BODY-ARG367)
                                            &BODY BODY-ARG368)
  ...)
```

### Define\-compiler\-macro components

#### Macro: WITH\-DEFINE\-COMPILER\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-COMPILER-MACRO-COMPONENTS (((&REST
                                                         COMPONENT-REST-ARGS373)
                                                        FUNCTION-BODY-ARG374)
                                                       &BODY BODY-ARG375)
  ...)
```

### Define\-condition components

#### Macro: WITH\-DEFINE\-CONDITION\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-CONDITION-COMPONENTS (((&REST
                                                    COMPONENT-REST-ARGS380)
                                                   FUNCTION-BODY-ARG381)
                                                  &BODY BODY-ARG382)
  ...)
```

### Define\-method\-combination components

#### Macro: WITH\-DEFINE\-METHOD\-COMBINATION\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-METHOD-COMBINATION-COMPONENTS (((&REST
                                                             COMPONENT-REST-ARGS387)
                                                            FUNCTION-BODY-ARG388)
                                                           &BODY BODY-ARG389)
  ...)
```

### Define\-modify\-macro components

#### Macro: WITH\-DEFINE\-MODIFY\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-MODIFY-MACRO-COMPONENTS (((&REST
                                                       COMPONENT-REST-ARGS394)
                                                      FUNCTION-BODY-ARG395)
                                                     &BODY BODY-ARG396)
  ...)
```

### Define\-setf\-expander components

#### Macro: WITH\-DEFINE\-SETF\-EXPANDER\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-SETF-EXPANDER-COMPONENTS (((&REST
                                                        COMPONENT-REST-ARGS401)
                                                       FUNCTION-BODY-ARG402)
                                                      &BODY BODY-ARG403)
  ...)
```

### Define\-symbol\-macro components

#### Macro: WITH\-DEFINE\-SYMBOL\-MACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFINE-SYMBOL-MACRO-COMPONENTS (((&REST
                                                       COMPONENT-REST-ARGS408)
                                                      FUNCTION-BODY-ARG409)
                                                     &BODY BODY-ARG410)
  ...)
```

### Defmacro components

#### Macro: WITH\-DEFMACRO\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFMACRO-COMPONENTS (((&REST COMPONENT-REST-ARGS415)
                                           FUNCTION-BODY-ARG416)
                                          &BODY BODY-ARG417)
  ...)
```

### defmethod components

#### Macro: WITH\-DEFMETHOD\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFMETHOD-COMPONENTS (((&REST COMPONENT-REST-ARGS422)
                                            FUNCTION-BODY-ARG423)
                                           &BODY BODY-ARG424)
  ...)
```

### Defpackage components

#### Macro: WITH\-DEFPACKAGE\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFPACKAGE-COMPONENTS (((&REST COMPONENT-REST-ARGS429)
                                             FUNCTION-BODY-ARG430)
                                            &BODY BODY-ARG431)
  ...)
```

### Defparameter components

#### Macro: WITH\-DEFPARAMETER\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFPARAMETER-COMPONENTS (((&REST COMPONENT-REST-ARGS436)
                                               FUNCTION-BODY-ARG437)
                                              &BODY BODY-ARG438)
  ...)
```

### Defsetf components

#### Macro: WITH\-DEFSETF\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFSETF-COMPONENTS (((&REST COMPONENT-REST-ARGS443)
                                          FUNCTION-BODY-ARG444)
                                         &BODY BODY-ARG445)
  ...)
```

### Defstruct components

#### Macro: WITH\-DEFSTRUCT\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFSTRUCT-COMPONENTS (((&REST COMPONENT-REST-ARGS450)
                                            FUNCTION-BODY-ARG451)
                                           &BODY BODY-ARG452)
  ...)
```

### Deftype components

#### Macro: WITH\-DEFTYPE\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFTYPE-COMPONENTS (((&REST COMPONENT-REST-ARGS457)
                                          FUNCTION-BODY-ARG458)
                                         &BODY BODY-ARG459)
  ...)
```

### Defun components

#### Macro: WITH\-DEFUN\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFUN-COMPONENTS (((&REST COMPONENT-REST-ARGS464)
                                        FUNCTION-BODY-ARG465)
                                       &BODY BODY-ARG466)
  ...)
```

### Defvar components

#### Macro: WITH\-DEFVAR\-COMPONENTS

```Lisp
(defmacro ADPSM:WITH-DEFVAR-COMPONENTS (((&REST COMPONENT-REST-ARGS471)
                                         FUNCTION-BODY-ARG472)
                                        &BODY BODY-ARG473)
  ...)
```

