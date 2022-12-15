<h1 id="ADP:USER-API-HEADER">ADP User Interface</h1>

<h2 id="ADP:HEADERTAG0">Literate programming functions</h2>

#### Macro: HEADER

```Lisp
(defmacro ADP:HEADER (STR &OPTIONAL TAG)
  ...)
```

````
Add a header with name str. Also, if tag is not nil but a symbol, a new header-tag is created.
````

#### Macro: SUBHEADER

```Lisp
(defmacro ADP:SUBHEADER (STR &OPTIONAL TAG)
  ...)
```

````
Add a subheader with name str. Also, if tag is not nil but a symbol, a new header-tag is created.
````

#### Macro: SUBSUBHEADER

```Lisp
(defmacro ADP:SUBSUBHEADER (STR &OPTIONAL TAG)
  ...)
```

````
Add a subsubheader with name str. Also, if tag is not nil but a symbol, a new header-tag is created.
````

#### Macro: TEXT

```Lisp
(defmacro ADP:TEXT (&REST OBJECTS)
  ...)
```

````
Add plain text. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your text: bold, italic, emphasis, inline-code, web-link, header-ref, symbol-ref, function-ref and type-ref.
````

#### Macro: CELL

```Lisp
(defmacro ADP:CELL (&REST OBJECTS)
  ...)
```

````
Create a cell to place into a table. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your cell text: bold, italic, emphasis, inline-code, web-link, header-ref, symbol-ref, function-ref and type-ref.
````

#### Macro: TABLE

```Lisp
(defmacro ADP:TABLE (&REST ROWS)
  ...)
```

````
Add a table. Each argument must be a list of text macro calls.
````

#### Macro: ITEM

```Lisp
(defmacro ADP:ITEM (&REST ITEMS)
  ...)
```

````
Create an item to be placed into a iterate/enumerate form. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your cell text: bold, italic, emphasis, inline-code, web-link, header-ref, symbol-ref, function-ref and type-ref.
````

#### Macro: ITEMIZE

```Lisp
(defmacro ADP:ITEMIZE (&WHOLE ITEMIZE-FORM &REST ITEMS)
  ...)
```

````
Add a list of items. Each argument must be a list. Each list must start with the symbol item, itemize or enumerate. If 
item is used, the rest of the elements in that list will be treated as if using the macro text. If itemize or enumerate is used the rest 
of elements must be lists that must start with item, itemize or enumerate. In other words, when itemize or enumerate is used 
a nested list is added. A certain symbol will be printed before each element of the list.
````

#### Macro: ENUMERATE

```Lisp
(defmacro ADP:ENUMERATE (&WHOLE ENUMERATE-FORM &REST ITEMS)
  ...)
```

````
Same as itemize, but a number is printed before each element.
````

#### Macro: TABLE\-OF\-CONTENTS

```Lisp
(defmacro ADP:TABLE-OF-CONTENTS NIL
  ...)
```

````
Add a list of all headers and subheaders used in the system. The headers from different
files are shown in the same order the files are loaded.
````

#### Macro: MINI\-TABLE\-OF\-CONTENTS

```Lisp
(defmacro ADP:MINI-TABLE-OF-CONTENTS NIL
  ...)
```

````
Add a list of all headers, subheaders and subsubheaders used in the current documentation file.
````

#### Macro: TABLE\-OF\-FUNCTIONS

```Lisp
(defmacro ADP:TABLE-OF-FUNCTIONS NIL
  ...)
```

````
Add an ordered list of all functions and macros defined using ADP.
````

#### Macro: TABLE\-OF\-SYMBOLS

```Lisp
(defmacro ADP:TABLE-OF-SYMBOLS NIL
  ...)
```

````
Add an ordered list of all variables defined using ADP.
````

#### Macro: TABLE\-OF\-TYPES

```Lisp
(defmacro ADP:TABLE-OF-TYPES NIL
  ...)
```

````
Add an ordered list of all types defined using ADP.
````

#### Macro: IMAGE

```Lisp
(defmacro ADP:IMAGE (ALT-TEXT PATH)
  ...)
```

````
Add an image with alt-text as the alternative text and path must be the pathname, relative to the system's root directory, 
where the image is located.
````

#### Macro: BOLD

```Lisp
(defmacro ADP:BOLD (&REST ARGS)
  ...)
```

````
Add bold style to text. Each argument is princ-ed and concatenated into a string.
````

#### Macro: ITALIC

```Lisp
(defmacro ADP:ITALIC (&REST ARGS)
  ...)
```

````
Add italic style to text. Each argument is princ-ed and concatenated into a string.
````

#### Macro: EMPHASIS

```Lisp
(defmacro ADP:EMPHASIS (&REST ARGS)
  ...)
```

````
Add bold and italic style to text. Each argument is princ-ed and concatenated into a string.
````

#### Macro: INLINE\-CODE

```Lisp
(defmacro ADP:INLINE-CODE (&REST ARGS)
  ...)
```

````
Add inlined style to text. Each argument is princ-ed and concatenated into a string.
````

#### Macro: WEB\-LINK

```Lisp
(defmacro ADP:WEB-LINK (NAME LINK)
  ...)
```

````
Add a hyperlink. The text showed is name and link must be a valid web URL. Both arguments must be strings.
````

#### Macro: HEADER\-REF

```Lisp
(defmacro ADP:HEADER-REF (TAG)
  ...)
```

````
Add a reference to a header when using the macros text, table or itemize. The argument is a symbol denoting a header-tag.
Only the symbols used with the macros header, subheader and subsubheader are valid.
````

#### Macro: SYMBOL\-REF

```Lisp
(defmacro ADP:SYMBOL-REF (TAG)
  ...)
```

````
Add a reference to a variable when using the macros text, table or itemize. The argument is a symbol denoting a variable
defined with adp:deconstant, adp:define-symbol-macro, adp:defparameter or adp:defvar.
````

#### Macro: FUNCTION\-REF

```Lisp
(defmacro ADP:FUNCTION-REF (TAG)
  ...)
```

````
Add a reference to a function symbol when using the macros text, table or itemize. The argument is a symbol denoting a function
defined with adp:defgeneric, adp:define-modify-macro, adp:defmacro or adp:defun.
````

#### Macro: TYPE\-REF

```Lisp
(defmacro ADP:TYPE-REF (TAG)
  ...)
```

````
Add a reference to a type symbol when using the macros text, table or itemize. The argument is a symbol denoting a type
defined with adp:defclass, adp:define-condition, adp:defstruct or adp:deftype.
````

#### Macro: CODE\-QUOTE

```Lisp
(defmacro ADP:CODE-QUOTE (&BODY BODY)
  ...)
```

````
Form recognized by code-tag that prevents the expressions from being evaluated.
````

#### Macro: CODE\-COMMENT

```Lisp
(defmacro ADP:CODE-COMMENT (COMMENT &BODY BODY)
  ...)
```

````
Form recognized by code-tag that prints a comment before the body expressions when using the tag defined by
code-tag inside a code-block.
````

#### Macro: CODE\-HIDE

```Lisp
(defmacro ADP:CODE-HIDE ((&REST TAGS) &BODY BODY)
  ...)
```

````
Form recognized by code-tag that will hide the code printing '...'. The printing will be done when using the
tag defined by code-tag inside a code-block.
````

#### Macro: CODE\-REMOVE

```Lisp
(defmacro ADP:CODE-REMOVE ((&REST TAGS) &BODY BODY)
  ...)
```

````
Form recognized by code-tag that will remove the code printing. This will be done when using the tag defined
by code-tag inside a code-block.
````

#### Macro: CODE\-TAG

```Lisp
(defmacro ADP:CODE-TAG (TAGS &BODY EXPRS)
  ...)
```

````
Assign several tags to several forms. The forms are placed into a progn form. The argument tags must be a list
of symbols. If no tags are provided, an error is raised. Each symbol in tags will be a code-tag assigned to code.
The same tag can be used several times in different calls to code-tag.  Inside the code-tag form it is correct to use
the next forms: code-hide, code-remove, code-show and code-comment. 
  - code-hide: It has the syntax (code-hide (&rest tags) &rest forms). code-hide receives a list of tags. If a tag 
               used in code-tag also appears in code-hide, the rest of forms will be hidden when using the macro code-block. 
               If the list of tags in code-hide is empty, the forms will be hidden for every tag used in code-tag.
               Hidding the code means printing "..." instead of the forms.
  - code-remove: Same as code-hide, but removes the code instead of printing "..."
  - code-quote: Every expression placed inside code-quote will have its evaluation disabled.
  - code-comment: Receive a string. This string will be printed as a comment (printing ';;').
````

#### Macro: CODE\-BLOCK

```Lisp
(defmacro ADP:CODE-BLOCK ((&REST TAGS) &BODY CODE)
  ...)
```

````
Add a block of code. Each element of code will be prin1-ed but not evaluated. If a symbol is used and that symbol appears as a tag in tags, then 
the code assigned to that tag is printed instead of the symbol.
````

#### Macro: VERBATIM\-CODE\-BLOCK

```Lisp
(defmacro ADP:VERBATIM-CODE-BLOCK (LANG ADP:TEXT)
  ...)
```

````
Add a block of text. It receives two arguments. The first argument must be a string or NIL and it should
denote the programming language that will be used. The second argument must be a string with the text that will
be printed.
````

#### Macro: CODE\-EXAMPLE

```Lisp
(defmacro ADP:CODE-EXAMPLE (&BODY CODE)
  ...)
```

````
Same as code-block, but tags cannot be used and the code is evaluated. The standard output and the last-form's results are also printed.
````

<h2 id="ADP:API-SUBHEADER">API reference functions</h2>

#### Macro: DEFCLASS

```Lisp
(defmacro ADP:DEFCLASS (&BODY BODY)
  ...)
```

````
Add a defclass declaration. The macro expands to cl:defclass. Also, the class name is used to create a type-tag.
````

#### Macro: DEFCONSTANT

```Lisp
(defmacro ADP:DEFCONSTANT (&BODY BODY)
  ...)
```

````
Add a defconstant declaration. The macro expands to cl:defconstant. Also, the constant name is used to create a symbol-tag.
````

#### Macro: DEFGENERIC

```Lisp
(defmacro ADP:DEFGENERIC (&BODY BODY)
  ...)
```

````
Add a defgeneric declaration. The macro expands to cl:defgeneric. Also, the generic function name is used to create a function-tag.
````

#### Macro: DEFINE\-COMPILER\-MACRO

```Lisp
(defmacro ADP:DEFINE-COMPILER-MACRO (&BODY BODY)
  ...)
```

````
Add a define-compiler-macro declaration. The macro expands to cl:define-compiler-macro.
````

#### Macro: DEFINE\-CONDITION

```Lisp
(defmacro ADP:DEFINE-CONDITION (&BODY BODY)
  ...)
```

````
Add a define-condition declaration. The macro expands to cl:define-condition. Also, the condition name is used to create a type-tag.
````

#### Macro: DEFINE\-METHOD\-COMBINATION

```Lisp
(defmacro ADP:DEFINE-METHOD-COMBINATION (&BODY BODY)
  ...)
```

````
Add a define-method-combination declaration. The macro expands to cl:define-method-combination.
````

#### Macro: DEFINE\-MODIFY\-MACRO

```Lisp
(defmacro ADP:DEFINE-MODIFY-MACRO (&BODY BODY)
  ...)
```

````
Add a define-modify-macro declaration. The macro expands to cl:define-modify-macro. Also, the macro name is used to create a function-tag.
````

#### Macro: DEFINE\-SETF\-EXPANDER

```Lisp
(defmacro ADP:DEFINE-SETF-EXPANDER (&BODY BODY)
  ...)
```

````
Add a define-setf-expander declaration. The macro expands to cl:define-setf-expander.
````

#### Macro: DEFINE\-SYMBOL\-MACRO

```Lisp
(defmacro ADP:DEFINE-SYMBOL-MACRO (&BODY BODY)
  ...)
```

````
Add a define-symbol-macro declaration. The macro expands to cl:define-symbol-macro. Also, the symbol name is used to create a symbol-tag.
````

#### Macro: DEFMACRO

```Lisp
(defmacro ADP:DEFMACRO (&BODY BODY)
  ...)
```

````
Add a defmacro declaration. The macro expands to cl:defmacro. Also, the macro name is used to create a function-tag.
````

#### Macro: DEFMETHOD

```Lisp
(defmacro ADP:DEFMETHOD (&BODY BODY)
  ...)
```

````
Add a defmethod declaration. The macro expands to cl:defmethod.
````

#### Macro: DEFPACKAGE

```Lisp
(defmacro ADP:DEFPACKAGE (&BODY BODY)
  ...)
```

````
Add a defpackage declaration. The macro expands to cl:defpackage.
````

#### Macro: DEFPARAMETER

```Lisp
(defmacro ADP:DEFPARAMETER (&BODY BODY)
  ...)
```

````
Add a defparameter declaration. The macro expands to cl:defparameter. Also, the parameter name is used to create a symbol-tag.
````

#### Macro: DEFSETF

```Lisp
(defmacro ADP:DEFSETF (&BODY BODY)
  ...)
```

````
Add a defsetf declaration. The macro expands to cl:defsetf.
````

#### Macro: DEFSTRUCT

```Lisp
(defmacro ADP:DEFSTRUCT (&BODY BODY)
  ...)
```

````
Add a defstruct declaration. The macro expands to cl:defstruct. Also, the struct name is used to create a type-tag.
````

#### Macro: DEFTYPE

```Lisp
(defmacro ADP:DEFTYPE (&BODY BODY)
  ...)
```

````
Add a deftype declaration. The macro expands to cl:deftype. Also, the type name is used to create a type-tag.
````

#### Macro: DEFUN

```Lisp
(defmacro ADP:DEFUN (&BODY BODY)
  ...)
```

````
Add a defun declaration. The macro expands to cl:defun. Also, the function name is used to create a function-tag.
````

#### Macro: DEFVAR

```Lisp
(defmacro ADP:DEFVAR (&BODY BODY)
  ...)
```

````
Add a defvar declaration. The macro expands to cl:defvar. Also, the variable name is used to create a symbol-tag.
````

<h2 id="ADP:HEADERTAG1">Writer functions</h2>

#### Macro: IN\-FILE

```Lisp
(defmacro ADP:IN-FILE (PATH)
  ...)
```

#### Function: LOAD\-STYLE

```Lisp
(defun LOAD-STYLE (STYLE)
  ...)
```

````
Load an adp style.
````

#### Function: LOAD\-SYSTEM

```Lisp
(defun ADP:LOAD-SYSTEM (SYSTEM STYLE &REST STYLE-ARGS)
  ...)
```

````
Load a system with documentation generation activated. The style must be a keyword denoting a valid style.
Each style will create different files. The style-args are style-dependent. In other words, each style can have its own 
arguments to let the user customize briefly how documentation is printed.
````

<h2 id="ADP:ADDITIONAL-FUNCTIONS-SUBHEADER">Additional functions</h2>

#### Macro: EVAL\-WHEN\-ADP

```Lisp
(defmacro EVAL-WHEN-ADP (&BODY BODY)
  ...)
```

````
The body forms will be placed into a progn form only when documentation generation is activated.
Otherwise, this macro expands to NIL.
````

#### Macro: CL\-REF

```Lisp
(defmacro ADP:CL-REF (SYM)
  ...)
```

````
Add a reference to a Common Lisp symbol when using the macros text, cell or item.
````

<h2 id="ADP:MACRO-CHARACTERS-SUBHEADER">Macro characters</h2>

The next table shows which macro characters can be used and what they expand to\:

| Character | Macro | Example |
| --- | --- | --- |
| \@b | [ADP\:BOLD](/docs/user-api.md#macro-bold) | ``` @b("This text is bold") ``` |
| \@i | [ADP\:ITALIC](/docs/user-api.md#macro-italic) | ``` @i("This text is italic") ``` |
| \@e | [ADP\:EMPHASIS](/docs/user-api.md#macro-emphasis) | ``` @e("This text is emphasized") ``` |
| \@c | [ADP\:INLINE\-CODE](/docs/user-api.md#macro-inline-code) | ``` @c("This text is inlined") ``` |
| \@w | [ADP\:WEB\-LINK](/docs/user-api.md#macro-web-link) | ``` @w("Name of link" "www.example.com") ``` |
| \@h | [ADP\:HEADER\-REF](/docs/user-api.md#macro-header-ref) | ``` @h(header) ``` |
| \@f | [ADP\:FUNCTION\-REF](/docs/user-api.md#macro-function-ref) | ``` @f(function) ``` |
| \@s | [ADP\:SYMBOL\-REF](/docs/user-api.md#macro-symbol-ref) | ``` @s(variable) ``` |
| \@t | [ADP\:TYPE\-REF](/docs/user-api.md#macro-type-ref) | ``` @t(type) ``` |
| \@l | [ADP\:CL\-REF](/docs/user-api.md#macro-cl-ref) | ``` @l(princ) ``` |
| \@\' | [ADP\:CODE\-QUOTE](/docs/user-api.md#macro-code-quote) | ``` @'((code (not evaluated))) ``` |
| \@\; | [ADP\:CODE\-COMMENT](/docs/user-api.md#macro-code-comment) | ``` @;("This is a comment" expr) ``` |
| \@\_ | [ADP\:CODE\-REMOVE](/docs/user-api.md#macro-code-remove) | ``` @_((tag1 tag2) (code (to be (removed)))) ``` |
| \@\. | [ADP\:CODE\-HIDE](/docs/user-api.md#macro-code-hide) | ``` @.((tag1 tag2) (code (to be (hidden)))) ``` |


