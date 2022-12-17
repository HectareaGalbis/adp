<h1 id="header:ADP:USER-API-HEADER">ADP User Interface</h1>

<h2 id="header:ADP:HEADERTAG0">Literate programming functions</h2>

<h4 id="function:ADP:HEADER">Macro: HEADER</h4>

```Lisp
(defmacro ADP:HEADER (STR &OPTIONAL TAG)
  ...)
```

````
Add a header with name str. Also, if tag is not nil but an interned symbol, a new header-tag is created.
````

<h4 id="function:ADP:SUBHEADER">Macro: SUBHEADER</h4>

```Lisp
(defmacro ADP:SUBHEADER (STR &OPTIONAL TAG)
  ...)
```

````
Add a subheader with name str. Also, if tag is not nil but an interned symbol, a new header-tag is created.
````

<h4 id="function:ADP:SUBSUBHEADER">Macro: SUBSUBHEADER</h4>

```Lisp
(defmacro ADP:SUBSUBHEADER (STR &OPTIONAL TAG)
  ...)
```

````
Add a subsubheader with name str. Also, if tag is not nil but an interned symbol, a new header-tag is created.
````

<h4 id="function:ADP:TEXT">Macro: TEXT</h4>

```Lisp
(defmacro ADP:TEXT (&REST OBJECTS)
  ...)
```

````
Add plain text. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your text: bold, italic, emphasis, inline-code, web-link, header-ref, symbol-ref, function-ref and type-ref.
````

<h4 id="function:ADP:CELL">Macro: CELL</h4>

```Lisp
(defmacro ADP:CELL (&REST OBJECTS)
  ...)
```

````
Create a cell to place into a table. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your cell text: bold, italic, emphasis, inline-code, web-link, header-ref, symbol-ref, function-ref and type-ref.
````

<h4 id="function:ADP:TABLE">Macro: TABLE</h4>

```Lisp
(defmacro ADP:TABLE (&REST ROWS)
  ...)
```

````
Add a table. Each argument must be a list of cell macro calls.
````

<h4 id="function:ADP:ITEM">Macro: ITEM</h4>

```Lisp
(defmacro ADP:ITEM (&REST ITEMS)
  ...)
```

````
Create an item to be placed into a iterate/enumerate form. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your cell text: bold, italic, emphasis, inline-code, web-link, header-ref, symbol-ref, function-ref and type-ref.
````

<h4 id="function:ADP:ITEMIZE">Macro: ITEMIZE</h4>

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

<h4 id="function:ADP:ENUMERATE">Macro: ENUMERATE</h4>

```Lisp
(defmacro ADP:ENUMERATE (&WHOLE ENUMERATE-FORM &REST ITEMS)
  ...)
```

````
Same as itemize, but a number is printed before each element.
````

<h4 id="function:ADP:TABLE-OF-CONTENTS">Macro: TABLE-OF-CONTENTS</h4>

```Lisp
(defmacro ADP:TABLE-OF-CONTENTS NIL
  ...)
```

````
Add a list of all headers and subheaders used in the system. The headers from different
files are shown in the same order the files are loaded.
````

<h4 id="function:ADP:MINI-TABLE-OF-CONTENTS">Macro: MINI-TABLE-OF-CONTENTS</h4>

```Lisp
(defmacro ADP:MINI-TABLE-OF-CONTENTS NIL
  ...)
```

````
Add a list of all headers, subheaders and subsubheaders used in the current documentation file.
````

<h4 id="function:ADP:TABLE-OF-FUNCTIONS">Macro: TABLE-OF-FUNCTIONS</h4>

```Lisp
(defmacro ADP:TABLE-OF-FUNCTIONS NIL
  ...)
```

````
Add an ordered list of all functions and macros defined using ADP.
````

<h4 id="function:ADP:TABLE-OF-SYMBOLS">Macro: TABLE-OF-SYMBOLS</h4>

```Lisp
(defmacro ADP:TABLE-OF-SYMBOLS NIL
  ...)
```

````
Add an ordered list of all variables defined using ADP.
````

<h4 id="function:ADP:TABLE-OF-TYPES">Macro: TABLE-OF-TYPES</h4>

```Lisp
(defmacro ADP:TABLE-OF-TYPES NIL
  ...)
```

````
Add an ordered list of all types defined using ADP.
````

<h4 id="function:ADP:IMAGE">Macro: IMAGE</h4>

```Lisp
(defmacro ADP:IMAGE (ALT-TEXT PATH)
  ...)
```

````
Add an image with alt-text as the alternative text and path must be the pathname, relative to the system's root directory, 
where the image is located.
````

<h4 id="function:ADP:BOLD">Macro: BOLD</h4>

```Lisp
(defmacro ADP:BOLD (&REST ARGS)
  ...)
```

````
Add bold style to text. Each argument is princ-ed and concatenated into a string.
````

<h4 id="function:ADP:ITALIC">Macro: ITALIC</h4>

```Lisp
(defmacro ADP:ITALIC (&REST ARGS)
  ...)
```

````
Add italic style to text. Each argument is princ-ed and concatenated into a string.
````

<h4 id="function:ADP:EMPHASIS">Macro: EMPHASIS</h4>

```Lisp
(defmacro ADP:EMPHASIS (&REST ARGS)
  ...)
```

````
Add bold and italic style to text. Each argument is princ-ed and concatenated into a string.
````

<h4 id="function:ADP:INLINE-CODE">Macro: INLINE-CODE</h4>

```Lisp
(defmacro ADP:INLINE-CODE (&REST ARGS)
  ...)
```

````
Add inlined style to text. Each argument is princ-ed and concatenated into a string.
````

<h4 id="function:ADP:WEB-LINK">Macro: WEB-LINK</h4>

```Lisp
(defmacro ADP:WEB-LINK (NAME LINK)
  ...)
```

````
Add a hyperlink. The text showed is name and link must be a valid web URL. Both arguments must be strings.
````

<h4 id="function:ADP:HEADER-REF">Macro: HEADER-REF</h4>

```Lisp
(defmacro ADP:HEADER-REF (TAG)
  ...)
```

````
Add a reference to a header when using the macros text, cell or item. The argument is a symbol denoting a header-tag.
Only the symbols used with the macros header, subheader and subsubheader are valid.
````

<h4 id="function:ADP:SYMBOL-REF">Macro: SYMBOL-REF</h4>

```Lisp
(defmacro ADP:SYMBOL-REF (TAG)
  ...)
```

````
Add a reference to a variable when using the macros text, cell or item. The argument is a symbol denoting a variable
defined with adp:defconstant, adp:define-symbol-macro, adp:defparameter or adp:defvar.
````

<h4 id="function:ADP:FUNCTION-REF">Macro: FUNCTION-REF</h4>

```Lisp
(defmacro ADP:FUNCTION-REF (TAG)
  ...)
```

````
Add a reference to a function symbol when using the macros text, cell or item. The argument is a symbol denoting a function
defined with adp:defgeneric, adp:define-modify-macro, adp:defmacro or adp:defun.
````

<h4 id="function:ADP:TYPE-REF">Macro: TYPE-REF</h4>

```Lisp
(defmacro ADP:TYPE-REF (TAG)
  ...)
```

````
Add a reference to a type symbol when using the macros text, cell or item. The argument is a symbol denoting a type
defined with adp:defclass, adp:define-condition, adp:defstruct or adp:deftype.
````

<h4 id="function:ADP:CODE-QUOTE">Macro: CODE-QUOTE</h4>

```Lisp
(defmacro ADP:CODE-QUOTE (&BODY BODY)
  ...)
```

````
Form recognized by code-tag that prevents the expressions from being evaluated.
````

<h4 id="function:ADP:CODE-COMMENT">Macro: CODE-COMMENT</h4>

```Lisp
(defmacro ADP:CODE-COMMENT (COMMENT &BODY BODY)
  ...)
```

````
Form recognized by code-tag that prints a comment before the body expressions when using the tag defined by
code-tag inside a code-block.
````

<h4 id="function:ADP:CODE-HIDE">Macro: CODE-HIDE</h4>

```Lisp
(defmacro ADP:CODE-HIDE ((&REST TAGS) &BODY BODY)
  ...)
```

````
Form recognized by code-tag that will hide the code printing '...'. The printing will be done when using the
tag defined by code-tag inside a code-block.
````

<h4 id="function:ADP:CODE-REMOVE">Macro: CODE-REMOVE</h4>

```Lisp
(defmacro ADP:CODE-REMOVE ((&REST TAGS) &BODY BODY)
  ...)
```

````
Form recognized by code-tag that will remove the code printing. This will be done when using the tag defined
by code-tag inside a code-block.
````

<h4 id="function:ADP:CODE-TAG">Macro: CODE-TAG</h4>

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
  - code-comment: Receive a string and the forms to be commented. This string will be printed as a comment (printing ';;').
````

<h4 id="function:ADP:CODE-BLOCK">Macro: CODE-BLOCK</h4>

```Lisp
(defmacro ADP:CODE-BLOCK ((&REST TAGS) &BODY CODE)
  ...)
```

````
Add a block of code. Each element of code will be prin1-ed but not evaluated. If a symbol is used and that symbol appears as a tag in tags, then 
the code assigned to that tag is printed instead of the symbol.
````

<h4 id="function:ADP:VERBATIM-CODE-BLOCK">Macro: VERBATIM-CODE-BLOCK</h4>

```Lisp
(defmacro ADP:VERBATIM-CODE-BLOCK (LANG ADP:TEXT)
  ...)
```

````
Add a block of text. It receives two arguments. The first argument must be a string or NIL and it should
denote the programming language that will be used. The second argument must be a string with the text that will
be printed.
````

<h4 id="function:ADP:CODE-EXAMPLE">Macro: CODE-EXAMPLE</h4>

```Lisp
(defmacro ADP:CODE-EXAMPLE (&BODY CODE)
  ...)
```

````
Same as code-block, but tags cannot be used and the code is evaluated. The standard output and the last-form's results are also printed.
````

<h2 id="header:ADP:API-SUBHEADER">API reference functions</h2>

<h4 id="function:ADP:DEFCLASS">Macro: DEFCLASS</h4>

```Lisp
(defmacro ADP:DEFCLASS (&BODY BODY)
  ...)
```

````
Add a defclass declaration. The macro expands to cl:defclass. Also, the class name is used to create a type-tag.
````

<h4 id="function:ADP:DEFCONSTANT">Macro: DEFCONSTANT</h4>

```Lisp
(defmacro ADP:DEFCONSTANT (&BODY BODY)
  ...)
```

````
Add a defconstant declaration. The macro expands to cl:defconstant. Also, the constant name is used to create a symbol-tag.
````

<h4 id="function:ADP:DEFGENERIC">Macro: DEFGENERIC</h4>

```Lisp
(defmacro ADP:DEFGENERIC (&BODY BODY)
  ...)
```

````
Add a defgeneric declaration. The macro expands to cl:defgeneric. Also, the generic function name is used to create a function-tag.
````

<h4 id="function:ADP:DEFINE-COMPILER-MACRO">Macro: DEFINE-COMPILER-MACRO</h4>

```Lisp
(defmacro ADP:DEFINE-COMPILER-MACRO (&BODY BODY)
  ...)
```

````
Add a define-compiler-macro declaration. The macro expands to cl:define-compiler-macro.
````

<h4 id="function:ADP:DEFINE-CONDITION">Macro: DEFINE-CONDITION</h4>

```Lisp
(defmacro ADP:DEFINE-CONDITION (&BODY BODY)
  ...)
```

````
Add a define-condition declaration. The macro expands to cl:define-condition. Also, the condition name is used to create a type-tag.
````

<h4 id="function:ADP:DEFINE-METHOD-COMBINATION">Macro: DEFINE-METHOD-COMBINATION</h4>

```Lisp
(defmacro ADP:DEFINE-METHOD-COMBINATION (&BODY BODY)
  ...)
```

````
Add a define-method-combination declaration. The macro expands to cl:define-method-combination.
````

<h4 id="function:ADP:DEFINE-MODIFY-MACRO">Macro: DEFINE-MODIFY-MACRO</h4>

```Lisp
(defmacro ADP:DEFINE-MODIFY-MACRO (&BODY BODY)
  ...)
```

````
Add a define-modify-macro declaration. The macro expands to cl:define-modify-macro. Also, the macro name is used to create a function-tag.
````

<h4 id="function:ADP:DEFINE-SETF-EXPANDER">Macro: DEFINE-SETF-EXPANDER</h4>

```Lisp
(defmacro ADP:DEFINE-SETF-EXPANDER (&BODY BODY)
  ...)
```

````
Add a define-setf-expander declaration. The macro expands to cl:define-setf-expander.
````

<h4 id="function:ADP:DEFINE-SYMBOL-MACRO">Macro: DEFINE-SYMBOL-MACRO</h4>

```Lisp
(defmacro ADP:DEFINE-SYMBOL-MACRO (&BODY BODY)
  ...)
```

````
Add a define-symbol-macro declaration. The macro expands to cl:define-symbol-macro. Also, the symbol name is used to create a symbol-tag.
````

<h4 id="function:ADP:DEFMACRO">Macro: DEFMACRO</h4>

```Lisp
(defmacro ADP:DEFMACRO (&BODY BODY)
  ...)
```

````
Add a defmacro declaration. The macro expands to cl:defmacro. Also, the macro name is used to create a function-tag.
````

<h4 id="function:ADP:DEFMETHOD">Macro: DEFMETHOD</h4>

```Lisp
(defmacro ADP:DEFMETHOD (&BODY BODY)
  ...)
```

````
Add a defmethod declaration. The macro expands to cl:defmethod.
````

<h4 id="function:ADP:DEFPACKAGE">Macro: DEFPACKAGE</h4>

```Lisp
(defmacro ADP:DEFPACKAGE (&BODY BODY)
  ...)
```

````
Add a defpackage declaration. The macro expands to cl:defpackage.
````

<h4 id="function:ADP:DEFPARAMETER">Macro: DEFPARAMETER</h4>

```Lisp
(defmacro ADP:DEFPARAMETER (&BODY BODY)
  ...)
```

````
Add a defparameter declaration. The macro expands to cl:defparameter. Also, the parameter name is used to create a symbol-tag.
````

<h4 id="function:ADP:DEFSETF">Macro: DEFSETF</h4>

```Lisp
(defmacro ADP:DEFSETF (&BODY BODY)
  ...)
```

````
Add a defsetf declaration. The macro expands to cl:defsetf.
````

<h4 id="function:ADP:DEFSTRUCT">Macro: DEFSTRUCT</h4>

```Lisp
(defmacro ADP:DEFSTRUCT (&BODY BODY)
  ...)
```

````
Add a defstruct declaration. The macro expands to cl:defstruct. Also, the struct name is used to create a type-tag.
````

<h4 id="function:ADP:DEFTYPE">Macro: DEFTYPE</h4>

```Lisp
(defmacro ADP:DEFTYPE (&BODY BODY)
  ...)
```

````
Add a deftype declaration. The macro expands to cl:deftype. Also, the type name is used to create a type-tag.
````

<h4 id="function:ADP:DEFUN">Macro: DEFUN</h4>

```Lisp
(defmacro ADP:DEFUN (&BODY BODY)
  ...)
```

````
Add a defun declaration. The macro expands to cl:defun. Also, the function name is used to create a function-tag.
````

<h4 id="function:ADP:DEFVAR">Macro: DEFVAR</h4>

```Lisp
(defmacro ADP:DEFVAR (&BODY BODY)
  ...)
```

````
Add a defvar declaration. The macro expands to cl:defvar. Also, the variable name is used to create a symbol-tag.
````

<h2 id="header:ADP:HEADERTAG1">Writer functions</h2>

<h4 id="function:ADP:IN-FILE">Macro: IN-FILE</h4>

```Lisp
(defmacro ADP:IN-FILE (PATH)
  ...)
```

<h4 id="function:ADP:LOAD-STYLE">Function: LOAD-STYLE</h4>

```Lisp
(defun LOAD-STYLE (STYLE)
  ...)
```

````
Load an adp style.
````

<h4 id="function:ADP:LOAD-SYSTEM">Function: LOAD-SYSTEM</h4>

```Lisp
(defun ADP:LOAD-SYSTEM (SYSTEM STYLE &REST STYLE-ARGS)
  ...)
```

````
Load a system with documentation generation activated. The style must be a keyword denoting a valid style.
Each style will create different files. The style-args are style-dependent. In other words, each style can have its own 
arguments to let the user customize briefly how documentation is printed.
````

<h2 id="header:ADP:ADDITIONAL-FUNCTIONS-SUBHEADER">Additional functions</h2>

<h4 id="function:ADP:EVAL-WHEN-ADP">Macro: EVAL-WHEN-ADP</h4>

```Lisp
(defmacro EVAL-WHEN-ADP (&BODY BODY)
  ...)
```

````
The body forms will be placed into a progn form only when documentation generation is activated.
Otherwise, this macro expands to NIL.
````

<h4 id="function:ADP:CL-REF">Macro: CL-REF</h4>

```Lisp
(defmacro ADP:CL-REF (SYM)
  ...)
```

````
Add a reference to a Common Lisp symbol when using the macros text, cell or item.
````

<h2 id="header:ADP:MACRO-CHARACTERS-SUBHEADER">Macro characters</h2>

The next table shows which macro characters can be used and what they expand to\:

| Character | Macro | Example |
| --- | --- | --- |
| \@b | <a href="/docs/user-api.md#function:ADP:BOLD">ADP:BOLD</a> | ``` @b("This text is bold") ``` |
| \@i | <a href="/docs/user-api.md#function:ADP:ITALIC">ADP:ITALIC</a> | ``` @i("This text is italic") ``` |
| \@e | <a href="/docs/user-api.md#function:ADP:EMPHASIS">ADP:EMPHASIS</a> | ``` @e("This text is emphasized") ``` |
| \@c | <a href="/docs/user-api.md#function:ADP:INLINE-CODE">ADP:INLINE-CODE</a> | ``` @c("This text is inlined") ``` |
| \@w | <a href="/docs/user-api.md#function:ADP:WEB-LINK">ADP:WEB-LINK</a> | ``` @w("Name of link" "www.example.com") ``` |
| \@h | <a href="/docs/user-api.md#function:ADP:HEADER-REF">ADP:HEADER-REF</a> | ``` @h(header) ``` |
| \@f | <a href="/docs/user-api.md#function:ADP:FUNCTION-REF">ADP:FUNCTION-REF</a> | ``` @f(function) ``` |
| \@s | <a href="/docs/user-api.md#function:ADP:SYMBOL-REF">ADP:SYMBOL-REF</a> | ``` @s(variable) ``` |
| \@t | <a href="/docs/user-api.md#function:ADP:TYPE-REF">ADP:TYPE-REF</a> | ``` @t(type) ``` |
| \@l | <a href="/docs/user-api.md#function:ADP:CL-REF">ADP:CL-REF</a> | ``` @l(princ) ``` |
| \@\' | <a href="/docs/user-api.md#function:ADP:CODE-QUOTE">ADP:CODE-QUOTE</a> | ``` @'((code (not evaluated))) ``` |
| \@\; | <a href="/docs/user-api.md#function:ADP:CODE-COMMENT">ADP:CODE-COMMENT</a> | ``` @;("This is a comment" expr) ``` |
| \@\_ | <a href="/docs/user-api.md#function:ADP:CODE-REMOVE">ADP:CODE-REMOVE</a> | ``` @_((tag1 tag2) (code (to be (removed)))) ``` |
| \@\. | <a href="/docs/user-api.md#function:ADP:CODE-HIDE">ADP:CODE-HIDE</a> | ``` @.((tag1 tag2) (code (to be (hidden)))) ``` |


