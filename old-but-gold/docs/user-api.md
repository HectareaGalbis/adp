<h1 id="header:ADP:USER-API-HEADER">ADP User Interface</h1>

<h2 id="header:ADP:HEADERTAG0">Literate programming functions</h2>

<h4 id="function:ADP:HEADER">Macro: header</h4>

```Lisp
(defmacro adp:header (str &optional tag)
  ...)
```

````
Add a header with name str. Also, if tag is not nil but an interned symbol, a new header-tag is created.
````

<h4 id="function:ADP:SUBHEADER">Macro: subheader</h4>

```Lisp
(defmacro adp:subheader (str &optional tag)
  ...)
```

````
Add a subheader with name str. Also, if tag is not nil but an interned symbol, a new header-tag is created.
````

<h4 id="function:ADP:SUBSUBHEADER">Macro: subsubheader</h4>

```Lisp
(defmacro adp:subsubheader (str &optional tag)
  ...)
```

````
Add a subsubheader with name str. Also, if tag is not nil but an interned symbol, a new header-tag is created.
````

<h4 id="function:ADP:TEXT">Macro: text</h4>

```Lisp
(defmacro adp:text (&rest objects)
  ...)
```

````
Add plain text. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your text: bold, italic, emphasis, inline-code, web-link, header-ref, symbol-ref, function-ref and type-ref.
````

<h4 id="function:ADP:CELL">Macro: cell</h4>

```Lisp
(defmacro adp:cell (&rest objects)
  ...)
```

````
Create a cell to place into a table. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your cell text: bold, italic, emphasis, inline-code, web-link, header-ref, symbol-ref, function-ref and type-ref.
````

<h4 id="function:ADP:TABLE">Macro: table</h4>

```Lisp
(defmacro adp:table (&rest rows)
  ...)
```

````
Add a table. Each argument must be a list of cell macro calls.
````

<h4 id="function:ADP:ITEM">Macro: item</h4>

```Lisp
(defmacro adp:item (&rest items)
  ...)
```

````
Create an item to be placed into a iterate/enumerate form. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your cell text: bold, italic, emphasis, inline-code, web-link, header-ref, symbol-ref, function-ref and type-ref.
````

<h4 id="function:ADP:ITEMIZE">Macro: itemize</h4>

```Lisp
(defmacro adp:itemize (&whole itemize-form &rest items)
  ...)
```

````
Add a list of items. Each argument must be a list. Each list must start with the symbol item, itemize or enumerate. If 
item is used, the rest of the elements in that list will be treated as if using the macro text. If itemize or enumerate is used the rest 
of elements must be lists that must start with item, itemize or enumerate. In other words, when itemize or enumerate is used 
a nested list is added. A certain symbol will be printed before each element of the list.
````

<h4 id="function:ADP:ENUMERATE">Macro: enumerate</h4>

```Lisp
(defmacro adp:enumerate (&whole enumerate-form &rest items)
  ...)
```

````
Same as itemize, but a number is printed before each element.
````

<h4 id="function:ADP:TABLE-OF-CONTENTS">Macro: table-of-contents</h4>

```Lisp
(defmacro adp:table-of-contents nil
  ...)
```

````
Add a list of all headers and subheaders used in the system. The headers from different
files are shown in the same order the files are loaded.
````

<h4 id="function:ADP:MINI-TABLE-OF-CONTENTS">Macro: mini-table-of-contents</h4>

```Lisp
(defmacro adp:mini-table-of-contents nil
  ...)
```

````
Add a list of all headers, subheaders and subsubheaders used in the current documentation file.
````

<h4 id="function:ADP:TABLE-OF-FUNCTIONS">Macro: table-of-functions</h4>

```Lisp
(defmacro adp:table-of-functions nil
  ...)
```

````
Add an ordered list of all functions and macros defined using ADP.
````

<h4 id="function:ADP:TABLE-OF-SYMBOLS">Macro: table-of-symbols</h4>

```Lisp
(defmacro adp:table-of-symbols nil
  ...)
```

````
Add an ordered list of all variables defined using ADP.
````

<h4 id="function:ADP:TABLE-OF-TYPES">Macro: table-of-types</h4>

```Lisp
(defmacro adp:table-of-types nil
  ...)
```

````
Add an ordered list of all types defined using ADP.
````

<h4 id="function:ADP:IMAGE">Macro: image</h4>

```Lisp
(defmacro adp:image (alt-text path)
  ...)
```

````
Add an image with alt-text as the alternative text and path must be the pathname, relative to the system's root directory, 
where the image is located.
````

<h4 id="function:ADP:BOLD">Macro: bold</h4>

```Lisp
(defmacro adp:bold (&rest args)
  ...)
```

````
Add bold style to text. Each argument is princ-ed and concatenated into a string.
````

<h4 id="function:ADP:ITALIC">Macro: italic</h4>

```Lisp
(defmacro adp:italic (&rest args)
  ...)
```

````
Add italic style to text. Each argument is princ-ed and concatenated into a string.
````

<h4 id="function:ADP:EMPHASIS">Macro: emphasis</h4>

```Lisp
(defmacro adp:emphasis (&rest args)
  ...)
```

````
Add bold and italic style to text. Each argument is princ-ed and concatenated into a string.
````

<h4 id="function:ADP:INLINE-CODE">Macro: inline-code</h4>

```Lisp
(defmacro adp:inline-code (&rest args)
  ...)
```

````
Add inlined style to text. Each argument is princ-ed and concatenated into a string.
````

<h4 id="function:ADP:WEB-LINK">Macro: web-link</h4>

```Lisp
(defmacro adp:web-link (name link)
  ...)
```

````
Add a hyperlink. The text showed is name and link must be a valid web URL. Both arguments must be strings.
````

<h4 id="function:ADP:HEADER-REF">Macro: header-ref</h4>

```Lisp
(defmacro adp:header-ref (tag)
  ...)
```

````
Add a reference to a header when using the macros text, cell or item. The argument is a symbol denoting a header-tag.
Only the symbols used with the macros header, subheader and subsubheader are valid.
````

<h4 id="function:ADP:SYMBOL-REF">Macro: symbol-ref</h4>

```Lisp
(defmacro adp:symbol-ref (tag)
  ...)
```

````
Add a reference to a variable when using the macros text, cell or item. The argument is a symbol denoting a variable
defined with adp:defconstant, adp:define-symbol-macro, adp:defparameter or adp:defvar.
````

<h4 id="function:ADP:FUNCTION-REF">Macro: function-ref</h4>

```Lisp
(defmacro adp:function-ref (tag)
  ...)
```

````
Add a reference to a function symbol when using the macros text, cell or item. The argument is a symbol denoting a function
defined with adp:defgeneric, adp:define-modify-macro, adp:defmacro or adp:defun.
````

<h4 id="function:ADP:TYPE-REF">Macro: type-ref</h4>

```Lisp
(defmacro adp:type-ref (tag)
  ...)
```

````
Add a reference to a type symbol when using the macros text, cell or item. The argument is a symbol denoting a type
defined with adp:defclass, adp:define-condition, adp:defstruct or adp:deftype.
````

<h4 id="function:ADP:CODE-QUOTE">Macro: code-quote</h4>

```Lisp
(defmacro adp:code-quote (&body body)
  ...)
```

````
Form recognized by code-tag that prevents the expressions from being evaluated.
````

<h4 id="function:ADP:CODE-COMMENT">Macro: code-comment</h4>

```Lisp
(defmacro adp:code-comment (comment &body body)
  ...)
```

````
Form recognized by code-tag that prints a comment before the body expressions when using the tag defined by
code-tag inside a code-block.
````

<h4 id="function:ADP:CODE-HIDE">Macro: code-hide</h4>

```Lisp
(defmacro adp:code-hide ((&rest tags) &body body)
  ...)
```

````
Form recognized by code-tag that will hide the code printing '...'. The printing will be done when using the
tag defined by code-tag inside a code-block.
````

<h4 id="function:ADP:CODE-REMOVE">Macro: code-remove</h4>

```Lisp
(defmacro adp:code-remove ((&rest tags) &body body)
  ...)
```

````
Form recognized by code-tag that will remove the code printing. This will be done when using the tag defined
by code-tag inside a code-block.
````

<h4 id="function:ADP:CODE-TAG">Macro: code-tag</h4>

```Lisp
(defmacro adp:code-tag (tags &body exprs)
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

<h4 id="function:ADP:CODE-BLOCK">Macro: code-block</h4>

```Lisp
(defmacro adp:code-block ((&rest tags) &body code)
  ...)
```

````
Add a block of code. Each element of code will be prin1-ed but not evaluated. If a symbol is used and that symbol appears as a tag in tags, then 
the code assigned to that tag is printed instead of the symbol.
````

<h4 id="function:ADP:VERBATIM-CODE-BLOCK">Macro: verbatim-code-block</h4>

```Lisp
(defmacro adp:verbatim-code-block (lang adp:text)
  ...)
```

````
Add a block of text. It receives two arguments. The first argument must be a string or NIL and it should
denote the programming language that will be used. The second argument must be a string with the text that will
be printed.
````

<h4 id="function:ADP:CODE-EXAMPLE">Macro: code-example</h4>

```Lisp
(defmacro adp:code-example (&body code)
  ...)
```

````
Same as code-block, but tags cannot be used and the code is evaluated. The standard output and the last-form's results are also printed.
````

<h2 id="header:ADP:API-SUBHEADER">API reference functions</h2>

<h4 id="function:ADP:DEFCLASS">Macro: defclass</h4>

```Lisp
(defmacro adp:defclass (&body body)
  ...)
```

````
Add a defclass declaration. The macro expands to cl:defclass. Also, the class name is used to create a type-tag.
````

<h4 id="function:ADP:DEFCONSTANT">Macro: defconstant</h4>

```Lisp
(defmacro adp:defconstant (&body body)
  ...)
```

````
Add a defconstant declaration. The macro expands to cl:defconstant. Also, the constant name is used to create a symbol-tag.
````

<h4 id="function:ADP:DEFGENERIC">Macro: defgeneric</h4>

```Lisp
(defmacro adp:defgeneric (&body body)
  ...)
```

````
Add a defgeneric declaration. The macro expands to cl:defgeneric. Also, the generic function name is used to create a function-tag.
````

<h4 id="function:ADP:DEFINE-COMPILER-MACRO">Macro: define-compiler-macro</h4>

```Lisp
(defmacro adp:define-compiler-macro (&body body)
  ...)
```

````
Add a define-compiler-macro declaration. The macro expands to cl:define-compiler-macro.
````

<h4 id="function:ADP:DEFINE-CONDITION">Macro: define-condition</h4>

```Lisp
(defmacro adp:define-condition (&body body)
  ...)
```

````
Add a define-condition declaration. The macro expands to cl:define-condition. Also, the condition name is used to create a type-tag.
````

<h4 id="function:ADP:DEFINE-METHOD-COMBINATION">Macro: define-method-combination</h4>

```Lisp
(defmacro adp:define-method-combination (&body body)
  ...)
```

````
Add a define-method-combination declaration. The macro expands to cl:define-method-combination.
````

<h4 id="function:ADP:DEFINE-MODIFY-MACRO">Macro: define-modify-macro</h4>

```Lisp
(defmacro adp:define-modify-macro (&body body)
  ...)
```

````
Add a define-modify-macro declaration. The macro expands to cl:define-modify-macro. Also, the macro name is used to create a function-tag.
````

<h4 id="function:ADP:DEFINE-SETF-EXPANDER">Macro: define-setf-expander</h4>

```Lisp
(defmacro adp:define-setf-expander (&body body)
  ...)
```

````
Add a define-setf-expander declaration. The macro expands to cl:define-setf-expander.
````

<h4 id="function:ADP:DEFINE-SYMBOL-MACRO">Macro: define-symbol-macro</h4>

```Lisp
(defmacro adp:define-symbol-macro (&body body)
  ...)
```

````
Add a define-symbol-macro declaration. The macro expands to cl:define-symbol-macro. Also, the symbol name is used to create a symbol-tag.
````

<h4 id="function:ADP:DEFMACRO">Macro: defmacro</h4>

```Lisp
(defmacro adp:defmacro (&body body)
  ...)
```

````
Add a defmacro declaration. The macro expands to cl:defmacro. Also, the macro name is used to create a function-tag.
````

<h4 id="function:ADP:DEFMETHOD">Macro: defmethod</h4>

```Lisp
(defmacro adp:defmethod (&body body)
  ...)
```

````
Add a defmethod declaration. The macro expands to cl:defmethod.
````

<h4 id="function:ADP:DEFPACKAGE">Macro: defpackage</h4>

```Lisp
(defmacro adp:defpackage (&body body)
  ...)
```

````
Add a defpackage declaration. The macro expands to cl:defpackage.
````

<h4 id="function:ADP:DEFPARAMETER">Macro: defparameter</h4>

```Lisp
(defmacro adp:defparameter (&body body)
  ...)
```

````
Add a defparameter declaration. The macro expands to cl:defparameter. Also, the parameter name is used to create a symbol-tag.
````

<h4 id="function:ADP:DEFSETF">Macro: defsetf</h4>

```Lisp
(defmacro adp:defsetf (&body body)
  ...)
```

````
Add a defsetf declaration. The macro expands to cl:defsetf.
````

<h4 id="function:ADP:DEFSTRUCT">Macro: defstruct</h4>

```Lisp
(defmacro adp:defstruct (&body body)
  ...)
```

````
Add a defstruct declaration. The macro expands to cl:defstruct. Also, the struct name is used to create a type-tag.
````

<h4 id="function:ADP:DEFTYPE">Macro: deftype</h4>

```Lisp
(defmacro adp:deftype (&body body)
  ...)
```

````
Add a deftype declaration. The macro expands to cl:deftype. Also, the type name is used to create a type-tag.
````

<h4 id="function:ADP:DEFUN">Macro: defun</h4>

```Lisp
(defmacro adp:defun (&body body)
  ...)
```

````
Add a defun declaration. The macro expands to cl:defun. Also, the function name is used to create a function-tag.
````

<h4 id="function:ADP:DEFVAR">Macro: defvar</h4>

```Lisp
(defmacro adp:defvar (&body body)
  ...)
```

````
Add a defvar declaration. The macro expands to cl:defvar. Also, the variable name is used to create a symbol-tag.
````

<h2 id="header:ADP:HEADERTAG1">Writer functions</h2>

<h4 id="function:ADP:IN-FILE">Macro: in-file</h4>

```Lisp
(defmacro adp:in-file (path)
  ...)
```

<h4 id="function:ADP:LOAD-STYLE">Function: load-style</h4>

```Lisp
(defun load-style (style)
  ...)
```

````
Load an adp style.
````

<h4 id="function:ADP:LOAD-SYSTEM">Function: load-system</h4>

```Lisp
(defun adp:load-system (system style &rest style-args)
  ...)
```

````
Load a system with documentation generation activated. The style must be a keyword denoting a valid style.
Each style will create different files. The style-args are style-dependent. In other words, each style can have its own 
arguments to let the user customize briefly how documentation is printed.
````

<h2 id="header:ADP:ADDITIONAL-FUNCTIONS-SUBHEADER">Additional functions</h2>

<h4 id="function:ADP:EVAL-WHEN-ADP">Macro: eval-when-adp</h4>

```Lisp
(defmacro eval-when-adp (&body body)
  ...)
```

````
The body forms will be placed into a progn form only when documentation generation is activated.
Otherwise, this macro expands to NIL.
````

<h4 id="function:ADP:CL-REF">Macro: cl-ref</h4>

```Lisp
(defmacro adp:cl-ref (sym)
  ...)
```

````
Add a reference to a Common Lisp symbol when using the macros text, cell or item.
````

<h2 id="header:ADP:MACRO-CHARACTERS-SUBHEADER">Macro characters</h2>

The next table shows which macro characters can be used and what they expand to\:

| Character | Macro | Example |
| --- | --- | --- |
| \@b | <a href="/docs/user-api.md#function:ADP:BOLD">adp:bold</a> | ``` @b("This text is bold") ``` |
| \@i | <a href="/docs/user-api.md#function:ADP:ITALIC">adp:italic</a> | ``` @i("This text is italic") ``` |
| \@e | <a href="/docs/user-api.md#function:ADP:EMPHASIS">adp:emphasis</a> | ``` @e("This text is emphasized") ``` |
| \@c | <a href="/docs/user-api.md#function:ADP:INLINE-CODE">adp:inline-code</a> | ``` @c("This text is inlined") ``` |
| \@w | <a href="/docs/user-api.md#function:ADP:WEB-LINK">adp:web-link</a> | ``` @w("Name of link" "www.example.com") ``` |
| \@h | <a href="/docs/user-api.md#function:ADP:HEADER-REF">adp:header-ref</a> | ``` @h(header) ``` |
| \@f | <a href="/docs/user-api.md#function:ADP:FUNCTION-REF">adp:function-ref</a> | ``` @f(function) ``` |
| \@s | <a href="/docs/user-api.md#function:ADP:SYMBOL-REF">adp:symbol-ref</a> | ``` @s(variable) ``` |
| \@t | <a href="/docs/user-api.md#function:ADP:TYPE-REF">adp:type-ref</a> | ``` @t(type) ``` |
| \@l | <a href="/docs/user-api.md#function:ADP:CL-REF">adp:cl-ref</a> | ``` @l(princ) ``` |
| \@\' | <a href="/docs/user-api.md#function:ADP:CODE-QUOTE">adp:code-quote</a> | ``` @'((code (not evaluated))) ``` |
| \@\; | <a href="/docs/user-api.md#function:ADP:CODE-COMMENT">adp:code-comment</a> | ``` @;("This is a comment" expr) ``` |
| \@\_ | <a href="/docs/user-api.md#function:ADP:CODE-REMOVE">adp:code-remove</a> | ``` @_((tag1 tag2) (code (to be (removed)))) ``` |
| \@\. | <a href="/docs/user-api.md#function:ADP:CODE-HIDE">adp:code-hide</a> | ``` @.((tag1 tag2) (code (to be (hidden)))) ``` |


