# ADP User Interface

## Literate programming functions

#### Macro: HEADER

```Lisp
(defmacro ADP:HEADER (STR &OPTIONAL TAG)
  ...)
```

Add a header with name str. Also, if tag is not nil but a symbol, a new header-tag is created.

#### Macro: SUBHEADER

```Lisp
(defmacro ADP:SUBHEADER (STR &OPTIONAL TAG)
  ...)
```

Same as header, but add a subheader.

#### Macro: SUBSUBHEADER

```Lisp
(defmacro ADP:SUBSUBHEADER (STR &OPTIONAL TAG)
  ...)
```

Same as header, but add a subsubheader.

#### Macro: TEXT

```Lisp
(defmacro ADP:TEXT (&REST OBJECTS)
  ...)
```

Add plain text. The arguments in objects can be any lisp object. They will be princ-ed and concatenated into a single string.
You can use the following macros to enrich your text: bold, italic, bold-italic, code-inline, web-link, header-ref, symbol-ref, 
function-ref, type-ref and file-ref.

#### Macro: TABLE

```Lisp
(defmacro ADP:TABLE (&REST ROWS)
  ...)
```

Add a table. Each argument must be a list of lists. Each inner list must have as first element the keyword :cell and the rest 
are treated as if using the macro text.

#### Macro: ITEMIZE

```Lisp
(defmacro ADP:ITEMIZE (&REST ITEMS)
  ...)
```

Add a list of items. Each argument must be a list. Each list must have as first argument the keyword :item or :itemize. If 
:item is used, the rest of the elements in that list will be treated as if using the macro text. If :itemize is used the rest 
of elements must be lists where its first elements are the keywords :item or :itemize. In other words, when :itemize is used 
a nested list is added.

#### Macro: IMAGE

```Lisp
(defmacro ADP:IMAGE (ALT-TEXT PATH)
  ...)
```

Add an image with alt-text as the alternative text and path must be the pathname, relative to the system's root directory, 
where the image is located.

#### Macro: BOLD

```Lisp
(defmacro ADP:BOLD (&REST ARGS)
  ...)
```

Add bold style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string.

#### Macro: ITALIC

```Lisp
(defmacro ADP:ITALIC (&REST ARGS)
  ...)
```

Add italic style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string.

#### Macro: BOLD-ITALIC

```Lisp
(defmacro ADP:BOLD-ITALIC (&REST ARGS)
  ...)
```

Add bold and italic style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string.

#### Macro: CODE-INLINE

```Lisp
(defmacro ADP:CODE-INLINE (&REST CODE)
  ...)
```

Add inlined style to text when using the macros text, table or itemize. Each argument is princ-ed and concatenated into a string.

#### Macro: WEB-LINK

```Lisp
(defmacro ADP:WEB-LINK (NAME LINK)
  ...)
```

Add a hyperlink. The text showed is name and link must be a valid web URL. Both arguments must be strings.

#### Macro: FILE-REF

```Lisp
(defmacro ADP:FILE-REF (PATH)
  ...)
```

Add a reference to a documentation file when using the macros text, table or itemize. The argument is the pathname, relative 
to the system's root directory, to the referenced file. Only pathnames used with write-in-file are valid. 

#### Macro: HEADER-REF

```Lisp
(defmacro ADP:HEADER-REF (TAG)
  ...)
```

Add a reference to a header when using the macros text, table or itemize. The argument is a symbol denoting a header-tag.
Only the symbols used with the macros header, subheader and subsubheader are valid.

#### Macro: SYMBOL-REF

```Lisp
(defmacro ADP:SYMBOL-REF (TAG)
  ...)
```

Add a reference to a variable when using the macros text, table or itemize. The argument is a symbol denoting a variable
defined with adp:deconstant, adp:define-symbol-macro, adp:defparameter or adp:defvar.

#### Macro: FUNCTION-REF

```Lisp
(defmacro ADP:FUNCTION-REF (TAG)
  ...)
```

Add a reference to a function symbol when using the macros text, table or itemize. The argument is a symbol denoting a function
defined with adp:defgeneric, adp:define-modify-macro, adp:defmacro or adp:defun.

#### Macro: TYPE-REF

```Lisp
(defmacro ADP:TYPE-REF (TAG)
  ...)
```

Add a reference to a type symbol when using the macros text, table or itemize. The argument is a symbol denoting a type
defined with adp:defclass, adp:define-condition, adp:defstruct or adp:deftype.

#### Macro: CODE-TAG

```Lisp
(defmacro ADP:CODE-TAG (TAGS &BODY CODE)
  ...)
```

Assign several tags to a piece of code. The code is placed into a progn form. The argument tags must be a list
of symbols. If no tags are provided, then code-tag will do nothing. Each symbol in tags will be a code-tag assigned to code. 
Inside the code-tag form it is correct to use a (code-hide tags &rest forms) form. It only indicates to code-tag 
which parts of code can be hidden when using the tag in the macro code-block. code-hide accepts also a list of tags. If a tag 
used in code-tag also appears in code-hide, that piece of code will be hidden when using the macro code-block. If the list of 
tags in code-hide is empty, the that piece of code will be hidden for every tag used in code-tag.

#### Macro: CODE-BLOCK

```Lisp
(defmacro ADP:CODE-BLOCK ((&REST TAGS) &BODY CODE)
  ...)
```

Add a block of code. Each element of code will be prin1-ed but not evaluated. If a symbol is used and that symbol appears as a tag in tags, then 
the code assigned to that tag is prin1-ed instead of the symbol.

#### Macro: CODE-EXAMPLE

```Lisp
(defmacro ADP:CODE-EXAMPLE (&BODY CODE)
  ...)
```

Same as code-block, but tags cannot be used and the code is evaluated. The standard output and the last-form's results are also printed.

## API documentation functions

#### Macro: DEFCLASS

```Lisp
(defmacro ADP:DEFCLASS (&BODY DEFCLASS-BODY)
  ...)
```

Add a defclass declaration. The macro expands to cl:defclass. Also, the class name is used to create a type-tag.

#### Macro: DEFCONSTANT

```Lisp
(defmacro ADP:DEFCONSTANT (&BODY DEFCONSTANT-BODY)
  ...)
```

Add a defconstant declaration. The macro expands to cl:defconstant. Also, the constant name is used to create a symbol-tag.

#### Macro: DEFGENERIC

```Lisp
(defmacro ADP:DEFGENERIC (&BODY DEFGENERIC-BODY)
  ...)
```

Add a defgeneric declaration. The macro expands to cl:defgeneric. Also, the generic function name is used to create a function-tag.

#### Macro: DEFINE-COMPILER-MACRO

```Lisp
(defmacro ADP:DEFINE-COMPILER-MACRO (&BODY DEFINE-COMPILER-MACRO-BODY)
  ...)
```

Add a define-compiler-macro declaration. The macro expands to cl:define-compiler-macro.

#### Macro: DEFINE-CONDITION

```Lisp
(defmacro ADP:DEFINE-CONDITION (&BODY DEFINE-CONDITION-BODY)
  ...)
```

Add a define-condition declaration. The macro expands to cl:define-condition. Also, the condition name is used to create a type-tag.

#### Macro: DEFINE-METHOD-COMBINATION

```Lisp
(defmacro ADP:DEFINE-METHOD-COMBINATION (&BODY DEFINE-METHOD-COMBINATION-BODY)
  ...)
```

Add a define-method-combination declaration. The macro expands to cl:define-method-combination.

#### Macro: DEFINE-MODIFY-MACRO

```Lisp
(defmacro ADP:DEFINE-MODIFY-MACRO (&BODY DEFINE-MODIFY-MACRO-BODY)
  ...)
```

Add a define-modify-macro declaration. The macro expands to cl:define-modify-macro. Also, the macro name is used to create a function-tag.

#### Macro: DEFINE-SETF-EXPANDER

```Lisp
(defmacro ADP:DEFINE-SETF-EXPANDER (&BODY DEFINE-SETF-EXPANDER-BODY)
  ...)
```

Add a define-setf-expander declaration. The macro expands to cl:define-setf-expander.

#### Macro: DEFINE-SYMBOL-MACRO

```Lisp
(defmacro ADP:DEFINE-SYMBOL-MACRO (&BODY DEFINE-SYMBOL-MACRO-BODY)
  ...)
```

Add a define-symbol-macro declaration. The macro expands to cl:define-symbol-macro. Also, the symbol name is used to create a symbol-tag.

#### Macro: DEFMACRO

```Lisp
(defmacro ADP:DEFMACRO (&BODY DEFMACRO-BODY)
  ...)
```

Add a defmacro declaration. The macro expands to cl:defmacro. Also, the macro name is used to create a function-tag.

#### Macro: DEFMETHOD

```Lisp
(defmacro ADP:DEFMETHOD (&BODY DEFMETHOD-BODY)
  ...)
```

Add a defmethod declaration. The macro expands to cl:defmethod.

#### Macro: DEFPACKAGE

```Lisp
(defmacro ADP:DEFPACKAGE (&BODY DEFPACKAGE-BODY)
  ...)
```

Add a defpackage declaration. The macro expands to cl:defpackage.

#### Macro: DEFPARAMETER

```Lisp
(defmacro ADP:DEFPARAMETER (&BODY DEFPARAMETER-BODY)
  ...)
```

Add a defparameter declaration. The macro expands to cl:defparameter. Also, the parameter name is used to create a symbol-tag.

#### Macro: DEFSETF

```Lisp
(defmacro ADP:DEFSETF (&BODY DEFSETF-BODY)
  ...)
```

Add a defsetf declaration. The macro expands to cl:defsetf.

#### Macro: DEFSTRUCT

```Lisp
(defmacro ADP:DEFSTRUCT (&BODY DEFSTRUCT-BODY)
  ...)
```

Add a defstruct declaration. The macro expands to cl:defstruct. Also, the struct name is used to create a type-tag.

#### Macro: DEFTYPE

```Lisp
(defmacro ADP:DEFTYPE (&BODY DEFTYPE-BODY)
  ...)
```

Add a deftype declaration. The macro expands to cl:deftype. Also, the type name is used to create a type-tag.

#### Macro: DEFUN

```Lisp
(defmacro ADP:DEFUN (&BODY DEFUN-BODY)
  ...)
```

Add a defun declaration. The macro expands to cl:defun. Also, the function name is used to create a function-tag.

#### Macro: DEFVAR

```Lisp
(defmacro ADP:DEFVAR (&BODY DEFVAR-BODY)
  ...)
```

Add a defvar declaration. The macro expands to cl:defvar. Also, the variable name is used to create a symbol-tag.

## Documentation writer function

#### Macro: WRITE-IN-FILE

```Lisp
(defmacro ADP:WRITE-IN-FILE (FILE-PATH)
  ...)
```

Associate all the information gathered so far with the pathname file-path. This will cause the creation of a file 
where all the information will be printed in. The pathname will be considered relative to the system's root directory.
Only the directory and name parts of file-path are considered. The rest are ignored (including the extension). This 
macro can be used multiple times.

#### Function: LOAD-DOCUMENTATION-SYSTEM

```Lisp
(defun ADP:LOAD-DOCUMENTATION-SYSTEM (SYSTEM STYLE &REST STYLE-ARGS)
  ...)
```

Load a system with documentation generation activated. The style must be a keyword denoting a valid style.
Each style will create different files. The style-args are style-dependent. In other words, each style can have its own 
arguments to let the user customize briefly how documentation is printed.

## Additional functions

#### Macro: CL-REF

```Lisp
(defmacro ADP:CL-REF (SYM)
  ...)
```

Add a reference to a Common Lisp symbol when using the macros text, table or itemize.

## Macro characters

The next table shows what macro characters can be used and what they expand to:

| Character | Macro | Example |
| --- | --- | --- |
| b | [ADP:BOLD](/docs/user-api.md#macro-bold) | `@b("This text is bold")` |
| i | [ADP:ITALIC](/docs/user-api.md#macro-italic) | `@i("This text is italic")` |
| e | [ADP:ITALIC](/docs/user-api.md#macro-italic) | `@e("This text is emphasized")` |
| c | [ADP:CODE-INLINE](/docs/user-api.md#macro-code-inline) | `@c("This text is inlined")` |
| w | [ADP:HEADER-REF](/docs/user-api.md#macro-header-ref) | `@w("Name of link" "www.example.com")` |
| h | [ADP:HEADER-REF](/docs/user-api.md#macro-header-ref) | `@h(header)` |
| f | [ADP:FUNCTION-REF](/docs/user-api.md#macro-function-ref) | `@f(function)` |
| s | [ADP:SYMBOL-REF](/docs/user-api.md#macro-symbol-ref) | `@s(variable)` |
| t | [ADP:TYPE-REF](/docs/user-api.md#macro-type-ref) | `@t(type)` |
| p | [ADP:FILE-REF](/docs/user-api.md#macro-file-ref) | `@p(#P"path/to/file")` |
| l | [ADP:CL-REF](/docs/user-api.md#macro-cl-ref) | `@l(princ)` |


