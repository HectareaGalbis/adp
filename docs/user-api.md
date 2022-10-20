# ADP User Interface

## Literate programming functions

Macro: HEADER

```Lisp
(defmacro ADP:HEADER (ADP::STR &OPTIONAL ADP::LABEL))
```

Macro: SUBHEADER

```Lisp
(defmacro ADP:SUBHEADER (ADP::STR &OPTIONAL ADP::LABEL))
```

Macro: SUBSUBHEADER

```Lisp
(defmacro ADP:SUBSUBHEADER (ADP::STR &OPTIONAL ADP::LABEL))
```

Macro: TEXT

```Lisp
(defmacro ADP:TEXT (&REST ADP::OBJECTS))
```

Macro: TABLE

```Lisp
(defmacro ADP:TABLE (&REST ADP::ROWS))
```

Macro: ITEMIZE

```Lisp
(defmacro ADP:ITEMIZE (&REST ADP::ITEMS))
```

Macro: IMAGE

```Lisp
(defmacro ADP:IMAGE (ADP::ALT-TEXT ADP::PATH))
```

Macro: BOLD

```Lisp
(defmacro ADP:BOLD (&REST ADP::ARGS))
```

Macro: ITALIC

```Lisp
(defmacro ADP:ITALIC (&REST ADP::ARGS))
```

Macro: BOLD-ITALIC

```Lisp
(defmacro ADP:BOLD-ITALIC (&REST ADP::ARGS))
```

Macro: CODE-INLINE

```Lisp
(defmacro ADP:CODE-INLINE (&REST ADP::CODE))
```

Macro: WEB-LINK

```Lisp
(defmacro ADP:WEB-LINK (ADP::NAME ADP::LINK))
```

Macro: FILE-REF

```Lisp
(defmacro ADP:FILE-REF (ADP::PATH))
```

Macro: HEADER-REF

```Lisp
(defmacro ADP:HEADER-REF (ADP::LABEL))
```

Macro: SYMBOL-REF

```Lisp
(defmacro ADP:SYMBOL-REF (ADP::LABEL))
```

Macro: FUNCTION-REF

```Lisp
(defmacro ADP:FUNCTION-REF (ADP::LABEL))
```

Macro: TYPE-REF

```Lisp
(defmacro ADP:TYPE-REF (ADP::LABEL))
```

Macro: CODE-TAG

```Lisp
(defmacro ADP::CODE-TAG (ADP::TAGS &BODY ADP::CODE))
```

Macro: CODE-BLOCK

```Lisp
(defmacro ADP:CODE-BLOCK (ADP::TAGS &BODY ADP::CODE))
```

Macro: CODE-EXAMPLE

```Lisp
(defmacro ADP:CODE-EXAMPLE (&BODY ADP::CODE))
```

## API documentation functions

Macro: DEFCLASS

```Lisp
(defmacro ADP:DEFCLASS (&BODY ADP::DEFCLASS-BODY))
```

Macro: DEFCONSTANT

```Lisp
(defmacro ADP:DEFCONSTANT (&BODY ADP::DEFCONSTANT-BODY))
```

Macro: DEFGENERIC

```Lisp
(defmacro ADP:DEFGENERIC (&BODY ADP::DEFGENERIC-BODY))
```

Macro: DEFINE-COMPILER-MACRO

```Lisp
(defmacro ADP:DEFINE-COMPILER-MACRO (&BODY ADP::DEFINE-COMPILER-MACRO-BODY))
```

Macro: DEFINE-CONDITION

```Lisp
(defmacro ADP:DEFINE-CONDITION (&BODY ADP::DEFINE-CONDITION-BODY))
```

Macro: DEFINE-METHOD-COMBINATION

```Lisp
(defmacro ADP:DEFINE-METHOD-COMBINATION (&BODY
                                         ADP::DEFINE-METHOD-COMBINATION-BODY))
```

Macro: DEFINE-MODIFY-MACRO

```Lisp
(defmacro ADP:DEFINE-MODIFY-MACRO (&BODY ADP::DEFINE-MODIFY-MACRO-BODY))
```

Macro: DEFINE-SETF-EXPANDER

```Lisp
(defmacro ADP:DEFINE-SETF-EXPANDER (&BODY ADP::DEFINE-SETF-EXPANDER-BODY))
```

Macro: DEFINE-SYMBOL-MACRO

```Lisp
(defmacro ADP:DEFINE-SYMBOL-MACRO (&BODY ADP::DEFINE-SYMBOL-MACRO-BODY))
```

Macro: DEFMACRO

```Lisp
(defmacro ADP:DEFMACRO (&BODY ADP::DEFMACRO-BODY))
```

Macro: DEFMETHOD

```Lisp
(defmacro ADP:DEFMETHOD (&BODY ADP::DEFMETHOD-BODY))
```

Macro: DEFPACKAGE

```Lisp
(defmacro ADP:DEFPACKAGE (&BODY ADP::DEFPACKAGE-BODY))
```

Macro: DEFPARAMETER

```Lisp
(defmacro ADP:DEFPARAMETER (&BODY ADP::DEFPARAMETER-BODY))
```

Macro: DEFSETF

```Lisp
(defmacro ADP:DEFSETF (&BODY ADP::DEFSETF-BODY))
```

Macro: DEFSTRUCT

```Lisp
(defmacro ADP:DEFSTRUCT (&BODY ADP::DEFSTRUCT-BODY))
```

Macro: DEFTYPE

```Lisp
(defmacro ADP:DEFTYPE (&BODY ADP::DEFTYPE-BODY))
```

Macro: DEFUN

```Lisp
(defmacro ADP:DEFUN (&BODY ADP::DEFUN-BODY))
```

Macro: DEFVAR

```Lisp
(defmacro ADP:DEFVAR (&BODY ADP::DEFVAR-BODY))
```

## Documentation writer function

Macro: WRITE-IN-FILE

```Lisp
(defmacro ADP:WRITE-IN-FILE (ADP::FILE-PATH))
```

Function: LOAD-DOCUMENTATION-SYSTEM

```Lisp
(defun ADP:LOAD-DOCUMENTATION-SYSTEM (ADP::SYSTEM ADP::STYLE &REST
                                      ADP::STYLE-ARGS))
```

