# ADP User Interface

## Literate programming functions

#### Macro: HEADER

```Lisp
(defmacro ADP:HEADER (STR &OPTIONAL LABEL)
  ...)
```

#### Macro: SUBHEADER

```Lisp
(defmacro ADP:SUBHEADER (STR &OPTIONAL LABEL)
  ...)
```

#### Macro: SUBSUBHEADER

```Lisp
(defmacro ADP:SUBSUBHEADER (STR &OPTIONAL LABEL)
  ...)
```

#### Macro: TEXT

```Lisp
(defmacro ADP:TEXT (&REST OBJECTS)
  ...)
```

#### Macro: TABLE

```Lisp
(defmacro ADP:TABLE (&REST ROWS)
  ...)
```

#### Macro: ITEMIZE

```Lisp
(defmacro ADP:ITEMIZE (&REST ITEMS)
  ...)
```

#### Macro: IMAGE

```Lisp
(defmacro ADP:IMAGE (ALT-TEXT PATH)
  ...)
```

#### Macro: BOLD

```Lisp
(defmacro ADP:BOLD (&REST ARGS)
  ...)
```

#### Macro: ITALIC

```Lisp
(defmacro ADP:ITALIC (&REST ARGS)
  ...)
```

#### Macro: BOLD-ITALIC

```Lisp
(defmacro ADP:BOLD-ITALIC (&REST ARGS)
  ...)
```

#### Macro: CODE-INLINE

```Lisp
(defmacro ADP:CODE-INLINE (&REST CODE)
  ...)
```

#### Macro: WEB-LINK

```Lisp
(defmacro ADP:WEB-LINK (NAME LINK)
  ...)
```

#### Macro: FILE-REF

```Lisp
(defmacro ADP:FILE-REF (PATH)
  ...)
```

#### Macro: HEADER-REF

```Lisp
(defmacro ADP:HEADER-REF (LABEL)
  ...)
```

#### Macro: SYMBOL-REF

```Lisp
(defmacro ADP:SYMBOL-REF (LABEL)
  ...)
```

#### Macro: FUNCTION-REF

```Lisp
(defmacro ADP:FUNCTION-REF (LABEL)
  ...)
```

#### Macro: TYPE-REF

```Lisp
(defmacro ADP:TYPE-REF (LABEL)
  ...)
```

#### Macro: CODE-TAG

```Lisp
(defmacro ADP::CODE-TAG (TAGS &BODY CODE)
  ...)
```

#### Macro: CODE-BLOCK

```Lisp
(defmacro ADP:CODE-BLOCK (TAGS &BODY CODE)
  ...)
```

#### Macro: CODE-EXAMPLE

```Lisp
(defmacro ADP:CODE-EXAMPLE (&BODY CODE)
  ...)
```

## API documentation functions

#### Macro: DEFCLASS

```Lisp
(defmacro ADP:DEFCLASS (&BODY DEFCLASS-BODY)
  ...)
```

#### Macro: DEFCONSTANT

```Lisp
(defmacro ADP:DEFCONSTANT (&BODY DEFCONSTANT-BODY)
  ...)
```

#### Macro: DEFGENERIC

```Lisp
(defmacro ADP:DEFGENERIC (&BODY DEFGENERIC-BODY)
  ...)
```

#### Macro: DEFINE-COMPILER-MACRO

```Lisp
(defmacro ADP:DEFINE-COMPILER-MACRO (&BODY DEFINE-COMPILER-MACRO-BODY)
  ...)
```

#### Macro: DEFINE-CONDITION

```Lisp
(defmacro ADP:DEFINE-CONDITION (&BODY DEFINE-CONDITION-BODY)
  ...)
```

#### Macro: DEFINE-METHOD-COMBINATION

```Lisp
(defmacro ADP:DEFINE-METHOD-COMBINATION (&BODY DEFINE-METHOD-COMBINATION-BODY)
  ...)
```

#### Macro: DEFINE-MODIFY-MACRO

```Lisp
(defmacro ADP:DEFINE-MODIFY-MACRO (&BODY DEFINE-MODIFY-MACRO-BODY)
  ...)
```

#### Macro: DEFINE-SETF-EXPANDER

```Lisp
(defmacro ADP:DEFINE-SETF-EXPANDER (&BODY DEFINE-SETF-EXPANDER-BODY)
  ...)
```

#### Macro: DEFINE-SYMBOL-MACRO

```Lisp
(defmacro ADP:DEFINE-SYMBOL-MACRO (&BODY DEFINE-SYMBOL-MACRO-BODY)
  ...)
```

#### Macro: DEFMACRO

```Lisp
(defmacro ADP:DEFMACRO (&BODY DEFMACRO-BODY)
  ...)
```

#### Macro: DEFMETHOD

```Lisp
(defmacro ADP:DEFMETHOD (&BODY DEFMETHOD-BODY)
  ...)
```

#### Macro: DEFPACKAGE

```Lisp
(defmacro ADP:DEFPACKAGE (&BODY DEFPACKAGE-BODY)
  ...)
```

#### Macro: DEFPARAMETER

```Lisp
(defmacro ADP:DEFPARAMETER (&BODY DEFPARAMETER-BODY)
  ...)
```

#### Macro: DEFSETF

```Lisp
(defmacro ADP:DEFSETF (&BODY DEFSETF-BODY)
  ...)
```

#### Macro: DEFSTRUCT

```Lisp
(defmacro ADP:DEFSTRUCT (&BODY DEFSTRUCT-BODY)
  ...)
```

#### Macro: DEFTYPE

```Lisp
(defmacro ADP:DEFTYPE (&BODY DEFTYPE-BODY)
  ...)
```

#### Macro: DEFUN

```Lisp
(defmacro ADP:DEFUN (&BODY DEFUN-BODY)
  ...)
```

#### Macro: DEFVAR

```Lisp
(defmacro ADP:DEFVAR (&BODY DEFVAR-BODY)
  ...)
```

## Documentation writer function

#### Macro: WRITE-IN-FILE

```Lisp
(defmacro ADP:WRITE-IN-FILE (FILE-PATH)
  ...)
```

#### Function: LOAD-DOCUMENTATION-SYSTEM

```Lisp
(defun ADP:LOAD-DOCUMENTATION-SYSTEM (SYSTEM STYLE &REST STYLE-ARGS))
```

