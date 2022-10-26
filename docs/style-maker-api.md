# Style-maker interface

## Style parameters

#### Macro: DEF-STYLE-PARAMETER

```Lisp
(defmacro ADP-PRIVATE:DEF-STYLE-PARAMETER (NAME &KEY (VALUE NIL) (KEY-NAME NIL)
                                           (REQUIRED NIL))
  ...)
```

## Customizable writer macros

#### Macro: DEF-HEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-HEADER-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-SUBHEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SUBHEADER-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-SUBSUBHEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SUBSUBHEADER-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-TEXT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TEXT-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-TABLE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TABLE-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-ITEMIZE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-ITEMIZE-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-IMAGE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-IMAGE-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-BOLD-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-BOLD-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-ITALIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-ITALIC-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-BOLD-ITALIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-BOLD-ITALIC-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-CODE-INLINE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-INLINE-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-WEB-LINK-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-WEB-LINK-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-HEADER-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-HEADER-REF-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-SYMBOL-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SYMBOL-REF-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-FUNCTION-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FUNCTION-REF-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-TYPE-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TYPE-REF-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-CODE-BLOCK-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-BLOCK-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-CODE-EXAMPLE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-EXAMPLE-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFCLASS-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFCLASS-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFCONSTANT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFCONSTANT-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFGENERIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFGENERIC-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFINE-COMPILER-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-COMPILER-MACRO-WRITER (#:WRITER-ARGS1 &BODY
                                                        #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFINE-CONDITION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-CONDITION-WRITER (#:WRITER-ARGS1 &BODY
                                                   #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFINE-METHOD-COMBINATION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-METHOD-COMBINATION-WRITER (#:WRITER-ARGS1
                                                            &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFINE-MODIFY-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-MODIFY-MACRO-WRITER (#:WRITER-ARGS1 &BODY
                                                      #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFINE-SETF-EXPANDER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-SETF-EXPANDER-WRITER (#:WRITER-ARGS1 &BODY
                                                       #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFINE-SYMBOL-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-SYMBOL-MACRO-WRITER (#:WRITER-ARGS1 &BODY
                                                      #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFMACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFMACRO-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFMETHOD-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFMETHOD-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFPACKAGE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFPACKAGE-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFPARAMETER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFPARAMETER-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFSETF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFSETF-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFSTRUCT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFSTRUCT-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFTYPE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFTYPE-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFUN-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFUN-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-DEFVAR-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFVAR-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-GET-FILE-EXTENSION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-GET-FILE-EXTENSION-WRITER (#:WRITER-ARGS1 &BODY
                                                     #:BODY-ARG2)
  ...)
```

#### Macro: DEF-FILE-HEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FILE-HEADER-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-FILE-FOOT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FILE-FOOT-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```

#### Macro: DEF-SYSTEM-FILES-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SYSTEM-FILES-WRITER (#:WRITER-ARGS1 &BODY #:BODY-ARG2)
  ...)
```
