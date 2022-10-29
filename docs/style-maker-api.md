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
(defmacro ADP-PRIVATE:DEF-HEADER-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-SUBHEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SUBHEADER-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-SUBSUBHEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SUBSUBHEADER-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-TEXT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TEXT-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-TABLE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TABLE-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-ITEMIZE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-ITEMIZE-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-IMAGE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-IMAGE-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-BOLD-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-BOLD-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-ITALIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-ITALIC-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-BOLD-ITALIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-BOLD-ITALIC-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-CODE-INLINE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-INLINE-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-WEB-LINK-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-WEB-LINK-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-HEADER-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-HEADER-REF-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-SYMBOL-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SYMBOL-REF-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-FUNCTION-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FUNCTION-REF-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-TYPE-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TYPE-REF-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-CODE-BLOCK-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-BLOCK-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-CODE-EXAMPLE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-EXAMPLE-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFCLASS-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFCLASS-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFCONSTANT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFCONSTANT-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFGENERIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFGENERIC-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFINE-COMPILER-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-COMPILER-MACRO-WRITER (#:WRITER-ARGS &BODY
                                                        #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFINE-CONDITION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-CONDITION-WRITER (#:WRITER-ARGS &BODY
                                                   #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFINE-METHOD-COMBINATION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-METHOD-COMBINATION-WRITER (#:WRITER-ARGS &BODY
                                                            #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFINE-MODIFY-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-MODIFY-MACRO-WRITER (#:WRITER-ARGS &BODY
                                                      #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFINE-SETF-EXPANDER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-SETF-EXPANDER-WRITER (#:WRITER-ARGS &BODY
                                                       #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFINE-SYMBOL-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-SYMBOL-MACRO-WRITER (#:WRITER-ARGS &BODY
                                                      #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFMACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFMACRO-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFMETHOD-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFMETHOD-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFPACKAGE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFPACKAGE-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFPARAMETER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFPARAMETER-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFSETF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFSETF-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFSTRUCT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFSTRUCT-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFTYPE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFTYPE-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFUN-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFUN-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-DEFVAR-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFVAR-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-GET-FILE-EXTENSION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-GET-FILE-EXTENSION-WRITER (#:WRITER-ARGS &BODY
                                                     #:BODY-ARG)
  ...)
```

#### Macro: DEF-FILE-HEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FILE-HEADER-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-FILE-FOOT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FILE-FOOT-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

#### Macro: DEF-SYSTEM-FILES-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SYSTEM-FILES-WRITER (#:WRITER-ARGS &BODY #:BODY-ARG)
  ...)
```

