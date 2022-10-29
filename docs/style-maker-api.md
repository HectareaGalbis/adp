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
(defmacro ADP-PRIVATE:DEF-HEADER-WRITER (#:WRITER-ARGS11 &BODY #:BODY-ARG12)
  ...)
```

#### Macro: DEF-SUBHEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SUBHEADER-WRITER (#:WRITER-ARGS14 &BODY #:BODY-ARG15)
  ...)
```

#### Macro: DEF-SUBSUBHEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SUBSUBHEADER-WRITER (#:WRITER-ARGS17 &BODY
                                               #:BODY-ARG18)
  ...)
```

#### Macro: DEF-TEXT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TEXT-WRITER (#:WRITER-ARGS20 &BODY #:BODY-ARG21)
  ...)
```

#### Macro: DEF-TABLE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TABLE-WRITER (#:WRITER-ARGS23 &BODY #:BODY-ARG24)
  ...)
```

#### Macro: DEF-ITEMIZE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-ITEMIZE-WRITER (#:WRITER-ARGS26 &BODY #:BODY-ARG27)
  ...)
```

#### Macro: DEF-IMAGE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-IMAGE-WRITER (#:WRITER-ARGS29 &BODY #:BODY-ARG30)
  ...)
```

#### Macro: DEF-BOLD-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-BOLD-WRITER (#:WRITER-ARGS32 &BODY #:BODY-ARG33)
  ...)
```

#### Macro: DEF-ITALIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-ITALIC-WRITER (#:WRITER-ARGS35 &BODY #:BODY-ARG36)
  ...)
```

#### Macro: DEF-BOLD-ITALIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-BOLD-ITALIC-WRITER (#:WRITER-ARGS38 &BODY
                                              #:BODY-ARG39)
  ...)
```

#### Macro: DEF-CODE-INLINE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-INLINE-WRITER (#:WRITER-ARGS41 &BODY
                                              #:BODY-ARG42)
  ...)
```

#### Macro: DEF-WEB-LINK-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-WEB-LINK-WRITER (#:WRITER-ARGS44 &BODY #:BODY-ARG45)
  ...)
```

#### Macro: DEF-HEADER-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-HEADER-REF-WRITER (#:WRITER-ARGS47 &BODY #:BODY-ARG48)
  ...)
```

#### Macro: DEF-SYMBOL-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SYMBOL-REF-WRITER (#:WRITER-ARGS50 &BODY #:BODY-ARG51)
  ...)
```

#### Macro: DEF-FUNCTION-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FUNCTION-REF-WRITER (#:WRITER-ARGS53 &BODY
                                               #:BODY-ARG54)
  ...)
```

#### Macro: DEF-TYPE-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TYPE-REF-WRITER (#:WRITER-ARGS56 &BODY #:BODY-ARG57)
  ...)
```

#### Macro: DEF-CODE-BLOCK-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-BLOCK-WRITER (#:WRITER-ARGS59 &BODY #:BODY-ARG60)
  ...)
```

#### Macro: DEF-CODE-EXAMPLE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-EXAMPLE-WRITER (#:WRITER-ARGS62 &BODY
                                               #:BODY-ARG63)
  ...)
```

#### Macro: DEF-DEFCLASS-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFCLASS-WRITER (#:WRITER-ARGS65 &BODY #:BODY-ARG66)
  ...)
```

#### Macro: DEF-DEFCONSTANT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFCONSTANT-WRITER (#:WRITER-ARGS68 &BODY
                                              #:BODY-ARG69)
  ...)
```

#### Macro: DEF-DEFGENERIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFGENERIC-WRITER (#:WRITER-ARGS71 &BODY #:BODY-ARG72)
  ...)
```

#### Macro: DEF-DEFINE-COMPILER-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-COMPILER-MACRO-WRITER (#:WRITER-ARGS74 &BODY
                                                        #:BODY-ARG75)
  ...)
```

#### Macro: DEF-DEFINE-CONDITION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-CONDITION-WRITER (#:WRITER-ARGS77 &BODY
                                                   #:BODY-ARG78)
  ...)
```

#### Macro: DEF-DEFINE-METHOD-COMBINATION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-METHOD-COMBINATION-WRITER (#:WRITER-ARGS80
                                                            &BODY #:BODY-ARG81)
  ...)
```

#### Macro: DEF-DEFINE-MODIFY-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-MODIFY-MACRO-WRITER (#:WRITER-ARGS83 &BODY
                                                      #:BODY-ARG84)
  ...)
```

#### Macro: DEF-DEFINE-SETF-EXPANDER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-SETF-EXPANDER-WRITER (#:WRITER-ARGS86 &BODY
                                                       #:BODY-ARG87)
  ...)
```

#### Macro: DEF-DEFINE-SYMBOL-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-SYMBOL-MACRO-WRITER (#:WRITER-ARGS89 &BODY
                                                      #:BODY-ARG90)
  ...)
```

#### Macro: DEF-DEFMACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFMACRO-WRITER (#:WRITER-ARGS92 &BODY #:BODY-ARG93)
  ...)
```

#### Macro: DEF-DEFMETHOD-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFMETHOD-WRITER (#:WRITER-ARGS95 &BODY #:BODY-ARG96)
  ...)
```

#### Macro: DEF-DEFPACKAGE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFPACKAGE-WRITER (#:WRITER-ARGS98 &BODY #:BODY-ARG99)
  ...)
```

#### Macro: DEF-DEFPARAMETER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFPARAMETER-WRITER (#:WRITER-ARGS101 &BODY
                                               #:BODY-ARG102)
  ...)
```

#### Macro: DEF-DEFSETF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFSETF-WRITER (#:WRITER-ARGS104 &BODY #:BODY-ARG105)
  ...)
```

#### Macro: DEF-DEFSTRUCT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFSTRUCT-WRITER (#:WRITER-ARGS107 &BODY
                                            #:BODY-ARG108)
  ...)
```

#### Macro: DEF-DEFTYPE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFTYPE-WRITER (#:WRITER-ARGS110 &BODY #:BODY-ARG111)
  ...)
```

#### Macro: DEF-DEFUN-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFUN-WRITER (#:WRITER-ARGS113 &BODY #:BODY-ARG114)
  ...)
```

#### Macro: DEF-DEFVAR-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFVAR-WRITER (#:WRITER-ARGS116 &BODY #:BODY-ARG117)
  ...)
```

#### Macro: DEF-GET-FILE-EXTENSION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-GET-FILE-EXTENSION-WRITER (#:WRITER-ARGS119 &BODY
                                                     #:BODY-ARG120)
  ...)
```

#### Macro: DEF-FILE-HEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FILE-HEADER-WRITER (#:WRITER-ARGS122 &BODY
                                              #:BODY-ARG123)
  ...)
```

#### Macro: DEF-FILE-FOOT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FILE-FOOT-WRITER (#:WRITER-ARGS125 &BODY
                                            #:BODY-ARG126)
  ...)
```

#### Macro: DEF-SYSTEM-FILES-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SYSTEM-FILES-WRITER (#:WRITER-ARGS128 &BODY
                                               #:BODY-ARG129)
  ...)
```

