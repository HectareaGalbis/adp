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
(defmacro ADP-PRIVATE:DEF-HEADER-WRITER (#:WRITER-ARGS806 &BODY #:BODY-ARG807)
  ...)
```

#### Macro: DEF-SUBHEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SUBHEADER-WRITER (#:WRITER-ARGS809 &BODY
                                            #:BODY-ARG810)
  ...)
```

#### Macro: DEF-SUBSUBHEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SUBSUBHEADER-WRITER (#:WRITER-ARGS812 &BODY
                                               #:BODY-ARG813)
  ...)
```

#### Macro: DEF-TEXT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TEXT-WRITER (#:WRITER-ARGS815 &BODY #:BODY-ARG816)
  ...)
```

#### Macro: DEF-TABLE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TABLE-WRITER (#:WRITER-ARGS818 &BODY #:BODY-ARG819)
  ...)
```

#### Macro: DEF-ITEMIZE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-ITEMIZE-WRITER (#:WRITER-ARGS821 &BODY #:BODY-ARG822)
  ...)
```

#### Macro: DEF-IMAGE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-IMAGE-WRITER (#:WRITER-ARGS824 &BODY #:BODY-ARG825)
  ...)
```

#### Macro: DEF-BOLD-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-BOLD-WRITER (#:WRITER-ARGS827 &BODY #:BODY-ARG828)
  ...)
```

#### Macro: DEF-ITALIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-ITALIC-WRITER (#:WRITER-ARGS830 &BODY #:BODY-ARG831)
  ...)
```

#### Macro: DEF-BOLD-ITALIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-BOLD-ITALIC-WRITER (#:WRITER-ARGS833 &BODY
                                              #:BODY-ARG834)
  ...)
```

#### Macro: DEF-CODE-INLINE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-INLINE-WRITER (#:WRITER-ARGS836 &BODY
                                              #:BODY-ARG837)
  ...)
```

#### Macro: DEF-WEB-LINK-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-WEB-LINK-WRITER (#:WRITER-ARGS839 &BODY #:BODY-ARG840)
  ...)
```

#### Macro: DEF-HEADER-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-HEADER-REF-WRITER (#:WRITER-ARGS842 &BODY
                                             #:BODY-ARG843)
  ...)
```

#### Macro: DEF-SYMBOL-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SYMBOL-REF-WRITER (#:WRITER-ARGS845 &BODY
                                             #:BODY-ARG846)
  ...)
```

#### Macro: DEF-FUNCTION-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FUNCTION-REF-WRITER (#:WRITER-ARGS848 &BODY
                                               #:BODY-ARG849)
  ...)
```

#### Macro: DEF-TYPE-REF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-TYPE-REF-WRITER (#:WRITER-ARGS851 &BODY #:BODY-ARG852)
  ...)
```

#### Macro: DEF-CODE-BLOCK-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-BLOCK-WRITER (#:WRITER-ARGS854 &BODY
                                             #:BODY-ARG855)
  ...)
```

#### Macro: DEF-CODE-EXAMPLE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-CODE-EXAMPLE-WRITER (#:WRITER-ARGS857 &BODY
                                               #:BODY-ARG858)
  ...)
```

#### Macro: DEF-DEFCLASS-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFCLASS-WRITER (#:WRITER-ARGS860 &BODY #:BODY-ARG861)
  ...)
```

#### Macro: DEF-DEFCONSTANT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFCONSTANT-WRITER (#:WRITER-ARGS863 &BODY
                                              #:BODY-ARG864)
  ...)
```

#### Macro: DEF-DEFGENERIC-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFGENERIC-WRITER (#:WRITER-ARGS866 &BODY
                                             #:BODY-ARG867)
  ...)
```

#### Macro: DEF-DEFINE-COMPILER-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-COMPILER-MACRO-WRITER (#:WRITER-ARGS869 &BODY
                                                        #:BODY-ARG870)
  ...)
```

#### Macro: DEF-DEFINE-CONDITION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-CONDITION-WRITER (#:WRITER-ARGS872 &BODY
                                                   #:BODY-ARG873)
  ...)
```

#### Macro: DEF-DEFINE-METHOD-COMBINATION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-METHOD-COMBINATION-WRITER (#:WRITER-ARGS875
                                                            &BODY #:BODY-ARG876)
  ...)
```

#### Macro: DEF-DEFINE-MODIFY-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-MODIFY-MACRO-WRITER (#:WRITER-ARGS878 &BODY
                                                      #:BODY-ARG879)
  ...)
```

#### Macro: DEF-DEFINE-SETF-EXPANDER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-SETF-EXPANDER-WRITER (#:WRITER-ARGS881 &BODY
                                                       #:BODY-ARG882)
  ...)
```

#### Macro: DEF-DEFINE-SYMBOL-MACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFINE-SYMBOL-MACRO-WRITER (#:WRITER-ARGS884 &BODY
                                                      #:BODY-ARG885)
  ...)
```

#### Macro: DEF-DEFMACRO-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFMACRO-WRITER (#:WRITER-ARGS887 &BODY #:BODY-ARG888)
  ...)
```

#### Macro: DEF-DEFMETHOD-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFMETHOD-WRITER (#:WRITER-ARGS890 &BODY
                                            #:BODY-ARG891)
  ...)
```

#### Macro: DEF-DEFPACKAGE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFPACKAGE-WRITER (#:WRITER-ARGS893 &BODY
                                             #:BODY-ARG894)
  ...)
```

#### Macro: DEF-DEFPARAMETER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFPARAMETER-WRITER (#:WRITER-ARGS896 &BODY
                                               #:BODY-ARG897)
  ...)
```

#### Macro: DEF-DEFSETF-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFSETF-WRITER (#:WRITER-ARGS899 &BODY #:BODY-ARG900)
  ...)
```

#### Macro: DEF-DEFSTRUCT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFSTRUCT-WRITER (#:WRITER-ARGS902 &BODY
                                            #:BODY-ARG903)
  ...)
```

#### Macro: DEF-DEFTYPE-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFTYPE-WRITER (#:WRITER-ARGS905 &BODY #:BODY-ARG906)
  ...)
```

#### Macro: DEF-DEFUN-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFUN-WRITER (#:WRITER-ARGS908 &BODY #:BODY-ARG909)
  ...)
```

#### Macro: DEF-DEFVAR-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-DEFVAR-WRITER (#:WRITER-ARGS911 &BODY #:BODY-ARG912)
  ...)
```

#### Macro: DEF-GET-FILE-EXTENSION-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-GET-FILE-EXTENSION-WRITER (#:WRITER-ARGS914 &BODY
                                                     #:BODY-ARG915)
  ...)
```

#### Macro: DEF-FILE-HEADER-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FILE-HEADER-WRITER (#:WRITER-ARGS917 &BODY
                                              #:BODY-ARG918)
  ...)
```

#### Macro: DEF-FILE-FOOT-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-FILE-FOOT-WRITER (#:WRITER-ARGS920 &BODY
                                            #:BODY-ARG921)
  ...)
```

#### Macro: DEF-SYSTEM-FILES-WRITER

```Lisp
(defmacro ADP-PRIVATE:DEF-SYSTEM-FILES-WRITER (#:WRITER-ARGS923 &BODY
                                               #:BODY-ARG924)
  ...)
```

