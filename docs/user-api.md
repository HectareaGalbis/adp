# ADP User Interface

## Literate programming functions

#### ***Macro*** ADP:HEADER

```Lisp
(DEFMACRO ADP:HEADER (ADP::STR &OPTIONAL ADP::LABEL)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(PROGN
      ,@(WHEN ADP::LABEL
          `((ADP-PRIVATE:PUSH-HEADER-TAG ',ADP::LABEL ,ADP::STR)))
      (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :HEADER ,ADP::STR ',ADP::LABEL))))
```

#### ***Macro*** ADP:SUBHEADER

```Lisp
(DEFMACRO ADP:SUBHEADER (ADP::STR &OPTIONAL ADP::LABEL)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(PROGN
      ,@(WHEN ADP::LABEL
          `((ADP-PRIVATE:PUSH-HEADER-TAG ',ADP::LABEL ,ADP::STR)))
      (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :SUBHEADER ,ADP::STR ',ADP::LABEL))))
```

#### ***Macro*** ADP:SUBSUBHEADER

```Lisp
(DEFMACRO ADP:SUBSUBHEADER (ADP::STR &OPTIONAL ADP::LABEL)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(PROGN
      ,@(WHEN ADP::LABEL
          `((ADP-PRIVATE:PUSH-HEADER-TAG ',ADP::LABEL ,ADP::STR)))
      (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :SUBSUBHEADER ,ADP::STR ',ADP::LABEL))))
```

#### ***Macro*** ADP:TEXT

```Lisp
(DEFMACRO ADP:TEXT (&REST ADP::OBJECTS)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(ADP-PRIVATE:EMPLACE-ADP-ELEMENT :TEXT ,@ADP::OBJECTS)))
```

#### ***Macro*** ADP:TABLE

```Lisp
(DEFMACRO ADP:TABLE (&REST ADP::ROWS)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    (LOOP ADP::FOR ADP::ROW ADP::IN ADP::ROWS
          DO (CHECK-TYPE ADP::ROW LIST
                         "a list") (LOOP ADP::FOR ADP::ELEM ADP::IN ADP::ROW
                                         DO (ASSERT (EQ (CAR ADP::ELEM) :CELL)
                                                    NIL
                                                    "Each cell of a table must be a list starting with :cell. Found: ~s"
                                                    ADP::ELEM)))
    `(ADP-PRIVATE:EMPLACE-ADP-ELEMENT :TABLE
                                      ,@(LOOP ADP::FOR ADP::ROW ADP::IN ADP::ROWS
                                              ADP::COLLECT (CONS 'LIST
                                                                 (LOOP ADP::FOR ADP::ELEM ADP::IN ADP::ROW
                                                                       ADP::COLLECT (CONS
                                                                                     'LIST
                                                                                     ADP::ELEM)))))))
```

#### ***Macro*** ADP:ITEMIZE

```Lisp
(DEFMACRO ADP:ITEMIZE (&REST ADP::ITEMS)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    (LABELS ((ADP::CHECK-ITEMS (ADP::ITEM-LIST)
               (LOOP ADP::FOR ADP::ITEM ADP::IN ADP::ITEM-LIST
                     IF (NOT (EQ (CAR ADP::ITEM) :ITEM))
                     DO (IF (EQ (CAR ADP::ITEM) :ITEMIZE)
                            (ADP::CHECK-ITEMS (CDR ADP::ITEM))
                            (ERROR
                             "Each item of itemize must be a list starting with :item ot :itemize. Found: ~s"
                             ADP::ITEM)))))
      (ADP::CHECK-ITEMS ADP::ITEMS))
    (LABELS ((ADP::PROCESS-ITEMIZE-ITEMS (ADP::ITEM-LIST)
               (LOOP ADP::FOR ADP::ITEM ADP::IN ADP::ITEM-LIST
                     IF (EQ (CAR ADP::ITEM) :ITEM)
                     ADP::COLLECT (CONS 'LIST ADP::ITEM) ADP::ELSE
                     ADP::COLLECT (LIST* 'LIST :ITEMIZE
                                         (ADP::PROCESS-ITEMIZE-ITEMS
                                          (CDR ADP::ITEM))))))
      `(ADP-PRIVATE:EMPLACE-ADP-ELEMENT :ITEMIZE
                                        ,@(ADP::PROCESS-ITEMIZE-ITEMS
                                           ADP::ITEMS)))))
```

#### ***Macro*** ADP:IMAGE

```Lisp
(DEFMACRO ADP:IMAGE (ADP::ALT-TEXT ADP::PATH)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    (ALEXANDRIA:WITH-GENSYMS (ADP::LET-ALT-TEXT ADP::LET-PATH)
      `(LET ((,ADP::LET-ALT-TEXT ,ADP::ALT-TEXT) (,ADP::LET-PATH ,ADP::PATH))
         (DECLARE (TYPE STRING ,ADP::LET-ALT-TEXT)
                  (TYPE PATHNAME ,ADP::LET-PATH))
         (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :IMAGE ,ADP::LET-ALT-TEXT
                                          ,ADP::LET-PATH)))))
```

#### ***Macro*** ADP:BOLD

```Lisp
(DEFMACRO ADP:BOLD (&REST ADP::ARGS)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(ADP-PRIVATE:CREATE-BOLD-TEXT ,@ADP::ARGS)))
```

#### ***Macro*** ADP:ITALIC

```Lisp
(DEFMACRO ADP:ITALIC (&REST ADP::ARGS)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(ADP-PRIVATE:CREATE-ITALIC-TEXT ,@ADP::ARGS)))
```

#### ***Macro*** ADP:CODE-INLINE

```Lisp
(DEFMACRO ADP:CODE-INLINE (ADP::CODE)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(ADP-PRIVATE:CREATE-CODE-INLINE-TEXT ,ADP::CODE)))
```

#### ***Macro*** ADP:WEB-LINK

```Lisp
(DEFMACRO ADP:WEB-LINK (ADP::NAME ADP::LINK)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(ADP-PRIVATE:CREATE-WEB-LINK-TEXT ,ADP::NAME ,ADP::LINK)))
```

#### ***Macro*** ADP:HEADER-REF

```Lisp
(DEFMACRO ADP:HEADER-REF (ADP::LABEL)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(ADP-PRIVATE:CREATE-HEADER-REF-TEXT ',ADP::LABEL)))
```

#### ***Macro*** ADP:SYMBOL-REF

```Lisp
(DEFMACRO ADP:SYMBOL-REF (ADP::LABEL)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(ADP-PRIVATE:CREATE-SYMBOL-REF-TEXT ',ADP::LABEL)))
```

#### ***Macro*** ADP:FUNCTION-REF

```Lisp
(DEFMACRO ADP:FUNCTION-REF (ADP::LABEL)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(ADP-PRIVATE:CREATE-FUNCTION-REF-TEXT ',ADP::LABEL)))
```

#### ***Macro*** ADP:TYPE-REF

```Lisp
(DEFMACRO ADP:TYPE-REF (ADP::LABEL)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    `(ADP-PRIVATE:CREATE-TYPE-REF-TEXT ',ADP::LABEL)))
```

#### ***Macro*** ADP::CODE-TAG

```Lisp
(DEFMACRO ADP::CODE-TAG (ADP::TAGS &BODY ADP::CODE)
  (ALEXANDRIA:WITH-GENSYMS (ADP::TAG)
    `(PROGN
      ,@(ADP-PRIVATE:REMOVE-OWN-CODE-HIDE-EXPRS ADP::CODE)
      ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
          (ASSERT (OR (SYMBOLP ADP::TAGS) (LISTP ADP::TAGS)) (ADP::TAGS)
                  "~s is not a symbol nor a list" ADP::TAGS)
          (WHEN (LISTP ADP::TAGS)
            (ASSERT (EVERY #'SYMBOLP ADP::TAGS) (ADP::TAGS)
                    "Thare is a non-symbol in ~s" ADP::TAGS))
          `((LOOP ADP::FOR ,ADP::TAG ADP::IN ',ADP::TAGS
                  DO (APPLY #'ADP-PRIVATE:ADD-CODE-TAG ,ADP::TAG
                            ',ADP::CODE)))))))
```

#### ***Macro*** ADP:CODE-BLOCK

```Lisp
(DEFMACRO ADP:CODE-BLOCK (ADP::TAGS &BODY ADP::CODE)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    (ALEXANDRIA:WITH-GENSYMS (ADP::EXPR)
      `(ADP-PRIVATE:EMPLACE-ADP-ELEMENT :CODE-BLOCK
                                        (LOOP ADP::FOR ,ADP::EXPR ADP::IN ',ADP::CODE
                                              IF (AND (SYMBOLP ,ADP::EXPR)
                                                      (MEMBER ,ADP::EXPR
                                                              ',ADP::TAGS))
                                              APPEND (ADP-PRIVATE:PROCESS-CODE-TAG
                                                      ,ADP::EXPR
                                                      (COERCE
                                                       (ADP-PRIVATE:GET-CODE-TAG
                                                        ,ADP::EXPR)
                                                       'LIST)) ADP::ELSE
                                              ADP::COLLECT (ADP-PRIVATE:PROCESS-CODE-TAG
                                                            '#:DUMMY-TAG
                                                            ,ADP::EXPR))))))
```

#### ***Macro*** ADP:CODE-EXAMPLE

```Lisp
(DEFMACRO ADP:CODE-EXAMPLE (&BODY ADP::CODE)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    (LET ((ADP::EVALUATED-CODE
           (LOOP ADP::FOR ADP::EXPR ADP::IN ADP::CODE
                 ADP::COLLECT (ALEXANDRIA:WITH-GENSYMS (ADP::OUTPUT ADP::RESULT)
                                `(LET* ((,ADP::OUTPUT
                                         (MAKE-ARRAY 10 :ADJUSTABLE T
                                                     :FILL-POINTER 0
                                                     :ELEMENT-TYPE 'CHARACTER))
                                        (,ADP::RESULT
                                         (MULTIPLE-VALUE-LIST
                                          (WITH-OUTPUT-TO-STRING
                                              (*STANDARD-OUTPUT* ,ADP::OUTPUT)
                                            ,(ADP-PRIVATE:REMOVE-CODE-TAG-EXPRS
                                              ADP::EXPR)))))
                                   (LIST
                                    ',(ADP-PRIVATE:PROCESS-CODE-TAG
                                       '#:DUMMY-TAG ADP::EXPR)
                                    ,ADP::OUTPUT ,ADP::RESULT))))))
      `(ADP-PRIVATE:EMPLACE-ADP-ELEMENT :CODE-EXAMPLE
                                        (LIST ,@ADP::EVALUATED-CODE)))))
```

## API documentation functions

#### ***Macro*** ADP:DEFCLASS

```Lisp
(DEFMACRO ADP:DEFCLASS (&BODY ADP::DEFCLASS-BODY)
  `(PROGN
    (DEFCLASS ,@ADP::DEFCLASS-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-TYPE-TAG (CAR ',ADP::DEFCLASS-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFCLASS
                                           '(DEFCLASS ,@ADP::DEFCLASS-BODY))))))
```

#### ***Macro*** ADP:DEFCONSTANT

```Lisp
(DEFMACRO ADP:DEFCONSTANT (&BODY ADP::DEFCONSTANT-BODY)
  `(PROGN
    (DEFCONSTANT ,@ADP::DEFCONSTANT-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-SYMBOL-TAG (CAR ',ADP::DEFCONSTANT-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFCONSTANT
                                           '(DEFCONSTANT
                                                ,@ADP::DEFCONSTANT-BODY))))))
```

#### ***Macro*** ADP:DEFGENERIC

```Lisp
(DEFMACRO ADP:DEFGENERIC (&BODY ADP::DEFGENERIC-BODY)
  `(PROGN
    (DEFGENERIC ,@ADP::DEFGENERIC-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-FUNCTION-TAG (CAR ',ADP::DEFGENERIC-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFGENERIC
                                           '(DEFGENERIC ,@ADP::DEFGENERIC-BODY))))))
```

#### ***Macro*** ADP:DEFINE-COMPILER-MACRO

```Lisp
(DEFMACRO ADP:DEFINE-COMPILER-MACRO (&BODY ADP::DEFINE-COMPILER-MACRO-BODY)
  `(PROGN
    (DEFINE-COMPILER-MACRO ,@ADP::DEFINE-COMPILER-MACRO-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFINE-COMPILER-MACRO
                                           '(DEFINE-COMPILER-MACRO ,@ADP::DEFINE-COMPILER-MACRO-BODY))))))
```

#### ***Macro*** ADP:DEFINE-CONDITION

```Lisp
(DEFMACRO ADP:DEFINE-CONDITION (&BODY ADP::DEFINE-CONDITION-BODY)
  `(PROGN
    (DEFINE-CONDITION ,@ADP::DEFINE-CONDITION-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-TYPE-TAG (CAR ',ADP::DEFINE-CONDITION-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFINE-CONDITION
                                           '(DEFINE-CONDITION ,@ADP::DEFINE-CONDITION-BODY))))))
```

#### ***Macro*** ADP:DEFINE-METHOD-COMBINATION

```Lisp
(DEFMACRO ADP:DEFINE-METHOD-COMBINATION
          (&BODY ADP::DEFINE-METHOD-COMBINATION-BODY)
  `(PROGN
    (DEFINE-METHOD-COMBINATION ,@ADP::DEFINE-METHOD-COMBINATION-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFINE-METHOD-COMBINATION
                                           '(DEFINE-METHOD-COMBINATION ,@ADP::DEFINE-METHOD-COMBINATION-BODY))))))
```

#### ***Macro*** ADP:DEFINE-MODIFY-MACRO

```Lisp
(DEFMACRO ADP:DEFINE-MODIFY-MACRO (&BODY ADP::DEFINE-MODIFY-MACRO-BODY)
  `(PROGN
    (DEFINE-MODIFY-MACRO ,@ADP::DEFINE-MODIFY-MACRO-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-FUNCTION-TAG (CAR ',ADP::DEFINE-MODIFY-MACRO-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFINE-MODIFY-MACRO
                                           '(DEFINE-MODIFY-MACRO ,@ADP::DEFINE-MODIFY-MACRO-BODY))))))
```

#### ***Macro*** ADP:DEFINE-SETF-EXPANDER

```Lisp
(DEFMACRO ADP:DEFINE-SETF-EXPANDER (&BODY ADP::DEFINE-SETF-EXPANDER-BODY)
  `(PROGN
    (DEFINE-SETF-EXPANDER ,@ADP::DEFINE-SETF-EXPANDER-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-FUNCTION-TAG
           (LIST 'SETF (CAR ',ADP::DEFINE-SETF-EXPANDER-BODY)))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFINE-SETF-EXPANDER
                                           '(DEFINE-SETF-EXPANDER ,@ADP::DEFINE-SETF-EXPANDER-BODY))))))
```

#### ***Macro*** ADP:DEFINE-SYMBOL-MACRO

```Lisp
(DEFMACRO ADP:DEFINE-SYMBOL-MACRO (&BODY ADP::DEFINE-SYMBOL-MACRO-BODY)
  `(PROGN
    (DEFINE-SYMBOL-MACRO ,@ADP::DEFINE-SYMBOL-MACRO-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-SYMBOL-TAG (CAR ',ADP::DEFINE-SYMBOL-MACRO-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFINE-SYMBOL-MACRO
                                           '(DEFINE-SYMBOL-MACRO
                                             ,@ADP::DEFINE-SYMBOL-MACRO-BODY))))))
```

#### ***Macro*** ADP:DEFMACRO

```Lisp
(DEFMACRO ADP:DEFMACRO (&BODY ADP::DEFMACRO-BODY)
  `(PROGN
    (DEFMACRO ,@ADP::DEFMACRO-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-FUNCTION-TAG (CAR ',ADP::DEFMACRO-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFMACRO
                                           '(DEFMACRO ,@ADP::DEFMACRO-BODY))))))
```

#### ***Macro*** ADP:DEFMETHOD

```Lisp
(DEFMACRO ADP:DEFMETHOD (&BODY ADP::DEFMETHOD-BODY)
  `(PROGN
    (DEFMETHOD ,@ADP::DEFMETHOD-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFMETHOD
                                           '(DEFMETHOD ,@ADP::DEFMETHOD-BODY))))))
```

#### ***Macro*** ADP:DEFPACKAGE

```Lisp
(DEFMACRO ADP:DEFPACKAGE (&BODY ADP::DEFPACKAGE-BODY)
  `(PROGN
    (DEFPACKAGE ,@ADP::DEFPACKAGE-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFPACKAGE
                                           '(DEFPACKAGE
                                                ,@ADP::DEFPACKAGE-BODY))))))
```

#### ***Macro*** ADP:DEFPARAMETER

```Lisp
(DEFMACRO ADP:DEFPARAMETER (&BODY ADP::DEFPARAMETER-BODY)
  `(PROGN
    (DEFPARAMETER ,@ADP::DEFPARAMETER-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-SYMBOL-TAG (CAR ',ADP::DEFPARAMETER-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFPARAMETER
                                           '(DEFPARAMETER
                                                ,@ADP::DEFPARAMETER-BODY))))))
```

#### ***Macro*** ADP:DEFSETF

```Lisp
(DEFMACRO ADP:DEFSETF (&BODY ADP::DEFSETF-BODY)
  `(PROGN
    (DEFSETF ,@ADP::DEFSETF-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-FUNCTION-TAG
           (LIST 'SETF (CAR ',ADP::DEFSETF-BODY)))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFSETF
                                           '(DEFSETF ,@ADP::DEFSETF-BODY))))))
```

#### ***Macro*** ADP:DEFSTRUCT

```Lisp
(DEFMACRO ADP:DEFSTRUCT (&BODY ADP::DEFSTRUCT-BODY)
  `(PROGN
    (DEFSTRUCT ,@ADP::DEFSTRUCT-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-TYPE-TAG (CAR ',ADP::DEFSTRUCT-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFSTRUCT
                                           '(DEFSTRUCT
                                                ,@ADP::DEFSTRUCT-BODY))))))
```

#### ***Macro*** ADP:DEFTYPE

```Lisp
(DEFMACRO ADP:DEFTYPE (&BODY ADP::DEFTYPE-BODY)
  `(PROGN
    (DEFTYPE ,@ADP::DEFTYPE-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-TYPE-TAG (CAR ',ADP::DEFTYPE-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFTYPE
                                           '(DEFTYPE ,@ADP::DEFTYPE-BODY))))))
```

#### ***Macro*** ADP:DEFUN

```Lisp
(DEFMACRO ADP:DEFUN (&BODY ADP::DEFUN-BODY)
  `(PROGN
    (DEFUN ,@ADP::DEFUN-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-FUNCTION-TAG (CAR ',ADP::DEFUN-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFUN
                                           '(DEFUN ,@ADP::DEFUN-BODY))))))
```

#### ***Macro*** ADP:DEFVAR

```Lisp
(DEFMACRO ADP:DEFVAR (&BODY ADP::DEFVAR-BODY)
  `(PROGN
    (DEFVAR ,@ADP::DEFVAR-BODY)
    ,@(WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
        `((ADP-PRIVATE:PUSH-SYMBOL-TAG (CAR ',ADP::DEFVAR-BODY))
          (ADP-PRIVATE:EMPLACE-ADP-ELEMENT :DEFVAR
                                           '(DEFVAR ,@ADP::DEFVAR-BODY))))))
```

## Documentation writer function

#### ***Macro*** ADP:WRITE-IN-FILE

```Lisp
(DEFMACRO ADP:WRITE-IN-FILE (ADP::FILE-PATH)
  (WHEN ADP-PRIVATE:*ADD-DOCUMENTATION*
    (ASSERT (PATHNAME-NAME ADP::FILE-PATH) (ADP::FILE-PATH)
            "The ~s pathname has not a name part." ADP::FILE-PATH)
    (ALEXANDRIA:WITH-GENSYMS (ADP::LET-FILE-PATH ADP::HEADER-TAG
                              ADP::HEADER-STR ADP::SYMBOL-TAG ADP::FUNCTION-TAG
                              ADP::TYPE-TAG)
      (ALEXANDRIA:ONCE-ONLY (ADP::FILE-PATH)
        `(PROGN
          (LET ((,ADP::LET-FILE-PATH
                 (MAKE-PATHNAME :DIRECTORY
                                (IF (PATHNAME-DIRECTORY ,ADP::FILE-PATH)
                                    (CONS :RELATIVE
                                          (CDR
                                           (PATHNAME-DIRECTORY
                                            ,ADP::FILE-PATH)))
                                    NIL)
                                :NAME (PATHNAME-NAME ,ADP::FILE-PATH))))
            (ADP-PRIVATE:EMPLACE-ADP-FILE ,ADP::LET-FILE-PATH
                                          (ALEXANDRIA:COPY-ARRAY
                                           ADP-PRIVATE:*FILE-ADP-ELEMENTS*))
            (ADP-PRIVATE:EMPTY-ADP-ELEMENTS)
            (LOOP ADP::FOR (,ADP::HEADER-TAG
                            . ,ADP::HEADER-STR) ADP::ACROSS ADP-PRIVATE:*HEADER-TAGS*
                  DO (ADP-PRIVATE:ADD-HEADER-TAG-PATH ,ADP::HEADER-TAG
                                                      ,ADP::HEADER-STR
                                                      ,ADP::LET-FILE-PATH)
                  ADP::FINALLY (ADP-PRIVATE:EMPTY-HEADER-TAGS))
            (LOOP ADP::FOR ,ADP::SYMBOL-TAG ADP::ACROSS ADP-PRIVATE:*SYMBOL-TAGS*
                  DO (ADP-PRIVATE:ADD-SYMBOL-TAG-PATH ,ADP::SYMBOL-TAG
                                                      ,ADP::LET-FILE-PATH)
                  ADP::FINALLY (ADP-PRIVATE:EMPTY-SYMBOL-TAGS))
            (LOOP ADP::FOR ,ADP::FUNCTION-TAG ADP::ACROSS ADP-PRIVATE:*FUNCTION-TAGS*
                  DO (ADP-PRIVATE:ADD-FUNCTION-TAG-PATH ,ADP::FUNCTION-TAG
                                                        ,ADP::LET-FILE-PATH)
                  ADP::FINALLY (ADP-PRIVATE:EMPTY-FUNCTION-TAGS))
            (LOOP ADP::FOR ,ADP::TYPE-TAG ADP::ACROSS ADP-PRIVATE:*TYPE-TAGS*
                  DO (ADP-PRIVATE:ADD-TYPE-TAG-PATH ,ADP::TYPE-TAG
                                                    ,ADP::LET-FILE-PATH)
                  ADP::FINALLY (ADP-PRIVATE:EMPTY-TYPE-TAGS))))))))
```

#### ***Function*** ADP:LOAD-DOCUMENTATION-SYSTEM

```Lisp
(DEFUN ADP:LOAD-DOCUMENTATION-SYSTEM
       (ADP::SYSTEM ADP::STYLE ADP::ROOT-PATH &REST ADP::STYLE-ARGS)
  (ADP-PRIVATE:REMOVE-CURRENT-PROCS)
  (LET ((ADP::STYLE-SYSTEM
         (INTERN (CONCATENATE 'STRING "ADP/" (SYMBOL-NAME ADP::STYLE))
                 :KEYWORD)))
    (ASDF/OPERATE:LOAD-SYSTEM ADP::STYLE-SYSTEM :FORCE T))
  (ADP-PRIVATE:CHECK-CURRENT-PROCS)
  (ADP-PRIVATE:CHECK-STYLE-PARAMETERS ADP::STYLE-ARGS)
  (ADP-PRIVATE:REMOVE-CURRENT-DATA)
  (LET ((ADP-PRIVATE:*ADD-DOCUMENTATION* T))
    (ASDF/OPERATE:LOAD-SYSTEM ADP::SYSTEM :FORCE T))
  (LOOP ADP::FOR (ADP::NAME ADP::VALUE) ADP::IN ADP::STYLE-ARGS ADP::BY #'CDDR
        DO (ADP-PRIVATE:SET-PARAMETER-VALUE ADP::NAME ADP::VALUE))
  (LET ((ADP::FIXED-ROOT-PATH
         (MAKE-PATHNAME :HOST (PATHNAME-HOST ADP::ROOT-PATH) :DEVICE
                        (PATHNAME-DEVICE ADP::ROOT-PATH) :DIRECTORY
                        (CONS :ABSOLUTE
                              (CDR (PATHNAME-DIRECTORY ADP::ROOT-PATH))))))
    (ADP-PRIVATE:WRITE-SYSTEM-FILES ADP::FIXED-ROOT-PATH)))
```

