<a id="TITLE:ADP-DOCS:REFERENCE"></a>
# Reference

* [adp\:define\-adp\-file](/docs/scribble/reference.md#FUNCTION:ADP-DOCS:TAG20)
* [adp\:define\-adp\-system](/docs/scribble/reference.md#FUNCTION:ADP-DOCS:TAG21)
* [adp\:defmacro](/docs/scribble/reference.md#FUNCTION:ADP-DOCS:TAG15)
* [adp\:defun](/docs/scribble/reference.md#FUNCTION:ADP-DOCS:TAG13)
* [adp\:element\-form](/docs/scribble/reference.md#FUNCTION:ADP-DOCS:TAG22)
* [adp\:element\-value](/docs/scribble/reference.md#FUNCTION:ADP-DOCS:TAG17)
* [adp\:export\-content](/docs/scribble/reference.md#FUNCTION:ADP-DOCS:TAG19)
* [adp\:file\-component](/docs/scribble/reference.md#FUNCTION:ADP-DOCS:TAG18)
* [adp\:file\-elements](/docs/scribble/reference.md#FUNCTION:ADP-DOCS:TAG16)
* [adp\:function\-lambda\-list](/docs/scribble/reference.md#FUNCTION:ADP-DOCS:TAG14)


<a id="FUNCTION:ADP:DEFINE-ADP-FILE"></a>
<a id="FUNCTION:ADP-DOCS:TAG20"></a>
#### Macro: adp:define-adp-file (name &key (type "scrbl") (default-readtable :scribble))

`````text
Defines an adp file.

NAME must be a symbol denoting the class of a file.
The user will need to use a keyword with the same name to specify
the defined file in its documentation system.
TYPE denotes the extension of the file.
DEFAULT-READTABLE denotes the default readtable to use when loading
the scribble file. This can be used if the exporter defines some extension to scribble
like latex math mode.
`````

<a id="FUNCTION:ADP:DEFINE-ADP-SYSTEM"></a>
<a id="FUNCTION:ADP-DOCS:TAG21"></a>
#### Macro: adp:define-adp-system (name)

`````text
Defines an adp system class.

NAME must be a symbol denoting the class of a system.
The user will need to use a keyword with the same name to specify
the defined class in its documentation system.
`````

<a id="FUNCTION:ADP:DEFMACRO"></a>
<a id="FUNCTION:ADP-DOCS:TAG15"></a>
#### Macro: adp:defmacro (name (&rest args) &body body)

`````text
Same as ADP:DEFUN but arguments are not evaluated.
`````

<a id="FUNCTION:ADP:DEFUN"></a>
<a id="FUNCTION:ADP-DOCS:TAG13"></a>
#### Macro: adp:defun (name (&rest args) &body body)

`````text
Racket-like version of DEFUN.

This macro has the following syntax:

(defun name racket-lambda-list [[declaration* | documentation]] form*)

  racket-lambda-list ::= (arg*)
                       | (arg* . rest-symbol)
                       | (arg* &rest rest-symbol)
                 arg ::= symbol
                       | (symbol default-form [supplied-symbol])
                       | keyword symbol
                       | keyword (symbol default-form [supplied-symbol])

The following explanation was taken and modified from: https://docs.racket-lang.org/reference/lambda.html

Regarding the racket-lambda-list and considering only the first case of arg, DEFUN has one of the
following three forms:

  - (arg*)

    The function accepts as many non-keyword argument values as the number of symbols.
    Each symbol is associated with an argument value by position.

  - (arg* . rest-symbol)
  - (arg* &rest rest-symbol)

    The function accepts any number of non-keyword arguments greater or equal to the number of symbols.
    When the function is applied, the symbols are associated with argument values by position,
    and all leftover arguments are placed into a list that is associated to rest-symbol.

More generally, an arg can include a keyword and/or default value. Thus, the three cases above are
more completely specified as follows:

  - symbol

    Adds one to both the minimum and maximum number of non-keyword arguments accepted by the function.
    The symbol is associated with an actual argument by position.

  - (symbol default-form [supplied-symbol])

    Adds one to the maximum number of non-keyword arguments accepted by the procedure.
    The symbol is associated with an actual argument by position, and if no such argument is provided,
    the default-form is evaluated to produce a value associated with symbol. If supplied-symbol is
    specified, it is associated with t or nil indicating if the argument is provided.
    No arg with a default-expr can appear before a symbol without a default-form and without a keyword.

  - keyword symbol

    The function requires a keyword-based argument using keyword. The symbol is associated
    with a keyword-based actual argument using keyword.

  - keyword (symbol default-form [supplied-symbol])

    The function accepts a keyword-based argument using keyword. The symbol is associated
    with a keyword-based actual argument using keyword, if supplied in an application; otherwise,
    the default-expr is evaluated to obtain a value to associate with id. If supplied-symbol is
    specified, it is associated with t or nil indicating if the argument is provided.
`````

<a id="FUNCTION:ADP:ELEMENT-FORM"></a>
<a id="FUNCTION:ADP-DOCS:TAG22"></a>
#### Function: adp:element-form (scribble-element)

`````text
Returns the form of an element.

This form is the lisp form equivalent to a scribble one that the programmer wrote
within a scribble file. This form returned a value that can be retrieved with ADP:ELEMENT-VALUE.
`````

<a id="FUNCTION:ADP:ELEMENT-VALUE"></a>
<a id="FUNCTION:ADP-DOCS:TAG17"></a>
#### Function: adp:element-value (scribble-element)

`````text
Returns the value of an element.

This value is what a certain form returned within a scribble file.
The form can be retrieved with ADP:ELEMENT-FORM.
`````

<a id="FUNCTION:ADP:EXPORT-CONTENT"></a>
<a id="FUNCTION:ADP-DOCS:TAG19"></a>
#### Generic function: adp:export-content (system files)

`````text
Exports the elements gathered by ADP.

This function must be implemented by the exporter and is called after all files are loaded.
SYSTEM argument must be specialized with a class defined with DEFINE-ADP-SYSTEM.
FILES is a list of ADP::SCRIBBLE-FILE objects. These objects has two attributes that can be retrieved
using ADP:FILE-COMPONENT and ADP:FILE-ELEMENTS. The former returns the ASDF component that represents
the file. The exporter must use the ASDF interface to retrieve information about this file like
its pathname. The latter is a list of ADP::SCRIBBLE-ELEMENT objects. Each object has two attributes
as well that can be retrieved with ADP:ELEMENT-VALUE and ADP:ELEMENT-FORM. The former returns
the value of the element. This value is what the exporter must print. The former is the form
that has generated the value. This can be useful for error messages.

Regarding the type of each file, they are specifically of type a class defined with ADP:DEFINE-ADP-FILE.
In fact, every file type defined with ADP:DEFINE-ADP-FILE has as direct superclass the
 type ADP::SCRIBBLE-FILE.

All files are in the same order as they were loaded. The same goes to elements within a file.
`````

<a id="FUNCTION:ADP:FILE-COMPONENT"></a>
<a id="FUNCTION:ADP-DOCS:TAG18"></a>
#### Function: adp:file-component (&rest args-sym)

`````text
Returns the ASDF component of a scribble file.
`````

<a id="FUNCTION:ADP:FILE-ELEMENTS"></a>
<a id="FUNCTION:ADP-DOCS:TAG16"></a>
#### Function: adp:file-elements (&rest args-sym)

`````text
Returns all the elements of a scribble file.
`````

<a id="FUNCTION:ADP:FUNCTION-LAMBDA-LIST"></a>
<a id="FUNCTION:ADP-DOCS:TAG14"></a>
#### Function: adp:function-lambda-list (symbol)

`````text
Retrieves the lambda list of an adp function or macro whose name is SYMBOL.
A second value is returned specifing if SYMBOL does or does not denote an adp function or macro.
`````