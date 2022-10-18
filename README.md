# Add Documentation, Please

_Add Documentation, Please_ is a library for literate programming and semi-automatic API generation.

Let's try to use some links. For example: [Hyperspec](http://www.lispworks.com/documentation/HyperSpec/Front/)

#### ***Function*** ADP::PRUEBA

```Lisp
(DEFUN ADP::PRUEBA (ADP::X ADP::Y ADP::Z)
  (LET ((ADP::H ADP::X) (ADP::U ADP::Y) (ADP::L ADP::Z))
    (+ ADP::H ADP::Z ADP::L ADP::U)))
```

```
(ADP:DEFUN
  ADP::PRUEBA
  (ADP::X ADP::Y ADP::Z)
  (LET (...)
    (+ ADP::H ADP::Z ADP::L ADP::U)
    ...))

(ADP:DEFUN
  ADP::OTRA-PRUEBA
  (ADP::X)
  (PRINT "Hola"))
```

* Esto es una lista:
  * Esto es una sublista:
    * Otra sublista.
    * 3+4 = 7
  * Final de la sublista
* Final de la lista.

```
(LET ((ADP::BOTTOM 2) (ADP::CEIL 8))
  (LOOP ADP::FOR ADP::I ADP::FROM ADP::BOTTOM ADP::BELOW ADP::CEIL
        DO (FORMAT T "Iteración ~s~%" ADP::I))
  (VALUES 'ADP::HOLA 'ADP::ADIOS))
Iteración 2
Iteración 3
Iteración 4
Iteración 5
Iteración 6
Iteración 7
ADP::HOLA
ADP::ADIOS
```

| Header1 | Header 2 | Header 3 |
| --- | --- | --- |
| 3 | 4 | 5 |
| Hola 7 | Adios | Casa |


We can also write function reference. Above we had defined the function `PRUEBA`.

Let's define the parameter `*EXAMPLE-PARAMETER*` and the type `WEIRD-TYPE`. Look how we can make reference before the definitions have been evaluated.

#### ***Parameter*** ADP::*EXAMPLE-PARAMETER*

```Lisp
(DEFPARAMETER ADP::*EXAMPLE-PARAMETER* 3)
```

#### ***Type*** ADP::WEIRD-TYPE

```Lisp
(DEFTYPE ADP::WEIRD-TYPE () '(CONS STRING INTEGER))
```

```
'ADP-PRIVATE:*ADD-DOCUMENTATION*
ADP-PRIVATE:*ADD-DOCUMENTATION*

(ADP:DEFPARAMETER
  ADP::*EXAMPLE-PARAMETER*
  3)

#(*EXAMPLE-PARAMETER*) 10

(ADP:DEFTYPE
  ADP::WEIRD-TYPE
  NIL
  '(CONS STRING INTEGER))
11
```

Header tags also work! For example: ***Add Documentation, Please***

Lastly, we can make text _italic_, **bold** and ***bold-italic***. Also, we can make inline code: `(+ 3 4)`

