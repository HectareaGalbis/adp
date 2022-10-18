# Add Documentation, Please

_Add Documentation, Please_ is a library for literate programming and semi-automatic API generation.

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


