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
```

