# Lambda Interpreter in Haskell

## Descriere generală

Acest proiect reprezintă un interpretator pentru expresii lambda în Haskell. Scopul este de a implementa evaluarea expresiilor lambda folosind diferite strategii de reducere, parsarea expresiilor sub formă de string-uri, lucrul cu macro-uri, și definirea unei biblioteci standard.

## Structură cod

### Tipuri de date

* `Lambda`: definirea expresiilor lambda (`Var`, `App`, `Abs`, `Macro`).
* `Line`: definește o linie de cod (evaluare sau binding).
* `Parser a`: tipul de date pentru parsarea expresiilor.

### Modul `Lambda.hs`

#### Funcții auxiliare

* `vars :: Lambda -> [String]` – returnează toate variabilele dintr-o expresie.
* `freeVars :: Lambda -> [String]` – returnează variabilele libere.
* `newVar :: [String] -> String` – returnează prima variabilă disponibilă lexicografic.
* `isNormalForm :: Lambda -> Bool` – verifică dacă expresia este în formă normală.

#### Reducere

* `reduce :: String -> Lambda -> Lambda -> Lambda` – face substituția unei variabile cu o expresie, având grijă la variable capture.
* `normalStep :: Lambda -> Maybe Lambda` – aplică un pas de reducere normală.
* `applicativeStep :: Lambda -> Maybe Lambda` – aplică un pas de reducere aplicativă.
* `simplify :: (Lambda -> Maybe Lambda) -> Lambda -> [Lambda]` – aplică reducerile până la formă normală.

### Modul `Parser.hs`

* `parseLambda :: Parser Lambda` – parsează o expresie lambda simplă sau compusă.
* `parseLine :: String -> Either String Line` – parsează o linie de cod (evaluare sau binding).

### Modul `Macro.hs`

* `simplifyCtx :: [(String, Lambda)] -> (Lambda -> Maybe Lambda) -> Lambda -> Either String [Lambda]` – evaluează o expresie cu macro-uri într-un context.

### Modul `Default.hs`

* Conține definiții lambda pentru:

  * **Booleeni:** `TRUE`, `FALSE`, `AND`, `OR`, `NOT`, `XOR`
  * **Perechi:** `PAIR`, `FIRST`, `SECOND`
  * **Numere naturale:** `N0`, `N1`, `N2`, `SUCC`, `PRED`, `ADD`, `SUB`, `MULT`

### Modul `Main.hs`

* Implementare REPL:

  * `:q` – ieșire
  * `:r` – resetare context
  * `:ctx` – afișare context curent

## Cum rulezi proiectul

### REPL

```bash
runhaskell Main.hs
```

### Testare

```bash
runhaskell test.hs
```

Pentru a testa doar o parte:

```bash
runhaskell test.hs lambda
runhaskell test.hs parser
runhaskell test.hs binding
runhaskell test.hs default
```

## Punctaj

* Total: **150p**, scalat la **1p** din nota finală.
* Bonus: toate cele 3 teme completate => scutire de examen cu nota maximă.

## Recomandări

* Folosește `do`-notation și monade pentru lucrul cu `Maybe` și `Either`.
* Nu hardcodați expresii sau rezultate.
* Nu plagiați – riscați depunctare sau invalidarea temei.

---

Tema implementă un subset semnificativ dintr-un limbaj funcțional și oferă o experiență practică excelentă cu parsing, evaluare lazy, context lexical și construirea unui mini-limbaj funcțional.
