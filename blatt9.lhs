\documentclass[12pt]{scrartcl}

%include polycode.fmt
\usepackage[ngerman]{babel}
\usepackage[utf8]{inputenc}
\usepackage{mathtools}
\usepackage{hyperref}

\newcommand{\code}[1]{\texttt{ #1 }}

\begin{document}

\section*{Aufgabe 71}

Der LBA den wir konstruieren wollen wird den Sortieralgorithmus
``Bubblesort'' implementieren.
Sei der Automat $M = \{Z, \Sigma, \Gamma, \delta, q_0, E \}$, wobei
\begin{align*}
  Z &= \{ I, (S,0), (S,1), C, (U,0), (U,1), R, F \}, \\
  \Sigma &= \{0,1,\hat{0}, \hat{1} \}, \\
  \Gamma &= \Sigma \cup \{\sqcup, \dot{0} ,\dot{1}\}, \\
  q_0 &= I, \\
  E &= \{ F \}.
\end{align*}

Die Übergangsfunktion $\delta$ sei durch folgende Tabelle gegeben.
\begin{equation*}
  \delta = \left\{
  \begin{matrix*}[l]
    I, 0 & \mapsto & (S,0), \dot0, R \\
    I, 1 & \mapsto & (S,1), \dot1, R \\
    I, \hat0 & \mapsto & E, \hat0, N \\
    I, \hat1 & \mapsto & E, \hat1, N \\
    (S,0), 0 & \mapsto & (S,0), 0, R \\
    (S,0), 1 & \mapsto & (S,1), 1, R \\
    (S,0), \hat0 & \mapsto & E, \hat0, N \\
    (S,0), \hat1 & \mapsto & E, \hat1, N \\
    (S,1), 0 & \mapsto & C, 1, L \\
    (S,1), 1 & \mapsto & (S,1), 1, R \\
    (S,1), \hat0 & \mapsto & C, \hat1, L \\
    (S,1), \hat1 & \mapsto & E, \hat1, N \\
    C, 1 & \mapsto & (U, 0), 0, R \\
    C, \dot1 & \mapsto & (U, 0), \dot0, R \\
    (U,0), 0 & \mapsto & (U,0), 0, R \\
    (U,0), 1 & \mapsto & (U,1), 1, R \\
    (U,0), \hat0 & \mapsto & E, \hat0, N \\
    (U,0), \hat1 & \mapsto & E, \hat1, N \\
    (U,1), 0 & \mapsto & C, 1, L \\
    (U,1), 1 & \mapsto & (U,1), 1, R \\
    (U,1), \hat0 & \mapsto & C, \hat1, L \\
    (U,1), \hat1 & \mapsto & R, \hat1, L \\
    R, 0 & \mapsto & R, 0, L \\
    R, 1 & \mapsto & R, 1, L \\
    R, \dot0 & \mapsto & (S,0), \dot0, R \\
    R, \dot1 & \mapsto & (S,1), \dot1, R \\
  \end{matrix*}
  \right.
\end{equation*}

Der Automat terminiert im ersten Schritt, falls er ein Wort der Länge
1 liest.  Im allgemeinen wird das erste Zeichen markiert und dann das
Eingabeband von links nach rechts gelesen.  Der Automat ``merkt'' sich
das zuletzt gelesene Zeichen und ob das Eingabeband bereits sortiert
ist.  Trift der Automat auf eine Stelle an der zuvor eine 1 gelesen
wurde dann eine 0 auf dem Eingabeband erscheint, werden die 1 und die
0 ``getauscht'' und der Automat betrachtet die Eingabe als unsortiert.
Wenn der Automat das letzte Zeichen liest und das Band sortiert ist,
geht der Automat in den Endzustand, ansonsten wird der Lesekopf an den
Anfang des Eingabebandes bewegt und die Prozedur wiederholt.

\pagebreak[4]

\section*{Aufgabe 72}

Sei der DLBA $M = \{ Z, \Sigma, \Gamma, \delta, q_0, E \}$.  Dabei sind

\begin{align*}
  Z &= \{I,A,B,C,F\} \\
  \Sigma &= \{a,b,c,\hat{a},\hat{b},\hat{c}\} \\
  \Gamma &= \Sigma \cup \{\sqcup, x, \dot{x}\} \\
  q_0 &= I \\
  E = F
\end{align*}

Sei $\delta$ durch folgende Tabelle gegeben.
\begin{equation*}
  \delta = \left\{
  \begin{matrix*}[l]
    I, a & B, \dot{x}, R \\
    A, a & B, x, R \\
    A, b & A, b, L \\
    A, c & A, c, L \\
    A, x & A, x, L \\
    A, \dot{x} & X, \dot{x}, R \\
    B, a & B, a, R \\
    B, b & C, x, R \\
    B, x & B, x, R \\
    C, b & C, b, R \\
    C, c & A, x, L \\
    C, \hat{c} & A, \hat{x}, L \\
    X, x & X, x, R \\
    X, \hat{x} & F, \hat{x}, N
  \end{matrix*} \right.
\end{equation*}

Der Automat ist so konstruiert, dass er sukzessive alle Vorkommen von
$a$, $b$ und $c$ durch $x$ ersetzt.  Wenn der Automat gestartet wird
markiert er das erste Zeichen auf dem Band, dass ein $a$ sein muss,
durch ein $\dot{x}$.  Dann sucht der Automat nach dem nächsten $b$ auf
dem Eingabeband.  Findet der Automat vorher ein $c$, bricht die
Berechnung ab und das Wort wird nicht erkannt.  Wenn ein $b$ gefunden
wurde, dann sucht der Automat nach einem $c$ und bricht ab, falls
``unterwegs'' ein $a$ gefunden wurde.  Wenn dann ein $c$ gefunden
wurde, ``läuft'' der Lesekopf nach links und sucht nach dem nächsten
$a$.  Wird statt dessen das markierte $\dot{x}$ gefunden, d.h. es gibt
keine $a$ mehr auf dem Band, läuft der Lesekopf nach rechts bis zum
Ende und ``überprüft'' ob auch alle $a$, $b$ und $c$ mit $x$
überschrieben wurden.  Ist das der Fall, geht der Automat in seinen
Endzustand über.

\pagebreak[4]

\section*{Aufgabe 73}

Sei die gesuchte Grammatik $G(L)$ gegeben durch folgende Regeln, wobei
$a$ und $b$ Terminale und alle anderen Symbole Variablen sind.  $S$
ist das Startsymbol.

\begin{align}
  S \mapsto & \dot{X}X, \dot{Y}Y \\
  \dot{X} \mapsto & \dot{X}AV, \dot{X}BW, a \\
  \dot{Y} \mapsto & \dot{Y}AV, \dot{Y}BW, b \\
  VA \mapsto & AV \\
  VB \mapsto & BV \\
  VX \mapsto & XV \\
  WA \mapsto & AW \\
  WB \mapsto & BW \\
  WX \mapsto & XW \\
  XV \mapsto & aX \\
  XW \mapsto & bX \\
  YV \mapsto & aY \\
  YW \mapsto & bY \\
  A \mapsto & a \\
  B \mapsto & b \\
  X \mapsto & a \\
  Y \mapsto & b
\end{align}

Die Produktion des Wortes läuft in drei ``Phasen'' ab.  Zu erst wird
ein beliebiges Wort bestehend aus $A$ und $B$ erzeugt.  Zu jedem $A$
oder $B$ wird eine entsprechende Variable erzeugt, wobei $V$ dem $A$
entspricht und $W$ dem $B$ entspricht.  Die $V$ und $W$ sind in der
selben Reihenfolge wie die $A$ und $B$ angeordnet.  Es gibt in der
gegebenen Grammatik keine Regel, die diese Reihenfolge verändern kann.
In der zweiten ``Phase'' werden alle $V$ und $W$ nach hinten
verschoben, das heißt hinter das zweite $X$ respektive $Y$, dass zu
Beginn aus dem Startsymbol erzeugt wurde.  In ``Phase'' 3 werden dann
alle Variablen in Terminale umgewandelt.

\pagebreak[4]

\section*{Aufgabe 80}

\subsection*{a)}

\begin{description}
\item[$T^6_0$] $=$ $\{$ $A\,\}$
\item[$T^6_1$] $=$ $\{$ $A$, $BabC\,\}$
\item[$T^6_2$] $=$ $\{$ $A$, $aBabC$, $BaBbCa$, $Babb$, $BabC$,
  $BabCb$, $CbabC\,\}$
\item[$T^6_3$] $=$ $\{$ $A$, $aaBabC$, $aBabb$, $aBabC$, $aBabCb$,
  $aCbabC$, $BaBbba$, $BaBbCa$, $Babb$, $Babbb$, $BabC$, $BabCb$,
  $BabCbb$, $bbabC$, $Cbabb$, $CbabC$, $CbabCb\,\}$
\item[$T^6_4$] $=$ $\{$ $A$, $aaBabb$, $aaBabC$, $aBabb$, $aBabbb$,
  $aBabC$, $aBabCb$, $abbabC$, $aCbabb$, $aCbabC$, $BaBbba$, $BaBbCa$,
  $Babb$, $Babbb$, $Babbbb$, $BabC$, $BabCb$, $BabCbb$, $bbabb$,
  $bbabC$, $bbabCb$, $Cbabb$, $Cbabbb$, $CbabC$, $CbabCb\,\}$
\item[$T^6_5$] $=$ $\{$ $A$, $aaBabb$, $aaBabC$, $aBabb$, $aBabbb$,
  $aBabC$, $aBabCb$, $abbabb$, $abbabC$, $aCbabb$, $aCbabC$, $BaBbba$,
  $BaBbCa$, $Babb$, $Babbb$, $Babbbb$, $BabC$, $BabCb$, $BabCbb$,
  $bbabb$, $bbabbb$, $bbabC$, $bbabCb$, $Cbabb$, $Cbabbb$, $CbabC$,
  $CbabCb\,\}$
\item[$T^6_6$] $=$ $\{$ $A$, $aaBabb$, $aaBabC$, $aBabb$, $aBabbb$,
  $aBabC$, $aBabCb$, $abbabb$, $abbabC$, $aCbabb$, $aCbabC$, $BaBbba$,
  $BaBbCa$, $Babb$, $Babbb$, $Babbbb$, $BabC$, $BabCb$, $BabCbb$,
  $bbabb$, $bbabbb$, $bbabC$, $bbabCb$, $Cbabb$, $Cbabbb$, $CbabC$,
  $CbabCb\,\}$
\item[$T^6_7$] $=$ $T^6_6$
\end{description}

Ab $T^6_5$ kommen keine weiteren Elemente zu den folgenden Listen mehr
hinzu.

Für den Fall, dass es den Korrektor oder die Korrektorin interessiert,
wie diese Ergebnisse zu stande gekommen sind, habe ich das ``literate
Haskell'', dass ich zur Lösung der Aufgabe geschrieben habe,
beigefügt.  Viel Spaß beim Lesen\footnote{URL:
  \url{https://github.com/seppeljordan/eti/blob/master/blatt9.lhs}}.

\subsubsection*{Haskellprogramm\footnote{
    Wir verzichten auf \code{main}-Funktion, d.h. dass der Code mithilfe
    einer REPL, wie zum Beispiel ``ghci'' aufgerufen werden sollte.
  }
  zur Lösung der Aufgabe}


Da wir es mit nicht-Determinismus zutun haben, werden wir wohl die
Implementation von Listen als \code{Applicative} nutzen.  Außerdem
werden wir einige Funktionen aus der \code{Data.List}-Bibliothek
nutzen.

\begin{code}
import Data.List
import Control.Applicative
\end{code}

Wir repräsentieren alle Symbole als Char, egal ob Terminal oder
Variable.

\begin{code}
type Symbol = Char
\end{code}

Eine Regel ist die Zuordung einer Symbolfolge zu einer oder mehreren
Symbolfolgen.

\begin{code}
type Rule = ([Symbol], [[Symbol]])
\end{code}

Eine Grammatik ist als Tupel definiert.

\begin{code}
type Grammar = ([Symbol], [Symbol], [Rule], Symbol)
grammarStart (_,_,_,q0) = q0
grammarRules (_,_,rs,_) = rs
grammarVars (vs,_,_,_) = vs
grammarTerms (_,ts,_,_) = ts
\end{code}

Die Grammatik aus der Aufgabenstellung lässt sich also
folgendermaßen darstellen.

\begin{code}
g = ( "ABCD"
    , "ab"
    , [ ("A", ["BabC"]) -- 1
      , ("Ba", ["Cba", "aBa"]) -- 2,3
      , ("bCB", ["aCb"]) -- 4
      , ("C", ["b"]) -- 5
      , ("bC", ["BbCa", "bCb"]) --6,7
      ]
    , 'A'
    )
\end{code}

Die Anwendung eines Regelkatalogs auf ein Teilwort liefert eine Liste
mit Ableitungen.  Dabei definieren wir ersteinmal, was es bedeutet,
etwas abzuleiten, dass genau so auf der linken Seite der Regeln
vorkommt.

\begin{code}
rewrite :: [Rule] -> [Symbol] -> [[Symbol]]
rewrite rules exp =
    case lookup exp rules of
      Nothing -> []
      Just rewritten -> rewritten
\end{code}

Wir wollen definieren, was es heißt, dass alle Ableitungen aller
Präfixe gebildet werden.

\begin{code}
derivePrefixes :: [Rule] -> [Symbol] -> [[Symbol]]
derivePrefixes rules word =
    zip (inits word) (tails word) >>=
    (\(prefix, suffix) -> (++suffix) <$> rewrite rules prefix)
\end{code}

Wir wollen nun definieren, wie eine Ableitung gebildet wird.  Eine
Ableitung ist eine Funktion, die eine Satzform annimmt und eine Liste
möglicher Ableitungen zurück gibt.

\begin{code}
deriveWord :: [Rule] -> [Symbol] -> [[Symbol]]
deriveWord rules word =
    zip (inits word) (tails word) >>= \(prefix, suffix) ->
    (prefix++) <$> derivePrefixes rules suffix
\end{code}

Nun wollen wir definieren, was es heißt, dass $n$ mal abgeleitet wird.

\begin{code}
deriveN :: Int -> [Rule] -> [Symbol] -> [[Symbol]]
deriveN n rules word =
    derivation word
    where derivation w =
              foldl
              (\accu mf ->
                   accu >>= mf)
              (return w)
              (take n (repeat (deriveWord rules)))
\end{code}

Wir müssen noch die Sortierung der Worte definieren.  Dafür
definieren wir erstmal die Reihenfolge pro Zeichen mithilfe einer
Liste.

\begin{code}
order :: [Symbol]
order = "AaBbCD"
\end{code}

Um bei einem Vergleich zu entscheiden, welches Symbol zuerst
erscheint, werden die Indizes der Symbole in der Liste verglichen.
Die Funktion ist nicht total und gibt einen Fehler aus, wenn Symbole
verwendet werden, die nicht in der Liste \code{order} vorkommen.

\begin{code}
compSymb :: Symbol -> Symbol -> Ordering
compSymb a b =
    case (findIndex (==a) order, findIndex (==b) order) of
      (Just n, Just m) -> compare n m
      (Nothing, _) -> error ([a] ++ " is not in ordering")
      (_, Nothing) -> error ([b] ++ " is not in ordering")
\end{code}

Nun definieren wir, was es heißt, Satzformen zu vergleichen.

\begin{code}
compWord :: [Symbol] -> [Symbol] -> Ordering
compWord x y
    | foldOrderlist compList == EQ &&
      length x /= length y =
          compare (length x) (length y)
    | otherwise =
        foldOrderlist compList
    where compList = zipWith compSymb x y
          foldOrderlist l =
              foldl
              (\predOrdering ordering ->
                   case predOrdering of
                     EQ -> ordering
                     _ -> predOrdering)
              EQ
              l
\end{code}

Kommen wir nun zur Lösung des eigentlichen Problems.  Wir definieren
die Funktion ``t'' so wie $T_m^m$ in der Aufgabenstellung.

\begin{code}
t :: Grammar -> Int -> Int -> [[Symbol]]
t gram m n =
    (map head.group.sortBy compWord.filter (\l -> length l <= n).concat)$
    map
    (\i -> deriveN i rules [startingSymbol])
    [0..m]
    where rules = grammarRules gram
          startingSymbol = grammarStart gram
\end{code}

Jetzt können wir die Funktion \code{t} nutzen um netten
\LaTeX{}-Output zu erzeugen.

\begin{code}
showAsTex :: IO ()
showAsTex =
    mapM_ putSetM [0..6]
    where putSetM m =
              putStrLn $
              "\\item[$T^6_"++show m++"$] $=$ "++showList' (t g m 6)
          showList [] = ""
          showList (x:[]) = "$"++x++"\\,\\}$"
          showList (x:xs) = "$"++x++"$, "++showList xs
          showList' [] = "$\\emptyset$"
          showList' xs = "$\\{$ "++showList xs
\end{code}

Die Funktion \code{showAsTex} schreibt nun die ersten 6 $T_m^6$ auf
die Standardausgabe.

\subsection*{b)}

Um das Wortproblem für kontextsensitive Grammatiken zu lösen, kann
folgender Algorithmus verwendet werden.  Betrachte Wort $w$ und
Grammatik $G$.  Die Frage ist, ob $w$ in $L(G)$ ist.  Sei $n$ die
Länge von $w$.

\begin{itemize}
\item Ermittle $T_0^n$.
\item Setze $i=0$
\item Erhöhe $i$ um 1
\item Ermittle $T_i^n$ und speichere das Ergebnis.
\item Gehe 2 Schritte zurück, falls $T_i^n \neq T_{i-1}^{n}$
\item Falls $w \in T^n_i$, beende mit Rückgabewert 1, sonst beende mit
  Rückgabewert 0
\end{itemize}

$w$ ist in $L(G)$, falls der Algorithmus mit $1$ terminiert, sonst ist
das Gegenteil der Fall.

\subsubsection*{Implementation in Haskell}

Auch hier wieder für den\_die interessierten Korrektor\_in die
Implementation in Haskell.

Zu aller erst implementieren wir eine Funktion, die die oben
beschriebene Schleife implementiert. Dazu nutzen wir die ``layzness''
von Haskell, indem wir eine unendlichlange Liste von $T$s erzeugen.
Die $T$s werden dann mit ihren Vorgängern verglichen.  Zurückgegeben
wird das erste Elemen für das gilt, dass die Länge der Liste von
möglichen Ableitungen gleichgroß wie die Länge der Vorgängerliste ist.

\begin{code}
t_inf :: Grammar -> Int -> [[Symbol]]
t_inf g n =
    (fst.head) $ dropWhile wordAdded t_mPairs
    where t_ms = map (\m -> t g m n) [1..]
          t_mPairs = zip t_ms (tail t_ms)
          wordAdded (xs,ys) = length xs < length ys
\end{code}

Nun implementieren wir, die Funktion, die Überprüft, dass ein Wort in
der Sprache der erzeugenden Grammatik ist.

\begin{code}
isInL :: [Symbol] -> Grammar -> Bool
isInL w g =
    w `elem` t_inf g (length w)
\end{code}
      
\end{document}
