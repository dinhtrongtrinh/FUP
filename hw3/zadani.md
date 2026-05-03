Evaluátor λ-kalkulu

Cílem tohoto domácího úkolu je implementovat evaluátor λ-výrazů v Haskellu, který redukuje zadaný λ-výraz do jeho normální formy podle strategie vyhodnocování normálního pořadí (normal order evaluation strategy). Jako vedlejší efekt vám tento domácí úkol také pomůže upevnit vaše znalosti λ-kalkulu. λ-výrazy budou reprezentovány jako instance vhodně definovaného datového typu v Haskellu. Procvičíte si tedy práci s takovými typy, zejména jak z nich vytvořit instanci třídy Show a jak s nimi používat porovnávání se vzorem (pattern matching).

Interpret by měl být implementován jako modul Haskellu s názvem Hw3. Všimněte si velkého H. Názvy modulů v Haskellu musí začínat velkými písmeny. Jelikož soubor obsahující kód modulu musí mít stejný název jako je název modulu, veškerý váš kód musí být v jediném souboru s názvem Hw3.hs. Váš soubor Hw3.hs musí začínat následujícími řádky:
Haskell

module Hw3 where

type Symbol = String

data Expr = Var Symbol
          | App Expr Expr
          | Lambda Symbol Expr deriving Eq

První řádek definuje modul s názvem Hw3. Názvy proměnných v λ-termech jsou reprezentovány instancemi typu String.
Druhý řádek pouze zavádí nový název Symbol pro typ String, abychom vizuálně odlišili, kdy pracujeme s názvy proměnných. Poslední řádek definuje datový typ reprezentující λ-výrazy. Existují tři datové konstruktory: jeden pro proměnnou, jeden pro aplikaci a jeden pro λ-abstrakci. Klauzule deriving Eq činí tento datový typ instancí třídy Eq, takže je možné kontrolovat, zda jsou dva λ-výrazy shodné, či nikoliv.

Specifikace evaluátoru

Nejprve vytvořte z datového typu Expr instanci třídy Show, aby mohl ghci zobrazovat λ-výrazy. Musíte tedy definovat funkci show, která převádí λ-výrazy na řetězec. Jakmile do příkazového řádku ghci zadáte následující výrazy, mělo by se to chovat následovně:
Haskell

λ> Var "x"
x

λ> App (Var "x") (Var "y")
(x y)

λ> Lambda "x" (Var "x")
(\x.x)

λ> App (Lambda "x" (Var "x")) (Var "y")
((\x.x) y)

λ> Lambda "s" (Lambda "z" (App (Var "s") (App (Var "s") (Var "z"))))
(\s.(\z.(s (s z))))

Symbol λ se tedy zobrazuje jako \. Proměnné se zobrazují jako jejich názvy. Aplikace se zobrazují jako (e1 e2) s mezerou oddělující výrazy e1, e2 a λ-abstrakce jako (\x.e).

V dalším kroku je vaším úkolem implementovat funkci eval :: Expr -> Expr.
Tato funkce pro zadaný vstupní λ-výraz vrátí jeho normální formu, pokud existuje. Navíc se musí řídit strategií vyhodnocování normálního pořadí. Abyste tedy provedli jeden krok β-redukce, musíte identifikovat nejlevější vnější redex a zredukovat jej. Poté tento proces opakujete, dokud neexistuje žádný redex.

Pro redukci redexu musíte implementovat mechanismus substituce, který vám umožní nahradit λ-výraz za všechny volné výskyty proměnné v jiném λ-výrazu. Tento mechanismus se musí vypořádat s konflikty jmen, jak víte z přednášky o λ-kalkulu. Jednou z možností, jak to bezpečně udělat, je následující rekurzivní definice:
x[x:=e]y[x:=e](e1​,e2​)[x:=e](λx.e′)[x:=e](λy.e′)[x:=e](λy.e′)[x:=e]​=e=ypokud y=x=(e1​[x:=e],e2​[x:=e])=(λx.e′)=(λy.e′[x:=e])pokud y=x a y nenıˊ volnaˊ v e=(λz.e′[y:=z][x:=e])pokud y=x a y je volnaˊ v e;z je cˇerstvaˊ promeˇnnaˊ​

Poslední případ řeší konflikty jmen, tj. používá α-konverzi. Jelikož je v tomto případě y volná v e, mohla by se po substituci v e′ stát vázanou. Proto přejmenujeme y v λy.e′ na novou čerstvou proměnnou z, tj. vypočítáme e′[y:=z] a poté nahradíme proměnnou v λ-abstrakci za λz.e′[y:=z]. Pak můžeme pokračovat a rekurzivně nahradit e za x v e′[y:=z].

Ve svém kódu se tedy řiďte výše uvedenou rekurzivní definicí. Váš kód musí v případě potřeby generovat nové (čerstvé) symboly. Čerstvé symboly mohou být označeny např. "a0", "a1", "a2", ...
Ke vygenerování čerstvého symbolu stačí inkrementovat číslo naposledy použitého symbolu. Toto číslo je stavem vašeho výpočtu. Jelikož je Haskell čistě funkcionální jazyk, nemůžete mít stav a aktualizovat ho, když je to potřeba. Místo toho musíte tento index zahrnout do signatur vašich funkcí podobně, jako jsme to udělali ve cvičení z Laboratoře 9, kde jsme implementovali funkci indexující uzly binárního stromu.

Testovací případy

Níže naleznete několik veřejných testovacích případů. Pokud je λ-výraz již ve své normální formě, funkce eval pouze vrátí svůj vstup.
Haskell

λ> eval (App (Var "x") (Var "y"))
(x y)
λ> eval (Lambda "x" (Var "x"))
(\x.x)

Pokud je redukovatelný, vrátí jeho normální formu. Například (λx.x)y se zredukuje na y:
Haskell

λ> eval (App (Lambda "x" (Var "x")) (Var "y"))
y

Uvažujme výraz (λx.(λy.(x,y)))y. Redukce dává (λy.(x,y))[x:=y]. Podle definice substituce musíme přejmenovat y na a0 a poté substituovat y za volné výskyty x, tj.
(λy.(x,y))[x:=y]​=(λa0.(x,y)[y:=a0][x:=y])=(λa0.(x,a0)[x:=y])=(λa0.(y,a0))​
Haskell

λ> eval (App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Var "y"))
(\a0.(y a0))

Uvažujme λ-výraz (λx.(λy.y))y. Redukce vede na (λy.y)[x:=y].
Protože y je volná, zavedeme podle definice čerstvou proměnnou a0 místo y, čímž získáme λa0.y[y:=a0]=λa0.a0. Poté vypočítáme λa0.a0[x:=y]=λa0.a0.
Haskell

λ> eval (App (Lambda "x" (Lambda "y" (Var "y"))) (Var "y"))
(\a0.a0)

Uvažujme λ-výraz (λx.(λy.(λz.((xy)z))))(y,z).
Protože y a z jsou volné v (y,z), musíme je přejmenovat v λx.(λy.(λz.((xy)z))), čímž získáme λa0.(λa1.(((y,z),a0),a1)).
Haskell

ex = App (Lambda "x"
           (Lambda "y"
             (Lambda "z" (App (App (Var "x") (Var "y")) (Var "z")))))
         (App (Var "y") (Var "z"))

λ> eval ex
(\a0.(\a1.(((y z) a0) a1)))

Chcete-li napsat složitější testovací případy, můžete definovat podvýrazy a poté z nich skládat ty složitější. Například pro otestování, že S1 se zredukuje na 2:
Haskell

one = Lambda "s" (Lambda "z" (App (Var "s") (Var "z")))
suc = Lambda "w"
       (Lambda "y"
         (Lambda "x"
           (App (Var "y")
                (App (App (Var "w") (Var "y"))
                     (Var "x")))))

λ> eval (App suc one)
(\y.(\x.(y (y x))))

Ještě jeden testovací případ využívající definici one=λs.(λz.(s,z)). Uvažujme λ-výraz
(λz.one)(s,z)=(λz.(λs.(λz.(s,z))))(s,z)

Ten se zredukuje na λa0.(λz.(a0,z)). Protože s je volná v (s,z), vázaný výskyt s v one je přejmenován. Ale z přejmenována není, protože podle definice substituce máme λz.(a0,z)[z:=(s,z)]=λz.(a0,z).
Haskell

λ> eval (App (Lambda "z" one) (App (Var "s") (Var "z")))
(\a0.(\z.(a0 z)))