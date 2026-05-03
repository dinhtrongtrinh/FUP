-- Signatura: Funkce se jmenuje ozviSe. Na vstup vezme typ Zvire a vrátí typ String.

data Zvire = Pes String
            | Kocka String 


ozviSe :: Zvire -> String
-- Samotná funkce:
ozviSe (Pes jmeno) = "Haf! Ja jsem " ++ jmeno
ozviSe (Kocka jmeno) = "Mnau! Ja jsem " ++ jmeno

instance Show Zvire where
    show (Pes jmeno) = "Tady je pes jmenem " ++ jmeno
    show (Kocka jmeno) = "Tady je kocka jmenem " ++ jmeno