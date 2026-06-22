module Justify (justify) where

-- 1. POMOCNÁ FUNKCE: Vybere slova, která se vejdou na jeden řádek
wordsInOneLine :: [String] -> Int -> [String]
wordsInOneLine [] _ = []
-- První slovo rovnou vložíme do akumulátoru, abychom zamezili prázdným řádkům
wordsInOneLine (w:ws) maxWidth = go ws [w] (length w)
  where
    -- go přijímá: zbylá slova, akumulátor slov (pozpátku), aktuální délku řádku
    go [] acc _ = reverse acc
    go (currWord:rest) acc accLen
        -- Vejde se slovo i s povinnou mezerou? (+1 za mezeru)
        | accLen + 1 + length currWord <= maxWidth = 
            go rest (currWord : acc) (accLen + 1 + length currWord)
        -- Nevejde se -> řádek je plný, vrátíme ho otočený
        | otherwise = reverse acc


-- 2. POMOCNÁ FUNKCE: Vezme seznam slov pro jeden řádek a spravedlivě rozdělí mezery
oneLine :: [String] -> Int -> String
-- CHYTÁK 1: Řádek má jen jedno slovo -> slovo + všechny mezery za něj (pomocí replicate)
oneLine [word] maxWidth = word ++ replicate (maxWidth - length word) ' '
-- HLAVNÍ PŘÍPAD: Více slov na řádku
oneLine words maxWidth =
    let totalWordLen = sum (map length words)
        slots = length words - 1
        totalSpaces = maxWidth - totalWordLen
        baseSpaces = totalSpaces `div` slots -- Celočíselné dělení
        remainder = totalSpaces `mod` slots  -- Zbytek po dělení
    in buildLine words remainder baseSpaces
  where
    -- Pomocná vnitřní funkce pro sestavení řádku (nahrazuje Racket set! slot-index)
    buildLine [lastWord] _ _ = lastWord
    buildLine (w:ws) rem bSpaces =
        let -- Pokud nám ještě zbývá zbytek (remainder), přidáme 1 mezeru navíc
            extraSpace = if rem > 0 then 1 else 0
            currentSpaces = bSpaces + extraSpace
            -- Snížíme remainder pro další rekurzivní volání
            nextRem = if rem > 0 then rem - 1 else 0
        in w ++ replicate currentSpaces ' ' ++ buildLine ws nextRem bSpaces


-- 3. HLAVNÍ FUNKCE: Propojuje vše dohromady do výsledného seznamu zarovnaných řádků
justify :: [String] -> Int -> [String]
justify [] _ = []
justify words maxWidth =
    let wordsForRow = wordsInOneLine words maxWidth   -- 1. Slova pro první řádek
        rowLength = length wordsForRow
        remainingWords = drop rowLength words         -- 2. Ořízneme spotřebovaná slova
        formattedRow = oneLine wordsForRow maxWidth   -- 3. Vyrobíme zarovnaný string
    in formattedRow : justify remainingWords maxWidth -- 4. Spojíme s výsledkem rekurze