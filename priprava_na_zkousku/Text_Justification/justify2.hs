
wordsInOneLine :: [String] -> Int -> [String]
wordsInOneLine [] _ = []
wordsInOneLine (w:ws) maxWidht = go ws [w] (length w)
    where 
        go [] acc _ = reverse acc
        go (x:xs) acc currWidht=
            if currWidht + 1 + length x <= maxWidht
            then go xs (x : acc) (currWidht + 1 + length x)
            else reverse acc

oneLine :: [String] -> Int -> String
oneLine [] _ = ""
oneLine [singleWord] maxWidth = singleWord ++ replicate (maxWidth - length singleWord) ' '
oneLine listString maxWidth = 
    let totalWordLen = sum (map length listString)
        slots = length listString - 1
        totalSpaces = maxWidth - totalWordLen
        baseSpaces = totalSpaces `div` slots
        remainder  = totalSpaces `mod` slots
    in go listString remainder baseSpaces-- Tady začíná výraz po 'in'
  where -- Klíčové slovo 'where' je zarovnané s okrajem nebo mírně odsazené
    go [x] _ _= x
    go (x:xs) rem baseSpaces=
        let bonus = if rem > 0 then 1 else 0
            spacesCount = baseSpaces + bonus -- Tady už baseSpaces MUSÍ vidět
            currentSpaces = replicate spacesCount ' '
        in x ++ currentSpaces ++ go xs (rem - 1) baseSpaces

lastLine :: [String] -> Int -> String
lastLine [] _ = ""
lastLine listString maxWidth =
    let joined = unwords listString          -- Spojí slova klasicky jednou mezerou (např. "toto je konec")
        currentLen = length joined           -- Zjistí reálnou délku tohoto spojeného textu
        missingSpaces = maxWidth - currentLen -- Spočítá, kolik mezer chybí do maxWidth
    in joined ++ replicate missingSpaces ' ' -- Přilepí chybějící mezery na úplný konec


justify :: [String] -> Int -> [String]
justify words maxWidth =
    let wordForRow = wordsInOneLine words maxWidth
        rowLength = length wordForRow
        remainingWords = drop rowLength words
        formattedRow = oneLine wordForRow maxWidth
    in if null remainingWords
        then [lastLine wordForRow maxWidth]
        else formattedRow : justify remainingWords maxWidth



