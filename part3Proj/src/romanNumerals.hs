import qualified Data.Text as T
import qualified Data.Maybe as M

----------------
-- RN Rewrite System
----------------
--basic grammar of roman numerals
order :: T.Text -> T.Text
order s
    | let ind = M.fromJust (T.findIndex ('I' ==) s), let (bef, aft) = T.splitAt ind s, T.isInfixOf (T.singleton 'I') s = T.snoc (order (T.append bef (T.tail aft))) 'I'
    | let ind = M.fromJust (T.findIndex ('V' ==) s), let (bef, aft) = T.splitAt ind s, T.isInfixOf (T.singleton 'V') s = T.snoc (order (T.append bef (T.tail aft))) 'V'
    | let ind = M.fromJust (T.findIndex ('X' ==) s), let (bef, aft) = T.splitAt ind s, T.isInfixOf (T.singleton 'X') s = T.snoc (order (T.append bef (T.tail aft))) 'X'
    | let ind = M.fromJust (T.findIndex ('L' ==) s), let (bef, aft) = T.splitAt ind s, T.isInfixOf (T.singleton 'L') s = T.snoc (order (T.append bef (T.tail aft))) 'L'
    | let ind = M.fromJust (T.findIndex ('C' ==) s), let (bef, aft) = T.splitAt ind s, T.isInfixOf (T.singleton 'C') s = T.snoc (order (T.append bef (T.tail aft))) 'C'
    | let ind = M.fromJust (T.findIndex ('D' ==) s), let (bef, aft) = T.splitAt ind s, T.isInfixOf (T.singleton 'D') s = T.snoc (order (T.append bef (T.tail aft))) 'D'
    | let ind = M.fromJust (T.findIndex ('M' ==) s), let (bef, aft) = T.splitAt ind s, T.isInfixOf (T.singleton 'M') s = T.snoc (order (T.append bef (T.tail aft))) 'M'
    | otherwise = T.empty

--
reduce :: T.Text -> T.Text
reduce s
    | T.isSuffixOf (T.pack "VV") s = reduce (order (T.append (T.dropEnd 2 s)(T.pack "X") ))
    | T.isSuffixOf (T.pack "IIIII") s = reduce (order (T.append (T.dropEnd 5 s) (T.pack "V")))
    | T.isSuffixOf (T.pack "VIIII") s = reduce (T.append (T.dropEnd 5 s) (T.pack "IX"))
    | T.isSuffixOf (T.pack "IIII") s = reduce (T.append (T.dropEnd 4 s) (T.pack "IV"))
    | T.isSuffixOf (T.pack "VIX") s = reduce (T.append (T.dropEnd 3 s) (T.pack "XIV"))
    | T.isSuffixOf (T.pack "LL") s = reduce (order (T.append (T.dropEnd 2 s) (T.pack "C")))
    | T.isSuffixOf (T.pack "XXXXX") s = reduce (order (T.append (T.dropEnd 5 s) (T.pack "L")))
    | T.isSuffixOf (T.pack "LXXXX") s = reduce (T.append (T.dropEnd 5 s) (T.pack "XC"))
    | T.isSuffixOf (T.pack "XXXX") s = reduce (T.append (T.dropEnd 4 s) (T.pack "XL"))
    | T.isSuffixOf (T.pack "LXC") s = reduce (T.append (T.dropEnd 3 s) (T.pack "CLX"))
    | T.isSuffixOf (T.pack "DD") s = reduce (order (T.append (T.dropEnd 2 s) (T.pack "M")))
    | T.isSuffixOf (T.pack "CCCCC") s = reduce (order (T.append (T.dropEnd 5 s) (T.pack "D")))
    | T.isSuffixOf (T.pack "DCCCC") s = reduce (T.append (T.dropEnd 5 s) (T.pack "CM"))
    | T.isSuffixOf (T.pack "CCCC") s = reduce (T.append (T.dropEnd 4 s) (T.pack "CD"))
    | T.isSuffixOf (T.pack "DCM") s = reduce (T.append (T.dropEnd 3 s) (T.pack "MCD"))
    | not (T.null s) = T.snoc (reduce (T.init s)) (T.last s)
    | otherwise = T.empty

extend :: T.Text -> T.Text 
extend s
    | T.isPrefixOf (T.pack "CM") s = T.append (T.pack "DCCCC") (extend (T.drop 2 s))
    | T.isPrefixOf (T.pack "CD") s = T.append (T.pack "CCCC") (extend (T.drop 2 s))
    | T.isPrefixOf (T.pack "XC") s = T.append (T.pack "LXXXX") (extend (T.drop 2 s))
    | T.isPrefixOf (T.pack "XL") s = T.append (T.pack "XXXX") (extend (T.drop 2 s))
    | T.isPrefixOf (T.pack "IX") s = T.append (T.pack "VIIII") (extend (T.drop 2 s))
    | T.isPrefixOf (T.pack "IV") s = T.append (T.pack "IIII") (extend (T.drop 2 s))
    | not (T.null s) = T.cons (T.head s) (extend (T.tail s))
    | otherwise = T.empty


----------------
-- RN Arithmetic
----------------
addRN :: T.Text -> T.Text -> T.Text
addRN n m = normalize (T.concat [extend n, extend m])

subRN :: T.Text -> T.Text
subRN s
    | T.isSuffixOf (T.pack "I") s = T.dropEnd 1 s
    | T.isSuffixOf (T.pack "IV") s = T.append (T.dropEnd 2 s) (T.pack "III")
    | T.isSuffixOf (T.pack "V") s = T.append (T.dropEnd 1 s) (T.pack "IV")
    | T.isSuffixOf (T.pack "IX") s = T.append (T.dropEnd 2 s) (T.pack "VIII")
    | T.isSuffixOf (T.pack "X") s = T.append (T.dropEnd 1 s) (T.pack "IX")
    | T.isSuffixOf (T.pack "XL") s = T.append (T.dropEnd 2 s) (T.pack "XXXIX")
    | T.isSuffixOf (T.pack "L") s = T.append (T.dropEnd 1 s) (T.pack "XLIX")
    | T.isSuffixOf (T.pack "XC") s = T.append (T.dropEnd 2 s) (T.pack "LXXXIX")
    | T.isSuffixOf (T.pack "C") s = T.append (T.dropEnd 1 s) (T.pack "XCIX")
    | T.isSuffixOf (T.pack "CD") s = T.append (T.dropEnd 2 s) (T.pack "CCCXCIX")
    | T.isSuffixOf (T.pack "D") s = T.append (T.dropEnd 1 s) (T.pack "CDXCIX")
    | T.isSuffixOf (T.pack "CM") s = T.append (T.dropEnd 2 s) (T.pack "DCCCXCIX")
    | T.isSuffixOf (T.pack "M") s = T.append (T.dropEnd 1 s) (T.pack "CMXCIX")
    | otherwise = T.empty

minusRN :: T.Text -> T.Text -> T.Text
minusRN n m
    | n == T.empty = T.empty
    | m == T.empty = n
    | otherwise = minusRN (subRN n) (subRN m)

multRN :: T.Text -> T.Text -> T.Text
multRN n m 
    | (n == T.empty) || (m  == T.empty) = T.empty 
    | m == T.pack "I" = n
    | n == T.pack "I" = m
    | otherwise = addRN n (multRN n (subRN m))

----------------------------------------------------
-- Converting between RN-numbers and Haskell-numbers
----------------------------------------------------
--converting from an integer to roman numeral, do have to call normalize on the function
intRN :: Integer -> T.Text
intRN 0 = T.empty
intRN n = T.append (T.pack "I") (intRN(n - 1))

--converting from a roman numeral to integer
rnInt :: T.Text -> Int
rnInt m
    | m == T.empty = 0
    | otherwise = 1 + rnInt (subRN m)

----------------
-- Normalization
----------------
normalize :: T.Text -> T.Text
normalize x = reduce (order x)

main = do
    let i = 4
    let j = 2
    let k = 1
    let l = 3
-- testing arithmetic
    print $ addRN (normalize(intRN l)) (addRN (normalize (intRN k)) (addRN (normalize(intRN i)) (normalize(intRN j)))) -- = X
    print $ minusRN (normalize(intRN i)) (normalize(intRN j)) -- = II
    print $ rnInt (multRN (normalize(intRN l)) (normalize(intRN j))) -- = 6
-- testing conversions
    print $ rnInt (T.pack "XLVI") -- = 46
    print $ normalize (intRN 223) -- = CCXXIII


