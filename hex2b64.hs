import Data.Char (ord, chr)

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

xorarr :: [Bool] -> [Bool] -> [Bool]
xorarr = zipWith xor

hex2dec :: Char -> Int
hex2dec n =
  let mapeo = zip [0..15] "0123456789abcdef"
      match = filter (\m -> (snd m) == n) mapeo
  in fst . head $ match

dec2hex :: Int -> String
dec2hex num =
  let left = div num 16
      right = mod num 16
      tpl = "0123456789abcdef"
  in [tpl !! left, tpl !! right]

hexstring2bytes :: String -> [Int]
hexstring2bytes "" = []
hexstring2bytes (l:r:demas) = (16 * (hex2dec l) + (hex2dec r)):(hexstring2bytes demas)

dec2bits :: Int -> [Bool]
dec2bits 0 = [False]
dec2bits 1 = [True]
dec2bits n =
  let this = (mod n 2) /= 0
  in (dec2bits (div n 2)) ++ [this]

dec2byte n =
  let bits = dec2bits n
      pad = replicate (8 - (length bits)) False
  in pad ++ bits

toSextets :: [Bool] -> [[Bool]]
toSextets bools
  -- we have less than 6 bools, so we pad with zero (false) until we have 6
  | (length bools) < 6 = [(bools ++ (replicate (6 - (length bools)) False))]
  -- we have 6 bools, so this is already a sextet
  | (length bools) == 6 = [bools]
  -- we have more than 6 bools, so we have more than one sextet
  | otherwise = (take 6 bools) : (toSextets (drop 6 bools))

bin2dec :: [Bool] -> Int
bin2dec [False] = 0
bin2dec [True] = 1
bin2dec (False:xs) = (bin2dec xs)
bin2dec (True:xs) = (bin2dec xs) + (2 ^ (length xs))

dec2b64 :: Int -> Char
dec2b64 num =
  let values = ['A'..'Z'] ++ ['a'..'z'] ++ "0123456789+/"
  in values !! num

-- PROBLEMA 1
hex2b64 :: String -> String
hex2b64 str =
  let pad = ((6 - ((length str) `mod` 6)) `div` 2) `mod` 3
      offt = replicate pad '='
  in (map (dec2b64 . bin2dec) (toSextets ((hexstring2bytes str) >>= dec2byte))) ++ offt

-- PROBLEMA 2
fixed_xor :: String -> String -> String
fixed_xor msg key =
  let bits_msg = map dec2byte (hexstring2bytes msg)
      bits_key = map dec2byte (hexstring2bytes key)
      ciphered = zipWith xorarr bits_msg bits_key
  in concat (map (dec2hex . bin2dec) ciphered)

dec2hexstring :: [Int] -> String
dec2hexstring n = concat (map dec2hex n)

-- good_english_1 deja fuera las que tienen caracteres no imprimibles
good_english_1 :: [Int] -> Bool
good_english_1 msg = (all (>= 32) msg) && (all (<= 126) msg)

good_english_2 :: [Int] -> Bool
good_english_2 msg = (any (== 32) msg) && (not (all (== 32) msg))

karma_english :: [Int] -> Int
karma_english msg =
  let letters = [65, 69, 97, 101, 32]
      occurrences = \l -> length (filter (== l) msg)
  in sum (map occurrences letters)

comparePair :: ([Int], Int) -> ([Int], Int) -> ([Int], Int)
comparePair left@(_,sl) right@(_,sr) = if sl > sr then left else right

maxpair :: [([Int], Int)] -> [Int]
maxpair tuples = fst (foldr comparePair ([], 0) tuples)

-- problema 3
-- problema_3 :: [[Int]]
problema_3 =
  let msg = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
      longitud = div (length msg) 2
      keyspace = map (replicate longitud) [0..255]
      msgspace = map (hexstring2bytes . (fixed_xor msg) . dec2hexstring) keyspace
      printable = filter good_english_2 (filter good_english_1 msgspace)
      scored = map (\str -> (str, karma_english str)) printable
      maxp = maxpair scored
      in map chr maxp

problema_5 =
  let msg = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
      expected = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
      chars = map ord msg
      clave = take (length chars) (cycle [73, 67, 69]) -- "ICEICEICEICE..."
      ciphered = zipWith xorarr (map dec2byte chars) (map dec2byte clave)
  in expected == concat (map (dec2hex . bin2dec) ciphered)
