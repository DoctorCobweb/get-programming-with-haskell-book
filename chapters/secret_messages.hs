-- this capstone covers:
-- 1. learning the basics of cryptography
-- 2. using basic types to model your data
-- 3. makeing practical use of `Enum` and `Bounded`
-- 4. writing and making instances of your own `Cipher` class
--
-- a cipher in cryptography is a means of encoding a message so 
-- the others can't read it.
--
-- implementing ROT13 cipher
-- let's experiment with a small 4 letter alphabet. we add Bounded
-- because it will allow us to automatically convert the data
-- constructors to type `Int`. you can use `fromEnum` to convert your
-- letters to Ints, and `toEnum` to take Ints and turn them back to letters
--
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

-- a genereal encoder to use on alphabets with either odd or even lengths
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum c + halfAlphabet 
          rotation = offset `mod` alphabetSize

-- a general decoder to use on alphabets with either odd or even lengths.
-- when defined this way, you can always encode and then recover the message
-- in TWO steps. encoders then decoder.
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
    where halfN = n `div` 2
          offset = if even n
                   then fromEnum c + halfN
                   else 1 + fromEnum c + halfN
          rotation = offset `mod` n

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

-- let's try to encrypt a message which is a list of elements
-- from our 4 letter alphabet
message :: [FourLetterAlphabet]
message = [L1,L3,L4,L1,L1,L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder msg = map rot4Letters msg
    where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          rot4Letters =  rotN alphaSize

fourLetterDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterDecoder msg = map rot4LettersDecoder msg
    where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          rot4LettersDecoder = rotNdecoder alphaSize


-- an odd-numbered alphabet
data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder msg = map rot3Letters msg
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3Letters = rotN alphaSize    

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder msg = map rot3LettersDecoder msg
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3LettersDecoder = rotNdecoder alphaSize


--finally for encoder and decoder functions to use on Strings
rotEncoder :: String -> String
rotEncoder msg = map rotChar msg
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder msg = map rotCharDecoder msg
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotCharDecoder = rotNdecoder alphaSize


-----------------------------------
-- 15.2 XOR: the magic of cryptopgraphy
-----------------------------------
-- XOR has 2 important properties:
--
-- 1. XOR is symmetric. XOR-ing two lists of Bools results in
--    a new list of Bools. XOR-ing this new list with either one
--    of the original lists results in the OTHER.
--
-- 2. When given a uniform distribution of True and False values 
--    (or in practice a string of bits), no matter what the distributions
--    of True and False values in your plain text, the output of the XOR-ing
--    will be a uniform distribution of True and False values. 
--    In practice, if you take a nonrandom string of values, such as text,
--    and XOR it with a random string of values, the results will appear
--    to the observer a random noise.
--
--    When properly XOR-ing data, the output is indistinguishable from noise!
--

-- define a helper function which operates on 2 Bools
xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && (not (v1 && v2))

-- since the main goal will be to XOR 2 lists of Bool values, define a
-- function which operates on pairs of Bools.
xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

-- now putting this all together allows us to operation on 2 lists of Bools
xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)


-----------------------------------
-- 15.3 representing values as bits
-----------------------------------
-- you dont usually think of lists of Booleans, rather you think of streams of bits.
-- to make reasoning about your code easier let's create a useful synonym, `Bits`:
--
type Bits = [Bool]

-- need to convert strings into bits. strings are made of characters and characters
-- are equivalent to Ints (each Char can be converted into an Int). once you have an
-- Int you then convert it to binary, aka bits.

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
               then False : intToBits' nextVal
               else True : intToBits' nextVal
    where remainder = n `mod` 2
          nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (intToBits' n)
          missingBits = maxBits - (length reversedBits)
          leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) locationsOfTrues)
    where size = length bits
          indices = [size-1, size-2 .. 0]
          locationsOfTrues = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

-- ----------------------
-- 15.4 The one-time pad
-- ----------------------
--
myPad :: String 
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

-- to encrypt myPlainText, you convert your pad and plain text into bits, and then XOR the
-- results.
applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair -> (fst pair) `xor` (snd pair))
                              (zip padBits plaintextBits)
    where padBits = map charToBits pad
          plaintextBits = map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar (applyOTP' pad plaintext)

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad


-------------------
-- 15.5 A Cipher class
-- -----------------
--
class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])
