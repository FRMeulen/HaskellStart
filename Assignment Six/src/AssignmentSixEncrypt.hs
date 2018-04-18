module Main
    where
        import System.Random
        import System.IO
        import Data.Bits
        import Control.Monad
        import Data.Char

        main::IO[Char]
        main = do
            getCipherText

        --Function to read text file -> monadic
        readTextFile = do
            inp <- readFile "AssignmentSixEncryptable.txt"
            return inp

        --Function to get message as string
        getPlainText = do
            plainText <- readTextFile
            return plainText

        --Function to convert string to [Int]
        getTextInts = do
            text <- getPlainText
            let textInts = map (convertLetter) text
            return textInts

        --Function to get key
        getKey = do
            plainText <- getPlainText
            key <- randomList $ keyLength plainText
            return key

        --Function to XOR textInts and key, making ciphertext
        getCipherText = do
            textInts <- getTextInts
            key <- getKey
            let cipherInts = map (`mod` 128) $ zipWith (xor) textInts key
            let cipherText = map convertAscii cipherInts
            writeKey $ show key
            writeResult cipherText
            return cipherText

        --Function to generate list of x random numbers -> monadic
        randomList::Int -> IO([Int])
        randomList n = replicateM n $ randomRIO (0,127)

        --Function to convert letter to Int
        convertLetter::Char -> Int
        convertLetter letter = fromEnum(letter)

        --Function to convert int to letter
        convertAscii::Int -> Char
        convertAscii ascii = toEnum(ascii)

        --Function to determine key length
        keyLength::String -> Int
        keyLength text = length(text)

        --Function to write file
        writeKey::String -> IO ()
        writeKey text = writeFile "key.txt" text

        --Function to write result
        writeResult::String -> IO ()
        writeResult text = writeFile "encrypted.txt" text