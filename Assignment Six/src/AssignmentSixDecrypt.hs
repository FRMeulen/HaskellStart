module Main
    where
        import System.Random
        import System.IO
        import Data.Bits
        import Control.Monad
        import Data.Char
        import Data.List
        import Data.List.Split

        --Main function
        main = do
            getPlainText

        --Function to read text file
        readTextFile = do
            inp <- readFile "encrypted.txt"
            return inp

        --Function to read key
        readKey = do
            inp <- readFile "key.txt"
            return inp

        --Function to convert letter to Int
        convertLetter::Char -> Int
        convertLetter letter = fromEnum(letter)

        --Function to convert string to int list
        getTextInts = do
            text <- readTextFile
            let textInts = map convertLetter text
            return textInts

        --Function to get plaintext
        getPlainText = do
            cipherInts <- getTextInts
            key <- prepareKey
            let plainTextInts = map (`mod` 128) $ zipWith (xor) cipherInts key
            let plainText = map convertAscii plainTextInts
            writeResult plainText
            return plainText

        --Function to convert Int to letter
        convertAscii::Int -> Char
        convertAscii ascii = toEnum(ascii)

        --Function to prepare key for use
        prepareKey = do
            rawKey <- readKey
            let stage1 = filter (/='[') rawKey
            let stage2 = filter (/=']') stage1
            let stage3 = map replaceComma stage2
            let key = map read $ words stage3 :: [Int]
            return key

        --Function to replace comma with space
        replaceComma::Char -> Char
        replaceComma x
            |x == ',' = ' '
            |otherwise = x

        --Function to write result
        writeResult::String -> IO ()
        writeResult text = writeFile "decrypted.txt" text