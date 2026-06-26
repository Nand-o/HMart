module Utils where

import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.IO (hFlush, stdout)

screenWidth :: Int
screenWidth = 58

data AnsiColor
    = Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

ansiColorCode :: AnsiColor -> String
ansiColorCode Black   = "30"
ansiColorCode Red     = "31"
ansiColorCode Green   = "32"
ansiColorCode Yellow  = "33"
ansiColorCode Blue    = "34"
ansiColorCode Magenta = "35"
ansiColorCode Cyan    = "36"
ansiColorCode White   = "37"

colorText :: AnsiColor -> String -> String
colorText color value = "\ESC[" ++ ansiColorCode color ++ "m" ++ value ++ "\ESC[0m"

boldText :: String -> String
boldText value = "\ESC[1m" ++ value ++ "\ESC[0m"

-- Fungsi parsing angka yang AMAN (mencegah runtime errors)
readInt :: String -> Int -> Int
readInt str defaultValue =
    case reads str :: [(Int, String)] of
        [(n, "")] -> n             -- Jika parsing berhasil, kembalikan nilai yang di-parse
        _         -> defaultValue  -- Jika parsing gagal, kembalikan nilai default

-- Parsing angka yang lebih ketat: gagal jika input tidak murni angka
parseInt :: String -> Maybe Int
parseInt str =
    case reads str :: [(Int, String)] of
        [(n, "")] -> Just n
        _          -> Nothing

-- Meminta input angka sampai valid
promptInt :: String -> (Int -> Bool) -> String -> IO Int
promptInt prompt isValid errorMessage = do
    putStr prompt
    input <- getLine
    case parseInt input of
        Just value | isValid value -> return value
        _ -> do
            printWarning errorMessage
            promptInt prompt isValid errorMessage

-- Meminta input angka opsional: Enter untuk skip, angka lain harus valid
promptMaybeInt :: String -> (Int -> Bool) -> String -> IO (Maybe Int)
promptMaybeInt prompt isValid errorMessage = do
    putStr prompt
    input <- getLine
    if null input
        then return Nothing
        else case parseInt input of
            Just value | isValid value -> return (Just value)
            _ -> do
                printWarning errorMessage
                promptMaybeInt prompt isValid errorMessage

-- Fungsi format Rupiah
formatNum :: Int -> String
formatNum n
    | n < 0     = "-" ++ formatNum (-n)
    | n < 1000  = show n
    | otherwise = formatNum (n `div` 1000) ++ "." ++ pad3 (n `mod` 1000)
    where
    pad3 x = let s = show x in replicate (3 - length s) '0' ++ s

-- Fungsi perataan teks untuk tampilan tabel ASCII (padding)
padRight :: Int -> String -> String
padRight n s = take n (s ++ repeat ' ')

padLeft :: Int -> String -> String
padLeft n s
    | length s >= n = s
    | otherwise     = replicate (n - length s) ' ' ++ s

centerText :: Int -> String -> String
centerText n s
    | length s >= n = take n s
    | otherwise     = let total = n - length s
                          left  = total `div` 2
                          right = total - left
                      in replicate left ' ' ++ s ++ replicate right ' '

-- Fungsi pencetak Header Menu
printHeader :: String -> IO ()
printHeader title = do
    let line = replicate screenWidth '='
    putStrLn $ colorText Cyan line
    putStrLn $ colorText Cyan $ centerText screenWidth (boldText title)
    putStrLn $ colorText Cyan line

printSectionGap :: IO ()
printSectionGap = putStrLn ""

printKeyValue :: String -> String -> IO ()
printKeyValue label value =
    putStrLn $ "  " ++ colorText Blue (padRight 18 label) ++ ": " ++ value

printSuccess :: String -> IO ()
printSuccess message = putStrLn $ colorText Green $ "[OK] " ++ message

printWarning :: String -> IO ()
printWarning message = putStrLn $ colorText Yellow $ "[!] " ++ message

printError :: String -> IO ()
printError message = putStrLn $ colorText Red $ "[!] " ++ message

clearScreen :: IO ()
clearScreen = do
    putStr "\ESC[2J\ESC[H"
    hFlush stdout

printExitScreen :: IO ()
printExitScreen = do
    printSectionGap
    putStrLn $ colorText Cyan "     ░▒▓█▓▒░░▒▓█▓▒░▒▓██████████████▓▒░ ░▒▓██████▓▒░░▒▓███████▓▒░▒▓████████▓▒░"
    putStrLn $ colorText Cyan "     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░"
    putStrLn $ colorText Cyan "     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░"
    putStrLn $ colorText Cyan "     ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░▒▓███████▓▒░  ░▒▓█▓▒░"
    putStrLn $ colorText Cyan "     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░"
    putStrLn $ colorText Cyan "     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░"
    putStrLn $ colorText Cyan "     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░"
    putStrLn ""
    putStrLn $ colorText Green "  Mungkin Kita memang harus berpisah untuk tumbuh, bukan untuk melupakan. See you later."
    printSectionGap
    putStrLn $ colorText Yellow "  Bukan selamat tinggal, tapi sampai bertemu kembali di cerita kehidupan yang berbeda."
    putStrLn $ colorText Yellow "  - HMart Cashier System -"

-- Fungsi untuk mendapatkan TimeStamp waktu saat checkout
getTimeStamp :: IO String
getTimeStamp = do
    now <- getCurrentTime
    return $ formatTime defaultTimeLocale "%d/%m/%Y %H:%M" now

