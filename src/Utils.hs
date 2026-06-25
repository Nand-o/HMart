module Utils where

import Types
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

-- Fungsi parsing angka yang AMAN (mencegah runtime errors)
readInt :: String -> Int -> Int
readInt str defaultValue =
    case reads str :: [(Int, String)] of
        [(n, "")] -> n             -- Jika parsing berhasil, kembalikan nilai yang di-parse
        _         -> defaultValue  -- Jika parsing gagal, kembalikan nilai default

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

-- Fungsi pencetak Header Menu
printHeader :: String -> IO ()
printHeader title = do
    let line = replicate 45 '='
    putStrLn line
    putStrLn $ "  " ++ title
    putStrLn line

-- Fungsi untuk mendapatkan TimeStamp waktu saat checkout
getTimeStamp :: IO String
getTimeStamp = do
    now <- getCurrentTime
    return $ formatTime defaultTimeLocale "%d/%m/%Y %H:%M" now
