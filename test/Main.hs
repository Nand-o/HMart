module Main where

import Cart
import Reports
import Types
import Utils
import System.Exit (exitFailure)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
assertEqual label expected actual = do
    if expected == actual
        then do
            putStrLn $ "[PASS] " ++ label
            return True
        else do
            putStrLn $ "[FAIL] " ++ label
            putStrLn $ "       expected: " ++ show expected
            putStrLn $ "       actual:   " ++ show actual
            return False

sampleProduct1 :: Product
sampleProduct1 = Product 1 "Indomie Goreng" 3500 50

sampleProduct2 :: Product
sampleProduct2 = Product 2 "Susu Ultra 1L" 12000 30

sampleCart :: Cart
sampleCart =
    [ CartItem sampleProduct1 2
    , CartItem sampleProduct2 3
    ]

sampleCatalog :: Catalog
sampleCatalog = [sampleProduct1, sampleProduct2]

sampleLowStockProduct :: Product
sampleLowStockProduct = Product 3 "Teh Botol Sosro" 5000 5

sampleSafeStockProduct :: Product
sampleSafeStockProduct = Product 4 "Sabun Mandi Lifebuoy" 8000 25

sampleState :: AppState
sampleState = AppState sampleCatalog sampleCart [Transaction 1 sampleCart 43000 50000 7000 "01/01/2026 10:00"]

main :: IO ()
main = do
    results <- sequence
        [ assertEqual "formatNum 0" "0" (formatNum 0)
        , assertEqual "formatNum 1234" "1.234" (formatNum 1234)
        , assertEqual "formatNum 12345678" "12.345.678" (formatNum 12345678)
        , assertEqual "formatNum negative" "-12.345" (formatNum (-12345))
        , assertEqual "readInt valid" 42 (readInt "42" 0)
        , assertEqual "readInt invalid fallback" 7 (readInt "abc" 7)
        , assertEqual "readInt negative" (-8) (readInt "-8" 0)
        , assertEqual "parseInt valid" (Just 99) (parseInt "99")
        , assertEqual "parseInt invalid" Nothing (parseInt "9x")
        , assertEqual "parseInt empty" Nothing (parseInt "")
        , assertEqual "hitungTotal sample cart" 43000 (hitungTotal sampleCart)
        , assertEqual "hitungTotal empty cart" 0 (hitungTotal [])
        , assertEqual "stok threshold" 5 stockThreshold
        , assertEqual "updateTally new item" [("Indomie Goreng", 2)] (updateTally [] "Indomie Goreng" 2)
        , assertEqual "updateTally existing item" [("Indomie Goreng", 5), ("Susu Ultra 1L", 3)] (updateTally [("Indomie Goreng", 2), ("Susu Ultra 1L", 3)] "Indomie Goreng" 3)
        , assertEqual "stokMenipisLC threshold inclusive" [sampleLowStockProduct] (stokMenipisLC [sampleLowStockProduct, sampleSafeStockProduct])
        , assertEqual "AppState catalog accessor" sampleCatalog (appCatalog sampleState)
        , assertEqual "AppState cart accessor" sampleCart (appCart sampleState)
        , assertEqual "AppState history accessor" [Transaction 1 sampleCart 43000 50000 7000 "01/01/2026 10:00"] (appHistory sampleState)
        ]

    if and results
        then putStrLn "All tests passed."
        else exitFailure
