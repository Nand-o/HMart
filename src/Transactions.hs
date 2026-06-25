module Transactions where

import Types
import Utils

-- | C.1 - Fungsi Helper untuk mencetak satu baris ringkasan transaksi
printTransRow :: Transaction -> IO ()
printTransRow t = putStrLn $
    "| " ++ padLeft 4 (show (transactionId t))
    ++ " | " ++ padLeft 11 (show (length (transactionItems t)))
    ++ " | Rp" ++ padLeft 12 (formatNum (transactionTotal t))
    ++ " | " ++ transactionDate t ++ " |"

-- | Menampilkan ringkasan semua transaksi
lihatSemuaTransaksi :: History -> IO ()
lihatSemuaTransaksi history
    | null history = printWarning "Belum ada transaksi."
    | otherwise = do
        printSectionGap
        putStrLn "+------+-------------+----------------+------------------+"
        putStrLn "| ID   | Jml Item    | Total (Rp)     | Tanggal          |"
        putStrLn "+------+-------------+----------------+------------------+"
        -- HOF: mapM_ untuk mencetak tiap elemen list history
        mapM_ printTransRow history 
        putStrLn "+------+-------------+----------------+------------------+"
        printSectionGap
        -- HOF: foldl untuk mengakumulasi grand total seluruh transaksi
        let grandTotal = foldl (\acc t -> acc + transactionTotal t) 0 history 
        printKeyValue "Grand Total" ("Rp " ++ formatNum grandTotal)

-- | C.2 - Mencari transaksi spesifik berdasarkan ID
cariTransaksi :: History -> IO ()
cariTransaksi history = do
    printSectionGap
    putStr "Masukkan ID transaksi yang dicari: "; idStr <- getLine
    let targetId = readInt idStr 0
        -- HOF: filter untuk mencari transaksi spesifik
        found = filter (\t -> transactionId t == targetId) history 
        
    case found of
        [] -> printWarning $ "Transaksi ID #" ++ show targetId ++ " tidak ditemukan."
        (t:_) -> do
            printSectionGap
            printSuccess $ "Transaksi #" ++ show (transactionId t) ++ " ditemukan."
            printKeyValue "Jumlah item" (show (length (transactionItems t)) ++ " produk")
            printKeyValue "Total" ("Rp " ++ formatNum (transactionTotal t))
            printKeyValue "Dibayar" ("Rp " ++ formatNum (transactionPaid t))
            printKeyValue "Kembalian" ("Rp " ++ formatNum (transactionChange t))
            printKeyValue "Tanggal" (transactionDate t)
            putStrLn "  (Pilih 'Detail Transaksi' untuk melihat daftar barang lengkap)"

-- | C.3 - Melihat detail barang di dalam satu transaksi
detailTransaksi :: History -> IO ()
detailTransaksi history = do
    printSectionGap
    putStr "Masukkan ID transaksi: "; idStr <- getLine
    let targetId = readInt idStr 0
        -- HOF: filter 
        found = filter (\t -> transactionId t == targetId) history 
        
    case found of
        [] -> printWarning "Transaksi tidak ditemukan."
        (t:_) -> do
            printSectionGap
            printHeader $ "Detail Transaksi #" ++ show (transactionId t)
            printSectionGap
            printKeyValue "Tanggal" (transactionDate t)
            printSectionGap
            putStrLn "+----------------------+-----+---------------+"
            putStrLn "| Nama Barang          | Qty | Subtotal      |"
            putStrLn "+----------------------+-----+---------------+"
            
            -- HOF: mapM_ untuk mencetak item di dalam transaksi
            mapM_ (\c -> putStrLn $ 
                "| " ++ padRight 20 (productName (cartProduct c))
                ++ " | " ++ padLeft 3 (show (cartQty c))
                ++ " | Rp" ++ padLeft 8 (formatNum (productPrice (cartProduct c) * cartQty c))
                ++ " |"
                ) (transactionItems t)
                
            putStrLn "+----------------------+-----+---------------+"
            printSectionGap
            printKeyValue "TOTAL" ("Rp " ++ formatNum (transactionTotal t))
            printKeyValue "DIBAYAR" ("Rp " ++ formatNum (transactionPaid t))
            printKeyValue "KEMBALIAN" ("Rp " ++ formatNum (transactionChange t))
