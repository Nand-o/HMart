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
    | null history = putStrLn "[!] Belum ada transaksi."
    | otherwise = do
        putStrLn "+------+-------------+----------------+------------------+"
        putStrLn "| ID   | Jml Item    | Total (Rp)     | Tanggal          |"
        putStrLn "+------+-------------+----------------+------------------+"
        -- HOF: mapM_ untuk mencetak tiap elemen list history
        mapM_ printTransRow history 
        putStrLn "+------+-------------+----------------+------------------+"
        
        -- HOF: foldl untuk mengakumulasi grand total seluruh transaksi
        let grandTotal = foldl (\acc t -> acc + transactionTotal t) 0 history 
        putStrLn $ "  Grand Total: Rp " ++ formatNum grandTotal

-- | C.2 - Mencari transaksi spesifik berdasarkan ID
cariTransaksi :: History -> IO ()
cariTransaksi history = do
    putStr "Masukkan ID transaksi yang dicari: "; idStr <- getLine
    let targetId = readInt idStr 0
        -- HOF: filter untuk mencari transaksi spesifik
        found = filter (\t -> transactionId t == targetId) history 
        
    case found of
        [] -> putStrLn $ "[!] Transaksi ID #" ++ show targetId ++ " tidak ditemukan."
        (t:_) -> do
            putStrLn $ "\n[OK] Transaksi #" ++ show (transactionId t) ++ " ditemukan:"
            putStrLn $ "  Jumlah item: " ++ show (length (transactionItems t)) ++ " produk"
            putStrLn $ "  Total      : Rp" ++ formatNum (transactionTotal t)
            putStrLn $ "  Dibayar    : Rp" ++ formatNum (transactionPaid t)
            putStrLn $ "  Kembalian  : Rp" ++ formatNum (transactionChange t)
            putStrLn $ "  Tanggal    : " ++ transactionDate t
            putStrLn "  (Pilih 'Detail Transaksi' untuk melihat daftar barang lengkap)"

-- | C.3 - Melihat detail barang di dalam satu transaksi
detailTransaksi :: History -> IO ()
detailTransaksi history = do
    putStr "Masukkan ID transaksi: "; idStr <- getLine
    let targetId = readInt idStr 0
        -- HOF: filter 
        found = filter (\t -> transactionId t == targetId) history 
        
    case found of
        [] -> putStrLn "[!] Transaksi tidak ditemukan."
        (t:_) -> do
            putStrLn $ "\n=== Detail Transaksi #" ++ show (transactionId t) ++ " ==="
            putStrLn $ "Tanggal: " ++ transactionDate t
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
            putStrLn $ "TOTAL    : Rp " ++ formatNum (transactionTotal t)
            putStrLn $ "DIBAYAR  : Rp " ++ formatNum (transactionPaid t)
            putStrLn $ "KEMBALIAN: Rp " ++ formatNum (transactionChange t)
