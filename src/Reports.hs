module Reports where

import Types
import Utils
import Data.List (sortBy)

-- | Ambang batas stok untuk peringatan
stockThreshold :: Int
stockThreshold = 5

-- | D.1 - Total Pendapatan Keseluruhan
totalPenjualan :: History -> IO ()
totalPenjualan history = do
    -- HOF: foldl untuk akumulasi total nilai penjualan
    let grandTotal = foldl (\acc t -> acc + transactionTotal t) 0 history 
    putStrLn "\n--- Total Penjualan ---"
    putStrLn $ "Jumlah transaksi: " ++ show (length history)
    putStrLn $ "Total penjualan : Rp " ++ formatNum grandTotal

-- | D.2 - Laporan Barang Terlaris
-- Helper fungsi rekursif untuk menghitung total penjualan per nama barang
updateTally :: [(String, Int)] -> String -> Int -> [(String, Int)]
updateTally [] nm q = [(nm, q)]
updateTally ((n, c):rest) nm q
    | n == nm   = (n, c + q) : rest
    | otherwise = (n, c) : updateTally rest nm q

barangTerlaris :: History -> IO ()
barangTerlaris history
    | null history = putStrLn "[!] Belum ada transaksi."
    | otherwise = do
        -- HOF: concatMap untuk membongkar daftar di dalam daftar
        let allItems = concatMap transactionItems history 
            
            -- HOF: foldl untuk menghitung tally (frekuensi)
            tally = foldl (\acc item -> 
                        let nm = productName (cartProduct item)
                            q = cartQty item
                        in updateTally acc nm q
                    ) [] allItems
                    
            -- HOF: sortBy untuk mengurutkan hasil secara descending (besar ke kecil)
            sorted = sortBy (\(_, a) (_, b) -> compare b a) tally 
            top5 = take 5 sorted
            
        putStrLn "\n--- Barang Terlaris (Top 5) ---"
        putStrLn "+---+------------------------+-------------+"
        putStrLn "| # | Nama Barang            | Terjual     |"
        putStrLn "+---+------------------------+-------------+"
        -- HOF: mapM_ untuk mencetak tabel akhir
        mapM_ (\(rank, (nm, q)) -> putStrLn $ 
            "| " ++ padLeft 1 (show rank)
            ++ " | " ++ padRight 22 nm
            ++ " | " ++ padLeft 6 (show q) ++ " pcs |"
            ) (zip [1 :: Int ..] top5)
        putStrLn "+---+------------------------+-------------+"

-- | D.3 - Laporan Stok Menipis
stokMenipis :: Catalog -> IO ()
stokMenipis catalog = do
    -- HOF: filter untuk memisahkan barang dengan stok rendah
    let lowStock = filter (\p -> productStock p <= stockThreshold) catalog 
    if null lowStock
        then putStrLn "[OK] Semua stok aman (tidak ada yang di bawah batas minimum)."
        else do
            putStrLn $ "\n[!] " ++ show (length lowStock) ++ " barang perlu direstok:"
            putStrLn "+----+------------------------+-------+"
            putStrLn "| ID | Nama Barang            | Stok  |"
            putStrLn "+----+------------------------+-------+"
            
            -- HOF: map untuk merubah list Product menjadi list kalimat peringatan
            let warnings = map (\p -> 
                    "| " ++ padLeft 2 (show (productId p))
                    ++ " | " ++ padRight 22 (productName p)
                    ++ " | " ++ padLeft 5 (show (productStock p)) ++ " |"
                    ) lowStock
            mapM_ putStrLn warnings -- HOF: mapM_
            putStrLn "+----+------------------------+-------+"

-- | D.3 (Alternatif) - Stok menipis menggunakan LIST COMPREHENSION
stokMenipisLC :: Catalog -> [Product]
stokMenipisLC catalog = [p | p <- catalog, productStock p <= stockThreshold]

-- | D.4 - Pendapatan Kasir
pendapatan :: History -> IO ()
pendapatan history
    | null history = putStrLn "[!] Belum ada transaksi."
    | otherwise = do
        -- HOF: Tiga foldl yang berjalan terpisah
        let totalMasuk     = foldl (\acc t -> acc + transactionPaid t) 0 history 
            totalNilai     = foldl (\acc t -> acc + transactionTotal t) 0 history 
            totalKembalian = foldl (\acc t -> acc + transactionChange t) 0 history 
            
        putStrLn "\n--- Laporan Pendapatan ---"
        putStrLn $ "Jumlah transaksi      : " ++ show (length history)
        putStrLn $ "Total uang masuk      : Rp " ++ formatNum totalMasuk
        putStrLn $ "Total nilai penjualan : Rp " ++ formatNum totalNilai
        putStrLn $ "Total kembalian       : Rp " ++ formatNum totalKembalian
        putStrLn $ "(Verifikasi: " ++ formatNum totalMasuk ++ " - " ++ formatNum totalKembalian ++ " = " ++ formatNum totalNilai ++ ")"
