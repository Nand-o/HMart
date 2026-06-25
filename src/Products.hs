module Products where

import Types
import Utils
import Data.Char (toLower)
import Data.List (isInfixOf)

-- Fungsi Pembantu untuk mencetak satu baris produk
printProductRow :: Product -> IO ()
printProductRow p = putStrLn $
    "| " ++ padLeft 2 (show (productId p))
    ++ " | " ++ padRight 20 (productName p)
    ++ " | Rp" ++ padLeft 9 (formatNum (productPrice p))
    ++ " | " ++ padLeft 5 (show (productStock p)) ++ " |"

-- Menampilkan seluruh katalog barang
lihatDaftarBarang :: Catalog -> IO ()
lihatDaftarBarang catalog
    | null catalog = printWarning "Belum ada barang di katalog."
    | otherwise = do
        printSectionGap
        putStrLn "+----+----------------------+---------------+-------+"
        putStrLn "| ID | Nama Barang          | Harga         | Stok  |"
        putStrLn "+----+----------------------+---------------+-------+"
        -- HOF: mapM_ (monadic map) untuk mencetak tiap elemen list ke IO action
        mapM_ printProductRow catalog 
        putStrLn "+----+----------------------+---------------+-------+"
        printSectionGap
        printKeyValue "Total barang" (show (length catalog) ++ " item")

-- Menampilkan katalog barang (khusus untuk hasil filter/pencarian)
lihatDaftarBarangList :: Catalog -> IO ()
lihatDaftarBarangList filtered
    | null filtered = printWarning "Tidak ada barang yang cocok dengan pencarian."
    | otherwise = do
        printSectionGap
        putStrLn "+----+----------------------+---------------+-------+"
        putStrLn "| ID | Nama Barang          | Harga         | Stok  |"
        putStrLn "+----+----------------------+---------------+-------+"
        mapM_ printProductRow filtered
        putStrLn "+----+----------------------+---------------+-------+"
        printSectionGap
        printKeyValue "Ditemukan" (show (length filtered) ++ " item")

-- Menambah barang baru
tambahBarang :: Catalog -> IO Catalog
tambahBarang catalog = do
    printSectionGap
    printHeader "Tambah Barang Baru"
    printSectionGap
    putStr "Nama barang      : "; nama <- getLine
    putStr "Harga satuan (Rp): "; hargaStr <- getLine
    putStr "Stok awal        : "; stokStr <- getLine
    
    let newId = if null catalog then 1 else productId (last catalog) + 1
        -- Wajib menggunakan readInt dari Utils.hs
        harga = readInt hargaStr 0 
        stok  = readInt stokStr 0  

    if null nama || harga <= 0 || stok < 0
        then do
            putStrLn "[!] Input tidak valid. Nama tidak boleh kosong, harga > 0, stok >= 0."
            return catalog
        else do
            let prod = Product newId nama harga stok
            printSectionGap
            printSuccess $ "'" ++ nama ++ "' berhasil ditambahkan (ID: " ++ show newId ++ ")."
            return (catalog ++ [prod])

-- Mengedit data barang
editBarang :: Catalog -> IO Catalog
editBarang catalog = do
    lihatDaftarBarang catalog
    printSectionGap
    putStr "\nID barang yang akan diedit: "; idStr <- getLine
    let targetId = readInt idStr 0
        -- HOF: filter
        found = filter (\p -> productId p == targetId) catalog
        
    if null found
        then do 
            printWarning "Barang tidak ditemukan."
            return catalog
        else do
            let lama = head found
            putStrLn $ "\nMengedit: " ++ productName lama
            putStr "Nama baru  (Enter=skip): "; namaBaru <- getLine
            putStr "Harga baru (Enter=skip): "; hargaStr <- getLine
            putStr "Stok baru  (Enter=skip): "; stokStr <- getLine
            
            let finalNama  = if null namaBaru then productName lama else namaBaru
                finalHarga = if null hargaStr then productPrice lama else readInt hargaStr (productPrice lama)
                finalStok  = if null stokStr  then productStock lama else readInt stokStr (productStock lama)
                
                -- HOF: map untuk update spesifik 1 elemen
                newCatalog = map (\p -> 
                                if productId p == targetId 
                                then p { productName = finalNama, productPrice = finalHarga, productStock = finalStok }
                                else p
                            ) catalog
                            
            printSectionGap
            printSuccess "Barang berhasil diperbarui."
            return newCatalog

-- Menghapus barang
hapusBarang :: Catalog -> IO Catalog
hapusBarang catalog = do
    lihatDaftarBarang catalog
    printSectionGap
    putStr "\nID barang yang akan dihapus: "; idStr <- getLine
    let targetId = readInt idStr 0
        -- HOF: filter untuk membuang barang
        newCatalog = filter (\p -> productId p /= targetId) catalog
        
    if length newCatalog == length catalog
        then printWarning "Barang tidak ditemukan."
        else do
            printSectionGap
            printSuccess "Barang berhasil dihapus."
    return newCatalog

-- Mencari barang
cariBarang :: Catalog -> IO ()
cariBarang catalog = do
    printSectionGap
    putStr "Kata kunci pencarian: "; keyword <- getLine
    let kw = map toLower keyword -- HOF: map pada string
        -- HOF: filter dan map
        results = filter (\p -> kw `isInfixOf` map toLower (productName p)) catalog
    lihatDaftarBarangList results
