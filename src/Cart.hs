module Cart where

import Types
import Utils
import Products (lihatDaftarBarang)
import Data.List (findIndex)

-- Fungsi Pembantu untuk mencetak satu baris item keranjang
printCartRow :: (Int, CartItem) -> IO ()
printCartRow (no, item) = do
    let p      = cartProduct item
        qty    = cartQty item
        subtot = productPrice p * qty
    putStrLn $
        "| " ++ padLeft 3 (show no)
        ++ " | " ++ padRight 20 (productName p)
        ++ " | " ++ padLeft 4 (show qty)
        ++ " | Rp" ++ padLeft 9 (formatNum subtot) ++ " |"

-- Menampilkan isi keranjang belanja
lihatKeranjang :: Cart -> IO ()
lihatKeranjang cart
    | null cart = printWarning "Keranjang belanja kosong."
    | otherwise = do
        printSectionGap
        putStrLn "+-----+----------------------+------+---------------+"
        putStrLn "| No. | Nama Barang          | Qty  | Subtotal      |"
        putStrLn "+-----+----------------------+------+---------------+"
        -- HOF: mapM_ dan zip
        mapM_ printCartRow (zip [1..] cart)
        putStrLn "+-----+----------------------+------+---------------+"
        let total = hitungTotal cart
        printSectionGap
        printKeyValue "TOTAL BELANJA" ("Rp " ++ formatNum total)

-- Pure Function: Menghitung total keranjang menggunakan foldl
hitungTotal :: Cart -> Int
hitungTotal cart = foldl (\acc item -> acc + (productPrice (cartProduct item) * cartQty item)) 0 cart -- HOF: foldl

-- Menambah atau memperbarui item ke keranjang
tambahKeKeranjang :: Catalog -> Cart -> IO Cart
tambahKeKeranjang catalog cart = do
    lihatDaftarBarang catalog
    printSectionGap
    putStr "\nID barang: "; idStr <- getLine
    putStr "Jumlah   : "; qtyStr <- getLine
    
    let targetId = readInt idStr 0
        qty      = readInt qtyStr 0
        found    = filter (\p -> productId p == targetId) catalog -- HOF: filter
        
    if qty <= 0
        then do 
            printWarning "Jumlah harus lebih dari 0."
            return cart
        else case found of
            [] -> do 
                printWarning "Barang tidak ditemukan."
                return cart
            (p:_) -> do
                -- LIST COMPREHENSION (LC-2): Hitung jumlah barang ini yang sudah ada di keranjang
                let sudahDiKeranjang = sum [cartQty c | c <- cart, productId (cartProduct c) == targetId]
                    totalQty = sudahDiKeranjang + qty
                    
                if totalQty > productStock p
                    then do
                        printWarning $ "Stok tidak mencukupi. Stok tersedia: " ++ show (productStock p)
                        return cart
                    else do
                        let alreadyIn = findIndex (\c -> productId (cartProduct c) == targetId) cart
                        case alreadyIn of
                            Nothing -> do
                                printSectionGap
                                printSuccess $ productName p ++ " ditambahkan ke keranjang."
                                return (cart ++ [CartItem p qty])
                            Just _ -> do
                                -- HOF: map untuk update QTY item yang sudah ada
                                let newCart = map (\c -> 
                                                if productId (cartProduct c) == targetId
                                                then c { cartQty = cartQty c + qty }
                                                else c
                                            ) cart
                                printSectionGap
                                printSuccess "Jumlah barang di keranjang diperbarui."
                                return newCart

-- Menghapus item dari keranjang menggunakan List Comprehension
hapusDariKeranjang :: Cart -> IO Cart
hapusDariKeranjang cart
    | null cart = do 
        printWarning "Keranjang sudah kosong."
        return cart
    | otherwise = do
        lihatKeranjang cart
        printSectionGap
        putStr "\nNomor urut item yang ingin dihapus: "; noStr <- getLine
        let no = readInt noStr 0
        if no < 1 || no > length cart
            then do 
                printWarning "Nomor urut tidak valid."
                return cart
            else do
                -- LIST COMPREHENSION (LC-1): Ambil semua item yang nomor urutnya BUKAN 'no'
                let newCart = [item | (i, item) <- zip [1..] cart, i /= no]
                printSectionGap
                printSuccess "Item berhasil dihapus dari keranjang."
                return newCart

-- Checkout dan simpan transaksi
checkout :: Catalog -> Cart -> History -> IO (Catalog, History)
checkout catalog cart history = do
    let total = hitungTotal cart
    printSectionGap
    printHeader "CHECKOUT"
    lihatKeranjang cart
    printSectionGap
    printKeyValue "TOTAL BELANJA" ("Rp " ++ formatNum total)
    putStr "Uang pelanggan: Rp"; paidStr <- getLine
    let paid = readInt paidStr 0
    
    if paid < total
        then do
            printSectionGap
            printWarning $ "Uang kurang! Kurang Rp" ++ formatNum (total - paid)
            putStrLn "Silakan batalkan transaksi atau hapus barang jika perlu."
            return (catalog, history)
        else do
            let change = paid - total
            let newTransId = length history + 1
            timestamp <- getTimeStamp
            
            let newTrans = Transaction newTransId cart total paid change timestamp
                newHistory = history ++ [newTrans]
                
                -- HOF: map & filter untuk mengurangi stok barang di katalog utama
                newCatalog = map (\p -> 
                                case filter (\c -> productId (cartProduct c) == productId p) cart of
                                    []    -> p
                                    (c:_) -> p { productStock = productStock p - cartQty c }
                            ) catalog
            printSectionGap
            printKeyValue "Kembalian" ("Rp " ++ formatNum change)
            printSectionGap
            printSuccess "Transaksi berhasil! Terima kasih."
            printSectionGap
            return (newCatalog, newHistory)
