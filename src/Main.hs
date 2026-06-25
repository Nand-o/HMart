module Main where

import Types
import Utils
import Products
import Cart
import Transactions
import Reports
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

-- | 1. Data Default untuk Simulasi (Agar saat presentasi tidak perlu input manual dari nol)
defaultProducts :: Catalog
defaultProducts = 
    [ Product 1 "Indomie Goreng" 3500 50
    , Product 2 "Susu Ultra 1L" 12000 30
    , Product 3 "Roti Tawar Sari" 15000 20
    , Product 4 "Teh Botol Sosro" 5000 40
    , Product 5 "Sabun Mandi Lifebuoy" 8000 25
    , Product 6 "Minyak Goreng 1L" 18000 15
    , Product 7 "Gula Pasir 1Kg" 14000 10
    , Product 8 "Kopi Kapal Api" 3000 60
    , Product 9 "Sampo Pantene" 12000 3  -- Stok sengaja rendah untuk demo laporan
    , Product 10 "Detergen Rinso" 10000 2 -- Stok sengaja rendah untuk demo laporan
    ]

-- | 2. Loop Sub-Menu: Kelola Barang
menuKelolaBarang :: Catalog -> IO Catalog
menuKelolaBarang catalog = do
    printSectionGap
    printHeader "Kelola Barang"
    printSectionGap
    printKeyValue "Barang terdaftar" (show (length catalog) ++ " item")
    printSectionGap
    putStrLn "  1. Lihat Daftar Barang"
    putStrLn "  2. Tambah Barang"
    putStrLn "  3. Edit Barang"
    putStrLn "  4. Hapus Barang"
    putStrLn "  5. Cari Barang"
    putStrLn "  0. Kembali ke Menu Utama"
    putStr "Pilih [0-5]: "
    
    choice <- getLine
    -- Pattern Matching untuk mengeksekusi aksi
    case choice of
        "1" -> lihatDaftarBarang catalog >> menuKelolaBarang catalog
        "2" -> tambahBarang catalog >>= menuKelolaBarang
        "3" -> editBarang catalog >>= menuKelolaBarang
        "4" -> hapusBarang catalog >>= menuKelolaBarang
        "5" -> cariBarang catalog >> menuKelolaBarang catalog
        "0" -> return catalog -- Keluar dari rekursi, kembalikan state catalog ke Main Menu
        _   -> do 
            printWarning "Pilihan tidak valid."
            menuKelolaBarang catalog

-- | 3. Loop Sub-Menu: Transaksi Penjualan
menuTransaksi :: Catalog -> Cart -> History -> IO (Catalog, History)
menuTransaksi catalog cart history = do
    printSectionGap
    printHeader "Transaksi Penjualan"
    printSectionGap
    printKeyValue "Keranjang aktif" (show (length cart) ++ " item")
    printKeyValue "Total belanja" ("Rp " ++ formatNum (hitungTotal cart))
    printSectionGap
    putStrLn "  1. Tambah Barang ke Keranjang"
    putStrLn "  2. Lihat Keranjang"
    putStrLn "  3. Hapus Barang dari Keranjang"
    putStrLn "  4. Checkout"
    putStrLn "  5. Batalkan Transaksi"
    putStrLn "  0. Kembali ke Menu Utama"
    putStr "Pilih [0-5]: "
    
    choice <- getLine
    case choice of
        "1" -> do
            newCart <- tambahKeKeranjang catalog cart
            menuTransaksi catalog newCart history
        "2" -> do
            lihatKeranjang cart
            menuTransaksi catalog cart history
        "3" -> do
            newCart <- hapusDariKeranjang cart
            menuTransaksi catalog newCart history
        "4" -> do
            if null cart
                then do
                    printWarning "Keranjang kosong. Tambahkan barang terlebih dahulu."
                    menuTransaksi catalog cart history
                else checkout catalog cart history -- checkout me-return (Catalog, History)
        "5" -> do
            printSectionGap
            printSuccess "Transaksi dibatalkan. Keranjang dikosongkan."
            printSectionGap
            return (catalog, history)
        "0" -> return (catalog, history)
        _   -> do 
            printWarning "Pilihan tidak valid."
            menuTransaksi catalog cart history

-- | 4. Loop Sub-Menu: Riwayat Transaksi
menuRiwayat :: History -> IO ()
menuRiwayat history = do
    printSectionGap
    printHeader "Riwayat Transaksi"
    printSectionGap
    printKeyValue "Total transaksi" (show (length history) ++ " item")
    printSectionGap
    putStrLn "  1. Lihat Semua Transaksi"
    putStrLn "  2. Cari Transaksi Berdasarkan ID"
    putStrLn "  3. Detail Transaksi"
    putStrLn "  0. Kembali ke Menu Utama"
    putStr "Pilih [0-3]: "
    
    choice <- getLine
    case choice of
        "1" -> lihatSemuaTransaksi history >> menuRiwayat history
        "2" -> cariTransaksi history >> menuRiwayat history
        "3" -> detailTransaksi history >> menuRiwayat history
        "0" -> return ()
        _   -> do 
            printWarning "Pilihan tidak valid."
            menuRiwayat history

-- | 5. Loop Sub-Menu: Laporan Penjualan
menuLaporan :: Catalog -> History -> IO ()
menuLaporan catalog history = do
    printSectionGap
    printHeader "Laporan Penjualan"
    printSectionGap
    putStrLn "  1. Total Penjualan (Keseluruhan)"
    putStrLn "  2. Barang Terlaris"
    putStrLn "  3. Stok Barang Menipis"
    putStrLn "  4. Pendapatan (Total Uang Masuk)"
    putStrLn "  0. Kembali ke Menu Utama"
    putStr "Pilih [0-4]: "
    
    choice <- getLine
    case choice of
        "1" -> totalPenjualan history >> menuLaporan catalog history
        "2" -> barangTerlaris history >> menuLaporan catalog history
        "3" -> stokMenipis catalog >> menuLaporan catalog history
        "4" -> pendapatan history >> menuLaporan catalog history
        "0" -> return ()
        _   -> do 
            printWarning "Pilihan tidak valid."
            menuLaporan catalog history

-- | 6. Informasi Statis Tentang Program
tentangProgram :: IO ()
tentangProgram = do
    printSectionGap
    printHeader "HMart Cashier System"
    printSectionGap
    printKeyValue "Versi" "1.0.0"
    printKeyValue "Bahasa" "Haskell (GHC)"
    printKeyValue "Mata Kuliah" "Pemrograman Deklaratif"
    printKeyValue "Kelompok" "1"
    printSectionGap
    putStrLn "  Anggota"
    putStrLn "    1. Ernando Febrian           - 1313624021"
    putStrLn "    2. Candra Afriansyah         - 1313624023"
    putStrLn "    3. Sukarno Adi Prasetyo      - 1313624010"
    putStrLn "    4. Nandana Ammar Triabimanyu - 1313624030"
    printSectionGap
    putStrLn "  Konsep Haskell"
    putStrLn "    [v] Rekursi & Pure Functions"
    putStrLn "    [v] Map, Filter, Foldl (HOF)"
    putStrLn "    [v] List Comprehension"
    putStrLn "    [v] Algebraic Data Types & Pattern Matching"

-- | 7. Loop Menu Utama (Jantung Program)
mainMenu :: Catalog -> Cart -> History -> IO ()
mainMenu catalog cart history = do
    printSectionGap
    printHeader "HMart Cashier"
    printSectionGap
    putStrLn "  1. Kelola Barang"
    putStrLn "  2. Transaksi Penjualan"
    putStrLn "  3. Riwayat Transaksi"
    putStrLn "  4. Laporan Penjualan"
    putStrLn "  5. Tentang Program"
    putStrLn "  0. Keluar"
    putStr "Pilih menu [0-5]: "
    
    choice <- getLine
    case choice of
        "1" -> do
            newCatalog <- menuKelolaBarang catalog
            mainMenu newCatalog cart history
        "2" -> do
            (newCatalog, newHistory) <- menuTransaksi catalog cart history
            mainMenu newCatalog [] newHistory -- Cart dikosongkan setelah kembali ke Main Menu
        "3" -> do
            menuRiwayat history
            mainMenu catalog cart history
        "4" -> do
            menuLaporan catalog history
            mainMenu catalog cart history
        "5" -> do
            tentangProgram
            mainMenu catalog cart history
        "0" -> printExitScreen
        _   -> do
            printWarning "Pilihan tidak valid. Masukkan angka 0-5."
            mainMenu catalog cart history

-- | 8. Entry Point Aplikasi
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- Agar output input selaras di terminal
    putStrLn "\nSelamat datang di HMart Cashier System!"
    -- Inisialisasi awal program: catalog terisi data dummy, cart kosong, history kosong
    mainMenu defaultProducts [] []
