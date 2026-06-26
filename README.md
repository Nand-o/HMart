# HMart

HMart adalah aplikasi kasir berbasis CLI yang ditulis menggunakan Haskell. Proyek ini dibuat untuk memenuhi tugas akhir mata kuliah Pemrograman Deklaratif dan dirancang sebagai simulasi sederhana alur kerja kasir, mulai dari pengelolaan katalog barang hingga laporan penjualan.

## Daftar Isi

- [Gambaran Umum](#gambaran-umum)
- [Fitur](#fitur)
- [Kebutuhan Sistem](#kebutuhan-sistem)
- [Instalasi](#instalasi)
- [Menjalankan Aplikasi](#menjalankan-aplikasi)
- [Pengujian](#pengujian)
- [Panduan Penggunaan](#panduan-penggunaan)
- [Struktur Proyek](#struktur-proyek)
- [Catatan Implementasi](#catatan-implementasi)
- [Teknologi](#teknologi)
- [Kontributor](#kontributor)
- [Lisensi](#lisensi)

## Gambaran Umum

Program berjalan di terminal dan menyajikan menu interaktif untuk mengelola data barang, memproses transaksi, serta melihat riwayat dan laporan. Aplikasi sudah dibekali data awal agar dapat langsung dijalankan tanpa perlu input manual dari nol.

## Fitur

- Kelola katalog barang: lihat daftar barang, tambah data baru, edit, hapus, dan cari produk.
- Transaksi penjualan: tambah barang ke keranjang, lihat isi keranjang, hapus item, checkout, dan batalkan transaksi.
- Riwayat transaksi: lihat seluruh transaksi, cari transaksi berdasarkan ID, dan tampilkan detail transaksi.
- Laporan penjualan: total penjualan, barang terlaris, stok menipis, dan laporan pendapatan.
- Validasi input angka untuk mengurangi kesalahan input saat digunakan di terminal.
- Tampilan tabel ASCII dan warna terminal untuk meningkatkan keterbacaan.

## Kebutuhan Sistem

Pastikan lingkungan berikut sudah tersedia:

- [GHC](https://www.haskell.org/ghc/) sebagai compiler Haskell.
- [Cabal](https://www.haskell.org/cabal/) untuk build dan pengelolaan proyek.
- Terminal atau command prompt yang mendukung aplikasi CLI.

Dependensi yang digunakan proyek ini:

- `base`
- `time`

## Instalasi

### 1. Ambil kode sumber

```bash
git clone https://github.com/Nand-o/HMart.git
cd HMart
```

Jika Anda sudah berada di folder proyek, langkah ini dapat dilewati.

### 2. Verifikasi instalasi Haskell

Jalankan perintah berikut untuk memastikan Cabal dan GHC sudah tersedia:

```bash
cabal --version
ghc --version
```

Jika ini adalah pertama kali menggunakan Cabal di mesin Anda, perbarui indeks paket lokal:

```bash
cabal update
```

### 3. Bangun proyek

```bash
cabal build
```

Perintah ini akan mengompilasi aplikasi dan memastikan semua modul dapat dibangun dengan benar.

## Menjalankan Aplikasi

Setelah build selesai, jalankan aplikasi dengan:

```bash
cabal run hmart
```

Saat aplikasi dijalankan, menu utama akan tampil di terminal dan Anda bisa langsung mencoba seluruh fitur yang tersedia.

## Pengujian

Proyek ini menyediakan test suite sederhana untuk memeriksa utilitas dasar dan logika laporan.

```bash
cabal test hmart-test
```

## Panduan Penggunaan

### Menu Utama

Menu utama terdiri dari:

1. Kelola Barang
2. Transaksi Penjualan
3. Riwayat Transaksi
4. Laporan Penjualan
5. Tentang Program
0. Keluar

### Kelola Barang

Menu ini digunakan untuk mengelola katalog produk.

- Menampilkan daftar barang yang tersedia.
- Menambahkan barang baru beserta harga dan stok awal.
- Mengubah nama, harga, atau stok barang yang sudah ada.
- Menghapus barang dari katalog.
- Mencari barang berdasarkan kata kunci nama.

### Transaksi Penjualan

Menu ini digunakan untuk proses transaksi kasir.

- Menambahkan barang ke keranjang berdasarkan ID barang.
- Menampilkan isi keranjang dan subtotal tiap item.
- Menghapus item tertentu dari keranjang.
- Melakukan checkout untuk menyelesaikan transaksi dan mengurangi stok barang.
- Membatalkan transaksi dan mengosongkan keranjang.

Catatan: jika pembayaran kurang dari total belanja, transaksi tidak akan disimpan dan keranjang tetap dipertahankan agar pengguna dapat mencoba lagi tanpa memasukkan ulang item.

### Riwayat Transaksi

Menu ini menampilkan histori transaksi yang berhasil disimpan.

- Melihat semua transaksi.
- Mencari transaksi berdasarkan ID.
- Menampilkan detail item di dalam transaksi tertentu.

### Laporan Penjualan

Menu ini menyediakan ringkasan operasional.

- Total penjualan keseluruhan.
- Barang terlaris berdasarkan jumlah item terjual.
- Daftar stok barang yang menipis.
- Laporan pendapatan yang mencakup uang masuk, total penjualan, dan total kembalian.

### Tentang Program

Menu ini menampilkan informasi singkat mengenai aplikasi, versi, dan anggota kelompok.

## Struktur Proyek

```text
src/
  Main.hs          - titik masuk aplikasi dan navigasi menu
  Types.hs         - definisi tipe data utama
  Utils.hs         - helper umum, formatting, input, dan tampilan
  Products.hs      - pengelolaan katalog barang
  Cart.hs          - keranjang belanja dan checkout
  Transactions.hs  - riwayat dan detail transaksi
  Reports.hs       - laporan penjualan dan stok
test/
  Main.hs          - test suite sederhana
```

## Catatan Implementasi

- Data awal tersedia melalui `defaultProducts`, sehingga aplikasi bisa langsung dipakai untuk demo atau presentasi.
- Checkout akan memperbarui stok barang dan menambahkan transaksi ke riwayat.
- Jika checkout gagal karena uang kurang, katalog dan keranjang tidak direset, sehingga pengguna dapat mencoba ulang dengan cepat.
- Tampilan terminal memakai warna ANSI; hasil terbaik diperoleh pada terminal yang mendukung ANSI color.

## Teknologi

- Bahasa pemrograman: Haskell
- Compiler: GHC
- Build tool: Cabal
- Antarmuka: CLI / terminal

## Kontributor

Proyek ini dikerjakan oleh Kelompok 1:

- Ernando Febrian - 1313624021
- Candra Afriansyah - 1313624023
- Sukarno Adi Prasetyo - 1313624010
- Nandana Ammar Triabimanyu - 1313624030

## Lisensi

Proyek ini dibuat untuk kebutuhan akademik mata kuliah Pemrograman Deklaratif.
