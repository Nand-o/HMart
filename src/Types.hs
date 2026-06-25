module Types where

-- Tipe Data Product (Barang)
data Product = Product
    { productId :: Int
    , productName :: String
    , productPrice :: Int
    , productStock :: Int
    } deriving (Show, Eq)

-- Tipe Data CartItem (Item dalam Keranjang)
data CartItem = CartItem
    { cartProduct :: Product
    , cartQty :: Int
    } deriving (Show, Eq)

-- Tipe Data Transaction (Riwayat Transaksi)
data Transaction = Transaction
    { transactionId :: Int
    , transactionItems :: [CartItem]
    , transactionTotal :: Int
    , transactionPaid :: Int
    , transactionChange :: Int
    , transactionDate :: String
    } deriving (Show, Eq)

-- Type Aliases (untuk mempermudah penggunaan tipe data)
type Catalog = [Product]
type Cart    = [CartItem]
type History = [Transaction]
