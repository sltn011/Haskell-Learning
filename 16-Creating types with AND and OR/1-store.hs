type FirstName = String
type MiddleName = String
type LastName = String

data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName deriving Show

data Author = Author Name deriving Show

type BandName = String

data Artist = Artist Name | Band BandName deriving Show

data Book = Book {
      author :: Author
    , bookTitle :: String
    , bookPrice :: Double
} deriving Show

data Disk = Disk {
      artist :: Artist
    , albumTitle :: String
    , albumPrice :: Double  
} deriving Show

data StoreItem = BookItem Book | DiskItem Disk deriving Show

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (DiskItem disk) = albumPrice disk