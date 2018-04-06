module AssignmentFive
    where
        import Data.List
        --import Data.Maybe
        type Price = Float
        type Title = String
        type Author = String

        data Book = Book Price Title Author
            deriving (Show)

        data Box something = Box something | Empty
            deriving (Show)

        data Sack something = Sack something | Nada
            deriving (Show)

        data List something = Head something (List something) | Nope
            deriving (Show)

        instance Eq Book where
            (Book a b c) == (Book d e f) = a == d && b == e && c == f

        instance Ord Book where
            compare (Book a b c) (Book d e f) = compare b e
            (Book a b c) <= (Book d e f) = b <= e

        instance Functor Box where
            fmap function Empty = Empty
            fmap function (Box something) = Box (function something)

        instance Functor Sack where
            fmap function Nada = Nada
            fmap function (Sack something) = Sack (function something)

        --Books for tests
        booka = Book 9000.00 "Book A" "A. Uthor"
        bookb = Book 25.75 "Book B" "B. Rother"
        bookc = Book 150.50 "Book C" "C. Rafter"
        bookd = Book 14.95 "Book D" "D. River"
        booke = Book 61.99 "Book E" "E. Raser"

        --Book list
        booksList = [bookd, bookc, booke, booka, bookb]
 
        packBox::something -> Box something
        packBox something = Box something

        packBoxes::[something] -> [Box something]
        packBoxes somethings = map packBox somethings

        unpackBox::(Box something) -> something
        unpackBox (Box something) = something

        unpackBoxes::[(Box something)] -> [something]
        unpackBoxes boxes = map unpackBox boxes

        packSack::something -> Sack something
        packSack something = Sack something

        packSacks::[something] -> [Sack something]
        packSacks somethings = map packSack somethings

        unpackSack::(Sack something) -> something
        unpackSack (Sack something) = something

        unpackSacks::[(Sack something)]->[something]
        unpackSacks sacks = map unpackSack sacks

        packBookSack::Book->(Box (Sack Book))
        packBookSack book = packBox sackedBook
            where sackedBook = packSack book

        packBookSacks::[Book]->[(Box (Sack Book))]
        packBookSacks books = map packBookSack books