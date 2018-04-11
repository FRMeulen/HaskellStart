module AssignmentFive
    where
        --Imports
        import Data.List
        import Data.Char
        
        --
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

        instance Functor List where
            fmap function Nope = Nope
            fmap function (Head s rest) = Head(function s)(fmap function rest)

        --Test lists
        testList = Head 5 (Head 7 (Head 9 (Head 2 Nope)))
        testListTwo = Head 'r' (Head 'e' (Head 'p' (Head 'y' Nope)))

        --Books for tests
        booka = Book 9000.00 "Book A" "A. Uthor"
        bookb = Book 25.75 "Book B" "B. Rother"
        bookc = Book 150.50 "Book C" "C. Rafter"
        bookd = Book 14.95 "Book D" "D. River"
        booke = Book 61.99 "Book E" "E. Raser"

        --Book list
        booksList = [bookd, bookc, booke, booka, bookb]

        --Number list
        numbersList = [0..100]
 
        packBox::something -> Box something    --Pack something in a Box
        packBox something = Box something

        packBoxes::[something] -> [Box something]   --Pack a list of Boxes
        packBoxes somethings = map packBox somethings

        unpackBox::(Box something) -> something    --Unpack a Box
        unpackBox (Box something) = something

        unpackBoxes::[(Box something)] -> [something]   --Unpack a list of Boxes
        unpackBoxes boxes = map unpackBox boxes

        packSack::something -> Sack something  --Pack a Sack
        packSack something = Sack something

        packSacks::[something] -> [Sack something]  --Pack a list of Sacks
        packSacks somethings = map packSack somethings

        unpackSack::(Sack something) -> something   --Unpack a Sack
        unpackSack (Sack something) = something

        unpackSacks::[(Sack something)]->[something]    --Unpack a list of Sacks
        unpackSacks sacks = map unpackSack sacks

        packBookSack::Book->(Box (Sack Book))   --Pack a Book in a Sack in a Box
        packBookSack book = packBox sackedBook
            where sackedBook = packSack book

        packBookSacks::[Book]->[(Box (Sack Book))]  --Pack a list of Books in Sacks in Boxes
        packBookSacks books = map packBookSack books

        packNumber::Integer->Box Integer    --Pack a number in a Box
        packNumber number = Box number

        packNumbers::[Integer] -> [(Box Integer)]   --Pack a list of numbers in Boxes
        packNumbers numbers = map packNumber numbers

        push::a -> List a -> List a     --Push an item to a List
        push a Nope = Head a Nope
        push a (Head h rest) = Head a (Head h rest)

        pushList::List a -> [a] -> List a   --Push a list of items to a List
        pushList Nope list = foldr push Nope list
        pushList (Head h rest) list = foldr push (Head h rest) list

        boxToSack::Box a -> Sack a  --Swap a Box for a Sack
        boxToSack (Box a) = (Sack a)

        convertBoxList::List (Box a) -> List (Sack a)   --Swap a List of Boxes for a List of Sacks
        convertBoxList Nope = Nope
        convertBoxList list = fmap boxToSack list