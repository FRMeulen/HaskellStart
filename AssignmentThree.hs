module AssignmentThree
    where
        import Data.List
        
        --Differentieer d.m.v. differentiequotiënt
        differentieer::(Double->Double)->Double->Double->Double
        differentieer f p x = (f(x + p) - f(x)) / p
        
        --Integreer d.m.v. riemannintegratie
        integreer::(Double->Double)->Double->Double->Double->Double
        integreer f a b p =
            foldr (\l r->(dx * (f(a + (l * dx)))) + r) 0 [0..p-1]
            where dx = (b-a)/p
        
        --Scan een lijst voor dubbele entries
        dubbelen::(Ord a) => [a]->[a]
        dubbelen s = sort(nub((s) \\ (nub s)))
        
        --Stenen
        s = [1..6]
        stenen = [[a,b,c,d,e]|a<-s,b<-s,c<-s,d<-s,e<-s]
        
        --Count
        count::Integer->[Integer]->Integer
        count c [] = 0
        count c (x:xs)
            |c==x = 1 + (count c xs)
            |otherwise = count c xs
        
        --Convert list
        convert list = ([a,b,c,d,e,f],list) where
            a = count 1 list
            b = count 2 list
            c = count 3 list
            d = count 4 list
            e = count 5 list
            f = count 6 list
        
        --Zelfde
        zelfde::Integer->[[Integer]]->[[Integer]]
        zelfde x list = filter(elem x) $map fst(map convert list)
        
        --Poker / 5 dezelfde
        poker = (fromIntegral totaalPoker) / (fromIntegral (length stenen))
            where
                totaalPoker = length pokerList 
                pokerList = filter (elem 5) (map fst (map convert stenen))
        
        --Four of a kind / 4 dezelfde
        fourOfaKind = (fromIntegral totaalFourOfaKind) / (fromIntegral (length stenen))
            where
                totaalFourOfaKind = length fourOfaKindList
                fourOfaKindList = filter (elem 4) (map fst (map convert stenen))
        
        --Three of a kind / 3 dezelfde
        threeOfaKind = (fromIntegral totaalThreeOfaKind) / (fromIntegral (length stenen))
            where
                totaalThreeOfaKind = length threeOfaKindList
                threeOfaKindList = filter (elem 3) (map fst (map convert stenen))
        
        --Full house / 3 of a kind + 2 of a kind
        fullHouse = (fromIntegral totaalFullHouse) / (fromIntegral (length stenen))
            where
                totaalFullHouse = length fullHouseList
                fullHouseList = filter (elem 3) (map fst (map convert twoList))
                twoList = zelfde 2 stenen
        
        --Two pair / 2x 2 dezelfde
        twoPair = (fromIntegral totaalTwoPair) / (fromIntegral (length stenen))
            where
                totaalTwoPair = length twoPairList
                twoPairList = filter (elem 2) (map fst (map convert twoList))
                twoList = zelfde 2 stenen
        
        --One Pair / 2 dezelfde
        onePair = (fromIntegral totaalOnePair) / (fromIntegral (length stenen))
            where
                totaalOnePair = length onePairList
                onePairList = filter (elem 2) (map fst (map convert stenen))