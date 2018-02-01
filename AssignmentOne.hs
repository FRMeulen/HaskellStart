module OpdrachtEen
    where
    
    --Bereken som
    som x y z = x+y+z
    
    --Bereken vermenigvuldiging
    mult x y = x*y
    
    --Faculteit pattern matching
    faca::Int->Int
    faca 0 = 1
    faca x = x*faca(x-1)

    --Faculteit guards
    facb::Int->Int
    facb x
        |x<=0 = 1
        |otherwise = x*facb(x-1)

    --Nulpunten pattern matching
    nulpuntena::Double->Double->Double->[Double]
    nulpuntena a b c =
        if (b^2-4*a*c) == 0
            then [(-b/2*a)]
        else if (b^2-4*a*c) > 0
            then [((-b + sqrt(b^2 - 4*a*c))/2*a),((-b - sqrt(b^2 - 4*a*c))/2*a)]
        else []
    
    --Nulpunten guards
    nulpuntenb::Double->Double->Double->[Double]
    nulpuntenb a b c
        |d==0 = [(-b/2*a)]
        |d>0 = [((-b + sqrt(d))/2*a),((-b - sqrt(d))/2*a)]
        |otherwise = []
        where d = (b^2-4*a*c)

    --Dobbelstenen
    dobbelstenen:: Int-> [(Int, Int, Int)]
    dobbelstenen x = [(a,b,c)|a<-[1..6],b<-[1..6],c<-[1..6],som a b c `mod` x==0]