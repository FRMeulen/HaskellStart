module AssignmentTwo
    where
        import Data.Char
    
        --Find greatest common divider
        euclid::Integer->Integer->Integer
        euclid x 0 = x
        euclid x y = euclid y (x `mod` y)
    
        --Extended greatest common divider (given)
        egcd::Integer->Integer->(Integer,Integer,Integer)
        egcd 0 b = (b, 0, 1)
        egcd a b =
            let (g, s, t) = egcd (b `mod` a) a
            in (g, t - (b `div` a) * s, s)
    
        --Pick middle result from tuple
        middle::(Integer,Integer,Integer)->Integer
        middle (_,b,_) = b
    
        --My version of egcd that won't return negatives
        myegcd a b
            |x <0 = 
                let (g, s, t) = egcd (b `mod` a) a
                in (g, t - (b `div` a) * s, s)
            |otherwise = egcd a b
            where x = middle(egcd a b)
    
        --Check for prime
        prime::Integer->Bool
        prime n = (n > 1)&&all(\ x-> rem n x/=0) [2..n-1]
    
        --Show prime numbers
        showPrimes::[Integer]
        showPrimes = [x|x<-[100..500], prime x]
    
        --Choose two primes
        p = 157
        q = 389
            --Picked from showPrimes
    
        --Calculate m
        m = p * q
            --Equals 61073
    
        --Calculate m'
        m' = (p-1) * (q-1)
            --Equals 60528
    
        --Show valid values for e
        showValidEs::[Integer]
        showValidEs = [x|x<-[1..m'], gcd x m' == 1]
    
        --Calculate e, e must be smaller than and relatively prime with m'
        e = 31903
            --Picked from showValidEs
    
        --Calculate d
        d = middle(myegcd e m')
            --Equals 28159
    
        --Pick first from two
        first (a,_) = a
    
        --Pick second from two
        second (_,b) = b
    
        --m is the modulus
        --e is the private key
        --d is the public key
    
        --Encrypt
        rsaEncrypt::(Integer,Integer)->Integer->Integer
        rsaEncrypt (e, m) x = (x^e) `mod` m
    
        --Decrypt
        rsaDecrypt::(Integer,Integer)->Integer->Integer
        rsaDecrypt (d, m) x = (x^d) `mod` m
        
        --Encrypt a char
        charEncrypt::Char->Integer
        charEncrypt y = 
            let x = toInteger(ord y)
            in rsaEncrypt(e, m) x
        
        --Decrypt a char
        charDecrypt::Integer->Char
        charDecrypt y = 
            let x = fromIntegral(rsaDecrypt (d, m) y)
            in chr x