

ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m n-1)

acks :: [[Int]]
acks = [ [ case (m, n) of
            (0,_) -> n + 1
            (_, 0) -> acks !! (m - 1) !! 1
            (_, _) -> acks !! (m - 1) !! (acks !! m !! (n - 1))
            | n <- [0..] ]
            | m <- [0..] ]

