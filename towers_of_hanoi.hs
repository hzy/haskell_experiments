type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 r1 r2 r3 = [(r1, r2)]
hanoi n r1 r2 r3 = (hanoi (n - 1) r1 r3 r2) ++ (hanoi 1 r1 r2 r3) ++ (hanoi (n - 1) r3 r2 r1)

main = do
    print (hanoi 2 "a" "b" "c")
    print (hanoi 3 "a" "b" "c")
