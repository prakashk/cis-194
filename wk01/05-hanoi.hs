-- CIS 194 -- Week 01

-- Exercise 05

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)

-- Exercise 06
-- use four pegs instead of three

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 2 a b c _ = [(a, c), (a, b), (c, b)]
hanoi4 3 a b c d = [(a, c), (a, d), (a, b), (d, b), (c, b)]
hanoi4 n a b c d = (hanoi4 (n-2) a c b d) ++ (hanoi4 2 a b c d) ++ (hanoi4 (n-2) c b a d)
