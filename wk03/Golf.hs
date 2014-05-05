module Golf where

-- Exercise 01
-- Hopscotch

-- skips :: [a] -> [[a]]
-- skips xs = map (\n -> nth n xs) [1..l]
--   where l = length xs
--         nth n xs = map snd $ filter (\(i, x) -> i `rem` n == 0) $ zip [1..l] xs

      -- nth n = (map snd) . (filter (\(i, x) -> i `rem` n == 0)) . (zip ns)
      -- nth n xs = map snd $ filter (\(i, x) -> i `rem` n == 0) $ zip ns xs

skips xs =
  let ns = [1..length xs]
      nth n = (map snd) . (filter (\x -> fst x `rem` n == 0)) . (zip ns)
  in map (\n -> nth n xs) ns


-- Exercise 02
-- Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_, x, _) -> x) $ filter (\(a, b, c) -> b > a && b > c) $ zip3 xs (tail xs) (drop 2 xs)
