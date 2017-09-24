-- See http://gitcommit.co.uk/2017/09/24/the-expressiveness-of-haskell-random-numbers/
import Data.Time.Clock.POSIX

numTodigs :: Integral a => a -> [a]
numTodigs 0 = []
numTodigs x = numTodigs (x `div` 10) ++ [x `mod` 10]

pad :: Integral a => [a] -> [a]
pad xs
  | length xs >= 8 = xs
  | otherwise = pad (0 : xs)

mid4 :: [a] -> [a]
mid4 = substr 4 4

substr :: Int -> Int -> [a] -> [a]
substr s e xs = take e (drop s xs)

digsToNum :: [Int] -> Int
digsToNum ds = eval (10 ^ l) ds
  where
    l = length ds - 1
    eval _ [] = 0
    eval n (x:xs) = (n * x) + eval (n `div` 10) xs

sq :: Int -> Int
sq = (^ 2)

vnRan :: Int -> Int
vnRan = digsToNum . mid4 . pad . numTodigs . sq

sysInt :: IO Int
sysInt = round . (* 1000) <$> getPOSIXTime

fourDigits :: Int -> Int
fourDigits n = n `mod` 10000

main = do
  n <- sysInt
  print $ take 150 $ iterate vnRan (fourDigits n)
