import Data.List
import Data.Maybe
main :: IO ()
main  = do c <- getLine
           putStrLn $ show (map (\x-> replace "ACGT" "TGCA" x) $ reverse c)
replace::Eq a=>[a]->[a]->a->a
replace a b x
  | isJust i  = b!!(fromJust i)
  | otherwise = x
  where i = elemIndex x a
