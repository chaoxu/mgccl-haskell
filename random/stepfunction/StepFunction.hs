module StepFunction ( StepFunction, eval, toStepFunction, fromStepFunction, compress, intervalNotation, values) where
import Control.Arrow
import Control.Applicative
import Data.Monoid
data Bound a = Minimum | Value a | Maximum  deriving (Eq, Ord, Show)
type Point a b = (Bound a, b)
data StepFunction a b = Step [Point a b] 
    deriving (Show, Eq, Ord)

instance (Ord x, Monoid y) => Monoid (StepFunction x y) where
  mempty = Step [(Minimum,mempty)]
  mappend x y = fmap mappend x <*> y

instance Functor (StepFunction a) where
  fmap f (Step xs) = Step (map (second f) xs)

instance (Ord a) => Applicative (StepFunction a) where
  pure f  = Step [(Minimum, f)]
  (Step fs) <*> (Step xs) =  Step (applyStepFunction (fs++[(Maximum,f)]) (xs++[(Maximum,x)]))
    where (_,x) = head xs
          (_,f) = head fs

compress :: (Ord a, Eq b)=>StepFunction a b->StepFunction a b
compress (Step xs) = Step (merge xs)
  where merge ((a,b):(c,d):xs)
         | b == d    = merge ((a,b):xs)
         | otherwise = (a,b):merge ((c,d):xs)
        merge x = x

toStepFunction :: (Ord a) => ([a],[b]) -> StepFunction a b
toStepFunction (as,bs)
 | isIncreasing Minimum as = Step (zip (Minimum:map Value as) bs)
 | otherwise               = error "Not a increasing sequence of points."

fromStepFunction :: (Ord a) => StepFunction a b -> ([a],[b])
fromStepFunction (Step xs) = (map (\(Value a,_)->a) (tail xs), map snd xs)

eval :: (Ord a) => StepFunction a b -> a -> b
eval (Step xs) x = snd $ fst $ head $ dropWhile (\((a,_),(b,_))-> a > Value x || b < Value x) z
  where z = zip xs (tail xs ++ end xs)

isIncreasing :: (Ord a)=>Bound a->[a]->Bool
isIncreasing p (x:xs)
 | p < Value x = isIncreasing (Value x) xs
 | otherwise   = False
isIncreasing _ [] = True

end :: [Point a b] -> [Point a b]
end xs = [(Maximum, snd (head xs))]

-- in the beginning, both are at Minimum, we will maintain invariant that 
-- the head of both list have the same starting point.
applyStepFunction :: Ord a => [Point a (b->c)]->[Point a b]->[Point a c]
applyStepFunction ((begin, f):fs) ((_,x):xs) 
  | begin == Maximum = []
  | endf <  endx  = fx:applyStepFunction fs ((endf,x):xs)
  | endf == endx  = fx:applyStepFunction fs xs
  | endf >  endx  = fx:applyStepFunction ((endx,f):fs) xs
  where (endf, _) = head fs
        (endx, _) = head xs
        fx        = (begin, f x)

intervalNotation :: Ord a => StepFunction a b -> [(a,a,b)]
intervalNotation s
 | null a = []
 | otherwise = zip3 (init a) (tail a) (init $ tail b)
    where (a,b) = fromStepFunction s

values :: Ord a => StepFunction a b -> [b]
values (Step xs) = map snd xs
