module Newick where 
import Text.Parsec
import Text.Parsec.String
import Data.Tree
import Data.Set (Set, difference, fromList, size)
import Rosalind
type NewickTree = (Tree Info)
type Info = (String,Int)

newickTree :: String -> Tree Info
newickTree s = right $ parse newickParser "" s
  where right (Right x) = x

newickNameTree :: String -> Tree String
newickNameTree s = fmap fst (newickTree s)
tag :: t1->Tree (t, t1) -> Tree (t, t1)
tag l (Node (n,_) ls)  = Node (n,l) ls 

-- This parser ASSUMES that whitespace has been prefiltered from the input.
newickParser :: Parser (Tree Info)
newickParser = 
   do x <- subtree
      l <- len
      char ';'
      return $ tag l x

subtree :: Parser (Tree Info)
subtree = internal <|> leaf

leaf :: Parser (Tree Info)
leaf = do n<-name; return$ Node (n,1) []

internal :: Parser (Tree Info)
internal = do char '('
              bs <- branchset
              char ')'
              n <- name -- IGNORED
              return$ Node (n,1) bs

branchset :: Parser [Tree Info]
branchset =
    do b <- branch <?> "at least one branch"
       rest <- option [] $ try$ do char ','; branchset
       return (b:rest)

branch :: Parser (Tree Info)
branch = do s<-subtree; l<-len; 
	    return$ tag l s

len :: Parser Int
len = option 1 $ do char ':'; number

number :: Parser Int
number = 
  do sign <- option "" $ string "-"
     num <- many1 digit
     return (read (sign ++ num) :: Int)

name :: Parser String
name = option "" $ many1 (letter <|> digit <|> oneOf "_.-")

splits :: NewickTree -> [(Set String, Set String)]
splits t = map (\(x,y)-> (min x y,max x y)) $ 
           filter (\(x,y)-> size x >1 && size y>1) pair 
    where l    = fromList $ leafLabels $ fmap fst t
          cand = map (fromList.leafLabels) $ subTrees $ fmap fst t
          pair = zip (map (l `difference`) cand) cand

leafLabels :: Tree a -> [a]
leafLabels (Node x forest) 
  | null forest = [x]
  | otherwise   = concatMap leafLabels forest
printNewickTree :: Tree String -> String
printNewickTree x = p x++";"
  where p (Node a []) = a
        p (Node a xs) = "("++join (map p xs)++")"++a
        join [y] = y
        join (y:ys) = y++","++join ys
