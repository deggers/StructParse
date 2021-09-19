{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (lookup)
import Data.Map
import Data.Maybe
import Data.List (sort)

-- Receives argument via StdIn and can parse multiple newline separated structures
-- Call main with something like :: echo "((..))\n(())" | main
main :: IO ()
main = do
    ls <- fmap lines $ getContents
    mapM_ (print . stackParser) $ Prelude.filter (not . Prelude.null) ls

openBracket = "(<{[" ++ [ 'A' .. 'Z' ]
closBracket = ")>}]" ++ [ 'a' .. 'z' ]

matched :: Map Char Char
matched = fromList $ zip closBracket openBracket
-- only takes well formed strings equal amounts of open and bracket signs
stackParser :: String -> [(Int,Int)]
stackParser xs = sort $ go stack (zip xs [0..])
  where
    stack :: Map Char [Int]
    stack = empty
    go _ [] = []
    go stack ((x,j):xs)
      | x `elem` openBracket =
        let
          braStack :: [Int]
          braStack = findWithDefault [] x stack
          updStack = (j:braStack)
          newStack = insert x updStack stack
        in  go newStack xs
      | x `elem` closBracket =
        let
          openSymbol :: Char
          openSymbol = fromJust $ lookup x matched
          (i:braStack) = findWithDefault [] openSymbol stack
          newStack = insert openSymbol braStack stack
        in (i,j):go newStack xs
      | otherwise =   -- will skip
         go stack xs
