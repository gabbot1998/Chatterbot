module Chatterbot where
import Utilities
import System.Random
import Data.Char

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- TO BE WRITTEN -}
stateOfMind brain = do
  (map (\p -> ((fst p), (snd p) !! >>= (getRandom (length (snd p))))) brain)


rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply pp p = maybe (reflect p) id (transformationsApply "*" reflect (map (\x -> (fst x, snd x)) pp) p) -- returns p if Nothing (this funciton is eld)

getRandom :: Int -> IO Int
getRandom x = do
  r <- randomIO :: IO Float
  return (floor (r*(fromIntegral x) + 1))

--reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = map (switch reflections)

switch :: [(String, String)] -> String -> String
switch [] word  = word
switch ((s1, s2):ss) word
 | word == s1 = s2
 | otherwise = switch ss word

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN -}
rulesCompile _ = []


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wildcard (x:xs) sub
  | x == wildcard && xs == [] = sub
  | x == wildcard = sub ++ substitute wildcard xs sub
  | xs == [] = [x]
  | otherwise = [x] ++ substitute wildcard xs sub
{- TO BE WRITTEN -}


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match wildcard _ [] = Nothing
match wildcard [] _ = Nothing
match wildcard (p:ps) (x:xs)
  | wildcard == p = orElse (singleWildcardMatch (p:ps) (x:xs)) (longerWildcardMatch (p:ps) (x:xs))
  | p == x = match wildcard ps xs
  | otherwise = Nothing

--Bygga en model som jämför förlust per distans och vind modell


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x] ) (match wc ps xs)
longerWildcardMatch (wc:ps) (x:xs) = mmap (x: ) (match wc (wc:ps) xs)


--longerWildcardMatch (wc:ps) word
-- | matchLen == 1 = Nothing
-- | drop matchLen word == ps = Just (take (matchLen -1) word)
-- | otherwise = match wc ps word
-- where matchLen = (length word - length ps)


longerWildcardMatch (wc:ps) (x:xs)
  | ps == [] && xs /= [] = Just (x:xs)
  | xs == [] = Nothing
  | last ps == last xs = longerWildcardMatch (init (wc:ps)) (init (x:xs))
  | otherwise = Nothing

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wildcard f s (t1, t2) = mmap (substitute wildcard t2) (mmap f (match wildcard t1 s))
{- TO BE WRITTEN -}

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wildcard f tl@(t:ts) s
  | transformationApply wildcard f s t == Nothing = transformationsApply wildcard f ts s
  | transformationApply wildcard f s t /= Nothing = transformationApply wildcard f s t
  | otherwise = Nothing
