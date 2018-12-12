module BitFont (printLetters) where

type Letter = [String]
type Letters = [String]

printLetters :: String -> IO ()
printLetters xs = do
    a <- readLetters xs
    mapM_ putStrLn $ composeLetter space $ foldr1 composeLetter a
    putStrLn ""
    mapM_ putStrLn $ foldr1 composeLetter a

space = ["     ", "    ", "   ", "  ", " ", ""]

readLetters :: String -> IO [Letter]
readLetters = mapM readLetter
    where
        readLetter c = fmap lines <$> readFile $ "src/bit-font/large/" ++ [c]

composeLetter :: Letter -> Letter -> Letters
composeLetter a [] = a
composeLetter a b = (head a ++ "  " ++ head b) : composeLetter (tail a) (tail b)
