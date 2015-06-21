import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Development.Shake
import Development.Shake.FilePath

latexmk :: String
latexmk = "latexmk"

latexmkArgs :: [String]
latexmkArgs = ["-r", ".latexmkrc"]

main :: IO ()
main = shakeArgs shakeOptions { shakeThreads = 4 } $ do
  want ["thesis.pdf"]
  "*.tex" %> \_ -> return ()
  "*.pdf" %> \out -> (do
     let inp = out -<.> "tex"
     includes <- map extractInclude . filter isInclude <$> readFileLines inp
     need $ "bibliography.bib" : includes
     cmd latexmk latexmkArgs [inp])

isInclude :: String -> Bool
isInclude = ("\\include" `isPrefixOf`) . dropWhile isSpace

extractInclude :: String -> String
extractInclude = (<.> "tex") . dropTillBrace
  where dropTillBrace ('{':cs) = takeTillBrace "" cs
        dropTillBrace (_:cs) = dropTillBrace cs
        takeTillBrace acc ('}':_) = acc
        takeTillBrace acc (c:cs) = takeTillBrace (acc ++ [c]) cs
