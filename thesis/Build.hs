import Data.Char (isSpace)
import Data.List (isPrefixOf, findIndex, isInfixOf)
import Data.Maybe (fromMaybe)
import Development.Shake
import Development.Shake.FilePath

latexmk :: String
latexmk = "latexmk"

latexmkArgs :: [String]
latexmkArgs = ["-r", ".latexmkrc"]

main :: IO ()
main = shakeArgs shakeOptions { shakeThreads = 4 } $ do
   examples
   texs
   pdfs
   want ["CryptoF.example","thesis.pdf"]

texs :: Rules ()
texs = "*.tex" %> const (return ())

pdfs :: Rules ()
pdfs = "*.pdf" %> \out -> (do
     let inp = out -<.> "tex"
     includes <- map extractInclude . filter isInclude <$> readFileLines inp
     need $ "bibliography.bib" : includes
     cmd latexmk latexmkArgs [inp])

examples :: Rules ()
examples = "*.example" %> \out -> (do
    let inp = out -<.> "snippet"
    [scalaFile,searchStart,searchEnd] <- readFileLines inp
    need [inp,scalaFile]
    fileContents <- readFileLines scalaFile
    let start = fromMaybe (error "start not found") $
          findIndex (searchStart `isInfixOf`) fileContents
        end = fromMaybe (error "end not found") $
          findIndex (searchEnd `isInfixOf`) fileContents
    writeFileLines out (take (end - start + 1) (drop start fileContents))
    )

isInclude :: String -> Bool
isInclude = ("\\include" `isPrefixOf`) . dropWhile isSpace

extractInclude :: String -> String
extractInclude = (<.> "tex") . dropTillBrace
  where dropTillBrace ('{':cs) = takeTillBrace "" cs
        dropTillBrace (_:cs) = dropTillBrace cs
        takeTillBrace acc ('}':_) = acc
        takeTillBrace acc (c:cs) = takeTillBrace (acc ++ [c]) cs
