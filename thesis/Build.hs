import Control.Monad (replicateM_,void)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

pdflatex :: String
pdflatex = "pdflatex"

bibtex :: String
bibtex = "bibtex"

pdflatexArgs :: String
pdflatexArgs = "-interaction batchmode -synctex=1 -halt-on-error --shell-escape"

main :: IO ()
main = shakeArgs shakeOptions $ do
  want ["thesis.pdf"]
  "*.tex" %> \out -> return ()
  "*.pdf" %> \out -> (do
     let inp = out -<.> "tex"
     includes <- map extractInclude . filter isInclude <$> readFileLines inp
     need includes
     () <- cmd Shell [pdflatex] pdflatexArgs [inp] "> /dev/null"
     () <- cmd Shell [bibtex] [dropExtension inp] "> /dev/null"
     () <- cmd Shell [pdflatex] pdflatexArgs [inp] "> /dev/null"
     () <- cmd Shell [pdflatex] pdflatexArgs [inp] "> /dev/null"
     return ())

isInclude :: String -> Bool
isInclude = ("\\include" `isPrefixOf`) . dropWhile isSpace

extractInclude :: String -> String
extractInclude = (<.> "tex") . dropTillBrace
  where dropTillBrace ('{':cs) = takeTillBrace "" cs
        dropTillBrace (_:cs) = dropTillBrace cs
        takeTillBrace acc ('}':_) = acc
        takeTillBrace acc (c:cs) = takeTillBrace (acc ++ [c]) cs
