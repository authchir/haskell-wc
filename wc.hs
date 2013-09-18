import Data.List
import System.Environment
import Text.Printf

type LineCount = Int
type WordCount = Int
type CharCount = Int
type Padding = Int

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: wc file..."
        [file] -> fmap (wcFile file) (readFile file) >>= putStrLn
        files -> fmap (wcFiles files) (mapM readFile files) >>= putStrLn
    where wcFile name content = (intercalate "\n" . init . processContents [name]) [content]
          wcFiles names = intercalate "\n" . processContents names

processContents :: [FilePath] -> [String] -> [String]
processContents files contents = formatWcAll padding files stats
    where stats = wcAll contents
          padding = maxPadding stats

wc :: String -> (LineCount,WordCount,CharCount)
wc content = (l,w,c)
    where l= (length . lines) content
          w= (length . words) content
          c= length content

wcAll :: [String] -> [(LineCount,WordCount,CharCount)]
wcAll = map wc

formatWc :: Padding -> FilePath -> (LineCount,WordCount,CharCount) -> String
formatWc padding path (l,w,c) = printf "%*d %*d %*d %s" padding l padding w padding c path

formatWcAll :: Padding -> [FilePath] -> [(LineCount,WordCount,CharCount)] -> [String]
formatWcAll padding files stats = filesStats ++ [totalStats]
    where filesStats = map formatWc' $ zip files stats
          totalStats = formatWc padding "total" $ foldr addTriples (0,0,0) stats
          formatWc' = uncurry (formatWc padding)
          addTriples (a,b,c) (d,e,f) = (a+d,b+e,c+f)

maxPadding :: [(LineCount,WordCount,CharCount)] -> Int
maxPadding = foldr f 0
    where f (_,_,c) = max $ (length . show) c
