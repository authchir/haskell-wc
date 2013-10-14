import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSChar8
import Data.Int
import Data.List
import System.Environment
import Text.Printf

type LineCount = Int64
type WordCount = Int64
type CharCount = Int64
type Padding = Int

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: wc file..."
        [file] -> fmap (wcFile file) (BS.readFile file) >>= putStrLn
        files -> fmap (wcFiles files) (mapM BS.readFile files) >>= putStrLn
    where wcFile name = intercalate "\n" . init . processContents [name] . (:[])
          wcFiles names = intercalate "\n" . processContents names

processContents :: [FilePath] -> [BS.ByteString] -> [String]
processContents files contents = formatWcAll padding files stats
    where stats = wcAll contents
          padding = maxPadding stats

wc :: BS.ByteString -> (LineCount,WordCount,CharCount)
wc content = (l,w,c)
    where l= (fromIntegral . length . BSChar8.lines) content
          w= (fromIntegral . length . BSChar8.words) content
          c= BS.length content

wcAll :: [BS.ByteString] -> [(LineCount,WordCount,CharCount)]
wcAll = map wc

formatWc :: Padding -> FilePath -> (LineCount,WordCount,CharCount) -> String
formatWc padding path (l,w,c) = printf "%*d %*d %*d %s" padding l padding w padding c path

formatWcAll :: Padding -> [FilePath] -> [(LineCount,WordCount,CharCount)] -> [String]
formatWcAll padding files stats = filesStats ++ [totalStats]
    where filesStats = map formatWc' $ zip files stats
          totalStats = formatWc padding "total" $ foldr addTriples (0,0,0) stats
          formatWc' = uncurry (formatWc padding)
          addTriples (a,b,c) (d,e,f) = (a+d,b+e,c+f)

maxPadding :: [(LineCount,WordCount,CharCount)] -> Padding
maxPadding = foldr f 0
    where f (_,_,c) = max $ (length . show) c
