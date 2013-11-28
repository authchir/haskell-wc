module Main (main) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSChar8
import Data.Int
import Data.List
import System.Console.GetOpt
import System.Environment
import System.IO
import Text.Printf

type LineCount = Int64
type WordCount = Int64
type CharCount = Int64
type Padding = Int

data Flag = Help | Version
          deriving (Show)

options :: [OptDescr Flag]
options =
    [ Option [] ["help"] (NoArg Help) "display this help and exit"
    , Option [] ["version"] (NoArg Version) "output version information and exit"
    ]

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        (opts,nonOpts,[]) -> runWithOptionsAndArgs opts nonOpts --mapM_ print opts >> putStrLn "" >> mapM_ print nonOpts
        (_,_,errs) -> hPutStr stdout $ "Error:\n" ++ intercalate "\n" errs

runWithOptionsAndArgs :: [Flag] -> [String] -> IO ()
runWithOptionsAndArgs (Help:_) _ = printHelp
runWithOptionsAndArgs (Version:_) _ = putStrLn "haskell-wc 0.5"
runWithOptionsAndArgs [] [] = printHelp
runWithOptionsAndArgs [] args = 
    case args of 
        [file] -> fmap (wcFile file) (BS.readFile file) >>= putStrLn
        files -> fmap (wcFiles files) (mapM BS.readFile files) >>= putStrLn
    where wcFile name = intercalate "\n" . init . processContents [name] . (:[])
          wcFiles names = intercalate "\n" . processContents names

printHelp :: IO ()
printHelp = fmap ((flip usageInfo) options . ("usage: "++) . (++" [OPTION]... file...")) getProgName >>= putStr

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
