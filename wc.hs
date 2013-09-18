import Data.List
import System.Environment
import Text.Printf

type LineCount = Int
type WordCount = Int
type CharCount = Int

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: wc file..."
        files -> do
            contents <- mapM readFile files
            let stats = map wc contents
                padding = maxPadding stats
                filesStats = map (uncurry (formatWc padding)) $ zip files stats
                totalStats = formatWc padding "total" $ foldr addTriples (0,0,0) stats
                allStats = filesStats ++ [totalStats]
            putStrLn $ intercalate "\n" allStats
            where addTriples (a,b,c) (d,e,f) = (a+d,b+e,c+f)

wc :: String -> (LineCount,WordCount,CharCount)
wc content = (l,w,c)
    where l= (length . lines) content
          w= (length . words) content
          c= length content

formatWc :: Int -> FilePath -> (LineCount,WordCount,CharCount) -> String
formatWc padding path (l,w,c) = printf "%*d %*d %*d %s" padding l padding w padding c path

maxPadding :: [(LineCount,WordCount,CharCount)] -> Int
maxPadding = foldr calcPadding 0
    where calcPadding (_,_,c) = max $ (length . show) c
