import Data.List (intercalate)
import Data.List.Split (splitOn)

convert = intercalate "-" . splitOn "_"

main :: IO ()
main = interact convert