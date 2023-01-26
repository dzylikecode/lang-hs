import Data.List (intercalate)
import Data.List.Split

convert = intercalate "-" . splitOn "_"

main = interact convert