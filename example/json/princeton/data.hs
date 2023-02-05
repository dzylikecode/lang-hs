data JVal
  = JStr String
  | JNum Double
  | JBln Bool
  | JObj [(String, JVal)]
  | JArr [JVal]
  deriving (Eq, Ord, Show)

-- SERIALIZING HASKELL VALUES TO JSON

doubleToJSON :: Double -> JVal
doubleToJSON = JNum

stringToJSON :: String -> JVal
stringToJSON = JStr

boolToJSON :: Bool -> JVal
boolToJSON = JBln

doublesToJSON :: [Double] -> JVal
doublesToJSON = JArr . map doubleToJSON

boolsToJSON :: [Bool] -> JVal
boolsToJSON = JArr . map boolToJSON

stringsToJSON :: [String] -> JVal
stringsToJSON = JArr . map stringToJSON

xsToJSON :: (a -> JVal) -> [a] -> JVal
xsToJSON f = JArr . map f

xysToJSON :: (a -> JVal) -> [(String, a)] -> JVal
xysToJSON f = JObj . map (second f)