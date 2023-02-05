import data

-- The typeclass approach is more flexible, 
-- because it allows us to define a single function     
-- that can be used to convert any type to JSON. 

class JSON a where
  toJSON :: a -> JVal

instance JSON Double where
  toJSON = JNum

instance JSON String where
  toJSON = JStr

instance JSON Bool where
  toJSON = JBln

instance JSON a => JSON [a] where
  toJSON = JArr . map toJSON

instance JSON a => JSON [(String, a)] where
  toJSON = JObj . map (second toJSON)

