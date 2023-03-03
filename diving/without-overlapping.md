# JSON typeclasses without overlapping instances

- source: https://book.realworldhaskell.org/read/using-typeclasses.html#jsonclass.instances

```hs
newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)

newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)

data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject (JObj JValue)   -- was [(String, JValue)]
  | JArray (JAry JValue)    -- was [JValue]
      deriving (Eq, Ord, Show)
```

```hs
jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)

jaryToJValue :: (JSON a) => JAry a -> JValue

instance (JSON a) => JSON (JAry a) where
    toJValue   = jaryToJValue
    fromJValue = jaryFromJValue
```

分步思考类型即可

```hs
jaryToJValue = JArray . JAry . map toJValue . fromJAry
```

!> `JAry`与`fromJAry`是`[a]`的同名

```hs
jaryFromJValue (JArray (JAry a)) =
  whenRight JAry . mapEithers fromJValue $ a
jaryFromJValue _ = Left "not a JSON array"
```

---

可以类比思考

```hs
import Control.Arrow (second)

instance (JSON a) => JSON (JObj a) where
  toJValue = JObject . JObj . map (second toJValue) . fromJObj

  fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
    where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
  fromJValue _ = Left "not a JSON object"
```
