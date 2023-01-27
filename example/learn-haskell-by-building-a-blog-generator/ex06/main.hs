import Html

main :: IO ()
main = putStrLn (render (html_ "<title>" (p_ "<body>")))
