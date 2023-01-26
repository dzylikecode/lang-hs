import Geometry
  ( cubeArea,
    cubeVolume,
    cuboidArea,
    cuboidVolume,
    sphereArea,
    sphereVolume,
  )

main :: IO ()
main = do
  print (sphereVolume 2)
  print (sphereArea 2)
  print (cubeVolume 2)
  print (cubeArea 2)
  print (cuboidVolume 2 3 4)
  print (cuboidArea 2 3 4)