import qualified GeometryM.Cube as Cube
import qualified GeometryM.Cuboid as Cuboid
import qualified GeometryM.Sphere as Sphere

main :: IO ()
main = do
  print (Sphere.volume 2)
  print (Sphere.area 2)
  print (Cube.volume 2)
  print (Cube.area 2)
  print (Cuboid.volume 2 3 4)
  print (Cuboid.area 2 3 4)