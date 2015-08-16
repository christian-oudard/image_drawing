-- TODO: antialiasing

import Codec.Picture (writePng, generateImage)
import Codec.Picture.Types (Image, PixelYA8(..))

type Coord = (Int, Int)
type Point = (Double, Double)
type Vector = (Double, Double)

type Shape = Point -> Bool


main = do
  let image = (translate (0.5, 0) . scaleX 0.5) (circle 1)
  let grid = (Grid (-1.0) (1.0) (-1.0) (1.0) 50)

  writePng "test.png" (render image grid)


-- Grid --

data Grid = Grid {
  left :: Double,
  right :: Double,
  bottom :: Double,
  top :: Double,
  resolution :: Double -- px per unit
  } deriving Show

pxSize :: Grid -> Coord
pxSize grid = (floor xSize, floor ySize)
  where xSize = (right grid - left grid) * resolution grid
        ySize = (top grid - bottom grid) * resolution grid

pxCenter :: Grid -> Coord -> Point
pxCenter grid (lx, ly) = (x, y)
  where x = left grid + pixelSize * fromIntegral lx + (pixelSize / 2)
        y = bottom grid + pixelSize * fromIntegral ly + (pixelSize / 2)
        pixelSize = 1 / resolution grid


-- Rendering --

render :: Shape -> Grid -> Image PixelYA8
render shape grid = generateImage f xSize ySize
  where f x y = sample shape grid (x, y)
        (xSize, ySize) = pxSize grid

sample :: Shape -> Grid -> Coord -> PixelYA8
sample shape grid location = if shape (pxCenter grid location) then pixel 0 else pixel 1

pixel :: Float -> PixelYA8
pixel y
  | (y < 0.0) || (y > 1.0) = error "Pixel value outside of [0..1]"
  | otherwise  = PixelYA8 (round (y * 255)) 255


-- Geometry --

black :: Shape
black p = True

white :: Shape
white p = False

circle :: Double -> Shape
circle r = \(x, y) -> x^2 + y^2 < r^2

scaleX :: Double -> Shape -> Shape
scaleX s shape = \(x, y) -> shape (x/s, y)

scaleY :: Double -> Shape -> Shape
scaleY s shape = \(x, y) -> shape (x, y/s)

translate :: Vector -> Shape -> Shape
translate (dx, dy) shape = \(x, y) -> shape (x-dx, y-dy)
