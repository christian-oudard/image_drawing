import Codec.Picture (writePng, generateImage)
import Codec.Picture.Types (Image, PixelYA8(..))

type Coord = (Int, Int)
type Point = (Double, Double)
type Vector = (Double, Double)

type Shape = Point -> Bool

main = do
  let image = union (scaleX 0.5 (circle 0.7)) (scaleY 0.5 (circle 0.7))
  let grid = (Grid (-1.0) (1.0) (-1.0) (1.0) 150)
  writePng "test.png" (render image grid)


-- Grid --

data Grid = Grid {
  left :: Double,
  right :: Double,
  bottom :: Double,
  top :: Double,
  resolution :: Double -- px per unit
  } deriving Show

xSize grid = floor $ (right grid - left grid) * resolution grid
ySize grid = floor $ (top grid - bottom grid) * resolution grid

-- The width or height of one pixel of a grid.
pixelSize :: Grid -> Double
pixelSize grid = 1 / resolution grid

-- List the coordinates that a grid extends over.
gridCoords :: Grid -> [Coord]
gridCoords grid = [(x, y) | x <- [0..xSize grid - 1], y <- [0..ySize grid - 1]]

-- Find the point at the center of a given pixel.
pxCenter :: Grid -> Coord -> Point
pxCenter grid (lx, ly) = (x, y)
  where x = left grid + s * fromIntegral lx + (s / 2)
        y = bottom grid + s * fromIntegral ly + (s / 2)
        s = pixelSize grid

-- Create a grid that extends over exactly one pixel of the given grid.
pxSubGrid :: Grid -> Coord -> Int -> Grid
pxSubGrid grid (lx, ly) multiplier = Grid l (l+s) b (b+s) res
  where l = left grid + s * fromIntegral lx
        b = bottom grid + s * fromIntegral ly
        s = pixelSize grid
        res = resolution grid * fromIntegral multiplier


-- Rendering --

render :: Shape -> Grid -> Image PixelYA8
render shape grid = generateImage f (xSize grid) (ySize grid)
  where f x y = sample shape grid (x, y)

sample :: Shape -> Grid -> Coord -> PixelYA8
sample shape grid location = pixel $ 1 - (fromIntegral hits / fromIntegral total)
    where hits = length (filter shape ps)
          total = length ps
          subGrid = pxSubGrid grid location 3
          ps = map (pxCenter subGrid) (gridCoords subGrid)

pixel :: Double -> PixelYA8
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

union :: Shape -> Shape -> Shape
union a b = \p -> a p || b p

intersection :: Shape -> Shape -> Shape
intersection a b = \p -> a p && b p
