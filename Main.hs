import qualified Data.Map as Map
import Data.Map(Map)
import Graphics.Gloss.Interface.Game
import Graphics.Gloss.Data.Picture
import RigidBody

data World = World (Map Index RigidBody)
instance Show World where
  show (World bodies) = "{World " ++ show (Map.size bodies) ++ "}"

-- todo: use StateMonad to give indices to bodies


--config
windowSize = (600, 600)
simResolution = 600 -- fps

-- | The initial world
worldInit :: World
worldInit = World $ Map.union (ballInit 30) rulers

bodiesListToMap :: [RigidBody] -> Map Index RigidBody
bodiesListToMap = Map.fromList . (map (\a -> (index a, a)))

ballInit m = bodiesListToMap $ map (makeBall m) [0..m]

makeBall m n = dynamic { shape = color bodyColor (ThickCircle 5 8)
                         , mass = 5
                         , position = pos
                         , index = floor n+14000
                         }
          where pos = (-m/2*spacer + n*spacer, 200) -- centered, spaced out
                spacer = 1150 / m

rulerColor = makeColor8 240 240 177 255
lineColor = red

makeRuler w h = Pictures ( [ color rulerColor (rectangleSolid w h) ] ++ rulerLines )
  where rulerLines = map line [-w/2,-w/2+10..w/2]
        line x
          | (round x) `mod` 100 == 0 = color lineColor (Line [(x, -h/2), (x, h/2)])
          | (round x) `mod` 50  == 0 = color lineColor (Line [(x, -h/2), (x,   0)])
          | otherwise                = color lineColor (Line [(x, -h/2), (x,-h/4)])

rulers = bodiesListToMap [static {position = (0,370), index = 100, shape = rotate 180 (makeRuler 1400 50)}
                         ,static {position = (0,-370), index = 101, shape = (makeRuler 1400 50)}
                         ,static {position = (-615,0), index = 102, shape = rotate 90 (makeRuler 700 50)}
                         ,static {position = (615,0), index = 103, shape = rotate 270 (makeRuler 700 50)}
                         ]

main = do
  putStrLn "Starting"

  gameInWindow
    "Hello World"
    windowSize    -- x and y size of window (in pixels).
    (0, 0)        -- position of window
    black         -- background color
    simResolution -- simulation resolution
                  -- (number of steps to take for each second of time)
    worldInit     -- the initial world.
    drawWorld     -- a function to convert the world to a Picture.
    handleInput   -- a function to handle incoming keyboard/mouse/etc. input
    advanceWorld  -- a function to advance the world to the next simulation step.

bodyColor = makeColor 0.5 0.5 1.0 1.0

drawWorld :: World -> Picture
drawWorld (World bodies) = Pictures $ map drawBody (Map.elems bodies)
  where drawBody b = let
          (px, py) = position b
          in
           Translate px py $ Pictures [(shape b)]

handleInput :: (Event -> World -> World)
-- r : Reset the whole world to the initial World.
handleInput (EventKey key keyState _ _) _
        | Char 'r'                    <- key
        , Down                        <- keyState
        = worldInit
handleInput _ w = w

advanceWorld ::    Time  -- ^ time to advance them for.
                -> World -- ^ the world to advance.
                -> World -- ^ the new world.
advanceWorld time (World bodies)
  = let
  outsideForces = [gravity, wind]
    where gravity = (0, -980 / 2) -- gravity constant
          wind = (30,0) -- apparently the wind is blowing to +x-axis
  moved_bodies = Map.map (applyForces time outsideForces) bodies
  in World moved_bodies
