import qualified Data.Map as Map
import Data.Map(Map)
import Graphics.Gloss.Interface.Simulate
import Graphics.Gloss.Data.Picture
import RigidBody

data World = World (Map Index RigidBody)
instance Show World where
  show (World bodies) = "{World " ++ show (Map.size bodies) ++ "}"

-- todo: use statemonad to give indices to bodies

--config
windowSize = (600, 600)
simResolution = 600 -- fps
gravity = -980 -- gravity constant

-- | The initial world
worldInit :: World
worldInit = World $ Map.union ballInit ground

bodiesListToMap :: [RigidBody] -> Map Index RigidBody
bodiesListToMap = Map.fromList . (map (\a -> (index a, a)))

ballInit = bodiesListToMap $ [ dynamic { shape = ThickCircle 10 40, mass = 5, position = (0,30), index = 1 } ]

ground = bodiesListToMap $ [ static { position = (0,-350), shape = box 1000 30 } ]
  where box w h = Polygon $ [ (-w/2, -h/2) -- bottom-left
                            , (-w/2,  h/2) -- top-left
                            , ( w/2,  h/2) -- top-right
                            , ( w/2, -h/2) -- bottom-right
                            ]

main = do
  putStrLn "Starting"

  simulateInWindow
    "Hello World"
    windowSize    -- x and y size of window (in pixels).
    (0, 0)        -- position of window
    black         -- background color
    simResolution -- simulation resolution
                  -- (number of steps to take for each second of time)
    worldInit     -- the initial world.
    drawWorld     -- a function to convert the world to a Picture.
    advanceWorld  -- a function to advance the world to the next simulation step.

  putStrLn "Exiting"

bodyColor = makeColor 0.5 0.5 1.0 1.0

drawWorld :: World -> Picture
drawWorld (World bodies) = Color bodyColor $ Pictures $
                           map drawBody (Map.elems bodies)
  where drawBody b = let
          (px, py) = position b
          in
           Translate px py $ Pictures [(shape b)]

advanceWorld :: ViewPort -- ^ current viewport
                -> Time  -- ^ time to advance them for.
                -> World -- ^ the world to advance.
                -> World -- ^ the new world.
advanceWorld viewport time (World bodies)
  = let
  outsideForces = [(0, gravity)]
  moved_bodies = Map.map (applyForces time outsideForces) bodies
  in World moved_bodies
