module RigidBody
       where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import Vector

type Time = Float
type Index = Int
type Mass = Float
type Force = Vector

data RigidBody = Dynamic { index    :: !Index
                         , position :: !Vector
                         , mass     :: !Mass
                         , velocity :: !Vector -- linear_velocity
                         , spin     :: !Vector -- angular_velocity
                         , shape    :: Picture
                         }
               | Static { index :: !Index
                        , position :: !Vector
                        , shape    :: Picture
                 }
                 deriving (Show)

-- | Equality and ordering will consider their index only.
--      We need Ord so we can put them in Maps and Sets.
instance Eq RigidBody where
  x == y = index x == index y

instance Ord RigidBody where
  compare x y = compare (index x) (index y)


dynamic = Dynamic 0 (0,0) 0.0 (0,0) (0,0) Blank
static = Static 0 (0,0) Blank

applyForces :: Time ->  [Force] -> RigidBody -> RigidBody
applyForces _ _ b@(Static _ _ _) = b
applyForces t fs b@(Dynamic i p m v s sh ) =
  let
    force = foldr1 (<+>) fs -- sum of forces
    a = (1 / m) <*> force
    p' = p <+> (t <*> v) <+> ((0.5 * t * t) <*> a)
    v' = v <+> (t <*> a)
    -- todo: calculate angular velocity
  in
   Dynamic i p' m v' s sh
