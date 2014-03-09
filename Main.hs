{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Arrow ((***), second)
import Control.Lens

import Control.Monad.State

import Data.Array.Accelerate as A hiding (use)
import Data.Array.Accelerate.Smart (untup2)

import Data.Vect.Floating
import Data.Vect.Floating.Util.Quaternion

import Data.Char (toLower)

import Data.Complex
import Data.Complex.Lens

import Graphics.Gloss
import Graphics.Gloss.Accelerate.Raster.Field as R
import Graphics.Gloss.Interface.Pure.Game as G

import Prelude as P

import System.IO.Unsafe (unsafePerformIO)

import RayAcc (yaw, pitch, buildVector, checkIntersection, explore, BoundingSphere(..))
import JuliaAcc (exploreJulia)

type ViewPoint = Vec3 Float
type ViewDirection = U Float
type ScreenSize = (Int,Int)
type MouseLocation = (Float,Float)
type Keys = Vec3 (Complex Float)

keyVector :: IndexPreservingGetter Keys (Vec3 Float)
keyVector = to keyVector'
  where keyVector' (Vec3 xc yc zc) = Vec3 (xc ^. _imagPart) (yc ^. _imagPart) (zc ^. _imagPart)

data Ray =
  Ray
  { _viewP :: ViewPoint
  , _viewD :: ViewDirection
  , _keys :: Keys
  , _screenDim :: ScreenSize
  , _mouse :: MouseLocation
  , _prevMouse :: MouseLocation
  , _viewQ :: Quaternion Float
  } deriving Show
  
makeLenses ''Ray

xlens :: Lens' (Vec3 a) a
xlens = lens (\(Vec3 x _ _) -> x) (\(Vec3 _ y z) x -> Vec3 x y z)

ylens :: Lens' (Vec3 a) a
ylens = lens (\(Vec3 _ y _) -> y) (\(Vec3 x _ z) y -> Vec3 x y z)

zlens :: Lens' (Vec3 a) a
zlens = lens (\(Vec3 _ _ z) -> z) (\(Vec3 x y _) z -> Vec3 x y z)

qxLens :: Lens' (Quaternion a) a
qxLens = lens (\(Q (Vec4 x y z w)) -> x) (\(Q (Vec4 _ y z w)) x -> toQ $ Vec4 x y z w)

qyLens :: Lens' (Quaternion a) a
qyLens = lens (\(Q (Vec4 x y z w)) -> y) (\(Q (Vec4 x _ z w)) y -> toQ $ Vec4 x y z w)

qzLens :: Lens' (Quaternion a) a
qzLens = lens (\(Q (Vec4 x y z w)) -> z) (\(Q (Vec4 x y _ w)) z -> toQ $ Vec4 x y z w)

qwLens :: Lens' (Quaternion a) a
qwLens = lens (\(Q (Vec4 x y z w)) -> w) (\(Q (Vec4 x y z _)) w -> toQ $ Vec4 x y z w)

myDisplay :: Display
myDisplay = InWindow "Dog" (1600,1600) (1600,1600)

myColor :: G.Color
myColor = G.black

mySteps :: Int
mySteps = 60

type World = (Scalar (U Float), Scalar (Vec3 Float), Scalar (Quaternion Float))

myInitState :: Ray
myInitState = Ray viewP viewD k screen mouse prevMouse quaternion
  where k = Vec3 (1 :+ 0) (1 :+ 0) (1 :+ 0)
        screen = (100,100)
        mouse = (0,0)
        prevMouse = (0,0)
        viewP = (Vec3 0 0 0)
        viewD = U $ Vec4 0 1 0 0
        quaternion = initQuaternion
        
initQuaternion = toQ $ Vec4 (-0.2) 0.6 0.2 0.2
        
myEventHandler :: Event -> Ray -> Ray
myEventHandler (EventMotion motion) = (mouse .~ motion)
myEventHandler (EventKey key keyState mod _) = (viewQ %~ qHandler) . (keys %~ keyHandler)
  where keyHandler :: Vec3 (Complex Float) -> Vec3 (Complex Float)
        keyHandler =
          case key of
            Char c -> case toLower c of
              'w' -> ylens %~ (* s)
              's' -> ylens %~ (* conjugate s)
              'a' -> xlens %~ (* conjugate s)
              'd' -> xlens %~ (* s)
              _ -> id
              where s = case keyState of
                      Down -> 0 :+ 1
                      Up -> 0 :+ (-1)
            SpecialKey KeySpace -> case G.shift mod of
              Down -> zlens .~ conjugate s
              Up -> zlens .~ s
              where s = case keyState of
                      Down -> 0 :+ 1
                      Up -> 0 :+ 0
            _ -> id
            
        qHandler :: Quaternion Float -> Quaternion Float
        qHandler = 
          case key of
            Char c -> case c of
              'V' -> qxLens +~ step
              'v' -> qxLens -~ step
              'B' -> qyLens +~ step
              'b' -> qyLens -~ step
              'N' -> qzLens +~ step
              'n' -> qzLens -~ step
              'M' -> qwLens +~ step
              'm' -> qwLens -~ step
              'r' -> const initQuaternion
              _ -> id
              where step = -0.05
            _ -> id
              
                    
myEventHandler (EventResize r) = screenDim .~ r
{-# INLINE myEventHandler #-}

myIterationHandler :: Float -> Ray -> Ray
myIterationHandler = execState . myIterationHandler'
{-# INLINE myIterationHandler #-}

myIterationHandler' :: Float -> State Ray ()
myIterationHandler' seconds = do
  (xrad,yrad) <- mouseDiffInRadians
  viewD' <- viewD <%= (.*. (rotU' yaw xrad .*. rotU' pitch yrad))
  direction <- fmap (actU viewD') $ use (keys.keyVector)
  viewP %= (&+ direction &* (seconds * 2))
  return ()
{-# INLINE myIterationHandler' #-}
  
-- Return and reset the mouse movement difference in radians
mouseDiffInRadians :: State Ray (Float,Float)
mouseDiffInRadians = do
  curr <- use mouse
  prev <- prevMouse <<.= curr
  let diff = liftT2 subtract prev curr
  fmap ((both *~ (2 * pi)) . liftT2 normalizeP diff) $ use screenDim
    where normalizeP x dim = x / P.fromIntegral dim * 2
          liftT2 :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
          liftT2 f = P.uncurry (***) . join (***) f
{-# INLINE mouseDiffInRadians #-}
          
myStateToWorld :: Ray -> World
myStateToWorld s = (d,p,q)
  where d = fromList Z [s ^. viewD]
        p = fromList Z [s ^. viewP]
        q = fromList Z [s ^. viewQ]
        
g :: Acc World -> Exp R.Point -> Exp R.Color
g accWorld point = bool &&* bool2 ? (R.black, R.white)
  where (x,z) = xyOfPoint point
        (d',p') = buildVector (Vec3 x 0 z) (-10)
        (d,p,q) = unliftWorld accWorld
--          where unliftWorld :: Acc World -> (U (Exp Float), Vec3 (Exp Float))
--                unliftWorld a = (unlift . the) *** (unlift . the) $ unlift a
        (d'',p'') = (actU' d d', actU d p' &+ p)
        (bool,ps) = checkIntersection bSphere d'' p''
        (bool2, _) = explore bSphere d'' ps
        rot = unitU
        bSphere = BoundingSphere ((Vec3 0 3 0, 3, rot)) f
          where f d2 p2 = second unlift . untup2 $ exploreJulia (A.lift q) (A.lift d2) (A.lift p2)
                

unliftWorld :: Acc World -> (U (Exp Float), Vec3 (Exp Float), Quaternion (Exp Float))
unliftWorld accWorld = (unlift $ the d, unlift $ the p, unlift $ the q)
  where (d,p,q) = unlift accWorld

main :: IO ()
main = playField myDisplay (1,1) mySteps myInitState myStateToWorld g myEventHandler myIterationHandler
