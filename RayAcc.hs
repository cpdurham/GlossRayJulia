{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RayAcc where

import Control.Arrow
import Control.Monad

import Data.Function

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Smart

import Data.Vect.Floating
import Data.Vect.Floating.Util.Quaternion
import Data.Vect.Floating

import Data.Vect.Floating.Accelerate.Instances

type Scale a = a
type Res a = a
type Pixel a = a

type Position a = Vec3 a
type Direction a = Normal3 a
type Radius a = a

pitch :: Floating a => Normal3 a
pitch = mkNormal $ Vec3 1 0 0 

yaw :: Floating a => Normal3 a
yaw = mkNormal $ Vec3 0 0 (-1)

roll :: Floating a => Normal3 a
roll = mkNormal $ Vec3 0 1 0

buildVector :: Floating a => Position a -> a -> (Direction a, Position a)
buildVector p d = (mkNormal $ p &- (Vec3 0 d 0), p)

pixelCoord :: (Elt a, IsFloating a, Elt b, IsIntegral b)
              => (Scale (Exp a), Scale (Exp a))
              -> (Res (Exp b), Res (Exp b)) 
              -> (Pixel (Exp b), Pixel (Exp b)) 
              -> Position (Exp a)
pixelCoord (sx,sz) (rx,rz) (px,pz) = Vec3 x y z
  where x = sx * pixelCoord' rx px
        y = 0        
        z = sz * pixelCoord' rz pz

pixelCoord' :: (Elt a, IsFloating a, Elt b, IsIntegral b)
               => Exp (Res b) 
               -> Exp (Pixel b) 
               -> Exp a
pixelCoord' num p = (2*p) // num -1
  where (//) = (/) `on` A.fromIntegral
        
data BoundingSphere a = BoundingSphere (Sphere (Exp a)) (Direction (Exp a) -> Position (Exp a) -> (Exp Bool, Position (Exp a)))

explore :: (Elt a, IsFloating a) => BoundingSphere a -> Direction (Exp a) -> Position (Exp a) -> (Exp Bool, Vec3 (Exp a))
explore (BoundingSphere (ps,r,o) e) d p = second (&+ ps) $ e (actU' o d) (actU o (p &- ps)) 

checkIntersection :: (Elt a, IsFloating a) => BoundingSphere a -> Direction (Exp a) -> Vec3 (Exp a) -> (Exp Bool, Position (Exp a))
checkIntersection (BoundingSphere s _) d p = second p' $ sphereIntersection d p s
    where p' m = p &+ (fromNormal d &* m)

type Orientation a = UnitQuaternion a

type Sphere a = (Position a, Radius a, Orientation a)

sphereIntersection :: (Elt a, IsFloating a) => Direction (Exp a) -> Position (Exp a) -> Sphere (Exp a) -> (Exp Bool,Exp a)
sphereIntersection d p0 (pc,r,_) = (lift $ q' >* 0 &&* max t0 t1 >* 0, lift $ max 0 $ min t0 t1) 
  where t0 = c / q
        t1 = q / a
        q = (b <* 0) ? ((negate b + q'') / 2, (negate b - q'') / 2)
        q'' = sqrt q'
        q' = b^2 - 4 * a * c
        a = join (&.) d'
        b = (2 *& d') &. dist
        c = join (&.) dist - r^2
        d' = fromNormal d
        dist = p0 &- pc

sphereIntersectionRegular :: Direction Float -> Position Float -> Sphere Float -> (Bool,(Float,Float))
sphereIntersectionRegular d p0 (pc,r,_) = (q' > 0,(t0,t1))
  where t0 = c / q
        t1 = q / a
        q = if (b < 0) then (negate b + q'') / 2 else (negate b - q'') / 2
        q'' = sqrt q'
        q' = b^2 - 4 * a * c
        a = join (&.) d'
        b = (2 *& d') &. dist
        c = join (&.) dist - r^2
        d' = fromNormal d
        dist = p0 &- pc
