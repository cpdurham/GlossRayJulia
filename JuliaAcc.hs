{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE StandaloneDeriving #-}

module JuliaAcc where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State hiding (lift)

import Data.Vect.Floating
import Data.Vect.Floating.Util.Quaternion

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Interpreter as I

import Data.Vect.Floating.Accelerate.Instances

import RayAcc

expLift2 :: Elt a => (Quaternion (Exp a), Quaternion (Exp a)) -> Exp (Quaternion a, Quaternion a)
expLift2 = g . join (***) h
  where h = lift :: Elt a => Quaternion (Exp a) -> Exp (Quaternion a)
        g = lift :: Elt a => (Exp (Quaternion a), Exp (Quaternion a)) -> Exp (Quaternion a, Quaternion a)
        
expUnlift2 :: Elt a => Exp (Quaternion a, Quaternion a) -> (Quaternion (Exp a), Quaternion (Exp a))
expUnlift2 = join (***) h . g
  where h = unlift :: Elt a => Exp (Quaternion a) -> Quaternion (Exp a)
        g = unlift :: Elt a => Exp (Quaternion a, Quaternion a) -> (Exp (Quaternion a), Exp (Quaternion a))

juliaFunction :: (Elt a, IsFloating a) => Exp (Quaternion a) -> Exp (Quaternion a, Quaternion a) -> Exp (Quaternion a, Quaternion a)
juliaFunction expC expQ = Prelude.curry (lensqr q >* 10 ?) expQ $ lift (q', dq')
  where (q, dq) = expUnlift2 expQ
        c = unlift expC
        q' = julia c q
        dq' = juliaDerivative q dq
        
{- need to profile -}
juliaFunction2 :: (Elt a, IsFloating a) => Exp (Quaternion a) -> Exp (Quaternion a, Quaternion a) -> Exp (Quaternion a, Quaternion a)
juliaFunction2 expC expQ = lift (b *& q &+ notb *& q', b *& dq &+ notb *& q')  
  where (q,dq) = expUnlift2 expQ
        a = lensqr q >* 10
        b = a ? (1.0,0.0)
        notb = A.not a ? (0.0,1.0)
        c = unlift expC
        q' = julia c q
        dq' = juliaDerivative q dq

julia :: Floating a => Quaternion a -> Quaternion a -> Quaternion a
julia c q = join multQ q &+ c

juliaDerivative :: Floating a => Quaternion a -> Quaternion a -> Quaternion a
juliaDerivative q dq = 2 *& multQ q dq

distanceEstimate :: Floating a => Q a -> Q a -> a
distanceEstimate q dq = 0.5 * r * log r / dr
  where r = len q 
        dr = len dq
        
epsilon :: Floating a => a
epsilon = 1e-3
        
is = 50
--is = 10

exploreJulia :: (Elt a, Floating a, IsFloating a) => Exp (Quaternion a) -> Exp (Direction a) -> Exp (Position a) -> Exp (Bool, Position a)
exploreJulia q expD expP = lift $ first ((<* epsilon) . last) . runState (replicateM is . state $ distanceEstimateJump q expD) $ expP

initial :: Floating a => Position a -> (Quaternion a, Quaternion a)
initial p = (Q $ extendWith 0.5 p, Q $ Vec4 1 0 0 0)

distanceEstimateJump :: forall a. (Elt a, Floating a, IsFloating a) => Exp (Quaternion a) -> Exp (Direction a) -> Exp (Position a) -> (Exp a, Exp (Position a))
distanceEstimateJump c expD expP = (lift dist, lift p')
  where d = unlift expD
        p = unlift expP
        p' = p &+ dist *& fromNormal d
        (q,dq) = expUnlift2 $ Prelude.iterate (juliaFunction c) (expLift2 $ initial p) Prelude.!! 8
        dist = distanceEstimate q dq
