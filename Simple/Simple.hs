import System.Random
import GHC.Float
import Data.Vect.Double.Base hiding(translation)

import Simulate
import Data.Vect.Double.Util.Projective
import Physics.Falling.World.GenericWorld
import Physics.Falling.RigidBody.OrderedRigidBody
import qualified Physics.Falling.RigidBody.Positionable as RB (translate)
import Physics.Falling.RigidBody.RigidBody
import Physics.Falling.RigidBody.DynamicBody
import Physics.Falling.RigidBody.Dynamic
import Physics.Falling.RigidBody.StaticBody
import Physics.Falling.Shape.Ball
import qualified Physics.Falling.Shape.Plane as P
import Physics.Falling3d.World3d
import Physics.Falling3d.Shape3d
import Physics.Falling3d.Box
import Physics.Falling3d.RigidBody3d

world :: DefaultWorld3d Int
world = addRigidBodies (spheres ++ boxes ++ ground) $ mkWorld3d
        where
        spheres = [] -- map (generateDynamicBody $ Ball3d $ Ball 0.5)       [1 .. 10]
        boxes   = map (generateDynamicBody $ Box3d $ Box 0.5 0.5 0.5) [11 .. 15]
        ground  = [
                    orderRigidBody (-1) $ StaticBody $ RB.translate (Vec3 0 (-1) 0)
                                                     $ mkStaticBody idmtx (Plane3d $ P.Plane $ Vec3 0 1 1)
                    , orderRigidBody (-2) $ StaticBody $ RB.translate (Vec3 0 (-1) 0)
                                                       $ mkStaticBody idmtx (Plane3d $ P.Plane $ Vec3 0 1 (-1))
                    , orderRigidBody (-3) $ StaticBody $ RB.translate (Vec3 0 (-1) 0)
                                                       $ mkStaticBody idmtx (Plane3d $ P.Plane $ Vec3 1 1 0)
                    , orderRigidBody (-4) $ StaticBody $ RB.translate (Vec3 0 (-1) 0)
                                                       $ mkStaticBody idmtx (Plane3d $ P.Plane $ Vec3 (-1) 1 0)
                  ]

generateDynamicBody :: DynamicShape3d -> Int -> OrderedRigidBody3d Int
generateDynamicBody shape i = orderRigidBody i $ DynamicBody
                                               $ RB.translate (Vec3 fdx (fdy + 2) fdz)
                                               $ setExternalLinearForce (Vec3 0.0 (-9.81) 0.0)
                                               -- $ setExternalAngularForce (Vec3 0.0 (-9.81) 0.0)
                                               $ mkDynamicBody idmtx shape 1.0 zero zero
                        where
                        g          = mkStdGen (i * 20)
                        (_, g')    = next g
                        (dx, g'')  = next g'
                        (dy, g''') = next g''
                        (dz, _)    = next g'''
                        (m, mx)    = genRange g
                        range      = int2Double mx - int2Double m
                        fdx        = int2Double dx / range * 3.0 - 1.5
                        fdy        = int2Double dy / range * 3.0 - 1.5
                        fdz        = int2Double dz / range * 3.0 - 1.5

main :: IO ()
main = simulateDisplay world
