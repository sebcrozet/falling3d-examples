import System.Random
import Data.Vect.Double.Base hiding(translation)

import Simulate

-- needed to get 'addRigidBodies'
import Physics.Falling.World.GenericWorld

-- needed to get 'orderRigidBody'
import Physics.Falling.RigidBody.OrderedRigidBody

-- needed to get 'translate'
import qualified Physics.Falling.RigidBody.Positionable as RB (translate)

-- needed to get setExternalLinearForce
import Physics.Falling.RigidBody.Dynamic

-- needed to get 'DynamicBody' and 'StaticBody' constructors.
import Physics.Falling.RigidBody.RigidBody

-- needed to get 'mkDynamicBody
import Physics.Falling.RigidBody.DynamicBody

-- needed to get 'mkStaticBody'
import Physics.Falling.RigidBody.StaticBody

-- needed to get 'planev'
import qualified Physics.Falling.Shape.Plane as P

-- needed to get 'mkWorld3d'
import Physics.Falling3d.World3d

-- needed to get 'Plane3d' and 'Box3d' constructors.
import Physics.Falling3d.Shape3d

-- needed to get 'Box' constructor.
import Physics.Falling3d.Box

-- FIXME: importing all that is quite inconvenient. Some modules should re-exporte others.

world :: DefaultWorld3d Int
world = addRigidBodies [box, plane] -- add our box and our plane…
        $ mkWorld3d                 -- … inside a 3d physics world.
        where
        -- create a moving body
        box   = orderRigidBody 1                                 -- assign a unique identifier…
                $ DynamicBody                                    -- … to a dynamic body (which can move)…
                $ setExternalLinearForce (Vec3 0.0 (-9.81) 0.0)  -- … affected by a 9.81 gravity along the Y axis …
                $ RB.translate (Vec3 0 5 0)                      -- … centered at (0, 5, 0) …
                $ mkDynamicBody idmtx                            -- … with no rotation …
                                (Box3d $ Box 0.5 0.5 0.5)        -- … having the shape of a box with 1-metter long sides…
                                1.0 zero zero                    -- … with a density of 1.0, and no initial velocities

        -- create the floor
        plane = orderRigidBody (-1)                 -- assign a unique identifier…
                $ StaticBody                        -- … to a static body (which cannot move)…
                $ RB.translate (Vec3 0 (-1) 0)      -- … centered at (0, -1, 0)…
                $ mkStaticBody idmtx                -- … with an initial identity transform…
                $ (Plane3d $ P.planev $ Vec3 0 1 0) -- … having the shape of an infinite plane poiting on
                                                    -- the Y axis direction.

main :: IO ()
main = simulateDisplay world
