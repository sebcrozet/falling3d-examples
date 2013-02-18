module WorldDraw
(
drawWorld
)
where
import Graphics.UI.GLUT
import qualified Data.Vect.Double.OpenGL as VO

import Physics.Falling.RigidBody.OrderedRigidBody
import Physics.Falling.RigidBody.Positionable()
import Physics.Falling.RigidBody.RigidBody
import Physics.Falling.Math.Transform()
import Physics.Falling.Shape.Ball
import Physics.Falling.World.GenericWorld
import Physics.Falling.RigidBody.Positionable
import Physics.Falling.RigidBody.CollisionVolume
import qualified Physics.Falling.Shape.Plane as P
import Physics.Falling3d.Shape3d
import Physics.Falling3d.World3d
import Physics.Falling3d.RigidBody3d

drawWorld :: DefaultWorld3d Int -> IO ()
drawWorld world = mapM_ drawBody (rigidBodies world)

drawBody :: OrderedRigidBody3d Int -> IO ()
drawBody body
 = case rigidBody body of
   DynamicBody db -> let l2w = localToWorld db in
                     preservingMatrix (VO.multMatrix l2w >> (drawDynamicShape $ collisionVolume db))
   StaticBody sb -> let l2w = localToWorld sb in
                     preservingMatrix (VO.multMatrix l2w >> (drawStaticShape $ collisionVolume sb))

drawDynamicShape :: DynamicShape3d -> IO ()
drawDynamicShape (Ball3d (Ball _ radius)) = renderObject Solid (Sphere' (VO.glflt radius) 20 16)
drawDynamicShape (Box3d  _)               = renderObject Solid (Cube 1.0) -- FIXME: convert the actual length into scale

drawStaticShape :: StaticShape3d -> IO ()
drawStaticShape (StaticBall3d (Ball _ radius)) = renderObject Solid (Sphere' (VO.glflt radius) 20 16)
drawStaticShape (StaticBox3d  _)               = renderObject Solid (Cube 1.0) -- FIXME: convert the actual length into scale
drawStaticShape (Plane3d      (P.Plane _ _))   = return ()
