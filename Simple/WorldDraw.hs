module WorldDraw
(
drawWorld
)
where
import Graphics.UI.GLUT
import qualified Data.Vect.Double.OpenGL as VO
import Data.Vect.Double.Base

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
   DynamicBody db -> let l2w = getLocalToWorld db in
                     preservingMatrix (VO.multMatrix l2w >> (drawDynamicShape $ getCollisionVolume db))
   StaticBody sb -> let l2w = getLocalToWorld sb in
                     preservingMatrix (VO.multMatrix l2w >> (drawStaticShape $ getCollisionVolume sb))

drawDynamicShape :: DynamicShape3d -> IO ()
drawDynamicShape (Ball3d (Ball radius)) = renderObject Solid (Sphere' (VO.glflt radius) 20 16)

drawStaticShape  :: StaticShape3d -> IO ()
drawStaticShape (StaticBall3d (Ball radius)) = renderObject Solid (Sphere' (VO.glflt radius) 20 16)
drawStaticShape (Plane3d      (P.Plane (Vec3 _ _ _))) = return ()
