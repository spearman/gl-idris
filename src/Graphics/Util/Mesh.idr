module Graphics.Util.Mesh

import Control.Algebra
import Data.Vect 

%access public export
%default total
Vec2 : Type
Vec2 = Vect 2 Double

Vec3 : Type
Vec3 = Vect 3 Double

Semigroup Vec3 where
  (<+>) (x1 :: y1 :: z1 :: Nil) (x2 :: y2 :: z2 :: Nil) =
    (x1 + x2) :: (y1 + y2) :: (z1 + z2) :: Nil

Monoid Vec3 where
  neutral = 0 :: 0 :: 0 :: Nil

Group Vec3 where
  inverse (x :: y :: z :: Nil) = (-x) :: (-y) :: (-z) :: Nil

Vec4 : Type
Vec4 = Vect 4 Double

||| a data type for meshes.
|||
||| a mesh is a collection of positions, of UV texture coordinates, vertex normals and indices
||| 
data Mesh : Type where
  UvMesh :  (positions: Vect n Vec3)
         -> (normals: Vect n Vec3) 
         -> (uvs: Vect n Vec2)
         -> (indices: List Int)
         -> Mesh

positions : Mesh -> List Vec3
positions (UvMesh p _ _ _) = toList p

indices : Mesh -> List Int
indices (UvMesh _ _ _ xs) = xs
