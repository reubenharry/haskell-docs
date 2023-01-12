{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F33
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Raw functions from the
-- <http://www.opengl.org/registry/ OpenGL registry>.
--
--------------------------------------------------------------------------------

module Graphics.GL.Functions.F33 (
  glWaitSync,
  glWaitSyncAPPLE,
  glWaitVkSemaphoreNV,
  glWeightPathsNV,
  glWeightPointerARB,
  glWeightPointerOES,
  glWeightbvARB,
  glWeightdvARB,
  glWeightfvARB,
  glWeightivARB,
  glWeightsvARB,
  glWeightubvARB,
  glWeightuivARB,
  glWeightusvARB,
  glWindowPos2d,
  glWindowPos2dARB,
  glWindowPos2dMESA,
  glWindowPos2dv,
  glWindowPos2dvARB,
  glWindowPos2dvMESA,
  glWindowPos2f,
  glWindowPos2fARB,
  glWindowPos2fMESA,
  glWindowPos2fv,
  glWindowPos2fvARB,
  glWindowPos2fvMESA,
  glWindowPos2i,
  glWindowPos2iARB,
  glWindowPos2iMESA,
  glWindowPos2iv,
  glWindowPos2ivARB,
  glWindowPos2ivMESA,
  glWindowPos2s,
  glWindowPos2sARB,
  glWindowPos2sMESA,
  glWindowPos2sv,
  glWindowPos2svARB,
  glWindowPos2svMESA,
  glWindowPos3d,
  glWindowPos3dARB,
  glWindowPos3dMESA,
  glWindowPos3dv,
  glWindowPos3dvARB,
  glWindowPos3dvMESA,
  glWindowPos3f,
  glWindowPos3fARB,
  glWindowPos3fMESA,
  glWindowPos3fv,
  glWindowPos3fvARB,
  glWindowPos3fvMESA,
  glWindowPos3i,
  glWindowPos3iARB,
  glWindowPos3iMESA,
  glWindowPos3iv,
  glWindowPos3ivARB,
  glWindowPos3ivMESA,
  glWindowPos3s,
  glWindowPos3sARB,
  glWindowPos3sMESA,
  glWindowPos3sv,
  glWindowPos3svARB,
  glWindowPos3svMESA,
  glWindowPos4dMESA,
  glWindowPos4dvMESA,
  glWindowPos4fMESA,
  glWindowPos4fvMESA,
  glWindowPos4iMESA,
  glWindowPos4ivMESA,
  glWindowPos4sMESA,
  glWindowPos4svMESA,
  glWindowRectanglesEXT,
  glWriteMaskEXT
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glWaitSync ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glWaitSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glWaitSync.xhtml OpenGL 4.x>.
glWaitSync
  :: MonadIO m
  => GLsync -- ^ @sync@ of type @sync@.
  -> GLbitfield -- ^ @flags@.
  -> GLuint64 -- ^ @timeout@.
  -> m ()
glWaitSync v1 v2 v3 = liftIO $ dyn934 ptr_glWaitSync v1 v2 v3

{-# NOINLINE ptr_glWaitSync #-}
ptr_glWaitSync :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO ())
ptr_glWaitSync = unsafePerformIO $ getCommand "glWaitSync"

-- glWaitSyncAPPLE -------------------------------------------------------------

-- | This command is an alias for 'glWaitSync'.
glWaitSyncAPPLE
  :: MonadIO m
  => GLsync -- ^ @sync@.
  -> GLbitfield -- ^ @flags@.
  -> GLuint64 -- ^ @timeout@.
  -> m ()
glWaitSyncAPPLE v1 v2 v3 = liftIO $ dyn934 ptr_glWaitSyncAPPLE v1 v2 v3

{-# NOINLINE ptr_glWaitSyncAPPLE #-}
ptr_glWaitSyncAPPLE :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO ())
ptr_glWaitSyncAPPLE = unsafePerformIO $ getCommand "glWaitSyncAPPLE"

-- glWaitVkSemaphoreNV ---------------------------------------------------------

glWaitVkSemaphoreNV
  :: MonadIO m
  => GLuint64 -- ^ @vkSemaphore@.
  -> m ()
glWaitVkSemaphoreNV v1 = liftIO $ dyn517 ptr_glWaitVkSemaphoreNV v1

{-# NOINLINE ptr_glWaitVkSemaphoreNV #-}
ptr_glWaitVkSemaphoreNV :: FunPtr (GLuint64 -> IO ())
ptr_glWaitVkSemaphoreNV = unsafePerformIO $ getCommand "glWaitVkSemaphoreNV"

-- glWeightPathsNV -------------------------------------------------------------

glWeightPathsNV
  :: MonadIO m
  => GLuint -- ^ @resultPath@ of type @Path@.
  -> GLsizei -- ^ @numPaths@.
  -> Ptr GLuint -- ^ @paths@ pointing to @numPaths@ elements of type @Path@.
  -> Ptr GLfloat -- ^ @weights@ pointing to @numPaths@ elements of type @GLfloat@.
  -> m ()
glWeightPathsNV v1 v2 v3 v4 = liftIO $ dyn935 ptr_glWeightPathsNV v1 v2 v3 v4

{-# NOINLINE ptr_glWeightPathsNV #-}
ptr_glWeightPathsNV :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> Ptr GLfloat -> IO ())
ptr_glWeightPathsNV = unsafePerformIO $ getCommand "glWeightPathsNV"

-- glWeightPointerARB ----------------------------------------------------------

glWeightPointerARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [WeightPointerTypeARB](Graphics-GL-Groups.html#WeightPointerTypeARB).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glWeightPointerARB v1 v2 v3 v4 = liftIO $ dyn133 ptr_glWeightPointerARB v1 v2 v3 v4

{-# NOINLINE ptr_glWeightPointerARB #-}
ptr_glWeightPointerARB :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glWeightPointerARB = unsafePerformIO $ getCommand "glWeightPointerARB"

-- glWeightPointerOES ----------------------------------------------------------

glWeightPointerOES
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glWeightPointerOES v1 v2 v3 v4 = liftIO $ dyn133 ptr_glWeightPointerOES v1 v2 v3 v4

{-# NOINLINE ptr_glWeightPointerOES #-}
ptr_glWeightPointerOES :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glWeightPointerOES = unsafePerformIO $ getCommand "glWeightPointerOES"

-- glWeightbvARB ---------------------------------------------------------------

glWeightbvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLbyte -- ^ @weights@ pointing to @size@ elements of type @GLbyte@.
  -> m ()
glWeightbvARB v1 v2 = liftIO $ dyn936 ptr_glWeightbvARB v1 v2

{-# NOINLINE ptr_glWeightbvARB #-}
ptr_glWeightbvARB :: FunPtr (GLint -> Ptr GLbyte -> IO ())
ptr_glWeightbvARB = unsafePerformIO $ getCommand "glWeightbvARB"

-- glWeightdvARB ---------------------------------------------------------------

glWeightdvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLdouble -- ^ @weights@ pointing to @size@ elements of type @GLdouble@.
  -> m ()
glWeightdvARB v1 v2 = liftIO $ dyn937 ptr_glWeightdvARB v1 v2

{-# NOINLINE ptr_glWeightdvARB #-}
ptr_glWeightdvARB :: FunPtr (GLint -> Ptr GLdouble -> IO ())
ptr_glWeightdvARB = unsafePerformIO $ getCommand "glWeightdvARB"

-- glWeightfvARB ---------------------------------------------------------------

glWeightfvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLfloat -- ^ @weights@ pointing to @size@ elements of type @GLfloat@.
  -> m ()
glWeightfvARB v1 v2 = liftIO $ dyn938 ptr_glWeightfvARB v1 v2

{-# NOINLINE ptr_glWeightfvARB #-}
ptr_glWeightfvARB :: FunPtr (GLint -> Ptr GLfloat -> IO ())
ptr_glWeightfvARB = unsafePerformIO $ getCommand "glWeightfvARB"

-- glWeightivARB ---------------------------------------------------------------

glWeightivARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLint -- ^ @weights@ pointing to @size@ elements of type @GLint@.
  -> m ()
glWeightivARB v1 v2 = liftIO $ dyn939 ptr_glWeightivARB v1 v2

{-# NOINLINE ptr_glWeightivARB #-}
ptr_glWeightivARB :: FunPtr (GLint -> Ptr GLint -> IO ())
ptr_glWeightivARB = unsafePerformIO $ getCommand "glWeightivARB"

-- glWeightsvARB ---------------------------------------------------------------

glWeightsvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLshort -- ^ @weights@ pointing to @size@ elements of type @GLshort@.
  -> m ()
glWeightsvARB v1 v2 = liftIO $ dyn940 ptr_glWeightsvARB v1 v2

{-# NOINLINE ptr_glWeightsvARB #-}
ptr_glWeightsvARB :: FunPtr (GLint -> Ptr GLshort -> IO ())
ptr_glWeightsvARB = unsafePerformIO $ getCommand "glWeightsvARB"

-- glWeightubvARB --------------------------------------------------------------

glWeightubvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLubyte -- ^ @weights@ pointing to @size@ elements of type @GLubyte@.
  -> m ()
glWeightubvARB v1 v2 = liftIO $ dyn543 ptr_glWeightubvARB v1 v2

{-# NOINLINE ptr_glWeightubvARB #-}
ptr_glWeightubvARB :: FunPtr (GLint -> Ptr GLubyte -> IO ())
ptr_glWeightubvARB = unsafePerformIO $ getCommand "glWeightubvARB"

-- glWeightuivARB --------------------------------------------------------------

glWeightuivARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLuint -- ^ @weights@ pointing to @size@ elements of type @GLuint@.
  -> m ()
glWeightuivARB v1 v2 = liftIO $ dyn544 ptr_glWeightuivARB v1 v2

{-# NOINLINE ptr_glWeightuivARB #-}
ptr_glWeightuivARB :: FunPtr (GLint -> Ptr GLuint -> IO ())
ptr_glWeightuivARB = unsafePerformIO $ getCommand "glWeightuivARB"

-- glWeightusvARB --------------------------------------------------------------

glWeightusvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLushort -- ^ @weights@ pointing to @size@ elements of type @GLushort@.
  -> m ()
glWeightusvARB v1 v2 = liftIO $ dyn545 ptr_glWeightusvARB v1 v2

{-# NOINLINE ptr_glWeightusvARB #-}
ptr_glWeightusvARB :: FunPtr (GLint -> Ptr GLushort -> IO ())
ptr_glWeightusvARB = unsafePerformIO $ getCommand "glWeightusvARB"

-- glWindowPos2d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos2dv'.
glWindowPos2d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> m ()
glWindowPos2d v1 v2 = liftIO $ dyn225 ptr_glWindowPos2d v1 v2

{-# NOINLINE ptr_glWindowPos2d #-}
ptr_glWindowPos2d :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glWindowPos2d = unsafePerformIO $ getCommand "glWindowPos2d"

-- glWindowPos2dARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2dvARB'. This command is an alias for 'glWindowPos2d'.
glWindowPos2dARB
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> m ()
glWindowPos2dARB v1 v2 = liftIO $ dyn225 ptr_glWindowPos2dARB v1 v2

{-# NOINLINE ptr_glWindowPos2dARB #-}
ptr_glWindowPos2dARB :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glWindowPos2dARB = unsafePerformIO $ getCommand "glWindowPos2dARB"

-- glWindowPos2dMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2dvMESA'. This command is an alias for 'glWindowPos2d'.
glWindowPos2dMESA
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> m ()
glWindowPos2dMESA v1 v2 = liftIO $ dyn225 ptr_glWindowPos2dMESA v1 v2

{-# NOINLINE ptr_glWindowPos2dMESA #-}
ptr_glWindowPos2dMESA :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glWindowPos2dMESA = unsafePerformIO $ getCommand "glWindowPos2dMESA"

-- glWindowPos2dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos2dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glWindowPos2dv v1 = liftIO $ dyn42 ptr_glWindowPos2dv v1

{-# NOINLINE ptr_glWindowPos2dv #-}
ptr_glWindowPos2dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos2dv = unsafePerformIO $ getCommand "glWindowPos2dv"

-- glWindowPos2dvARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2dv'.
glWindowPos2dvARB
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glWindowPos2dvARB v1 = liftIO $ dyn42 ptr_glWindowPos2dvARB v1

{-# NOINLINE ptr_glWindowPos2dvARB #-}
ptr_glWindowPos2dvARB :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos2dvARB = unsafePerformIO $ getCommand "glWindowPos2dvARB"

-- glWindowPos2dvMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2dv'.
glWindowPos2dvMESA
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glWindowPos2dvMESA v1 = liftIO $ dyn42 ptr_glWindowPos2dvMESA v1

{-# NOINLINE ptr_glWindowPos2dvMESA #-}
ptr_glWindowPos2dvMESA :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos2dvMESA = unsafePerformIO $ getCommand "glWindowPos2dvMESA"

-- glWindowPos2f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos2fv'.
glWindowPos2f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> m ()
glWindowPos2f v1 v2 = liftIO $ dyn230 ptr_glWindowPos2f v1 v2

{-# NOINLINE ptr_glWindowPos2f #-}
ptr_glWindowPos2f :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glWindowPos2f = unsafePerformIO $ getCommand "glWindowPos2f"

-- glWindowPos2fARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2fvARB'. This command is an alias for 'glWindowPos2f'.
glWindowPos2fARB
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> m ()
glWindowPos2fARB v1 v2 = liftIO $ dyn230 ptr_glWindowPos2fARB v1 v2

{-# NOINLINE ptr_glWindowPos2fARB #-}
ptr_glWindowPos2fARB :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glWindowPos2fARB = unsafePerformIO $ getCommand "glWindowPos2fARB"

-- glWindowPos2fMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2fvMESA'. This command is an alias for 'glWindowPos2f'.
glWindowPos2fMESA
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> m ()
glWindowPos2fMESA v1 v2 = liftIO $ dyn230 ptr_glWindowPos2fMESA v1 v2

{-# NOINLINE ptr_glWindowPos2fMESA #-}
ptr_glWindowPos2fMESA :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glWindowPos2fMESA = unsafePerformIO $ getCommand "glWindowPos2fMESA"

-- glWindowPos2fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos2fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glWindowPos2fv v1 = liftIO $ dyn44 ptr_glWindowPos2fv v1

{-# NOINLINE ptr_glWindowPos2fv #-}
ptr_glWindowPos2fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos2fv = unsafePerformIO $ getCommand "glWindowPos2fv"

-- glWindowPos2fvARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2fv'.
glWindowPos2fvARB
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glWindowPos2fvARB v1 = liftIO $ dyn44 ptr_glWindowPos2fvARB v1

{-# NOINLINE ptr_glWindowPos2fvARB #-}
ptr_glWindowPos2fvARB :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos2fvARB = unsafePerformIO $ getCommand "glWindowPos2fvARB"

-- glWindowPos2fvMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2fv'.
glWindowPos2fvMESA
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glWindowPos2fvMESA v1 = liftIO $ dyn44 ptr_glWindowPos2fvMESA v1

{-# NOINLINE ptr_glWindowPos2fvMESA #-}
ptr_glWindowPos2fvMESA :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos2fvMESA = unsafePerformIO $ getCommand "glWindowPos2fvMESA"

-- glWindowPos2i ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos2iv'.
glWindowPos2i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> m ()
glWindowPos2i v1 v2 = liftIO $ dyn277 ptr_glWindowPos2i v1 v2

{-# NOINLINE ptr_glWindowPos2i #-}
ptr_glWindowPos2i :: FunPtr (GLint -> GLint -> IO ())
ptr_glWindowPos2i = unsafePerformIO $ getCommand "glWindowPos2i"

-- glWindowPos2iARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2ivARB'. This command is an alias for 'glWindowPos2i'.
glWindowPos2iARB
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> m ()
glWindowPos2iARB v1 v2 = liftIO $ dyn277 ptr_glWindowPos2iARB v1 v2

{-# NOINLINE ptr_glWindowPos2iARB #-}
ptr_glWindowPos2iARB :: FunPtr (GLint -> GLint -> IO ())
ptr_glWindowPos2iARB = unsafePerformIO $ getCommand "glWindowPos2iARB"

-- glWindowPos2iMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2ivMESA'. This command is an alias for 'glWindowPos2i'.
glWindowPos2iMESA
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> m ()
glWindowPos2iMESA v1 v2 = liftIO $ dyn277 ptr_glWindowPos2iMESA v1 v2

{-# NOINLINE ptr_glWindowPos2iMESA #-}
ptr_glWindowPos2iMESA :: FunPtr (GLint -> GLint -> IO ())
ptr_glWindowPos2iMESA = unsafePerformIO $ getCommand "glWindowPos2iMESA"

-- glWindowPos2iv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos2iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glWindowPos2iv v1 = liftIO $ dyn46 ptr_glWindowPos2iv v1

{-# NOINLINE ptr_glWindowPos2iv #-}
ptr_glWindowPos2iv :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos2iv = unsafePerformIO $ getCommand "glWindowPos2iv"

-- glWindowPos2ivARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2iv'.
glWindowPos2ivARB
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glWindowPos2ivARB v1 = liftIO $ dyn46 ptr_glWindowPos2ivARB v1

{-# NOINLINE ptr_glWindowPos2ivARB #-}
ptr_glWindowPos2ivARB :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos2ivARB = unsafePerformIO $ getCommand "glWindowPos2ivARB"

-- glWindowPos2ivMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2iv'.
glWindowPos2ivMESA
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glWindowPos2ivMESA v1 = liftIO $ dyn46 ptr_glWindowPos2ivMESA v1

{-# NOINLINE ptr_glWindowPos2ivMESA #-}
ptr_glWindowPos2ivMESA :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos2ivMESA = unsafePerformIO $ getCommand "glWindowPos2ivMESA"

-- glWindowPos2s ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos2sv'.
glWindowPos2s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> m ()
glWindowPos2s v1 v2 = liftIO $ dyn709 ptr_glWindowPos2s v1 v2

{-# NOINLINE ptr_glWindowPos2s #-}
ptr_glWindowPos2s :: FunPtr (GLshort -> GLshort -> IO ())
ptr_glWindowPos2s = unsafePerformIO $ getCommand "glWindowPos2s"

-- glWindowPos2sARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2svARB'. This command is an alias for 'glWindowPos2s'.
glWindowPos2sARB
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> m ()
glWindowPos2sARB v1 v2 = liftIO $ dyn709 ptr_glWindowPos2sARB v1 v2

{-# NOINLINE ptr_glWindowPos2sARB #-}
ptr_glWindowPos2sARB :: FunPtr (GLshort -> GLshort -> IO ())
ptr_glWindowPos2sARB = unsafePerformIO $ getCommand "glWindowPos2sARB"

-- glWindowPos2sMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2svMESA'. This command is an alias for 'glWindowPos2s'.
glWindowPos2sMESA
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> m ()
glWindowPos2sMESA v1 v2 = liftIO $ dyn709 ptr_glWindowPos2sMESA v1 v2

{-# NOINLINE ptr_glWindowPos2sMESA #-}
ptr_glWindowPos2sMESA :: FunPtr (GLshort -> GLshort -> IO ())
ptr_glWindowPos2sMESA = unsafePerformIO $ getCommand "glWindowPos2sMESA"

-- glWindowPos2sv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos2sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glWindowPos2sv v1 = liftIO $ dyn48 ptr_glWindowPos2sv v1

{-# NOINLINE ptr_glWindowPos2sv #-}
ptr_glWindowPos2sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos2sv = unsafePerformIO $ getCommand "glWindowPos2sv"

-- glWindowPos2svARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2sv'.
glWindowPos2svARB
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glWindowPos2svARB v1 = liftIO $ dyn48 ptr_glWindowPos2svARB v1

{-# NOINLINE ptr_glWindowPos2svARB #-}
ptr_glWindowPos2svARB :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos2svARB = unsafePerformIO $ getCommand "glWindowPos2svARB"

-- glWindowPos2svMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2sv'.
glWindowPos2svMESA
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glWindowPos2svMESA v1 = liftIO $ dyn48 ptr_glWindowPos2svMESA v1

{-# NOINLINE ptr_glWindowPos2svMESA #-}
ptr_glWindowPos2svMESA :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos2svMESA = unsafePerformIO $ getCommand "glWindowPos2svMESA"

-- glWindowPos3d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos3dv'.
glWindowPos3d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> m ()
glWindowPos3d v1 v2 v3 = liftIO $ dyn41 ptr_glWindowPos3d v1 v2 v3

{-# NOINLINE ptr_glWindowPos3d #-}
ptr_glWindowPos3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glWindowPos3d = unsafePerformIO $ getCommand "glWindowPos3d"

-- glWindowPos3dARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3dvARB'. This command is an alias for 'glWindowPos3d'.
glWindowPos3dARB
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> m ()
glWindowPos3dARB v1 v2 v3 = liftIO $ dyn41 ptr_glWindowPos3dARB v1 v2 v3

{-# NOINLINE ptr_glWindowPos3dARB #-}
ptr_glWindowPos3dARB :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glWindowPos3dARB = unsafePerformIO $ getCommand "glWindowPos3dARB"

-- glWindowPos3dMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3dvMESA'. This command is an alias for 'glWindowPos3d'.
glWindowPos3dMESA
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> m ()
glWindowPos3dMESA v1 v2 v3 = liftIO $ dyn41 ptr_glWindowPos3dMESA v1 v2 v3

{-# NOINLINE ptr_glWindowPos3dMESA #-}
ptr_glWindowPos3dMESA :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glWindowPos3dMESA = unsafePerformIO $ getCommand "glWindowPos3dMESA"

-- glWindowPos3dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glWindowPos3dv v1 = liftIO $ dyn42 ptr_glWindowPos3dv v1

{-# NOINLINE ptr_glWindowPos3dv #-}
ptr_glWindowPos3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos3dv = unsafePerformIO $ getCommand "glWindowPos3dv"

-- glWindowPos3dvARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3dv'.
glWindowPos3dvARB
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glWindowPos3dvARB v1 = liftIO $ dyn42 ptr_glWindowPos3dvARB v1

{-# NOINLINE ptr_glWindowPos3dvARB #-}
ptr_glWindowPos3dvARB :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos3dvARB = unsafePerformIO $ getCommand "glWindowPos3dvARB"

-- glWindowPos3dvMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3dv'.
glWindowPos3dvMESA
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glWindowPos3dvMESA v1 = liftIO $ dyn42 ptr_glWindowPos3dvMESA v1

{-# NOINLINE ptr_glWindowPos3dvMESA #-}
ptr_glWindowPos3dvMESA :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos3dvMESA = unsafePerformIO $ getCommand "glWindowPos3dvMESA"

-- glWindowPos3f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos3fv'.
glWindowPos3f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> m ()
glWindowPos3f v1 v2 v3 = liftIO $ dyn43 ptr_glWindowPos3f v1 v2 v3

{-# NOINLINE ptr_glWindowPos3f #-}
ptr_glWindowPos3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glWindowPos3f = unsafePerformIO $ getCommand "glWindowPos3f"

-- glWindowPos3fARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3fvARB'. This command is an alias for 'glWindowPos3f'.
glWindowPos3fARB
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> m ()
glWindowPos3fARB v1 v2 v3 = liftIO $ dyn43 ptr_glWindowPos3fARB v1 v2 v3

{-# NOINLINE ptr_glWindowPos3fARB #-}
ptr_glWindowPos3fARB :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glWindowPos3fARB = unsafePerformIO $ getCommand "glWindowPos3fARB"

-- glWindowPos3fMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3fvMESA'. This command is an alias for 'glWindowPos3f'.
glWindowPos3fMESA
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> m ()
glWindowPos3fMESA v1 v2 v3 = liftIO $ dyn43 ptr_glWindowPos3fMESA v1 v2 v3

{-# NOINLINE ptr_glWindowPos3fMESA #-}
ptr_glWindowPos3fMESA :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glWindowPos3fMESA = unsafePerformIO $ getCommand "glWindowPos3fMESA"

-- glWindowPos3fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glWindowPos3fv v1 = liftIO $ dyn44 ptr_glWindowPos3fv v1

{-# NOINLINE ptr_glWindowPos3fv #-}
ptr_glWindowPos3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos3fv = unsafePerformIO $ getCommand "glWindowPos3fv"

-- glWindowPos3fvARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3fv'.
glWindowPos3fvARB
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glWindowPos3fvARB v1 = liftIO $ dyn44 ptr_glWindowPos3fvARB v1

{-# NOINLINE ptr_glWindowPos3fvARB #-}
ptr_glWindowPos3fvARB :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos3fvARB = unsafePerformIO $ getCommand "glWindowPos3fvARB"

-- glWindowPos3fvMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3fv'.
glWindowPos3fvMESA
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glWindowPos3fvMESA v1 = liftIO $ dyn44 ptr_glWindowPos3fvMESA v1

{-# NOINLINE ptr_glWindowPos3fvMESA #-}
ptr_glWindowPos3fvMESA :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos3fvMESA = unsafePerformIO $ getCommand "glWindowPos3fvMESA"

-- glWindowPos3i ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos3iv'.
glWindowPos3i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> m ()
glWindowPos3i v1 v2 v3 = liftIO $ dyn45 ptr_glWindowPos3i v1 v2 v3

{-# NOINLINE ptr_glWindowPos3i #-}
ptr_glWindowPos3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glWindowPos3i = unsafePerformIO $ getCommand "glWindowPos3i"

-- glWindowPos3iARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3ivARB'. This command is an alias for 'glWindowPos3i'.
glWindowPos3iARB
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> m ()
glWindowPos3iARB v1 v2 v3 = liftIO $ dyn45 ptr_glWindowPos3iARB v1 v2 v3

{-# NOINLINE ptr_glWindowPos3iARB #-}
ptr_glWindowPos3iARB :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glWindowPos3iARB = unsafePerformIO $ getCommand "glWindowPos3iARB"

-- glWindowPos3iMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3ivMESA'. This command is an alias for 'glWindowPos3i'.
glWindowPos3iMESA
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> m ()
glWindowPos3iMESA v1 v2 v3 = liftIO $ dyn45 ptr_glWindowPos3iMESA v1 v2 v3

{-# NOINLINE ptr_glWindowPos3iMESA #-}
ptr_glWindowPos3iMESA :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glWindowPos3iMESA = unsafePerformIO $ getCommand "glWindowPos3iMESA"

-- glWindowPos3iv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glWindowPos3iv v1 = liftIO $ dyn46 ptr_glWindowPos3iv v1

{-# NOINLINE ptr_glWindowPos3iv #-}
ptr_glWindowPos3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos3iv = unsafePerformIO $ getCommand "glWindowPos3iv"

-- glWindowPos3ivARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3iv'.
glWindowPos3ivARB
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glWindowPos3ivARB v1 = liftIO $ dyn46 ptr_glWindowPos3ivARB v1

{-# NOINLINE ptr_glWindowPos3ivARB #-}
ptr_glWindowPos3ivARB :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos3ivARB = unsafePerformIO $ getCommand "glWindowPos3ivARB"

-- glWindowPos3ivMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3iv'.
glWindowPos3ivMESA
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glWindowPos3ivMESA v1 = liftIO $ dyn46 ptr_glWindowPos3ivMESA v1

{-# NOINLINE ptr_glWindowPos3ivMESA #-}
ptr_glWindowPos3ivMESA :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos3ivMESA = unsafePerformIO $ getCommand "glWindowPos3ivMESA"

-- glWindowPos3s ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos3sv'.
glWindowPos3s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> m ()
glWindowPos3s v1 v2 v3 = liftIO $ dyn47 ptr_glWindowPos3s v1 v2 v3

{-# NOINLINE ptr_glWindowPos3s #-}
ptr_glWindowPos3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glWindowPos3s = unsafePerformIO $ getCommand "glWindowPos3s"

-- glWindowPos3sARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3svARB'. This command is an alias for 'glWindowPos3s'.
glWindowPos3sARB
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> m ()
glWindowPos3sARB v1 v2 v3 = liftIO $ dyn47 ptr_glWindowPos3sARB v1 v2 v3

{-# NOINLINE ptr_glWindowPos3sARB #-}
ptr_glWindowPos3sARB :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glWindowPos3sARB = unsafePerformIO $ getCommand "glWindowPos3sARB"

-- glWindowPos3sMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3svMESA'. This command is an alias for 'glWindowPos3s'.
glWindowPos3sMESA
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> m ()
glWindowPos3sMESA v1 v2 v3 = liftIO $ dyn47 ptr_glWindowPos3sMESA v1 v2 v3

{-# NOINLINE ptr_glWindowPos3sMESA #-}
ptr_glWindowPos3sMESA :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glWindowPos3sMESA = unsafePerformIO $ getCommand "glWindowPos3sMESA"

-- glWindowPos3sv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glWindowPos3sv v1 = liftIO $ dyn48 ptr_glWindowPos3sv v1

{-# NOINLINE ptr_glWindowPos3sv #-}
ptr_glWindowPos3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos3sv = unsafePerformIO $ getCommand "glWindowPos3sv"

-- glWindowPos3svARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3sv'.
glWindowPos3svARB
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glWindowPos3svARB v1 = liftIO $ dyn48 ptr_glWindowPos3svARB v1

{-# NOINLINE ptr_glWindowPos3svARB #-}
ptr_glWindowPos3svARB :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos3svARB = unsafePerformIO $ getCommand "glWindowPos3svARB"

-- glWindowPos3svMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3sv'.
glWindowPos3svMESA
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glWindowPos3svMESA v1 = liftIO $ dyn48 ptr_glWindowPos3svMESA v1

{-# NOINLINE ptr_glWindowPos3svMESA #-}
ptr_glWindowPos3svMESA :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos3svMESA = unsafePerformIO $ getCommand "glWindowPos3svMESA"

-- glWindowPos4dMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos4dvMESA'.
glWindowPos4dMESA
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> GLdouble -- ^ @w@ of type @CoordD@.
  -> m ()
glWindowPos4dMESA v1 v2 v3 v4 = liftIO $ dyn116 ptr_glWindowPos4dMESA v1 v2 v3 v4

{-# NOINLINE ptr_glWindowPos4dMESA #-}
ptr_glWindowPos4dMESA :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glWindowPos4dMESA = unsafePerformIO $ getCommand "glWindowPos4dMESA"

-- glWindowPos4dvMESA ----------------------------------------------------------

glWindowPos4dvMESA
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @CoordD@.
  -> m ()
glWindowPos4dvMESA v1 = liftIO $ dyn42 ptr_glWindowPos4dvMESA v1

{-# NOINLINE ptr_glWindowPos4dvMESA #-}
ptr_glWindowPos4dvMESA :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos4dvMESA = unsafePerformIO $ getCommand "glWindowPos4dvMESA"

-- glWindowPos4fMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos4fvMESA'.
glWindowPos4fMESA
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> GLfloat -- ^ @w@ of type @CoordF@.
  -> m ()
glWindowPos4fMESA v1 v2 v3 v4 = liftIO $ dyn52 ptr_glWindowPos4fMESA v1 v2 v3 v4

{-# NOINLINE ptr_glWindowPos4fMESA #-}
ptr_glWindowPos4fMESA :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glWindowPos4fMESA = unsafePerformIO $ getCommand "glWindowPos4fMESA"

-- glWindowPos4fvMESA ----------------------------------------------------------

glWindowPos4fvMESA
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glWindowPos4fvMESA v1 = liftIO $ dyn44 ptr_glWindowPos4fvMESA v1

{-# NOINLINE ptr_glWindowPos4fvMESA #-}
ptr_glWindowPos4fvMESA :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos4fvMESA = unsafePerformIO $ getCommand "glWindowPos4fvMESA"

-- glWindowPos4iMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos4ivMESA'.
glWindowPos4iMESA
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> GLint -- ^ @w@ of type @CoordI@.
  -> m ()
glWindowPos4iMESA v1 v2 v3 v4 = liftIO $ dyn82 ptr_glWindowPos4iMESA v1 v2 v3 v4

{-# NOINLINE ptr_glWindowPos4iMESA #-}
ptr_glWindowPos4iMESA :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glWindowPos4iMESA = unsafePerformIO $ getCommand "glWindowPos4iMESA"

-- glWindowPos4ivMESA ----------------------------------------------------------

glWindowPos4ivMESA
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glWindowPos4ivMESA v1 = liftIO $ dyn46 ptr_glWindowPos4ivMESA v1

{-# NOINLINE ptr_glWindowPos4ivMESA #-}
ptr_glWindowPos4ivMESA :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos4ivMESA = unsafePerformIO $ getCommand "glWindowPos4ivMESA"

-- glWindowPos4sMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos4svMESA'.
glWindowPos4sMESA
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> GLshort -- ^ @w@ of type @CoordS@.
  -> m ()
glWindowPos4sMESA v1 v2 v3 v4 = liftIO $ dyn120 ptr_glWindowPos4sMESA v1 v2 v3 v4

{-# NOINLINE ptr_glWindowPos4sMESA #-}
ptr_glWindowPos4sMESA :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glWindowPos4sMESA = unsafePerformIO $ getCommand "glWindowPos4sMESA"

-- glWindowPos4svMESA ----------------------------------------------------------

glWindowPos4svMESA
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glWindowPos4svMESA v1 = liftIO $ dyn48 ptr_glWindowPos4svMESA v1

{-# NOINLINE ptr_glWindowPos4svMESA #-}
ptr_glWindowPos4svMESA :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos4svMESA = unsafePerformIO $ getCommand "glWindowPos4svMESA"

-- glWindowRectanglesEXT -------------------------------------------------------

glWindowRectanglesEXT
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @box@ pointing to @COMPSIZE(count)@ elements of type @GLint@.
  -> m ()
glWindowRectanglesEXT v1 v2 v3 = liftIO $ dyn941 ptr_glWindowRectanglesEXT v1 v2 v3

{-# NOINLINE ptr_glWindowRectanglesEXT #-}
ptr_glWindowRectanglesEXT :: FunPtr (GLenum -> GLsizei -> Ptr GLint -> IO ())
ptr_glWindowRectanglesEXT = unsafePerformIO $ getCommand "glWindowRectanglesEXT"

-- glWriteMaskEXT --------------------------------------------------------------

glWriteMaskEXT
  :: MonadIO m
  => GLuint -- ^ @res@.
  -> GLuint -- ^ @in@.
  -> GLenum -- ^ @outX@ of type [VertexShaderWriteMaskEXT](Graphics-GL-Groups.html#VertexShaderWriteMaskEXT).
  -> GLenum -- ^ @outY@ of type [VertexShaderWriteMaskEXT](Graphics-GL-Groups.html#VertexShaderWriteMaskEXT).
  -> GLenum -- ^ @outZ@ of type [VertexShaderWriteMaskEXT](Graphics-GL-Groups.html#VertexShaderWriteMaskEXT).
  -> GLenum -- ^ @outW@ of type [VertexShaderWriteMaskEXT](Graphics-GL-Groups.html#VertexShaderWriteMaskEXT).
  -> m ()
glWriteMaskEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn763 ptr_glWriteMaskEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glWriteMaskEXT #-}
ptr_glWriteMaskEXT :: FunPtr (GLuint -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glWriteMaskEXT = unsafePerformIO $ getCommand "glWriteMaskEXT"

