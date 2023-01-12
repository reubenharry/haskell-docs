{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F30
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

module Graphics.GL.Functions.F30 (
  glVertexArrayVertexBindingDivisorEXT,
  glVertexArrayVertexBuffer,
  glVertexArrayVertexBuffers,
  glVertexArrayVertexOffsetEXT,
  glVertexAttrib1d,
  glVertexAttrib1dARB,
  glVertexAttrib1dNV,
  glVertexAttrib1dv,
  glVertexAttrib1dvARB,
  glVertexAttrib1dvNV,
  glVertexAttrib1f,
  glVertexAttrib1fARB,
  glVertexAttrib1fNV,
  glVertexAttrib1fv,
  glVertexAttrib1fvARB,
  glVertexAttrib1fvNV,
  glVertexAttrib1hNV,
  glVertexAttrib1hvNV,
  glVertexAttrib1s,
  glVertexAttrib1sARB,
  glVertexAttrib1sNV,
  glVertexAttrib1sv,
  glVertexAttrib1svARB,
  glVertexAttrib1svNV,
  glVertexAttrib2d,
  glVertexAttrib2dARB,
  glVertexAttrib2dNV,
  glVertexAttrib2dv,
  glVertexAttrib2dvARB,
  glVertexAttrib2dvNV,
  glVertexAttrib2f,
  glVertexAttrib2fARB,
  glVertexAttrib2fNV,
  glVertexAttrib2fv,
  glVertexAttrib2fvARB,
  glVertexAttrib2fvNV,
  glVertexAttrib2hNV,
  glVertexAttrib2hvNV,
  glVertexAttrib2s,
  glVertexAttrib2sARB,
  glVertexAttrib2sNV,
  glVertexAttrib2sv,
  glVertexAttrib2svARB,
  glVertexAttrib2svNV,
  glVertexAttrib3d,
  glVertexAttrib3dARB,
  glVertexAttrib3dNV,
  glVertexAttrib3dv,
  glVertexAttrib3dvARB,
  glVertexAttrib3dvNV,
  glVertexAttrib3f,
  glVertexAttrib3fARB,
  glVertexAttrib3fNV,
  glVertexAttrib3fv,
  glVertexAttrib3fvARB,
  glVertexAttrib3fvNV,
  glVertexAttrib3hNV,
  glVertexAttrib3hvNV,
  glVertexAttrib3s,
  glVertexAttrib3sARB,
  glVertexAttrib3sNV,
  glVertexAttrib3sv,
  glVertexAttrib3svARB,
  glVertexAttrib3svNV,
  glVertexAttrib4Nbv,
  glVertexAttrib4NbvARB,
  glVertexAttrib4Niv,
  glVertexAttrib4NivARB,
  glVertexAttrib4Nsv,
  glVertexAttrib4NsvARB,
  glVertexAttrib4Nub,
  glVertexAttrib4NubARB,
  glVertexAttrib4Nubv,
  glVertexAttrib4NubvARB,
  glVertexAttrib4Nuiv,
  glVertexAttrib4NuivARB,
  glVertexAttrib4Nusv,
  glVertexAttrib4NusvARB,
  glVertexAttrib4bv,
  glVertexAttrib4bvARB,
  glVertexAttrib4d,
  glVertexAttrib4dARB,
  glVertexAttrib4dNV,
  glVertexAttrib4dv,
  glVertexAttrib4dvARB,
  glVertexAttrib4dvNV,
  glVertexAttrib4f,
  glVertexAttrib4fARB,
  glVertexAttrib4fNV,
  glVertexAttrib4fv,
  glVertexAttrib4fvARB,
  glVertexAttrib4fvNV,
  glVertexAttrib4hNV,
  glVertexAttrib4hvNV,
  glVertexAttrib4iv,
  glVertexAttrib4ivARB,
  glVertexAttrib4s,
  glVertexAttrib4sARB,
  glVertexAttrib4sNV,
  glVertexAttrib4sv
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glVertexArrayVertexBindingDivisorEXT ----------------------------------------

glVertexArrayVertexBindingDivisorEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexArrayVertexBindingDivisorEXT v1 v2 v3 = liftIO $ dyn109 ptr_glVertexArrayVertexBindingDivisorEXT v1 v2 v3

{-# NOINLINE ptr_glVertexArrayVertexBindingDivisorEXT #-}
ptr_glVertexArrayVertexBindingDivisorEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexArrayVertexBindingDivisorEXT = unsafePerformIO $ getCommand "glVertexArrayVertexBindingDivisorEXT"

-- glVertexArrayVertexBuffer ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffer.xhtml OpenGL 4.x>.
glVertexArrayVertexBuffer
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexArrayVertexBuffer v1 v2 v3 v4 v5 = liftIO $ dyn887 ptr_glVertexArrayVertexBuffer v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayVertexBuffer #-}
ptr_glVertexArrayVertexBuffer :: FunPtr (GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> IO ())
ptr_glVertexArrayVertexBuffer = unsafePerformIO $ getCommand "glVertexArrayVertexBuffer"

-- glVertexArrayVertexBuffers --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffers.xhtml OpenGL 4.x>.
glVertexArrayVertexBuffers
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @buffers@.
  -> Ptr GLintptr -- ^ @offsets@.
  -> Ptr GLsizei -- ^ @strides@.
  -> m ()
glVertexArrayVertexBuffers v1 v2 v3 v4 v5 v6 = liftIO $ dyn894 ptr_glVertexArrayVertexBuffers v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayVertexBuffers #-}
ptr_glVertexArrayVertexBuffers :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> IO ())
ptr_glVertexArrayVertexBuffers = unsafePerformIO $ getCommand "glVertexArrayVertexBuffers"

-- glVertexArrayVertexOffsetEXT ------------------------------------------------

glVertexArrayVertexOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayVertexOffsetEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn888 ptr_glVertexArrayVertexOffsetEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayVertexOffsetEXT #-}
ptr_glVertexArrayVertexOffsetEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayVertexOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayVertexOffsetEXT"

-- glVertexAttrib1d ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib1dv'.
glVertexAttrib1d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexAttrib1d v1 v2 = liftIO $ dyn895 ptr_glVertexAttrib1d v1 v2

{-# NOINLINE ptr_glVertexAttrib1d #-}
ptr_glVertexAttrib1d :: FunPtr (GLuint -> GLdouble -> IO ())
ptr_glVertexAttrib1d = unsafePerformIO $ getCommand "glVertexAttrib1d"

-- glVertexAttrib1dARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1dvARB'. This command is an alias for 'glVertexAttrib1d'.
glVertexAttrib1dARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexAttrib1dARB v1 v2 = liftIO $ dyn895 ptr_glVertexAttrib1dARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1dARB #-}
ptr_glVertexAttrib1dARB :: FunPtr (GLuint -> GLdouble -> IO ())
ptr_glVertexAttrib1dARB = unsafePerformIO $ getCommand "glVertexAttrib1dARB"

-- glVertexAttrib1dNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1dvNV'. This command is an alias for 'glVertexAttrib1d'.
glVertexAttrib1dNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexAttrib1dNV v1 v2 = liftIO $ dyn895 ptr_glVertexAttrib1dNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1dNV #-}
ptr_glVertexAttrib1dNV :: FunPtr (GLuint -> GLdouble -> IO ())
ptr_glVertexAttrib1dNV = unsafePerformIO $ getCommand "glVertexAttrib1dNV"

-- glVertexAttrib1dv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib1dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexAttrib1dv v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib1dv v1 v2

{-# NOINLINE ptr_glVertexAttrib1dv #-}
ptr_glVertexAttrib1dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib1dv = unsafePerformIO $ getCommand "glVertexAttrib1dv"

-- glVertexAttrib1dvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1dv'.
glVertexAttrib1dvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexAttrib1dvARB v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib1dvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1dvARB #-}
ptr_glVertexAttrib1dvARB :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib1dvARB = unsafePerformIO $ getCommand "glVertexAttrib1dvARB"

-- glVertexAttrib1dvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1dv'.
glVertexAttrib1dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexAttrib1dvNV v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib1dvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1dvNV #-}
ptr_glVertexAttrib1dvNV :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib1dvNV = unsafePerformIO $ getCommand "glVertexAttrib1dvNV"

-- glVertexAttrib1f ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib1fv'.
glVertexAttrib1f
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> m ()
glVertexAttrib1f v1 v2 = liftIO $ dyn896 ptr_glVertexAttrib1f v1 v2

{-# NOINLINE ptr_glVertexAttrib1f #-}
ptr_glVertexAttrib1f :: FunPtr (GLuint -> GLfloat -> IO ())
ptr_glVertexAttrib1f = unsafePerformIO $ getCommand "glVertexAttrib1f"

-- glVertexAttrib1fARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1fvARB'. This command is an alias for 'glVertexAttrib1f'.
glVertexAttrib1fARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> m ()
glVertexAttrib1fARB v1 v2 = liftIO $ dyn896 ptr_glVertexAttrib1fARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1fARB #-}
ptr_glVertexAttrib1fARB :: FunPtr (GLuint -> GLfloat -> IO ())
ptr_glVertexAttrib1fARB = unsafePerformIO $ getCommand "glVertexAttrib1fARB"

-- glVertexAttrib1fNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1fvNV'. This command is an alias for 'glVertexAttrib1f'.
glVertexAttrib1fNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> m ()
glVertexAttrib1fNV v1 v2 = liftIO $ dyn896 ptr_glVertexAttrib1fNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1fNV #-}
ptr_glVertexAttrib1fNV :: FunPtr (GLuint -> GLfloat -> IO ())
ptr_glVertexAttrib1fNV = unsafePerformIO $ getCommand "glVertexAttrib1fNV"

-- glVertexAttrib1fv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib1fv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glVertexAttrib1fv v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib1fv v1 v2

{-# NOINLINE ptr_glVertexAttrib1fv #-}
ptr_glVertexAttrib1fv :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib1fv = unsafePerformIO $ getCommand "glVertexAttrib1fv"

-- glVertexAttrib1fvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1fv'.
glVertexAttrib1fvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glVertexAttrib1fvARB v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib1fvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1fvARB #-}
ptr_glVertexAttrib1fvARB :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib1fvARB = unsafePerformIO $ getCommand "glVertexAttrib1fvARB"

-- glVertexAttrib1fvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1fv'.
glVertexAttrib1fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glVertexAttrib1fvNV v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib1fvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1fvNV #-}
ptr_glVertexAttrib1fvNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib1fvNV = unsafePerformIO $ getCommand "glVertexAttrib1fvNV"

-- glVertexAttrib1hNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1hvNV'.
glVertexAttrib1hNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> m ()
glVertexAttrib1hNV v1 v2 = liftIO $ dyn897 ptr_glVertexAttrib1hNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1hNV #-}
ptr_glVertexAttrib1hNV :: FunPtr (GLuint -> GLhalfNV -> IO ())
ptr_glVertexAttrib1hNV = unsafePerformIO $ getCommand "glVertexAttrib1hNV"

-- glVertexAttrib1hvNV ---------------------------------------------------------

glVertexAttrib1hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @1@ element of type @Half16NV@.
  -> m ()
glVertexAttrib1hvNV v1 v2 = liftIO $ dyn898 ptr_glVertexAttrib1hvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1hvNV #-}
ptr_glVertexAttrib1hvNV :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttrib1hvNV = unsafePerformIO $ getCommand "glVertexAttrib1hvNV"

-- glVertexAttrib1s ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib1sv'.
glVertexAttrib1s
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> m ()
glVertexAttrib1s v1 v2 = liftIO $ dyn899 ptr_glVertexAttrib1s v1 v2

{-# NOINLINE ptr_glVertexAttrib1s #-}
ptr_glVertexAttrib1s :: FunPtr (GLuint -> GLshort -> IO ())
ptr_glVertexAttrib1s = unsafePerformIO $ getCommand "glVertexAttrib1s"

-- glVertexAttrib1sARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1svARB'. This command is an alias for 'glVertexAttrib1s'.
glVertexAttrib1sARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> m ()
glVertexAttrib1sARB v1 v2 = liftIO $ dyn899 ptr_glVertexAttrib1sARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1sARB #-}
ptr_glVertexAttrib1sARB :: FunPtr (GLuint -> GLshort -> IO ())
ptr_glVertexAttrib1sARB = unsafePerformIO $ getCommand "glVertexAttrib1sARB"

-- glVertexAttrib1sNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1svNV'. This command is an alias for 'glVertexAttrib1s'.
glVertexAttrib1sNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> m ()
glVertexAttrib1sNV v1 v2 = liftIO $ dyn899 ptr_glVertexAttrib1sNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1sNV #-}
ptr_glVertexAttrib1sNV :: FunPtr (GLuint -> GLshort -> IO ())
ptr_glVertexAttrib1sNV = unsafePerformIO $ getCommand "glVertexAttrib1sNV"

-- glVertexAttrib1sv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib1sv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @1@ element of type @GLshort@.
  -> m ()
glVertexAttrib1sv v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib1sv v1 v2

{-# NOINLINE ptr_glVertexAttrib1sv #-}
ptr_glVertexAttrib1sv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib1sv = unsafePerformIO $ getCommand "glVertexAttrib1sv"

-- glVertexAttrib1svARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1sv'.
glVertexAttrib1svARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @1@ element of type @GLshort@.
  -> m ()
glVertexAttrib1svARB v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib1svARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1svARB #-}
ptr_glVertexAttrib1svARB :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib1svARB = unsafePerformIO $ getCommand "glVertexAttrib1svARB"

-- glVertexAttrib1svNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1sv'.
glVertexAttrib1svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @1@ element of type @GLshort@.
  -> m ()
glVertexAttrib1svNV v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib1svNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1svNV #-}
ptr_glVertexAttrib1svNV :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib1svNV = unsafePerformIO $ getCommand "glVertexAttrib1svNV"

-- glVertexAttrib2d ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib2dv'.
glVertexAttrib2d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexAttrib2d v1 v2 v3 = liftIO $ dyn228 ptr_glVertexAttrib2d v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2d #-}
ptr_glVertexAttrib2d :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib2d = unsafePerformIO $ getCommand "glVertexAttrib2d"

-- glVertexAttrib2dARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2dvARB'. This command is an alias for 'glVertexAttrib2d'.
glVertexAttrib2dARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexAttrib2dARB v1 v2 v3 = liftIO $ dyn228 ptr_glVertexAttrib2dARB v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2dARB #-}
ptr_glVertexAttrib2dARB :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib2dARB = unsafePerformIO $ getCommand "glVertexAttrib2dARB"

-- glVertexAttrib2dNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2dvNV'. This command is an alias for 'glVertexAttrib2d'.
glVertexAttrib2dNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexAttrib2dNV v1 v2 v3 = liftIO $ dyn228 ptr_glVertexAttrib2dNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2dNV #-}
ptr_glVertexAttrib2dNV :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib2dNV = unsafePerformIO $ getCommand "glVertexAttrib2dNV"

-- glVertexAttrib2dv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib2dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib2dv v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib2dv v1 v2

{-# NOINLINE ptr_glVertexAttrib2dv #-}
ptr_glVertexAttrib2dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib2dv = unsafePerformIO $ getCommand "glVertexAttrib2dv"

-- glVertexAttrib2dvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2dv'.
glVertexAttrib2dvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib2dvARB v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib2dvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib2dvARB #-}
ptr_glVertexAttrib2dvARB :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib2dvARB = unsafePerformIO $ getCommand "glVertexAttrib2dvARB"

-- glVertexAttrib2dvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2dv'.
glVertexAttrib2dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib2dvNV v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib2dvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib2dvNV #-}
ptr_glVertexAttrib2dvNV :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib2dvNV = unsafePerformIO $ getCommand "glVertexAttrib2dvNV"

-- glVertexAttrib2f ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib2fv'.
glVertexAttrib2f
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m ()
glVertexAttrib2f v1 v2 v3 = liftIO $ dyn229 ptr_glVertexAttrib2f v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2f #-}
ptr_glVertexAttrib2f :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib2f = unsafePerformIO $ getCommand "glVertexAttrib2f"

-- glVertexAttrib2fARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2fvARB'. This command is an alias for 'glVertexAttrib2f'.
glVertexAttrib2fARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m ()
glVertexAttrib2fARB v1 v2 v3 = liftIO $ dyn229 ptr_glVertexAttrib2fARB v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2fARB #-}
ptr_glVertexAttrib2fARB :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib2fARB = unsafePerformIO $ getCommand "glVertexAttrib2fARB"

-- glVertexAttrib2fNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2fvNV'. This command is an alias for 'glVertexAttrib2f'.
glVertexAttrib2fNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m ()
glVertexAttrib2fNV v1 v2 v3 = liftIO $ dyn229 ptr_glVertexAttrib2fNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2fNV #-}
ptr_glVertexAttrib2fNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib2fNV = unsafePerformIO $ getCommand "glVertexAttrib2fNV"

-- glVertexAttrib2fv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib2fv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib2fv v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib2fv v1 v2

{-# NOINLINE ptr_glVertexAttrib2fv #-}
ptr_glVertexAttrib2fv :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib2fv = unsafePerformIO $ getCommand "glVertexAttrib2fv"

-- glVertexAttrib2fvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2fv'.
glVertexAttrib2fvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib2fvARB v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib2fvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib2fvARB #-}
ptr_glVertexAttrib2fvARB :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib2fvARB = unsafePerformIO $ getCommand "glVertexAttrib2fvARB"

-- glVertexAttrib2fvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2fv'.
glVertexAttrib2fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib2fvNV v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib2fvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib2fvNV #-}
ptr_glVertexAttrib2fvNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib2fvNV = unsafePerformIO $ getCommand "glVertexAttrib2fvNV"

-- glVertexAttrib2hNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2hvNV'.
glVertexAttrib2hNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> m ()
glVertexAttrib2hNV v1 v2 v3 = liftIO $ dyn900 ptr_glVertexAttrib2hNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2hNV #-}
ptr_glVertexAttrib2hNV :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertexAttrib2hNV = unsafePerformIO $ getCommand "glVertexAttrib2hNV"

-- glVertexAttrib2hvNV ---------------------------------------------------------

glVertexAttrib2hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @2@ elements of type @Half16NV@.
  -> m ()
glVertexAttrib2hvNV v1 v2 = liftIO $ dyn898 ptr_glVertexAttrib2hvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib2hvNV #-}
ptr_glVertexAttrib2hvNV :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttrib2hvNV = unsafePerformIO $ getCommand "glVertexAttrib2hvNV"

-- glVertexAttrib2s ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib2sv'.
glVertexAttrib2s
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> m ()
glVertexAttrib2s v1 v2 v3 = liftIO $ dyn901 ptr_glVertexAttrib2s v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2s #-}
ptr_glVertexAttrib2s :: FunPtr (GLuint -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib2s = unsafePerformIO $ getCommand "glVertexAttrib2s"

-- glVertexAttrib2sARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2svARB'. This command is an alias for 'glVertexAttrib2s'.
glVertexAttrib2sARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> m ()
glVertexAttrib2sARB v1 v2 v3 = liftIO $ dyn901 ptr_glVertexAttrib2sARB v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2sARB #-}
ptr_glVertexAttrib2sARB :: FunPtr (GLuint -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib2sARB = unsafePerformIO $ getCommand "glVertexAttrib2sARB"

-- glVertexAttrib2sNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2svNV'. This command is an alias for 'glVertexAttrib2s'.
glVertexAttrib2sNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> m ()
glVertexAttrib2sNV v1 v2 v3 = liftIO $ dyn901 ptr_glVertexAttrib2sNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2sNV #-}
ptr_glVertexAttrib2sNV :: FunPtr (GLuint -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib2sNV = unsafePerformIO $ getCommand "glVertexAttrib2sNV"

-- glVertexAttrib2sv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib2sv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @GLshort@.
  -> m ()
glVertexAttrib2sv v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib2sv v1 v2

{-# NOINLINE ptr_glVertexAttrib2sv #-}
ptr_glVertexAttrib2sv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib2sv = unsafePerformIO $ getCommand "glVertexAttrib2sv"

-- glVertexAttrib2svARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2sv'.
glVertexAttrib2svARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @GLshort@.
  -> m ()
glVertexAttrib2svARB v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib2svARB v1 v2

{-# NOINLINE ptr_glVertexAttrib2svARB #-}
ptr_glVertexAttrib2svARB :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib2svARB = unsafePerformIO $ getCommand "glVertexAttrib2svARB"

-- glVertexAttrib2svNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2sv'.
glVertexAttrib2svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @GLshort@.
  -> m ()
glVertexAttrib2svNV v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib2svNV v1 v2

{-# NOINLINE ptr_glVertexAttrib2svNV #-}
ptr_glVertexAttrib2svNV :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib2svNV = unsafePerformIO $ getCommand "glVertexAttrib2svNV"

-- glVertexAttrib3d ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib3dv'.
glVertexAttrib3d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexAttrib3d v1 v2 v3 v4 = liftIO $ dyn902 ptr_glVertexAttrib3d v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3d #-}
ptr_glVertexAttrib3d :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib3d = unsafePerformIO $ getCommand "glVertexAttrib3d"

-- glVertexAttrib3dARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3dvARB'. This command is an alias for 'glVertexAttrib3d'.
glVertexAttrib3dARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexAttrib3dARB v1 v2 v3 v4 = liftIO $ dyn902 ptr_glVertexAttrib3dARB v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3dARB #-}
ptr_glVertexAttrib3dARB :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib3dARB = unsafePerformIO $ getCommand "glVertexAttrib3dARB"

-- glVertexAttrib3dNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3dvNV'. This command is an alias for 'glVertexAttrib3d'.
glVertexAttrib3dNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexAttrib3dNV v1 v2 v3 v4 = liftIO $ dyn902 ptr_glVertexAttrib3dNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3dNV #-}
ptr_glVertexAttrib3dNV :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib3dNV = unsafePerformIO $ getCommand "glVertexAttrib3dNV"

-- glVertexAttrib3dv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib3dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib3dv v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib3dv v1 v2

{-# NOINLINE ptr_glVertexAttrib3dv #-}
ptr_glVertexAttrib3dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib3dv = unsafePerformIO $ getCommand "glVertexAttrib3dv"

-- glVertexAttrib3dvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3dv'.
glVertexAttrib3dvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib3dvARB v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib3dvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib3dvARB #-}
ptr_glVertexAttrib3dvARB :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib3dvARB = unsafePerformIO $ getCommand "glVertexAttrib3dvARB"

-- glVertexAttrib3dvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3dv'.
glVertexAttrib3dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib3dvNV v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib3dvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib3dvNV #-}
ptr_glVertexAttrib3dvNV :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib3dvNV = unsafePerformIO $ getCommand "glVertexAttrib3dvNV"

-- glVertexAttrib3f ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib3fv'.
glVertexAttrib3f
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glVertexAttrib3f v1 v2 v3 v4 = liftIO $ dyn733 ptr_glVertexAttrib3f v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3f #-}
ptr_glVertexAttrib3f :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib3f = unsafePerformIO $ getCommand "glVertexAttrib3f"

-- glVertexAttrib3fARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3fvARB'. This command is an alias for 'glVertexAttrib3f'.
glVertexAttrib3fARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glVertexAttrib3fARB v1 v2 v3 v4 = liftIO $ dyn733 ptr_glVertexAttrib3fARB v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3fARB #-}
ptr_glVertexAttrib3fARB :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib3fARB = unsafePerformIO $ getCommand "glVertexAttrib3fARB"

-- glVertexAttrib3fNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3fvNV'. This command is an alias for 'glVertexAttrib3f'.
glVertexAttrib3fNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glVertexAttrib3fNV v1 v2 v3 v4 = liftIO $ dyn733 ptr_glVertexAttrib3fNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3fNV #-}
ptr_glVertexAttrib3fNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib3fNV = unsafePerformIO $ getCommand "glVertexAttrib3fNV"

-- glVertexAttrib3fv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib3fv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib3fv v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib3fv v1 v2

{-# NOINLINE ptr_glVertexAttrib3fv #-}
ptr_glVertexAttrib3fv :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib3fv = unsafePerformIO $ getCommand "glVertexAttrib3fv"

-- glVertexAttrib3fvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3fv'.
glVertexAttrib3fvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib3fvARB v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib3fvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib3fvARB #-}
ptr_glVertexAttrib3fvARB :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib3fvARB = unsafePerformIO $ getCommand "glVertexAttrib3fvARB"

-- glVertexAttrib3fvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3fv'.
glVertexAttrib3fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib3fvNV v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib3fvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib3fvNV #-}
ptr_glVertexAttrib3fvNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib3fvNV = unsafePerformIO $ getCommand "glVertexAttrib3fvNV"

-- glVertexAttrib3hNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3hvNV'.
glVertexAttrib3hNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> GLhalfNV -- ^ @z@ of type @Half16NV@.
  -> m ()
glVertexAttrib3hNV v1 v2 v3 v4 = liftIO $ dyn903 ptr_glVertexAttrib3hNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3hNV #-}
ptr_glVertexAttrib3hNV :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertexAttrib3hNV = unsafePerformIO $ getCommand "glVertexAttrib3hNV"

-- glVertexAttrib3hvNV ---------------------------------------------------------

glVertexAttrib3hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glVertexAttrib3hvNV v1 v2 = liftIO $ dyn898 ptr_glVertexAttrib3hvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib3hvNV #-}
ptr_glVertexAttrib3hvNV :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttrib3hvNV = unsafePerformIO $ getCommand "glVertexAttrib3hvNV"

-- glVertexAttrib3s ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib3sv'.
glVertexAttrib3s
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> m ()
glVertexAttrib3s v1 v2 v3 v4 = liftIO $ dyn904 ptr_glVertexAttrib3s v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3s #-}
ptr_glVertexAttrib3s :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib3s = unsafePerformIO $ getCommand "glVertexAttrib3s"

-- glVertexAttrib3sARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3svARB'. This command is an alias for 'glVertexAttrib3s'.
glVertexAttrib3sARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> m ()
glVertexAttrib3sARB v1 v2 v3 v4 = liftIO $ dyn904 ptr_glVertexAttrib3sARB v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3sARB #-}
ptr_glVertexAttrib3sARB :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib3sARB = unsafePerformIO $ getCommand "glVertexAttrib3sARB"

-- glVertexAttrib3sNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3svNV'. This command is an alias for 'glVertexAttrib3s'.
glVertexAttrib3sNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> m ()
glVertexAttrib3sNV v1 v2 v3 v4 = liftIO $ dyn904 ptr_glVertexAttrib3sNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3sNV #-}
ptr_glVertexAttrib3sNV :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib3sNV = unsafePerformIO $ getCommand "glVertexAttrib3sNV"

-- glVertexAttrib3sv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib3sv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glVertexAttrib3sv v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib3sv v1 v2

{-# NOINLINE ptr_glVertexAttrib3sv #-}
ptr_glVertexAttrib3sv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib3sv = unsafePerformIO $ getCommand "glVertexAttrib3sv"

-- glVertexAttrib3svARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3sv'.
glVertexAttrib3svARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glVertexAttrib3svARB v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib3svARB v1 v2

{-# NOINLINE ptr_glVertexAttrib3svARB #-}
ptr_glVertexAttrib3svARB :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib3svARB = unsafePerformIO $ getCommand "glVertexAttrib3svARB"

-- glVertexAttrib3svNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3sv'.
glVertexAttrib3svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glVertexAttrib3svNV v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib3svNV v1 v2

{-# NOINLINE ptr_glVertexAttrib3svNV #-}
ptr_glVertexAttrib3svNV :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib3svNV = unsafePerformIO $ getCommand "glVertexAttrib3svNV"

-- glVertexAttrib4Nbv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nbv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttrib4Nbv v1 v2 = liftIO $ dyn881 ptr_glVertexAttrib4Nbv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Nbv #-}
ptr_glVertexAttrib4Nbv :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttrib4Nbv = unsafePerformIO $ getCommand "glVertexAttrib4Nbv"

-- glVertexAttrib4NbvARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nbv'.
glVertexAttrib4NbvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttrib4NbvARB v1 v2 = liftIO $ dyn881 ptr_glVertexAttrib4NbvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NbvARB #-}
ptr_glVertexAttrib4NbvARB :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttrib4NbvARB = unsafePerformIO $ getCommand "glVertexAttrib4NbvARB"

-- glVertexAttrib4Niv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Niv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttrib4Niv v1 v2 = liftIO $ dyn741 ptr_glVertexAttrib4Niv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Niv #-}
ptr_glVertexAttrib4Niv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttrib4Niv = unsafePerformIO $ getCommand "glVertexAttrib4Niv"

-- glVertexAttrib4NivARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Niv'.
glVertexAttrib4NivARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttrib4NivARB v1 v2 = liftIO $ dyn741 ptr_glVertexAttrib4NivARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NivARB #-}
ptr_glVertexAttrib4NivARB :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttrib4NivARB = unsafePerformIO $ getCommand "glVertexAttrib4NivARB"

-- glVertexAttrib4Nsv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nsv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttrib4Nsv v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib4Nsv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Nsv #-}
ptr_glVertexAttrib4Nsv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib4Nsv = unsafePerformIO $ getCommand "glVertexAttrib4Nsv"

-- glVertexAttrib4NsvARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nsv'.
glVertexAttrib4NsvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttrib4NsvARB v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib4NsvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NsvARB #-}
ptr_glVertexAttrib4NsvARB :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib4NsvARB = unsafePerformIO $ getCommand "glVertexAttrib4NsvARB"

-- glVertexAttrib4Nub ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nub
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLubyte -- ^ @x@.
  -> GLubyte -- ^ @y@.
  -> GLubyte -- ^ @z@.
  -> GLubyte -- ^ @w@.
  -> m ()
glVertexAttrib4Nub v1 v2 v3 v4 v5 = liftIO $ dyn905 ptr_glVertexAttrib4Nub v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4Nub #-}
ptr_glVertexAttrib4Nub :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glVertexAttrib4Nub = unsafePerformIO $ getCommand "glVertexAttrib4Nub"

-- glVertexAttrib4NubARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nub'.
glVertexAttrib4NubARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLubyte -- ^ @x@.
  -> GLubyte -- ^ @y@.
  -> GLubyte -- ^ @z@.
  -> GLubyte -- ^ @w@.
  -> m ()
glVertexAttrib4NubARB v1 v2 v3 v4 v5 = liftIO $ dyn905 ptr_glVertexAttrib4NubARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4NubARB #-}
ptr_glVertexAttrib4NubARB :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glVertexAttrib4NubARB = unsafePerformIO $ getCommand "glVertexAttrib4NubARB"

-- glVertexAttrib4Nubv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nubv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttrib4Nubv v1 v2 = liftIO $ dyn393 ptr_glVertexAttrib4Nubv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Nubv #-}
ptr_glVertexAttrib4Nubv :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttrib4Nubv = unsafePerformIO $ getCommand "glVertexAttrib4Nubv"

-- glVertexAttrib4NubvARB ------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nubv'.
glVertexAttrib4NubvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttrib4NubvARB v1 v2 = liftIO $ dyn393 ptr_glVertexAttrib4NubvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NubvARB #-}
ptr_glVertexAttrib4NubvARB :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttrib4NubvARB = unsafePerformIO $ getCommand "glVertexAttrib4NubvARB"

-- glVertexAttrib4Nuiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nuiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttrib4Nuiv v1 v2 = liftIO $ dyn201 ptr_glVertexAttrib4Nuiv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Nuiv #-}
ptr_glVertexAttrib4Nuiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttrib4Nuiv = unsafePerformIO $ getCommand "glVertexAttrib4Nuiv"

-- glVertexAttrib4NuivARB ------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nuiv'.
glVertexAttrib4NuivARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttrib4NuivARB v1 v2 = liftIO $ dyn201 ptr_glVertexAttrib4NuivARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NuivARB #-}
ptr_glVertexAttrib4NuivARB :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttrib4NuivARB = unsafePerformIO $ getCommand "glVertexAttrib4NuivARB"

-- glVertexAttrib4Nusv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nusv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttrib4Nusv v1 v2 = liftIO $ dyn884 ptr_glVertexAttrib4Nusv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Nusv #-}
ptr_glVertexAttrib4Nusv :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttrib4Nusv = unsafePerformIO $ getCommand "glVertexAttrib4Nusv"

-- glVertexAttrib4NusvARB ------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nusv'.
glVertexAttrib4NusvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttrib4NusvARB v1 v2 = liftIO $ dyn884 ptr_glVertexAttrib4NusvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NusvARB #-}
ptr_glVertexAttrib4NusvARB :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttrib4NusvARB = unsafePerformIO $ getCommand "glVertexAttrib4NusvARB"

-- glVertexAttrib4bv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4bv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttrib4bv v1 v2 = liftIO $ dyn881 ptr_glVertexAttrib4bv v1 v2

{-# NOINLINE ptr_glVertexAttrib4bv #-}
ptr_glVertexAttrib4bv :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttrib4bv = unsafePerformIO $ getCommand "glVertexAttrib4bv"

-- glVertexAttrib4bvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4bv'.
glVertexAttrib4bvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttrib4bvARB v1 v2 = liftIO $ dyn881 ptr_glVertexAttrib4bvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4bvARB #-}
ptr_glVertexAttrib4bvARB :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttrib4bvARB = unsafePerformIO $ getCommand "glVertexAttrib4bvARB"

-- glVertexAttrib4d ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib4dv'.
glVertexAttrib4d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexAttrib4d v1 v2 v3 v4 v5 = liftIO $ dyn906 ptr_glVertexAttrib4d v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4d #-}
ptr_glVertexAttrib4d :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib4d = unsafePerformIO $ getCommand "glVertexAttrib4d"

-- glVertexAttrib4dARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4dvARB'. This command is an alias for 'glVertexAttrib4d'.
glVertexAttrib4dARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexAttrib4dARB v1 v2 v3 v4 v5 = liftIO $ dyn906 ptr_glVertexAttrib4dARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4dARB #-}
ptr_glVertexAttrib4dARB :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib4dARB = unsafePerformIO $ getCommand "glVertexAttrib4dARB"

-- glVertexAttrib4dNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4dvNV'. This command is an alias for 'glVertexAttrib4d'.
glVertexAttrib4dNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexAttrib4dNV v1 v2 v3 v4 v5 = liftIO $ dyn906 ptr_glVertexAttrib4dNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4dNV #-}
ptr_glVertexAttrib4dNV :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib4dNV = unsafePerformIO $ getCommand "glVertexAttrib4dNV"

-- glVertexAttrib4dv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib4dv v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib4dv v1 v2

{-# NOINLINE ptr_glVertexAttrib4dv #-}
ptr_glVertexAttrib4dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib4dv = unsafePerformIO $ getCommand "glVertexAttrib4dv"

-- glVertexAttrib4dvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4dv'.
glVertexAttrib4dvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib4dvARB v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib4dvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4dvARB #-}
ptr_glVertexAttrib4dvARB :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib4dvARB = unsafePerformIO $ getCommand "glVertexAttrib4dvARB"

-- glVertexAttrib4dvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4dv'.
glVertexAttrib4dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib4dvNV v1 v2 = liftIO $ dyn882 ptr_glVertexAttrib4dvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib4dvNV #-}
ptr_glVertexAttrib4dvNV :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib4dvNV = unsafePerformIO $ getCommand "glVertexAttrib4dvNV"

-- glVertexAttrib4f ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib4fv'.
glVertexAttrib4f
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glVertexAttrib4f v1 v2 v3 v4 v5 = liftIO $ dyn907 ptr_glVertexAttrib4f v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4f #-}
ptr_glVertexAttrib4f :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib4f = unsafePerformIO $ getCommand "glVertexAttrib4f"

-- glVertexAttrib4fARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4fvARB'. This command is an alias for 'glVertexAttrib4f'.
glVertexAttrib4fARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glVertexAttrib4fARB v1 v2 v3 v4 v5 = liftIO $ dyn907 ptr_glVertexAttrib4fARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4fARB #-}
ptr_glVertexAttrib4fARB :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib4fARB = unsafePerformIO $ getCommand "glVertexAttrib4fARB"

-- glVertexAttrib4fNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4fvNV'. This command is an alias for 'glVertexAttrib4f'.
glVertexAttrib4fNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glVertexAttrib4fNV v1 v2 v3 v4 v5 = liftIO $ dyn907 ptr_glVertexAttrib4fNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4fNV #-}
ptr_glVertexAttrib4fNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib4fNV = unsafePerformIO $ getCommand "glVertexAttrib4fNV"

-- glVertexAttrib4fv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4fv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib4fv v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib4fv v1 v2

{-# NOINLINE ptr_glVertexAttrib4fv #-}
ptr_glVertexAttrib4fv :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib4fv = unsafePerformIO $ getCommand "glVertexAttrib4fv"

-- glVertexAttrib4fvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4fv'.
glVertexAttrib4fvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib4fvARB v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib4fvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4fvARB #-}
ptr_glVertexAttrib4fvARB :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib4fvARB = unsafePerformIO $ getCommand "glVertexAttrib4fvARB"

-- glVertexAttrib4fvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4fv'.
glVertexAttrib4fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib4fvNV v1 v2 = liftIO $ dyn394 ptr_glVertexAttrib4fvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib4fvNV #-}
ptr_glVertexAttrib4fvNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib4fvNV = unsafePerformIO $ getCommand "glVertexAttrib4fvNV"

-- glVertexAttrib4hNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4hvNV'.
glVertexAttrib4hNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> GLhalfNV -- ^ @z@ of type @Half16NV@.
  -> GLhalfNV -- ^ @w@ of type @Half16NV@.
  -> m ()
glVertexAttrib4hNV v1 v2 v3 v4 v5 = liftIO $ dyn908 ptr_glVertexAttrib4hNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4hNV #-}
ptr_glVertexAttrib4hNV :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertexAttrib4hNV = unsafePerformIO $ getCommand "glVertexAttrib4hNV"

-- glVertexAttrib4hvNV ---------------------------------------------------------

glVertexAttrib4hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @4@ elements of type @Half16NV@.
  -> m ()
glVertexAttrib4hvNV v1 v2 = liftIO $ dyn898 ptr_glVertexAttrib4hvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib4hvNV #-}
ptr_glVertexAttrib4hvNV :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttrib4hvNV = unsafePerformIO $ getCommand "glVertexAttrib4hvNV"

-- glVertexAttrib4iv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4iv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttrib4iv v1 v2 = liftIO $ dyn741 ptr_glVertexAttrib4iv v1 v2

{-# NOINLINE ptr_glVertexAttrib4iv #-}
ptr_glVertexAttrib4iv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttrib4iv = unsafePerformIO $ getCommand "glVertexAttrib4iv"

-- glVertexAttrib4ivARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4iv'.
glVertexAttrib4ivARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttrib4ivARB v1 v2 = liftIO $ dyn741 ptr_glVertexAttrib4ivARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4ivARB #-}
ptr_glVertexAttrib4ivARB :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttrib4ivARB = unsafePerformIO $ getCommand "glVertexAttrib4ivARB"

-- glVertexAttrib4s ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib4sv'.
glVertexAttrib4s
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> GLshort -- ^ @w@.
  -> m ()
glVertexAttrib4s v1 v2 v3 v4 v5 = liftIO $ dyn909 ptr_glVertexAttrib4s v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4s #-}
ptr_glVertexAttrib4s :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib4s = unsafePerformIO $ getCommand "glVertexAttrib4s"

-- glVertexAttrib4sARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4svARB'. This command is an alias for 'glVertexAttrib4s'.
glVertexAttrib4sARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> GLshort -- ^ @w@.
  -> m ()
glVertexAttrib4sARB v1 v2 v3 v4 v5 = liftIO $ dyn909 ptr_glVertexAttrib4sARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4sARB #-}
ptr_glVertexAttrib4sARB :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib4sARB = unsafePerformIO $ getCommand "glVertexAttrib4sARB"

-- glVertexAttrib4sNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4svNV'. This command is an alias for 'glVertexAttrib4s'.
glVertexAttrib4sNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> GLshort -- ^ @w@.
  -> m ()
glVertexAttrib4sNV v1 v2 v3 v4 v5 = liftIO $ dyn909 ptr_glVertexAttrib4sNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4sNV #-}
ptr_glVertexAttrib4sNV :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib4sNV = unsafePerformIO $ getCommand "glVertexAttrib4sNV"

-- glVertexAttrib4sv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4sv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttrib4sv v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib4sv v1 v2

{-# NOINLINE ptr_glVertexAttrib4sv #-}
ptr_glVertexAttrib4sv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib4sv = unsafePerformIO $ getCommand "glVertexAttrib4sv"

