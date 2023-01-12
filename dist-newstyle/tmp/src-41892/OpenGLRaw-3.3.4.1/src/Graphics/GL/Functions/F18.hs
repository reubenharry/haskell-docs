{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F18
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

module Graphics.GL.Functions.F18 (
  glMultiTexCoord3xvOES,
  glMultiTexCoord4bOES,
  glMultiTexCoord4bvOES,
  glMultiTexCoord4d,
  glMultiTexCoord4dARB,
  glMultiTexCoord4dv,
  glMultiTexCoord4dvARB,
  glMultiTexCoord4f,
  glMultiTexCoord4fARB,
  glMultiTexCoord4fv,
  glMultiTexCoord4fvARB,
  glMultiTexCoord4hNV,
  glMultiTexCoord4hvNV,
  glMultiTexCoord4i,
  glMultiTexCoord4iARB,
  glMultiTexCoord4iv,
  glMultiTexCoord4ivARB,
  glMultiTexCoord4s,
  glMultiTexCoord4sARB,
  glMultiTexCoord4sv,
  glMultiTexCoord4svARB,
  glMultiTexCoord4x,
  glMultiTexCoord4xOES,
  glMultiTexCoord4xvOES,
  glMultiTexCoordP1ui,
  glMultiTexCoordP1uiv,
  glMultiTexCoordP2ui,
  glMultiTexCoordP2uiv,
  glMultiTexCoordP3ui,
  glMultiTexCoordP3uiv,
  glMultiTexCoordP4ui,
  glMultiTexCoordP4uiv,
  glMultiTexCoordPointerEXT,
  glMultiTexEnvfEXT,
  glMultiTexEnvfvEXT,
  glMultiTexEnviEXT,
  glMultiTexEnvivEXT,
  glMultiTexGendEXT,
  glMultiTexGendvEXT,
  glMultiTexGenfEXT,
  glMultiTexGenfvEXT,
  glMultiTexGeniEXT,
  glMultiTexGenivEXT,
  glMultiTexImage1DEXT,
  glMultiTexImage2DEXT,
  glMultiTexImage3DEXT,
  glMultiTexParameterIivEXT,
  glMultiTexParameterIuivEXT,
  glMultiTexParameterfEXT,
  glMultiTexParameterfvEXT,
  glMultiTexParameteriEXT,
  glMultiTexParameterivEXT,
  glMultiTexRenderbufferEXT,
  glMultiTexSubImage1DEXT,
  glMultiTexSubImage2DEXT,
  glMultiTexSubImage3DEXT,
  glMulticastBarrierNV,
  glMulticastBlitFramebufferNV,
  glMulticastBufferSubDataNV,
  glMulticastCopyBufferSubDataNV,
  glMulticastCopyImageSubDataNV,
  glMulticastFramebufferSampleLocationsfvNV,
  glMulticastGetQueryObjecti64vNV,
  glMulticastGetQueryObjectivNV,
  glMulticastGetQueryObjectui64vNV,
  glMulticastGetQueryObjectuivNV,
  glMulticastScissorArrayvNVX,
  glMulticastViewportArrayvNVX,
  glMulticastViewportPositionWScaleNVX,
  glMulticastWaitSyncNV,
  glNamedBufferAttachMemoryNV,
  glNamedBufferData,
  glNamedBufferDataEXT,
  glNamedBufferPageCommitmentARB,
  glNamedBufferPageCommitmentEXT,
  glNamedBufferStorage,
  glNamedBufferStorageEXT,
  glNamedBufferStorageExternalEXT,
  glNamedBufferStorageMemEXT,
  glNamedBufferSubData,
  glNamedBufferSubDataEXT,
  glNamedCopyBufferSubDataEXT,
  glNamedFramebufferDrawBuffer,
  glNamedFramebufferDrawBuffers,
  glNamedFramebufferParameteri,
  glNamedFramebufferParameteriEXT,
  glNamedFramebufferReadBuffer,
  glNamedFramebufferRenderbuffer,
  glNamedFramebufferRenderbufferEXT,
  glNamedFramebufferSampleLocationsfvARB,
  glNamedFramebufferSampleLocationsfvNV,
  glNamedFramebufferSamplePositionsfvAMD,
  glNamedFramebufferTexture,
  glNamedFramebufferTexture1DEXT,
  glNamedFramebufferTexture2DEXT,
  glNamedFramebufferTexture3DEXT,
  glNamedFramebufferTextureEXT,
  glNamedFramebufferTextureFaceEXT,
  glNamedFramebufferTextureLayer,
  glNamedFramebufferTextureLayerEXT
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glMultiTexCoord3xvOES -------------------------------------------------------

glMultiTexCoord3xvOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfixed -- ^ @coords@ pointing to @3@ elements of type @GLfixed@.
  -> m ()
glMultiTexCoord3xvOES v1 v2 = liftIO $ dyn102 ptr_glMultiTexCoord3xvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord3xvOES #-}
ptr_glMultiTexCoord3xvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glMultiTexCoord3xvOES = unsafePerformIO $ getCommand "glMultiTexCoord3xvOES"

-- glMultiTexCoord4bOES --------------------------------------------------------

glMultiTexCoord4bOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLbyte -- ^ @s@.
  -> GLbyte -- ^ @t@.
  -> GLbyte -- ^ @r@.
  -> GLbyte -- ^ @q@.
  -> m ()
glMultiTexCoord4bOES v1 v2 v3 v4 v5 = liftIO $ dyn585 ptr_glMultiTexCoord4bOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4bOES #-}
ptr_glMultiTexCoord4bOES :: FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glMultiTexCoord4bOES = unsafePerformIO $ getCommand "glMultiTexCoord4bOES"

-- glMultiTexCoord4bvOES -------------------------------------------------------

glMultiTexCoord4bvOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLbyte -- ^ @coords@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glMultiTexCoord4bvOES v1 v2 = liftIO $ dyn568 ptr_glMultiTexCoord4bvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord4bvOES #-}
ptr_glMultiTexCoord4bvOES :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
ptr_glMultiTexCoord4bvOES = unsafePerformIO $ getCommand "glMultiTexCoord4bvOES"

-- glMultiTexCoord4d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord4dv'.
glMultiTexCoord4d
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> GLdouble -- ^ @r@ of type @CoordD@.
  -> GLdouble -- ^ @q@ of type @CoordD@.
  -> m ()
glMultiTexCoord4d v1 v2 v3 v4 v5 = liftIO $ dyn546 ptr_glMultiTexCoord4d v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4d #-}
ptr_glMultiTexCoord4d :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord4d = unsafePerformIO $ getCommand "glMultiTexCoord4d"

-- glMultiTexCoord4dARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord4dv'. This command is an alias for 'glMultiTexCoord4d'.
glMultiTexCoord4dARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> GLdouble -- ^ @r@ of type @CoordD@.
  -> GLdouble -- ^ @q@ of type @CoordD@.
  -> m ()
glMultiTexCoord4dARB v1 v2 v3 v4 v5 = liftIO $ dyn546 ptr_glMultiTexCoord4dARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4dARB #-}
ptr_glMultiTexCoord4dARB :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord4dARB = unsafePerformIO $ getCommand "glMultiTexCoord4dARB"

-- glMultiTexCoord4dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord4dv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord4dv v1 v2 = liftIO $ dyn100 ptr_glMultiTexCoord4dv v1 v2

{-# NOINLINE ptr_glMultiTexCoord4dv #-}
ptr_glMultiTexCoord4dv :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord4dv = unsafePerformIO $ getCommand "glMultiTexCoord4dv"

-- glMultiTexCoord4dvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord4dv'.
glMultiTexCoord4dvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord4dvARB v1 v2 = liftIO $ dyn100 ptr_glMultiTexCoord4dvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord4dvARB #-}
ptr_glMultiTexCoord4dvARB :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord4dvARB = unsafePerformIO $ getCommand "glMultiTexCoord4dvARB"

-- glMultiTexCoord4f -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord4fv'.
glMultiTexCoord4f
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> GLfloat -- ^ @r@ of type @CoordF@.
  -> GLfloat -- ^ @q@ of type @CoordF@.
  -> m ()
glMultiTexCoord4f v1 v2 v3 v4 v5 = liftIO $ dyn547 ptr_glMultiTexCoord4f v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4f #-}
ptr_glMultiTexCoord4f :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord4f = unsafePerformIO $ getCommand "glMultiTexCoord4f"

-- glMultiTexCoord4fARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord4fv'. This command is an alias for 'glMultiTexCoord4f'.
glMultiTexCoord4fARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> GLfloat -- ^ @r@ of type @CoordF@.
  -> GLfloat -- ^ @q@ of type @CoordF@.
  -> m ()
glMultiTexCoord4fARB v1 v2 v3 v4 v5 = liftIO $ dyn547 ptr_glMultiTexCoord4fARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4fARB #-}
ptr_glMultiTexCoord4fARB :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord4fARB = unsafePerformIO $ getCommand "glMultiTexCoord4fARB"

-- glMultiTexCoord4fv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord4fv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord4fv v1 v2 = liftIO $ dyn101 ptr_glMultiTexCoord4fv v1 v2

{-# NOINLINE ptr_glMultiTexCoord4fv #-}
ptr_glMultiTexCoord4fv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord4fv = unsafePerformIO $ getCommand "glMultiTexCoord4fv"

-- glMultiTexCoord4fvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord4fv'.
glMultiTexCoord4fvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord4fvARB v1 v2 = liftIO $ dyn101 ptr_glMultiTexCoord4fvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord4fvARB #-}
ptr_glMultiTexCoord4fvARB :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord4fvARB = unsafePerformIO $ getCommand "glMultiTexCoord4fvARB"

-- glMultiTexCoord4hNV ---------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord4hvNV'.
glMultiTexCoord4hNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> GLhalfNV -- ^ @t@ of type @Half16NV@.
  -> GLhalfNV -- ^ @r@ of type @Half16NV@.
  -> GLhalfNV -- ^ @q@ of type @Half16NV@.
  -> m ()
glMultiTexCoord4hNV v1 v2 v3 v4 v5 = liftIO $ dyn586 ptr_glMultiTexCoord4hNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4hNV #-}
ptr_glMultiTexCoord4hNV :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glMultiTexCoord4hNV = unsafePerformIO $ getCommand "glMultiTexCoord4hNV"

-- glMultiTexCoord4hvNV --------------------------------------------------------

glMultiTexCoord4hvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLhalfNV -- ^ @v@ pointing to @4@ elements of type @Half16NV@.
  -> m ()
glMultiTexCoord4hvNV v1 v2 = liftIO $ dyn571 ptr_glMultiTexCoord4hvNV v1 v2

{-# NOINLINE ptr_glMultiTexCoord4hvNV #-}
ptr_glMultiTexCoord4hvNV :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
ptr_glMultiTexCoord4hvNV = unsafePerformIO $ getCommand "glMultiTexCoord4hvNV"

-- glMultiTexCoord4i -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord4iv'.
glMultiTexCoord4i
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> GLint -- ^ @r@ of type @CoordI@.
  -> GLint -- ^ @q@ of type @CoordI@.
  -> m ()
glMultiTexCoord4i v1 v2 v3 v4 v5 = liftIO $ dyn276 ptr_glMultiTexCoord4i v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4i #-}
ptr_glMultiTexCoord4i :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord4i = unsafePerformIO $ getCommand "glMultiTexCoord4i"

-- glMultiTexCoord4iARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord4iv'. This command is an alias for 'glMultiTexCoord4i'.
glMultiTexCoord4iARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> GLint -- ^ @r@ of type @CoordI@.
  -> GLint -- ^ @q@ of type @CoordI@.
  -> m ()
glMultiTexCoord4iARB v1 v2 v3 v4 v5 = liftIO $ dyn276 ptr_glMultiTexCoord4iARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4iARB #-}
ptr_glMultiTexCoord4iARB :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord4iARB = unsafePerformIO $ getCommand "glMultiTexCoord4iARB"

-- glMultiTexCoord4iv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord4iv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord4iv v1 v2 = liftIO $ dyn143 ptr_glMultiTexCoord4iv v1 v2

{-# NOINLINE ptr_glMultiTexCoord4iv #-}
ptr_glMultiTexCoord4iv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord4iv = unsafePerformIO $ getCommand "glMultiTexCoord4iv"

-- glMultiTexCoord4ivARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord4iv'.
glMultiTexCoord4ivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord4ivARB v1 v2 = liftIO $ dyn143 ptr_glMultiTexCoord4ivARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord4ivARB #-}
ptr_glMultiTexCoord4ivARB :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord4ivARB = unsafePerformIO $ getCommand "glMultiTexCoord4ivARB"

-- glMultiTexCoord4s -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord4sv'.
glMultiTexCoord4s
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> GLshort -- ^ @r@ of type @CoordS@.
  -> GLshort -- ^ @q@ of type @CoordS@.
  -> m ()
glMultiTexCoord4s v1 v2 v3 v4 v5 = liftIO $ dyn587 ptr_glMultiTexCoord4s v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4s #-}
ptr_glMultiTexCoord4s :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord4s = unsafePerformIO $ getCommand "glMultiTexCoord4s"

-- glMultiTexCoord4sARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord4sv'. This command is an alias for 'glMultiTexCoord4s'.
glMultiTexCoord4sARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> GLshort -- ^ @r@ of type @CoordS@.
  -> GLshort -- ^ @q@ of type @CoordS@.
  -> m ()
glMultiTexCoord4sARB v1 v2 v3 v4 v5 = liftIO $ dyn587 ptr_glMultiTexCoord4sARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4sARB #-}
ptr_glMultiTexCoord4sARB :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord4sARB = unsafePerformIO $ getCommand "glMultiTexCoord4sARB"

-- glMultiTexCoord4sv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord4sv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord4sv v1 v2 = liftIO $ dyn573 ptr_glMultiTexCoord4sv v1 v2

{-# NOINLINE ptr_glMultiTexCoord4sv #-}
ptr_glMultiTexCoord4sv :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord4sv = unsafePerformIO $ getCommand "glMultiTexCoord4sv"

-- glMultiTexCoord4svARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord4sv'.
glMultiTexCoord4svARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord4svARB v1 v2 = liftIO $ dyn573 ptr_glMultiTexCoord4svARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord4svARB #-}
ptr_glMultiTexCoord4svARB :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord4svARB = unsafePerformIO $ getCommand "glMultiTexCoord4svARB"

-- glMultiTexCoord4x -----------------------------------------------------------

glMultiTexCoord4x
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @q@.
  -> m ()
glMultiTexCoord4x v1 v2 v3 v4 v5 = liftIO $ dyn588 ptr_glMultiTexCoord4x v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4x #-}
ptr_glMultiTexCoord4x :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glMultiTexCoord4x = unsafePerformIO $ getCommand "glMultiTexCoord4x"

-- glMultiTexCoord4xOES --------------------------------------------------------

glMultiTexCoord4xOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @q@.
  -> m ()
glMultiTexCoord4xOES v1 v2 v3 v4 v5 = liftIO $ dyn588 ptr_glMultiTexCoord4xOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4xOES #-}
ptr_glMultiTexCoord4xOES :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glMultiTexCoord4xOES = unsafePerformIO $ getCommand "glMultiTexCoord4xOES"

-- glMultiTexCoord4xvOES -------------------------------------------------------

glMultiTexCoord4xvOES
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> Ptr GLfixed -- ^ @coords@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glMultiTexCoord4xvOES v1 v2 = liftIO $ dyn102 ptr_glMultiTexCoord4xvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord4xvOES #-}
ptr_glMultiTexCoord4xvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glMultiTexCoord4xvOES = unsafePerformIO $ getCommand "glMultiTexCoord4xvOES"

-- glMultiTexCoordP1ui ---------------------------------------------------------

glMultiTexCoordP1ui
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLuint -- ^ @coords@.
  -> m ()
glMultiTexCoordP1ui v1 v2 v3 = liftIO $ dyn32 ptr_glMultiTexCoordP1ui v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP1ui #-}
ptr_glMultiTexCoordP1ui :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexCoordP1ui = unsafePerformIO $ getCommand "glMultiTexCoordP1ui"

-- glMultiTexCoordP1uiv --------------------------------------------------------

glMultiTexCoordP1uiv
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glMultiTexCoordP1uiv v1 v2 v3 = liftIO $ dyn432 ptr_glMultiTexCoordP1uiv v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP1uiv #-}
ptr_glMultiTexCoordP1uiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glMultiTexCoordP1uiv = unsafePerformIO $ getCommand "glMultiTexCoordP1uiv"

-- glMultiTexCoordP2ui ---------------------------------------------------------

glMultiTexCoordP2ui
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLuint -- ^ @coords@.
  -> m ()
glMultiTexCoordP2ui v1 v2 v3 = liftIO $ dyn32 ptr_glMultiTexCoordP2ui v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP2ui #-}
ptr_glMultiTexCoordP2ui :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexCoordP2ui = unsafePerformIO $ getCommand "glMultiTexCoordP2ui"

-- glMultiTexCoordP2uiv --------------------------------------------------------

glMultiTexCoordP2uiv
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glMultiTexCoordP2uiv v1 v2 v3 = liftIO $ dyn432 ptr_glMultiTexCoordP2uiv v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP2uiv #-}
ptr_glMultiTexCoordP2uiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glMultiTexCoordP2uiv = unsafePerformIO $ getCommand "glMultiTexCoordP2uiv"

-- glMultiTexCoordP3ui ---------------------------------------------------------

glMultiTexCoordP3ui
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLuint -- ^ @coords@.
  -> m ()
glMultiTexCoordP3ui v1 v2 v3 = liftIO $ dyn32 ptr_glMultiTexCoordP3ui v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP3ui #-}
ptr_glMultiTexCoordP3ui :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexCoordP3ui = unsafePerformIO $ getCommand "glMultiTexCoordP3ui"

-- glMultiTexCoordP3uiv --------------------------------------------------------

glMultiTexCoordP3uiv
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glMultiTexCoordP3uiv v1 v2 v3 = liftIO $ dyn432 ptr_glMultiTexCoordP3uiv v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP3uiv #-}
ptr_glMultiTexCoordP3uiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glMultiTexCoordP3uiv = unsafePerformIO $ getCommand "glMultiTexCoordP3uiv"

-- glMultiTexCoordP4ui ---------------------------------------------------------

glMultiTexCoordP4ui
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLuint -- ^ @coords@.
  -> m ()
glMultiTexCoordP4ui v1 v2 v3 = liftIO $ dyn32 ptr_glMultiTexCoordP4ui v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP4ui #-}
ptr_glMultiTexCoordP4ui :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexCoordP4ui = unsafePerformIO $ getCommand "glMultiTexCoordP4ui"

-- glMultiTexCoordP4uiv --------------------------------------------------------

glMultiTexCoordP4uiv
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glMultiTexCoordP4uiv v1 v2 v3 = liftIO $ dyn432 ptr_glMultiTexCoordP4uiv v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP4uiv #-}
ptr_glMultiTexCoordP4uiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glMultiTexCoordP4uiv = unsafePerformIO $ getCommand "glMultiTexCoordP4uiv"

-- glMultiTexCoordPointerEXT ---------------------------------------------------

glMultiTexCoordPointerEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glMultiTexCoordPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn589 ptr_glMultiTexCoordPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoordPointerEXT #-}
ptr_glMultiTexCoordPointerEXT :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glMultiTexCoordPointerEXT = unsafePerformIO $ getCommand "glMultiTexCoordPointerEXT"

-- glMultiTexEnvfEXT -----------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexEnvfvEXT'.
glMultiTexEnvfEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glMultiTexEnvfEXT v1 v2 v3 v4 = liftIO $ dyn590 ptr_glMultiTexEnvfEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexEnvfEXT #-}
ptr_glMultiTexEnvfEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLfloat -> IO ())
ptr_glMultiTexEnvfEXT = unsafePerformIO $ getCommand "glMultiTexEnvfEXT"

-- glMultiTexEnvfvEXT ----------------------------------------------------------

glMultiTexEnvfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glMultiTexEnvfvEXT v1 v2 v3 v4 = liftIO $ dyn334 ptr_glMultiTexEnvfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexEnvfvEXT #-}
ptr_glMultiTexEnvfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexEnvfvEXT = unsafePerformIO $ getCommand "glMultiTexEnvfvEXT"

-- glMultiTexEnviEXT -----------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexEnvivEXT'.
glMultiTexEnviEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glMultiTexEnviEXT v1 v2 v3 v4 = liftIO $ dyn591 ptr_glMultiTexEnviEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexEnviEXT #-}
ptr_glMultiTexEnviEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLint -> IO ())
ptr_glMultiTexEnviEXT = unsafePerformIO $ getCommand "glMultiTexEnviEXT"

-- glMultiTexEnvivEXT ----------------------------------------------------------

glMultiTexEnvivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glMultiTexEnvivEXT v1 v2 v3 v4 = liftIO $ dyn335 ptr_glMultiTexEnvivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexEnvivEXT #-}
ptr_glMultiTexEnvivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexEnvivEXT = unsafePerformIO $ getCommand "glMultiTexEnvivEXT"

-- glMultiTexGendEXT -----------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexGendvEXT'.
glMultiTexGendEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLdouble -- ^ @param@.
  -> m ()
glMultiTexGendEXT v1 v2 v3 v4 = liftIO $ dyn592 ptr_glMultiTexGendEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGendEXT #-}
ptr_glMultiTexGendEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLdouble -> IO ())
ptr_glMultiTexGendEXT = unsafePerformIO $ getCommand "glMultiTexGendEXT"

-- glMultiTexGendvEXT ----------------------------------------------------------

glMultiTexGendvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glMultiTexGendvEXT v1 v2 v3 v4 = liftIO $ dyn370 ptr_glMultiTexGendvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGendvEXT #-}
ptr_glMultiTexGendvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexGendvEXT = unsafePerformIO $ getCommand "glMultiTexGendvEXT"

-- glMultiTexGenfEXT -----------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexGenfvEXT'.
glMultiTexGenfEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glMultiTexGenfEXT v1 v2 v3 v4 = liftIO $ dyn590 ptr_glMultiTexGenfEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGenfEXT #-}
ptr_glMultiTexGenfEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLfloat -> IO ())
ptr_glMultiTexGenfEXT = unsafePerformIO $ getCommand "glMultiTexGenfEXT"

-- glMultiTexGenfvEXT ----------------------------------------------------------

glMultiTexGenfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glMultiTexGenfvEXT v1 v2 v3 v4 = liftIO $ dyn334 ptr_glMultiTexGenfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGenfvEXT #-}
ptr_glMultiTexGenfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexGenfvEXT = unsafePerformIO $ getCommand "glMultiTexGenfvEXT"

-- glMultiTexGeniEXT -----------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexGenivEXT'.
glMultiTexGeniEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glMultiTexGeniEXT v1 v2 v3 v4 = liftIO $ dyn591 ptr_glMultiTexGeniEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGeniEXT #-}
ptr_glMultiTexGeniEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLint -> IO ())
ptr_glMultiTexGeniEXT = unsafePerformIO $ getCommand "glMultiTexGeniEXT"

-- glMultiTexGenivEXT ----------------------------------------------------------

glMultiTexGenivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glMultiTexGenivEXT v1 v2 v3 v4 = liftIO $ dyn335 ptr_glMultiTexGenivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGenivEXT #-}
ptr_glMultiTexGenivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexGenivEXT = unsafePerformIO $ getCommand "glMultiTexGenivEXT"

-- glMultiTexImage1DEXT --------------------------------------------------------

glMultiTexImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn593 ptr_glMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glMultiTexImage1DEXT #-}
ptr_glMultiTexImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexImage1DEXT = unsafePerformIO $ getCommand "glMultiTexImage1DEXT"

-- glMultiTexImage2DEXT --------------------------------------------------------

glMultiTexImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn594 ptr_glMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glMultiTexImage2DEXT #-}
ptr_glMultiTexImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexImage2DEXT = unsafePerformIO $ getCommand "glMultiTexImage2DEXT"

-- glMultiTexImage3DEXT --------------------------------------------------------

glMultiTexImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glMultiTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn595 ptr_glMultiTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glMultiTexImage3DEXT #-}
ptr_glMultiTexImage3DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexImage3DEXT = unsafePerformIO $ getCommand "glMultiTexImage3DEXT"

-- glMultiTexParameterIivEXT ---------------------------------------------------

glMultiTexParameterIivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glMultiTexParameterIivEXT v1 v2 v3 v4 = liftIO $ dyn335 ptr_glMultiTexParameterIivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameterIivEXT #-}
ptr_glMultiTexParameterIivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexParameterIivEXT = unsafePerformIO $ getCommand "glMultiTexParameterIivEXT"

-- glMultiTexParameterIuivEXT --------------------------------------------------

glMultiTexParameterIuivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glMultiTexParameterIuivEXT v1 v2 v3 v4 = liftIO $ dyn374 ptr_glMultiTexParameterIuivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameterIuivEXT #-}
ptr_glMultiTexParameterIuivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glMultiTexParameterIuivEXT = unsafePerformIO $ getCommand "glMultiTexParameterIuivEXT"

-- glMultiTexParameterfEXT -----------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexParameterfvEXT'.
glMultiTexParameterfEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glMultiTexParameterfEXT v1 v2 v3 v4 = liftIO $ dyn590 ptr_glMultiTexParameterfEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameterfEXT #-}
ptr_glMultiTexParameterfEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLfloat -> IO ())
ptr_glMultiTexParameterfEXT = unsafePerformIO $ getCommand "glMultiTexParameterfEXT"

-- glMultiTexParameterfvEXT ----------------------------------------------------

glMultiTexParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glMultiTexParameterfvEXT v1 v2 v3 v4 = liftIO $ dyn334 ptr_glMultiTexParameterfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameterfvEXT #-}
ptr_glMultiTexParameterfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexParameterfvEXT = unsafePerformIO $ getCommand "glMultiTexParameterfvEXT"

-- glMultiTexParameteriEXT -----------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexParameterivEXT'.
glMultiTexParameteriEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glMultiTexParameteriEXT v1 v2 v3 v4 = liftIO $ dyn591 ptr_glMultiTexParameteriEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameteriEXT #-}
ptr_glMultiTexParameteriEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLint -> IO ())
ptr_glMultiTexParameteriEXT = unsafePerformIO $ getCommand "glMultiTexParameteriEXT"

-- glMultiTexParameterivEXT ----------------------------------------------------

glMultiTexParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glMultiTexParameterivEXT v1 v2 v3 v4 = liftIO $ dyn335 ptr_glMultiTexParameterivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameterivEXT #-}
ptr_glMultiTexParameterivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexParameterivEXT = unsafePerformIO $ getCommand "glMultiTexParameterivEXT"

-- glMultiTexRenderbufferEXT ---------------------------------------------------

glMultiTexRenderbufferEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glMultiTexRenderbufferEXT v1 v2 v3 = liftIO $ dyn32 ptr_glMultiTexRenderbufferEXT v1 v2 v3

{-# NOINLINE ptr_glMultiTexRenderbufferEXT #-}
ptr_glMultiTexRenderbufferEXT :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexRenderbufferEXT = unsafePerformIO $ getCommand "glMultiTexRenderbufferEXT"

-- glMultiTexSubImage1DEXT -----------------------------------------------------

glMultiTexSubImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn596 ptr_glMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glMultiTexSubImage1DEXT #-}
ptr_glMultiTexSubImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexSubImage1DEXT = unsafePerformIO $ getCommand "glMultiTexSubImage1DEXT"

-- glMultiTexSubImage2DEXT -----------------------------------------------------

glMultiTexSubImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn597 ptr_glMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glMultiTexSubImage2DEXT #-}
ptr_glMultiTexSubImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexSubImage2DEXT = unsafePerformIO $ getCommand "glMultiTexSubImage2DEXT"

-- glMultiTexSubImage3DEXT -----------------------------------------------------

glMultiTexSubImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn598 ptr_glMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glMultiTexSubImage3DEXT #-}
ptr_glMultiTexSubImage3DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexSubImage3DEXT = unsafePerformIO $ getCommand "glMultiTexSubImage3DEXT"

-- glMulticastBarrierNV --------------------------------------------------------

glMulticastBarrierNV
  :: MonadIO m
  => m ()
glMulticastBarrierNV = liftIO $ dyn11 ptr_glMulticastBarrierNV

{-# NOINLINE ptr_glMulticastBarrierNV #-}
ptr_glMulticastBarrierNV :: FunPtr (IO ())
ptr_glMulticastBarrierNV = unsafePerformIO $ getCommand "glMulticastBarrierNV"

-- glMulticastBlitFramebufferNV ------------------------------------------------

glMulticastBlitFramebufferNV
  :: MonadIO m
  => GLuint -- ^ @srcGpu@.
  -> GLuint -- ^ @dstGpu@.
  -> GLint -- ^ @srcX0@.
  -> GLint -- ^ @srcY0@.
  -> GLint -- ^ @srcX1@.
  -> GLint -- ^ @srcY1@.
  -> GLint -- ^ @dstX0@.
  -> GLint -- ^ @dstY0@.
  -> GLint -- ^ @dstX1@.
  -> GLint -- ^ @dstY1@.
  -> GLbitfield -- ^ @mask@ of type [ClearBufferMask](Graphics-GL-Groups.html#ClearBufferMask).
  -> GLenum -- ^ @filter@.
  -> m ()
glMulticastBlitFramebufferNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn60 ptr_glMulticastBlitFramebufferNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glMulticastBlitFramebufferNV #-}
ptr_glMulticastBlitFramebufferNV :: FunPtr (GLuint -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ())
ptr_glMulticastBlitFramebufferNV = unsafePerformIO $ getCommand "glMulticastBlitFramebufferNV"

-- glMulticastBufferSubDataNV --------------------------------------------------

glMulticastBufferSubDataNV
  :: MonadIO m
  => GLbitfield -- ^ @gpuMask@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@.
  -> Ptr a -- ^ @data@.
  -> m ()
glMulticastBufferSubDataNV v1 v2 v3 v4 v5 = liftIO $ dyn510 ptr_glMulticastBufferSubDataNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMulticastBufferSubDataNV #-}
ptr_glMulticastBufferSubDataNV :: FunPtr (GLbitfield -> GLuint -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glMulticastBufferSubDataNV = unsafePerformIO $ getCommand "glMulticastBufferSubDataNV"

-- glMulticastCopyBufferSubDataNV ----------------------------------------------

glMulticastCopyBufferSubDataNV
  :: MonadIO m
  => GLuint -- ^ @readGpu@.
  -> GLbitfield -- ^ @writeGpuMask@.
  -> GLuint -- ^ @readBuffer@.
  -> GLuint -- ^ @writeBuffer@.
  -> GLintptr -- ^ @readOffset@.
  -> GLintptr -- ^ @writeOffset@.
  -> GLsizeiptr -- ^ @size@.
  -> m ()
glMulticastCopyBufferSubDataNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn599 ptr_glMulticastCopyBufferSubDataNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glMulticastCopyBufferSubDataNV #-}
ptr_glMulticastCopyBufferSubDataNV :: FunPtr (GLuint -> GLbitfield -> GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
ptr_glMulticastCopyBufferSubDataNV = unsafePerformIO $ getCommand "glMulticastCopyBufferSubDataNV"

-- glMulticastCopyImageSubDataNV -----------------------------------------------

glMulticastCopyImageSubDataNV
  :: MonadIO m
  => GLuint -- ^ @srcGpu@.
  -> GLbitfield -- ^ @dstGpuMask@.
  -> GLuint -- ^ @srcName@.
  -> GLenum -- ^ @srcTarget@.
  -> GLint -- ^ @srcLevel@.
  -> GLint -- ^ @srcX@.
  -> GLint -- ^ @srcY@.
  -> GLint -- ^ @srcZ@.
  -> GLuint -- ^ @dstName@.
  -> GLenum -- ^ @dstTarget@.
  -> GLint -- ^ @dstLevel@.
  -> GLint -- ^ @dstX@.
  -> GLint -- ^ @dstY@.
  -> GLint -- ^ @dstZ@.
  -> GLsizei -- ^ @srcWidth@.
  -> GLsizei -- ^ @srcHeight@.
  -> GLsizei -- ^ @srcDepth@.
  -> m ()
glMulticastCopyImageSubDataNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 = liftIO $ dyn509 ptr_glMulticastCopyImageSubDataNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17

{-# NOINLINE ptr_glMulticastCopyImageSubDataNV #-}
ptr_glMulticastCopyImageSubDataNV :: FunPtr (GLuint -> GLbitfield -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glMulticastCopyImageSubDataNV = unsafePerformIO $ getCommand "glMulticastCopyImageSubDataNV"

-- glMulticastFramebufferSampleLocationsfvNV -----------------------------------

glMulticastFramebufferSampleLocationsfvNV
  :: MonadIO m
  => GLuint -- ^ @gpu@.
  -> GLuint -- ^ @framebuffer@.
  -> GLuint -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glMulticastFramebufferSampleLocationsfvNV v1 v2 v3 v4 v5 = liftIO $ dyn600 ptr_glMulticastFramebufferSampleLocationsfvNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMulticastFramebufferSampleLocationsfvNV #-}
ptr_glMulticastFramebufferSampleLocationsfvNV :: FunPtr (GLuint -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glMulticastFramebufferSampleLocationsfvNV = unsafePerformIO $ getCommand "glMulticastFramebufferSampleLocationsfvNV"

-- glMulticastGetQueryObjecti64vNV ---------------------------------------------

glMulticastGetQueryObjecti64vNV
  :: MonadIO m
  => GLuint -- ^ @gpu@.
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint64 -- ^ @params@.
  -> m ()
glMulticastGetQueryObjecti64vNV v1 v2 v3 v4 = liftIO $ dyn461 ptr_glMulticastGetQueryObjecti64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glMulticastGetQueryObjecti64vNV #-}
ptr_glMulticastGetQueryObjecti64vNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint64 -> IO ())
ptr_glMulticastGetQueryObjecti64vNV = unsafePerformIO $ getCommand "glMulticastGetQueryObjecti64vNV"

-- glMulticastGetQueryObjectivNV -----------------------------------------------

glMulticastGetQueryObjectivNV
  :: MonadIO m
  => GLuint -- ^ @gpu@.
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glMulticastGetQueryObjectivNV v1 v2 v3 v4 = liftIO $ dyn314 ptr_glMulticastGetQueryObjectivNV v1 v2 v3 v4

{-# NOINLINE ptr_glMulticastGetQueryObjectivNV #-}
ptr_glMulticastGetQueryObjectivNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glMulticastGetQueryObjectivNV = unsafePerformIO $ getCommand "glMulticastGetQueryObjectivNV"

-- glMulticastGetQueryObjectui64vNV --------------------------------------------

glMulticastGetQueryObjectui64vNV
  :: MonadIO m
  => GLuint -- ^ @gpu@.
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint64 -- ^ @params@.
  -> m ()
glMulticastGetQueryObjectui64vNV v1 v2 v3 v4 = liftIO $ dyn601 ptr_glMulticastGetQueryObjectui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glMulticastGetQueryObjectui64vNV #-}
ptr_glMulticastGetQueryObjectui64vNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLuint64 -> IO ())
ptr_glMulticastGetQueryObjectui64vNV = unsafePerformIO $ getCommand "glMulticastGetQueryObjectui64vNV"

-- glMulticastGetQueryObjectuivNV ----------------------------------------------

glMulticastGetQueryObjectuivNV
  :: MonadIO m
  => GLuint -- ^ @gpu@.
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @params@.
  -> m ()
glMulticastGetQueryObjectuivNV v1 v2 v3 v4 = liftIO $ dyn602 ptr_glMulticastGetQueryObjectuivNV v1 v2 v3 v4

{-# NOINLINE ptr_glMulticastGetQueryObjectuivNV #-}
ptr_glMulticastGetQueryObjectuivNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glMulticastGetQueryObjectuivNV = unsafePerformIO $ getCommand "glMulticastGetQueryObjectuivNV"

-- glMulticastScissorArrayvNVX -------------------------------------------------

glMulticastScissorArrayvNVX
  :: MonadIO m
  => GLuint -- ^ @gpu@.
  -> GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @v@ pointing to @COMPSIZE(count)@ elements of type @GLint@.
  -> m ()
glMulticastScissorArrayvNVX v1 v2 v3 v4 = liftIO $ dyn603 ptr_glMulticastScissorArrayvNVX v1 v2 v3 v4

{-# NOINLINE ptr_glMulticastScissorArrayvNVX #-}
ptr_glMulticastScissorArrayvNVX :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLint -> IO ())
ptr_glMulticastScissorArrayvNVX = unsafePerformIO $ getCommand "glMulticastScissorArrayvNVX"

-- glMulticastViewportArrayvNVX ------------------------------------------------

glMulticastViewportArrayvNVX
  :: MonadIO m
  => GLuint -- ^ @gpu@.
  -> GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @COMPSIZE(count)@ elements of type @GLfloat@.
  -> m ()
glMulticastViewportArrayvNVX v1 v2 v3 v4 = liftIO $ dyn604 ptr_glMulticastViewportArrayvNVX v1 v2 v3 v4

{-# NOINLINE ptr_glMulticastViewportArrayvNVX #-}
ptr_glMulticastViewportArrayvNVX :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glMulticastViewportArrayvNVX = unsafePerformIO $ getCommand "glMulticastViewportArrayvNVX"

-- glMulticastViewportPositionWScaleNVX ----------------------------------------

glMulticastViewportPositionWScaleNVX
  :: MonadIO m
  => GLuint -- ^ @gpu@.
  -> GLuint -- ^ @index@.
  -> GLfloat -- ^ @xcoeff@.
  -> GLfloat -- ^ @ycoeff@.
  -> m ()
glMulticastViewportPositionWScaleNVX v1 v2 v3 v4 = liftIO $ dyn605 ptr_glMulticastViewportPositionWScaleNVX v1 v2 v3 v4

{-# NOINLINE ptr_glMulticastViewportPositionWScaleNVX #-}
ptr_glMulticastViewportPositionWScaleNVX :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> IO ())
ptr_glMulticastViewportPositionWScaleNVX = unsafePerformIO $ getCommand "glMulticastViewportPositionWScaleNVX"

-- glMulticastWaitSyncNV -------------------------------------------------------

glMulticastWaitSyncNV
  :: MonadIO m
  => GLuint -- ^ @signalGpu@.
  -> GLbitfield -- ^ @waitGpuMask@.
  -> m ()
glMulticastWaitSyncNV v1 v2 = liftIO $ dyn606 ptr_glMulticastWaitSyncNV v1 v2

{-# NOINLINE ptr_glMulticastWaitSyncNV #-}
ptr_glMulticastWaitSyncNV :: FunPtr (GLuint -> GLbitfield -> IO ())
ptr_glMulticastWaitSyncNV = unsafePerformIO $ getCommand "glMulticastWaitSyncNV"

-- glNamedBufferAttachMemoryNV -------------------------------------------------

glNamedBufferAttachMemoryNV
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLuint -- ^ @memory@.
  -> GLuint64 -- ^ @offset@.
  -> m ()
glNamedBufferAttachMemoryNV v1 v2 v3 = liftIO $ dyn607 ptr_glNamedBufferAttachMemoryNV v1 v2 v3

{-# NOINLINE ptr_glNamedBufferAttachMemoryNV #-}
ptr_glNamedBufferAttachMemoryNV :: FunPtr (GLuint -> GLuint -> GLuint64 -> IO ())
ptr_glNamedBufferAttachMemoryNV = unsafePerformIO $ getCommand "glNamedBufferAttachMemoryNV"

-- glNamedBufferData -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBufferData.xhtml OpenGL 4.x>.
glNamedBufferData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@.
  -> GLenum -- ^ @usage@ of type [VertexBufferObjectUsage](Graphics-GL-Groups.html#VertexBufferObjectUsage).
  -> m ()
glNamedBufferData v1 v2 v3 v4 = liftIO $ dyn608 ptr_glNamedBufferData v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferData #-}
ptr_glNamedBufferData :: FunPtr (GLuint -> GLsizeiptr -> Ptr a -> GLenum -> IO ())
ptr_glNamedBufferData = unsafePerformIO $ getCommand "glNamedBufferData"

-- glNamedBufferDataEXT --------------------------------------------------------

glNamedBufferDataEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLsizeiptr -- ^ @size@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(size)@ elements of type @a@.
  -> GLenum -- ^ @usage@ of type [VertexBufferObjectUsage](Graphics-GL-Groups.html#VertexBufferObjectUsage).
  -> m ()
glNamedBufferDataEXT v1 v2 v3 v4 = liftIO $ dyn608 ptr_glNamedBufferDataEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferDataEXT #-}
ptr_glNamedBufferDataEXT :: FunPtr (GLuint -> GLsizeiptr -> Ptr a -> GLenum -> IO ())
ptr_glNamedBufferDataEXT = unsafePerformIO $ getCommand "glNamedBufferDataEXT"

-- glNamedBufferPageCommitmentARB ----------------------------------------------

glNamedBufferPageCommitmentARB
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@.
  -> GLboolean -- ^ @commit@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glNamedBufferPageCommitmentARB v1 v2 v3 v4 = liftIO $ dyn609 ptr_glNamedBufferPageCommitmentARB v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferPageCommitmentARB #-}
ptr_glNamedBufferPageCommitmentARB :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> IO ())
ptr_glNamedBufferPageCommitmentARB = unsafePerformIO $ getCommand "glNamedBufferPageCommitmentARB"

-- glNamedBufferPageCommitmentEXT ----------------------------------------------

glNamedBufferPageCommitmentEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@.
  -> GLboolean -- ^ @commit@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glNamedBufferPageCommitmentEXT v1 v2 v3 v4 = liftIO $ dyn609 ptr_glNamedBufferPageCommitmentEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferPageCommitmentEXT #-}
ptr_glNamedBufferPageCommitmentEXT :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> IO ())
ptr_glNamedBufferPageCommitmentEXT = unsafePerformIO $ getCommand "glNamedBufferPageCommitmentEXT"

-- glNamedBufferStorage --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBufferStorage.xhtml OpenGL 4.x>.
glNamedBufferStorage
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> GLbitfield -- ^ @flags@ of type [BufferStorageMask](Graphics-GL-Groups.html#BufferStorageMask).
  -> m ()
glNamedBufferStorage v1 v2 v3 v4 = liftIO $ dyn610 ptr_glNamedBufferStorage v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferStorage #-}
ptr_glNamedBufferStorage :: FunPtr (GLuint -> GLsizeiptr -> Ptr a -> GLbitfield -> IO ())
ptr_glNamedBufferStorage = unsafePerformIO $ getCommand "glNamedBufferStorage"

-- glNamedBufferStorageEXT -----------------------------------------------------

-- | This command is an alias for 'glNamedBufferStorage'.
glNamedBufferStorageEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> GLbitfield -- ^ @flags@ of type [BufferStorageMask](Graphics-GL-Groups.html#BufferStorageMask).
  -> m ()
glNamedBufferStorageEXT v1 v2 v3 v4 = liftIO $ dyn610 ptr_glNamedBufferStorageEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferStorageEXT #-}
ptr_glNamedBufferStorageEXT :: FunPtr (GLuint -> GLsizeiptr -> Ptr a -> GLbitfield -> IO ())
ptr_glNamedBufferStorageEXT = unsafePerformIO $ getCommand "glNamedBufferStorageEXT"

-- glNamedBufferStorageExternalEXT ---------------------------------------------

glNamedBufferStorageExternalEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@.
  -> GLeglClientBufferEXT -- ^ @clientBuffer@.
  -> GLbitfield -- ^ @flags@ of type [BufferStorageMask](Graphics-GL-Groups.html#BufferStorageMask).
  -> m ()
glNamedBufferStorageExternalEXT v1 v2 v3 v4 v5 = liftIO $ dyn611 ptr_glNamedBufferStorageExternalEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedBufferStorageExternalEXT #-}
ptr_glNamedBufferStorageExternalEXT :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLeglClientBufferEXT -> GLbitfield -> IO ())
ptr_glNamedBufferStorageExternalEXT = unsafePerformIO $ getCommand "glNamedBufferStorageExternalEXT"

-- glNamedBufferStorageMemEXT --------------------------------------------------

glNamedBufferStorageMemEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> GLuint -- ^ @memory@.
  -> GLuint64 -- ^ @offset@.
  -> m ()
glNamedBufferStorageMemEXT v1 v2 v3 v4 = liftIO $ dyn612 ptr_glNamedBufferStorageMemEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferStorageMemEXT #-}
ptr_glNamedBufferStorageMemEXT :: FunPtr (GLuint -> GLsizeiptr -> GLuint -> GLuint64 -> IO ())
ptr_glNamedBufferStorageMemEXT = unsafePerformIO $ getCommand "glNamedBufferStorageMemEXT"

-- glNamedBufferSubData --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBufferSubData.xhtml OpenGL 4.x>.
glNamedBufferSubData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(size)@ elements of type @a@.
  -> m ()
glNamedBufferSubData v1 v2 v3 v4 = liftIO $ dyn378 ptr_glNamedBufferSubData v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferSubData #-}
ptr_glNamedBufferSubData :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glNamedBufferSubData = unsafePerformIO $ getCommand "glNamedBufferSubData"

-- glNamedBufferSubDataEXT -----------------------------------------------------

-- | This command is an alias for 'glNamedBufferSubData'.
glNamedBufferSubDataEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(size)@ elements of type @a@.
  -> m ()
glNamedBufferSubDataEXT v1 v2 v3 v4 = liftIO $ dyn378 ptr_glNamedBufferSubDataEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferSubDataEXT #-}
ptr_glNamedBufferSubDataEXT :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glNamedBufferSubDataEXT = unsafePerformIO $ getCommand "glNamedBufferSubDataEXT"

-- glNamedCopyBufferSubDataEXT -------------------------------------------------

glNamedCopyBufferSubDataEXT
  :: MonadIO m
  => GLuint -- ^ @readBuffer@.
  -> GLuint -- ^ @writeBuffer@.
  -> GLintptr -- ^ @readOffset@.
  -> GLintptr -- ^ @writeOffset@.
  -> GLsizeiptr -- ^ @size@.
  -> m ()
glNamedCopyBufferSubDataEXT v1 v2 v3 v4 v5 = liftIO $ dyn181 ptr_glNamedCopyBufferSubDataEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedCopyBufferSubDataEXT #-}
ptr_glNamedCopyBufferSubDataEXT :: FunPtr (GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
ptr_glNamedCopyBufferSubDataEXT = unsafePerformIO $ getCommand "glNamedCopyBufferSubDataEXT"

-- glNamedFramebufferDrawBuffer ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawBuffer.xhtml OpenGL 4.x>.
glNamedFramebufferDrawBuffer
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @buf@ of type [ColorBuffer](Graphics-GL-Groups.html#ColorBuffer).
  -> m ()
glNamedFramebufferDrawBuffer v1 v2 = liftIO $ dyn18 ptr_glNamedFramebufferDrawBuffer v1 v2

{-# NOINLINE ptr_glNamedFramebufferDrawBuffer #-}
ptr_glNamedFramebufferDrawBuffer :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glNamedFramebufferDrawBuffer = unsafePerformIO $ getCommand "glNamedFramebufferDrawBuffer"

-- glNamedFramebufferDrawBuffers -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawBuffers.xhtml OpenGL 4.x>.
glNamedFramebufferDrawBuffers
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to elements of type [ColorBuffer](Graphics-GL-Groups.html#ColorBuffer).
  -> m ()
glNamedFramebufferDrawBuffers v1 v2 v3 = liftIO $ dyn293 ptr_glNamedFramebufferDrawBuffers v1 v2 v3

{-# NOINLINE ptr_glNamedFramebufferDrawBuffers #-}
ptr_glNamedFramebufferDrawBuffers :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> IO ())
ptr_glNamedFramebufferDrawBuffers = unsafePerformIO $ getCommand "glNamedFramebufferDrawBuffers"

-- glNamedFramebufferParameteri ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFramebufferParameteri.xhtml OpenGL 4.x>.
glNamedFramebufferParameteri
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @pname@ of type [FramebufferParameterName](Graphics-GL-Groups.html#FramebufferParameterName).
  -> GLint -- ^ @param@.
  -> m ()
glNamedFramebufferParameteri v1 v2 v3 = liftIO $ dyn491 ptr_glNamedFramebufferParameteri v1 v2 v3

{-# NOINLINE ptr_glNamedFramebufferParameteri #-}
ptr_glNamedFramebufferParameteri :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glNamedFramebufferParameteri = unsafePerformIO $ getCommand "glNamedFramebufferParameteri"

-- glNamedFramebufferParameteriEXT ---------------------------------------------

glNamedFramebufferParameteriEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @pname@ of type [FramebufferParameterName](Graphics-GL-Groups.html#FramebufferParameterName).
  -> GLint -- ^ @param@.
  -> m ()
glNamedFramebufferParameteriEXT v1 v2 v3 = liftIO $ dyn491 ptr_glNamedFramebufferParameteriEXT v1 v2 v3

{-# NOINLINE ptr_glNamedFramebufferParameteriEXT #-}
ptr_glNamedFramebufferParameteriEXT :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glNamedFramebufferParameteriEXT = unsafePerformIO $ getCommand "glNamedFramebufferParameteriEXT"

-- glNamedFramebufferReadBuffer ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glReadBuffer.xhtml OpenGL 4.x>.
glNamedFramebufferReadBuffer
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @src@ of type [ColorBuffer](Graphics-GL-Groups.html#ColorBuffer).
  -> m ()
glNamedFramebufferReadBuffer v1 v2 = liftIO $ dyn18 ptr_glNamedFramebufferReadBuffer v1 v2

{-# NOINLINE ptr_glNamedFramebufferReadBuffer #-}
ptr_glNamedFramebufferReadBuffer :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glNamedFramebufferReadBuffer = unsafePerformIO $ getCommand "glNamedFramebufferReadBuffer"

-- glNamedFramebufferRenderbuffer ----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFramebufferRenderbuffer.xhtml OpenGL 4.x>.
glNamedFramebufferRenderbuffer
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @renderbuffertarget@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glNamedFramebufferRenderbuffer v1 v2 v3 v4 = liftIO $ dyn613 ptr_glNamedFramebufferRenderbuffer v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferRenderbuffer #-}
ptr_glNamedFramebufferRenderbuffer :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glNamedFramebufferRenderbuffer = unsafePerformIO $ getCommand "glNamedFramebufferRenderbuffer"

-- glNamedFramebufferRenderbufferEXT -------------------------------------------

glNamedFramebufferRenderbufferEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @renderbuffertarget@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLuint -- ^ @renderbuffer@ of type @Renderbuffer@.
  -> m ()
glNamedFramebufferRenderbufferEXT v1 v2 v3 v4 = liftIO $ dyn613 ptr_glNamedFramebufferRenderbufferEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferRenderbufferEXT #-}
ptr_glNamedFramebufferRenderbufferEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glNamedFramebufferRenderbufferEXT = unsafePerformIO $ getCommand "glNamedFramebufferRenderbufferEXT"

-- glNamedFramebufferSampleLocationsfvARB --------------------------------------

glNamedFramebufferSampleLocationsfvARB
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLuint -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glNamedFramebufferSampleLocationsfvARB v1 v2 v3 v4 = liftIO $ dyn604 ptr_glNamedFramebufferSampleLocationsfvARB v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferSampleLocationsfvARB #-}
ptr_glNamedFramebufferSampleLocationsfvARB :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glNamedFramebufferSampleLocationsfvARB = unsafePerformIO $ getCommand "glNamedFramebufferSampleLocationsfvARB"

-- glNamedFramebufferSampleLocationsfvNV ---------------------------------------

glNamedFramebufferSampleLocationsfvNV
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLuint -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glNamedFramebufferSampleLocationsfvNV v1 v2 v3 v4 = liftIO $ dyn604 ptr_glNamedFramebufferSampleLocationsfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferSampleLocationsfvNV #-}
ptr_glNamedFramebufferSampleLocationsfvNV :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glNamedFramebufferSampleLocationsfvNV = unsafePerformIO $ getCommand "glNamedFramebufferSampleLocationsfvNV"

-- glNamedFramebufferSamplePositionsfvAMD --------------------------------------

glNamedFramebufferSamplePositionsfvAMD
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLuint -- ^ @numsamples@.
  -> GLuint -- ^ @pixelindex@.
  -> Ptr GLfloat -- ^ @values@.
  -> m ()
glNamedFramebufferSamplePositionsfvAMD v1 v2 v3 v4 = liftIO $ dyn614 ptr_glNamedFramebufferSamplePositionsfvAMD v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferSamplePositionsfvAMD #-}
ptr_glNamedFramebufferSamplePositionsfvAMD :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLfloat -> IO ())
ptr_glNamedFramebufferSamplePositionsfvAMD = unsafePerformIO $ getCommand "glNamedFramebufferSamplePositionsfvAMD"

-- glNamedFramebufferTexture ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml OpenGL 4.x>.
glNamedFramebufferTexture
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glNamedFramebufferTexture v1 v2 v3 v4 = liftIO $ dyn615 ptr_glNamedFramebufferTexture v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferTexture #-}
ptr_glNamedFramebufferTexture :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> IO ())
ptr_glNamedFramebufferTexture = unsafePerformIO $ getCommand "glNamedFramebufferTexture"

-- glNamedFramebufferTexture1DEXT ----------------------------------------------

glNamedFramebufferTexture1DEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glNamedFramebufferTexture1DEXT v1 v2 v3 v4 v5 = liftIO $ dyn616 ptr_glNamedFramebufferTexture1DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedFramebufferTexture1DEXT #-}
ptr_glNamedFramebufferTexture1DEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glNamedFramebufferTexture1DEXT = unsafePerformIO $ getCommand "glNamedFramebufferTexture1DEXT"

-- glNamedFramebufferTexture2DEXT ----------------------------------------------

glNamedFramebufferTexture2DEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glNamedFramebufferTexture2DEXT v1 v2 v3 v4 v5 = liftIO $ dyn616 ptr_glNamedFramebufferTexture2DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedFramebufferTexture2DEXT #-}
ptr_glNamedFramebufferTexture2DEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glNamedFramebufferTexture2DEXT = unsafePerformIO $ getCommand "glNamedFramebufferTexture2DEXT"

-- glNamedFramebufferTexture3DEXT ----------------------------------------------

glNamedFramebufferTexture3DEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> m ()
glNamedFramebufferTexture3DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn617 ptr_glNamedFramebufferTexture3DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glNamedFramebufferTexture3DEXT #-}
ptr_glNamedFramebufferTexture3DEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glNamedFramebufferTexture3DEXT = unsafePerformIO $ getCommand "glNamedFramebufferTexture3DEXT"

-- glNamedFramebufferTextureEXT ------------------------------------------------

glNamedFramebufferTextureEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glNamedFramebufferTextureEXT v1 v2 v3 v4 = liftIO $ dyn615 ptr_glNamedFramebufferTextureEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferTextureEXT #-}
ptr_glNamedFramebufferTextureEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> IO ())
ptr_glNamedFramebufferTextureEXT = unsafePerformIO $ getCommand "glNamedFramebufferTextureEXT"

-- glNamedFramebufferTextureFaceEXT --------------------------------------------

glNamedFramebufferTextureFaceEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @face@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glNamedFramebufferTextureFaceEXT v1 v2 v3 v4 v5 = liftIO $ dyn618 ptr_glNamedFramebufferTextureFaceEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedFramebufferTextureFaceEXT #-}
ptr_glNamedFramebufferTextureFaceEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLenum -> IO ())
ptr_glNamedFramebufferTextureFaceEXT = unsafePerformIO $ getCommand "glNamedFramebufferTextureFaceEXT"

-- glNamedFramebufferTextureLayer ----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTextureLayer.xhtml OpenGL 4.x>.
glNamedFramebufferTextureLayer
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @layer@.
  -> m ()
glNamedFramebufferTextureLayer v1 v2 v3 v4 v5 = liftIO $ dyn619 ptr_glNamedFramebufferTextureLayer v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedFramebufferTextureLayer #-}
ptr_glNamedFramebufferTextureLayer :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glNamedFramebufferTextureLayer = unsafePerformIO $ getCommand "glNamedFramebufferTextureLayer"

-- glNamedFramebufferTextureLayerEXT -------------------------------------------

glNamedFramebufferTextureLayerEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @layer@ of type @CheckedInt32@.
  -> m ()
glNamedFramebufferTextureLayerEXT v1 v2 v3 v4 v5 = liftIO $ dyn619 ptr_glNamedFramebufferTextureLayerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedFramebufferTextureLayerEXT #-}
ptr_glNamedFramebufferTextureLayerEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glNamedFramebufferTextureLayerEXT = unsafePerformIO $ getCommand "glNamedFramebufferTextureLayerEXT"

