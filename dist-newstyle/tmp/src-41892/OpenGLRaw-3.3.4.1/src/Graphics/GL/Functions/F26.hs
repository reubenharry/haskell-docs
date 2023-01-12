{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F26
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

module Graphics.GL.Functions.F26 (
  glTexCoord4fVertex4fvSUN,
  glTexCoord4fv,
  glTexCoord4hNV,
  glTexCoord4hvNV,
  glTexCoord4i,
  glTexCoord4iv,
  glTexCoord4s,
  glTexCoord4sv,
  glTexCoord4xOES,
  glTexCoord4xvOES,
  glTexCoordFormatNV,
  glTexCoordP1ui,
  glTexCoordP1uiv,
  glTexCoordP2ui,
  glTexCoordP2uiv,
  glTexCoordP3ui,
  glTexCoordP3uiv,
  glTexCoordP4ui,
  glTexCoordP4uiv,
  glTexCoordPointer,
  glTexCoordPointerEXT,
  glTexCoordPointerListIBM,
  glTexCoordPointervINTEL,
  glTexEnvf,
  glTexEnvfv,
  glTexEnvi,
  glTexEnviv,
  glTexEnvx,
  glTexEnvxOES,
  glTexEnvxv,
  glTexEnvxvOES,
  glTexFilterFuncSGIS,
  glTexGend,
  glTexGendv,
  glTexGenf,
  glTexGenfOES,
  glTexGenfv,
  glTexGenfvOES,
  glTexGeni,
  glTexGeniOES,
  glTexGeniv,
  glTexGenivOES,
  glTexGenxOES,
  glTexGenxvOES,
  glTexImage1D,
  glTexImage2D,
  glTexImage2DMultisample,
  glTexImage2DMultisampleCoverageNV,
  glTexImage3D,
  glTexImage3DEXT,
  glTexImage3DMultisample,
  glTexImage3DMultisampleCoverageNV,
  glTexImage3DOES,
  glTexImage4DSGIS,
  glTexPageCommitmentARB,
  glTexPageCommitmentEXT,
  glTexParameterIiv,
  glTexParameterIivEXT,
  glTexParameterIivOES,
  glTexParameterIuiv,
  glTexParameterIuivEXT,
  glTexParameterIuivOES,
  glTexParameterf,
  glTexParameterfv,
  glTexParameteri,
  glTexParameteriv,
  glTexParameterx,
  glTexParameterxOES,
  glTexParameterxv,
  glTexParameterxvOES,
  glTexRenderbufferNV,
  glTexStorage1D,
  glTexStorage1DEXT,
  glTexStorage2D,
  glTexStorage2DEXT,
  glTexStorage2DMultisample,
  glTexStorage3D,
  glTexStorage3DEXT,
  glTexStorage3DMultisample,
  glTexStorage3DMultisampleOES,
  glTexStorageMem1DEXT,
  glTexStorageMem2DEXT,
  glTexStorageMem2DMultisampleEXT,
  glTexStorageMem3DEXT,
  glTexStorageMem3DMultisampleEXT,
  glTexStorageSparseAMD,
  glTexSubImage1D,
  glTexSubImage1DEXT,
  glTexSubImage2D,
  glTexSubImage2DEXT,
  glTexSubImage3D,
  glTexSubImage3DEXT,
  glTexSubImage3DOES,
  glTexSubImage4DSGIS,
  glTextureAttachMemoryNV,
  glTextureBarrier,
  glTextureBarrierNV,
  glTextureBuffer,
  glTextureBufferEXT,
  glTextureBufferRange
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glTexCoord4fVertex4fvSUN ----------------------------------------------------

glTexCoord4fVertex4fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @tc@ pointing to @4@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glTexCoord4fVertex4fvSUN v1 v2 = liftIO $ dyn104 ptr_glTexCoord4fVertex4fvSUN v1 v2

{-# NOINLINE ptr_glTexCoord4fVertex4fvSUN #-}
ptr_glTexCoord4fVertex4fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glTexCoord4fVertex4fvSUN = unsafePerformIO $ getCommand "glTexCoord4fVertex4fvSUN"

-- glTexCoord4fv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord4fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glTexCoord4fv v1 = liftIO $ dyn44 ptr_glTexCoord4fv v1

{-# NOINLINE ptr_glTexCoord4fv #-}
ptr_glTexCoord4fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glTexCoord4fv = unsafePerformIO $ getCommand "glTexCoord4fv"

-- glTexCoord4hNV --------------------------------------------------------------

-- | The vector equivalent of this command is 'glTexCoord4hvNV'.
glTexCoord4hNV
  :: MonadIO m
  => GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> GLhalfNV -- ^ @t@ of type @Half16NV@.
  -> GLhalfNV -- ^ @r@ of type @Half16NV@.
  -> GLhalfNV -- ^ @q@ of type @Half16NV@.
  -> m ()
glTexCoord4hNV v1 v2 v3 v4 = liftIO $ dyn119 ptr_glTexCoord4hNV v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4hNV #-}
ptr_glTexCoord4hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glTexCoord4hNV = unsafePerformIO $ getCommand "glTexCoord4hNV"

-- glTexCoord4hvNV -------------------------------------------------------------

glTexCoord4hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @4@ elements of type @Half16NV@.
  -> m ()
glTexCoord4hvNV v1 = liftIO $ dyn106 ptr_glTexCoord4hvNV v1

{-# NOINLINE ptr_glTexCoord4hvNV #-}
ptr_glTexCoord4hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glTexCoord4hvNV = unsafePerformIO $ getCommand "glTexCoord4hvNV"

-- glTexCoord4i ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord4iv'.
glTexCoord4i
  :: MonadIO m
  => GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> GLint -- ^ @r@ of type @CoordI@.
  -> GLint -- ^ @q@ of type @CoordI@.
  -> m ()
glTexCoord4i v1 v2 v3 v4 = liftIO $ dyn82 ptr_glTexCoord4i v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4i #-}
ptr_glTexCoord4i :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glTexCoord4i = unsafePerformIO $ getCommand "glTexCoord4i"

-- glTexCoord4iv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord4iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glTexCoord4iv v1 = liftIO $ dyn46 ptr_glTexCoord4iv v1

{-# NOINLINE ptr_glTexCoord4iv #-}
ptr_glTexCoord4iv :: FunPtr (Ptr GLint -> IO ())
ptr_glTexCoord4iv = unsafePerformIO $ getCommand "glTexCoord4iv"

-- glTexCoord4s ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord4sv'.
glTexCoord4s
  :: MonadIO m
  => GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> GLshort -- ^ @r@ of type @CoordS@.
  -> GLshort -- ^ @q@ of type @CoordS@.
  -> m ()
glTexCoord4s v1 v2 v3 v4 = liftIO $ dyn120 ptr_glTexCoord4s v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4s #-}
ptr_glTexCoord4s :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glTexCoord4s = unsafePerformIO $ getCommand "glTexCoord4s"

-- glTexCoord4sv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord4sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glTexCoord4sv v1 = liftIO $ dyn48 ptr_glTexCoord4sv v1

{-# NOINLINE ptr_glTexCoord4sv #-}
ptr_glTexCoord4sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glTexCoord4sv = unsafePerformIO $ getCommand "glTexCoord4sv"

-- glTexCoord4xOES -------------------------------------------------------------

glTexCoord4xOES
  :: MonadIO m
  => GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @q@.
  -> m ()
glTexCoord4xOES v1 v2 v3 v4 = liftIO $ dyn53 ptr_glTexCoord4xOES v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4xOES #-}
ptr_glTexCoord4xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glTexCoord4xOES = unsafePerformIO $ getCommand "glTexCoord4xOES"

-- glTexCoord4xvOES ------------------------------------------------------------

glTexCoord4xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glTexCoord4xvOES v1 = liftIO $ dyn114 ptr_glTexCoord4xvOES v1

{-# NOINLINE ptr_glTexCoord4xvOES #-}
ptr_glTexCoord4xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glTexCoord4xvOES = unsafePerformIO $ getCommand "glTexCoord4xvOES"

-- glTexCoordFormatNV ----------------------------------------------------------

glTexCoordFormatNV
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glTexCoordFormatNV v1 v2 v3 = liftIO $ dyn126 ptr_glTexCoordFormatNV v1 v2 v3

{-# NOINLINE ptr_glTexCoordFormatNV #-}
ptr_glTexCoordFormatNV :: FunPtr (GLint -> GLenum -> GLsizei -> IO ())
ptr_glTexCoordFormatNV = unsafePerformIO $ getCommand "glTexCoordFormatNV"

-- glTexCoordP1ui --------------------------------------------------------------

glTexCoordP1ui
  :: MonadIO m
  => GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLuint -- ^ @coords@.
  -> m ()
glTexCoordP1ui v1 v2 = liftIO $ dyn19 ptr_glTexCoordP1ui v1 v2

{-# NOINLINE ptr_glTexCoordP1ui #-}
ptr_glTexCoordP1ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glTexCoordP1ui = unsafePerformIO $ getCommand "glTexCoordP1ui"

-- glTexCoordP1uiv -------------------------------------------------------------

glTexCoordP1uiv
  :: MonadIO m
  => GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glTexCoordP1uiv v1 v2 = liftIO $ dyn132 ptr_glTexCoordP1uiv v1 v2

{-# NOINLINE ptr_glTexCoordP1uiv #-}
ptr_glTexCoordP1uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glTexCoordP1uiv = unsafePerformIO $ getCommand "glTexCoordP1uiv"

-- glTexCoordP2ui --------------------------------------------------------------

glTexCoordP2ui
  :: MonadIO m
  => GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLuint -- ^ @coords@.
  -> m ()
glTexCoordP2ui v1 v2 = liftIO $ dyn19 ptr_glTexCoordP2ui v1 v2

{-# NOINLINE ptr_glTexCoordP2ui #-}
ptr_glTexCoordP2ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glTexCoordP2ui = unsafePerformIO $ getCommand "glTexCoordP2ui"

-- glTexCoordP2uiv -------------------------------------------------------------

glTexCoordP2uiv
  :: MonadIO m
  => GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glTexCoordP2uiv v1 v2 = liftIO $ dyn132 ptr_glTexCoordP2uiv v1 v2

{-# NOINLINE ptr_glTexCoordP2uiv #-}
ptr_glTexCoordP2uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glTexCoordP2uiv = unsafePerformIO $ getCommand "glTexCoordP2uiv"

-- glTexCoordP3ui --------------------------------------------------------------

glTexCoordP3ui
  :: MonadIO m
  => GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLuint -- ^ @coords@.
  -> m ()
glTexCoordP3ui v1 v2 = liftIO $ dyn19 ptr_glTexCoordP3ui v1 v2

{-# NOINLINE ptr_glTexCoordP3ui #-}
ptr_glTexCoordP3ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glTexCoordP3ui = unsafePerformIO $ getCommand "glTexCoordP3ui"

-- glTexCoordP3uiv -------------------------------------------------------------

glTexCoordP3uiv
  :: MonadIO m
  => GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glTexCoordP3uiv v1 v2 = liftIO $ dyn132 ptr_glTexCoordP3uiv v1 v2

{-# NOINLINE ptr_glTexCoordP3uiv #-}
ptr_glTexCoordP3uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glTexCoordP3uiv = unsafePerformIO $ getCommand "glTexCoordP3uiv"

-- glTexCoordP4ui --------------------------------------------------------------

glTexCoordP4ui
  :: MonadIO m
  => GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLuint -- ^ @coords@.
  -> m ()
glTexCoordP4ui v1 v2 = liftIO $ dyn19 ptr_glTexCoordP4ui v1 v2

{-# NOINLINE ptr_glTexCoordP4ui #-}
ptr_glTexCoordP4ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glTexCoordP4ui = unsafePerformIO $ getCommand "glTexCoordP4ui"

-- glTexCoordP4uiv -------------------------------------------------------------

glTexCoordP4uiv
  :: MonadIO m
  => GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glTexCoordP4uiv v1 v2 = liftIO $ dyn132 ptr_glTexCoordP4uiv v1 v2

{-# NOINLINE ptr_glTexCoordP4uiv #-}
ptr_glTexCoordP4uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glTexCoordP4uiv = unsafePerformIO $ getCommand "glTexCoordP4uiv"

-- glTexCoordPointer -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoordPointer.xml OpenGL 2.x>.
glTexCoordPointer
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glTexCoordPointer v1 v2 v3 v4 = liftIO $ dyn133 ptr_glTexCoordPointer v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoordPointer #-}
ptr_glTexCoordPointer :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glTexCoordPointer = unsafePerformIO $ getCommand "glTexCoordPointer"

-- glTexCoordPointerEXT --------------------------------------------------------

glTexCoordPointerEXT
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLsizei -- ^ @count@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride,count)@ elements of type @a@.
  -> m ()
glTexCoordPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn134 ptr_glTexCoordPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexCoordPointerEXT #-}
ptr_glTexCoordPointerEXT :: FunPtr (GLint -> GLenum -> GLsizei -> GLsizei -> Ptr a -> IO ())
ptr_glTexCoordPointerEXT = unsafePerformIO $ getCommand "glTexCoordPointerEXT"

-- glTexCoordPointerListIBM ----------------------------------------------------

glTexCoordPointerListIBM
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glTexCoordPointerListIBM v1 v2 v3 v4 v5 = liftIO $ dyn135 ptr_glTexCoordPointerListIBM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexCoordPointerListIBM #-}
ptr_glTexCoordPointerListIBM :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glTexCoordPointerListIBM = unsafePerformIO $ getCommand "glTexCoordPointerListIBM"

-- glTexCoordPointervINTEL -----------------------------------------------------

glTexCoordPointervINTEL
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @4@ elements of type @Ptr a@.
  -> m ()
glTexCoordPointervINTEL v1 v2 v3 = liftIO $ dyn136 ptr_glTexCoordPointervINTEL v1 v2 v3

{-# NOINLINE ptr_glTexCoordPointervINTEL #-}
ptr_glTexCoordPointervINTEL :: FunPtr (GLint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glTexCoordPointervINTEL = unsafePerformIO $ getCommand "glTexCoordPointervINTEL"

-- glTexEnvf -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml OpenGL 2.x>.
glTexEnvf
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glTexEnvf v1 v2 v3 = liftIO $ dyn168 ptr_glTexEnvf v1 v2 v3

{-# NOINLINE ptr_glTexEnvf #-}
ptr_glTexEnvf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glTexEnvf = unsafePerformIO $ getCommand "glTexEnvf"

-- glTexEnvfv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml OpenGL 2.x>.
glTexEnvfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glTexEnvfv v1 v2 v3 = liftIO $ dyn139 ptr_glTexEnvfv v1 v2 v3

{-# NOINLINE ptr_glTexEnvfv #-}
ptr_glTexEnvfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glTexEnvfv = unsafePerformIO $ getCommand "glTexEnvfv"

-- glTexEnvi -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml OpenGL 2.x>.
glTexEnvi
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glTexEnvi v1 v2 v3 = liftIO $ dyn66 ptr_glTexEnvi v1 v2 v3

{-# NOINLINE ptr_glTexEnvi #-}
ptr_glTexEnvi :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glTexEnvi = unsafePerformIO $ getCommand "glTexEnvi"

-- glTexEnviv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml OpenGL 2.x>.
glTexEnviv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glTexEnviv v1 v2 v3 = liftIO $ dyn140 ptr_glTexEnviv v1 v2 v3

{-# NOINLINE ptr_glTexEnviv #-}
ptr_glTexEnviv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexEnviv = unsafePerformIO $ getCommand "glTexEnviv"

-- glTexEnvx -------------------------------------------------------------------

glTexEnvx
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glTexEnvx v1 v2 v3 = liftIO $ dyn169 ptr_glTexEnvx v1 v2 v3

{-# NOINLINE ptr_glTexEnvx #-}
ptr_glTexEnvx :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glTexEnvx = unsafePerformIO $ getCommand "glTexEnvx"

-- glTexEnvxOES ----------------------------------------------------------------

glTexEnvxOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glTexEnvxOES v1 v2 v3 = liftIO $ dyn169 ptr_glTexEnvxOES v1 v2 v3

{-# NOINLINE ptr_glTexEnvxOES #-}
ptr_glTexEnvxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glTexEnvxOES = unsafePerformIO $ getCommand "glTexEnvxOES"

-- glTexEnvxv ------------------------------------------------------------------

glTexEnvxv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glTexEnvxv v1 v2 v3 = liftIO $ dyn170 ptr_glTexEnvxv v1 v2 v3

{-# NOINLINE ptr_glTexEnvxv #-}
ptr_glTexEnvxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glTexEnvxv = unsafePerformIO $ getCommand "glTexEnvxv"

-- glTexEnvxvOES ---------------------------------------------------------------

glTexEnvxvOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glTexEnvxvOES v1 v2 v3 = liftIO $ dyn170 ptr_glTexEnvxvOES v1 v2 v3

{-# NOINLINE ptr_glTexEnvxvOES #-}
ptr_glTexEnvxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glTexEnvxvOES = unsafePerformIO $ getCommand "glTexEnvxvOES"

-- glTexFilterFuncSGIS ---------------------------------------------------------

glTexFilterFuncSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @filter@ of type [TextureFilterSGIS](Graphics-GL-Groups.html#TextureFilterSGIS).
  -> GLsizei -- ^ @n@.
  -> Ptr GLfloat -- ^ @weights@ pointing to @n@ elements of type @GLfloat@.
  -> m ()
glTexFilterFuncSGIS v1 v2 v3 v4 = liftIO $ dyn472 ptr_glTexFilterFuncSGIS v1 v2 v3 v4

{-# NOINLINE ptr_glTexFilterFuncSGIS #-}
ptr_glTexFilterFuncSGIS :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glTexFilterFuncSGIS = unsafePerformIO $ getCommand "glTexFilterFuncSGIS"

-- glTexGend -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGend
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLdouble -- ^ @param@.
  -> m ()
glTexGend v1 v2 v3 = liftIO $ dyn772 ptr_glTexGend v1 v2 v3

{-# NOINLINE ptr_glTexGend #-}
ptr_glTexGend :: FunPtr (GLenum -> GLenum -> GLdouble -> IO ())
ptr_glTexGend = unsafePerformIO $ getCommand "glTexGend"

-- glTexGendv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGendv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glTexGendv v1 v2 v3 = liftIO $ dyn368 ptr_glTexGendv v1 v2 v3

{-# NOINLINE ptr_glTexGendv #-}
ptr_glTexGendv :: FunPtr (GLenum -> GLenum -> Ptr GLdouble -> IO ())
ptr_glTexGendv = unsafePerformIO $ getCommand "glTexGendv"

-- glTexGenf -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGenf
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glTexGenf v1 v2 v3 = liftIO $ dyn168 ptr_glTexGenf v1 v2 v3

{-# NOINLINE ptr_glTexGenf #-}
ptr_glTexGenf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glTexGenf = unsafePerformIO $ getCommand "glTexGenf"

-- glTexGenfOES ----------------------------------------------------------------

glTexGenfOES
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLfloat -- ^ @param@.
  -> m ()
glTexGenfOES v1 v2 v3 = liftIO $ dyn168 ptr_glTexGenfOES v1 v2 v3

{-# NOINLINE ptr_glTexGenfOES #-}
ptr_glTexGenfOES :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glTexGenfOES = unsafePerformIO $ getCommand "glTexGenfOES"

-- glTexGenfv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGenfv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glTexGenfv v1 v2 v3 = liftIO $ dyn139 ptr_glTexGenfv v1 v2 v3

{-# NOINLINE ptr_glTexGenfv #-}
ptr_glTexGenfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glTexGenfv = unsafePerformIO $ getCommand "glTexGenfv"

-- glTexGenfvOES ---------------------------------------------------------------

glTexGenfvOES
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glTexGenfvOES v1 v2 v3 = liftIO $ dyn139 ptr_glTexGenfvOES v1 v2 v3

{-# NOINLINE ptr_glTexGenfvOES #-}
ptr_glTexGenfvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glTexGenfvOES = unsafePerformIO $ getCommand "glTexGenfvOES"

-- glTexGeni -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGeni
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glTexGeni v1 v2 v3 = liftIO $ dyn66 ptr_glTexGeni v1 v2 v3

{-# NOINLINE ptr_glTexGeni #-}
ptr_glTexGeni :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glTexGeni = unsafePerformIO $ getCommand "glTexGeni"

-- glTexGeniOES ----------------------------------------------------------------

glTexGeniOES
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLint -- ^ @param@.
  -> m ()
glTexGeniOES v1 v2 v3 = liftIO $ dyn66 ptr_glTexGeniOES v1 v2 v3

{-# NOINLINE ptr_glTexGeniOES #-}
ptr_glTexGeniOES :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glTexGeniOES = unsafePerformIO $ getCommand "glTexGeniOES"

-- glTexGeniv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGeniv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glTexGeniv v1 v2 v3 = liftIO $ dyn140 ptr_glTexGeniv v1 v2 v3

{-# NOINLINE ptr_glTexGeniv #-}
ptr_glTexGeniv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexGeniv = unsafePerformIO $ getCommand "glTexGeniv"

-- glTexGenivOES ---------------------------------------------------------------

glTexGenivOES
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glTexGenivOES v1 v2 v3 = liftIO $ dyn140 ptr_glTexGenivOES v1 v2 v3

{-# NOINLINE ptr_glTexGenivOES #-}
ptr_glTexGenivOES :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexGenivOES = unsafePerformIO $ getCommand "glTexGenivOES"

-- glTexGenxOES ----------------------------------------------------------------

glTexGenxOES
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glTexGenxOES v1 v2 v3 = liftIO $ dyn169 ptr_glTexGenxOES v1 v2 v3

{-# NOINLINE ptr_glTexGenxOES #-}
ptr_glTexGenxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glTexGenxOES = unsafePerformIO $ getCommand "glTexGenxOES"

-- glTexGenxvOES ---------------------------------------------------------------

glTexGenxvOES
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glTexGenxvOES v1 v2 v3 = liftIO $ dyn170 ptr_glTexGenxvOES v1 v2 v3

{-# NOINLINE ptr_glTexGenxvOES #-}
ptr_glTexGenxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glTexGenxvOES = unsafePerformIO $ getCommand "glTexGenxvOES"

-- glTexImage1D ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexImage1D.xhtml OpenGL 4.x>.
glTexImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glTexImage1D v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn773 ptr_glTexImage1D v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTexImage1D #-}
ptr_glTexImage1D :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage1D = unsafePerformIO $ getCommand "glTexImage1D"

-- glTexImage2D ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexImage2D.xhtml OpenGL 4.x>.
glTexImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glTexImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn774 ptr_glTexImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexImage2D #-}
ptr_glTexImage2D :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage2D = unsafePerformIO $ getCommand "glTexImage2D"

-- glTexImage2DMultisample -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glTexImage2DMultisample.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexImage2DMultisample.xhtml OpenGL 4.x>.
glTexImage2DMultisample
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexImage2DMultisample v1 v2 v3 v4 v5 v6 = liftIO $ dyn775 ptr_glTexImage2DMultisample v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTexImage2DMultisample #-}
ptr_glTexImage2DMultisample :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexImage2DMultisample = unsafePerformIO $ getCommand "glTexImage2DMultisample"

-- glTexImage2DMultisampleCoverageNV -------------------------------------------

glTexImage2DMultisampleCoverageNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @coverageSamples@.
  -> GLsizei -- ^ @colorSamples@.
  -> GLint -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLboolean -- ^ @fixedSampleLocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexImage2DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn776 ptr_glTexImage2DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexImage2DMultisampleCoverageNV #-}
ptr_glTexImage2DMultisampleCoverageNV :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexImage2DMultisampleCoverageNV = unsafePerformIO $ getCommand "glTexImage2DMultisampleCoverageNV"

-- glTexImage3D ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexImage3D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexImage3D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexImage3D.xhtml OpenGL 4.x>.
glTexImage3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
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
glTexImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn777 ptr_glTexImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glTexImage3D #-}
ptr_glTexImage3D :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage3D = unsafePerformIO $ getCommand "glTexImage3D"

-- glTexImage3DEXT -------------------------------------------------------------

-- | This command is an alias for 'glTexImage3D'.
glTexImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn778 ptr_glTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glTexImage3DEXT #-}
ptr_glTexImage3DEXT :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage3DEXT = unsafePerformIO $ getCommand "glTexImage3DEXT"

-- glTexImage3DMultisample -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glTexImage3DMultisample.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexImage3DMultisample.xhtml OpenGL 4.x>.
glTexImage3DMultisample
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexImage3DMultisample v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn779 ptr_glTexImage3DMultisample v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexImage3DMultisample #-}
ptr_glTexImage3DMultisample :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexImage3DMultisample = unsafePerformIO $ getCommand "glTexImage3DMultisample"

-- glTexImage3DMultisampleCoverageNV -------------------------------------------

glTexImage3DMultisampleCoverageNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @coverageSamples@.
  -> GLsizei -- ^ @colorSamples@.
  -> GLint -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedSampleLocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexImage3DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn780 ptr_glTexImage3DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTexImage3DMultisampleCoverageNV #-}
ptr_glTexImage3DMultisampleCoverageNV :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexImage3DMultisampleCoverageNV = unsafePerformIO $ getCommand "glTexImage3DMultisampleCoverageNV"

-- glTexImage3DOES -------------------------------------------------------------

glTexImage3DOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glTexImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn778 ptr_glTexImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glTexImage3DOES #-}
ptr_glTexImage3DOES :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage3DOES = unsafePerformIO $ getCommand "glTexImage3DOES"

-- glTexImage4DSGIS ------------------------------------------------------------

glTexImage4DSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLsizei -- ^ @size4d@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth,size4d)@ elements of type @a@.
  -> m ()
glTexImage4DSGIS v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn781 ptr_glTexImage4DSGIS v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glTexImage4DSGIS #-}
ptr_glTexImage4DSGIS :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage4DSGIS = unsafePerformIO $ getCommand "glTexImage4DSGIS"

-- glTexPageCommitmentARB ------------------------------------------------------

glTexPageCommitmentARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @commit@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexPageCommitmentARB v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn782 ptr_glTexPageCommitmentARB v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexPageCommitmentARB #-}
ptr_glTexPageCommitmentARB :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexPageCommitmentARB = unsafePerformIO $ getCommand "glTexPageCommitmentARB"

-- glTexPageCommitmentEXT ------------------------------------------------------

-- | This command is an alias for 'glTexPageCommitmentARB'.
glTexPageCommitmentEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @commit@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexPageCommitmentEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn782 ptr_glTexPageCommitmentEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexPageCommitmentEXT #-}
ptr_glTexPageCommitmentEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexPageCommitmentEXT = unsafePerformIO $ getCommand "glTexPageCommitmentEXT"

-- glTexParameterIiv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameterIiv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glTexParameterIiv v1 v2 v3 = liftIO $ dyn140 ptr_glTexParameterIiv v1 v2 v3

{-# NOINLINE ptr_glTexParameterIiv #-}
ptr_glTexParameterIiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexParameterIiv = unsafePerformIO $ getCommand "glTexParameterIiv"

-- glTexParameterIivEXT --------------------------------------------------------

-- | This command is an alias for 'glTexParameterIiv'.
glTexParameterIivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glTexParameterIivEXT v1 v2 v3 = liftIO $ dyn140 ptr_glTexParameterIivEXT v1 v2 v3

{-# NOINLINE ptr_glTexParameterIivEXT #-}
ptr_glTexParameterIivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexParameterIivEXT = unsafePerformIO $ getCommand "glTexParameterIivEXT"

-- glTexParameterIivOES --------------------------------------------------------

-- | This command is an alias for 'glTexParameterIiv'.
glTexParameterIivOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glTexParameterIivOES v1 v2 v3 = liftIO $ dyn140 ptr_glTexParameterIivOES v1 v2 v3

{-# NOINLINE ptr_glTexParameterIivOES #-}
ptr_glTexParameterIivOES :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexParameterIivOES = unsafePerformIO $ getCommand "glTexParameterIivOES"

-- glTexParameterIuiv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameterIuiv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glTexParameterIuiv v1 v2 v3 = liftIO $ dyn432 ptr_glTexParameterIuiv v1 v2 v3

{-# NOINLINE ptr_glTexParameterIuiv #-}
ptr_glTexParameterIuiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glTexParameterIuiv = unsafePerformIO $ getCommand "glTexParameterIuiv"

-- glTexParameterIuivEXT -------------------------------------------------------

-- | This command is an alias for 'glTexParameterIuiv'.
glTexParameterIuivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glTexParameterIuivEXT v1 v2 v3 = liftIO $ dyn432 ptr_glTexParameterIuivEXT v1 v2 v3

{-# NOINLINE ptr_glTexParameterIuivEXT #-}
ptr_glTexParameterIuivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glTexParameterIuivEXT = unsafePerformIO $ getCommand "glTexParameterIuivEXT"

-- glTexParameterIuivOES -------------------------------------------------------

-- | This command is an alias for 'glTexParameterIuiv'.
glTexParameterIuivOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glTexParameterIuivOES v1 v2 v3 = liftIO $ dyn432 ptr_glTexParameterIuivOES v1 v2 v3

{-# NOINLINE ptr_glTexParameterIuivOES #-}
ptr_glTexParameterIuivOES :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glTexParameterIuivOES = unsafePerformIO $ getCommand "glTexParameterIuivOES"

-- glTexParameterf -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameterf
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glTexParameterf v1 v2 v3 = liftIO $ dyn168 ptr_glTexParameterf v1 v2 v3

{-# NOINLINE ptr_glTexParameterf #-}
ptr_glTexParameterf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glTexParameterf = unsafePerformIO $ getCommand "glTexParameterf"

-- glTexParameterfv ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glTexParameterfv v1 v2 v3 = liftIO $ dyn139 ptr_glTexParameterfv v1 v2 v3

{-# NOINLINE ptr_glTexParameterfv #-}
ptr_glTexParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glTexParameterfv = unsafePerformIO $ getCommand "glTexParameterfv"

-- glTexParameteri -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameteri
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glTexParameteri v1 v2 v3 = liftIO $ dyn66 ptr_glTexParameteri v1 v2 v3

{-# NOINLINE ptr_glTexParameteri #-}
ptr_glTexParameteri :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glTexParameteri = unsafePerformIO $ getCommand "glTexParameteri"

-- glTexParameteriv ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glTexParameteriv v1 v2 v3 = liftIO $ dyn140 ptr_glTexParameteriv v1 v2 v3

{-# NOINLINE ptr_glTexParameteriv #-}
ptr_glTexParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexParameteriv = unsafePerformIO $ getCommand "glTexParameteriv"

-- glTexParameterx -------------------------------------------------------------

glTexParameterx
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glTexParameterx v1 v2 v3 = liftIO $ dyn169 ptr_glTexParameterx v1 v2 v3

{-# NOINLINE ptr_glTexParameterx #-}
ptr_glTexParameterx :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glTexParameterx = unsafePerformIO $ getCommand "glTexParameterx"

-- glTexParameterxOES ----------------------------------------------------------

glTexParameterxOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glTexParameterxOES v1 v2 v3 = liftIO $ dyn169 ptr_glTexParameterxOES v1 v2 v3

{-# NOINLINE ptr_glTexParameterxOES #-}
ptr_glTexParameterxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glTexParameterxOES = unsafePerformIO $ getCommand "glTexParameterxOES"

-- glTexParameterxv ------------------------------------------------------------

glTexParameterxv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glTexParameterxv v1 v2 v3 = liftIO $ dyn170 ptr_glTexParameterxv v1 v2 v3

{-# NOINLINE ptr_glTexParameterxv #-}
ptr_glTexParameterxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glTexParameterxv = unsafePerformIO $ getCommand "glTexParameterxv"

-- glTexParameterxvOES ---------------------------------------------------------

glTexParameterxvOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glTexParameterxvOES v1 v2 v3 = liftIO $ dyn170 ptr_glTexParameterxvOES v1 v2 v3

{-# NOINLINE ptr_glTexParameterxvOES #-}
ptr_glTexParameterxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glTexParameterxvOES = unsafePerformIO $ getCommand "glTexParameterxvOES"

-- glTexRenderbufferNV ---------------------------------------------------------

glTexRenderbufferNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glTexRenderbufferNV v1 v2 = liftIO $ dyn19 ptr_glTexRenderbufferNV v1 v2

{-# NOINLINE ptr_glTexRenderbufferNV #-}
ptr_glTexRenderbufferNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glTexRenderbufferNV = unsafePerformIO $ getCommand "glTexRenderbufferNV"

-- glTexStorage1D --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage1D.xhtml OpenGL 4.x>.
glTexStorage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> m ()
glTexStorage1D v1 v2 v3 v4 = liftIO $ dyn783 ptr_glTexStorage1D v1 v2 v3 v4

{-# NOINLINE ptr_glTexStorage1D #-}
ptr_glTexStorage1D :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> IO ())
ptr_glTexStorage1D = unsafePerformIO $ getCommand "glTexStorage1D"

-- glTexStorage1DEXT -----------------------------------------------------------

-- | This command is an alias for 'glTexStorage1D'.
glTexStorage1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> m ()
glTexStorage1DEXT v1 v2 v3 v4 = liftIO $ dyn783 ptr_glTexStorage1DEXT v1 v2 v3 v4

{-# NOINLINE ptr_glTexStorage1DEXT #-}
ptr_glTexStorage1DEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> IO ())
ptr_glTexStorage1DEXT = unsafePerformIO $ getCommand "glTexStorage1DEXT"

-- glTexStorage2D --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage2D.xhtml OpenGL 4.x>.
glTexStorage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glTexStorage2D v1 v2 v3 v4 v5 = liftIO $ dyn720 ptr_glTexStorage2D v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexStorage2D #-}
ptr_glTexStorage2D :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glTexStorage2D = unsafePerformIO $ getCommand "glTexStorage2D"

-- glTexStorage2DEXT -----------------------------------------------------------

-- | This command is an alias for 'glTexStorage2D'.
glTexStorage2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glTexStorage2DEXT v1 v2 v3 v4 v5 = liftIO $ dyn720 ptr_glTexStorage2DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexStorage2DEXT #-}
ptr_glTexStorage2DEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glTexStorage2DEXT = unsafePerformIO $ getCommand "glTexStorage2DEXT"

-- glTexStorage2DMultisample ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage2DMultisample.xhtml OpenGL 4.x>.
glTexStorage2DMultisample
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexStorage2DMultisample v1 v2 v3 v4 v5 v6 = liftIO $ dyn775 ptr_glTexStorage2DMultisample v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTexStorage2DMultisample #-}
ptr_glTexStorage2DMultisample :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexStorage2DMultisample = unsafePerformIO $ getCommand "glTexStorage2DMultisample"

-- glTexStorage3D --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage3D.xhtml OpenGL 4.x>.
glTexStorage3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> m ()
glTexStorage3D v1 v2 v3 v4 v5 v6 = liftIO $ dyn784 ptr_glTexStorage3D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTexStorage3D #-}
ptr_glTexStorage3D :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glTexStorage3D = unsafePerformIO $ getCommand "glTexStorage3D"

-- glTexStorage3DEXT -----------------------------------------------------------

-- | This command is an alias for 'glTexStorage3D'.
glTexStorage3DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> m ()
glTexStorage3DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn784 ptr_glTexStorage3DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTexStorage3DEXT #-}
ptr_glTexStorage3DEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glTexStorage3DEXT = unsafePerformIO $ getCommand "glTexStorage3DEXT"

-- glTexStorage3DMultisample ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage3DMultisample.xhtml OpenGL 4.x>.
glTexStorage3DMultisample
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexStorage3DMultisample v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn779 ptr_glTexStorage3DMultisample v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexStorage3DMultisample #-}
ptr_glTexStorage3DMultisample :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexStorage3DMultisample = unsafePerformIO $ getCommand "glTexStorage3DMultisample"

-- glTexStorage3DMultisampleOES ------------------------------------------------

-- | This command is an alias for 'glTexStorage3DMultisample'.
glTexStorage3DMultisampleOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexStorage3DMultisampleOES v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn779 ptr_glTexStorage3DMultisampleOES v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexStorage3DMultisampleOES #-}
ptr_glTexStorage3DMultisampleOES :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexStorage3DMultisampleOES = unsafePerformIO $ getCommand "glTexStorage3DMultisampleOES"

-- glTexStorageMem1DEXT --------------------------------------------------------

glTexStorageMem1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLuint -- ^ @memory@.
  -> GLuint64 -- ^ @offset@.
  -> m ()
glTexStorageMem1DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn785 ptr_glTexStorageMem1DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTexStorageMem1DEXT #-}
ptr_glTexStorageMem1DEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLuint -> GLuint64 -> IO ())
ptr_glTexStorageMem1DEXT = unsafePerformIO $ getCommand "glTexStorageMem1DEXT"

-- glTexStorageMem2DEXT --------------------------------------------------------

glTexStorageMem2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLuint -- ^ @memory@.
  -> GLuint64 -- ^ @offset@.
  -> m ()
glTexStorageMem2DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn786 ptr_glTexStorageMem2DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexStorageMem2DEXT #-}
ptr_glTexStorageMem2DEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLuint -> GLuint64 -> IO ())
ptr_glTexStorageMem2DEXT = unsafePerformIO $ getCommand "glTexStorageMem2DEXT"

-- glTexStorageMem2DMultisampleEXT ---------------------------------------------

glTexStorageMem2DMultisampleEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLboolean -- ^ @fixedSampleLocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @memory@.
  -> GLuint64 -- ^ @offset@.
  -> m ()
glTexStorageMem2DMultisampleEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn787 ptr_glTexStorageMem2DMultisampleEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTexStorageMem2DMultisampleEXT #-}
ptr_glTexStorageMem2DMultisampleEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> GLuint -> GLuint64 -> IO ())
ptr_glTexStorageMem2DMultisampleEXT = unsafePerformIO $ getCommand "glTexStorageMem2DMultisampleEXT"

-- glTexStorageMem3DEXT --------------------------------------------------------

glTexStorageMem3DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLuint -- ^ @memory@.
  -> GLuint64 -- ^ @offset@.
  -> m ()
glTexStorageMem3DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn788 ptr_glTexStorageMem3DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTexStorageMem3DEXT #-}
ptr_glTexStorageMem3DEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLuint -> GLuint64 -> IO ())
ptr_glTexStorageMem3DEXT = unsafePerformIO $ getCommand "glTexStorageMem3DEXT"

-- glTexStorageMem3DMultisampleEXT ---------------------------------------------

glTexStorageMem3DMultisampleEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedSampleLocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @memory@.
  -> GLuint64 -- ^ @offset@.
  -> m ()
glTexStorageMem3DMultisampleEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn789 ptr_glTexStorageMem3DMultisampleEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexStorageMem3DMultisampleEXT #-}
ptr_glTexStorageMem3DMultisampleEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> GLuint -> GLuint64 -> IO ())
ptr_glTexStorageMem3DMultisampleEXT = unsafePerformIO $ getCommand "glTexStorageMem3DMultisampleEXT"

-- glTexStorageSparseAMD -------------------------------------------------------

glTexStorageSparseAMD
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @internalFormat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLsizei -- ^ @layers@.
  -> GLbitfield -- ^ @flags@ of type [TextureStorageMaskAMD](Graphics-GL-Groups.html#TextureStorageMaskAMD).
  -> m ()
glTexStorageSparseAMD v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn790 ptr_glTexStorageSparseAMD v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexStorageSparseAMD #-}
ptr_glTexStorageSparseAMD :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> IO ())
ptr_glTexStorageSparseAMD = unsafePerformIO $ getCommand "glTexStorageSparseAMD"

-- glTexSubImage1D -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexSubImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexSubImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexSubImage1D.xhtml OpenGL 4.x>.
glTexSubImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glTexSubImage1D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn791 ptr_glTexSubImage1D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexSubImage1D #-}
ptr_glTexSubImage1D :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage1D = unsafePerformIO $ getCommand "glTexSubImage1D"

-- glTexSubImage1DEXT ----------------------------------------------------------

-- | This command is an alias for 'glTexSubImage1D'.
glTexSubImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn791 ptr_glTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexSubImage1DEXT #-}
ptr_glTexSubImage1DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage1DEXT = unsafePerformIO $ getCommand "glTexSubImage1DEXT"

-- glTexSubImage2D -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexSubImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexSubImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexSubImage2D.xhtml OpenGL 4.x>.
glTexSubImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn792 ptr_glTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexSubImage2D #-}
ptr_glTexSubImage2D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage2D = unsafePerformIO $ getCommand "glTexSubImage2D"

-- glTexSubImage2DEXT ----------------------------------------------------------

-- | This command is an alias for 'glTexSubImage2D'.
glTexSubImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn792 ptr_glTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexSubImage2DEXT #-}
ptr_glTexSubImage2DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage2DEXT = unsafePerformIO $ getCommand "glTexSubImage2DEXT"

-- glTexSubImage3D -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexSubImage3D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexSubImage3D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexSubImage3D.xhtml OpenGL 4.x>.
glTexSubImage3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
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
glTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn283 ptr_glTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glTexSubImage3D #-}
ptr_glTexSubImage3D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage3D = unsafePerformIO $ getCommand "glTexSubImage3D"

-- glTexSubImage3DEXT ----------------------------------------------------------

-- | This command is an alias for 'glTexSubImage3D'.
glTexSubImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
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
glTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn283 ptr_glTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glTexSubImage3DEXT #-}
ptr_glTexSubImage3DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage3DEXT = unsafePerformIO $ getCommand "glTexSubImage3DEXT"

-- glTexSubImage3DOES ----------------------------------------------------------

glTexSubImage3DOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn283 ptr_glTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glTexSubImage3DOES #-}
ptr_glTexSubImage3DOES :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage3DOES = unsafePerformIO $ getCommand "glTexSubImage3DOES"

-- glTexSubImage4DSGIS ---------------------------------------------------------

glTexSubImage4DSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @woffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLsizei -- ^ @size4d@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth,size4d)@ elements of type @a@.
  -> m ()
glTexSubImage4DSGIS v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 = liftIO $ dyn793 ptr_glTexSubImage4DSGIS v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13

{-# NOINLINE ptr_glTexSubImage4DSGIS #-}
ptr_glTexSubImage4DSGIS :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage4DSGIS = unsafePerformIO $ getCommand "glTexSubImage4DSGIS"

-- glTextureAttachMemoryNV -----------------------------------------------------

glTextureAttachMemoryNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLuint -- ^ @memory@.
  -> GLuint64 -- ^ @offset@.
  -> m ()
glTextureAttachMemoryNV v1 v2 v3 = liftIO $ dyn607 ptr_glTextureAttachMemoryNV v1 v2 v3

{-# NOINLINE ptr_glTextureAttachMemoryNV #-}
ptr_glTextureAttachMemoryNV :: FunPtr (GLuint -> GLuint -> GLuint64 -> IO ())
ptr_glTextureAttachMemoryNV = unsafePerformIO $ getCommand "glTextureAttachMemoryNV"

-- glTextureBarrier ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTextureBarrier.xhtml OpenGL 4.x>.
glTextureBarrier
  :: MonadIO m
  => m ()
glTextureBarrier = liftIO $ dyn11 ptr_glTextureBarrier

{-# NOINLINE ptr_glTextureBarrier #-}
ptr_glTextureBarrier :: FunPtr (IO ())
ptr_glTextureBarrier = unsafePerformIO $ getCommand "glTextureBarrier"

-- glTextureBarrierNV ----------------------------------------------------------

glTextureBarrierNV
  :: MonadIO m
  => m ()
glTextureBarrierNV = liftIO $ dyn11 ptr_glTextureBarrierNV

{-# NOINLINE ptr_glTextureBarrierNV #-}
ptr_glTextureBarrierNV :: FunPtr (IO ())
ptr_glTextureBarrierNV = unsafePerformIO $ getCommand "glTextureBarrierNV"

-- glTextureBuffer -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexBuffer.xhtml OpenGL 4.x>.
glTextureBuffer
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLuint -- ^ @buffer@.
  -> m ()
glTextureBuffer v1 v2 v3 = liftIO $ dyn755 ptr_glTextureBuffer v1 v2 v3

{-# NOINLINE ptr_glTextureBuffer #-}
ptr_glTextureBuffer :: FunPtr (GLuint -> GLenum -> GLuint -> IO ())
ptr_glTextureBuffer = unsafePerformIO $ getCommand "glTextureBuffer"

-- glTextureBufferEXT ----------------------------------------------------------

glTextureBufferEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLuint -- ^ @buffer@.
  -> m ()
glTextureBufferEXT v1 v2 v3 v4 = liftIO $ dyn613 ptr_glTextureBufferEXT v1 v2 v3 v4

{-# NOINLINE ptr_glTextureBufferEXT #-}
ptr_glTextureBufferEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glTextureBufferEXT = unsafePerformIO $ getCommand "glTextureBufferEXT"

-- glTextureBufferRange --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexBufferRange.xhtml OpenGL 4.x>.
glTextureBufferRange
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glTextureBufferRange v1 v2 v3 v4 v5 = liftIO $ dyn794 ptr_glTextureBufferRange v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTextureBufferRange #-}
ptr_glTextureBufferRange :: FunPtr (GLuint -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glTextureBufferRange = unsafePerformIO $ getCommand "glTextureBufferRange"

