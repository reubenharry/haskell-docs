{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F11
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

module Graphics.GL.Functions.F11 (
  glGetMaterialxOES,
  glGetMaterialxv,
  glGetMaterialxvOES,
  glGetMemoryObjectDetachedResourcesuivNV,
  glGetMemoryObjectParameterivEXT,
  glGetMinmax,
  glGetMinmaxEXT,
  glGetMinmaxParameterfv,
  glGetMinmaxParameterfvEXT,
  glGetMinmaxParameteriv,
  glGetMinmaxParameterivEXT,
  glGetMultiTexEnvfvEXT,
  glGetMultiTexEnvivEXT,
  glGetMultiTexGendvEXT,
  glGetMultiTexGenfvEXT,
  glGetMultiTexGenivEXT,
  glGetMultiTexImageEXT,
  glGetMultiTexLevelParameterfvEXT,
  glGetMultiTexLevelParameterivEXT,
  glGetMultiTexParameterIivEXT,
  glGetMultiTexParameterIuivEXT,
  glGetMultiTexParameterfvEXT,
  glGetMultiTexParameterivEXT,
  glGetMultisamplefv,
  glGetMultisamplefvNV,
  glGetNamedBufferParameteri64v,
  glGetNamedBufferParameteriv,
  glGetNamedBufferParameterivEXT,
  glGetNamedBufferParameterui64vNV,
  glGetNamedBufferPointerv,
  glGetNamedBufferPointervEXT,
  glGetNamedBufferSubData,
  glGetNamedBufferSubDataEXT,
  glGetNamedFramebufferAttachmentParameteriv,
  glGetNamedFramebufferAttachmentParameterivEXT,
  glGetNamedFramebufferParameterfvAMD,
  glGetNamedFramebufferParameteriv,
  glGetNamedFramebufferParameterivEXT,
  glGetNamedProgramLocalParameterIivEXT,
  glGetNamedProgramLocalParameterIuivEXT,
  glGetNamedProgramLocalParameterdvEXT,
  glGetNamedProgramLocalParameterfvEXT,
  glGetNamedProgramStringEXT,
  glGetNamedProgramivEXT,
  glGetNamedRenderbufferParameteriv,
  glGetNamedRenderbufferParameterivEXT,
  glGetNamedStringARB,
  glGetNamedStringivARB,
  glGetNextPerfQueryIdINTEL,
  glGetObjectBufferfvATI,
  glGetObjectBufferivATI,
  glGetObjectLabel,
  glGetObjectLabelEXT,
  glGetObjectLabelKHR,
  glGetObjectParameterfvARB,
  glGetObjectParameterivAPPLE,
  glGetObjectParameterivARB,
  glGetObjectPtrLabel,
  glGetObjectPtrLabelKHR,
  glGetOcclusionQueryivNV,
  glGetOcclusionQueryuivNV,
  glGetPathColorGenfvNV,
  glGetPathColorGenivNV,
  glGetPathCommandsNV,
  glGetPathCoordsNV,
  glGetPathDashArrayNV,
  glGetPathLengthNV,
  glGetPathMetricRangeNV,
  glGetPathMetricsNV,
  glGetPathParameterfvNV,
  glGetPathParameterivNV,
  glGetPathSpacingNV,
  glGetPathTexGenfvNV,
  glGetPathTexGenivNV,
  glGetPerfCounterInfoINTEL,
  glGetPerfMonitorCounterDataAMD,
  glGetPerfMonitorCounterInfoAMD,
  glGetPerfMonitorCounterStringAMD,
  glGetPerfMonitorCountersAMD,
  glGetPerfMonitorGroupStringAMD,
  glGetPerfMonitorGroupsAMD,
  glGetPerfQueryDataINTEL,
  glGetPerfQueryIdByNameINTEL,
  glGetPerfQueryInfoINTEL,
  glGetPixelMapfv,
  glGetPixelMapuiv,
  glGetPixelMapusv,
  glGetPixelMapxv,
  glGetPixelTexGenParameterfvSGIS,
  glGetPixelTexGenParameterivSGIS,
  glGetPixelTransformParameterfvEXT,
  glGetPixelTransformParameterivEXT,
  glGetPointerIndexedvEXT,
  glGetPointeri_vEXT,
  glGetPointerv,
  glGetPointervEXT,
  glGetPointervKHR,
  glGetPolygonStipple,
  glGetProgramBinary,
  glGetProgramBinaryOES
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glGetMaterialxOES -----------------------------------------------------------

glGetMaterialxOES
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> GLfixed -- ^ @param@.
  -> m ()
glGetMaterialxOES v1 v2 v3 = liftIO $ dyn169 ptr_glGetMaterialxOES v1 v2 v3

{-# NOINLINE ptr_glGetMaterialxOES #-}
ptr_glGetMaterialxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glGetMaterialxOES = unsafePerformIO $ getCommand "glGetMaterialxOES"

-- glGetMaterialxv -------------------------------------------------------------

glGetMaterialxv
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetMaterialxv v1 v2 v3 = liftIO $ dyn170 ptr_glGetMaterialxv v1 v2 v3

{-# NOINLINE ptr_glGetMaterialxv #-}
ptr_glGetMaterialxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetMaterialxv = unsafePerformIO $ getCommand "glGetMaterialxv"

-- glGetMaterialxvOES ----------------------------------------------------------

glGetMaterialxvOES
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetMaterialxvOES v1 v2 v3 = liftIO $ dyn170 ptr_glGetMaterialxvOES v1 v2 v3

{-# NOINLINE ptr_glGetMaterialxvOES #-}
ptr_glGetMaterialxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetMaterialxvOES = unsafePerformIO $ getCommand "glGetMaterialxvOES"

-- glGetMemoryObjectDetachedResourcesuivNV -------------------------------------

glGetMemoryObjectDetachedResourcesuivNV
  :: MonadIO m
  => GLuint -- ^ @memory@.
  -> GLenum -- ^ @pname@.
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @params@.
  -> m ()
glGetMemoryObjectDetachedResourcesuivNV v1 v2 v3 v4 v5 = liftIO $ dyn369 ptr_glGetMemoryObjectDetachedResourcesuivNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetMemoryObjectDetachedResourcesuivNV #-}
ptr_glGetMemoryObjectDetachedResourcesuivNV :: FunPtr (GLuint -> GLenum -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glGetMemoryObjectDetachedResourcesuivNV = unsafePerformIO $ getCommand "glGetMemoryObjectDetachedResourcesuivNV"

-- glGetMemoryObjectParameterivEXT ---------------------------------------------

glGetMemoryObjectParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @memoryObject@.
  -> GLenum -- ^ @pname@ of type [MemoryObjectParameterName](Graphics-GL-Groups.html#MemoryObjectParameterName).
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetMemoryObjectParameterivEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetMemoryObjectParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetMemoryObjectParameterivEXT #-}
ptr_glGetMemoryObjectParameterivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMemoryObjectParameterivEXT = unsafePerformIO $ getCommand "glGetMemoryObjectParameterivEXT"

-- glGetMinmax -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmax.xml OpenGL 2.x>.
glGetMinmax
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLboolean -- ^ @reset@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @values@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetMinmax v1 v2 v3 v4 v5 = liftIO $ dyn351 ptr_glGetMinmax v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetMinmax #-}
ptr_glGetMinmax :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetMinmax = unsafePerformIO $ getCommand "glGetMinmax"

-- glGetMinmaxEXT --------------------------------------------------------------

glGetMinmaxEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLboolean -- ^ @reset@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @values@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetMinmaxEXT v1 v2 v3 v4 v5 = liftIO $ dyn351 ptr_glGetMinmaxEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetMinmaxEXT #-}
ptr_glGetMinmaxEXT :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetMinmaxEXT = unsafePerformIO $ getCommand "glGetMinmaxEXT"

-- glGetMinmaxParameterfv ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmaxParameter.xml OpenGL 2.x>.
glGetMinmaxParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLenum -- ^ @pname@ of type [GetMinmaxParameterPNameEXT](Graphics-GL-Groups.html#GetMinmaxParameterPNameEXT).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMinmaxParameterfv v1 v2 v3 = liftIO $ dyn139 ptr_glGetMinmaxParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetMinmaxParameterfv #-}
ptr_glGetMinmaxParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMinmaxParameterfv = unsafePerformIO $ getCommand "glGetMinmaxParameterfv"

-- glGetMinmaxParameterfvEXT ---------------------------------------------------

glGetMinmaxParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLenum -- ^ @pname@ of type [GetMinmaxParameterPNameEXT](Graphics-GL-Groups.html#GetMinmaxParameterPNameEXT).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMinmaxParameterfvEXT v1 v2 v3 = liftIO $ dyn139 ptr_glGetMinmaxParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glGetMinmaxParameterfvEXT #-}
ptr_glGetMinmaxParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMinmaxParameterfvEXT = unsafePerformIO $ getCommand "glGetMinmaxParameterfvEXT"

-- glGetMinmaxParameteriv ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmaxParameter.xml OpenGL 2.x>.
glGetMinmaxParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLenum -- ^ @pname@ of type [GetMinmaxParameterPNameEXT](Graphics-GL-Groups.html#GetMinmaxParameterPNameEXT).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMinmaxParameteriv v1 v2 v3 = liftIO $ dyn140 ptr_glGetMinmaxParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetMinmaxParameteriv #-}
ptr_glGetMinmaxParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMinmaxParameteriv = unsafePerformIO $ getCommand "glGetMinmaxParameteriv"

-- glGetMinmaxParameterivEXT ---------------------------------------------------

glGetMinmaxParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLenum -- ^ @pname@ of type [GetMinmaxParameterPNameEXT](Graphics-GL-Groups.html#GetMinmaxParameterPNameEXT).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMinmaxParameterivEXT v1 v2 v3 = liftIO $ dyn140 ptr_glGetMinmaxParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetMinmaxParameterivEXT #-}
ptr_glGetMinmaxParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMinmaxParameterivEXT = unsafePerformIO $ getCommand "glGetMinmaxParameterivEXT"

-- glGetMultiTexEnvfvEXT -------------------------------------------------------

glGetMultiTexEnvfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMultiTexEnvfvEXT v1 v2 v3 v4 = liftIO $ dyn334 ptr_glGetMultiTexEnvfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexEnvfvEXT #-}
ptr_glGetMultiTexEnvfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMultiTexEnvfvEXT = unsafePerformIO $ getCommand "glGetMultiTexEnvfvEXT"

-- glGetMultiTexEnvivEXT -------------------------------------------------------

glGetMultiTexEnvivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMultiTexEnvivEXT v1 v2 v3 v4 = liftIO $ dyn335 ptr_glGetMultiTexEnvivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexEnvivEXT #-}
ptr_glGetMultiTexEnvivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMultiTexEnvivEXT = unsafePerformIO $ getCommand "glGetMultiTexEnvivEXT"

-- glGetMultiTexGendvEXT -------------------------------------------------------

glGetMultiTexGendvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetMultiTexGendvEXT v1 v2 v3 v4 = liftIO $ dyn370 ptr_glGetMultiTexGendvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexGendvEXT #-}
ptr_glGetMultiTexGendvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetMultiTexGendvEXT = unsafePerformIO $ getCommand "glGetMultiTexGendvEXT"

-- glGetMultiTexGenfvEXT -------------------------------------------------------

glGetMultiTexGenfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMultiTexGenfvEXT v1 v2 v3 v4 = liftIO $ dyn334 ptr_glGetMultiTexGenfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexGenfvEXT #-}
ptr_glGetMultiTexGenfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMultiTexGenfvEXT = unsafePerformIO $ getCommand "glGetMultiTexGenfvEXT"

-- glGetMultiTexGenivEXT -------------------------------------------------------

glGetMultiTexGenivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMultiTexGenivEXT v1 v2 v3 v4 = liftIO $ dyn335 ptr_glGetMultiTexGenivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexGenivEXT #-}
ptr_glGetMultiTexGenivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMultiTexGenivEXT = unsafePerformIO $ getCommand "glGetMultiTexGenivEXT"

-- glGetMultiTexImageEXT -------------------------------------------------------

glGetMultiTexImageEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(target,level,format,type)@ elements of type @a@.
  -> m ()
glGetMultiTexImageEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn371 ptr_glGetMultiTexImageEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetMultiTexImageEXT #-}
ptr_glGetMultiTexImageEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetMultiTexImageEXT = unsafePerformIO $ getCommand "glGetMultiTexImageEXT"

-- glGetMultiTexLevelParameterfvEXT --------------------------------------------

glGetMultiTexLevelParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMultiTexLevelParameterfvEXT v1 v2 v3 v4 v5 = liftIO $ dyn372 ptr_glGetMultiTexLevelParameterfvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetMultiTexLevelParameterfvEXT #-}
ptr_glGetMultiTexLevelParameterfvEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMultiTexLevelParameterfvEXT = unsafePerformIO $ getCommand "glGetMultiTexLevelParameterfvEXT"

-- glGetMultiTexLevelParameterivEXT --------------------------------------------

glGetMultiTexLevelParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMultiTexLevelParameterivEXT v1 v2 v3 v4 v5 = liftIO $ dyn373 ptr_glGetMultiTexLevelParameterivEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetMultiTexLevelParameterivEXT #-}
ptr_glGetMultiTexLevelParameterivEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMultiTexLevelParameterivEXT = unsafePerformIO $ getCommand "glGetMultiTexLevelParameterivEXT"

-- glGetMultiTexParameterIivEXT ------------------------------------------------

glGetMultiTexParameterIivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMultiTexParameterIivEXT v1 v2 v3 v4 = liftIO $ dyn335 ptr_glGetMultiTexParameterIivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexParameterIivEXT #-}
ptr_glGetMultiTexParameterIivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMultiTexParameterIivEXT = unsafePerformIO $ getCommand "glGetMultiTexParameterIivEXT"

-- glGetMultiTexParameterIuivEXT -----------------------------------------------

glGetMultiTexParameterIuivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetMultiTexParameterIuivEXT v1 v2 v3 v4 = liftIO $ dyn374 ptr_glGetMultiTexParameterIuivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexParameterIuivEXT #-}
ptr_glGetMultiTexParameterIuivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetMultiTexParameterIuivEXT = unsafePerformIO $ getCommand "glGetMultiTexParameterIuivEXT"

-- glGetMultiTexParameterfvEXT -------------------------------------------------

glGetMultiTexParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMultiTexParameterfvEXT v1 v2 v3 v4 = liftIO $ dyn334 ptr_glGetMultiTexParameterfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexParameterfvEXT #-}
ptr_glGetMultiTexParameterfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMultiTexParameterfvEXT = unsafePerformIO $ getCommand "glGetMultiTexParameterfvEXT"

-- glGetMultiTexParameterivEXT -------------------------------------------------

glGetMultiTexParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMultiTexParameterivEXT v1 v2 v3 v4 = liftIO $ dyn335 ptr_glGetMultiTexParameterivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexParameterivEXT #-}
ptr_glGetMultiTexParameterivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMultiTexParameterivEXT = unsafePerformIO $ getCommand "glGetMultiTexParameterivEXT"

-- glGetMultisamplefv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetMultisample.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetMultisample.xhtml OpenGL 4.x>.
glGetMultisamplefv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetMultisamplePNameNV](Graphics-GL-Groups.html#GetMultisamplePNameNV).
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @val@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMultisamplefv v1 v2 v3 = liftIO $ dyn278 ptr_glGetMultisamplefv v1 v2 v3

{-# NOINLINE ptr_glGetMultisamplefv #-}
ptr_glGetMultisamplefv :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetMultisamplefv = unsafePerformIO $ getCommand "glGetMultisamplefv"

-- glGetMultisamplefvNV --------------------------------------------------------

-- | This command is an alias for 'glGetMultisamplefv'.
glGetMultisamplefvNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetMultisamplePNameNV](Graphics-GL-Groups.html#GetMultisamplePNameNV).
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @val@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glGetMultisamplefvNV v1 v2 v3 = liftIO $ dyn278 ptr_glGetMultisamplefvNV v1 v2 v3

{-# NOINLINE ptr_glGetMultisamplefvNV #-}
ptr_glGetMultisamplefvNV :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetMultisamplefvNV = unsafePerformIO $ getCommand "glGetMultisamplefvNV"

-- glGetNamedBufferParameteri64v -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml OpenGL 4.x>.
glGetNamedBufferParameteri64v
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [VertexBufferObjectParameter](Graphics-GL-Groups.html#VertexBufferObjectParameter).
  -> Ptr GLint64 -- ^ @params@.
  -> m ()
glGetNamedBufferParameteri64v v1 v2 v3 = liftIO $ dyn375 ptr_glGetNamedBufferParameteri64v v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferParameteri64v #-}
ptr_glGetNamedBufferParameteri64v :: FunPtr (GLuint -> GLenum -> Ptr GLint64 -> IO ())
ptr_glGetNamedBufferParameteri64v = unsafePerformIO $ getCommand "glGetNamedBufferParameteri64v"

-- glGetNamedBufferParameteriv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml OpenGL 4.x>.
glGetNamedBufferParameteriv
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [VertexBufferObjectParameter](Graphics-GL-Groups.html#VertexBufferObjectParameter).
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetNamedBufferParameteriv v1 v2 v3 = liftIO $ dyn348 ptr_glGetNamedBufferParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferParameteriv #-}
ptr_glGetNamedBufferParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedBufferParameteriv = unsafePerformIO $ getCommand "glGetNamedBufferParameteriv"

-- glGetNamedBufferParameterivEXT ----------------------------------------------

glGetNamedBufferParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [VertexBufferObjectParameter](Graphics-GL-Groups.html#VertexBufferObjectParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetNamedBufferParameterivEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetNamedBufferParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferParameterivEXT #-}
ptr_glGetNamedBufferParameterivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedBufferParameterivEXT = unsafePerformIO $ getCommand "glGetNamedBufferParameterivEXT"

-- glGetNamedBufferParameterui64vNV --------------------------------------------

glGetNamedBufferParameterui64vNV
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [VertexBufferObjectParameter](Graphics-GL-Groups.html#VertexBufferObjectParameter).
  -> Ptr GLuint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64EXT@.
  -> m ()
glGetNamedBufferParameterui64vNV v1 v2 v3 = liftIO $ dyn376 ptr_glGetNamedBufferParameterui64vNV v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferParameterui64vNV #-}
ptr_glGetNamedBufferParameterui64vNV :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
ptr_glGetNamedBufferParameterui64vNV = unsafePerformIO $ getCommand "glGetNamedBufferParameterui64vNV"

-- glGetNamedBufferPointerv ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetBufferPointerv.xhtml OpenGL 4.x>.
glGetNamedBufferPointerv
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [VertexBufferObjectParameter](Graphics-GL-Groups.html#VertexBufferObjectParameter).
  -> Ptr (Ptr a) -- ^ @params@.
  -> m ()
glGetNamedBufferPointerv v1 v2 v3 = liftIO $ dyn377 ptr_glGetNamedBufferPointerv v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferPointerv #-}
ptr_glGetNamedBufferPointerv :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetNamedBufferPointerv = unsafePerformIO $ getCommand "glGetNamedBufferPointerv"

-- glGetNamedBufferPointervEXT -------------------------------------------------

glGetNamedBufferPointervEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [VertexBufferObjectParameter](Graphics-GL-Groups.html#VertexBufferObjectParameter).
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetNamedBufferPointervEXT v1 v2 v3 = liftIO $ dyn377 ptr_glGetNamedBufferPointervEXT v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferPointervEXT #-}
ptr_glGetNamedBufferPointervEXT :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetNamedBufferPointervEXT = unsafePerformIO $ getCommand "glGetNamedBufferPointervEXT"

-- glGetNamedBufferSubData -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetBufferSubData.xhtml OpenGL 4.x>.
glGetNamedBufferSubData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@.
  -> m ()
glGetNamedBufferSubData v1 v2 v3 v4 = liftIO $ dyn378 ptr_glGetNamedBufferSubData v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedBufferSubData #-}
ptr_glGetNamedBufferSubData :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glGetNamedBufferSubData = unsafePerformIO $ getCommand "glGetNamedBufferSubData"

-- glGetNamedBufferSubDataEXT --------------------------------------------------

glGetNamedBufferSubDataEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(size)@ elements of type @a@.
  -> m ()
glGetNamedBufferSubDataEXT v1 v2 v3 v4 = liftIO $ dyn378 ptr_glGetNamedBufferSubDataEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedBufferSubDataEXT #-}
ptr_glGetNamedBufferSubDataEXT :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glGetNamedBufferSubDataEXT = unsafePerformIO $ getCommand "glGetNamedBufferSubDataEXT"

-- glGetNamedFramebufferAttachmentParameteriv ----------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetFramebufferAttachmentParameter.xhtml OpenGL 4.x>.
glGetNamedFramebufferAttachmentParameteriv
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @pname@ of type [FramebufferAttachmentParameterName](Graphics-GL-Groups.html#FramebufferAttachmentParameterName).
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetNamedFramebufferAttachmentParameteriv v1 v2 v3 v4 = liftIO $ dyn379 ptr_glGetNamedFramebufferAttachmentParameteriv v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedFramebufferAttachmentParameteriv #-}
ptr_glGetNamedFramebufferAttachmentParameteriv :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedFramebufferAttachmentParameteriv = unsafePerformIO $ getCommand "glGetNamedFramebufferAttachmentParameteriv"

-- glGetNamedFramebufferAttachmentParameterivEXT -------------------------------

glGetNamedFramebufferAttachmentParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type [FramebufferAttachment](Graphics-GL-Groups.html#FramebufferAttachment).
  -> GLenum -- ^ @pname@ of type [FramebufferAttachmentParameterName](Graphics-GL-Groups.html#FramebufferAttachmentParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetNamedFramebufferAttachmentParameterivEXT v1 v2 v3 v4 = liftIO $ dyn379 ptr_glGetNamedFramebufferAttachmentParameterivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedFramebufferAttachmentParameterivEXT #-}
ptr_glGetNamedFramebufferAttachmentParameterivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedFramebufferAttachmentParameterivEXT = unsafePerformIO $ getCommand "glGetNamedFramebufferAttachmentParameterivEXT"

-- glGetNamedFramebufferParameterfvAMD -----------------------------------------

glGetNamedFramebufferParameterfvAMD
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @pname@.
  -> GLuint -- ^ @numsamples@.
  -> GLuint -- ^ @pixelindex@.
  -> GLsizei -- ^ @size@.
  -> Ptr GLfloat -- ^ @values@.
  -> m ()
glGetNamedFramebufferParameterfvAMD v1 v2 v3 v4 v5 v6 = liftIO $ dyn380 ptr_glGetNamedFramebufferParameterfvAMD v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetNamedFramebufferParameterfvAMD #-}
ptr_glGetNamedFramebufferParameterfvAMD :: FunPtr (GLuint -> GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetNamedFramebufferParameterfvAMD = unsafePerformIO $ getCommand "glGetNamedFramebufferParameterfvAMD"

-- glGetNamedFramebufferParameteriv --------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetFramebufferParameter.xhtml OpenGL 4.x>.
glGetNamedFramebufferParameteriv
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @pname@ of type [GetFramebufferParameter](Graphics-GL-Groups.html#GetFramebufferParameter).
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetNamedFramebufferParameteriv v1 v2 v3 = liftIO $ dyn348 ptr_glGetNamedFramebufferParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetNamedFramebufferParameteriv #-}
ptr_glGetNamedFramebufferParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedFramebufferParameteriv = unsafePerformIO $ getCommand "glGetNamedFramebufferParameteriv"

-- glGetNamedFramebufferParameterivEXT -----------------------------------------

glGetNamedFramebufferParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @pname@ of type [GetFramebufferParameter](Graphics-GL-Groups.html#GetFramebufferParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetNamedFramebufferParameterivEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetNamedFramebufferParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetNamedFramebufferParameterivEXT #-}
ptr_glGetNamedFramebufferParameterivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedFramebufferParameterivEXT = unsafePerformIO $ getCommand "glGetNamedFramebufferParameterivEXT"

-- glGetNamedProgramLocalParameterIivEXT ---------------------------------------

glGetNamedProgramLocalParameterIivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetNamedProgramLocalParameterIivEXT v1 v2 v3 v4 = liftIO $ dyn381 ptr_glGetNamedProgramLocalParameterIivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramLocalParameterIivEXT #-}
ptr_glGetNamedProgramLocalParameterIivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glGetNamedProgramLocalParameterIivEXT = unsafePerformIO $ getCommand "glGetNamedProgramLocalParameterIivEXT"

-- glGetNamedProgramLocalParameterIuivEXT --------------------------------------

glGetNamedProgramLocalParameterIuivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @params@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glGetNamedProgramLocalParameterIuivEXT v1 v2 v3 v4 = liftIO $ dyn382 ptr_glGetNamedProgramLocalParameterIuivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramLocalParameterIuivEXT #-}
ptr_glGetNamedProgramLocalParameterIuivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glGetNamedProgramLocalParameterIuivEXT = unsafePerformIO $ getCommand "glGetNamedProgramLocalParameterIuivEXT"

-- glGetNamedProgramLocalParameterdvEXT ----------------------------------------

glGetNamedProgramLocalParameterdvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetNamedProgramLocalParameterdvEXT v1 v2 v3 v4 = liftIO $ dyn383 ptr_glGetNamedProgramLocalParameterdvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramLocalParameterdvEXT #-}
ptr_glGetNamedProgramLocalParameterdvEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glGetNamedProgramLocalParameterdvEXT = unsafePerformIO $ getCommand "glGetNamedProgramLocalParameterdvEXT"

-- glGetNamedProgramLocalParameterfvEXT ----------------------------------------

glGetNamedProgramLocalParameterfvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetNamedProgramLocalParameterfvEXT v1 v2 v3 v4 = liftIO $ dyn384 ptr_glGetNamedProgramLocalParameterfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramLocalParameterfvEXT #-}
ptr_glGetNamedProgramLocalParameterfvEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetNamedProgramLocalParameterfvEXT = unsafePerformIO $ getCommand "glGetNamedProgramLocalParameterfvEXT"

-- glGetNamedProgramStringEXT --------------------------------------------------

glGetNamedProgramStringEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLenum -- ^ @pname@ of type [ProgramStringProperty](Graphics-GL-Groups.html#ProgramStringProperty).
  -> Ptr a -- ^ @string@ pointing to @COMPSIZE(program,pname)@ elements of type @a@.
  -> m ()
glGetNamedProgramStringEXT v1 v2 v3 v4 = liftIO $ dyn385 ptr_glGetNamedProgramStringEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramStringEXT #-}
ptr_glGetNamedProgramStringEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetNamedProgramStringEXT = unsafePerformIO $ getCommand "glGetNamedProgramStringEXT"

-- glGetNamedProgramivEXT ------------------------------------------------------

glGetNamedProgramivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLenum -- ^ @pname@ of type [ProgramPropertyARB](Graphics-GL-Groups.html#ProgramPropertyARB).
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetNamedProgramivEXT v1 v2 v3 v4 = liftIO $ dyn379 ptr_glGetNamedProgramivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramivEXT #-}
ptr_glGetNamedProgramivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedProgramivEXT = unsafePerformIO $ getCommand "glGetNamedProgramivEXT"

-- glGetNamedRenderbufferParameteriv -------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetRenderbufferParameter.xhtml OpenGL 4.x>.
glGetNamedRenderbufferParameteriv
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> GLenum -- ^ @pname@ of type [RenderbufferParameterName](Graphics-GL-Groups.html#RenderbufferParameterName).
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetNamedRenderbufferParameteriv v1 v2 v3 = liftIO $ dyn348 ptr_glGetNamedRenderbufferParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetNamedRenderbufferParameteriv #-}
ptr_glGetNamedRenderbufferParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedRenderbufferParameteriv = unsafePerformIO $ getCommand "glGetNamedRenderbufferParameteriv"

-- glGetNamedRenderbufferParameterivEXT ----------------------------------------

glGetNamedRenderbufferParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@ of type @Renderbuffer@.
  -> GLenum -- ^ @pname@ of type [RenderbufferParameterName](Graphics-GL-Groups.html#RenderbufferParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetNamedRenderbufferParameterivEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetNamedRenderbufferParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetNamedRenderbufferParameterivEXT #-}
ptr_glGetNamedRenderbufferParameterivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedRenderbufferParameterivEXT = unsafePerformIO $ getCommand "glGetNamedRenderbufferParameterivEXT"

-- glGetNamedStringARB ---------------------------------------------------------

glGetNamedStringARB
  :: MonadIO m
  => GLint -- ^ @namelen@.
  -> Ptr GLchar -- ^ @name@ pointing to @namelen@ elements of type @GLchar@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint -- ^ @stringlen@ pointing to @1@ element of type @GLint@.
  -> Ptr GLchar -- ^ @string@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetNamedStringARB v1 v2 v3 v4 v5 = liftIO $ dyn386 ptr_glGetNamedStringARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetNamedStringARB #-}
ptr_glGetNamedStringARB :: FunPtr (GLint -> Ptr GLchar -> GLsizei -> Ptr GLint -> Ptr GLchar -> IO ())
ptr_glGetNamedStringARB = unsafePerformIO $ getCommand "glGetNamedStringARB"

-- glGetNamedStringivARB -------------------------------------------------------

glGetNamedStringivARB
  :: MonadIO m
  => GLint -- ^ @namelen@.
  -> Ptr GLchar -- ^ @name@ pointing to @namelen@ elements of type @GLchar@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetNamedStringivARB v1 v2 v3 v4 = liftIO $ dyn387 ptr_glGetNamedStringivARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedStringivARB #-}
ptr_glGetNamedStringivARB :: FunPtr (GLint -> Ptr GLchar -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedStringivARB = unsafePerformIO $ getCommand "glGetNamedStringivARB"

-- glGetNextPerfQueryIdINTEL ---------------------------------------------------

glGetNextPerfQueryIdINTEL
  :: MonadIO m
  => GLuint -- ^ @queryId@.
  -> Ptr GLuint -- ^ @nextQueryId@.
  -> m ()
glGetNextPerfQueryIdINTEL v1 v2 = liftIO $ dyn201 ptr_glGetNextPerfQueryIdINTEL v1 v2

{-# NOINLINE ptr_glGetNextPerfQueryIdINTEL #-}
ptr_glGetNextPerfQueryIdINTEL :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glGetNextPerfQueryIdINTEL = unsafePerformIO $ getCommand "glGetNextPerfQueryIdINTEL"

-- glGetObjectBufferfvATI ------------------------------------------------------

glGetObjectBufferfvATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [ArrayObjectPNameATI](Graphics-GL-Groups.html#ArrayObjectPNameATI).
  -> Ptr GLfloat -- ^ @params@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glGetObjectBufferfvATI v1 v2 v3 = liftIO $ dyn364 ptr_glGetObjectBufferfvATI v1 v2 v3

{-# NOINLINE ptr_glGetObjectBufferfvATI #-}
ptr_glGetObjectBufferfvATI :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetObjectBufferfvATI = unsafePerformIO $ getCommand "glGetObjectBufferfvATI"

-- glGetObjectBufferivATI ------------------------------------------------------

glGetObjectBufferivATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type [ArrayObjectPNameATI](Graphics-GL-Groups.html#ArrayObjectPNameATI).
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetObjectBufferivATI v1 v2 v3 = liftIO $ dyn348 ptr_glGetObjectBufferivATI v1 v2 v3

{-# NOINLINE ptr_glGetObjectBufferivATI #-}
ptr_glGetObjectBufferivATI :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetObjectBufferivATI = unsafePerformIO $ getCommand "glGetObjectBufferivATI"

-- glGetObjectLabel ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetObjectLabel.xhtml OpenGL 4.x>.
glGetObjectLabel
  :: MonadIO m
  => GLenum -- ^ @identifier@ of type [ObjectIdentifier](Graphics-GL-Groups.html#ObjectIdentifier).
  -> GLuint -- ^ @name@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @label@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetObjectLabel v1 v2 v3 v4 v5 = liftIO $ dyn388 ptr_glGetObjectLabel v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetObjectLabel #-}
ptr_glGetObjectLabel :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetObjectLabel = unsafePerformIO $ getCommand "glGetObjectLabel"

-- glGetObjectLabelEXT ---------------------------------------------------------

glGetObjectLabelEXT
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @object@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @label@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetObjectLabelEXT v1 v2 v3 v4 v5 = liftIO $ dyn388 ptr_glGetObjectLabelEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetObjectLabelEXT #-}
ptr_glGetObjectLabelEXT :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetObjectLabelEXT = unsafePerformIO $ getCommand "glGetObjectLabelEXT"

-- glGetObjectLabelKHR ---------------------------------------------------------

-- | This command is an alias for 'glGetObjectLabel'.
glGetObjectLabelKHR
  :: MonadIO m
  => GLenum -- ^ @identifier@.
  -> GLuint -- ^ @name@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetObjectLabelKHR v1 v2 v3 v4 v5 = liftIO $ dyn388 ptr_glGetObjectLabelKHR v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetObjectLabelKHR #-}
ptr_glGetObjectLabelKHR :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetObjectLabelKHR = unsafePerformIO $ getCommand "glGetObjectLabelKHR"

-- glGetObjectParameterfvARB ---------------------------------------------------

glGetObjectParameterfvARB
  :: MonadIO m
  => GLhandleARB -- ^ @obj@ of type @handleARB@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetObjectParameterfvARB v1 v2 v3 = liftIO $ dyn389 ptr_glGetObjectParameterfvARB v1 v2 v3

{-# NOINLINE ptr_glGetObjectParameterfvARB #-}
ptr_glGetObjectParameterfvARB :: FunPtr (GLhandleARB -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetObjectParameterfvARB = unsafePerformIO $ getCommand "glGetObjectParameterfvARB"

-- glGetObjectParameterivAPPLE -------------------------------------------------

glGetObjectParameterivAPPLE
  :: MonadIO m
  => GLenum -- ^ @objectType@.
  -> GLuint -- ^ @name@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetObjectParameterivAPPLE v1 v2 v3 v4 = liftIO $ dyn366 ptr_glGetObjectParameterivAPPLE v1 v2 v3 v4

{-# NOINLINE ptr_glGetObjectParameterivAPPLE #-}
ptr_glGetObjectParameterivAPPLE :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetObjectParameterivAPPLE = unsafePerformIO $ getCommand "glGetObjectParameterivAPPLE"

-- glGetObjectParameterivARB ---------------------------------------------------

glGetObjectParameterivARB
  :: MonadIO m
  => GLhandleARB -- ^ @obj@ of type @handleARB@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetObjectParameterivARB v1 v2 v3 = liftIO $ dyn390 ptr_glGetObjectParameterivARB v1 v2 v3

{-# NOINLINE ptr_glGetObjectParameterivARB #-}
ptr_glGetObjectParameterivARB :: FunPtr (GLhandleARB -> GLenum -> Ptr GLint -> IO ())
ptr_glGetObjectParameterivARB = unsafePerformIO $ getCommand "glGetObjectParameterivARB"

-- glGetObjectPtrLabel ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetObjectPtrLabel.xhtml OpenGL 4.x>.
glGetObjectPtrLabel
  :: MonadIO m
  => Ptr a -- ^ @ptr@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @label@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetObjectPtrLabel v1 v2 v3 v4 = liftIO $ dyn391 ptr_glGetObjectPtrLabel v1 v2 v3 v4

{-# NOINLINE ptr_glGetObjectPtrLabel #-}
ptr_glGetObjectPtrLabel :: FunPtr (Ptr a -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetObjectPtrLabel = unsafePerformIO $ getCommand "glGetObjectPtrLabel"

-- glGetObjectPtrLabelKHR ------------------------------------------------------

-- | This command is an alias for 'glGetObjectPtrLabel'.
glGetObjectPtrLabelKHR
  :: MonadIO m
  => Ptr a -- ^ @ptr@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @label@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetObjectPtrLabelKHR v1 v2 v3 v4 = liftIO $ dyn391 ptr_glGetObjectPtrLabelKHR v1 v2 v3 v4

{-# NOINLINE ptr_glGetObjectPtrLabelKHR #-}
ptr_glGetObjectPtrLabelKHR :: FunPtr (Ptr a -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetObjectPtrLabelKHR = unsafePerformIO $ getCommand "glGetObjectPtrLabelKHR"

-- glGetOcclusionQueryivNV -----------------------------------------------------

glGetOcclusionQueryivNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [OcclusionQueryParameterNameNV](Graphics-GL-Groups.html#OcclusionQueryParameterNameNV).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetOcclusionQueryivNV v1 v2 v3 = liftIO $ dyn348 ptr_glGetOcclusionQueryivNV v1 v2 v3

{-# NOINLINE ptr_glGetOcclusionQueryivNV #-}
ptr_glGetOcclusionQueryivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetOcclusionQueryivNV = unsafePerformIO $ getCommand "glGetOcclusionQueryivNV"

-- glGetOcclusionQueryuivNV ----------------------------------------------------

glGetOcclusionQueryuivNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [OcclusionQueryParameterNameNV](Graphics-GL-Groups.html#OcclusionQueryParameterNameNV).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetOcclusionQueryuivNV v1 v2 v3 = liftIO $ dyn392 ptr_glGetOcclusionQueryuivNV v1 v2 v3

{-# NOINLINE ptr_glGetOcclusionQueryuivNV #-}
ptr_glGetOcclusionQueryuivNV :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetOcclusionQueryuivNV = unsafePerformIO $ getCommand "glGetOcclusionQueryuivNV"

-- glGetPathColorGenfvNV -------------------------------------------------------

glGetPathColorGenfvNV
  :: MonadIO m
  => GLenum -- ^ @color@ of type [PathColor](Graphics-GL-Groups.html#PathColor).
  -> GLenum -- ^ @pname@ of type [PathGenMode](Graphics-GL-Groups.html#PathGenMode).
  -> Ptr GLfloat -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetPathColorGenfvNV v1 v2 v3 = liftIO $ dyn139 ptr_glGetPathColorGenfvNV v1 v2 v3

{-# NOINLINE ptr_glGetPathColorGenfvNV #-}
ptr_glGetPathColorGenfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPathColorGenfvNV = unsafePerformIO $ getCommand "glGetPathColorGenfvNV"

-- glGetPathColorGenivNV -------------------------------------------------------

glGetPathColorGenivNV
  :: MonadIO m
  => GLenum -- ^ @color@ of type [PathColor](Graphics-GL-Groups.html#PathColor).
  -> GLenum -- ^ @pname@ of type [PathGenMode](Graphics-GL-Groups.html#PathGenMode).
  -> Ptr GLint -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetPathColorGenivNV v1 v2 v3 = liftIO $ dyn140 ptr_glGetPathColorGenivNV v1 v2 v3

{-# NOINLINE ptr_glGetPathColorGenivNV #-}
ptr_glGetPathColorGenivNV :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetPathColorGenivNV = unsafePerformIO $ getCommand "glGetPathColorGenivNV"

-- glGetPathCommandsNV ---------------------------------------------------------

glGetPathCommandsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> Ptr GLubyte -- ^ @commands@ pointing to @COMPSIZE(path)@ elements of type @PathCommand@.
  -> m ()
glGetPathCommandsNV v1 v2 = liftIO $ dyn393 ptr_glGetPathCommandsNV v1 v2

{-# NOINLINE ptr_glGetPathCommandsNV #-}
ptr_glGetPathCommandsNV :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glGetPathCommandsNV = unsafePerformIO $ getCommand "glGetPathCommandsNV"

-- glGetPathCoordsNV -----------------------------------------------------------

glGetPathCoordsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> Ptr GLfloat -- ^ @coords@ pointing to @COMPSIZE(path)@ elements of type @GLfloat@.
  -> m ()
glGetPathCoordsNV v1 v2 = liftIO $ dyn394 ptr_glGetPathCoordsNV v1 v2

{-# NOINLINE ptr_glGetPathCoordsNV #-}
ptr_glGetPathCoordsNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glGetPathCoordsNV = unsafePerformIO $ getCommand "glGetPathCoordsNV"

-- glGetPathDashArrayNV --------------------------------------------------------

glGetPathDashArrayNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> Ptr GLfloat -- ^ @dashArray@ pointing to @COMPSIZE(path)@ elements of type @GLfloat@.
  -> m ()
glGetPathDashArrayNV v1 v2 = liftIO $ dyn394 ptr_glGetPathDashArrayNV v1 v2

{-# NOINLINE ptr_glGetPathDashArrayNV #-}
ptr_glGetPathDashArrayNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glGetPathDashArrayNV = unsafePerformIO $ getCommand "glGetPathDashArrayNV"

-- glGetPathLengthNV -----------------------------------------------------------

glGetPathLengthNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @startSegment@.
  -> GLsizei -- ^ @numSegments@.
  -> m GLfloat
glGetPathLengthNV v1 v2 v3 = liftIO $ dyn395 ptr_glGetPathLengthNV v1 v2 v3

{-# NOINLINE ptr_glGetPathLengthNV #-}
ptr_glGetPathLengthNV :: FunPtr (GLuint -> GLsizei -> GLsizei -> IO GLfloat)
ptr_glGetPathLengthNV = unsafePerformIO $ getCommand "glGetPathLengthNV"

-- glGetPathMetricRangeNV ------------------------------------------------------

glGetPathMetricRangeNV
  :: MonadIO m
  => GLbitfield -- ^ @metricQueryMask@ of type [PathMetricMask](Graphics-GL-Groups.html#PathMetricMask).
  -> GLuint -- ^ @firstPathName@ of type @Path@.
  -> GLsizei -- ^ @numPaths@.
  -> GLsizei -- ^ @stride@.
  -> Ptr GLfloat -- ^ @metrics@ pointing to @COMPSIZE(metricQueryMask,numPaths,stride)@ elements of type @GLfloat@.
  -> m ()
glGetPathMetricRangeNV v1 v2 v3 v4 v5 = liftIO $ dyn396 ptr_glGetPathMetricRangeNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetPathMetricRangeNV #-}
ptr_glGetPathMetricRangeNV :: FunPtr (GLbitfield -> GLuint -> GLsizei -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetPathMetricRangeNV = unsafePerformIO $ getCommand "glGetPathMetricRangeNV"

-- glGetPathMetricsNV ----------------------------------------------------------

glGetPathMetricsNV
  :: MonadIO m
  => GLbitfield -- ^ @metricQueryMask@ of type [PathMetricMask](Graphics-GL-Groups.html#PathMetricMask).
  -> GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@ of type [PathElementType](Graphics-GL-Groups.html#PathElementType).
  -> Ptr a -- ^ @paths@ pointing to @COMPSIZE(numPaths,pathNameType,paths)@ elements of type @PathElement@.
  -> GLuint -- ^ @pathBase@ of type @Path@.
  -> GLsizei -- ^ @stride@.
  -> Ptr GLfloat -- ^ @metrics@ pointing to @COMPSIZE(metricQueryMask,numPaths,stride)@ elements of type @GLfloat@.
  -> m ()
glGetPathMetricsNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn397 ptr_glGetPathMetricsNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetPathMetricsNV #-}
ptr_glGetPathMetricsNV :: FunPtr (GLbitfield -> GLsizei -> GLenum -> Ptr a -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetPathMetricsNV = unsafePerformIO $ getCommand "glGetPathMetricsNV"

-- glGetPathParameterfvNV ------------------------------------------------------

glGetPathParameterfvNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type [PathParameter](Graphics-GL-Groups.html#PathParameter).
  -> Ptr GLfloat -- ^ @value@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetPathParameterfvNV v1 v2 v3 = liftIO $ dyn364 ptr_glGetPathParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glGetPathParameterfvNV #-}
ptr_glGetPathParameterfvNV :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPathParameterfvNV = unsafePerformIO $ getCommand "glGetPathParameterfvNV"

-- glGetPathParameterivNV ------------------------------------------------------

glGetPathParameterivNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type [PathParameter](Graphics-GL-Groups.html#PathParameter).
  -> Ptr GLint -- ^ @value@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetPathParameterivNV v1 v2 v3 = liftIO $ dyn348 ptr_glGetPathParameterivNV v1 v2 v3

{-# NOINLINE ptr_glGetPathParameterivNV #-}
ptr_glGetPathParameterivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetPathParameterivNV = unsafePerformIO $ getCommand "glGetPathParameterivNV"

-- glGetPathSpacingNV ----------------------------------------------------------

glGetPathSpacingNV
  :: MonadIO m
  => GLenum -- ^ @pathListMode@ of type [PathListMode](Graphics-GL-Groups.html#PathListMode).
  -> GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@ of type [PathElementType](Graphics-GL-Groups.html#PathElementType).
  -> Ptr a -- ^ @paths@ pointing to @COMPSIZE(numPaths,pathNameType,paths)@ elements of type @PathElement@.
  -> GLuint -- ^ @pathBase@ of type @Path@.
  -> GLfloat -- ^ @advanceScale@.
  -> GLfloat -- ^ @kerningScale@.
  -> GLenum -- ^ @transformType@ of type [PathTransformType](Graphics-GL-Groups.html#PathTransformType).
  -> Ptr GLfloat -- ^ @returnedSpacing@ pointing to @COMPSIZE(pathListMode,numPaths)@ elements of type @GLfloat@.
  -> m ()
glGetPathSpacingNV v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn398 ptr_glGetPathSpacingNV v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glGetPathSpacingNV #-}
ptr_glGetPathSpacingNV :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLuint -> GLfloat -> GLfloat -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPathSpacingNV = unsafePerformIO $ getCommand "glGetPathSpacingNV"

-- glGetPathTexGenfvNV ---------------------------------------------------------

glGetPathTexGenfvNV
  :: MonadIO m
  => GLenum -- ^ @texCoordSet@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @pname@ of type [PathGenMode](Graphics-GL-Groups.html#PathGenMode).
  -> Ptr GLfloat -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetPathTexGenfvNV v1 v2 v3 = liftIO $ dyn139 ptr_glGetPathTexGenfvNV v1 v2 v3

{-# NOINLINE ptr_glGetPathTexGenfvNV #-}
ptr_glGetPathTexGenfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPathTexGenfvNV = unsafePerformIO $ getCommand "glGetPathTexGenfvNV"

-- glGetPathTexGenivNV ---------------------------------------------------------

glGetPathTexGenivNV
  :: MonadIO m
  => GLenum -- ^ @texCoordSet@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @pname@ of type [PathGenMode](Graphics-GL-Groups.html#PathGenMode).
  -> Ptr GLint -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetPathTexGenivNV v1 v2 v3 = liftIO $ dyn140 ptr_glGetPathTexGenivNV v1 v2 v3

{-# NOINLINE ptr_glGetPathTexGenivNV #-}
ptr_glGetPathTexGenivNV :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetPathTexGenivNV = unsafePerformIO $ getCommand "glGetPathTexGenivNV"

-- glGetPerfCounterInfoINTEL ---------------------------------------------------

glGetPerfCounterInfoINTEL
  :: MonadIO m
  => GLuint -- ^ @queryId@.
  -> GLuint -- ^ @counterId@.
  -> GLuint -- ^ @counterNameLength@.
  -> Ptr GLchar -- ^ @counterName@.
  -> GLuint -- ^ @counterDescLength@.
  -> Ptr GLchar -- ^ @counterDesc@.
  -> Ptr GLuint -- ^ @counterOffset@.
  -> Ptr GLuint -- ^ @counterDataSize@.
  -> Ptr GLuint -- ^ @counterTypeEnum@.
  -> Ptr GLuint -- ^ @counterDataTypeEnum@.
  -> Ptr GLuint64 -- ^ @rawCounterMaxValue@.
  -> m ()
glGetPerfCounterInfoINTEL v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn399 ptr_glGetPerfCounterInfoINTEL v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glGetPerfCounterInfoINTEL #-}
ptr_glGetPerfCounterInfoINTEL :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint64 -> IO ())
ptr_glGetPerfCounterInfoINTEL = unsafePerformIO $ getCommand "glGetPerfCounterInfoINTEL"

-- glGetPerfMonitorCounterDataAMD ----------------------------------------------

glGetPerfMonitorCounterDataAMD
  :: MonadIO m
  => GLuint -- ^ @monitor@.
  -> GLenum -- ^ @pname@.
  -> GLsizei -- ^ @dataSize@.
  -> Ptr GLuint -- ^ @data@ pointing to @dataSize@ elements of type @GLuint@.
  -> Ptr GLint -- ^ @bytesWritten@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetPerfMonitorCounterDataAMD v1 v2 v3 v4 v5 = liftIO $ dyn400 ptr_glGetPerfMonitorCounterDataAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetPerfMonitorCounterDataAMD #-}
ptr_glGetPerfMonitorCounterDataAMD :: FunPtr (GLuint -> GLenum -> GLsizei -> Ptr GLuint -> Ptr GLint -> IO ())
ptr_glGetPerfMonitorCounterDataAMD = unsafePerformIO $ getCommand "glGetPerfMonitorCounterDataAMD"

-- glGetPerfMonitorCounterInfoAMD ----------------------------------------------

glGetPerfMonitorCounterInfoAMD
  :: MonadIO m
  => GLuint -- ^ @group@.
  -> GLuint -- ^ @counter@.
  -> GLenum -- ^ @pname@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(pname)@ elements of type @a@.
  -> m ()
glGetPerfMonitorCounterInfoAMD v1 v2 v3 v4 = liftIO $ dyn401 ptr_glGetPerfMonitorCounterInfoAMD v1 v2 v3 v4

{-# NOINLINE ptr_glGetPerfMonitorCounterInfoAMD #-}
ptr_glGetPerfMonitorCounterInfoAMD :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr a -> IO ())
ptr_glGetPerfMonitorCounterInfoAMD = unsafePerformIO $ getCommand "glGetPerfMonitorCounterInfoAMD"

-- glGetPerfMonitorCounterStringAMD --------------------------------------------

glGetPerfMonitorCounterStringAMD
  :: MonadIO m
  => GLuint -- ^ @group@.
  -> GLuint -- ^ @counter@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @counterString@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetPerfMonitorCounterStringAMD v1 v2 v3 v4 v5 = liftIO $ dyn319 ptr_glGetPerfMonitorCounterStringAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetPerfMonitorCounterStringAMD #-}
ptr_glGetPerfMonitorCounterStringAMD :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetPerfMonitorCounterStringAMD = unsafePerformIO $ getCommand "glGetPerfMonitorCounterStringAMD"

-- glGetPerfMonitorCountersAMD -------------------------------------------------

glGetPerfMonitorCountersAMD
  :: MonadIO m
  => GLuint -- ^ @group@.
  -> Ptr GLint -- ^ @numCounters@ pointing to @1@ element of type @GLint@.
  -> Ptr GLint -- ^ @maxActiveCounters@ pointing to @1@ element of type @GLint@.
  -> GLsizei -- ^ @counterSize@.
  -> Ptr GLuint -- ^ @counters@ pointing to @counterSize@ elements of type @GLuint@.
  -> m ()
glGetPerfMonitorCountersAMD v1 v2 v3 v4 v5 = liftIO $ dyn402 ptr_glGetPerfMonitorCountersAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetPerfMonitorCountersAMD #-}
ptr_glGetPerfMonitorCountersAMD :: FunPtr (GLuint -> Ptr GLint -> Ptr GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glGetPerfMonitorCountersAMD = unsafePerformIO $ getCommand "glGetPerfMonitorCountersAMD"

-- glGetPerfMonitorGroupStringAMD ----------------------------------------------

glGetPerfMonitorGroupStringAMD
  :: MonadIO m
  => GLuint -- ^ @group@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @groupString@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetPerfMonitorGroupStringAMD v1 v2 v3 v4 = liftIO $ dyn345 ptr_glGetPerfMonitorGroupStringAMD v1 v2 v3 v4

{-# NOINLINE ptr_glGetPerfMonitorGroupStringAMD #-}
ptr_glGetPerfMonitorGroupStringAMD :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetPerfMonitorGroupStringAMD = unsafePerformIO $ getCommand "glGetPerfMonitorGroupStringAMD"

-- glGetPerfMonitorGroupsAMD ---------------------------------------------------

glGetPerfMonitorGroupsAMD
  :: MonadIO m
  => Ptr GLint -- ^ @numGroups@ pointing to @1@ element of type @GLint@.
  -> GLsizei -- ^ @groupsSize@.
  -> Ptr GLuint -- ^ @groups@ pointing to @groupsSize@ elements of type @GLuint@.
  -> m ()
glGetPerfMonitorGroupsAMD v1 v2 v3 = liftIO $ dyn346 ptr_glGetPerfMonitorGroupsAMD v1 v2 v3

{-# NOINLINE ptr_glGetPerfMonitorGroupsAMD #-}
ptr_glGetPerfMonitorGroupsAMD :: FunPtr (Ptr GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glGetPerfMonitorGroupsAMD = unsafePerformIO $ getCommand "glGetPerfMonitorGroupsAMD"

-- glGetPerfQueryDataINTEL -----------------------------------------------------

glGetPerfQueryDataINTEL
  :: MonadIO m
  => GLuint -- ^ @queryHandle@.
  -> GLuint -- ^ @flags@.
  -> GLsizei -- ^ @dataSize@.
  -> Ptr a -- ^ @data@.
  -> Ptr GLuint -- ^ @bytesWritten@.
  -> m ()
glGetPerfQueryDataINTEL v1 v2 v3 v4 v5 = liftIO $ dyn403 ptr_glGetPerfQueryDataINTEL v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetPerfQueryDataINTEL #-}
ptr_glGetPerfQueryDataINTEL :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr a -> Ptr GLuint -> IO ())
ptr_glGetPerfQueryDataINTEL = unsafePerformIO $ getCommand "glGetPerfQueryDataINTEL"

-- glGetPerfQueryIdByNameINTEL -------------------------------------------------

glGetPerfQueryIdByNameINTEL
  :: MonadIO m
  => Ptr GLchar -- ^ @queryName@.
  -> Ptr GLuint -- ^ @queryId@.
  -> m ()
glGetPerfQueryIdByNameINTEL v1 v2 = liftIO $ dyn404 ptr_glGetPerfQueryIdByNameINTEL v1 v2

{-# NOINLINE ptr_glGetPerfQueryIdByNameINTEL #-}
ptr_glGetPerfQueryIdByNameINTEL :: FunPtr (Ptr GLchar -> Ptr GLuint -> IO ())
ptr_glGetPerfQueryIdByNameINTEL = unsafePerformIO $ getCommand "glGetPerfQueryIdByNameINTEL"

-- glGetPerfQueryInfoINTEL -----------------------------------------------------

glGetPerfQueryInfoINTEL
  :: MonadIO m
  => GLuint -- ^ @queryId@.
  -> GLuint -- ^ @queryNameLength@.
  -> Ptr GLchar -- ^ @queryName@.
  -> Ptr GLuint -- ^ @dataSize@.
  -> Ptr GLuint -- ^ @noCounters@.
  -> Ptr GLuint -- ^ @noInstances@.
  -> Ptr GLuint -- ^ @capsMask@.
  -> m ()
glGetPerfQueryInfoINTEL v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn405 ptr_glGetPerfQueryInfoINTEL v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetPerfQueryInfoINTEL #-}
ptr_glGetPerfQueryInfoINTEL :: FunPtr (GLuint -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> IO ())
ptr_glGetPerfQueryInfoINTEL = unsafePerformIO $ getCommand "glGetPerfQueryInfoINTEL"

-- glGetPixelMapfv -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml OpenGL 2.x>.
glGetPixelMapfv
  :: MonadIO m
  => GLenum -- ^ @map@ of type [PixelMap](Graphics-GL-Groups.html#PixelMap).
  -> Ptr GLfloat -- ^ @values@ pointing to @COMPSIZE(map)@ elements of type @GLfloat@.
  -> m ()
glGetPixelMapfv v1 v2 = liftIO $ dyn101 ptr_glGetPixelMapfv v1 v2

{-# NOINLINE ptr_glGetPixelMapfv #-}
ptr_glGetPixelMapfv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPixelMapfv = unsafePerformIO $ getCommand "glGetPixelMapfv"

-- glGetPixelMapuiv ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml OpenGL 2.x>.
glGetPixelMapuiv
  :: MonadIO m
  => GLenum -- ^ @map@ of type [PixelMap](Graphics-GL-Groups.html#PixelMap).
  -> Ptr GLuint -- ^ @values@ pointing to @COMPSIZE(map)@ elements of type @GLuint@.
  -> m ()
glGetPixelMapuiv v1 v2 = liftIO $ dyn132 ptr_glGetPixelMapuiv v1 v2

{-# NOINLINE ptr_glGetPixelMapuiv #-}
ptr_glGetPixelMapuiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glGetPixelMapuiv = unsafePerformIO $ getCommand "glGetPixelMapuiv"

-- glGetPixelMapusv ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml OpenGL 2.x>.
glGetPixelMapusv
  :: MonadIO m
  => GLenum -- ^ @map@ of type [PixelMap](Graphics-GL-Groups.html#PixelMap).
  -> Ptr GLushort -- ^ @values@ pointing to @COMPSIZE(map)@ elements of type @GLushort@.
  -> m ()
glGetPixelMapusv v1 v2 = liftIO $ dyn406 ptr_glGetPixelMapusv v1 v2

{-# NOINLINE ptr_glGetPixelMapusv #-}
ptr_glGetPixelMapusv :: FunPtr (GLenum -> Ptr GLushort -> IO ())
ptr_glGetPixelMapusv = unsafePerformIO $ getCommand "glGetPixelMapusv"

-- glGetPixelMapxv -------------------------------------------------------------

glGetPixelMapxv
  :: MonadIO m
  => GLenum -- ^ @map@ of type [PixelMap](Graphics-GL-Groups.html#PixelMap).
  -> GLint -- ^ @size@.
  -> Ptr GLfixed -- ^ @values@ pointing to @size@ elements of type @GLfixed@.
  -> m ()
glGetPixelMapxv v1 v2 v3 = liftIO $ dyn407 ptr_glGetPixelMapxv v1 v2 v3

{-# NOINLINE ptr_glGetPixelMapxv #-}
ptr_glGetPixelMapxv :: FunPtr (GLenum -> GLint -> Ptr GLfixed -> IO ())
ptr_glGetPixelMapxv = unsafePerformIO $ getCommand "glGetPixelMapxv"

-- glGetPixelTexGenParameterfvSGIS ---------------------------------------------

glGetPixelTexGenParameterfvSGIS
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelTexGenParameterNameSGIS](Graphics-GL-Groups.html#PixelTexGenParameterNameSGIS).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glGetPixelTexGenParameterfvSGIS v1 v2 = liftIO $ dyn101 ptr_glGetPixelTexGenParameterfvSGIS v1 v2

{-# NOINLINE ptr_glGetPixelTexGenParameterfvSGIS #-}
ptr_glGetPixelTexGenParameterfvSGIS :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPixelTexGenParameterfvSGIS = unsafePerformIO $ getCommand "glGetPixelTexGenParameterfvSGIS"

-- glGetPixelTexGenParameterivSGIS ---------------------------------------------

glGetPixelTexGenParameterivSGIS
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelTexGenParameterNameSGIS](Graphics-GL-Groups.html#PixelTexGenParameterNameSGIS).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glGetPixelTexGenParameterivSGIS v1 v2 = liftIO $ dyn143 ptr_glGetPixelTexGenParameterivSGIS v1 v2

{-# NOINLINE ptr_glGetPixelTexGenParameterivSGIS #-}
ptr_glGetPixelTexGenParameterivSGIS :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glGetPixelTexGenParameterivSGIS = unsafePerformIO $ getCommand "glGetPixelTexGenParameterivSGIS"

-- glGetPixelTransformParameterfvEXT -------------------------------------------

glGetPixelTransformParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetPixelTransformParameterfvEXT v1 v2 v3 = liftIO $ dyn139 ptr_glGetPixelTransformParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glGetPixelTransformParameterfvEXT #-}
ptr_glGetPixelTransformParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPixelTransformParameterfvEXT = unsafePerformIO $ getCommand "glGetPixelTransformParameterfvEXT"

-- glGetPixelTransformParameterivEXT -------------------------------------------

glGetPixelTransformParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetPixelTransformParameterivEXT v1 v2 v3 = liftIO $ dyn140 ptr_glGetPixelTransformParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetPixelTransformParameterivEXT #-}
ptr_glGetPixelTransformParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetPixelTransformParameterivEXT = unsafePerformIO $ getCommand "glGetPixelTransformParameterivEXT"

-- glGetPointerIndexedvEXT -----------------------------------------------------

glGetPointerIndexedvEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> Ptr (Ptr a) -- ^ @data@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetPointerIndexedvEXT v1 v2 v3 = liftIO $ dyn408 ptr_glGetPointerIndexedvEXT v1 v2 v3

{-# NOINLINE ptr_glGetPointerIndexedvEXT #-}
ptr_glGetPointerIndexedvEXT :: FunPtr (GLenum -> GLuint -> Ptr (Ptr a) -> IO ())
ptr_glGetPointerIndexedvEXT = unsafePerformIO $ getCommand "glGetPointerIndexedvEXT"

-- glGetPointeri_vEXT ----------------------------------------------------------

glGetPointeri_vEXT
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLuint -- ^ @index@.
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetPointeri_vEXT v1 v2 v3 = liftIO $ dyn408 ptr_glGetPointeri_vEXT v1 v2 v3

{-# NOINLINE ptr_glGetPointeri_vEXT #-}
ptr_glGetPointeri_vEXT :: FunPtr (GLenum -> GLuint -> Ptr (Ptr a) -> IO ())
ptr_glGetPointeri_vEXT = unsafePerformIO $ getCommand "glGetPointeri_vEXT"

-- glGetPointerv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetPointerv.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetPointerv.xhtml OpenGL 4.x>.
glGetPointerv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetPointervPName](Graphics-GL-Groups.html#GetPointervPName).
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetPointerv v1 v2 = liftIO $ dyn279 ptr_glGetPointerv v1 v2

{-# NOINLINE ptr_glGetPointerv #-}
ptr_glGetPointerv :: FunPtr (GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetPointerv = unsafePerformIO $ getCommand "glGetPointerv"

-- glGetPointervEXT ------------------------------------------------------------

-- | This command is an alias for 'glGetPointerv'.
glGetPointervEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetPointervPName](Graphics-GL-Groups.html#GetPointervPName).
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetPointervEXT v1 v2 = liftIO $ dyn279 ptr_glGetPointervEXT v1 v2

{-# NOINLINE ptr_glGetPointervEXT #-}
ptr_glGetPointervEXT :: FunPtr (GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetPointervEXT = unsafePerformIO $ getCommand "glGetPointervEXT"

-- glGetPointervKHR ------------------------------------------------------------

-- | This command is an alias for 'glGetPointerv'.
glGetPointervKHR
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr (Ptr a) -- ^ @params@.
  -> m ()
glGetPointervKHR v1 v2 = liftIO $ dyn279 ptr_glGetPointervKHR v1 v2

{-# NOINLINE ptr_glGetPointervKHR #-}
ptr_glGetPointervKHR :: FunPtr (GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetPointervKHR = unsafePerformIO $ getCommand "glGetPointervKHR"

-- glGetPolygonStipple ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetPolygonStipple.xml OpenGL 2.x>.
glGetPolygonStipple
  :: MonadIO m
  => Ptr GLubyte -- ^ @mask@ pointing to @COMPSIZE()@ elements of type @GLubyte@.
  -> m ()
glGetPolygonStipple v1 = liftIO $ dyn108 ptr_glGetPolygonStipple v1

{-# NOINLINE ptr_glGetPolygonStipple #-}
ptr_glGetPolygonStipple :: FunPtr (Ptr GLubyte -> IO ())
ptr_glGetPolygonStipple = unsafePerformIO $ getCommand "glGetPolygonStipple"

-- glGetProgramBinary ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramBinary.xhtml OpenGL 4.x>.
glGetProgramBinary
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLenum -- ^ @binaryFormat@ pointing to @1@ element of type @GLenum@.
  -> Ptr a -- ^ @binary@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glGetProgramBinary v1 v2 v3 v4 v5 = liftIO $ dyn409 ptr_glGetProgramBinary v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetProgramBinary #-}
ptr_glGetProgramBinary :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr a -> IO ())
ptr_glGetProgramBinary = unsafePerformIO $ getCommand "glGetProgramBinary"

-- glGetProgramBinaryOES -------------------------------------------------------

-- | This command is an alias for 'glGetProgramBinary'.
glGetProgramBinaryOES
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLenum -- ^ @binaryFormat@ pointing to @1@ element of type @GLenum@.
  -> Ptr a -- ^ @binary@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glGetProgramBinaryOES v1 v2 v3 v4 v5 = liftIO $ dyn409 ptr_glGetProgramBinaryOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetProgramBinaryOES #-}
ptr_glGetProgramBinaryOES :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr a -> IO ())
ptr_glGetProgramBinaryOES = unsafePerformIO $ getCommand "glGetProgramBinaryOES"

