{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F01
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

module Graphics.GL.Functions.F01 (
  glAccum,
  glAccumxOES,
  glAcquireKeyedMutexWin32EXT,
  glActiveProgramEXT,
  glActiveShaderProgram,
  glActiveShaderProgramEXT,
  glActiveStencilFaceEXT,
  glActiveTexture,
  glActiveTextureARB,
  glActiveVaryingNV,
  glAlphaFragmentOp1ATI,
  glAlphaFragmentOp2ATI,
  glAlphaFragmentOp3ATI,
  glAlphaFunc,
  glAlphaFuncQCOM,
  glAlphaFuncx,
  glAlphaFuncxOES,
  glAlphaToCoverageDitherControlNV,
  glApplyFramebufferAttachmentCMAAINTEL,
  glApplyTextureEXT,
  glAreProgramsResidentNV,
  glAreTexturesResident,
  glAreTexturesResidentEXT,
  glArrayElement,
  glArrayElementEXT,
  glArrayObjectATI,
  glAsyncCopyBufferSubDataNVX,
  glAsyncCopyImageSubDataNVX,
  glAsyncMarkerSGIX,
  glAttachObjectARB,
  glAttachShader,
  glBegin,
  glBeginConditionalRender,
  glBeginConditionalRenderNV,
  glBeginConditionalRenderNVX,
  glBeginFragmentShaderATI,
  glBeginOcclusionQueryNV,
  glBeginPerfMonitorAMD,
  glBeginPerfQueryINTEL,
  glBeginQuery,
  glBeginQueryARB,
  glBeginQueryEXT,
  glBeginQueryIndexed,
  glBeginTransformFeedback,
  glBeginTransformFeedbackEXT,
  glBeginTransformFeedbackNV,
  glBeginVertexShaderEXT,
  glBeginVideoCaptureNV,
  glBindAttribLocation,
  glBindAttribLocationARB,
  glBindBuffer,
  glBindBufferARB,
  glBindBufferBase,
  glBindBufferBaseEXT,
  glBindBufferBaseNV,
  glBindBufferOffsetEXT,
  glBindBufferOffsetNV,
  glBindBufferRange,
  glBindBufferRangeEXT,
  glBindBufferRangeNV,
  glBindBuffersBase,
  glBindBuffersRange,
  glBindFragDataLocation,
  glBindFragDataLocationEXT,
  glBindFragDataLocationIndexed,
  glBindFragDataLocationIndexedEXT,
  glBindFragmentShaderATI,
  glBindFramebuffer,
  glBindFramebufferEXT,
  glBindFramebufferOES,
  glBindImageTexture,
  glBindImageTextureEXT,
  glBindImageTextures,
  glBindLightParameterEXT,
  glBindMaterialParameterEXT,
  glBindMultiTextureEXT,
  glBindParameterEXT,
  glBindProgramARB,
  glBindProgramNV,
  glBindProgramPipeline,
  glBindProgramPipelineEXT,
  glBindRenderbuffer,
  glBindRenderbufferEXT,
  glBindRenderbufferOES,
  glBindSampler,
  glBindSamplers,
  glBindShadingRateImageNV,
  glBindTexGenParameterEXT,
  glBindTexture,
  glBindTextureEXT,
  glBindTextureUnit,
  glBindTextureUnitParameterEXT,
  glBindTextures,
  glBindTransformFeedback,
  glBindTransformFeedbackNV,
  glBindVertexArray,
  glBindVertexArrayAPPLE,
  glBindVertexArrayOES,
  glBindVertexBuffer,
  glBindVertexBuffers
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glAccum ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glAccum.xml OpenGL 2.x>.
glAccum
  :: MonadIO m
  => GLenum -- ^ @op@ of type [AccumOp](Graphics-GL-Groups.html#AccumOp).
  -> GLfloat -- ^ @value@ of type @CoordF@.
  -> m ()
glAccum v1 v2 = liftIO $ dyn0 ptr_glAccum v1 v2

{-# NOINLINE ptr_glAccum #-}
ptr_glAccum :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glAccum = unsafePerformIO $ getCommand "glAccum"

-- glAccumxOES -----------------------------------------------------------------

glAccumxOES
  :: MonadIO m
  => GLenum -- ^ @op@.
  -> GLfixed -- ^ @value@.
  -> m ()
glAccumxOES v1 v2 = liftIO $ dyn1 ptr_glAccumxOES v1 v2

{-# NOINLINE ptr_glAccumxOES #-}
ptr_glAccumxOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glAccumxOES = unsafePerformIO $ getCommand "glAccumxOES"

-- glAcquireKeyedMutexWin32EXT -------------------------------------------------

glAcquireKeyedMutexWin32EXT
  :: MonadIO m
  => GLuint -- ^ @memory@.
  -> GLuint64 -- ^ @key@.
  -> GLuint -- ^ @timeout@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glAcquireKeyedMutexWin32EXT v1 v2 v3 = liftIO $ dyn2 ptr_glAcquireKeyedMutexWin32EXT v1 v2 v3

{-# NOINLINE ptr_glAcquireKeyedMutexWin32EXT #-}
ptr_glAcquireKeyedMutexWin32EXT :: FunPtr (GLuint -> GLuint64 -> GLuint -> IO GLboolean)
ptr_glAcquireKeyedMutexWin32EXT = unsafePerformIO $ getCommand "glAcquireKeyedMutexWin32EXT"

-- glActiveProgramEXT ----------------------------------------------------------

glActiveProgramEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m ()
glActiveProgramEXT v1 = liftIO $ dyn3 ptr_glActiveProgramEXT v1

{-# NOINLINE ptr_glActiveProgramEXT #-}
ptr_glActiveProgramEXT :: FunPtr (GLuint -> IO ())
ptr_glActiveProgramEXT = unsafePerformIO $ getCommand "glActiveProgramEXT"

-- glActiveShaderProgram -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glActiveShaderProgram.xhtml OpenGL 4.x>.
glActiveShaderProgram
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLuint -- ^ @program@.
  -> m ()
glActiveShaderProgram v1 v2 = liftIO $ dyn4 ptr_glActiveShaderProgram v1 v2

{-# NOINLINE ptr_glActiveShaderProgram #-}
ptr_glActiveShaderProgram :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glActiveShaderProgram = unsafePerformIO $ getCommand "glActiveShaderProgram"

-- glActiveShaderProgramEXT ----------------------------------------------------

glActiveShaderProgramEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLuint -- ^ @program@.
  -> m ()
glActiveShaderProgramEXT v1 v2 = liftIO $ dyn4 ptr_glActiveShaderProgramEXT v1 v2

{-# NOINLINE ptr_glActiveShaderProgramEXT #-}
ptr_glActiveShaderProgramEXT :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glActiveShaderProgramEXT = unsafePerformIO $ getCommand "glActiveShaderProgramEXT"

-- glActiveStencilFaceEXT ------------------------------------------------------

glActiveStencilFaceEXT
  :: MonadIO m
  => GLenum -- ^ @face@ of type [StencilFaceDirection](Graphics-GL-Groups.html#StencilFaceDirection).
  -> m ()
glActiveStencilFaceEXT v1 = liftIO $ dyn5 ptr_glActiveStencilFaceEXT v1

{-# NOINLINE ptr_glActiveStencilFaceEXT #-}
ptr_glActiveStencilFaceEXT :: FunPtr (GLenum -> IO ())
ptr_glActiveStencilFaceEXT = unsafePerformIO $ getCommand "glActiveStencilFaceEXT"

-- glActiveTexture -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glActiveTexture.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glActiveTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glActiveTexture.xhtml OpenGL 4.x>.
glActiveTexture
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> m ()
glActiveTexture v1 = liftIO $ dyn5 ptr_glActiveTexture v1

{-# NOINLINE ptr_glActiveTexture #-}
ptr_glActiveTexture :: FunPtr (GLenum -> IO ())
ptr_glActiveTexture = unsafePerformIO $ getCommand "glActiveTexture"

-- glActiveTextureARB ----------------------------------------------------------

-- | This command is an alias for 'glActiveTexture'.
glActiveTextureARB
  :: MonadIO m
  => GLenum -- ^ @texture@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> m ()
glActiveTextureARB v1 = liftIO $ dyn5 ptr_glActiveTextureARB v1

{-# NOINLINE ptr_glActiveTextureARB #-}
ptr_glActiveTextureARB :: FunPtr (GLenum -> IO ())
ptr_glActiveTextureARB = unsafePerformIO $ getCommand "glActiveTextureARB"

-- glActiveVaryingNV -----------------------------------------------------------

glActiveVaryingNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m ()
glActiveVaryingNV v1 v2 = liftIO $ dyn6 ptr_glActiveVaryingNV v1 v2

{-# NOINLINE ptr_glActiveVaryingNV #-}
ptr_glActiveVaryingNV :: FunPtr (GLuint -> Ptr GLchar -> IO ())
ptr_glActiveVaryingNV = unsafePerformIO $ getCommand "glActiveVaryingNV"

-- glAlphaFragmentOp1ATI -------------------------------------------------------

glAlphaFragmentOp1ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type [FragmentOpATI](Graphics-GL-Groups.html#FragmentOpATI).
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> m ()
glAlphaFragmentOp1ATI v1 v2 v3 v4 v5 v6 = liftIO $ dyn7 ptr_glAlphaFragmentOp1ATI v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glAlphaFragmentOp1ATI #-}
ptr_glAlphaFragmentOp1ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glAlphaFragmentOp1ATI = unsafePerformIO $ getCommand "glAlphaFragmentOp1ATI"

-- glAlphaFragmentOp2ATI -------------------------------------------------------

glAlphaFragmentOp2ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type [FragmentOpATI](Graphics-GL-Groups.html#FragmentOpATI).
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> GLuint -- ^ @arg2@.
  -> GLuint -- ^ @arg2Rep@.
  -> GLuint -- ^ @arg2Mod@.
  -> m ()
glAlphaFragmentOp2ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn8 ptr_glAlphaFragmentOp2ATI v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glAlphaFragmentOp2ATI #-}
ptr_glAlphaFragmentOp2ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glAlphaFragmentOp2ATI = unsafePerformIO $ getCommand "glAlphaFragmentOp2ATI"

-- glAlphaFragmentOp3ATI -------------------------------------------------------

glAlphaFragmentOp3ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type [FragmentOpATI](Graphics-GL-Groups.html#FragmentOpATI).
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> GLuint -- ^ @arg2@.
  -> GLuint -- ^ @arg2Rep@.
  -> GLuint -- ^ @arg2Mod@.
  -> GLuint -- ^ @arg3@.
  -> GLuint -- ^ @arg3Rep@.
  -> GLuint -- ^ @arg3Mod@.
  -> m ()
glAlphaFragmentOp3ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn9 ptr_glAlphaFragmentOp3ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glAlphaFragmentOp3ATI #-}
ptr_glAlphaFragmentOp3ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glAlphaFragmentOp3ATI = unsafePerformIO $ getCommand "glAlphaFragmentOp3ATI"

-- glAlphaFunc -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glAlphaFunc.xml OpenGL 2.x>.
glAlphaFunc
  :: MonadIO m
  => GLenum -- ^ @func@ of type [AlphaFunction](Graphics-GL-Groups.html#AlphaFunction).
  -> GLfloat -- ^ @ref@.
  -> m ()
glAlphaFunc v1 v2 = liftIO $ dyn0 ptr_glAlphaFunc v1 v2

{-# NOINLINE ptr_glAlphaFunc #-}
ptr_glAlphaFunc :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glAlphaFunc = unsafePerformIO $ getCommand "glAlphaFunc"

-- glAlphaFuncQCOM -------------------------------------------------------------

glAlphaFuncQCOM
  :: MonadIO m
  => GLenum -- ^ @func@.
  -> GLclampf -- ^ @ref@.
  -> m ()
glAlphaFuncQCOM v1 v2 = liftIO $ dyn10 ptr_glAlphaFuncQCOM v1 v2

{-# NOINLINE ptr_glAlphaFuncQCOM #-}
ptr_glAlphaFuncQCOM :: FunPtr (GLenum -> GLclampf -> IO ())
ptr_glAlphaFuncQCOM = unsafePerformIO $ getCommand "glAlphaFuncQCOM"

-- glAlphaFuncx ----------------------------------------------------------------

glAlphaFuncx
  :: MonadIO m
  => GLenum -- ^ @func@ of type [AlphaFunction](Graphics-GL-Groups.html#AlphaFunction).
  -> GLfixed -- ^ @ref@.
  -> m ()
glAlphaFuncx v1 v2 = liftIO $ dyn1 ptr_glAlphaFuncx v1 v2

{-# NOINLINE ptr_glAlphaFuncx #-}
ptr_glAlphaFuncx :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glAlphaFuncx = unsafePerformIO $ getCommand "glAlphaFuncx"

-- glAlphaFuncxOES -------------------------------------------------------------

glAlphaFuncxOES
  :: MonadIO m
  => GLenum -- ^ @func@ of type [AlphaFunction](Graphics-GL-Groups.html#AlphaFunction).
  -> GLfixed -- ^ @ref@ of type @ClampedFixed@.
  -> m ()
glAlphaFuncxOES v1 v2 = liftIO $ dyn1 ptr_glAlphaFuncxOES v1 v2

{-# NOINLINE ptr_glAlphaFuncxOES #-}
ptr_glAlphaFuncxOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glAlphaFuncxOES = unsafePerformIO $ getCommand "glAlphaFuncxOES"

-- glAlphaToCoverageDitherControlNV --------------------------------------------

glAlphaToCoverageDitherControlNV
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> m ()
glAlphaToCoverageDitherControlNV v1 = liftIO $ dyn5 ptr_glAlphaToCoverageDitherControlNV v1

{-# NOINLINE ptr_glAlphaToCoverageDitherControlNV #-}
ptr_glAlphaToCoverageDitherControlNV :: FunPtr (GLenum -> IO ())
ptr_glAlphaToCoverageDitherControlNV = unsafePerformIO $ getCommand "glAlphaToCoverageDitherControlNV"

-- glApplyFramebufferAttachmentCMAAINTEL ---------------------------------------

glApplyFramebufferAttachmentCMAAINTEL
  :: MonadIO m
  => m ()
glApplyFramebufferAttachmentCMAAINTEL = liftIO $ dyn11 ptr_glApplyFramebufferAttachmentCMAAINTEL

{-# NOINLINE ptr_glApplyFramebufferAttachmentCMAAINTEL #-}
ptr_glApplyFramebufferAttachmentCMAAINTEL :: FunPtr (IO ())
ptr_glApplyFramebufferAttachmentCMAAINTEL = unsafePerformIO $ getCommand "glApplyFramebufferAttachmentCMAAINTEL"

-- glApplyTextureEXT -----------------------------------------------------------

glApplyTextureEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [LightTextureModeEXT](Graphics-GL-Groups.html#LightTextureModeEXT).
  -> m ()
glApplyTextureEXT v1 = liftIO $ dyn5 ptr_glApplyTextureEXT v1

{-# NOINLINE ptr_glApplyTextureEXT #-}
ptr_glApplyTextureEXT :: FunPtr (GLenum -> IO ())
ptr_glApplyTextureEXT = unsafePerformIO $ getCommand "glApplyTextureEXT"

-- glAreProgramsResidentNV -----------------------------------------------------

glAreProgramsResidentNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> Ptr GLboolean -- ^ @residences@ pointing to @n@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glAreProgramsResidentNV v1 v2 v3 = liftIO $ dyn12 ptr_glAreProgramsResidentNV v1 v2 v3

{-# NOINLINE ptr_glAreProgramsResidentNV #-}
ptr_glAreProgramsResidentNV :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean)
ptr_glAreProgramsResidentNV = unsafePerformIO $ getCommand "glAreProgramsResidentNV"

-- glAreTexturesResident -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glAreTexturesResident.xml OpenGL 2.x>.
glAreTexturesResident
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> Ptr GLboolean -- ^ @residences@ pointing to @n@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glAreTexturesResident v1 v2 v3 = liftIO $ dyn12 ptr_glAreTexturesResident v1 v2 v3

{-# NOINLINE ptr_glAreTexturesResident #-}
ptr_glAreTexturesResident :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean)
ptr_glAreTexturesResident = unsafePerformIO $ getCommand "glAreTexturesResident"

-- glAreTexturesResidentEXT ----------------------------------------------------

glAreTexturesResidentEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> Ptr GLboolean -- ^ @residences@ pointing to @n@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glAreTexturesResidentEXT v1 v2 v3 = liftIO $ dyn12 ptr_glAreTexturesResidentEXT v1 v2 v3

{-# NOINLINE ptr_glAreTexturesResidentEXT #-}
ptr_glAreTexturesResidentEXT :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean)
ptr_glAreTexturesResidentEXT = unsafePerformIO $ getCommand "glAreTexturesResidentEXT"

-- glArrayElement --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glArrayElement.xml OpenGL 2.x>.
glArrayElement
  :: MonadIO m
  => GLint -- ^ @i@.
  -> m ()
glArrayElement v1 = liftIO $ dyn13 ptr_glArrayElement v1

{-# NOINLINE ptr_glArrayElement #-}
ptr_glArrayElement :: FunPtr (GLint -> IO ())
ptr_glArrayElement = unsafePerformIO $ getCommand "glArrayElement"

-- glArrayElementEXT -----------------------------------------------------------

-- | This command is an alias for 'glArrayElement'.
glArrayElementEXT
  :: MonadIO m
  => GLint -- ^ @i@.
  -> m ()
glArrayElementEXT v1 = liftIO $ dyn13 ptr_glArrayElementEXT v1

{-# NOINLINE ptr_glArrayElementEXT #-}
ptr_glArrayElementEXT :: FunPtr (GLint -> IO ())
ptr_glArrayElementEXT = unsafePerformIO $ getCommand "glArrayElementEXT"

-- glArrayObjectATI ------------------------------------------------------------

glArrayObjectATI
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ScalarType](Graphics-GL-Groups.html#ScalarType).
  -> GLsizei -- ^ @stride@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @offset@.
  -> m ()
glArrayObjectATI v1 v2 v3 v4 v5 v6 = liftIO $ dyn14 ptr_glArrayObjectATI v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glArrayObjectATI #-}
ptr_glArrayObjectATI :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ())
ptr_glArrayObjectATI = unsafePerformIO $ getCommand "glArrayObjectATI"

-- glAsyncCopyBufferSubDataNVX -------------------------------------------------

glAsyncCopyBufferSubDataNVX
  :: MonadIO m
  => GLsizei -- ^ @waitSemaphoreCount@.
  -> Ptr GLuint -- ^ @waitSemaphoreArray@ pointing to @waitSemaphoreCount@ elements of type @GLuint@.
  -> Ptr GLuint64 -- ^ @fenceValueArray@ pointing to @waitSemaphoreCount@ elements of type @GLuint64@.
  -> GLuint -- ^ @readGpu@.
  -> GLbitfield -- ^ @writeGpuMask@.
  -> GLuint -- ^ @readBuffer@.
  -> GLuint -- ^ @writeBuffer@.
  -> GLintptr -- ^ @readOffset@.
  -> GLintptr -- ^ @writeOffset@.
  -> GLsizeiptr -- ^ @size@.
  -> GLsizei -- ^ @signalSemaphoreCount@.
  -> Ptr GLuint -- ^ @signalSemaphoreArray@ pointing to @signalSemaphoreCount@ elements of type @GLuint@.
  -> Ptr GLuint64 -- ^ @signalValueArray@ pointing to @signalSemaphoreCount@ elements of type @GLuint64@.
  -> m GLuint
glAsyncCopyBufferSubDataNVX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 = liftIO $ dyn15 ptr_glAsyncCopyBufferSubDataNVX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13

{-# NOINLINE ptr_glAsyncCopyBufferSubDataNVX #-}
ptr_glAsyncCopyBufferSubDataNVX :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLuint64 -> GLuint -> GLbitfield -> GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> GLsizei -> Ptr GLuint -> Ptr GLuint64 -> IO GLuint)
ptr_glAsyncCopyBufferSubDataNVX = unsafePerformIO $ getCommand "glAsyncCopyBufferSubDataNVX"

-- glAsyncCopyImageSubDataNVX --------------------------------------------------

glAsyncCopyImageSubDataNVX
  :: MonadIO m
  => GLsizei -- ^ @waitSemaphoreCount@.
  -> Ptr GLuint -- ^ @waitSemaphoreArray@ pointing to @waitSemaphoreCount@ elements of type @GLuint@.
  -> Ptr GLuint64 -- ^ @waitValueArray@ pointing to @waitSemaphoreCount@ elements of type @GLuint64@.
  -> GLuint -- ^ @srcGpu@.
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
  -> GLsizei -- ^ @signalSemaphoreCount@.
  -> Ptr GLuint -- ^ @signalSemaphoreArray@ pointing to @signalSemaphoreCount@ elements of type @GLuint@.
  -> Ptr GLuint64 -- ^ @signalValueArray@ pointing to @signalSemaphoreCount@ elements of type @GLuint64@.
  -> m GLuint
glAsyncCopyImageSubDataNVX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 = liftIO $ dyn16 ptr_glAsyncCopyImageSubDataNVX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23

{-# NOINLINE ptr_glAsyncCopyImageSubDataNVX #-}
ptr_glAsyncCopyImageSubDataNVX :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLuint64 -> GLuint -> GLbitfield -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> Ptr GLuint -> Ptr GLuint64 -> IO GLuint)
ptr_glAsyncCopyImageSubDataNVX = unsafePerformIO $ getCommand "glAsyncCopyImageSubDataNVX"

-- glAsyncMarkerSGIX -----------------------------------------------------------

glAsyncMarkerSGIX
  :: MonadIO m
  => GLuint -- ^ @marker@.
  -> m ()
glAsyncMarkerSGIX v1 = liftIO $ dyn3 ptr_glAsyncMarkerSGIX v1

{-# NOINLINE ptr_glAsyncMarkerSGIX #-}
ptr_glAsyncMarkerSGIX :: FunPtr (GLuint -> IO ())
ptr_glAsyncMarkerSGIX = unsafePerformIO $ getCommand "glAsyncMarkerSGIX"

-- glAttachObjectARB -----------------------------------------------------------

-- | This command is an alias for 'glAttachShader'.
glAttachObjectARB
  :: MonadIO m
  => GLhandleARB -- ^ @containerObj@ of type @handleARB@.
  -> GLhandleARB -- ^ @obj@ of type @handleARB@.
  -> m ()
glAttachObjectARB v1 v2 = liftIO $ dyn17 ptr_glAttachObjectARB v1 v2

{-# NOINLINE ptr_glAttachObjectARB #-}
ptr_glAttachObjectARB :: FunPtr (GLhandleARB -> GLhandleARB -> IO ())
ptr_glAttachObjectARB = unsafePerformIO $ getCommand "glAttachObjectARB"

-- glAttachShader --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glAttachShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glAttachShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glAttachShader.xhtml OpenGL 4.x>.
glAttachShader
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @shader@.
  -> m ()
glAttachShader v1 v2 = liftIO $ dyn4 ptr_glAttachShader v1 v2

{-# NOINLINE ptr_glAttachShader #-}
ptr_glAttachShader :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glAttachShader = unsafePerformIO $ getCommand "glAttachShader"

-- glBegin ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glBegin.xml OpenGL 2.x>.
glBegin
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> m ()
glBegin v1 = liftIO $ dyn5 ptr_glBegin v1

{-# NOINLINE ptr_glBegin #-}
ptr_glBegin :: FunPtr (GLenum -> IO ())
ptr_glBegin = unsafePerformIO $ getCommand "glBegin"

-- glBeginConditionalRender ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginConditionalRender.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginConditionalRender.xhtml OpenGL 4.x>.
glBeginConditionalRender
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @mode@ of type [ConditionalRenderMode](Graphics-GL-Groups.html#ConditionalRenderMode).
  -> m ()
glBeginConditionalRender v1 v2 = liftIO $ dyn18 ptr_glBeginConditionalRender v1 v2

{-# NOINLINE ptr_glBeginConditionalRender #-}
ptr_glBeginConditionalRender :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glBeginConditionalRender = unsafePerformIO $ getCommand "glBeginConditionalRender"

-- glBeginConditionalRenderNV --------------------------------------------------

-- | This command is an alias for 'glBeginConditionalRender'.
glBeginConditionalRenderNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @mode@ of type [ConditionalRenderMode](Graphics-GL-Groups.html#ConditionalRenderMode).
  -> m ()
glBeginConditionalRenderNV v1 v2 = liftIO $ dyn18 ptr_glBeginConditionalRenderNV v1 v2

{-# NOINLINE ptr_glBeginConditionalRenderNV #-}
ptr_glBeginConditionalRenderNV :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glBeginConditionalRenderNV = unsafePerformIO $ getCommand "glBeginConditionalRenderNV"

-- glBeginConditionalRenderNVX -------------------------------------------------

glBeginConditionalRenderNVX
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glBeginConditionalRenderNVX v1 = liftIO $ dyn3 ptr_glBeginConditionalRenderNVX v1

{-# NOINLINE ptr_glBeginConditionalRenderNVX #-}
ptr_glBeginConditionalRenderNVX :: FunPtr (GLuint -> IO ())
ptr_glBeginConditionalRenderNVX = unsafePerformIO $ getCommand "glBeginConditionalRenderNVX"

-- glBeginFragmentShaderATI ----------------------------------------------------

glBeginFragmentShaderATI
  :: MonadIO m
  => m ()
glBeginFragmentShaderATI = liftIO $ dyn11 ptr_glBeginFragmentShaderATI

{-# NOINLINE ptr_glBeginFragmentShaderATI #-}
ptr_glBeginFragmentShaderATI :: FunPtr (IO ())
ptr_glBeginFragmentShaderATI = unsafePerformIO $ getCommand "glBeginFragmentShaderATI"

-- glBeginOcclusionQueryNV -----------------------------------------------------

glBeginOcclusionQueryNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glBeginOcclusionQueryNV v1 = liftIO $ dyn3 ptr_glBeginOcclusionQueryNV v1

{-# NOINLINE ptr_glBeginOcclusionQueryNV #-}
ptr_glBeginOcclusionQueryNV :: FunPtr (GLuint -> IO ())
ptr_glBeginOcclusionQueryNV = unsafePerformIO $ getCommand "glBeginOcclusionQueryNV"

-- glBeginPerfMonitorAMD -------------------------------------------------------

glBeginPerfMonitorAMD
  :: MonadIO m
  => GLuint -- ^ @monitor@.
  -> m ()
glBeginPerfMonitorAMD v1 = liftIO $ dyn3 ptr_glBeginPerfMonitorAMD v1

{-# NOINLINE ptr_glBeginPerfMonitorAMD #-}
ptr_glBeginPerfMonitorAMD :: FunPtr (GLuint -> IO ())
ptr_glBeginPerfMonitorAMD = unsafePerformIO $ getCommand "glBeginPerfMonitorAMD"

-- glBeginPerfQueryINTEL -------------------------------------------------------

glBeginPerfQueryINTEL
  :: MonadIO m
  => GLuint -- ^ @queryHandle@.
  -> m ()
glBeginPerfQueryINTEL v1 = liftIO $ dyn3 ptr_glBeginPerfQueryINTEL v1

{-# NOINLINE ptr_glBeginPerfQueryINTEL #-}
ptr_glBeginPerfQueryINTEL :: FunPtr (GLuint -> IO ())
ptr_glBeginPerfQueryINTEL = unsafePerformIO $ getCommand "glBeginPerfQueryINTEL"

-- glBeginQuery ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBeginQuery.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginQuery.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginQuery.xhtml OpenGL 4.x>.
glBeginQuery
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> GLuint -- ^ @id@.
  -> m ()
glBeginQuery v1 v2 = liftIO $ dyn19 ptr_glBeginQuery v1 v2

{-# NOINLINE ptr_glBeginQuery #-}
ptr_glBeginQuery :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBeginQuery = unsafePerformIO $ getCommand "glBeginQuery"

-- glBeginQueryARB -------------------------------------------------------------

-- | This command is an alias for 'glBeginQuery'.
glBeginQueryARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @id@.
  -> m ()
glBeginQueryARB v1 v2 = liftIO $ dyn19 ptr_glBeginQueryARB v1 v2

{-# NOINLINE ptr_glBeginQueryARB #-}
ptr_glBeginQueryARB :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBeginQueryARB = unsafePerformIO $ getCommand "glBeginQueryARB"

-- glBeginQueryEXT -------------------------------------------------------------

glBeginQueryEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> GLuint -- ^ @id@.
  -> m ()
glBeginQueryEXT v1 v2 = liftIO $ dyn19 ptr_glBeginQueryEXT v1 v2

{-# NOINLINE ptr_glBeginQueryEXT #-}
ptr_glBeginQueryEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBeginQueryEXT = unsafePerformIO $ getCommand "glBeginQueryEXT"

-- glBeginQueryIndexed ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBeginQueryIndexed.xhtml OpenGL 4.x>.
glBeginQueryIndexed
  :: MonadIO m
  => GLenum -- ^ @target@ of type [QueryTarget](Graphics-GL-Groups.html#QueryTarget).
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @id@.
  -> m ()
glBeginQueryIndexed v1 v2 v3 = liftIO $ dyn20 ptr_glBeginQueryIndexed v1 v2 v3

{-# NOINLINE ptr_glBeginQueryIndexed #-}
ptr_glBeginQueryIndexed :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
ptr_glBeginQueryIndexed = unsafePerformIO $ getCommand "glBeginQueryIndexed"

-- glBeginTransformFeedback ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginTransformFeedback.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginTransformFeedback.xhtml OpenGL 4.x>.
glBeginTransformFeedback
  :: MonadIO m
  => GLenum -- ^ @primitiveMode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> m ()
glBeginTransformFeedback v1 = liftIO $ dyn5 ptr_glBeginTransformFeedback v1

{-# NOINLINE ptr_glBeginTransformFeedback #-}
ptr_glBeginTransformFeedback :: FunPtr (GLenum -> IO ())
ptr_glBeginTransformFeedback = unsafePerformIO $ getCommand "glBeginTransformFeedback"

-- glBeginTransformFeedbackEXT -------------------------------------------------

-- | This command is an alias for 'glBeginTransformFeedback'.
glBeginTransformFeedbackEXT
  :: MonadIO m
  => GLenum -- ^ @primitiveMode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> m ()
glBeginTransformFeedbackEXT v1 = liftIO $ dyn5 ptr_glBeginTransformFeedbackEXT v1

{-# NOINLINE ptr_glBeginTransformFeedbackEXT #-}
ptr_glBeginTransformFeedbackEXT :: FunPtr (GLenum -> IO ())
ptr_glBeginTransformFeedbackEXT = unsafePerformIO $ getCommand "glBeginTransformFeedbackEXT"

-- glBeginTransformFeedbackNV --------------------------------------------------

-- | This command is an alias for 'glBeginTransformFeedback'.
glBeginTransformFeedbackNV
  :: MonadIO m
  => GLenum -- ^ @primitiveMode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> m ()
glBeginTransformFeedbackNV v1 = liftIO $ dyn5 ptr_glBeginTransformFeedbackNV v1

{-# NOINLINE ptr_glBeginTransformFeedbackNV #-}
ptr_glBeginTransformFeedbackNV :: FunPtr (GLenum -> IO ())
ptr_glBeginTransformFeedbackNV = unsafePerformIO $ getCommand "glBeginTransformFeedbackNV"

-- glBeginVertexShaderEXT ------------------------------------------------------

glBeginVertexShaderEXT
  :: MonadIO m
  => m ()
glBeginVertexShaderEXT = liftIO $ dyn11 ptr_glBeginVertexShaderEXT

{-# NOINLINE ptr_glBeginVertexShaderEXT #-}
ptr_glBeginVertexShaderEXT :: FunPtr (IO ())
ptr_glBeginVertexShaderEXT = unsafePerformIO $ getCommand "glBeginVertexShaderEXT"

-- glBeginVideoCaptureNV -------------------------------------------------------

glBeginVideoCaptureNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> m ()
glBeginVideoCaptureNV v1 = liftIO $ dyn3 ptr_glBeginVideoCaptureNV v1

{-# NOINLINE ptr_glBeginVideoCaptureNV #-}
ptr_glBeginVideoCaptureNV :: FunPtr (GLuint -> IO ())
ptr_glBeginVideoCaptureNV = unsafePerformIO $ getCommand "glBeginVideoCaptureNV"

-- glBindAttribLocation --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBindAttribLocation.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBindAttribLocation.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindAttribLocation.xhtml OpenGL 4.x>.
glBindAttribLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> Ptr GLchar -- ^ @name@.
  -> m ()
glBindAttribLocation v1 v2 v3 = liftIO $ dyn21 ptr_glBindAttribLocation v1 v2 v3

{-# NOINLINE ptr_glBindAttribLocation #-}
ptr_glBindAttribLocation :: FunPtr (GLuint -> GLuint -> Ptr GLchar -> IO ())
ptr_glBindAttribLocation = unsafePerformIO $ getCommand "glBindAttribLocation"

-- glBindAttribLocationARB -----------------------------------------------------

-- | This command is an alias for 'glBindAttribLocation'.
glBindAttribLocationARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> GLuint -- ^ @index@.
  -> Ptr GLcharARB -- ^ @name@.
  -> m ()
glBindAttribLocationARB v1 v2 v3 = liftIO $ dyn22 ptr_glBindAttribLocationARB v1 v2 v3

{-# NOINLINE ptr_glBindAttribLocationARB #-}
ptr_glBindAttribLocationARB :: FunPtr (GLhandleARB -> GLuint -> Ptr GLcharARB -> IO ())
ptr_glBindAttribLocationARB = unsafePerformIO $ getCommand "glBindAttribLocationARB"

-- glBindBuffer ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBindBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBindBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindBuffer.xhtml OpenGL 4.x>.
glBindBuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @buffer@.
  -> m ()
glBindBuffer v1 v2 = liftIO $ dyn19 ptr_glBindBuffer v1 v2

{-# NOINLINE ptr_glBindBuffer #-}
ptr_glBindBuffer :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindBuffer = unsafePerformIO $ getCommand "glBindBuffer"

-- glBindBufferARB -------------------------------------------------------------

-- | This command is an alias for 'glBindBuffer'.
glBindBufferARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @buffer@.
  -> m ()
glBindBufferARB v1 v2 = liftIO $ dyn19 ptr_glBindBufferARB v1 v2

{-# NOINLINE ptr_glBindBufferARB #-}
ptr_glBindBufferARB :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindBufferARB = unsafePerformIO $ getCommand "glBindBufferARB"

-- glBindBufferBase ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindBufferBase.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindBufferBase.xhtml OpenGL 4.x>.
glBindBufferBase
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glBindBufferBase v1 v2 v3 = liftIO $ dyn20 ptr_glBindBufferBase v1 v2 v3

{-# NOINLINE ptr_glBindBufferBase #-}
ptr_glBindBufferBase :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
ptr_glBindBufferBase = unsafePerformIO $ getCommand "glBindBufferBase"

-- glBindBufferBaseEXT ---------------------------------------------------------

-- | This command is an alias for 'glBindBufferBase'.
glBindBufferBaseEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glBindBufferBaseEXT v1 v2 v3 = liftIO $ dyn20 ptr_glBindBufferBaseEXT v1 v2 v3

{-# NOINLINE ptr_glBindBufferBaseEXT #-}
ptr_glBindBufferBaseEXT :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
ptr_glBindBufferBaseEXT = unsafePerformIO $ getCommand "glBindBufferBaseEXT"

-- glBindBufferBaseNV ----------------------------------------------------------

-- | This command is an alias for 'glBindBufferBase'.
glBindBufferBaseNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glBindBufferBaseNV v1 v2 v3 = liftIO $ dyn20 ptr_glBindBufferBaseNV v1 v2 v3

{-# NOINLINE ptr_glBindBufferBaseNV #-}
ptr_glBindBufferBaseNV :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
ptr_glBindBufferBaseNV = unsafePerformIO $ getCommand "glBindBufferBaseNV"

-- glBindBufferOffsetEXT -------------------------------------------------------

glBindBufferOffsetEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> m ()
glBindBufferOffsetEXT v1 v2 v3 v4 = liftIO $ dyn23 ptr_glBindBufferOffsetEXT v1 v2 v3 v4

{-# NOINLINE ptr_glBindBufferOffsetEXT #-}
ptr_glBindBufferOffsetEXT :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> IO ())
ptr_glBindBufferOffsetEXT = unsafePerformIO $ getCommand "glBindBufferOffsetEXT"

-- glBindBufferOffsetNV --------------------------------------------------------

-- | This command is an alias for 'glBindBufferOffsetEXT'.
glBindBufferOffsetNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> m ()
glBindBufferOffsetNV v1 v2 v3 v4 = liftIO $ dyn23 ptr_glBindBufferOffsetNV v1 v2 v3 v4

{-# NOINLINE ptr_glBindBufferOffsetNV #-}
ptr_glBindBufferOffsetNV :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> IO ())
ptr_glBindBufferOffsetNV = unsafePerformIO $ getCommand "glBindBufferOffsetNV"

-- glBindBufferRange -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindBufferRange.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindBufferRange.xhtml OpenGL 4.x>.
glBindBufferRange
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glBindBufferRange v1 v2 v3 v4 v5 = liftIO $ dyn24 ptr_glBindBufferRange v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBindBufferRange #-}
ptr_glBindBufferRange :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glBindBufferRange = unsafePerformIO $ getCommand "glBindBufferRange"

-- glBindBufferRangeEXT --------------------------------------------------------

-- | This command is an alias for 'glBindBufferRange'.
glBindBufferRangeEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glBindBufferRangeEXT v1 v2 v3 v4 v5 = liftIO $ dyn24 ptr_glBindBufferRangeEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBindBufferRangeEXT #-}
ptr_glBindBufferRangeEXT :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glBindBufferRangeEXT = unsafePerformIO $ getCommand "glBindBufferRangeEXT"

-- glBindBufferRangeNV ---------------------------------------------------------

-- | This command is an alias for 'glBindBufferRange'.
glBindBufferRangeNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glBindBufferRangeNV v1 v2 v3 v4 v5 = liftIO $ dyn24 ptr_glBindBufferRangeNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBindBufferRangeNV #-}
ptr_glBindBufferRangeNV :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glBindBufferRangeNV = unsafePerformIO $ getCommand "glBindBufferRangeNV"

-- glBindBuffersBase -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindBuffersBase.xhtml OpenGL 4.x>.
glBindBuffersBase
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glBindBuffersBase v1 v2 v3 v4 = liftIO $ dyn25 ptr_glBindBuffersBase v1 v2 v3 v4

{-# NOINLINE ptr_glBindBuffersBase #-}
ptr_glBindBuffersBase :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glBindBuffersBase = unsafePerformIO $ getCommand "glBindBuffersBase"

-- glBindBuffersRange ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindBuffersRange.xhtml OpenGL 4.x>.
glBindBuffersRange
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @count@ elements of type @GLuint@.
  -> Ptr GLintptr -- ^ @offsets@ pointing to @count@ elements of type @GLintptr@.
  -> Ptr GLsizeiptr -- ^ @sizes@ pointing to @count@ elements of type @GLsizeiptr@.
  -> m ()
glBindBuffersRange v1 v2 v3 v4 v5 v6 = liftIO $ dyn26 ptr_glBindBuffersRange v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glBindBuffersRange #-}
ptr_glBindBuffersRange :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizeiptr -> IO ())
ptr_glBindBuffersRange = unsafePerformIO $ getCommand "glBindBuffersRange"

-- glBindFragDataLocation ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindFragDataLocation.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindFragDataLocation.xhtml OpenGL 4.x>.
glBindFragDataLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @color@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m ()
glBindFragDataLocation v1 v2 v3 = liftIO $ dyn21 ptr_glBindFragDataLocation v1 v2 v3

{-# NOINLINE ptr_glBindFragDataLocation #-}
ptr_glBindFragDataLocation :: FunPtr (GLuint -> GLuint -> Ptr GLchar -> IO ())
ptr_glBindFragDataLocation = unsafePerformIO $ getCommand "glBindFragDataLocation"

-- glBindFragDataLocationEXT ---------------------------------------------------

-- | This command is an alias for 'glBindFragDataLocation'.
glBindFragDataLocationEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @color@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m ()
glBindFragDataLocationEXT v1 v2 v3 = liftIO $ dyn21 ptr_glBindFragDataLocationEXT v1 v2 v3

{-# NOINLINE ptr_glBindFragDataLocationEXT #-}
ptr_glBindFragDataLocationEXT :: FunPtr (GLuint -> GLuint -> Ptr GLchar -> IO ())
ptr_glBindFragDataLocationEXT = unsafePerformIO $ getCommand "glBindFragDataLocationEXT"

-- glBindFragDataLocationIndexed -----------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindFragDataLocationIndexed.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindFragDataLocationIndexed.xhtml OpenGL 4.x>.
glBindFragDataLocationIndexed
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @colorNumber@.
  -> GLuint -- ^ @index@.
  -> Ptr GLchar -- ^ @name@.
  -> m ()
glBindFragDataLocationIndexed v1 v2 v3 v4 = liftIO $ dyn27 ptr_glBindFragDataLocationIndexed v1 v2 v3 v4

{-# NOINLINE ptr_glBindFragDataLocationIndexed #-}
ptr_glBindFragDataLocationIndexed :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> IO ())
ptr_glBindFragDataLocationIndexed = unsafePerformIO $ getCommand "glBindFragDataLocationIndexed"

-- glBindFragDataLocationIndexedEXT --------------------------------------------

-- | This command is an alias for 'glBindFragDataLocationIndexed'.
glBindFragDataLocationIndexedEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @colorNumber@.
  -> GLuint -- ^ @index@.
  -> Ptr GLchar -- ^ @name@.
  -> m ()
glBindFragDataLocationIndexedEXT v1 v2 v3 v4 = liftIO $ dyn27 ptr_glBindFragDataLocationIndexedEXT v1 v2 v3 v4

{-# NOINLINE ptr_glBindFragDataLocationIndexedEXT #-}
ptr_glBindFragDataLocationIndexedEXT :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> IO ())
ptr_glBindFragDataLocationIndexedEXT = unsafePerformIO $ getCommand "glBindFragDataLocationIndexedEXT"

-- glBindFragmentShaderATI -----------------------------------------------------

glBindFragmentShaderATI
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glBindFragmentShaderATI v1 = liftIO $ dyn3 ptr_glBindFragmentShaderATI v1

{-# NOINLINE ptr_glBindFragmentShaderATI #-}
ptr_glBindFragmentShaderATI :: FunPtr (GLuint -> IO ())
ptr_glBindFragmentShaderATI = unsafePerformIO $ getCommand "glBindFragmentShaderATI"

-- glBindFramebuffer -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindFramebuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindFramebuffer.xhtml OpenGL 4.x>.
glBindFramebuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLuint -- ^ @framebuffer@.
  -> m ()
glBindFramebuffer v1 v2 = liftIO $ dyn19 ptr_glBindFramebuffer v1 v2

{-# NOINLINE ptr_glBindFramebuffer #-}
ptr_glBindFramebuffer :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindFramebuffer = unsafePerformIO $ getCommand "glBindFramebuffer"

-- glBindFramebufferEXT --------------------------------------------------------

glBindFramebufferEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLuint -- ^ @framebuffer@.
  -> m ()
glBindFramebufferEXT v1 v2 = liftIO $ dyn19 ptr_glBindFramebufferEXT v1 v2

{-# NOINLINE ptr_glBindFramebufferEXT #-}
ptr_glBindFramebufferEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindFramebufferEXT = unsafePerformIO $ getCommand "glBindFramebufferEXT"

-- glBindFramebufferOES --------------------------------------------------------

glBindFramebufferOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLuint -- ^ @framebuffer@.
  -> m ()
glBindFramebufferOES v1 v2 = liftIO $ dyn19 ptr_glBindFramebufferOES v1 v2

{-# NOINLINE ptr_glBindFramebufferOES #-}
ptr_glBindFramebufferOES :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindFramebufferOES = unsafePerformIO $ getCommand "glBindFramebufferOES"

-- glBindImageTexture ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindImageTexture.xhtml OpenGL 4.x>.
glBindImageTexture
  :: MonadIO m
  => GLuint -- ^ @unit@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLboolean -- ^ @layered@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLint -- ^ @layer@.
  -> GLenum -- ^ @access@ of type [BufferAccessARB](Graphics-GL-Groups.html#BufferAccessARB).
  -> GLenum -- ^ @format@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> m ()
glBindImageTexture v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn28 ptr_glBindImageTexture v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glBindImageTexture #-}
ptr_glBindImageTexture :: FunPtr (GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLenum -> IO ())
ptr_glBindImageTexture = unsafePerformIO $ getCommand "glBindImageTexture"

-- glBindImageTextureEXT -------------------------------------------------------

glBindImageTextureEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLboolean -- ^ @layered@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLint -- ^ @layer@.
  -> GLenum -- ^ @access@ of type [BufferAccessARB](Graphics-GL-Groups.html#BufferAccessARB).
  -> GLint -- ^ @format@.
  -> m ()
glBindImageTextureEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn29 ptr_glBindImageTextureEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glBindImageTextureEXT #-}
ptr_glBindImageTextureEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLint -> IO ())
ptr_glBindImageTextureEXT = unsafePerformIO $ getCommand "glBindImageTextureEXT"

-- glBindImageTextures ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindImageTextures.xhtml OpenGL 4.x>.
glBindImageTextures
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @textures@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glBindImageTextures v1 v2 v3 = liftIO $ dyn30 ptr_glBindImageTextures v1 v2 v3

{-# NOINLINE ptr_glBindImageTextures #-}
ptr_glBindImageTextures :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glBindImageTextures = unsafePerformIO $ getCommand "glBindImageTextures"

-- glBindLightParameterEXT -----------------------------------------------------

glBindLightParameterEXT
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @value@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> m GLuint
glBindLightParameterEXT v1 v2 = liftIO $ dyn31 ptr_glBindLightParameterEXT v1 v2

{-# NOINLINE ptr_glBindLightParameterEXT #-}
ptr_glBindLightParameterEXT :: FunPtr (GLenum -> GLenum -> IO GLuint)
ptr_glBindLightParameterEXT = unsafePerformIO $ getCommand "glBindLightParameterEXT"

-- glBindMaterialParameterEXT --------------------------------------------------

glBindMaterialParameterEXT
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @value@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> m GLuint
glBindMaterialParameterEXT v1 v2 = liftIO $ dyn31 ptr_glBindMaterialParameterEXT v1 v2

{-# NOINLINE ptr_glBindMaterialParameterEXT #-}
ptr_glBindMaterialParameterEXT :: FunPtr (GLenum -> GLenum -> IO GLuint)
ptr_glBindMaterialParameterEXT = unsafePerformIO $ getCommand "glBindMaterialParameterEXT"

-- glBindMultiTextureEXT -------------------------------------------------------

glBindMultiTextureEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> m ()
glBindMultiTextureEXT v1 v2 v3 = liftIO $ dyn32 ptr_glBindMultiTextureEXT v1 v2 v3

{-# NOINLINE ptr_glBindMultiTextureEXT #-}
ptr_glBindMultiTextureEXT :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glBindMultiTextureEXT = unsafePerformIO $ getCommand "glBindMultiTextureEXT"

-- glBindParameterEXT ----------------------------------------------------------

glBindParameterEXT
  :: MonadIO m
  => GLenum -- ^ @value@ of type [VertexShaderParameterEXT](Graphics-GL-Groups.html#VertexShaderParameterEXT).
  -> m GLuint
glBindParameterEXT v1 = liftIO $ dyn33 ptr_glBindParameterEXT v1

{-# NOINLINE ptr_glBindParameterEXT #-}
ptr_glBindParameterEXT :: FunPtr (GLenum -> IO GLuint)
ptr_glBindParameterEXT = unsafePerformIO $ getCommand "glBindParameterEXT"

-- glBindProgramARB ------------------------------------------------------------

glBindProgramARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @program@.
  -> m ()
glBindProgramARB v1 v2 = liftIO $ dyn19 ptr_glBindProgramARB v1 v2

{-# NOINLINE ptr_glBindProgramARB #-}
ptr_glBindProgramARB :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindProgramARB = unsafePerformIO $ getCommand "glBindProgramARB"

-- glBindProgramNV -------------------------------------------------------------

-- | This command is an alias for 'glBindProgramARB'.
glBindProgramNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @id@.
  -> m ()
glBindProgramNV v1 v2 = liftIO $ dyn19 ptr_glBindProgramNV v1 v2

{-# NOINLINE ptr_glBindProgramNV #-}
ptr_glBindProgramNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindProgramNV = unsafePerformIO $ getCommand "glBindProgramNV"

-- glBindProgramPipeline -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindProgramPipeline.xhtml OpenGL 4.x>.
glBindProgramPipeline
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m ()
glBindProgramPipeline v1 = liftIO $ dyn3 ptr_glBindProgramPipeline v1

{-# NOINLINE ptr_glBindProgramPipeline #-}
ptr_glBindProgramPipeline :: FunPtr (GLuint -> IO ())
ptr_glBindProgramPipeline = unsafePerformIO $ getCommand "glBindProgramPipeline"

-- glBindProgramPipelineEXT ----------------------------------------------------

glBindProgramPipelineEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m ()
glBindProgramPipelineEXT v1 = liftIO $ dyn3 ptr_glBindProgramPipelineEXT v1

{-# NOINLINE ptr_glBindProgramPipelineEXT #-}
ptr_glBindProgramPipelineEXT :: FunPtr (GLuint -> IO ())
ptr_glBindProgramPipelineEXT = unsafePerformIO $ getCommand "glBindProgramPipelineEXT"

-- glBindRenderbuffer ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindRenderbuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindRenderbuffer.xhtml OpenGL 4.x>.
glBindRenderbuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glBindRenderbuffer v1 v2 = liftIO $ dyn19 ptr_glBindRenderbuffer v1 v2

{-# NOINLINE ptr_glBindRenderbuffer #-}
ptr_glBindRenderbuffer :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindRenderbuffer = unsafePerformIO $ getCommand "glBindRenderbuffer"

-- glBindRenderbufferEXT -------------------------------------------------------

glBindRenderbufferEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glBindRenderbufferEXT v1 v2 = liftIO $ dyn19 ptr_glBindRenderbufferEXT v1 v2

{-# NOINLINE ptr_glBindRenderbufferEXT #-}
ptr_glBindRenderbufferEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindRenderbufferEXT = unsafePerformIO $ getCommand "glBindRenderbufferEXT"

-- glBindRenderbufferOES -------------------------------------------------------

glBindRenderbufferOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [RenderbufferTarget](Graphics-GL-Groups.html#RenderbufferTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glBindRenderbufferOES v1 v2 = liftIO $ dyn19 ptr_glBindRenderbufferOES v1 v2

{-# NOINLINE ptr_glBindRenderbufferOES #-}
ptr_glBindRenderbufferOES :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindRenderbufferOES = unsafePerformIO $ getCommand "glBindRenderbufferOES"

-- glBindSampler ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindSampler.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindSampler.xhtml OpenGL 4.x>.
glBindSampler
  :: MonadIO m
  => GLuint -- ^ @unit@.
  -> GLuint -- ^ @sampler@.
  -> m ()
glBindSampler v1 v2 = liftIO $ dyn4 ptr_glBindSampler v1 v2

{-# NOINLINE ptr_glBindSampler #-}
ptr_glBindSampler :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glBindSampler = unsafePerformIO $ getCommand "glBindSampler"

-- glBindSamplers --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindSamplers.xhtml OpenGL 4.x>.
glBindSamplers
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @samplers@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glBindSamplers v1 v2 v3 = liftIO $ dyn30 ptr_glBindSamplers v1 v2 v3

{-# NOINLINE ptr_glBindSamplers #-}
ptr_glBindSamplers :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glBindSamplers = unsafePerformIO $ getCommand "glBindSamplers"

-- glBindShadingRateImageNV ----------------------------------------------------

glBindShadingRateImageNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> m ()
glBindShadingRateImageNV v1 = liftIO $ dyn3 ptr_glBindShadingRateImageNV v1

{-# NOINLINE ptr_glBindShadingRateImageNV #-}
ptr_glBindShadingRateImageNV :: FunPtr (GLuint -> IO ())
ptr_glBindShadingRateImageNV = unsafePerformIO $ getCommand "glBindShadingRateImageNV"

-- glBindTexGenParameterEXT ----------------------------------------------------

glBindTexGenParameterEXT
  :: MonadIO m
  => GLenum -- ^ @unit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @value@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> m GLuint
glBindTexGenParameterEXT v1 v2 v3 = liftIO $ dyn34 ptr_glBindTexGenParameterEXT v1 v2 v3

{-# NOINLINE ptr_glBindTexGenParameterEXT #-}
ptr_glBindTexGenParameterEXT :: FunPtr (GLenum -> GLenum -> GLenum -> IO GLuint)
ptr_glBindTexGenParameterEXT = unsafePerformIO $ getCommand "glBindTexGenParameterEXT"

-- glBindTexture ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBindTexture.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBindTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindTexture.xhtml OpenGL 4.x>.
glBindTexture
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> m ()
glBindTexture v1 v2 = liftIO $ dyn19 ptr_glBindTexture v1 v2

{-# NOINLINE ptr_glBindTexture #-}
ptr_glBindTexture :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindTexture = unsafePerformIO $ getCommand "glBindTexture"

-- glBindTextureEXT ------------------------------------------------------------

-- | This command is an alias for 'glBindTexture'.
glBindTextureEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> m ()
glBindTextureEXT v1 v2 = liftIO $ dyn19 ptr_glBindTextureEXT v1 v2

{-# NOINLINE ptr_glBindTextureEXT #-}
ptr_glBindTextureEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindTextureEXT = unsafePerformIO $ getCommand "glBindTextureEXT"

-- glBindTextureUnit -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindTextureUnit.xhtml OpenGL 4.x>.
glBindTextureUnit
  :: MonadIO m
  => GLuint -- ^ @unit@.
  -> GLuint -- ^ @texture@.
  -> m ()
glBindTextureUnit v1 v2 = liftIO $ dyn4 ptr_glBindTextureUnit v1 v2

{-# NOINLINE ptr_glBindTextureUnit #-}
ptr_glBindTextureUnit :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glBindTextureUnit = unsafePerformIO $ getCommand "glBindTextureUnit"

-- glBindTextureUnitParameterEXT -----------------------------------------------

glBindTextureUnitParameterEXT
  :: MonadIO m
  => GLenum -- ^ @unit@ of type [TextureUnit](Graphics-GL-Groups.html#TextureUnit).
  -> GLenum -- ^ @value@ of type [VertexShaderTextureUnitParameter](Graphics-GL-Groups.html#VertexShaderTextureUnitParameter).
  -> m GLuint
glBindTextureUnitParameterEXT v1 v2 = liftIO $ dyn31 ptr_glBindTextureUnitParameterEXT v1 v2

{-# NOINLINE ptr_glBindTextureUnitParameterEXT #-}
ptr_glBindTextureUnitParameterEXT :: FunPtr (GLenum -> GLenum -> IO GLuint)
ptr_glBindTextureUnitParameterEXT = unsafePerformIO $ getCommand "glBindTextureUnitParameterEXT"

-- glBindTextures --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindTextures.xhtml OpenGL 4.x>.
glBindTextures
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @textures@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glBindTextures v1 v2 v3 = liftIO $ dyn30 ptr_glBindTextures v1 v2 v3

{-# NOINLINE ptr_glBindTextures #-}
ptr_glBindTextures :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glBindTextures = unsafePerformIO $ getCommand "glBindTextures"

-- glBindTransformFeedback -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindTransformFeedback.xhtml OpenGL 4.x>.
glBindTransformFeedback
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BindTransformFeedbackTarget](Graphics-GL-Groups.html#BindTransformFeedbackTarget).
  -> GLuint -- ^ @id@.
  -> m ()
glBindTransformFeedback v1 v2 = liftIO $ dyn19 ptr_glBindTransformFeedback v1 v2

{-# NOINLINE ptr_glBindTransformFeedback #-}
ptr_glBindTransformFeedback :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindTransformFeedback = unsafePerformIO $ getCommand "glBindTransformFeedback"

-- glBindTransformFeedbackNV ---------------------------------------------------

glBindTransformFeedbackNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [BufferTargetARB](Graphics-GL-Groups.html#BufferTargetARB).
  -> GLuint -- ^ @id@.
  -> m ()
glBindTransformFeedbackNV v1 v2 = liftIO $ dyn19 ptr_glBindTransformFeedbackNV v1 v2

{-# NOINLINE ptr_glBindTransformFeedbackNV #-}
ptr_glBindTransformFeedbackNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindTransformFeedbackNV = unsafePerformIO $ getCommand "glBindTransformFeedbackNV"

-- glBindVertexArray -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindVertexArray.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindVertexArray.xhtml OpenGL 4.x>.
glBindVertexArray
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m ()
glBindVertexArray v1 = liftIO $ dyn3 ptr_glBindVertexArray v1

{-# NOINLINE ptr_glBindVertexArray #-}
ptr_glBindVertexArray :: FunPtr (GLuint -> IO ())
ptr_glBindVertexArray = unsafePerformIO $ getCommand "glBindVertexArray"

-- glBindVertexArrayAPPLE ------------------------------------------------------

glBindVertexArrayAPPLE
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m ()
glBindVertexArrayAPPLE v1 = liftIO $ dyn3 ptr_glBindVertexArrayAPPLE v1

{-# NOINLINE ptr_glBindVertexArrayAPPLE #-}
ptr_glBindVertexArrayAPPLE :: FunPtr (GLuint -> IO ())
ptr_glBindVertexArrayAPPLE = unsafePerformIO $ getCommand "glBindVertexArrayAPPLE"

-- glBindVertexArrayOES --------------------------------------------------------

-- | This command is an alias for 'glBindVertexArray'.
glBindVertexArrayOES
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m ()
glBindVertexArrayOES v1 = liftIO $ dyn3 ptr_glBindVertexArrayOES v1

{-# NOINLINE ptr_glBindVertexArrayOES #-}
ptr_glBindVertexArrayOES :: FunPtr (GLuint -> IO ())
ptr_glBindVertexArrayOES = unsafePerformIO $ getCommand "glBindVertexArrayOES"

-- glBindVertexBuffer ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffer.xhtml OpenGL 4.x>.
glBindVertexBuffer
  :: MonadIO m
  => GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glBindVertexBuffer v1 v2 v3 v4 = liftIO $ dyn35 ptr_glBindVertexBuffer v1 v2 v3 v4

{-# NOINLINE ptr_glBindVertexBuffer #-}
ptr_glBindVertexBuffer :: FunPtr (GLuint -> GLuint -> GLintptr -> GLsizei -> IO ())
ptr_glBindVertexBuffer = unsafePerformIO $ getCommand "glBindVertexBuffer"

-- glBindVertexBuffers ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffers.xhtml OpenGL 4.x>.
glBindVertexBuffers
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @count@ elements of type @GLuint@.
  -> Ptr GLintptr -- ^ @offsets@ pointing to @count@ elements of type @GLintptr@.
  -> Ptr GLsizei -- ^ @strides@ pointing to @count@ elements of type @GLsizei@.
  -> m ()
glBindVertexBuffers v1 v2 v3 v4 v5 = liftIO $ dyn36 ptr_glBindVertexBuffers v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBindVertexBuffers #-}
ptr_glBindVertexBuffers :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> IO ())
ptr_glBindVertexBuffers = unsafePerformIO $ getCommand "glBindVertexBuffers"

