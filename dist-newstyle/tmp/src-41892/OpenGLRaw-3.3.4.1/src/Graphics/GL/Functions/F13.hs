{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F13
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

module Graphics.GL.Functions.F13 (
  glGetTexParameterIuivEXT,
  glGetTexParameterIuivOES,
  glGetTexParameterPointervAPPLE,
  glGetTexParameterfv,
  glGetTexParameteriv,
  glGetTexParameterxv,
  glGetTexParameterxvOES,
  glGetTextureHandleARB,
  glGetTextureHandleIMG,
  glGetTextureHandleNV,
  glGetTextureImage,
  glGetTextureImageEXT,
  glGetTextureLevelParameterfv,
  glGetTextureLevelParameterfvEXT,
  glGetTextureLevelParameteriv,
  glGetTextureLevelParameterivEXT,
  glGetTextureParameterIiv,
  glGetTextureParameterIivEXT,
  glGetTextureParameterIuiv,
  glGetTextureParameterIuivEXT,
  glGetTextureParameterfv,
  glGetTextureParameterfvEXT,
  glGetTextureParameteriv,
  glGetTextureParameterivEXT,
  glGetTextureSamplerHandleARB,
  glGetTextureSamplerHandleIMG,
  glGetTextureSamplerHandleNV,
  glGetTextureSubImage,
  glGetTrackMatrixivNV,
  glGetTransformFeedbackVarying,
  glGetTransformFeedbackVaryingEXT,
  glGetTransformFeedbackVaryingNV,
  glGetTransformFeedbacki64_v,
  glGetTransformFeedbacki_v,
  glGetTransformFeedbackiv,
  glGetTranslatedShaderSourceANGLE,
  glGetUniformBlockIndex,
  glGetUniformBufferSizeEXT,
  glGetUniformIndices,
  glGetUniformLocation,
  glGetUniformLocationARB,
  glGetUniformOffsetEXT,
  glGetUniformSubroutineuiv,
  glGetUniformdv,
  glGetUniformfv,
  glGetUniformfvARB,
  glGetUniformi64vARB,
  glGetUniformi64vNV,
  glGetUniformiv,
  glGetUniformivARB,
  glGetUniformui64vARB,
  glGetUniformui64vNV,
  glGetUniformuiv,
  glGetUniformuivEXT,
  glGetUnsignedBytei_vEXT,
  glGetUnsignedBytevEXT,
  glGetVariantArrayObjectfvATI,
  glGetVariantArrayObjectivATI,
  glGetVariantBooleanvEXT,
  glGetVariantFloatvEXT,
  glGetVariantIntegervEXT,
  glGetVariantPointervEXT,
  glGetVaryingLocationNV,
  glGetVertexArrayIndexed64iv,
  glGetVertexArrayIndexediv,
  glGetVertexArrayIntegeri_vEXT,
  glGetVertexArrayIntegervEXT,
  glGetVertexArrayPointeri_vEXT,
  glGetVertexArrayPointervEXT,
  glGetVertexArrayiv,
  glGetVertexAttribArrayObjectfvATI,
  glGetVertexAttribArrayObjectivATI,
  glGetVertexAttribIiv,
  glGetVertexAttribIivEXT,
  glGetVertexAttribIuiv,
  glGetVertexAttribIuivEXT,
  glGetVertexAttribLdv,
  glGetVertexAttribLdvEXT,
  glGetVertexAttribLi64vNV,
  glGetVertexAttribLui64vARB,
  glGetVertexAttribLui64vNV,
  glGetVertexAttribPointerv,
  glGetVertexAttribPointervARB,
  glGetVertexAttribPointervNV,
  glGetVertexAttribdv,
  glGetVertexAttribdvARB,
  glGetVertexAttribdvNV,
  glGetVertexAttribfv,
  glGetVertexAttribfvARB,
  glGetVertexAttribfvNV,
  glGetVertexAttribiv,
  glGetVertexAttribivARB,
  glGetVertexAttribivNV,
  glGetVideoCaptureStreamdvNV,
  glGetVideoCaptureStreamfvNV,
  glGetVideoCaptureStreamivNV,
  glGetVideoCaptureivNV,
  glGetVideoi64vNV,
  glGetVideoivNV,
  glGetVideoui64vNV
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glGetTexParameterIuivEXT ----------------------------------------------------

-- | This command is an alias for 'glGetTexParameterIuiv'.
glGetTexParameterIuivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetTexParameterIuivEXT v1 v2 v3 = liftIO $ dyn432 ptr_glGetTexParameterIuivEXT v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIuivEXT #-}
ptr_glGetTexParameterIuivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetTexParameterIuivEXT = unsafePerformIO $ getCommand "glGetTexParameterIuivEXT"

-- glGetTexParameterIuivOES ----------------------------------------------------

-- | This command is an alias for 'glGetTexParameterIuiv'.
glGetTexParameterIuivOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetTexParameterIuivOES v1 v2 v3 = liftIO $ dyn432 ptr_glGetTexParameterIuivOES v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIuivOES #-}
ptr_glGetTexParameterIuivOES :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetTexParameterIuivOES = unsafePerformIO $ getCommand "glGetTexParameterIuivOES"

-- glGetTexParameterPointervAPPLE ----------------------------------------------

glGetTexParameterPointervAPPLE
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetTexParameterPointervAPPLE v1 v2 v3 = liftIO $ dyn330 ptr_glGetTexParameterPointervAPPLE v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterPointervAPPLE #-}
ptr_glGetTexParameterPointervAPPLE :: FunPtr (GLenum -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetTexParameterPointervAPPLE = unsafePerformIO $ getCommand "glGetTexParameterPointervAPPLE"

-- glGetTexParameterfv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTexParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexParameterfv v1 v2 v3 = liftIO $ dyn139 ptr_glGetTexParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterfv #-}
ptr_glGetTexParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexParameterfv = unsafePerformIO $ getCommand "glGetTexParameterfv"

-- glGetTexParameteriv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTexParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexParameteriv v1 v2 v3 = liftIO $ dyn140 ptr_glGetTexParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetTexParameteriv #-}
ptr_glGetTexParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexParameteriv = unsafePerformIO $ getCommand "glGetTexParameteriv"

-- glGetTexParameterxv ---------------------------------------------------------

glGetTexParameterxv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexParameterxv v1 v2 v3 = liftIO $ dyn170 ptr_glGetTexParameterxv v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterxv #-}
ptr_glGetTexParameterxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexParameterxv = unsafePerformIO $ getCommand "glGetTexParameterxv"

-- glGetTexParameterxvOES ------------------------------------------------------

glGetTexParameterxvOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexParameterxvOES v1 v2 v3 = liftIO $ dyn170 ptr_glGetTexParameterxvOES v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterxvOES #-}
ptr_glGetTexParameterxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexParameterxvOES = unsafePerformIO $ getCommand "glGetTexParameterxvOES"

-- glGetTextureHandleARB -------------------------------------------------------

glGetTextureHandleARB
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> m GLuint64
glGetTextureHandleARB v1 = liftIO $ dyn433 ptr_glGetTextureHandleARB v1

{-# NOINLINE ptr_glGetTextureHandleARB #-}
ptr_glGetTextureHandleARB :: FunPtr (GLuint -> IO GLuint64)
ptr_glGetTextureHandleARB = unsafePerformIO $ getCommand "glGetTextureHandleARB"

-- glGetTextureHandleIMG -------------------------------------------------------

-- | This command is an alias for 'glGetTextureHandleARB'.
glGetTextureHandleIMG
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> m GLuint64
glGetTextureHandleIMG v1 = liftIO $ dyn433 ptr_glGetTextureHandleIMG v1

{-# NOINLINE ptr_glGetTextureHandleIMG #-}
ptr_glGetTextureHandleIMG :: FunPtr (GLuint -> IO GLuint64)
ptr_glGetTextureHandleIMG = unsafePerformIO $ getCommand "glGetTextureHandleIMG"

-- glGetTextureHandleNV --------------------------------------------------------

glGetTextureHandleNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> m GLuint64
glGetTextureHandleNV v1 = liftIO $ dyn433 ptr_glGetTextureHandleNV v1

{-# NOINLINE ptr_glGetTextureHandleNV #-}
ptr_glGetTextureHandleNV :: FunPtr (GLuint -> IO GLuint64)
ptr_glGetTextureHandleNV = unsafePerformIO $ getCommand "glGetTextureHandleNV"

-- glGetTextureImage -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexImage.xhtml OpenGL 4.x>.
glGetTextureImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glGetTextureImage v1 v2 v3 v4 v5 v6 = liftIO $ dyn434 ptr_glGetTextureImage v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetTextureImage #-}
ptr_glGetTextureImage :: FunPtr (GLuint -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetTextureImage = unsafePerformIO $ getCommand "glGetTextureImage"

-- glGetTextureImageEXT --------------------------------------------------------

glGetTextureImageEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(target,level,format,type)@ elements of type @a@.
  -> m ()
glGetTextureImageEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn435 ptr_glGetTextureImageEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetTextureImageEXT #-}
ptr_glGetTextureImageEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetTextureImageEXT = unsafePerformIO $ getCommand "glGetTextureImageEXT"

-- glGetTextureLevelParameterfv ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml OpenGL 4.x>.
glGetTextureLevelParameterfv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@.
  -> m ()
glGetTextureLevelParameterfv v1 v2 v3 v4 = liftIO $ dyn436 ptr_glGetTextureLevelParameterfv v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureLevelParameterfv #-}
ptr_glGetTextureLevelParameterfv :: FunPtr (GLuint -> GLint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTextureLevelParameterfv = unsafePerformIO $ getCommand "glGetTextureLevelParameterfv"

-- glGetTextureLevelParameterfvEXT ---------------------------------------------

glGetTextureLevelParameterfvEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTextureLevelParameterfvEXT v1 v2 v3 v4 v5 = liftIO $ dyn437 ptr_glGetTextureLevelParameterfvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetTextureLevelParameterfvEXT #-}
ptr_glGetTextureLevelParameterfvEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTextureLevelParameterfvEXT = unsafePerformIO $ getCommand "glGetTextureLevelParameterfvEXT"

-- glGetTextureLevelParameteriv ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml OpenGL 4.x>.
glGetTextureLevelParameteriv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetTextureLevelParameteriv v1 v2 v3 v4 = liftIO $ dyn438 ptr_glGetTextureLevelParameteriv v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureLevelParameteriv #-}
ptr_glGetTextureLevelParameteriv :: FunPtr (GLuint -> GLint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureLevelParameteriv = unsafePerformIO $ getCommand "glGetTextureLevelParameteriv"

-- glGetTextureLevelParameterivEXT ---------------------------------------------

glGetTextureLevelParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTextureLevelParameterivEXT v1 v2 v3 v4 v5 = liftIO $ dyn282 ptr_glGetTextureLevelParameterivEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetTextureLevelParameterivEXT #-}
ptr_glGetTextureLevelParameterivEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureLevelParameterivEXT = unsafePerformIO $ getCommand "glGetTextureLevelParameterivEXT"

-- glGetTextureParameterIiv ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTextureParameterIiv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetTextureParameterIiv v1 v2 v3 = liftIO $ dyn348 ptr_glGetTextureParameterIiv v1 v2 v3

{-# NOINLINE ptr_glGetTextureParameterIiv #-}
ptr_glGetTextureParameterIiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureParameterIiv = unsafePerformIO $ getCommand "glGetTextureParameterIiv"

-- glGetTextureParameterIivEXT -------------------------------------------------

glGetTextureParameterIivEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTextureParameterIivEXT v1 v2 v3 v4 = liftIO $ dyn379 ptr_glGetTextureParameterIivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureParameterIivEXT #-}
ptr_glGetTextureParameterIivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureParameterIivEXT = unsafePerformIO $ getCommand "glGetTextureParameterIivEXT"

-- glGetTextureParameterIuiv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTextureParameterIuiv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLuint -- ^ @params@.
  -> m ()
glGetTextureParameterIuiv v1 v2 v3 = liftIO $ dyn392 ptr_glGetTextureParameterIuiv v1 v2 v3

{-# NOINLINE ptr_glGetTextureParameterIuiv #-}
ptr_glGetTextureParameterIuiv :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetTextureParameterIuiv = unsafePerformIO $ getCommand "glGetTextureParameterIuiv"

-- glGetTextureParameterIuivEXT ------------------------------------------------

glGetTextureParameterIuivEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetTextureParameterIuivEXT v1 v2 v3 v4 = liftIO $ dyn439 ptr_glGetTextureParameterIuivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureParameterIuivEXT #-}
ptr_glGetTextureParameterIuivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetTextureParameterIuivEXT = unsafePerformIO $ getCommand "glGetTextureParameterIuivEXT"

-- glGetTextureParameterfv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTextureParameterfv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@.
  -> m ()
glGetTextureParameterfv v1 v2 v3 = liftIO $ dyn364 ptr_glGetTextureParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetTextureParameterfv #-}
ptr_glGetTextureParameterfv :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTextureParameterfv = unsafePerformIO $ getCommand "glGetTextureParameterfv"

-- glGetTextureParameterfvEXT --------------------------------------------------

glGetTextureParameterfvEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTextureParameterfvEXT v1 v2 v3 v4 = liftIO $ dyn440 ptr_glGetTextureParameterfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureParameterfvEXT #-}
ptr_glGetTextureParameterfvEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTextureParameterfvEXT = unsafePerformIO $ getCommand "glGetTextureParameterfvEXT"

-- glGetTextureParameteriv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTextureParameteriv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetTextureParameteriv v1 v2 v3 = liftIO $ dyn348 ptr_glGetTextureParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetTextureParameteriv #-}
ptr_glGetTextureParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureParameteriv = unsafePerformIO $ getCommand "glGetTextureParameteriv"

-- glGetTextureParameterivEXT --------------------------------------------------

glGetTextureParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTextureParameterivEXT v1 v2 v3 v4 = liftIO $ dyn379 ptr_glGetTextureParameterivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureParameterivEXT #-}
ptr_glGetTextureParameterivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureParameterivEXT = unsafePerformIO $ getCommand "glGetTextureParameterivEXT"

-- glGetTextureSamplerHandleARB ------------------------------------------------

glGetTextureSamplerHandleARB
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLuint -- ^ @sampler@.
  -> m GLuint64
glGetTextureSamplerHandleARB v1 v2 = liftIO $ dyn441 ptr_glGetTextureSamplerHandleARB v1 v2

{-# NOINLINE ptr_glGetTextureSamplerHandleARB #-}
ptr_glGetTextureSamplerHandleARB :: FunPtr (GLuint -> GLuint -> IO GLuint64)
ptr_glGetTextureSamplerHandleARB = unsafePerformIO $ getCommand "glGetTextureSamplerHandleARB"

-- glGetTextureSamplerHandleIMG ------------------------------------------------

-- | This command is an alias for 'glGetTextureSamplerHandleARB'.
glGetTextureSamplerHandleIMG
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLuint -- ^ @sampler@.
  -> m GLuint64
glGetTextureSamplerHandleIMG v1 v2 = liftIO $ dyn441 ptr_glGetTextureSamplerHandleIMG v1 v2

{-# NOINLINE ptr_glGetTextureSamplerHandleIMG #-}
ptr_glGetTextureSamplerHandleIMG :: FunPtr (GLuint -> GLuint -> IO GLuint64)
ptr_glGetTextureSamplerHandleIMG = unsafePerformIO $ getCommand "glGetTextureSamplerHandleIMG"

-- glGetTextureSamplerHandleNV -------------------------------------------------

glGetTextureSamplerHandleNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLuint -- ^ @sampler@.
  -> m GLuint64
glGetTextureSamplerHandleNV v1 v2 = liftIO $ dyn441 ptr_glGetTextureSamplerHandleNV v1 v2

{-# NOINLINE ptr_glGetTextureSamplerHandleNV #-}
ptr_glGetTextureSamplerHandleNV :: FunPtr (GLuint -> GLuint -> IO GLuint64)
ptr_glGetTextureSamplerHandleNV = unsafePerformIO $ getCommand "glGetTextureSamplerHandleNV"

-- glGetTextureSubImage --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTextureSubImage.xhtml OpenGL 4.x>.
glGetTextureSubImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glGetTextureSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn442 ptr_glGetTextureSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glGetTextureSubImage #-}
ptr_glGetTextureSubImage :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetTextureSubImage = unsafePerformIO $ getCommand "glGetTextureSubImage"

-- glGetTrackMatrixivNV --------------------------------------------------------

glGetTrackMatrixivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @address@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetTrackMatrixivNV v1 v2 v3 v4 = liftIO $ dyn366 ptr_glGetTrackMatrixivNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetTrackMatrixivNV #-}
ptr_glGetTrackMatrixivNV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTrackMatrixivNV = unsafePerformIO $ getCommand "glGetTrackMatrixivNV"

-- glGetTransformFeedbackVarying -----------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTransformFeedbackVarying.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedbackVarying.xhtml OpenGL 4.x>.
glGetTransformFeedbackVarying
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLsizei -- ^ @size@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type [GlslTypeToken](Graphics-GL-Groups.html#GlslTypeToken).
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetTransformFeedbackVarying v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn321 ptr_glGetTransformFeedbackVarying v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetTransformFeedbackVarying #-}
ptr_glGetTransformFeedbackVarying :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ())
ptr_glGetTransformFeedbackVarying = unsafePerformIO $ getCommand "glGetTransformFeedbackVarying"

-- glGetTransformFeedbackVaryingEXT --------------------------------------------

-- | This command is an alias for 'glGetTransformFeedbackVarying'.
glGetTransformFeedbackVaryingEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLsizei -- ^ @size@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type [GlslTypeToken](Graphics-GL-Groups.html#GlslTypeToken).
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetTransformFeedbackVaryingEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn321 ptr_glGetTransformFeedbackVaryingEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetTransformFeedbackVaryingEXT #-}
ptr_glGetTransformFeedbackVaryingEXT :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ())
ptr_glGetTransformFeedbackVaryingEXT = unsafePerformIO $ getCommand "glGetTransformFeedbackVaryingEXT"

-- glGetTransformFeedbackVaryingNV ---------------------------------------------

glGetTransformFeedbackVaryingNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @location@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetTransformFeedbackVaryingNV v1 v2 v3 = liftIO $ dyn443 ptr_glGetTransformFeedbackVaryingNV v1 v2 v3

{-# NOINLINE ptr_glGetTransformFeedbackVaryingNV #-}
ptr_glGetTransformFeedbackVaryingNV :: FunPtr (GLuint -> GLuint -> Ptr GLint -> IO ())
ptr_glGetTransformFeedbackVaryingNV = unsafePerformIO $ getCommand "glGetTransformFeedbackVaryingNV"

-- glGetTransformFeedbacki64_v -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedback.xhtml OpenGL 4.x>.
glGetTransformFeedbacki64_v
  :: MonadIO m
  => GLuint -- ^ @xfb@.
  -> GLenum -- ^ @pname@ of type [TransformFeedbackPName](Graphics-GL-Groups.html#TransformFeedbackPName).
  -> GLuint -- ^ @index@.
  -> Ptr GLint64 -- ^ @param@.
  -> m ()
glGetTransformFeedbacki64_v v1 v2 v3 v4 = liftIO $ dyn444 ptr_glGetTransformFeedbacki64_v v1 v2 v3 v4

{-# NOINLINE ptr_glGetTransformFeedbacki64_v #-}
ptr_glGetTransformFeedbacki64_v :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint64 -> IO ())
ptr_glGetTransformFeedbacki64_v = unsafePerformIO $ getCommand "glGetTransformFeedbacki64_v"

-- glGetTransformFeedbacki_v ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedback.xhtml OpenGL 4.x>.
glGetTransformFeedbacki_v
  :: MonadIO m
  => GLuint -- ^ @xfb@.
  -> GLenum -- ^ @pname@ of type [TransformFeedbackPName](Graphics-GL-Groups.html#TransformFeedbackPName).
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetTransformFeedbacki_v v1 v2 v3 v4 = liftIO $ dyn381 ptr_glGetTransformFeedbacki_v v1 v2 v3 v4

{-# NOINLINE ptr_glGetTransformFeedbacki_v #-}
ptr_glGetTransformFeedbacki_v :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glGetTransformFeedbacki_v = unsafePerformIO $ getCommand "glGetTransformFeedbacki_v"

-- glGetTransformFeedbackiv ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedback.xhtml OpenGL 4.x>.
glGetTransformFeedbackiv
  :: MonadIO m
  => GLuint -- ^ @xfb@.
  -> GLenum -- ^ @pname@ of type [TransformFeedbackPName](Graphics-GL-Groups.html#TransformFeedbackPName).
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetTransformFeedbackiv v1 v2 v3 = liftIO $ dyn348 ptr_glGetTransformFeedbackiv v1 v2 v3

{-# NOINLINE ptr_glGetTransformFeedbackiv #-}
ptr_glGetTransformFeedbackiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTransformFeedbackiv = unsafePerformIO $ getCommand "glGetTransformFeedbackiv"

-- glGetTranslatedShaderSourceANGLE --------------------------------------------

glGetTranslatedShaderSourceANGLE
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @source@.
  -> m ()
glGetTranslatedShaderSourceANGLE v1 v2 v3 v4 = liftIO $ dyn345 ptr_glGetTranslatedShaderSourceANGLE v1 v2 v3 v4

{-# NOINLINE ptr_glGetTranslatedShaderSourceANGLE #-}
ptr_glGetTranslatedShaderSourceANGLE :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetTranslatedShaderSourceANGLE = unsafePerformIO $ getCommand "glGetTranslatedShaderSourceANGLE"

-- glGetUniformBlockIndex ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniformBlockIndex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniformBlockIndex.xhtml OpenGL 4.x>.
glGetUniformBlockIndex
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @uniformBlockName@ pointing to @COMPSIZE()@ elements of type @GLchar@.
  -> m GLuint
glGetUniformBlockIndex v1 v2 = liftIO $ dyn445 ptr_glGetUniformBlockIndex v1 v2

{-# NOINLINE ptr_glGetUniformBlockIndex #-}
ptr_glGetUniformBlockIndex :: FunPtr (GLuint -> Ptr GLchar -> IO GLuint)
ptr_glGetUniformBlockIndex = unsafePerformIO $ getCommand "glGetUniformBlockIndex"

-- glGetUniformBufferSizeEXT ---------------------------------------------------

glGetUniformBufferSizeEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> m GLint
glGetUniformBufferSizeEXT v1 v2 = liftIO $ dyn446 ptr_glGetUniformBufferSizeEXT v1 v2

{-# NOINLINE ptr_glGetUniformBufferSizeEXT #-}
ptr_glGetUniformBufferSizeEXT :: FunPtr (GLuint -> GLint -> IO GLint)
ptr_glGetUniformBufferSizeEXT = unsafePerformIO $ getCommand "glGetUniformBufferSizeEXT"

-- glGetUniformIndices ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniformIndices.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniformIndices.xhtml OpenGL 4.x>.
glGetUniformIndices
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @uniformCount@.
  -> Ptr (Ptr GLchar) -- ^ @uniformNames@ pointing to @COMPSIZE(uniformCount)@ elements of type @Ptr GLchar@.
  -> Ptr GLuint -- ^ @uniformIndices@ pointing to @COMPSIZE(uniformCount)@ elements of type @GLuint@.
  -> m ()
glGetUniformIndices v1 v2 v3 v4 = liftIO $ dyn447 ptr_glGetUniformIndices v1 v2 v3 v4

{-# NOINLINE ptr_glGetUniformIndices #-}
ptr_glGetUniformIndices :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLuint -> IO ())
ptr_glGetUniformIndices = unsafePerformIO $ getCommand "glGetUniformIndices"

-- glGetUniformLocation --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetUniformLocation.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniformLocation.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniformLocation.xhtml OpenGL 4.x>.
glGetUniformLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@.
  -> m GLint
glGetUniformLocation v1 v2 = liftIO $ dyn324 ptr_glGetUniformLocation v1 v2

{-# NOINLINE ptr_glGetUniformLocation #-}
ptr_glGetUniformLocation :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
ptr_glGetUniformLocation = unsafePerformIO $ getCommand "glGetUniformLocation"

-- glGetUniformLocationARB -----------------------------------------------------

-- | This command is an alias for 'glGetUniformLocation'.
glGetUniformLocationARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> Ptr GLcharARB -- ^ @name@.
  -> m GLint
glGetUniformLocationARB v1 v2 = liftIO $ dyn325 ptr_glGetUniformLocationARB v1 v2

{-# NOINLINE ptr_glGetUniformLocationARB #-}
ptr_glGetUniformLocationARB :: FunPtr (GLhandleARB -> Ptr GLcharARB -> IO GLint)
ptr_glGetUniformLocationARB = unsafePerformIO $ getCommand "glGetUniformLocationARB"

-- glGetUniformOffsetEXT -------------------------------------------------------

glGetUniformOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> m GLintptr -- ^ of type @BufferOffset@.
glGetUniformOffsetEXT v1 v2 = liftIO $ dyn448 ptr_glGetUniformOffsetEXT v1 v2

{-# NOINLINE ptr_glGetUniformOffsetEXT #-}
ptr_glGetUniformOffsetEXT :: FunPtr (GLuint -> GLint -> IO GLintptr)
ptr_glGetUniformOffsetEXT = unsafePerformIO $ getCommand "glGetUniformOffsetEXT"

-- glGetUniformSubroutineuiv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetUniformSubroutine.xhtml OpenGL 4.x>.
glGetUniformSubroutineuiv
  :: MonadIO m
  => GLenum -- ^ @shadertype@ of type [ShaderType](Graphics-GL-Groups.html#ShaderType).
  -> GLint -- ^ @location@.
  -> Ptr GLuint -- ^ @params@ pointing to @1@ element of type @GLuint@.
  -> m ()
glGetUniformSubroutineuiv v1 v2 v3 = liftIO $ dyn81 ptr_glGetUniformSubroutineuiv v1 v2 v3

{-# NOINLINE ptr_glGetUniformSubroutineuiv #-}
ptr_glGetUniformSubroutineuiv :: FunPtr (GLenum -> GLint -> Ptr GLuint -> IO ())
ptr_glGetUniformSubroutineuiv = unsafePerformIO $ getCommand "glGetUniformSubroutineuiv"

-- glGetUniformdv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetUniformdv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLdouble@.
  -> m ()
glGetUniformdv v1 v2 v3 = liftIO $ dyn449 ptr_glGetUniformdv v1 v2 v3

{-# NOINLINE ptr_glGetUniformdv #-}
ptr_glGetUniformdv :: FunPtr (GLuint -> GLint -> Ptr GLdouble -> IO ())
ptr_glGetUniformdv = unsafePerformIO $ getCommand "glGetUniformdv"

-- glGetUniformfv --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetUniformfv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLfloat@.
  -> m ()
glGetUniformfv v1 v2 v3 = liftIO $ dyn450 ptr_glGetUniformfv v1 v2 v3

{-# NOINLINE ptr_glGetUniformfv #-}
ptr_glGetUniformfv :: FunPtr (GLuint -> GLint -> Ptr GLfloat -> IO ())
ptr_glGetUniformfv = unsafePerformIO $ getCommand "glGetUniformfv"

-- glGetUniformfvARB -----------------------------------------------------------

-- | This command is an alias for 'glGetUniformfv'.
glGetUniformfvARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> GLint -- ^ @location@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(programObj,location)@ elements of type @GLfloat@.
  -> m ()
glGetUniformfvARB v1 v2 v3 = liftIO $ dyn451 ptr_glGetUniformfvARB v1 v2 v3

{-# NOINLINE ptr_glGetUniformfvARB #-}
ptr_glGetUniformfvARB :: FunPtr (GLhandleARB -> GLint -> Ptr GLfloat -> IO ())
ptr_glGetUniformfvARB = unsafePerformIO $ getCommand "glGetUniformfvARB"

-- glGetUniformi64vARB ---------------------------------------------------------

glGetUniformi64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLint64 -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLint64@.
  -> m ()
glGetUniformi64vARB v1 v2 v3 = liftIO $ dyn452 ptr_glGetUniformi64vARB v1 v2 v3

{-# NOINLINE ptr_glGetUniformi64vARB #-}
ptr_glGetUniformi64vARB :: FunPtr (GLuint -> GLint -> Ptr GLint64 -> IO ())
ptr_glGetUniformi64vARB = unsafePerformIO $ getCommand "glGetUniformi64vARB"

-- glGetUniformi64vNV ----------------------------------------------------------

glGetUniformi64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLint64EXT -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLint64EXT@.
  -> m ()
glGetUniformi64vNV v1 v2 v3 = liftIO $ dyn453 ptr_glGetUniformi64vNV v1 v2 v3

{-# NOINLINE ptr_glGetUniformi64vNV #-}
ptr_glGetUniformi64vNV :: FunPtr (GLuint -> GLint -> Ptr GLint64EXT -> IO ())
ptr_glGetUniformi64vNV = unsafePerformIO $ getCommand "glGetUniformi64vNV"

-- glGetUniformiv --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetUniformiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLint@.
  -> m ()
glGetUniformiv v1 v2 v3 = liftIO $ dyn454 ptr_glGetUniformiv v1 v2 v3

{-# NOINLINE ptr_glGetUniformiv #-}
ptr_glGetUniformiv :: FunPtr (GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glGetUniformiv = unsafePerformIO $ getCommand "glGetUniformiv"

-- glGetUniformivARB -----------------------------------------------------------

-- | This command is an alias for 'glGetUniformiv'.
glGetUniformivARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> GLint -- ^ @location@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(programObj,location)@ elements of type @GLint@.
  -> m ()
glGetUniformivARB v1 v2 v3 = liftIO $ dyn455 ptr_glGetUniformivARB v1 v2 v3

{-# NOINLINE ptr_glGetUniformivARB #-}
ptr_glGetUniformivARB :: FunPtr (GLhandleARB -> GLint -> Ptr GLint -> IO ())
ptr_glGetUniformivARB = unsafePerformIO $ getCommand "glGetUniformivARB"

-- glGetUniformui64vARB --------------------------------------------------------

glGetUniformui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLuint64 -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLuint64@.
  -> m ()
glGetUniformui64vARB v1 v2 v3 = liftIO $ dyn456 ptr_glGetUniformui64vARB v1 v2 v3

{-# NOINLINE ptr_glGetUniformui64vARB #-}
ptr_glGetUniformui64vARB :: FunPtr (GLuint -> GLint -> Ptr GLuint64 -> IO ())
ptr_glGetUniformui64vARB = unsafePerformIO $ getCommand "glGetUniformui64vARB"

-- glGetUniformui64vNV ---------------------------------------------------------

glGetUniformui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLuint64EXT -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLuint64EXT@.
  -> m ()
glGetUniformui64vNV v1 v2 v3 = liftIO $ dyn457 ptr_glGetUniformui64vNV v1 v2 v3

{-# NOINLINE ptr_glGetUniformui64vNV #-}
ptr_glGetUniformui64vNV :: FunPtr (GLuint -> GLint -> Ptr GLuint64EXT -> IO ())
ptr_glGetUniformui64vNV = unsafePerformIO $ getCommand "glGetUniformui64vNV"

-- glGetUniformuiv -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetUniformuiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLuint@.
  -> m ()
glGetUniformuiv v1 v2 v3 = liftIO $ dyn458 ptr_glGetUniformuiv v1 v2 v3

{-# NOINLINE ptr_glGetUniformuiv #-}
ptr_glGetUniformuiv :: FunPtr (GLuint -> GLint -> Ptr GLuint -> IO ())
ptr_glGetUniformuiv = unsafePerformIO $ getCommand "glGetUniformuiv"

-- glGetUniformuivEXT ----------------------------------------------------------

-- | This command is an alias for 'glGetUniformuiv'.
glGetUniformuivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLuint@.
  -> m ()
glGetUniformuivEXT v1 v2 v3 = liftIO $ dyn458 ptr_glGetUniformuivEXT v1 v2 v3

{-# NOINLINE ptr_glGetUniformuivEXT #-}
ptr_glGetUniformuivEXT :: FunPtr (GLuint -> GLint -> Ptr GLuint -> IO ())
ptr_glGetUniformuivEXT = unsafePerformIO $ getCommand "glGetUniformuivEXT"

-- glGetUnsignedBytei_vEXT -----------------------------------------------------

glGetUnsignedBytei_vEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type @GLubyte@.
  -> m ()
glGetUnsignedBytei_vEXT v1 v2 v3 = liftIO $ dyn459 ptr_glGetUnsignedBytei_vEXT v1 v2 v3

{-# NOINLINE ptr_glGetUnsignedBytei_vEXT #-}
ptr_glGetUnsignedBytei_vEXT :: FunPtr (GLenum -> GLuint -> Ptr GLubyte -> IO ())
ptr_glGetUnsignedBytei_vEXT = unsafePerformIO $ getCommand "glGetUnsignedBytei_vEXT"

-- glGetUnsignedBytevEXT -------------------------------------------------------

glGetUnsignedBytevEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetPName](Graphics-GL-Groups.html#GetPName).
  -> Ptr GLubyte -- ^ @data@ pointing to @COMPSIZE(pname)@ elements of type @GLubyte@.
  -> m ()
glGetUnsignedBytevEXT v1 v2 = liftIO $ dyn460 ptr_glGetUnsignedBytevEXT v1 v2

{-# NOINLINE ptr_glGetUnsignedBytevEXT #-}
ptr_glGetUnsignedBytevEXT :: FunPtr (GLenum -> Ptr GLubyte -> IO ())
ptr_glGetUnsignedBytevEXT = unsafePerformIO $ getCommand "glGetUnsignedBytevEXT"

-- glGetVariantArrayObjectfvATI ------------------------------------------------

glGetVariantArrayObjectfvATI
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [ArrayObjectPNameATI](Graphics-GL-Groups.html#ArrayObjectPNameATI).
  -> Ptr GLfloat -- ^ @params@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glGetVariantArrayObjectfvATI v1 v2 v3 = liftIO $ dyn364 ptr_glGetVariantArrayObjectfvATI v1 v2 v3

{-# NOINLINE ptr_glGetVariantArrayObjectfvATI #-}
ptr_glGetVariantArrayObjectfvATI :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVariantArrayObjectfvATI = unsafePerformIO $ getCommand "glGetVariantArrayObjectfvATI"

-- glGetVariantArrayObjectivATI ------------------------------------------------

glGetVariantArrayObjectivATI
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type [ArrayObjectPNameATI](Graphics-GL-Groups.html#ArrayObjectPNameATI).
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetVariantArrayObjectivATI v1 v2 v3 = liftIO $ dyn348 ptr_glGetVariantArrayObjectivATI v1 v2 v3

{-# NOINLINE ptr_glGetVariantArrayObjectivATI #-}
ptr_glGetVariantArrayObjectivATI :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVariantArrayObjectivATI = unsafePerformIO $ getCommand "glGetVariantArrayObjectivATI"

-- glGetVariantBooleanvEXT -----------------------------------------------------

glGetVariantBooleanvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type [GetVariantValueEXT](Graphics-GL-Groups.html#GetVariantValueEXT).
  -> Ptr GLboolean -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glGetVariantBooleanvEXT v1 v2 v3 = liftIO $ dyn363 ptr_glGetVariantBooleanvEXT v1 v2 v3

{-# NOINLINE ptr_glGetVariantBooleanvEXT #-}
ptr_glGetVariantBooleanvEXT :: FunPtr (GLuint -> GLenum -> Ptr GLboolean -> IO ())
ptr_glGetVariantBooleanvEXT = unsafePerformIO $ getCommand "glGetVariantBooleanvEXT"

-- glGetVariantFloatvEXT -------------------------------------------------------

glGetVariantFloatvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type [GetVariantValueEXT](Graphics-GL-Groups.html#GetVariantValueEXT).
  -> Ptr GLfloat -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type @GLfloat@.
  -> m ()
glGetVariantFloatvEXT v1 v2 v3 = liftIO $ dyn364 ptr_glGetVariantFloatvEXT v1 v2 v3

{-# NOINLINE ptr_glGetVariantFloatvEXT #-}
ptr_glGetVariantFloatvEXT :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVariantFloatvEXT = unsafePerformIO $ getCommand "glGetVariantFloatvEXT"

-- glGetVariantIntegervEXT -----------------------------------------------------

glGetVariantIntegervEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type [GetVariantValueEXT](Graphics-GL-Groups.html#GetVariantValueEXT).
  -> Ptr GLint -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type @GLint@.
  -> m ()
glGetVariantIntegervEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetVariantIntegervEXT v1 v2 v3

{-# NOINLINE ptr_glGetVariantIntegervEXT #-}
ptr_glGetVariantIntegervEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVariantIntegervEXT = unsafePerformIO $ getCommand "glGetVariantIntegervEXT"

-- glGetVariantPointervEXT -----------------------------------------------------

glGetVariantPointervEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type [GetVariantValueEXT](Graphics-GL-Groups.html#GetVariantValueEXT).
  -> Ptr (Ptr a) -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type @Ptr a@.
  -> m ()
glGetVariantPointervEXT v1 v2 v3 = liftIO $ dyn377 ptr_glGetVariantPointervEXT v1 v2 v3

{-# NOINLINE ptr_glGetVariantPointervEXT #-}
ptr_glGetVariantPointervEXT :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVariantPointervEXT = unsafePerformIO $ getCommand "glGetVariantPointervEXT"

-- glGetVaryingLocationNV ------------------------------------------------------

glGetVaryingLocationNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLint
glGetVaryingLocationNV v1 v2 = liftIO $ dyn324 ptr_glGetVaryingLocationNV v1 v2

{-# NOINLINE ptr_glGetVaryingLocationNV #-}
ptr_glGetVaryingLocationNV :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
ptr_glGetVaryingLocationNV = unsafePerformIO $ getCommand "glGetVaryingLocationNV"

-- glGetVertexArrayIndexed64iv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetVertexArrayIndexed.xhtml OpenGL 4.x>.
glGetVertexArrayIndexed64iv
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexArrayPName](Graphics-GL-Groups.html#VertexArrayPName).
  -> Ptr GLint64 -- ^ @param@.
  -> m ()
glGetVertexArrayIndexed64iv v1 v2 v3 v4 = liftIO $ dyn461 ptr_glGetVertexArrayIndexed64iv v1 v2 v3 v4

{-# NOINLINE ptr_glGetVertexArrayIndexed64iv #-}
ptr_glGetVertexArrayIndexed64iv :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint64 -> IO ())
ptr_glGetVertexArrayIndexed64iv = unsafePerformIO $ getCommand "glGetVertexArrayIndexed64iv"

-- glGetVertexArrayIndexediv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetVertexArrayIndexed.xhtml OpenGL 4.x>.
glGetVertexArrayIndexediv
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexArrayPName](Graphics-GL-Groups.html#VertexArrayPName).
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetVertexArrayIndexediv v1 v2 v3 v4 = liftIO $ dyn314 ptr_glGetVertexArrayIndexediv v1 v2 v3 v4

{-# NOINLINE ptr_glGetVertexArrayIndexediv #-}
ptr_glGetVertexArrayIndexediv :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexArrayIndexediv = unsafePerformIO $ getCommand "glGetVertexArrayIndexediv"

-- glGetVertexArrayIntegeri_vEXT -----------------------------------------------

glGetVertexArrayIntegeri_vEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexArrayPName](Graphics-GL-Groups.html#VertexArrayPName).
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetVertexArrayIntegeri_vEXT v1 v2 v3 v4 = liftIO $ dyn314 ptr_glGetVertexArrayIntegeri_vEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetVertexArrayIntegeri_vEXT #-}
ptr_glGetVertexArrayIntegeri_vEXT :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexArrayIntegeri_vEXT = unsafePerformIO $ getCommand "glGetVertexArrayIntegeri_vEXT"

-- glGetVertexArrayIntegervEXT -------------------------------------------------

glGetVertexArrayIntegervEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLenum -- ^ @pname@ of type [VertexArrayPName](Graphics-GL-Groups.html#VertexArrayPName).
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetVertexArrayIntegervEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetVertexArrayIntegervEXT v1 v2 v3

{-# NOINLINE ptr_glGetVertexArrayIntegervEXT #-}
ptr_glGetVertexArrayIntegervEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexArrayIntegervEXT = unsafePerformIO $ getCommand "glGetVertexArrayIntegervEXT"

-- glGetVertexArrayPointeri_vEXT -----------------------------------------------

glGetVertexArrayPointeri_vEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexArrayPName](Graphics-GL-Groups.html#VertexArrayPName).
  -> Ptr (Ptr a) -- ^ @param@.
  -> m ()
glGetVertexArrayPointeri_vEXT v1 v2 v3 v4 = liftIO $ dyn462 ptr_glGetVertexArrayPointeri_vEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetVertexArrayPointeri_vEXT #-}
ptr_glGetVertexArrayPointeri_vEXT :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVertexArrayPointeri_vEXT = unsafePerformIO $ getCommand "glGetVertexArrayPointeri_vEXT"

-- glGetVertexArrayPointervEXT -------------------------------------------------

glGetVertexArrayPointervEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLenum -- ^ @pname@ of type [VertexArrayPName](Graphics-GL-Groups.html#VertexArrayPName).
  -> Ptr (Ptr a) -- ^ @param@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetVertexArrayPointervEXT v1 v2 v3 = liftIO $ dyn377 ptr_glGetVertexArrayPointervEXT v1 v2 v3

{-# NOINLINE ptr_glGetVertexArrayPointervEXT #-}
ptr_glGetVertexArrayPointervEXT :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVertexArrayPointervEXT = unsafePerformIO $ getCommand "glGetVertexArrayPointervEXT"

-- glGetVertexArrayiv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetVertexArrayiv.xhtml OpenGL 4.x>.
glGetVertexArrayiv
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLenum -- ^ @pname@ of type [VertexArrayPName](Graphics-GL-Groups.html#VertexArrayPName).
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetVertexArrayiv v1 v2 v3 = liftIO $ dyn348 ptr_glGetVertexArrayiv v1 v2 v3

{-# NOINLINE ptr_glGetVertexArrayiv #-}
ptr_glGetVertexArrayiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexArrayiv = unsafePerformIO $ getCommand "glGetVertexArrayiv"

-- glGetVertexAttribArrayObjectfvATI -------------------------------------------

glGetVertexAttribArrayObjectfvATI
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [ArrayObjectPNameATI](Graphics-GL-Groups.html#ArrayObjectPNameATI).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetVertexAttribArrayObjectfvATI v1 v2 v3 = liftIO $ dyn364 ptr_glGetVertexAttribArrayObjectfvATI v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribArrayObjectfvATI #-}
ptr_glGetVertexAttribArrayObjectfvATI :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVertexAttribArrayObjectfvATI = unsafePerformIO $ getCommand "glGetVertexAttribArrayObjectfvATI"

-- glGetVertexAttribArrayObjectivATI -------------------------------------------

glGetVertexAttribArrayObjectivATI
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [ArrayObjectPNameATI](Graphics-GL-Groups.html#ArrayObjectPNameATI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetVertexAttribArrayObjectivATI v1 v2 v3 = liftIO $ dyn348 ptr_glGetVertexAttribArrayObjectivATI v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribArrayObjectivATI #-}
ptr_glGetVertexAttribArrayObjectivATI :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribArrayObjectivATI = unsafePerformIO $ getCommand "glGetVertexAttribArrayObjectivATI"

-- glGetVertexAttribIiv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribIiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnum](Graphics-GL-Groups.html#VertexAttribEnum).
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetVertexAttribIiv v1 v2 v3 = liftIO $ dyn348 ptr_glGetVertexAttribIiv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribIiv #-}
ptr_glGetVertexAttribIiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribIiv = unsafePerformIO $ getCommand "glGetVertexAttribIiv"

-- glGetVertexAttribIivEXT -----------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribIiv'.
glGetVertexAttribIivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnum](Graphics-GL-Groups.html#VertexAttribEnum).
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetVertexAttribIivEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetVertexAttribIivEXT v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribIivEXT #-}
ptr_glGetVertexAttribIivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribIivEXT = unsafePerformIO $ getCommand "glGetVertexAttribIivEXT"

-- glGetVertexAttribIuiv -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribIuiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnum](Graphics-GL-Groups.html#VertexAttribEnum).
  -> Ptr GLuint -- ^ @params@ pointing to @1@ element of type @GLuint@.
  -> m ()
glGetVertexAttribIuiv v1 v2 v3 = liftIO $ dyn392 ptr_glGetVertexAttribIuiv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribIuiv #-}
ptr_glGetVertexAttribIuiv :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetVertexAttribIuiv = unsafePerformIO $ getCommand "glGetVertexAttribIuiv"

-- glGetVertexAttribIuivEXT ----------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribIuiv'.
glGetVertexAttribIuivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnum](Graphics-GL-Groups.html#VertexAttribEnum).
  -> Ptr GLuint -- ^ @params@ pointing to @1@ element of type @GLuint@.
  -> m ()
glGetVertexAttribIuivEXT v1 v2 v3 = liftIO $ dyn392 ptr_glGetVertexAttribIuivEXT v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribIuivEXT #-}
ptr_glGetVertexAttribIuivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetVertexAttribIuivEXT = unsafePerformIO $ getCommand "glGetVertexAttribIuivEXT"

-- glGetVertexAttribLdv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribLdv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnum](Graphics-GL-Groups.html#VertexAttribEnum).
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetVertexAttribLdv v1 v2 v3 = liftIO $ dyn463 ptr_glGetVertexAttribLdv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribLdv #-}
ptr_glGetVertexAttribLdv :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVertexAttribLdv = unsafePerformIO $ getCommand "glGetVertexAttribLdv"

-- glGetVertexAttribLdvEXT -----------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribLdv'.
glGetVertexAttribLdvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnum](Graphics-GL-Groups.html#VertexAttribEnum).
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetVertexAttribLdvEXT v1 v2 v3 = liftIO $ dyn463 ptr_glGetVertexAttribLdvEXT v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribLdvEXT #-}
ptr_glGetVertexAttribLdvEXT :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVertexAttribLdvEXT = unsafePerformIO $ getCommand "glGetVertexAttribLdvEXT"

-- glGetVertexAttribLi64vNV ----------------------------------------------------

glGetVertexAttribLi64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnum](Graphics-GL-Groups.html#VertexAttribEnum).
  -> Ptr GLint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint64EXT@.
  -> m ()
glGetVertexAttribLi64vNV v1 v2 v3 = liftIO $ dyn464 ptr_glGetVertexAttribLi64vNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribLi64vNV #-}
ptr_glGetVertexAttribLi64vNV :: FunPtr (GLuint -> GLenum -> Ptr GLint64EXT -> IO ())
ptr_glGetVertexAttribLi64vNV = unsafePerformIO $ getCommand "glGetVertexAttribLi64vNV"

-- glGetVertexAttribLui64vARB --------------------------------------------------

glGetVertexAttribLui64vARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnum](Graphics-GL-Groups.html#VertexAttribEnum).
  -> Ptr GLuint64EXT -- ^ @params@.
  -> m ()
glGetVertexAttribLui64vARB v1 v2 v3 = liftIO $ dyn376 ptr_glGetVertexAttribLui64vARB v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribLui64vARB #-}
ptr_glGetVertexAttribLui64vARB :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
ptr_glGetVertexAttribLui64vARB = unsafePerformIO $ getCommand "glGetVertexAttribLui64vARB"

-- glGetVertexAttribLui64vNV ---------------------------------------------------

glGetVertexAttribLui64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnum](Graphics-GL-Groups.html#VertexAttribEnum).
  -> Ptr GLuint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64EXT@.
  -> m ()
glGetVertexAttribLui64vNV v1 v2 v3 = liftIO $ dyn376 ptr_glGetVertexAttribLui64vNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribLui64vNV #-}
ptr_glGetVertexAttribLui64vNV :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
ptr_glGetVertexAttribLui64vNV = unsafePerformIO $ getCommand "glGetVertexAttribLui64vNV"

-- glGetVertexAttribPointerv ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetVertexAttribPointerv.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttribPointerv.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttribPointerv.xhtml OpenGL 4.x>.
glGetVertexAttribPointerv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribPointerPropertyARB](Graphics-GL-Groups.html#VertexAttribPointerPropertyARB).
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetVertexAttribPointerv v1 v2 v3 = liftIO $ dyn377 ptr_glGetVertexAttribPointerv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribPointerv #-}
ptr_glGetVertexAttribPointerv :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVertexAttribPointerv = unsafePerformIO $ getCommand "glGetVertexAttribPointerv"

-- glGetVertexAttribPointervARB ------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribPointerv'.
glGetVertexAttribPointervARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribPointerPropertyARB](Graphics-GL-Groups.html#VertexAttribPointerPropertyARB).
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetVertexAttribPointervARB v1 v2 v3 = liftIO $ dyn377 ptr_glGetVertexAttribPointervARB v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribPointervARB #-}
ptr_glGetVertexAttribPointervARB :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVertexAttribPointervARB = unsafePerformIO $ getCommand "glGetVertexAttribPointervARB"

-- glGetVertexAttribPointervNV -------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribPointerv'.
glGetVertexAttribPointervNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetVertexAttribPointervNV v1 v2 v3 = liftIO $ dyn377 ptr_glGetVertexAttribPointervNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribPointervNV #-}
ptr_glGetVertexAttribPointervNV :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVertexAttribPointervNV = unsafePerformIO $ getCommand "glGetVertexAttribPointervNV"

-- glGetVertexAttribdv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribdv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribPropertyARB](Graphics-GL-Groups.html#VertexAttribPropertyARB).
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetVertexAttribdv v1 v2 v3 = liftIO $ dyn463 ptr_glGetVertexAttribdv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribdv #-}
ptr_glGetVertexAttribdv :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVertexAttribdv = unsafePerformIO $ getCommand "glGetVertexAttribdv"

-- glGetVertexAttribdvARB ------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribdv'.
glGetVertexAttribdvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribPropertyARB](Graphics-GL-Groups.html#VertexAttribPropertyARB).
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetVertexAttribdvARB v1 v2 v3 = liftIO $ dyn463 ptr_glGetVertexAttribdvARB v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribdvARB #-}
ptr_glGetVertexAttribdvARB :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVertexAttribdvARB = unsafePerformIO $ getCommand "glGetVertexAttribdvARB"

-- glGetVertexAttribdvNV -------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribdv'.
glGetVertexAttribdvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> Ptr GLdouble -- ^ @params@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glGetVertexAttribdvNV v1 v2 v3 = liftIO $ dyn463 ptr_glGetVertexAttribdvNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribdvNV #-}
ptr_glGetVertexAttribdvNV :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVertexAttribdvNV = unsafePerformIO $ getCommand "glGetVertexAttribdvNV"

-- glGetVertexAttribfv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribfv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribPropertyARB](Graphics-GL-Groups.html#VertexAttribPropertyARB).
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetVertexAttribfv v1 v2 v3 = liftIO $ dyn364 ptr_glGetVertexAttribfv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribfv #-}
ptr_glGetVertexAttribfv :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVertexAttribfv = unsafePerformIO $ getCommand "glGetVertexAttribfv"

-- glGetVertexAttribfvARB ------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribfv'.
glGetVertexAttribfvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribPropertyARB](Graphics-GL-Groups.html#VertexAttribPropertyARB).
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetVertexAttribfvARB v1 v2 v3 = liftIO $ dyn364 ptr_glGetVertexAttribfvARB v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribfvARB #-}
ptr_glGetVertexAttribfvARB :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVertexAttribfvARB = unsafePerformIO $ getCommand "glGetVertexAttribfvARB"

-- glGetVertexAttribfvNV -------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribfv'.
glGetVertexAttribfvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> Ptr GLfloat -- ^ @params@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glGetVertexAttribfvNV v1 v2 v3 = liftIO $ dyn364 ptr_glGetVertexAttribfvNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribfvNV #-}
ptr_glGetVertexAttribfvNV :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVertexAttribfvNV = unsafePerformIO $ getCommand "glGetVertexAttribfvNV"

-- glGetVertexAttribiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribPropertyARB](Graphics-GL-Groups.html#VertexAttribPropertyARB).
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetVertexAttribiv v1 v2 v3 = liftIO $ dyn348 ptr_glGetVertexAttribiv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribiv #-}
ptr_glGetVertexAttribiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribiv = unsafePerformIO $ getCommand "glGetVertexAttribiv"

-- glGetVertexAttribivARB ------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribiv'.
glGetVertexAttribivARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribPropertyARB](Graphics-GL-Groups.html#VertexAttribPropertyARB).
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetVertexAttribivARB v1 v2 v3 = liftIO $ dyn348 ptr_glGetVertexAttribivARB v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribivARB #-}
ptr_glGetVertexAttribivARB :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribivARB = unsafePerformIO $ getCommand "glGetVertexAttribivARB"

-- glGetVertexAttribivNV -------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribiv'.
glGetVertexAttribivNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetVertexAttribivNV v1 v2 v3 = liftIO $ dyn348 ptr_glGetVertexAttribivNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribivNV #-}
ptr_glGetVertexAttribivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribivNV = unsafePerformIO $ getCommand "glGetVertexAttribivNV"

-- glGetVideoCaptureStreamdvNV -------------------------------------------------

glGetVideoCaptureStreamdvNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetVideoCaptureStreamdvNV v1 v2 v3 v4 = liftIO $ dyn465 ptr_glGetVideoCaptureStreamdvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetVideoCaptureStreamdvNV #-}
ptr_glGetVideoCaptureStreamdvNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVideoCaptureStreamdvNV = unsafePerformIO $ getCommand "glGetVideoCaptureStreamdvNV"

-- glGetVideoCaptureStreamfvNV -------------------------------------------------

glGetVideoCaptureStreamfvNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetVideoCaptureStreamfvNV v1 v2 v3 v4 = liftIO $ dyn466 ptr_glGetVideoCaptureStreamfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetVideoCaptureStreamfvNV #-}
ptr_glGetVideoCaptureStreamfvNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVideoCaptureStreamfvNV = unsafePerformIO $ getCommand "glGetVideoCaptureStreamfvNV"

-- glGetVideoCaptureStreamivNV -------------------------------------------------

glGetVideoCaptureStreamivNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetVideoCaptureStreamivNV v1 v2 v3 v4 = liftIO $ dyn314 ptr_glGetVideoCaptureStreamivNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetVideoCaptureStreamivNV #-}
ptr_glGetVideoCaptureStreamivNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVideoCaptureStreamivNV = unsafePerformIO $ getCommand "glGetVideoCaptureStreamivNV"

-- glGetVideoCaptureivNV -------------------------------------------------------

glGetVideoCaptureivNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetVideoCaptureivNV v1 v2 v3 = liftIO $ dyn348 ptr_glGetVideoCaptureivNV v1 v2 v3

{-# NOINLINE ptr_glGetVideoCaptureivNV #-}
ptr_glGetVideoCaptureivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVideoCaptureivNV = unsafePerformIO $ getCommand "glGetVideoCaptureivNV"

-- glGetVideoi64vNV ------------------------------------------------------------

glGetVideoi64vNV
  :: MonadIO m
  => GLuint -- ^ @video_slot@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint64EXT@.
  -> m ()
glGetVideoi64vNV v1 v2 v3 = liftIO $ dyn464 ptr_glGetVideoi64vNV v1 v2 v3

{-# NOINLINE ptr_glGetVideoi64vNV #-}
ptr_glGetVideoi64vNV :: FunPtr (GLuint -> GLenum -> Ptr GLint64EXT -> IO ())
ptr_glGetVideoi64vNV = unsafePerformIO $ getCommand "glGetVideoi64vNV"

-- glGetVideoivNV --------------------------------------------------------------

glGetVideoivNV
  :: MonadIO m
  => GLuint -- ^ @video_slot@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetVideoivNV v1 v2 v3 = liftIO $ dyn348 ptr_glGetVideoivNV v1 v2 v3

{-# NOINLINE ptr_glGetVideoivNV #-}
ptr_glGetVideoivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVideoivNV = unsafePerformIO $ getCommand "glGetVideoivNV"

-- glGetVideoui64vNV -----------------------------------------------------------

glGetVideoui64vNV
  :: MonadIO m
  => GLuint -- ^ @video_slot@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64EXT@.
  -> m ()
glGetVideoui64vNV v1 v2 v3 = liftIO $ dyn376 ptr_glGetVideoui64vNV v1 v2 v3

{-# NOINLINE ptr_glGetVideoui64vNV #-}
ptr_glGetVideoui64vNV :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
ptr_glGetVideoui64vNV = unsafePerformIO $ getCommand "glGetVideoui64vNV"

