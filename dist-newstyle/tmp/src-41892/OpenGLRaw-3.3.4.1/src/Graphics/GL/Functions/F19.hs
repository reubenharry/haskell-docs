{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F19
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

module Graphics.GL.Functions.F19 (
  glNamedProgramLocalParameter4dEXT,
  glNamedProgramLocalParameter4dvEXT,
  glNamedProgramLocalParameter4fEXT,
  glNamedProgramLocalParameter4fvEXT,
  glNamedProgramLocalParameterI4iEXT,
  glNamedProgramLocalParameterI4ivEXT,
  glNamedProgramLocalParameterI4uiEXT,
  glNamedProgramLocalParameterI4uivEXT,
  glNamedProgramLocalParameters4fvEXT,
  glNamedProgramLocalParametersI4ivEXT,
  glNamedProgramLocalParametersI4uivEXT,
  glNamedProgramStringEXT,
  glNamedRenderbufferStorage,
  glNamedRenderbufferStorageEXT,
  glNamedRenderbufferStorageMultisample,
  glNamedRenderbufferStorageMultisampleAdvancedAMD,
  glNamedRenderbufferStorageMultisampleCoverageEXT,
  glNamedRenderbufferStorageMultisampleEXT,
  glNamedStringARB,
  glNewList,
  glNewObjectBufferATI,
  glNormal3b,
  glNormal3bv,
  glNormal3d,
  glNormal3dv,
  glNormal3f,
  glNormal3fVertex3fSUN,
  glNormal3fVertex3fvSUN,
  glNormal3fv,
  glNormal3hNV,
  glNormal3hvNV,
  glNormal3i,
  glNormal3iv,
  glNormal3s,
  glNormal3sv,
  glNormal3x,
  glNormal3xOES,
  glNormal3xvOES,
  glNormalFormatNV,
  glNormalP3ui,
  glNormalP3uiv,
  glNormalPointer,
  glNormalPointerEXT,
  glNormalPointerListIBM,
  glNormalPointervINTEL,
  glNormalStream3bATI,
  glNormalStream3bvATI,
  glNormalStream3dATI,
  glNormalStream3dvATI,
  glNormalStream3fATI,
  glNormalStream3fvATI,
  glNormalStream3iATI,
  glNormalStream3ivATI,
  glNormalStream3sATI,
  glNormalStream3svATI,
  glObjectLabel,
  glObjectLabelKHR,
  glObjectPtrLabel,
  glObjectPtrLabelKHR,
  glObjectPurgeableAPPLE,
  glObjectUnpurgeableAPPLE,
  glOrtho,
  glOrthof,
  glOrthofOES,
  glOrthox,
  glOrthoxOES,
  glPNTrianglesfATI,
  glPNTrianglesiATI,
  glPassTexCoordATI,
  glPassThrough,
  glPassThroughxOES,
  glPatchParameterfv,
  glPatchParameteri,
  glPatchParameteriEXT,
  glPatchParameteriOES,
  glPathColorGenNV,
  glPathCommandsNV,
  glPathCoordsNV,
  glPathCoverDepthFuncNV,
  glPathDashArrayNV,
  glPathFogGenNV,
  glPathGlyphIndexArrayNV,
  glPathGlyphIndexRangeNV,
  glPathGlyphRangeNV,
  glPathGlyphsNV,
  glPathMemoryGlyphIndexArrayNV,
  glPathParameterfNV,
  glPathParameterfvNV,
  glPathParameteriNV,
  glPathParameterivNV,
  glPathStencilDepthOffsetNV,
  glPathStencilFuncNV,
  glPathStringNV,
  glPathSubCommandsNV,
  glPathSubCoordsNV,
  glPathTexGenNV,
  glPauseTransformFeedback,
  glPauseTransformFeedbackNV,
  glPixelDataRangeNV,
  glPixelMapfv
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glNamedProgramLocalParameter4dEXT -------------------------------------------

-- | The vector equivalent of this command is 'glNamedProgramLocalParameter4dvEXT'.
glNamedProgramLocalParameter4dEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glNamedProgramLocalParameter4dEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn620 ptr_glNamedProgramLocalParameter4dEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glNamedProgramLocalParameter4dEXT #-}
ptr_glNamedProgramLocalParameter4dEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glNamedProgramLocalParameter4dEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameter4dEXT"

-- glNamedProgramLocalParameter4dvEXT ------------------------------------------

glNamedProgramLocalParameter4dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glNamedProgramLocalParameter4dvEXT v1 v2 v3 v4 = liftIO $ dyn383 ptr_glNamedProgramLocalParameter4dvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedProgramLocalParameter4dvEXT #-}
ptr_glNamedProgramLocalParameter4dvEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glNamedProgramLocalParameter4dvEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameter4dvEXT"

-- glNamedProgramLocalParameter4fEXT -------------------------------------------

-- | The vector equivalent of this command is 'glNamedProgramLocalParameter4fvEXT'.
glNamedProgramLocalParameter4fEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glNamedProgramLocalParameter4fEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn621 ptr_glNamedProgramLocalParameter4fEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glNamedProgramLocalParameter4fEXT #-}
ptr_glNamedProgramLocalParameter4fEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glNamedProgramLocalParameter4fEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameter4fEXT"

-- glNamedProgramLocalParameter4fvEXT ------------------------------------------

glNamedProgramLocalParameter4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glNamedProgramLocalParameter4fvEXT v1 v2 v3 v4 = liftIO $ dyn384 ptr_glNamedProgramLocalParameter4fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedProgramLocalParameter4fvEXT #-}
ptr_glNamedProgramLocalParameter4fvEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glNamedProgramLocalParameter4fvEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameter4fvEXT"

-- glNamedProgramLocalParameterI4iEXT ------------------------------------------

-- | The vector equivalent of this command is 'glNamedProgramLocalParameterI4ivEXT'.
glNamedProgramLocalParameterI4iEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @w@.
  -> m ()
glNamedProgramLocalParameterI4iEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn622 ptr_glNamedProgramLocalParameterI4iEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glNamedProgramLocalParameterI4iEXT #-}
ptr_glNamedProgramLocalParameterI4iEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glNamedProgramLocalParameterI4iEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameterI4iEXT"

-- glNamedProgramLocalParameterI4ivEXT -----------------------------------------

glNamedProgramLocalParameterI4ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glNamedProgramLocalParameterI4ivEXT v1 v2 v3 v4 = liftIO $ dyn381 ptr_glNamedProgramLocalParameterI4ivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedProgramLocalParameterI4ivEXT #-}
ptr_glNamedProgramLocalParameterI4ivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glNamedProgramLocalParameterI4ivEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameterI4ivEXT"

-- glNamedProgramLocalParameterI4uiEXT -----------------------------------------

-- | The vector equivalent of this command is 'glNamedProgramLocalParameterI4uivEXT'.
glNamedProgramLocalParameterI4uiEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> GLuint -- ^ @w@.
  -> m ()
glNamedProgramLocalParameterI4uiEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn623 ptr_glNamedProgramLocalParameterI4uiEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glNamedProgramLocalParameterI4uiEXT #-}
ptr_glNamedProgramLocalParameterI4uiEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glNamedProgramLocalParameterI4uiEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameterI4uiEXT"

-- glNamedProgramLocalParameterI4uivEXT ----------------------------------------

glNamedProgramLocalParameterI4uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @params@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glNamedProgramLocalParameterI4uivEXT v1 v2 v3 v4 = liftIO $ dyn382 ptr_glNamedProgramLocalParameterI4uivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedProgramLocalParameterI4uivEXT #-}
ptr_glNamedProgramLocalParameterI4uivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glNamedProgramLocalParameterI4uivEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameterI4uivEXT"

-- glNamedProgramLocalParameters4fvEXT -----------------------------------------

glNamedProgramLocalParameters4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @params@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glNamedProgramLocalParameters4fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn624 ptr_glNamedProgramLocalParameters4fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedProgramLocalParameters4fvEXT #-}
ptr_glNamedProgramLocalParameters4fvEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glNamedProgramLocalParameters4fvEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameters4fvEXT"

-- glNamedProgramLocalParametersI4ivEXT ----------------------------------------

glNamedProgramLocalParametersI4ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @params@ pointing to @count*4@ elements of type @GLint@.
  -> m ()
glNamedProgramLocalParametersI4ivEXT v1 v2 v3 v4 v5 = liftIO $ dyn625 ptr_glNamedProgramLocalParametersI4ivEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedProgramLocalParametersI4ivEXT #-}
ptr_glNamedProgramLocalParametersI4ivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ())
ptr_glNamedProgramLocalParametersI4ivEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParametersI4ivEXT"

-- glNamedProgramLocalParametersI4uivEXT ---------------------------------------

glNamedProgramLocalParametersI4uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @params@ pointing to @count*4@ elements of type @GLuint@.
  -> m ()
glNamedProgramLocalParametersI4uivEXT v1 v2 v3 v4 v5 = liftIO $ dyn626 ptr_glNamedProgramLocalParametersI4uivEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedProgramLocalParametersI4uivEXT #-}
ptr_glNamedProgramLocalParametersI4uivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glNamedProgramLocalParametersI4uivEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParametersI4uivEXT"

-- glNamedProgramStringEXT -----------------------------------------------------

glNamedProgramStringEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLenum -- ^ @format@ of type [ProgramFormat](Graphics-GL-Groups.html#ProgramFormat).
  -> GLsizei -- ^ @len@.
  -> Ptr a -- ^ @string@ pointing to @len@ elements of type @a@.
  -> m ()
glNamedProgramStringEXT v1 v2 v3 v4 v5 = liftIO $ dyn627 ptr_glNamedProgramStringEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedProgramStringEXT #-}
ptr_glNamedProgramStringEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glNamedProgramStringEXT = unsafePerformIO $ getCommand "glNamedProgramStringEXT"

-- glNamedRenderbufferStorage --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorage.xhtml OpenGL 4.x>.
glNamedRenderbufferStorage
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glNamedRenderbufferStorage v1 v2 v3 v4 = liftIO $ dyn628 ptr_glNamedRenderbufferStorage v1 v2 v3 v4

{-# NOINLINE ptr_glNamedRenderbufferStorage #-}
ptr_glNamedRenderbufferStorage :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glNamedRenderbufferStorage = unsafePerformIO $ getCommand "glNamedRenderbufferStorage"

-- glNamedRenderbufferStorageEXT -----------------------------------------------

glNamedRenderbufferStorageEXT
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@ of type @Renderbuffer@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glNamedRenderbufferStorageEXT v1 v2 v3 v4 = liftIO $ dyn628 ptr_glNamedRenderbufferStorageEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedRenderbufferStorageEXT #-}
ptr_glNamedRenderbufferStorageEXT :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glNamedRenderbufferStorageEXT = unsafePerformIO $ getCommand "glNamedRenderbufferStorageEXT"

-- glNamedRenderbufferStorageMultisample ---------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorageMultisample.xhtml OpenGL 4.x>.
glNamedRenderbufferStorageMultisample
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glNamedRenderbufferStorageMultisample v1 v2 v3 v4 v5 = liftIO $ dyn629 ptr_glNamedRenderbufferStorageMultisample v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedRenderbufferStorageMultisample #-}
ptr_glNamedRenderbufferStorageMultisample :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glNamedRenderbufferStorageMultisample = unsafePerformIO $ getCommand "glNamedRenderbufferStorageMultisample"

-- glNamedRenderbufferStorageMultisampleAdvancedAMD ----------------------------

glNamedRenderbufferStorageMultisampleAdvancedAMD
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@ of type @Renderbuffer@.
  -> GLsizei -- ^ @samples@.
  -> GLsizei -- ^ @storageSamples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glNamedRenderbufferStorageMultisampleAdvancedAMD v1 v2 v3 v4 v5 v6 = liftIO $ dyn630 ptr_glNamedRenderbufferStorageMultisampleAdvancedAMD v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glNamedRenderbufferStorageMultisampleAdvancedAMD #-}
ptr_glNamedRenderbufferStorageMultisampleAdvancedAMD :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glNamedRenderbufferStorageMultisampleAdvancedAMD = unsafePerformIO $ getCommand "glNamedRenderbufferStorageMultisampleAdvancedAMD"

-- glNamedRenderbufferStorageMultisampleCoverageEXT ----------------------------

glNamedRenderbufferStorageMultisampleCoverageEXT
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@ of type @Renderbuffer@.
  -> GLsizei -- ^ @coverageSamples@.
  -> GLsizei -- ^ @colorSamples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glNamedRenderbufferStorageMultisampleCoverageEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn630 ptr_glNamedRenderbufferStorageMultisampleCoverageEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glNamedRenderbufferStorageMultisampleCoverageEXT #-}
ptr_glNamedRenderbufferStorageMultisampleCoverageEXT :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glNamedRenderbufferStorageMultisampleCoverageEXT = unsafePerformIO $ getCommand "glNamedRenderbufferStorageMultisampleCoverageEXT"

-- glNamedRenderbufferStorageMultisampleEXT ------------------------------------

glNamedRenderbufferStorageMultisampleEXT
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@ of type @Renderbuffer@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glNamedRenderbufferStorageMultisampleEXT v1 v2 v3 v4 v5 = liftIO $ dyn629 ptr_glNamedRenderbufferStorageMultisampleEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedRenderbufferStorageMultisampleEXT #-}
ptr_glNamedRenderbufferStorageMultisampleEXT :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glNamedRenderbufferStorageMultisampleEXT = unsafePerformIO $ getCommand "glNamedRenderbufferStorageMultisampleEXT"

-- glNamedStringARB ------------------------------------------------------------

glNamedStringARB
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLint -- ^ @namelen@.
  -> Ptr GLchar -- ^ @name@ pointing to @namelen@ elements of type @GLchar@.
  -> GLint -- ^ @stringlen@.
  -> Ptr GLchar -- ^ @string@ pointing to @stringlen@ elements of type @GLchar@.
  -> m ()
glNamedStringARB v1 v2 v3 v4 v5 = liftIO $ dyn631 ptr_glNamedStringARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedStringARB #-}
ptr_glNamedStringARB :: FunPtr (GLenum -> GLint -> Ptr GLchar -> GLint -> Ptr GLchar -> IO ())
ptr_glNamedStringARB = unsafePerformIO $ getCommand "glNamedStringARB"

-- glNewList -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNewList.xml OpenGL 2.x>.
glNewList
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @mode@ of type [ListMode](Graphics-GL-Groups.html#ListMode).
  -> m ()
glNewList v1 v2 = liftIO $ dyn18 ptr_glNewList v1 v2

{-# NOINLINE ptr_glNewList #-}
ptr_glNewList :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glNewList = unsafePerformIO $ getCommand "glNewList"

-- glNewObjectBufferATI --------------------------------------------------------

glNewObjectBufferATI
  :: MonadIO m
  => GLsizei -- ^ @size@.
  -> Ptr a -- ^ @pointer@ pointing to @size@ elements of type @a@.
  -> GLenum -- ^ @usage@ of type [ArrayObjectUsageATI](Graphics-GL-Groups.html#ArrayObjectUsageATI).
  -> m GLuint
glNewObjectBufferATI v1 v2 v3 = liftIO $ dyn632 ptr_glNewObjectBufferATI v1 v2 v3

{-# NOINLINE ptr_glNewObjectBufferATI #-}
ptr_glNewObjectBufferATI :: FunPtr (GLsizei -> Ptr a -> GLenum -> IO GLuint)
ptr_glNewObjectBufferATI = unsafePerformIO $ getCommand "glNewObjectBufferATI"

-- glNormal3b ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>. The vector equivalent of this command is 'glNormal3bv'.
glNormal3b
  :: MonadIO m
  => GLbyte -- ^ @nx@.
  -> GLbyte -- ^ @ny@.
  -> GLbyte -- ^ @nz@.
  -> m ()
glNormal3b v1 v2 v3 = liftIO $ dyn39 ptr_glNormal3b v1 v2 v3

{-# NOINLINE ptr_glNormal3b #-}
ptr_glNormal3b :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glNormal3b = unsafePerformIO $ getCommand "glNormal3b"

-- glNormal3bv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>.
glNormal3bv
  :: MonadIO m
  => Ptr GLbyte -- ^ @v@ pointing to @3@ elements of type @GLbyte@.
  -> m ()
glNormal3bv v1 = liftIO $ dyn40 ptr_glNormal3bv v1

{-# NOINLINE ptr_glNormal3bv #-}
ptr_glNormal3bv :: FunPtr (Ptr GLbyte -> IO ())
ptr_glNormal3bv = unsafePerformIO $ getCommand "glNormal3bv"

-- glNormal3d ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>. The vector equivalent of this command is 'glNormal3dv'.
glNormal3d
  :: MonadIO m
  => GLdouble -- ^ @nx@ of type @CoordD@.
  -> GLdouble -- ^ @ny@ of type @CoordD@.
  -> GLdouble -- ^ @nz@ of type @CoordD@.
  -> m ()
glNormal3d v1 v2 v3 = liftIO $ dyn41 ptr_glNormal3d v1 v2 v3

{-# NOINLINE ptr_glNormal3d #-}
ptr_glNormal3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glNormal3d = unsafePerformIO $ getCommand "glNormal3d"

-- glNormal3dv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>.
glNormal3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glNormal3dv v1 = liftIO $ dyn42 ptr_glNormal3dv v1

{-# NOINLINE ptr_glNormal3dv #-}
ptr_glNormal3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glNormal3dv = unsafePerformIO $ getCommand "glNormal3dv"

-- glNormal3f ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>. The vector equivalent of this command is 'glNormal3fv'.
glNormal3f
  :: MonadIO m
  => GLfloat -- ^ @nx@ of type @CoordF@.
  -> GLfloat -- ^ @ny@ of type @CoordF@.
  -> GLfloat -- ^ @nz@ of type @CoordF@.
  -> m ()
glNormal3f v1 v2 v3 = liftIO $ dyn43 ptr_glNormal3f v1 v2 v3

{-# NOINLINE ptr_glNormal3f #-}
ptr_glNormal3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glNormal3f = unsafePerformIO $ getCommand "glNormal3f"

-- glNormal3fVertex3fSUN -------------------------------------------------------

glNormal3fVertex3fSUN
  :: MonadIO m
  => GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 = liftIO $ dyn103 ptr_glNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glNormal3fVertex3fSUN #-}
ptr_glNormal3fVertex3fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glNormal3fVertex3fSUN"

-- glNormal3fVertex3fvSUN ------------------------------------------------------

glNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glNormal3fVertex3fvSUN v1 v2 = liftIO $ dyn104 ptr_glNormal3fVertex3fvSUN v1 v2

{-# NOINLINE ptr_glNormal3fVertex3fvSUN #-}
ptr_glNormal3fVertex3fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glNormal3fVertex3fvSUN"

-- glNormal3fv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>.
glNormal3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glNormal3fv v1 = liftIO $ dyn44 ptr_glNormal3fv v1

{-# NOINLINE ptr_glNormal3fv #-}
ptr_glNormal3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glNormal3fv = unsafePerformIO $ getCommand "glNormal3fv"

-- glNormal3hNV ----------------------------------------------------------------

-- | The vector equivalent of this command is 'glNormal3hvNV'.
glNormal3hNV
  :: MonadIO m
  => GLhalfNV -- ^ @nx@ of type @Half16NV@.
  -> GLhalfNV -- ^ @ny@ of type @Half16NV@.
  -> GLhalfNV -- ^ @nz@ of type @Half16NV@.
  -> m ()
glNormal3hNV v1 v2 v3 = liftIO $ dyn105 ptr_glNormal3hNV v1 v2 v3

{-# NOINLINE ptr_glNormal3hNV #-}
ptr_glNormal3hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glNormal3hNV = unsafePerformIO $ getCommand "glNormal3hNV"

-- glNormal3hvNV ---------------------------------------------------------------

glNormal3hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glNormal3hvNV v1 = liftIO $ dyn106 ptr_glNormal3hvNV v1

{-# NOINLINE ptr_glNormal3hvNV #-}
ptr_glNormal3hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glNormal3hvNV = unsafePerformIO $ getCommand "glNormal3hvNV"

-- glNormal3i ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>. The vector equivalent of this command is 'glNormal3iv'.
glNormal3i
  :: MonadIO m
  => GLint -- ^ @nx@.
  -> GLint -- ^ @ny@.
  -> GLint -- ^ @nz@.
  -> m ()
glNormal3i v1 v2 v3 = liftIO $ dyn45 ptr_glNormal3i v1 v2 v3

{-# NOINLINE ptr_glNormal3i #-}
ptr_glNormal3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glNormal3i = unsafePerformIO $ getCommand "glNormal3i"

-- glNormal3iv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>.
glNormal3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @GLint@.
  -> m ()
glNormal3iv v1 = liftIO $ dyn46 ptr_glNormal3iv v1

{-# NOINLINE ptr_glNormal3iv #-}
ptr_glNormal3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glNormal3iv = unsafePerformIO $ getCommand "glNormal3iv"

-- glNormal3s ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>. The vector equivalent of this command is 'glNormal3sv'.
glNormal3s
  :: MonadIO m
  => GLshort -- ^ @nx@.
  -> GLshort -- ^ @ny@.
  -> GLshort -- ^ @nz@.
  -> m ()
glNormal3s v1 v2 v3 = liftIO $ dyn47 ptr_glNormal3s v1 v2 v3

{-# NOINLINE ptr_glNormal3s #-}
ptr_glNormal3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glNormal3s = unsafePerformIO $ getCommand "glNormal3s"

-- glNormal3sv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>.
glNormal3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glNormal3sv v1 = liftIO $ dyn48 ptr_glNormal3sv v1

{-# NOINLINE ptr_glNormal3sv #-}
ptr_glNormal3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glNormal3sv = unsafePerformIO $ getCommand "glNormal3sv"

-- glNormal3x ------------------------------------------------------------------

glNormal3x
  :: MonadIO m
  => GLfixed -- ^ @nx@.
  -> GLfixed -- ^ @ny@.
  -> GLfixed -- ^ @nz@.
  -> m ()
glNormal3x v1 v2 v3 = liftIO $ dyn113 ptr_glNormal3x v1 v2 v3

{-# NOINLINE ptr_glNormal3x #-}
ptr_glNormal3x :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glNormal3x = unsafePerformIO $ getCommand "glNormal3x"

-- glNormal3xOES ---------------------------------------------------------------

glNormal3xOES
  :: MonadIO m
  => GLfixed -- ^ @nx@.
  -> GLfixed -- ^ @ny@.
  -> GLfixed -- ^ @nz@.
  -> m ()
glNormal3xOES v1 v2 v3 = liftIO $ dyn113 ptr_glNormal3xOES v1 v2 v3

{-# NOINLINE ptr_glNormal3xOES #-}
ptr_glNormal3xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glNormal3xOES = unsafePerformIO $ getCommand "glNormal3xOES"

-- glNormal3xvOES --------------------------------------------------------------

glNormal3xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @3@ elements of type @GLfixed@.
  -> m ()
glNormal3xvOES v1 = liftIO $ dyn114 ptr_glNormal3xvOES v1

{-# NOINLINE ptr_glNormal3xvOES #-}
ptr_glNormal3xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glNormal3xvOES = unsafePerformIO $ getCommand "glNormal3xvOES"

-- glNormalFormatNV ------------------------------------------------------------

glNormalFormatNV
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glNormalFormatNV v1 v2 = liftIO $ dyn247 ptr_glNormalFormatNV v1 v2

{-# NOINLINE ptr_glNormalFormatNV #-}
ptr_glNormalFormatNV :: FunPtr (GLenum -> GLsizei -> IO ())
ptr_glNormalFormatNV = unsafePerformIO $ getCommand "glNormalFormatNV"

-- glNormalP3ui ----------------------------------------------------------------

glNormalP3ui
  :: MonadIO m
  => GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> GLuint -- ^ @coords@.
  -> m ()
glNormalP3ui v1 v2 = liftIO $ dyn19 ptr_glNormalP3ui v1 v2

{-# NOINLINE ptr_glNormalP3ui #-}
ptr_glNormalP3ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glNormalP3ui = unsafePerformIO $ getCommand "glNormalP3ui"

-- glNormalP3uiv ---------------------------------------------------------------

glNormalP3uiv
  :: MonadIO m
  => GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glNormalP3uiv v1 v2 = liftIO $ dyn132 ptr_glNormalP3uiv v1 v2

{-# NOINLINE ptr_glNormalP3uiv #-}
ptr_glNormalP3uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glNormalP3uiv = unsafePerformIO $ getCommand "glNormalP3uiv"

-- glNormalPointer -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormalPointer.xml OpenGL 2.x>.
glNormalPointer
  :: MonadIO m
  => GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glNormalPointer v1 v2 v3 = liftIO $ dyn49 ptr_glNormalPointer v1 v2 v3

{-# NOINLINE ptr_glNormalPointer #-}
ptr_glNormalPointer :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glNormalPointer = unsafePerformIO $ getCommand "glNormalPointer"

-- glNormalPointerEXT ----------------------------------------------------------

glNormalPointerEXT
  :: MonadIO m
  => GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLsizei -- ^ @count@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride,count)@ elements of type @a@.
  -> m ()
glNormalPointerEXT v1 v2 v3 v4 = liftIO $ dyn494 ptr_glNormalPointerEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNormalPointerEXT #-}
ptr_glNormalPointerEXT :: FunPtr (GLenum -> GLsizei -> GLsizei -> Ptr a -> IO ())
ptr_glNormalPointerEXT = unsafePerformIO $ getCommand "glNormalPointerEXT"

-- glNormalPointerListIBM ------------------------------------------------------

glNormalPointerListIBM
  :: MonadIO m
  => GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glNormalPointerListIBM v1 v2 v3 v4 = liftIO $ dyn291 ptr_glNormalPointerListIBM v1 v2 v3 v4

{-# NOINLINE ptr_glNormalPointerListIBM #-}
ptr_glNormalPointerListIBM :: FunPtr (GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glNormalPointerListIBM = unsafePerformIO $ getCommand "glNormalPointerListIBM"

-- glNormalPointervINTEL -------------------------------------------------------

glNormalPointervINTEL
  :: MonadIO m
  => GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @4@ elements of type @Ptr a@.
  -> m ()
glNormalPointervINTEL v1 v2 = liftIO $ dyn279 ptr_glNormalPointervINTEL v1 v2

{-# NOINLINE ptr_glNormalPointervINTEL #-}
ptr_glNormalPointervINTEL :: FunPtr (GLenum -> Ptr (Ptr a) -> IO ())
ptr_glNormalPointervINTEL = unsafePerformIO $ getCommand "glNormalPointervINTEL"

-- glNormalStream3bATI ---------------------------------------------------------

glNormalStream3bATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLbyte -- ^ @nx@.
  -> GLbyte -- ^ @ny@.
  -> GLbyte -- ^ @nz@.
  -> m ()
glNormalStream3bATI v1 v2 v3 v4 = liftIO $ dyn580 ptr_glNormalStream3bATI v1 v2 v3 v4

{-# NOINLINE ptr_glNormalStream3bATI #-}
ptr_glNormalStream3bATI :: FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glNormalStream3bATI = unsafePerformIO $ getCommand "glNormalStream3bATI"

-- glNormalStream3bvATI --------------------------------------------------------

glNormalStream3bvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLbyte -- ^ @coords@ pointing to @3@ elements of type @GLbyte@.
  -> m ()
glNormalStream3bvATI v1 v2 = liftIO $ dyn568 ptr_glNormalStream3bvATI v1 v2

{-# NOINLINE ptr_glNormalStream3bvATI #-}
ptr_glNormalStream3bvATI :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
ptr_glNormalStream3bvATI = unsafePerformIO $ getCommand "glNormalStream3bvATI"

-- glNormalStream3dATI ---------------------------------------------------------

glNormalStream3dATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLdouble -- ^ @nx@.
  -> GLdouble -- ^ @ny@.
  -> GLdouble -- ^ @nz@.
  -> m ()
glNormalStream3dATI v1 v2 v3 v4 = liftIO $ dyn548 ptr_glNormalStream3dATI v1 v2 v3 v4

{-# NOINLINE ptr_glNormalStream3dATI #-}
ptr_glNormalStream3dATI :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glNormalStream3dATI = unsafePerformIO $ getCommand "glNormalStream3dATI"

-- glNormalStream3dvATI --------------------------------------------------------

glNormalStream3dvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLdouble -- ^ @coords@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glNormalStream3dvATI v1 v2 = liftIO $ dyn100 ptr_glNormalStream3dvATI v1 v2

{-# NOINLINE ptr_glNormalStream3dvATI #-}
ptr_glNormalStream3dvATI :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glNormalStream3dvATI = unsafePerformIO $ getCommand "glNormalStream3dvATI"

-- glNormalStream3fATI ---------------------------------------------------------

glNormalStream3fATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> m ()
glNormalStream3fATI v1 v2 v3 v4 = liftIO $ dyn549 ptr_glNormalStream3fATI v1 v2 v3 v4

{-# NOINLINE ptr_glNormalStream3fATI #-}
ptr_glNormalStream3fATI :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glNormalStream3fATI = unsafePerformIO $ getCommand "glNormalStream3fATI"

-- glNormalStream3fvATI --------------------------------------------------------

glNormalStream3fvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLfloat -- ^ @coords@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glNormalStream3fvATI v1 v2 = liftIO $ dyn101 ptr_glNormalStream3fvATI v1 v2

{-# NOINLINE ptr_glNormalStream3fvATI #-}
ptr_glNormalStream3fvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glNormalStream3fvATI = unsafePerformIO $ getCommand "glNormalStream3fvATI"

-- glNormalStream3iATI ---------------------------------------------------------

glNormalStream3iATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLint -- ^ @nx@.
  -> GLint -- ^ @ny@.
  -> GLint -- ^ @nz@.
  -> m ()
glNormalStream3iATI v1 v2 v3 v4 = liftIO $ dyn582 ptr_glNormalStream3iATI v1 v2 v3 v4

{-# NOINLINE ptr_glNormalStream3iATI #-}
ptr_glNormalStream3iATI :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ())
ptr_glNormalStream3iATI = unsafePerformIO $ getCommand "glNormalStream3iATI"

-- glNormalStream3ivATI --------------------------------------------------------

glNormalStream3ivATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLint -- ^ @coords@ pointing to @3@ elements of type @GLint@.
  -> m ()
glNormalStream3ivATI v1 v2 = liftIO $ dyn143 ptr_glNormalStream3ivATI v1 v2

{-# NOINLINE ptr_glNormalStream3ivATI #-}
ptr_glNormalStream3ivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glNormalStream3ivATI = unsafePerformIO $ getCommand "glNormalStream3ivATI"

-- glNormalStream3sATI ---------------------------------------------------------

glNormalStream3sATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLshort -- ^ @nx@.
  -> GLshort -- ^ @ny@.
  -> GLshort -- ^ @nz@.
  -> m ()
glNormalStream3sATI v1 v2 v3 v4 = liftIO $ dyn583 ptr_glNormalStream3sATI v1 v2 v3 v4

{-# NOINLINE ptr_glNormalStream3sATI #-}
ptr_glNormalStream3sATI :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glNormalStream3sATI = unsafePerformIO $ getCommand "glNormalStream3sATI"

-- glNormalStream3svATI --------------------------------------------------------

glNormalStream3svATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLshort -- ^ @coords@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glNormalStream3svATI v1 v2 = liftIO $ dyn573 ptr_glNormalStream3svATI v1 v2

{-# NOINLINE ptr_glNormalStream3svATI #-}
ptr_glNormalStream3svATI :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glNormalStream3svATI = unsafePerformIO $ getCommand "glNormalStream3svATI"

-- glObjectLabel ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glObjectLabel.xhtml OpenGL 4.x>.
glObjectLabel
  :: MonadIO m
  => GLenum -- ^ @identifier@ of type [ObjectIdentifier](Graphics-GL-Groups.html#ObjectIdentifier).
  -> GLuint -- ^ @name@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@ pointing to @COMPSIZE(label,length)@ elements of type @GLchar@.
  -> m ()
glObjectLabel v1 v2 v3 v4 = liftIO $ dyn511 ptr_glObjectLabel v1 v2 v3 v4

{-# NOINLINE ptr_glObjectLabel #-}
ptr_glObjectLabel :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glObjectLabel = unsafePerformIO $ getCommand "glObjectLabel"

-- glObjectLabelKHR ------------------------------------------------------------

-- | This command is an alias for 'glObjectLabel'.
glObjectLabelKHR
  :: MonadIO m
  => GLenum -- ^ @identifier@ of type [ObjectIdentifier](Graphics-GL-Groups.html#ObjectIdentifier).
  -> GLuint -- ^ @name@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@.
  -> m ()
glObjectLabelKHR v1 v2 v3 v4 = liftIO $ dyn511 ptr_glObjectLabelKHR v1 v2 v3 v4

{-# NOINLINE ptr_glObjectLabelKHR #-}
ptr_glObjectLabelKHR :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glObjectLabelKHR = unsafePerformIO $ getCommand "glObjectLabelKHR"

-- glObjectPtrLabel ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glObjectPtrLabel.xhtml OpenGL 4.x>.
glObjectPtrLabel
  :: MonadIO m
  => Ptr a -- ^ @ptr@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@ pointing to @COMPSIZE(label,length)@ elements of type @GLchar@.
  -> m ()
glObjectPtrLabel v1 v2 v3 = liftIO $ dyn633 ptr_glObjectPtrLabel v1 v2 v3

{-# NOINLINE ptr_glObjectPtrLabel #-}
ptr_glObjectPtrLabel :: FunPtr (Ptr a -> GLsizei -> Ptr GLchar -> IO ())
ptr_glObjectPtrLabel = unsafePerformIO $ getCommand "glObjectPtrLabel"

-- glObjectPtrLabelKHR ---------------------------------------------------------

-- | This command is an alias for 'glObjectPtrLabel'.
glObjectPtrLabelKHR
  :: MonadIO m
  => Ptr a -- ^ @ptr@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@.
  -> m ()
glObjectPtrLabelKHR v1 v2 v3 = liftIO $ dyn633 ptr_glObjectPtrLabelKHR v1 v2 v3

{-# NOINLINE ptr_glObjectPtrLabelKHR #-}
ptr_glObjectPtrLabelKHR :: FunPtr (Ptr a -> GLsizei -> Ptr GLchar -> IO ())
ptr_glObjectPtrLabelKHR = unsafePerformIO $ getCommand "glObjectPtrLabelKHR"

-- glObjectPurgeableAPPLE ------------------------------------------------------

glObjectPurgeableAPPLE
  :: MonadIO m
  => GLenum -- ^ @objectType@.
  -> GLuint -- ^ @name@.
  -> GLenum -- ^ @option@.
  -> m GLenum
glObjectPurgeableAPPLE v1 v2 v3 = liftIO $ dyn634 ptr_glObjectPurgeableAPPLE v1 v2 v3

{-# NOINLINE ptr_glObjectPurgeableAPPLE #-}
ptr_glObjectPurgeableAPPLE :: FunPtr (GLenum -> GLuint -> GLenum -> IO GLenum)
ptr_glObjectPurgeableAPPLE = unsafePerformIO $ getCommand "glObjectPurgeableAPPLE"

-- glObjectUnpurgeableAPPLE ----------------------------------------------------

glObjectUnpurgeableAPPLE
  :: MonadIO m
  => GLenum -- ^ @objectType@.
  -> GLuint -- ^ @name@.
  -> GLenum -- ^ @option@.
  -> m GLenum
glObjectUnpurgeableAPPLE v1 v2 v3 = liftIO $ dyn634 ptr_glObjectUnpurgeableAPPLE v1 v2 v3

{-# NOINLINE ptr_glObjectUnpurgeableAPPLE #-}
ptr_glObjectUnpurgeableAPPLE :: FunPtr (GLenum -> GLuint -> GLenum -> IO GLenum)
ptr_glObjectUnpurgeableAPPLE = unsafePerformIO $ getCommand "glObjectUnpurgeableAPPLE"

-- glOrtho ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glOrtho.xml OpenGL 2.x>.
glOrtho
  :: MonadIO m
  => GLdouble -- ^ @left@.
  -> GLdouble -- ^ @right@.
  -> GLdouble -- ^ @bottom@.
  -> GLdouble -- ^ @top@.
  -> GLdouble -- ^ @zNear@.
  -> GLdouble -- ^ @zFar@.
  -> m ()
glOrtho v1 v2 v3 v4 v5 v6 = liftIO $ dyn309 ptr_glOrtho v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glOrtho #-}
ptr_glOrtho :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glOrtho = unsafePerformIO $ getCommand "glOrtho"

-- glOrthof --------------------------------------------------------------------

glOrthof
  :: MonadIO m
  => GLfloat -- ^ @l@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glOrthof v1 v2 v3 v4 v5 v6 = liftIO $ dyn103 ptr_glOrthof v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glOrthof #-}
ptr_glOrthof :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glOrthof = unsafePerformIO $ getCommand "glOrthof"

-- glOrthofOES -----------------------------------------------------------------

glOrthofOES
  :: MonadIO m
  => GLfloat -- ^ @l@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glOrthofOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn103 ptr_glOrthofOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glOrthofOES #-}
ptr_glOrthofOES :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glOrthofOES = unsafePerformIO $ getCommand "glOrthofOES"

-- glOrthox --------------------------------------------------------------------

glOrthox
  :: MonadIO m
  => GLfixed -- ^ @l@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @b@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @n@.
  -> GLfixed -- ^ @f@.
  -> m ()
glOrthox v1 v2 v3 v4 v5 v6 = liftIO $ dyn310 ptr_glOrthox v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glOrthox #-}
ptr_glOrthox :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glOrthox = unsafePerformIO $ getCommand "glOrthox"

-- glOrthoxOES -----------------------------------------------------------------

glOrthoxOES
  :: MonadIO m
  => GLfixed -- ^ @l@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @b@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @n@.
  -> GLfixed -- ^ @f@.
  -> m ()
glOrthoxOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn310 ptr_glOrthoxOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glOrthoxOES #-}
ptr_glOrthoxOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glOrthoxOES = unsafePerformIO $ getCommand "glOrthoxOES"

-- glPNTrianglesfATI -----------------------------------------------------------

glPNTrianglesfATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PNTrianglesPNameATI](Graphics-GL-Groups.html#PNTrianglesPNameATI).
  -> GLfloat -- ^ @param@.
  -> m ()
glPNTrianglesfATI v1 v2 = liftIO $ dyn0 ptr_glPNTrianglesfATI v1 v2

{-# NOINLINE ptr_glPNTrianglesfATI #-}
ptr_glPNTrianglesfATI :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glPNTrianglesfATI = unsafePerformIO $ getCommand "glPNTrianglesfATI"

-- glPNTrianglesiATI -----------------------------------------------------------

glPNTrianglesiATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PNTrianglesPNameATI](Graphics-GL-Groups.html#PNTrianglesPNameATI).
  -> GLint -- ^ @param@.
  -> m ()
glPNTrianglesiATI v1 v2 = liftIO $ dyn58 ptr_glPNTrianglesiATI v1 v2

{-# NOINLINE ptr_glPNTrianglesiATI #-}
ptr_glPNTrianglesiATI :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPNTrianglesiATI = unsafePerformIO $ getCommand "glPNTrianglesiATI"

-- glPassTexCoordATI -----------------------------------------------------------

glPassTexCoordATI
  :: MonadIO m
  => GLuint -- ^ @dst@.
  -> GLuint -- ^ @coord@.
  -> GLenum -- ^ @swizzle@ of type [SwizzleOpATI](Graphics-GL-Groups.html#SwizzleOpATI).
  -> m ()
glPassTexCoordATI v1 v2 v3 = liftIO $ dyn635 ptr_glPassTexCoordATI v1 v2 v3

{-# NOINLINE ptr_glPassTexCoordATI #-}
ptr_glPassTexCoordATI :: FunPtr (GLuint -> GLuint -> GLenum -> IO ())
ptr_glPassTexCoordATI = unsafePerformIO $ getCommand "glPassTexCoordATI"

-- glPassThrough ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPassThrough.xml OpenGL 2.x>.
glPassThrough
  :: MonadIO m
  => GLfloat -- ^ @token@ of type @FeedbackElement@.
  -> m ()
glPassThrough v1 = liftIO $ dyn85 ptr_glPassThrough v1

{-# NOINLINE ptr_glPassThrough #-}
ptr_glPassThrough :: FunPtr (GLfloat -> IO ())
ptr_glPassThrough = unsafePerformIO $ getCommand "glPassThrough"

-- glPassThroughxOES -----------------------------------------------------------

glPassThroughxOES
  :: MonadIO m
  => GLfixed -- ^ @token@.
  -> m ()
glPassThroughxOES v1 = liftIO $ dyn87 ptr_glPassThroughxOES v1

{-# NOINLINE ptr_glPassThroughxOES #-}
ptr_glPassThroughxOES :: FunPtr (GLfixed -> IO ())
ptr_glPassThroughxOES = unsafePerformIO $ getCommand "glPassThroughxOES"

-- glPatchParameterfv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glPatchParameter.xhtml OpenGL 4.x>.
glPatchParameterfv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PatchParameterName](Graphics-GL-Groups.html#PatchParameterName).
  -> Ptr GLfloat -- ^ @values@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glPatchParameterfv v1 v2 = liftIO $ dyn101 ptr_glPatchParameterfv v1 v2

{-# NOINLINE ptr_glPatchParameterfv #-}
ptr_glPatchParameterfv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glPatchParameterfv = unsafePerformIO $ getCommand "glPatchParameterfv"

-- glPatchParameteri -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glPatchParameter.xhtml OpenGL 4.x>.
glPatchParameteri
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PatchParameterName](Graphics-GL-Groups.html#PatchParameterName).
  -> GLint -- ^ @value@.
  -> m ()
glPatchParameteri v1 v2 = liftIO $ dyn58 ptr_glPatchParameteri v1 v2

{-# NOINLINE ptr_glPatchParameteri #-}
ptr_glPatchParameteri :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPatchParameteri = unsafePerformIO $ getCommand "glPatchParameteri"

-- glPatchParameteriEXT --------------------------------------------------------

-- | This command is an alias for 'glPatchParameteri'.
glPatchParameteriEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PatchParameterName](Graphics-GL-Groups.html#PatchParameterName).
  -> GLint -- ^ @value@.
  -> m ()
glPatchParameteriEXT v1 v2 = liftIO $ dyn58 ptr_glPatchParameteriEXT v1 v2

{-# NOINLINE ptr_glPatchParameteriEXT #-}
ptr_glPatchParameteriEXT :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPatchParameteriEXT = unsafePerformIO $ getCommand "glPatchParameteriEXT"

-- glPatchParameteriOES --------------------------------------------------------

-- | This command is an alias for 'glPatchParameteri'.
glPatchParameteriOES
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PatchParameterName](Graphics-GL-Groups.html#PatchParameterName).
  -> GLint -- ^ @value@.
  -> m ()
glPatchParameteriOES v1 v2 = liftIO $ dyn58 ptr_glPatchParameteriOES v1 v2

{-# NOINLINE ptr_glPatchParameteriOES #-}
ptr_glPatchParameteriOES :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPatchParameteriOES = unsafePerformIO $ getCommand "glPatchParameteriOES"

-- glPathColorGenNV ------------------------------------------------------------

glPathColorGenNV
  :: MonadIO m
  => GLenum -- ^ @color@ of type [PathColor](Graphics-GL-Groups.html#PathColor).
  -> GLenum -- ^ @genMode@ of type [PathGenMode](Graphics-GL-Groups.html#PathGenMode).
  -> GLenum -- ^ @colorFormat@ of type [PathColorFormat](Graphics-GL-Groups.html#PathColorFormat).
  -> Ptr GLfloat -- ^ @coeffs@ pointing to @COMPSIZE(genMode,colorFormat)@ elements of type @GLfloat@.
  -> m ()
glPathColorGenNV v1 v2 v3 v4 = liftIO $ dyn334 ptr_glPathColorGenNV v1 v2 v3 v4

{-# NOINLINE ptr_glPathColorGenNV #-}
ptr_glPathColorGenNV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glPathColorGenNV = unsafePerformIO $ getCommand "glPathColorGenNV"

-- glPathCommandsNV ------------------------------------------------------------

glPathCommandsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @numCommands@.
  -> Ptr GLubyte -- ^ @commands@ pointing to @numCommands@ elements of type @PathCommand@.
  -> GLsizei -- ^ @numCoords@.
  -> GLenum -- ^ @coordType@ of type [PathCoordType](Graphics-GL-Groups.html#PathCoordType).
  -> Ptr a -- ^ @coords@ pointing to @COMPSIZE(numCoords,coordType)@ elements of type @a@.
  -> m ()
glPathCommandsNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn636 ptr_glPathCommandsNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glPathCommandsNV #-}
ptr_glPathCommandsNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glPathCommandsNV = unsafePerformIO $ getCommand "glPathCommandsNV"

-- glPathCoordsNV --------------------------------------------------------------

glPathCoordsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @numCoords@.
  -> GLenum -- ^ @coordType@ of type [PathCoordType](Graphics-GL-Groups.html#PathCoordType).
  -> Ptr a -- ^ @coords@ pointing to @COMPSIZE(numCoords,coordType)@ elements of type @a@.
  -> m ()
glPathCoordsNV v1 v2 v3 v4 = liftIO $ dyn637 ptr_glPathCoordsNV v1 v2 v3 v4

{-# NOINLINE ptr_glPathCoordsNV #-}
ptr_glPathCoordsNV :: FunPtr (GLuint -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glPathCoordsNV = unsafePerformIO $ getCommand "glPathCoordsNV"

-- glPathCoverDepthFuncNV ------------------------------------------------------

glPathCoverDepthFuncNV
  :: MonadIO m
  => GLenum -- ^ @func@ of type [DepthFunction](Graphics-GL-Groups.html#DepthFunction).
  -> m ()
glPathCoverDepthFuncNV v1 = liftIO $ dyn5 ptr_glPathCoverDepthFuncNV v1

{-# NOINLINE ptr_glPathCoverDepthFuncNV #-}
ptr_glPathCoverDepthFuncNV :: FunPtr (GLenum -> IO ())
ptr_glPathCoverDepthFuncNV = unsafePerformIO $ getCommand "glPathCoverDepthFuncNV"

-- glPathDashArrayNV -----------------------------------------------------------

glPathDashArrayNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @dashCount@.
  -> Ptr GLfloat -- ^ @dashArray@ pointing to @dashCount@ elements of type @GLfloat@.
  -> m ()
glPathDashArrayNV v1 v2 v3 = liftIO $ dyn226 ptr_glPathDashArrayNV v1 v2 v3

{-# NOINLINE ptr_glPathDashArrayNV #-}
ptr_glPathDashArrayNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glPathDashArrayNV = unsafePerformIO $ getCommand "glPathDashArrayNV"

-- glPathFogGenNV --------------------------------------------------------------

glPathFogGenNV
  :: MonadIO m
  => GLenum -- ^ @genMode@ of type [PathGenMode](Graphics-GL-Groups.html#PathGenMode).
  -> m ()
glPathFogGenNV v1 = liftIO $ dyn5 ptr_glPathFogGenNV v1

{-# NOINLINE ptr_glPathFogGenNV #-}
ptr_glPathFogGenNV :: FunPtr (GLenum -> IO ())
ptr_glPathFogGenNV = unsafePerformIO $ getCommand "glPathFogGenNV"

-- glPathGlyphIndexArrayNV -----------------------------------------------------

glPathGlyphIndexArrayNV
  :: MonadIO m
  => GLuint -- ^ @firstPathName@.
  -> GLenum -- ^ @fontTarget@.
  -> Ptr a -- ^ @fontName@.
  -> GLbitfield -- ^ @fontStyle@ of type [PathFontStyle](Graphics-GL-Groups.html#PathFontStyle).
  -> GLuint -- ^ @firstGlyphIndex@.
  -> GLsizei -- ^ @numGlyphs@.
  -> GLuint -- ^ @pathParameterTemplate@.
  -> GLfloat -- ^ @emScale@.
  -> m GLenum
glPathGlyphIndexArrayNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn638 ptr_glPathGlyphIndexArrayNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glPathGlyphIndexArrayNV #-}
ptr_glPathGlyphIndexArrayNV :: FunPtr (GLuint -> GLenum -> Ptr a -> GLbitfield -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum)
ptr_glPathGlyphIndexArrayNV = unsafePerformIO $ getCommand "glPathGlyphIndexArrayNV"

-- glPathGlyphIndexRangeNV -----------------------------------------------------

glPathGlyphIndexRangeNV
  :: MonadIO m
  => GLenum -- ^ @fontTarget@.
  -> Ptr a -- ^ @fontName@.
  -> GLbitfield -- ^ @fontStyle@ of type [PathFontStyle](Graphics-GL-Groups.html#PathFontStyle).
  -> GLuint -- ^ @pathParameterTemplate@.
  -> GLfloat -- ^ @emScale@.
  -> Ptr GLuint -- ^ @baseAndCount@.
  -> m GLenum
glPathGlyphIndexRangeNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn639 ptr_glPathGlyphIndexRangeNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glPathGlyphIndexRangeNV #-}
ptr_glPathGlyphIndexRangeNV :: FunPtr (GLenum -> Ptr a -> GLbitfield -> GLuint -> GLfloat -> Ptr GLuint -> IO GLenum)
ptr_glPathGlyphIndexRangeNV = unsafePerformIO $ getCommand "glPathGlyphIndexRangeNV"

-- glPathGlyphRangeNV ----------------------------------------------------------

glPathGlyphRangeNV
  :: MonadIO m
  => GLuint -- ^ @firstPathName@ of type @Path@.
  -> GLenum -- ^ @fontTarget@ of type [PathFontTarget](Graphics-GL-Groups.html#PathFontTarget).
  -> Ptr a -- ^ @fontName@ pointing to @COMPSIZE(fontTarget,fontName)@ elements of type @a@.
  -> GLbitfield -- ^ @fontStyle@ of type [PathFontStyle](Graphics-GL-Groups.html#PathFontStyle).
  -> GLuint -- ^ @firstGlyph@.
  -> GLsizei -- ^ @numGlyphs@.
  -> GLenum -- ^ @handleMissingGlyphs@ of type [PathHandleMissingGlyphs](Graphics-GL-Groups.html#PathHandleMissingGlyphs).
  -> GLuint -- ^ @pathParameterTemplate@ of type @Path@.
  -> GLfloat -- ^ @emScale@.
  -> m ()
glPathGlyphRangeNV v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn640 ptr_glPathGlyphRangeNV v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glPathGlyphRangeNV #-}
ptr_glPathGlyphRangeNV :: FunPtr (GLuint -> GLenum -> Ptr a -> GLbitfield -> GLuint -> GLsizei -> GLenum -> GLuint -> GLfloat -> IO ())
ptr_glPathGlyphRangeNV = unsafePerformIO $ getCommand "glPathGlyphRangeNV"

-- glPathGlyphsNV --------------------------------------------------------------

glPathGlyphsNV
  :: MonadIO m
  => GLuint -- ^ @firstPathName@ of type @Path@.
  -> GLenum -- ^ @fontTarget@ of type [PathFontTarget](Graphics-GL-Groups.html#PathFontTarget).
  -> Ptr a -- ^ @fontName@ pointing to @COMPSIZE(fontTarget,fontName)@ elements of type @a@.
  -> GLbitfield -- ^ @fontStyle@ of type [PathFontStyle](Graphics-GL-Groups.html#PathFontStyle).
  -> GLsizei -- ^ @numGlyphs@.
  -> GLenum -- ^ @type@ of type [PathElementType](Graphics-GL-Groups.html#PathElementType).
  -> Ptr b -- ^ @charcodes@ pointing to @COMPSIZE(numGlyphs,type,charcodes)@ elements of type @b@.
  -> GLenum -- ^ @handleMissingGlyphs@ of type [PathHandleMissingGlyphs](Graphics-GL-Groups.html#PathHandleMissingGlyphs).
  -> GLuint -- ^ @pathParameterTemplate@ of type @Path@.
  -> GLfloat -- ^ @emScale@.
  -> m ()
glPathGlyphsNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn641 ptr_glPathGlyphsNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glPathGlyphsNV #-}
ptr_glPathGlyphsNV :: FunPtr (GLuint -> GLenum -> Ptr a -> GLbitfield -> GLsizei -> GLenum -> Ptr b -> GLenum -> GLuint -> GLfloat -> IO ())
ptr_glPathGlyphsNV = unsafePerformIO $ getCommand "glPathGlyphsNV"

-- glPathMemoryGlyphIndexArrayNV -----------------------------------------------

glPathMemoryGlyphIndexArrayNV
  :: MonadIO m
  => GLuint -- ^ @firstPathName@.
  -> GLenum -- ^ @fontTarget@.
  -> GLsizeiptr -- ^ @fontSize@.
  -> Ptr a -- ^ @fontData@.
  -> GLsizei -- ^ @faceIndex@.
  -> GLuint -- ^ @firstGlyphIndex@.
  -> GLsizei -- ^ @numGlyphs@.
  -> GLuint -- ^ @pathParameterTemplate@.
  -> GLfloat -- ^ @emScale@.
  -> m GLenum
glPathMemoryGlyphIndexArrayNV v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn642 ptr_glPathMemoryGlyphIndexArrayNV v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glPathMemoryGlyphIndexArrayNV #-}
ptr_glPathMemoryGlyphIndexArrayNV :: FunPtr (GLuint -> GLenum -> GLsizeiptr -> Ptr a -> GLsizei -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum)
ptr_glPathMemoryGlyphIndexArrayNV = unsafePerformIO $ getCommand "glPathMemoryGlyphIndexArrayNV"

-- glPathParameterfNV ----------------------------------------------------------

glPathParameterfNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type [PathParameter](Graphics-GL-Groups.html#PathParameter).
  -> GLfloat -- ^ @value@.
  -> m ()
glPathParameterfNV v1 v2 v3 = liftIO $ dyn514 ptr_glPathParameterfNV v1 v2 v3

{-# NOINLINE ptr_glPathParameterfNV #-}
ptr_glPathParameterfNV :: FunPtr (GLuint -> GLenum -> GLfloat -> IO ())
ptr_glPathParameterfNV = unsafePerformIO $ getCommand "glPathParameterfNV"

-- glPathParameterfvNV ---------------------------------------------------------

glPathParameterfvNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type [PathParameter](Graphics-GL-Groups.html#PathParameter).
  -> Ptr GLfloat -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glPathParameterfvNV v1 v2 v3 = liftIO $ dyn364 ptr_glPathParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glPathParameterfvNV #-}
ptr_glPathParameterfvNV :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glPathParameterfvNV = unsafePerformIO $ getCommand "glPathParameterfvNV"

-- glPathParameteriNV ----------------------------------------------------------

glPathParameteriNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type [PathParameter](Graphics-GL-Groups.html#PathParameter).
  -> GLint -- ^ @value@.
  -> m ()
glPathParameteriNV v1 v2 v3 = liftIO $ dyn491 ptr_glPathParameteriNV v1 v2 v3

{-# NOINLINE ptr_glPathParameteriNV #-}
ptr_glPathParameteriNV :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glPathParameteriNV = unsafePerformIO $ getCommand "glPathParameteriNV"

-- glPathParameterivNV ---------------------------------------------------------

glPathParameterivNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type [PathParameter](Graphics-GL-Groups.html#PathParameter).
  -> Ptr GLint -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glPathParameterivNV v1 v2 v3 = liftIO $ dyn348 ptr_glPathParameterivNV v1 v2 v3

{-# NOINLINE ptr_glPathParameterivNV #-}
ptr_glPathParameterivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glPathParameterivNV = unsafePerformIO $ getCommand "glPathParameterivNV"

-- glPathStencilDepthOffsetNV --------------------------------------------------

glPathStencilDepthOffsetNV
  :: MonadIO m
  => GLfloat -- ^ @factor@.
  -> GLfloat -- ^ @units@.
  -> m ()
glPathStencilDepthOffsetNV v1 v2 = liftIO $ dyn230 ptr_glPathStencilDepthOffsetNV v1 v2

{-# NOINLINE ptr_glPathStencilDepthOffsetNV #-}
ptr_glPathStencilDepthOffsetNV :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glPathStencilDepthOffsetNV = unsafePerformIO $ getCommand "glPathStencilDepthOffsetNV"

-- glPathStencilFuncNV ---------------------------------------------------------

glPathStencilFuncNV
  :: MonadIO m
  => GLenum -- ^ @func@ of type [StencilFunction](Graphics-GL-Groups.html#StencilFunction).
  -> GLint -- ^ @ref@ of type @ClampedStencilValue@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> m ()
glPathStencilFuncNV v1 v2 v3 = liftIO $ dyn643 ptr_glPathStencilFuncNV v1 v2 v3

{-# NOINLINE ptr_glPathStencilFuncNV #-}
ptr_glPathStencilFuncNV :: FunPtr (GLenum -> GLint -> GLuint -> IO ())
ptr_glPathStencilFuncNV = unsafePerformIO $ getCommand "glPathStencilFuncNV"

-- glPathStringNV --------------------------------------------------------------

glPathStringNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @format@ of type [PathStringFormat](Graphics-GL-Groups.html#PathStringFormat).
  -> GLsizei -- ^ @length@.
  -> Ptr a -- ^ @pathString@ pointing to @length@ elements of type @a@.
  -> m ()
glPathStringNV v1 v2 v3 v4 = liftIO $ dyn644 ptr_glPathStringNV v1 v2 v3 v4

{-# NOINLINE ptr_glPathStringNV #-}
ptr_glPathStringNV :: FunPtr (GLuint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glPathStringNV = unsafePerformIO $ getCommand "glPathStringNV"

-- glPathSubCommandsNV ---------------------------------------------------------

glPathSubCommandsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @commandStart@.
  -> GLsizei -- ^ @commandsToDelete@.
  -> GLsizei -- ^ @numCommands@.
  -> Ptr GLubyte -- ^ @commands@ pointing to @numCommands@ elements of type @PathCommand@.
  -> GLsizei -- ^ @numCoords@.
  -> GLenum -- ^ @coordType@ of type [PathCoordType](Graphics-GL-Groups.html#PathCoordType).
  -> Ptr a -- ^ @coords@ pointing to @COMPSIZE(numCoords,coordType)@ elements of type @a@.
  -> m ()
glPathSubCommandsNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn645 ptr_glPathSubCommandsNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glPathSubCommandsNV #-}
ptr_glPathSubCommandsNV :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glPathSubCommandsNV = unsafePerformIO $ getCommand "glPathSubCommandsNV"

-- glPathSubCoordsNV -----------------------------------------------------------

glPathSubCoordsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @coordStart@.
  -> GLsizei -- ^ @numCoords@.
  -> GLenum -- ^ @coordType@ of type [PathCoordType](Graphics-GL-Groups.html#PathCoordType).
  -> Ptr a -- ^ @coords@ pointing to @COMPSIZE(numCoords,coordType)@ elements of type @a@.
  -> m ()
glPathSubCoordsNV v1 v2 v3 v4 v5 = liftIO $ dyn646 ptr_glPathSubCoordsNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glPathSubCoordsNV #-}
ptr_glPathSubCoordsNV :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glPathSubCoordsNV = unsafePerformIO $ getCommand "glPathSubCoordsNV"

-- glPathTexGenNV --------------------------------------------------------------

glPathTexGenNV
  :: MonadIO m
  => GLenum -- ^ @texCoordSet@ of type [PathColor](Graphics-GL-Groups.html#PathColor).
  -> GLenum -- ^ @genMode@ of type [PathGenMode](Graphics-GL-Groups.html#PathGenMode).
  -> GLint -- ^ @components@.
  -> Ptr GLfloat -- ^ @coeffs@ pointing to @COMPSIZE(genMode,components)@ elements of type @GLfloat@.
  -> m ()
glPathTexGenNV v1 v2 v3 v4 = liftIO $ dyn647 ptr_glPathTexGenNV v1 v2 v3 v4

{-# NOINLINE ptr_glPathTexGenNV #-}
ptr_glPathTexGenNV :: FunPtr (GLenum -> GLenum -> GLint -> Ptr GLfloat -> IO ())
ptr_glPathTexGenNV = unsafePerformIO $ getCommand "glPathTexGenNV"

-- glPauseTransformFeedback ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glPauseTransformFeedback.xhtml OpenGL 4.x>.
glPauseTransformFeedback
  :: MonadIO m
  => m ()
glPauseTransformFeedback = liftIO $ dyn11 ptr_glPauseTransformFeedback

{-# NOINLINE ptr_glPauseTransformFeedback #-}
ptr_glPauseTransformFeedback :: FunPtr (IO ())
ptr_glPauseTransformFeedback = unsafePerformIO $ getCommand "glPauseTransformFeedback"

-- glPauseTransformFeedbackNV --------------------------------------------------

-- | This command is an alias for 'glPauseTransformFeedback'.
glPauseTransformFeedbackNV
  :: MonadIO m
  => m ()
glPauseTransformFeedbackNV = liftIO $ dyn11 ptr_glPauseTransformFeedbackNV

{-# NOINLINE ptr_glPauseTransformFeedbackNV #-}
ptr_glPauseTransformFeedbackNV :: FunPtr (IO ())
ptr_glPauseTransformFeedbackNV = unsafePerformIO $ getCommand "glPauseTransformFeedbackNV"

-- glPixelDataRangeNV ----------------------------------------------------------

glPixelDataRangeNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [PixelDataRangeTargetNV](Graphics-GL-Groups.html#PixelDataRangeTargetNV).
  -> GLsizei -- ^ @length@.
  -> Ptr a -- ^ @pointer@ pointing to @length@ elements of type @a@.
  -> m ()
glPixelDataRangeNV v1 v2 v3 = liftIO $ dyn49 ptr_glPixelDataRangeNV v1 v2 v3

{-# NOINLINE ptr_glPixelDataRangeNV #-}
ptr_glPixelDataRangeNV :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glPixelDataRangeNV = unsafePerformIO $ getCommand "glPixelDataRangeNV"

-- glPixelMapfv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPixelMap.xml OpenGL 2.x>.
glPixelMapfv
  :: MonadIO m
  => GLenum -- ^ @map@ of type [PixelMap](Graphics-GL-Groups.html#PixelMap).
  -> GLsizei -- ^ @mapsize@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @values@ pointing to @mapsize@ elements of type @GLfloat@.
  -> m ()
glPixelMapfv v1 v2 v3 = liftIO $ dyn233 ptr_glPixelMapfv v1 v2 v3

{-# NOINLINE ptr_glPixelMapfv #-}
ptr_glPixelMapfv :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glPixelMapfv = unsafePerformIO $ getCommand "glPixelMapfv"

