{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F21
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

module Graphics.GL.Functions.F21 (
  glProgramNamedParameter4fNV,
  glProgramNamedParameter4fvNV,
  glProgramParameter4dNV,
  glProgramParameter4dvNV,
  glProgramParameter4fNV,
  glProgramParameter4fvNV,
  glProgramParameteri,
  glProgramParameteriARB,
  glProgramParameteriEXT,
  glProgramParameters4dvNV,
  glProgramParameters4fvNV,
  glProgramPathFragmentInputGenNV,
  glProgramStringARB,
  glProgramSubroutineParametersuivNV,
  glProgramUniform1d,
  glProgramUniform1dEXT,
  glProgramUniform1dv,
  glProgramUniform1dvEXT,
  glProgramUniform1f,
  glProgramUniform1fEXT,
  glProgramUniform1fv,
  glProgramUniform1fvEXT,
  glProgramUniform1i,
  glProgramUniform1i64ARB,
  glProgramUniform1i64NV,
  glProgramUniform1i64vARB,
  glProgramUniform1i64vNV,
  glProgramUniform1iEXT,
  glProgramUniform1iv,
  glProgramUniform1ivEXT,
  glProgramUniform1ui,
  glProgramUniform1ui64ARB,
  glProgramUniform1ui64NV,
  glProgramUniform1ui64vARB,
  glProgramUniform1ui64vNV,
  glProgramUniform1uiEXT,
  glProgramUniform1uiv,
  glProgramUniform1uivEXT,
  glProgramUniform2d,
  glProgramUniform2dEXT,
  glProgramUniform2dv,
  glProgramUniform2dvEXT,
  glProgramUniform2f,
  glProgramUniform2fEXT,
  glProgramUniform2fv,
  glProgramUniform2fvEXT,
  glProgramUniform2i,
  glProgramUniform2i64ARB,
  glProgramUniform2i64NV,
  glProgramUniform2i64vARB,
  glProgramUniform2i64vNV,
  glProgramUniform2iEXT,
  glProgramUniform2iv,
  glProgramUniform2ivEXT,
  glProgramUniform2ui,
  glProgramUniform2ui64ARB,
  glProgramUniform2ui64NV,
  glProgramUniform2ui64vARB,
  glProgramUniform2ui64vNV,
  glProgramUniform2uiEXT,
  glProgramUniform2uiv,
  glProgramUniform2uivEXT,
  glProgramUniform3d,
  glProgramUniform3dEXT,
  glProgramUniform3dv,
  glProgramUniform3dvEXT,
  glProgramUniform3f,
  glProgramUniform3fEXT,
  glProgramUniform3fv,
  glProgramUniform3fvEXT,
  glProgramUniform3i,
  glProgramUniform3i64ARB,
  glProgramUniform3i64NV,
  glProgramUniform3i64vARB,
  glProgramUniform3i64vNV,
  glProgramUniform3iEXT,
  glProgramUniform3iv,
  glProgramUniform3ivEXT,
  glProgramUniform3ui,
  glProgramUniform3ui64ARB,
  glProgramUniform3ui64NV,
  glProgramUniform3ui64vARB,
  glProgramUniform3ui64vNV,
  glProgramUniform3uiEXT,
  glProgramUniform3uiv,
  glProgramUniform3uivEXT,
  glProgramUniform4d,
  glProgramUniform4dEXT,
  glProgramUniform4dv,
  glProgramUniform4dvEXT,
  glProgramUniform4f,
  glProgramUniform4fEXT,
  glProgramUniform4fv,
  glProgramUniform4fvEXT,
  glProgramUniform4i,
  glProgramUniform4i64ARB,
  glProgramUniform4i64NV,
  glProgramUniform4i64vARB,
  glProgramUniform4i64vNV,
  glProgramUniform4iEXT
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glProgramNamedParameter4fNV -------------------------------------------------

-- | The vector equivalent of this command is 'glProgramNamedParameter4fvNV'.
glProgramNamedParameter4fNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @name@ pointing to @1@ element of type @GLubyte@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glProgramNamedParameter4fNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn666 ptr_glProgramNamedParameter4fNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glProgramNamedParameter4fNV #-}
ptr_glProgramNamedParameter4fNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramNamedParameter4fNV = unsafePerformIO $ getCommand "glProgramNamedParameter4fNV"

-- glProgramNamedParameter4fvNV ------------------------------------------------

glProgramNamedParameter4fvNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @name@ pointing to @1@ element of type @GLubyte@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glProgramNamedParameter4fvNV v1 v2 v3 v4 = liftIO $ dyn411 ptr_glProgramNamedParameter4fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramNamedParameter4fvNV #-}
ptr_glProgramNamedParameter4fvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> IO ())
ptr_glProgramNamedParameter4fvNV = unsafePerformIO $ getCommand "glProgramNamedParameter4fvNV"

-- glProgramParameter4dNV ------------------------------------------------------

-- | The vector equivalent of this command is 'glProgramParameter4dvNV'.
glProgramParameter4dNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glProgramParameter4dNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn661 ptr_glProgramParameter4dNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramParameter4dNV #-}
ptr_glProgramParameter4dNV :: FunPtr (GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramParameter4dNV = unsafePerformIO $ getCommand "glProgramParameter4dNV"

-- glProgramParameter4dvNV -----------------------------------------------------

glProgramParameter4dvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glProgramParameter4dvNV v1 v2 v3 = liftIO $ dyn344 ptr_glProgramParameter4dvNV v1 v2 v3

{-# NOINLINE ptr_glProgramParameter4dvNV #-}
ptr_glProgramParameter4dvNV :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glProgramParameter4dvNV = unsafePerformIO $ getCommand "glProgramParameter4dvNV"

-- glProgramParameter4fNV ------------------------------------------------------

-- | The vector equivalent of this command is 'glProgramParameter4fvNV'.
glProgramParameter4fNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glProgramParameter4fNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn662 ptr_glProgramParameter4fNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramParameter4fNV #-}
ptr_glProgramParameter4fNV :: FunPtr (GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramParameter4fNV = unsafePerformIO $ getCommand "glProgramParameter4fNV"

-- glProgramParameter4fvNV -----------------------------------------------------

glProgramParameter4fvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glProgramParameter4fvNV v1 v2 v3 = liftIO $ dyn278 ptr_glProgramParameter4fvNV v1 v2 v3

{-# NOINLINE ptr_glProgramParameter4fvNV #-}
ptr_glProgramParameter4fvNV :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glProgramParameter4fvNV = unsafePerformIO $ getCommand "glProgramParameter4fvNV"

-- glProgramParameteri ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramParameter.xhtml OpenGL 4.x>.
glProgramParameteri
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @pname@ of type [ProgramParameterPName](Graphics-GL-Groups.html#ProgramParameterPName).
  -> GLint -- ^ @value@.
  -> m ()
glProgramParameteri v1 v2 v3 = liftIO $ dyn491 ptr_glProgramParameteri v1 v2 v3

{-# NOINLINE ptr_glProgramParameteri #-}
ptr_glProgramParameteri :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glProgramParameteri = unsafePerformIO $ getCommand "glProgramParameteri"

-- glProgramParameteriARB ------------------------------------------------------

-- | This command is an alias for 'glProgramParameteri'.
glProgramParameteriARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @pname@ of type [ProgramParameterPName](Graphics-GL-Groups.html#ProgramParameterPName).
  -> GLint -- ^ @value@.
  -> m ()
glProgramParameteriARB v1 v2 v3 = liftIO $ dyn491 ptr_glProgramParameteriARB v1 v2 v3

{-# NOINLINE ptr_glProgramParameteriARB #-}
ptr_glProgramParameteriARB :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glProgramParameteriARB = unsafePerformIO $ getCommand "glProgramParameteriARB"

-- glProgramParameteriEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramParameteri'.
glProgramParameteriEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @pname@ of type [ProgramParameterPName](Graphics-GL-Groups.html#ProgramParameterPName).
  -> GLint -- ^ @value@.
  -> m ()
glProgramParameteriEXT v1 v2 v3 = liftIO $ dyn491 ptr_glProgramParameteriEXT v1 v2 v3

{-# NOINLINE ptr_glProgramParameteriEXT #-}
ptr_glProgramParameteriEXT :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glProgramParameteriEXT = unsafePerformIO $ getCommand "glProgramParameteriEXT"

-- glProgramParameters4dvNV ----------------------------------------------------

glProgramParameters4dvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glProgramParameters4dvNV v1 v2 v3 v4 = liftIO $ dyn667 ptr_glProgramParameters4dvNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramParameters4dvNV #-}
ptr_glProgramParameters4dvNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramParameters4dvNV = unsafePerformIO $ getCommand "glProgramParameters4dvNV"

-- glProgramParameters4fvNV ----------------------------------------------------

glProgramParameters4fvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glProgramParameters4fvNV v1 v2 v3 v4 = liftIO $ dyn297 ptr_glProgramParameters4fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramParameters4fvNV #-}
ptr_glProgramParameters4fvNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramParameters4fvNV = unsafePerformIO $ getCommand "glProgramParameters4fvNV"

-- glProgramPathFragmentInputGenNV ---------------------------------------------

glProgramPathFragmentInputGenNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLenum -- ^ @genMode@.
  -> GLint -- ^ @components@.
  -> Ptr GLfloat -- ^ @coeffs@.
  -> m ()
glProgramPathFragmentInputGenNV v1 v2 v3 v4 v5 = liftIO $ dyn668 ptr_glProgramPathFragmentInputGenNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramPathFragmentInputGenNV #-}
ptr_glProgramPathFragmentInputGenNV :: FunPtr (GLuint -> GLint -> GLenum -> GLint -> Ptr GLfloat -> IO ())
ptr_glProgramPathFragmentInputGenNV = unsafePerformIO $ getCommand "glProgramPathFragmentInputGenNV"

-- glProgramStringARB ----------------------------------------------------------

glProgramStringARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ProgramTarget](Graphics-GL-Groups.html#ProgramTarget).
  -> GLenum -- ^ @format@ of type [ProgramFormat](Graphics-GL-Groups.html#ProgramFormat).
  -> GLsizei -- ^ @len@.
  -> Ptr a -- ^ @string@ pointing to @len@ elements of type @a@.
  -> m ()
glProgramStringARB v1 v2 v3 v4 = liftIO $ dyn669 ptr_glProgramStringARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramStringARB #-}
ptr_glProgramStringARB :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glProgramStringARB = unsafePerformIO $ getCommand "glProgramStringARB"

-- glProgramSubroutineParametersuivNV ------------------------------------------

glProgramSubroutineParametersuivNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @params@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glProgramSubroutineParametersuivNV v1 v2 v3 = liftIO $ dyn204 ptr_glProgramSubroutineParametersuivNV v1 v2 v3

{-# NOINLINE ptr_glProgramSubroutineParametersuivNV #-}
ptr_glProgramSubroutineParametersuivNV :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramSubroutineParametersuivNV = unsafePerformIO $ getCommand "glProgramSubroutineParametersuivNV"

-- glProgramUniform1d ----------------------------------------------------------

glProgramUniform1d
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @v0@.
  -> m ()
glProgramUniform1d v1 v2 v3 = liftIO $ dyn670 ptr_glProgramUniform1d v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1d #-}
ptr_glProgramUniform1d :: FunPtr (GLuint -> GLint -> GLdouble -> IO ())
ptr_glProgramUniform1d = unsafePerformIO $ getCommand "glProgramUniform1d"

-- glProgramUniform1dEXT -------------------------------------------------------

glProgramUniform1dEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> m ()
glProgramUniform1dEXT v1 v2 v3 = liftIO $ dyn670 ptr_glProgramUniform1dEXT v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1dEXT #-}
ptr_glProgramUniform1dEXT :: FunPtr (GLuint -> GLint -> GLdouble -> IO ())
ptr_glProgramUniform1dEXT = unsafePerformIO $ getCommand "glProgramUniform1dEXT"

-- glProgramUniform1dv ---------------------------------------------------------

glProgramUniform1dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniform1dv v1 v2 v3 v4 = liftIO $ dyn478 ptr_glProgramUniform1dv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1dv #-}
ptr_glProgramUniform1dv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform1dv = unsafePerformIO $ getCommand "glProgramUniform1dv"

-- glProgramUniform1dvEXT ------------------------------------------------------

glProgramUniform1dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniform1dvEXT v1 v2 v3 v4 = liftIO $ dyn478 ptr_glProgramUniform1dvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1dvEXT #-}
ptr_glProgramUniform1dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform1dvEXT = unsafePerformIO $ getCommand "glProgramUniform1dvEXT"

-- glProgramUniform1f ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1f
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> m ()
glProgramUniform1f v1 v2 v3 = liftIO $ dyn671 ptr_glProgramUniform1f v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1f #-}
ptr_glProgramUniform1f :: FunPtr (GLuint -> GLint -> GLfloat -> IO ())
ptr_glProgramUniform1f = unsafePerformIO $ getCommand "glProgramUniform1f"

-- glProgramUniform1fEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform1f'.
glProgramUniform1fEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> m ()
glProgramUniform1fEXT v1 v2 v3 = liftIO $ dyn671 ptr_glProgramUniform1fEXT v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1fEXT #-}
ptr_glProgramUniform1fEXT :: FunPtr (GLuint -> GLint -> GLfloat -> IO ())
ptr_glProgramUniform1fEXT = unsafePerformIO $ getCommand "glProgramUniform1fEXT"

-- glProgramUniform1fv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glProgramUniform1fv v1 v2 v3 v4 = liftIO $ dyn479 ptr_glProgramUniform1fv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1fv #-}
ptr_glProgramUniform1fv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform1fv = unsafePerformIO $ getCommand "glProgramUniform1fv"

-- glProgramUniform1fvEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform1fv'.
glProgramUniform1fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glProgramUniform1fvEXT v1 v2 v3 v4 = liftIO $ dyn479 ptr_glProgramUniform1fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1fvEXT #-}
ptr_glProgramUniform1fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform1fvEXT = unsafePerformIO $ getCommand "glProgramUniform1fvEXT"

-- glProgramUniform1i ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1i
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> m ()
glProgramUniform1i v1 v2 v3 = liftIO $ dyn672 ptr_glProgramUniform1i v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1i #-}
ptr_glProgramUniform1i :: FunPtr (GLuint -> GLint -> GLint -> IO ())
ptr_glProgramUniform1i = unsafePerformIO $ getCommand "glProgramUniform1i"

-- glProgramUniform1i64ARB -----------------------------------------------------

glProgramUniform1i64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> m ()
glProgramUniform1i64ARB v1 v2 v3 = liftIO $ dyn673 ptr_glProgramUniform1i64ARB v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1i64ARB #-}
ptr_glProgramUniform1i64ARB :: FunPtr (GLuint -> GLint -> GLint64 -> IO ())
ptr_glProgramUniform1i64ARB = unsafePerformIO $ getCommand "glProgramUniform1i64ARB"

-- glProgramUniform1i64NV ------------------------------------------------------

glProgramUniform1i64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> m ()
glProgramUniform1i64NV v1 v2 v3 = liftIO $ dyn674 ptr_glProgramUniform1i64NV v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1i64NV #-}
ptr_glProgramUniform1i64NV :: FunPtr (GLuint -> GLint -> GLint64EXT -> IO ())
ptr_glProgramUniform1i64NV = unsafePerformIO $ getCommand "glProgramUniform1i64NV"

-- glProgramUniform1i64vARB ----------------------------------------------------

glProgramUniform1i64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count@ elements of type @GLint64@.
  -> m ()
glProgramUniform1i64vARB v1 v2 v3 v4 = liftIO $ dyn480 ptr_glProgramUniform1i64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1i64vARB #-}
ptr_glProgramUniform1i64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glProgramUniform1i64vARB = unsafePerformIO $ getCommand "glProgramUniform1i64vARB"

-- glProgramUniform1i64vNV -----------------------------------------------------

glProgramUniform1i64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count@ elements of type @GLint64EXT@.
  -> m ()
glProgramUniform1i64vNV v1 v2 v3 v4 = liftIO $ dyn675 ptr_glProgramUniform1i64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1i64vNV #-}
ptr_glProgramUniform1i64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glProgramUniform1i64vNV = unsafePerformIO $ getCommand "glProgramUniform1i64vNV"

-- glProgramUniform1iEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform1i'.
glProgramUniform1iEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> m ()
glProgramUniform1iEXT v1 v2 v3 = liftIO $ dyn672 ptr_glProgramUniform1iEXT v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1iEXT #-}
ptr_glProgramUniform1iEXT :: FunPtr (GLuint -> GLint -> GLint -> IO ())
ptr_glProgramUniform1iEXT = unsafePerformIO $ getCommand "glProgramUniform1iEXT"

-- glProgramUniform1iv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1iv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count@ elements of type @GLint@.
  -> m ()
glProgramUniform1iv v1 v2 v3 v4 = liftIO $ dyn481 ptr_glProgramUniform1iv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1iv #-}
ptr_glProgramUniform1iv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform1iv = unsafePerformIO $ getCommand "glProgramUniform1iv"

-- glProgramUniform1ivEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform1iv'.
glProgramUniform1ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count@ elements of type @GLint@.
  -> m ()
glProgramUniform1ivEXT v1 v2 v3 v4 = liftIO $ dyn481 ptr_glProgramUniform1ivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1ivEXT #-}
ptr_glProgramUniform1ivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform1ivEXT = unsafePerformIO $ getCommand "glProgramUniform1ivEXT"

-- glProgramUniform1ui ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1ui
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> m ()
glProgramUniform1ui v1 v2 v3 = liftIO $ dyn676 ptr_glProgramUniform1ui v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1ui #-}
ptr_glProgramUniform1ui :: FunPtr (GLuint -> GLint -> GLuint -> IO ())
ptr_glProgramUniform1ui = unsafePerformIO $ getCommand "glProgramUniform1ui"

-- glProgramUniform1ui64ARB ----------------------------------------------------

glProgramUniform1ui64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> m ()
glProgramUniform1ui64ARB v1 v2 v3 = liftIO $ dyn677 ptr_glProgramUniform1ui64ARB v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1ui64ARB #-}
ptr_glProgramUniform1ui64ARB :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ())
ptr_glProgramUniform1ui64ARB = unsafePerformIO $ getCommand "glProgramUniform1ui64ARB"

-- glProgramUniform1ui64NV -----------------------------------------------------

glProgramUniform1ui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> m ()
glProgramUniform1ui64NV v1 v2 v3 = liftIO $ dyn678 ptr_glProgramUniform1ui64NV v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1ui64NV #-}
ptr_glProgramUniform1ui64NV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> IO ())
ptr_glProgramUniform1ui64NV = unsafePerformIO $ getCommand "glProgramUniform1ui64NV"

-- glProgramUniform1ui64vARB ---------------------------------------------------

glProgramUniform1ui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glProgramUniform1ui64vARB v1 v2 v3 v4 = liftIO $ dyn482 ptr_glProgramUniform1ui64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1ui64vARB #-}
ptr_glProgramUniform1ui64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniform1ui64vARB = unsafePerformIO $ getCommand "glProgramUniform1ui64vARB"

-- glProgramUniform1ui64vNV ----------------------------------------------------

glProgramUniform1ui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count@ elements of type @GLuint64EXT@.
  -> m ()
glProgramUniform1ui64vNV v1 v2 v3 v4 = liftIO $ dyn679 ptr_glProgramUniform1ui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1ui64vNV #-}
ptr_glProgramUniform1ui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glProgramUniform1ui64vNV = unsafePerformIO $ getCommand "glProgramUniform1ui64vNV"

-- glProgramUniform1uiEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform1ui'.
glProgramUniform1uiEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> m ()
glProgramUniform1uiEXT v1 v2 v3 = liftIO $ dyn676 ptr_glProgramUniform1uiEXT v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1uiEXT #-}
ptr_glProgramUniform1uiEXT :: FunPtr (GLuint -> GLint -> GLuint -> IO ())
ptr_glProgramUniform1uiEXT = unsafePerformIO $ getCommand "glProgramUniform1uiEXT"

-- glProgramUniform1uiv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1uiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glProgramUniform1uiv v1 v2 v3 v4 = liftIO $ dyn483 ptr_glProgramUniform1uiv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1uiv #-}
ptr_glProgramUniform1uiv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform1uiv = unsafePerformIO $ getCommand "glProgramUniform1uiv"

-- glProgramUniform1uivEXT -----------------------------------------------------

-- | This command is an alias for 'glProgramUniform1uiv'.
glProgramUniform1uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glProgramUniform1uivEXT v1 v2 v3 v4 = liftIO $ dyn483 ptr_glProgramUniform1uivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1uivEXT #-}
ptr_glProgramUniform1uivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform1uivEXT = unsafePerformIO $ getCommand "glProgramUniform1uivEXT"

-- glProgramUniform2d ----------------------------------------------------------

glProgramUniform2d
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @v0@.
  -> GLdouble -- ^ @v1@.
  -> m ()
glProgramUniform2d v1 v2 v3 v4 = liftIO $ dyn680 ptr_glProgramUniform2d v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2d #-}
ptr_glProgramUniform2d :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform2d = unsafePerformIO $ getCommand "glProgramUniform2d"

-- glProgramUniform2dEXT -------------------------------------------------------

glProgramUniform2dEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glProgramUniform2dEXT v1 v2 v3 v4 = liftIO $ dyn680 ptr_glProgramUniform2dEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2dEXT #-}
ptr_glProgramUniform2dEXT :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform2dEXT = unsafePerformIO $ getCommand "glProgramUniform2dEXT"

-- glProgramUniform2dv ---------------------------------------------------------

glProgramUniform2dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*2@ elements of type @GLdouble@.
  -> m ()
glProgramUniform2dv v1 v2 v3 v4 = liftIO $ dyn478 ptr_glProgramUniform2dv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2dv #-}
ptr_glProgramUniform2dv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform2dv = unsafePerformIO $ getCommand "glProgramUniform2dv"

-- glProgramUniform2dvEXT ------------------------------------------------------

glProgramUniform2dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*2@ elements of type @GLdouble@.
  -> m ()
glProgramUniform2dvEXT v1 v2 v3 v4 = liftIO $ dyn478 ptr_glProgramUniform2dvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2dvEXT #-}
ptr_glProgramUniform2dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform2dvEXT = unsafePerformIO $ getCommand "glProgramUniform2dvEXT"

-- glProgramUniform2f ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2f
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> m ()
glProgramUniform2f v1 v2 v3 v4 = liftIO $ dyn681 ptr_glProgramUniform2f v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2f #-}
ptr_glProgramUniform2f :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform2f = unsafePerformIO $ getCommand "glProgramUniform2f"

-- glProgramUniform2fEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform2f'.
glProgramUniform2fEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> m ()
glProgramUniform2fEXT v1 v2 v3 v4 = liftIO $ dyn681 ptr_glProgramUniform2fEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2fEXT #-}
ptr_glProgramUniform2fEXT :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform2fEXT = unsafePerformIO $ getCommand "glProgramUniform2fEXT"

-- glProgramUniform2fv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*2@ elements of type @GLfloat@.
  -> m ()
glProgramUniform2fv v1 v2 v3 v4 = liftIO $ dyn479 ptr_glProgramUniform2fv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2fv #-}
ptr_glProgramUniform2fv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform2fv = unsafePerformIO $ getCommand "glProgramUniform2fv"

-- glProgramUniform2fvEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform2fv'.
glProgramUniform2fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*2@ elements of type @GLfloat@.
  -> m ()
glProgramUniform2fvEXT v1 v2 v3 v4 = liftIO $ dyn479 ptr_glProgramUniform2fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2fvEXT #-}
ptr_glProgramUniform2fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform2fvEXT = unsafePerformIO $ getCommand "glProgramUniform2fvEXT"

-- glProgramUniform2i ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2i
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> m ()
glProgramUniform2i v1 v2 v3 v4 = liftIO $ dyn682 ptr_glProgramUniform2i v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2i #-}
ptr_glProgramUniform2i :: FunPtr (GLuint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform2i = unsafePerformIO $ getCommand "glProgramUniform2i"

-- glProgramUniform2i64ARB -----------------------------------------------------

glProgramUniform2i64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> GLint64 -- ^ @y@.
  -> m ()
glProgramUniform2i64ARB v1 v2 v3 v4 = liftIO $ dyn683 ptr_glProgramUniform2i64ARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2i64ARB #-}
ptr_glProgramUniform2i64ARB :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> IO ())
ptr_glProgramUniform2i64ARB = unsafePerformIO $ getCommand "glProgramUniform2i64ARB"

-- glProgramUniform2i64NV ------------------------------------------------------

glProgramUniform2i64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> m ()
glProgramUniform2i64NV v1 v2 v3 v4 = liftIO $ dyn684 ptr_glProgramUniform2i64NV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2i64NV #-}
ptr_glProgramUniform2i64NV :: FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glProgramUniform2i64NV = unsafePerformIO $ getCommand "glProgramUniform2i64NV"

-- glProgramUniform2i64vARB ----------------------------------------------------

glProgramUniform2i64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*2@ elements of type @GLint64@.
  -> m ()
glProgramUniform2i64vARB v1 v2 v3 v4 = liftIO $ dyn480 ptr_glProgramUniform2i64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2i64vARB #-}
ptr_glProgramUniform2i64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glProgramUniform2i64vARB = unsafePerformIO $ getCommand "glProgramUniform2i64vARB"

-- glProgramUniform2i64vNV -----------------------------------------------------

glProgramUniform2i64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*2@ elements of type @GLint64EXT@.
  -> m ()
glProgramUniform2i64vNV v1 v2 v3 v4 = liftIO $ dyn675 ptr_glProgramUniform2i64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2i64vNV #-}
ptr_glProgramUniform2i64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glProgramUniform2i64vNV = unsafePerformIO $ getCommand "glProgramUniform2i64vNV"

-- glProgramUniform2iEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform2i'.
glProgramUniform2iEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> m ()
glProgramUniform2iEXT v1 v2 v3 v4 = liftIO $ dyn682 ptr_glProgramUniform2iEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2iEXT #-}
ptr_glProgramUniform2iEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform2iEXT = unsafePerformIO $ getCommand "glProgramUniform2iEXT"

-- glProgramUniform2iv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2iv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*2@ elements of type @GLint@.
  -> m ()
glProgramUniform2iv v1 v2 v3 v4 = liftIO $ dyn481 ptr_glProgramUniform2iv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2iv #-}
ptr_glProgramUniform2iv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform2iv = unsafePerformIO $ getCommand "glProgramUniform2iv"

-- glProgramUniform2ivEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform2iv'.
glProgramUniform2ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*2@ elements of type @GLint@.
  -> m ()
glProgramUniform2ivEXT v1 v2 v3 v4 = liftIO $ dyn481 ptr_glProgramUniform2ivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ivEXT #-}
ptr_glProgramUniform2ivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform2ivEXT = unsafePerformIO $ getCommand "glProgramUniform2ivEXT"

-- glProgramUniform2ui ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2ui
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> m ()
glProgramUniform2ui v1 v2 v3 v4 = liftIO $ dyn685 ptr_glProgramUniform2ui v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ui #-}
ptr_glProgramUniform2ui :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform2ui = unsafePerformIO $ getCommand "glProgramUniform2ui"

-- glProgramUniform2ui64ARB ----------------------------------------------------

glProgramUniform2ui64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> m ()
glProgramUniform2ui64ARB v1 v2 v3 v4 = liftIO $ dyn686 ptr_glProgramUniform2ui64ARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ui64ARB #-}
ptr_glProgramUniform2ui64ARB :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> IO ())
ptr_glProgramUniform2ui64ARB = unsafePerformIO $ getCommand "glProgramUniform2ui64ARB"

-- glProgramUniform2ui64NV -----------------------------------------------------

glProgramUniform2ui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> m ()
glProgramUniform2ui64NV v1 v2 v3 v4 = liftIO $ dyn687 ptr_glProgramUniform2ui64NV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ui64NV #-}
ptr_glProgramUniform2ui64NV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glProgramUniform2ui64NV = unsafePerformIO $ getCommand "glProgramUniform2ui64NV"

-- glProgramUniform2ui64vARB ---------------------------------------------------

glProgramUniform2ui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*2@ elements of type @GLuint64@.
  -> m ()
glProgramUniform2ui64vARB v1 v2 v3 v4 = liftIO $ dyn482 ptr_glProgramUniform2ui64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ui64vARB #-}
ptr_glProgramUniform2ui64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniform2ui64vARB = unsafePerformIO $ getCommand "glProgramUniform2ui64vARB"

-- glProgramUniform2ui64vNV ----------------------------------------------------

glProgramUniform2ui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*2@ elements of type @GLuint64EXT@.
  -> m ()
glProgramUniform2ui64vNV v1 v2 v3 v4 = liftIO $ dyn679 ptr_glProgramUniform2ui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ui64vNV #-}
ptr_glProgramUniform2ui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glProgramUniform2ui64vNV = unsafePerformIO $ getCommand "glProgramUniform2ui64vNV"

-- glProgramUniform2uiEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform2ui'.
glProgramUniform2uiEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> m ()
glProgramUniform2uiEXT v1 v2 v3 v4 = liftIO $ dyn685 ptr_glProgramUniform2uiEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2uiEXT #-}
ptr_glProgramUniform2uiEXT :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform2uiEXT = unsafePerformIO $ getCommand "glProgramUniform2uiEXT"

-- glProgramUniform2uiv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2uiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*2@ elements of type @GLuint@.
  -> m ()
glProgramUniform2uiv v1 v2 v3 v4 = liftIO $ dyn483 ptr_glProgramUniform2uiv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2uiv #-}
ptr_glProgramUniform2uiv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform2uiv = unsafePerformIO $ getCommand "glProgramUniform2uiv"

-- glProgramUniform2uivEXT -----------------------------------------------------

-- | This command is an alias for 'glProgramUniform2uiv'.
glProgramUniform2uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*2@ elements of type @GLuint@.
  -> m ()
glProgramUniform2uivEXT v1 v2 v3 v4 = liftIO $ dyn483 ptr_glProgramUniform2uivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2uivEXT #-}
ptr_glProgramUniform2uivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform2uivEXT = unsafePerformIO $ getCommand "glProgramUniform2uivEXT"

-- glProgramUniform3d ----------------------------------------------------------

glProgramUniform3d
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @v0@.
  -> GLdouble -- ^ @v1@.
  -> GLdouble -- ^ @v2@.
  -> m ()
glProgramUniform3d v1 v2 v3 v4 v5 = liftIO $ dyn688 ptr_glProgramUniform3d v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3d #-}
ptr_glProgramUniform3d :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform3d = unsafePerformIO $ getCommand "glProgramUniform3d"

-- glProgramUniform3dEXT -------------------------------------------------------

glProgramUniform3dEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glProgramUniform3dEXT v1 v2 v3 v4 v5 = liftIO $ dyn688 ptr_glProgramUniform3dEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3dEXT #-}
ptr_glProgramUniform3dEXT :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform3dEXT = unsafePerformIO $ getCommand "glProgramUniform3dEXT"

-- glProgramUniform3dv ---------------------------------------------------------

glProgramUniform3dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*3@ elements of type @GLdouble@.
  -> m ()
glProgramUniform3dv v1 v2 v3 v4 = liftIO $ dyn478 ptr_glProgramUniform3dv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3dv #-}
ptr_glProgramUniform3dv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform3dv = unsafePerformIO $ getCommand "glProgramUniform3dv"

-- glProgramUniform3dvEXT ------------------------------------------------------

glProgramUniform3dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*3@ elements of type @GLdouble@.
  -> m ()
glProgramUniform3dvEXT v1 v2 v3 v4 = liftIO $ dyn478 ptr_glProgramUniform3dvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3dvEXT #-}
ptr_glProgramUniform3dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform3dvEXT = unsafePerformIO $ getCommand "glProgramUniform3dvEXT"

-- glProgramUniform3f ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3f
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> m ()
glProgramUniform3f v1 v2 v3 v4 v5 = liftIO $ dyn689 ptr_glProgramUniform3f v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3f #-}
ptr_glProgramUniform3f :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform3f = unsafePerformIO $ getCommand "glProgramUniform3f"

-- glProgramUniform3fEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform3f'.
glProgramUniform3fEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> m ()
glProgramUniform3fEXT v1 v2 v3 v4 v5 = liftIO $ dyn689 ptr_glProgramUniform3fEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3fEXT #-}
ptr_glProgramUniform3fEXT :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform3fEXT = unsafePerformIO $ getCommand "glProgramUniform3fEXT"

-- glProgramUniform3fv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*3@ elements of type @GLfloat@.
  -> m ()
glProgramUniform3fv v1 v2 v3 v4 = liftIO $ dyn479 ptr_glProgramUniform3fv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3fv #-}
ptr_glProgramUniform3fv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform3fv = unsafePerformIO $ getCommand "glProgramUniform3fv"

-- glProgramUniform3fvEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform3fv'.
glProgramUniform3fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*3@ elements of type @GLfloat@.
  -> m ()
glProgramUniform3fvEXT v1 v2 v3 v4 = liftIO $ dyn479 ptr_glProgramUniform3fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3fvEXT #-}
ptr_glProgramUniform3fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform3fvEXT = unsafePerformIO $ getCommand "glProgramUniform3fvEXT"

-- glProgramUniform3i ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3i
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> m ()
glProgramUniform3i v1 v2 v3 v4 v5 = liftIO $ dyn690 ptr_glProgramUniform3i v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3i #-}
ptr_glProgramUniform3i :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform3i = unsafePerformIO $ getCommand "glProgramUniform3i"

-- glProgramUniform3i64ARB -----------------------------------------------------

glProgramUniform3i64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> GLint64 -- ^ @y@.
  -> GLint64 -- ^ @z@.
  -> m ()
glProgramUniform3i64ARB v1 v2 v3 v4 v5 = liftIO $ dyn691 ptr_glProgramUniform3i64ARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3i64ARB #-}
ptr_glProgramUniform3i64ARB :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> IO ())
ptr_glProgramUniform3i64ARB = unsafePerformIO $ getCommand "glProgramUniform3i64ARB"

-- glProgramUniform3i64NV ------------------------------------------------------

glProgramUniform3i64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> m ()
glProgramUniform3i64NV v1 v2 v3 v4 v5 = liftIO $ dyn692 ptr_glProgramUniform3i64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3i64NV #-}
ptr_glProgramUniform3i64NV :: FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glProgramUniform3i64NV = unsafePerformIO $ getCommand "glProgramUniform3i64NV"

-- glProgramUniform3i64vARB ----------------------------------------------------

glProgramUniform3i64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*3@ elements of type @GLint64@.
  -> m ()
glProgramUniform3i64vARB v1 v2 v3 v4 = liftIO $ dyn480 ptr_glProgramUniform3i64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3i64vARB #-}
ptr_glProgramUniform3i64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glProgramUniform3i64vARB = unsafePerformIO $ getCommand "glProgramUniform3i64vARB"

-- glProgramUniform3i64vNV -----------------------------------------------------

glProgramUniform3i64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*3@ elements of type @GLint64EXT@.
  -> m ()
glProgramUniform3i64vNV v1 v2 v3 v4 = liftIO $ dyn675 ptr_glProgramUniform3i64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3i64vNV #-}
ptr_glProgramUniform3i64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glProgramUniform3i64vNV = unsafePerformIO $ getCommand "glProgramUniform3i64vNV"

-- glProgramUniform3iEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform3i'.
glProgramUniform3iEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> m ()
glProgramUniform3iEXT v1 v2 v3 v4 v5 = liftIO $ dyn690 ptr_glProgramUniform3iEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3iEXT #-}
ptr_glProgramUniform3iEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform3iEXT = unsafePerformIO $ getCommand "glProgramUniform3iEXT"

-- glProgramUniform3iv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3iv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*3@ elements of type @GLint@.
  -> m ()
glProgramUniform3iv v1 v2 v3 v4 = liftIO $ dyn481 ptr_glProgramUniform3iv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3iv #-}
ptr_glProgramUniform3iv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform3iv = unsafePerformIO $ getCommand "glProgramUniform3iv"

-- glProgramUniform3ivEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform3iv'.
glProgramUniform3ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*3@ elements of type @GLint@.
  -> m ()
glProgramUniform3ivEXT v1 v2 v3 v4 = liftIO $ dyn481 ptr_glProgramUniform3ivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3ivEXT #-}
ptr_glProgramUniform3ivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform3ivEXT = unsafePerformIO $ getCommand "glProgramUniform3ivEXT"

-- glProgramUniform3ui ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3ui
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> m ()
glProgramUniform3ui v1 v2 v3 v4 v5 = liftIO $ dyn693 ptr_glProgramUniform3ui v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3ui #-}
ptr_glProgramUniform3ui :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform3ui = unsafePerformIO $ getCommand "glProgramUniform3ui"

-- glProgramUniform3ui64ARB ----------------------------------------------------

glProgramUniform3ui64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> GLuint64 -- ^ @z@.
  -> m ()
glProgramUniform3ui64ARB v1 v2 v3 v4 v5 = liftIO $ dyn694 ptr_glProgramUniform3ui64ARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3ui64ARB #-}
ptr_glProgramUniform3ui64ARB :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
ptr_glProgramUniform3ui64ARB = unsafePerformIO $ getCommand "glProgramUniform3ui64ARB"

-- glProgramUniform3ui64NV -----------------------------------------------------

glProgramUniform3ui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> m ()
glProgramUniform3ui64NV v1 v2 v3 v4 v5 = liftIO $ dyn695 ptr_glProgramUniform3ui64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3ui64NV #-}
ptr_glProgramUniform3ui64NV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glProgramUniform3ui64NV = unsafePerformIO $ getCommand "glProgramUniform3ui64NV"

-- glProgramUniform3ui64vARB ---------------------------------------------------

glProgramUniform3ui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*3@ elements of type @GLuint64@.
  -> m ()
glProgramUniform3ui64vARB v1 v2 v3 v4 = liftIO $ dyn482 ptr_glProgramUniform3ui64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3ui64vARB #-}
ptr_glProgramUniform3ui64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniform3ui64vARB = unsafePerformIO $ getCommand "glProgramUniform3ui64vARB"

-- glProgramUniform3ui64vNV ----------------------------------------------------

glProgramUniform3ui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*3@ elements of type @GLuint64EXT@.
  -> m ()
glProgramUniform3ui64vNV v1 v2 v3 v4 = liftIO $ dyn679 ptr_glProgramUniform3ui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3ui64vNV #-}
ptr_glProgramUniform3ui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glProgramUniform3ui64vNV = unsafePerformIO $ getCommand "glProgramUniform3ui64vNV"

-- glProgramUniform3uiEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform3ui'.
glProgramUniform3uiEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> m ()
glProgramUniform3uiEXT v1 v2 v3 v4 v5 = liftIO $ dyn693 ptr_glProgramUniform3uiEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3uiEXT #-}
ptr_glProgramUniform3uiEXT :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform3uiEXT = unsafePerformIO $ getCommand "glProgramUniform3uiEXT"

-- glProgramUniform3uiv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3uiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*3@ elements of type @GLuint@.
  -> m ()
glProgramUniform3uiv v1 v2 v3 v4 = liftIO $ dyn483 ptr_glProgramUniform3uiv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3uiv #-}
ptr_glProgramUniform3uiv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform3uiv = unsafePerformIO $ getCommand "glProgramUniform3uiv"

-- glProgramUniform3uivEXT -----------------------------------------------------

-- | This command is an alias for 'glProgramUniform3uiv'.
glProgramUniform3uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*3@ elements of type @GLuint@.
  -> m ()
glProgramUniform3uivEXT v1 v2 v3 v4 = liftIO $ dyn483 ptr_glProgramUniform3uivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3uivEXT #-}
ptr_glProgramUniform3uivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform3uivEXT = unsafePerformIO $ getCommand "glProgramUniform3uivEXT"

-- glProgramUniform4d ----------------------------------------------------------

glProgramUniform4d
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @v0@.
  -> GLdouble -- ^ @v1@.
  -> GLdouble -- ^ @v2@.
  -> GLdouble -- ^ @v3@.
  -> m ()
glProgramUniform4d v1 v2 v3 v4 v5 v6 = liftIO $ dyn696 ptr_glProgramUniform4d v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4d #-}
ptr_glProgramUniform4d :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform4d = unsafePerformIO $ getCommand "glProgramUniform4d"

-- glProgramUniform4dEXT -------------------------------------------------------

glProgramUniform4dEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glProgramUniform4dEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn696 ptr_glProgramUniform4dEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4dEXT #-}
ptr_glProgramUniform4dEXT :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform4dEXT = unsafePerformIO $ getCommand "glProgramUniform4dEXT"

-- glProgramUniform4dv ---------------------------------------------------------

glProgramUniform4dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glProgramUniform4dv v1 v2 v3 v4 = liftIO $ dyn478 ptr_glProgramUniform4dv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4dv #-}
ptr_glProgramUniform4dv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform4dv = unsafePerformIO $ getCommand "glProgramUniform4dv"

-- glProgramUniform4dvEXT ------------------------------------------------------

glProgramUniform4dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glProgramUniform4dvEXT v1 v2 v3 v4 = liftIO $ dyn478 ptr_glProgramUniform4dvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4dvEXT #-}
ptr_glProgramUniform4dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform4dvEXT = unsafePerformIO $ getCommand "glProgramUniform4dvEXT"

-- glProgramUniform4f ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4f
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> GLfloat -- ^ @v3@.
  -> m ()
glProgramUniform4f v1 v2 v3 v4 v5 v6 = liftIO $ dyn697 ptr_glProgramUniform4f v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4f #-}
ptr_glProgramUniform4f :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform4f = unsafePerformIO $ getCommand "glProgramUniform4f"

-- glProgramUniform4fEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform4f'.
glProgramUniform4fEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> GLfloat -- ^ @v3@.
  -> m ()
glProgramUniform4fEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn697 ptr_glProgramUniform4fEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4fEXT #-}
ptr_glProgramUniform4fEXT :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform4fEXT = unsafePerformIO $ getCommand "glProgramUniform4fEXT"

-- glProgramUniform4fv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glProgramUniform4fv v1 v2 v3 v4 = liftIO $ dyn479 ptr_glProgramUniform4fv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4fv #-}
ptr_glProgramUniform4fv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform4fv = unsafePerformIO $ getCommand "glProgramUniform4fv"

-- glProgramUniform4fvEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform4fv'.
glProgramUniform4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glProgramUniform4fvEXT v1 v2 v3 v4 = liftIO $ dyn479 ptr_glProgramUniform4fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4fvEXT #-}
ptr_glProgramUniform4fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform4fvEXT = unsafePerformIO $ getCommand "glProgramUniform4fvEXT"

-- glProgramUniform4i ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4i
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> GLint -- ^ @v3@.
  -> m ()
glProgramUniform4i v1 v2 v3 v4 v5 v6 = liftIO $ dyn698 ptr_glProgramUniform4i v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4i #-}
ptr_glProgramUniform4i :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform4i = unsafePerformIO $ getCommand "glProgramUniform4i"

-- glProgramUniform4i64ARB -----------------------------------------------------

glProgramUniform4i64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> GLint64 -- ^ @y@.
  -> GLint64 -- ^ @z@.
  -> GLint64 -- ^ @w@.
  -> m ()
glProgramUniform4i64ARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn699 ptr_glProgramUniform4i64ARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4i64ARB #-}
ptr_glProgramUniform4i64ARB :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ())
ptr_glProgramUniform4i64ARB = unsafePerformIO $ getCommand "glProgramUniform4i64ARB"

-- glProgramUniform4i64NV ------------------------------------------------------

glProgramUniform4i64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> GLint64EXT -- ^ @w@.
  -> m ()
glProgramUniform4i64NV v1 v2 v3 v4 v5 v6 = liftIO $ dyn700 ptr_glProgramUniform4i64NV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4i64NV #-}
ptr_glProgramUniform4i64NV :: FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glProgramUniform4i64NV = unsafePerformIO $ getCommand "glProgramUniform4i64NV"

-- glProgramUniform4i64vARB ----------------------------------------------------

glProgramUniform4i64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*4@ elements of type @GLint64@.
  -> m ()
glProgramUniform4i64vARB v1 v2 v3 v4 = liftIO $ dyn480 ptr_glProgramUniform4i64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4i64vARB #-}
ptr_glProgramUniform4i64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glProgramUniform4i64vARB = unsafePerformIO $ getCommand "glProgramUniform4i64vARB"

-- glProgramUniform4i64vNV -----------------------------------------------------

glProgramUniform4i64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*4@ elements of type @GLint64EXT@.
  -> m ()
glProgramUniform4i64vNV v1 v2 v3 v4 = liftIO $ dyn675 ptr_glProgramUniform4i64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4i64vNV #-}
ptr_glProgramUniform4i64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glProgramUniform4i64vNV = unsafePerformIO $ getCommand "glProgramUniform4i64vNV"

-- glProgramUniform4iEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform4i'.
glProgramUniform4iEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> GLint -- ^ @v3@.
  -> m ()
glProgramUniform4iEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn698 ptr_glProgramUniform4iEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4iEXT #-}
ptr_glProgramUniform4iEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform4iEXT = unsafePerformIO $ getCommand "glProgramUniform4iEXT"

