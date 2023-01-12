{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F31
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

module Graphics.GL.Functions.F31 (
  glVertexAttrib4svARB,
  glVertexAttrib4svNV,
  glVertexAttrib4ubNV,
  glVertexAttrib4ubv,
  glVertexAttrib4ubvARB,
  glVertexAttrib4ubvNV,
  glVertexAttrib4uiv,
  glVertexAttrib4uivARB,
  glVertexAttrib4usv,
  glVertexAttrib4usvARB,
  glVertexAttribArrayObjectATI,
  glVertexAttribBinding,
  glVertexAttribDivisor,
  glVertexAttribDivisorANGLE,
  glVertexAttribDivisorARB,
  glVertexAttribDivisorEXT,
  glVertexAttribDivisorNV,
  glVertexAttribFormat,
  glVertexAttribFormatNV,
  glVertexAttribI1i,
  glVertexAttribI1iEXT,
  glVertexAttribI1iv,
  glVertexAttribI1ivEXT,
  glVertexAttribI1ui,
  glVertexAttribI1uiEXT,
  glVertexAttribI1uiv,
  glVertexAttribI1uivEXT,
  glVertexAttribI2i,
  glVertexAttribI2iEXT,
  glVertexAttribI2iv,
  glVertexAttribI2ivEXT,
  glVertexAttribI2ui,
  glVertexAttribI2uiEXT,
  glVertexAttribI2uiv,
  glVertexAttribI2uivEXT,
  glVertexAttribI3i,
  glVertexAttribI3iEXT,
  glVertexAttribI3iv,
  glVertexAttribI3ivEXT,
  glVertexAttribI3ui,
  glVertexAttribI3uiEXT,
  glVertexAttribI3uiv,
  glVertexAttribI3uivEXT,
  glVertexAttribI4bv,
  glVertexAttribI4bvEXT,
  glVertexAttribI4i,
  glVertexAttribI4iEXT,
  glVertexAttribI4iv,
  glVertexAttribI4ivEXT,
  glVertexAttribI4sv,
  glVertexAttribI4svEXT,
  glVertexAttribI4ubv,
  glVertexAttribI4ubvEXT,
  glVertexAttribI4ui,
  glVertexAttribI4uiEXT,
  glVertexAttribI4uiv,
  glVertexAttribI4uivEXT,
  glVertexAttribI4usv,
  glVertexAttribI4usvEXT,
  glVertexAttribIFormat,
  glVertexAttribIFormatNV,
  glVertexAttribIPointer,
  glVertexAttribIPointerEXT,
  glVertexAttribL1d,
  glVertexAttribL1dEXT,
  glVertexAttribL1dv,
  glVertexAttribL1dvEXT,
  glVertexAttribL1i64NV,
  glVertexAttribL1i64vNV,
  glVertexAttribL1ui64ARB,
  glVertexAttribL1ui64NV,
  glVertexAttribL1ui64vARB,
  glVertexAttribL1ui64vNV,
  glVertexAttribL2d,
  glVertexAttribL2dEXT,
  glVertexAttribL2dv,
  glVertexAttribL2dvEXT,
  glVertexAttribL2i64NV,
  glVertexAttribL2i64vNV,
  glVertexAttribL2ui64NV,
  glVertexAttribL2ui64vNV,
  glVertexAttribL3d,
  glVertexAttribL3dEXT,
  glVertexAttribL3dv,
  glVertexAttribL3dvEXT,
  glVertexAttribL3i64NV,
  glVertexAttribL3i64vNV,
  glVertexAttribL3ui64NV,
  glVertexAttribL3ui64vNV,
  glVertexAttribL4d,
  glVertexAttribL4dEXT,
  glVertexAttribL4dv,
  glVertexAttribL4dvEXT,
  glVertexAttribL4i64NV,
  glVertexAttribL4i64vNV,
  glVertexAttribL4ui64NV,
  glVertexAttribL4ui64vNV,
  glVertexAttribLFormat,
  glVertexAttribLFormatNV,
  glVertexAttribLPointer
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glVertexAttrib4svARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4sv'.
glVertexAttrib4svARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttrib4svARB v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib4svARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4svARB #-}
ptr_glVertexAttrib4svARB :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib4svARB = unsafePerformIO $ getCommand "glVertexAttrib4svARB"

-- glVertexAttrib4svNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4sv'.
glVertexAttrib4svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttrib4svNV v1 v2 = liftIO $ dyn883 ptr_glVertexAttrib4svNV v1 v2

{-# NOINLINE ptr_glVertexAttrib4svNV #-}
ptr_glVertexAttrib4svNV :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib4svNV = unsafePerformIO $ getCommand "glVertexAttrib4svNV"

-- glVertexAttrib4ubNV ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4ubvNV'. This command is an alias for 'glVertexAttrib4Nub'.
glVertexAttrib4ubNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLubyte -- ^ @x@ of type @ColorUB@.
  -> GLubyte -- ^ @y@ of type @ColorUB@.
  -> GLubyte -- ^ @z@ of type @ColorUB@.
  -> GLubyte -- ^ @w@ of type @ColorUB@.
  -> m ()
glVertexAttrib4ubNV v1 v2 v3 v4 v5 = liftIO $ dyn905 ptr_glVertexAttrib4ubNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4ubNV #-}
ptr_glVertexAttrib4ubNV :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glVertexAttrib4ubNV = unsafePerformIO $ getCommand "glVertexAttrib4ubNV"

-- glVertexAttrib4ubv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4ubv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttrib4ubv v1 v2 = liftIO $ dyn393 ptr_glVertexAttrib4ubv v1 v2

{-# NOINLINE ptr_glVertexAttrib4ubv #-}
ptr_glVertexAttrib4ubv :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttrib4ubv = unsafePerformIO $ getCommand "glVertexAttrib4ubv"

-- glVertexAttrib4ubvARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4ubv'.
glVertexAttrib4ubvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttrib4ubvARB v1 v2 = liftIO $ dyn393 ptr_glVertexAttrib4ubvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4ubvARB #-}
ptr_glVertexAttrib4ubvARB :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttrib4ubvARB = unsafePerformIO $ getCommand "glVertexAttrib4ubvARB"

-- glVertexAttrib4ubvNV --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nubv'.
glVertexAttrib4ubvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @ColorUB@.
  -> m ()
glVertexAttrib4ubvNV v1 v2 = liftIO $ dyn393 ptr_glVertexAttrib4ubvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib4ubvNV #-}
ptr_glVertexAttrib4ubvNV :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttrib4ubvNV = unsafePerformIO $ getCommand "glVertexAttrib4ubvNV"

-- glVertexAttrib4uiv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttrib4uiv v1 v2 = liftIO $ dyn201 ptr_glVertexAttrib4uiv v1 v2

{-# NOINLINE ptr_glVertexAttrib4uiv #-}
ptr_glVertexAttrib4uiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttrib4uiv = unsafePerformIO $ getCommand "glVertexAttrib4uiv"

-- glVertexAttrib4uivARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4uiv'.
glVertexAttrib4uivARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttrib4uivARB v1 v2 = liftIO $ dyn201 ptr_glVertexAttrib4uivARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4uivARB #-}
ptr_glVertexAttrib4uivARB :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttrib4uivARB = unsafePerformIO $ getCommand "glVertexAttrib4uivARB"

-- glVertexAttrib4usv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4usv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttrib4usv v1 v2 = liftIO $ dyn884 ptr_glVertexAttrib4usv v1 v2

{-# NOINLINE ptr_glVertexAttrib4usv #-}
ptr_glVertexAttrib4usv :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttrib4usv = unsafePerformIO $ getCommand "glVertexAttrib4usv"

-- glVertexAttrib4usvARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4usv'.
glVertexAttrib4usvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttrib4usvARB v1 v2 = liftIO $ dyn884 ptr_glVertexAttrib4usvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4usvARB #-}
ptr_glVertexAttrib4usvARB :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttrib4usvARB = unsafePerformIO $ getCommand "glVertexAttrib4usvARB"

-- glVertexAttribArrayObjectATI ------------------------------------------------

glVertexAttribArrayObjectATI
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLsizei -- ^ @stride@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @offset@.
  -> m ()
glVertexAttribArrayObjectATI v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn910 ptr_glVertexAttribArrayObjectATI v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glVertexAttribArrayObjectATI #-}
ptr_glVertexAttribArrayObjectATI :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribArrayObjectATI = unsafePerformIO $ getCommand "glVertexAttribArrayObjectATI"

-- glVertexAttribBinding -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribBinding.xhtml OpenGL 4.x>.
glVertexAttribBinding
  :: MonadIO m
  => GLuint -- ^ @attribindex@.
  -> GLuint -- ^ @bindingindex@.
  -> m ()
glVertexAttribBinding v1 v2 = liftIO $ dyn4 ptr_glVertexAttribBinding v1 v2

{-# NOINLINE ptr_glVertexAttribBinding #-}
ptr_glVertexAttribBinding :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribBinding = unsafePerformIO $ getCommand "glVertexAttribBinding"

-- glVertexAttribDivisor -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttribDivisor.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribDivisor.xhtml OpenGL 4.x>.
glVertexAttribDivisor
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexAttribDivisor v1 v2 = liftIO $ dyn4 ptr_glVertexAttribDivisor v1 v2

{-# NOINLINE ptr_glVertexAttribDivisor #-}
ptr_glVertexAttribDivisor :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribDivisor = unsafePerformIO $ getCommand "glVertexAttribDivisor"

-- glVertexAttribDivisorANGLE --------------------------------------------------

-- | This command is an alias for 'glVertexAttribDivisor'.
glVertexAttribDivisorANGLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexAttribDivisorANGLE v1 v2 = liftIO $ dyn4 ptr_glVertexAttribDivisorANGLE v1 v2

{-# NOINLINE ptr_glVertexAttribDivisorANGLE #-}
ptr_glVertexAttribDivisorANGLE :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribDivisorANGLE = unsafePerformIO $ getCommand "glVertexAttribDivisorANGLE"

-- glVertexAttribDivisorARB ----------------------------------------------------

-- | This command is an alias for 'glVertexAttribDivisor'.
glVertexAttribDivisorARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexAttribDivisorARB v1 v2 = liftIO $ dyn4 ptr_glVertexAttribDivisorARB v1 v2

{-# NOINLINE ptr_glVertexAttribDivisorARB #-}
ptr_glVertexAttribDivisorARB :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribDivisorARB = unsafePerformIO $ getCommand "glVertexAttribDivisorARB"

-- glVertexAttribDivisorEXT ----------------------------------------------------

-- | This command is an alias for 'glVertexAttribDivisor'.
glVertexAttribDivisorEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexAttribDivisorEXT v1 v2 = liftIO $ dyn4 ptr_glVertexAttribDivisorEXT v1 v2

{-# NOINLINE ptr_glVertexAttribDivisorEXT #-}
ptr_glVertexAttribDivisorEXT :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribDivisorEXT = unsafePerformIO $ getCommand "glVertexAttribDivisorEXT"

-- glVertexAttribDivisorNV -----------------------------------------------------

-- | This command is an alias for 'glVertexAttribDivisor'.
glVertexAttribDivisorNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexAttribDivisorNV v1 v2 = liftIO $ dyn4 ptr_glVertexAttribDivisorNV v1 v2

{-# NOINLINE ptr_glVertexAttribDivisorNV #-}
ptr_glVertexAttribDivisorNV :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribDivisorNV = unsafePerformIO $ getCommand "glVertexAttribDivisorNV"

-- glVertexAttribFormat --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexAttribFormat
  :: MonadIO m
  => GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribType](Graphics-GL-Groups.html#VertexAttribType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexAttribFormat v1 v2 v3 v4 v5 = liftIO $ dyn911 ptr_glVertexAttribFormat v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribFormat #-}
ptr_glVertexAttribFormat :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexAttribFormat = unsafePerformIO $ getCommand "glVertexAttribFormat"

-- glVertexAttribFormatNV ------------------------------------------------------

glVertexAttribFormatNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribType](Graphics-GL-Groups.html#VertexAttribType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexAttribFormatNV v1 v2 v3 v4 v5 = liftIO $ dyn912 ptr_glVertexAttribFormatNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribFormatNV #-}
ptr_glVertexAttribFormatNV :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> IO ())
ptr_glVertexAttribFormatNV = unsafePerformIO $ getCommand "glVertexAttribFormatNV"

-- glVertexAttribI1i -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI1iv'.
glVertexAttribI1i
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> m ()
glVertexAttribI1i v1 v2 = liftIO $ dyn499 ptr_glVertexAttribI1i v1 v2

{-# NOINLINE ptr_glVertexAttribI1i #-}
ptr_glVertexAttribI1i :: FunPtr (GLuint -> GLint -> IO ())
ptr_glVertexAttribI1i = unsafePerformIO $ getCommand "glVertexAttribI1i"

-- glVertexAttribI1iEXT --------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI1ivEXT'. This command is an alias for 'glVertexAttribI1i'.
glVertexAttribI1iEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> m ()
glVertexAttribI1iEXT v1 v2 = liftIO $ dyn499 ptr_glVertexAttribI1iEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI1iEXT #-}
ptr_glVertexAttribI1iEXT :: FunPtr (GLuint -> GLint -> IO ())
ptr_glVertexAttribI1iEXT = unsafePerformIO $ getCommand "glVertexAttribI1iEXT"

-- glVertexAttribI1iv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI1iv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @1@ element of type @GLint@.
  -> m ()
glVertexAttribI1iv v1 v2 = liftIO $ dyn741 ptr_glVertexAttribI1iv v1 v2

{-# NOINLINE ptr_glVertexAttribI1iv #-}
ptr_glVertexAttribI1iv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI1iv = unsafePerformIO $ getCommand "glVertexAttribI1iv"

-- glVertexAttribI1ivEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI1iv'.
glVertexAttribI1ivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @1@ element of type @GLint@.
  -> m ()
glVertexAttribI1ivEXT v1 v2 = liftIO $ dyn741 ptr_glVertexAttribI1ivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI1ivEXT #-}
ptr_glVertexAttribI1ivEXT :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI1ivEXT = unsafePerformIO $ getCommand "glVertexAttribI1ivEXT"

-- glVertexAttribI1ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI1uiv'.
glVertexAttribI1ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> m ()
glVertexAttribI1ui v1 v2 = liftIO $ dyn4 ptr_glVertexAttribI1ui v1 v2

{-# NOINLINE ptr_glVertexAttribI1ui #-}
ptr_glVertexAttribI1ui :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribI1ui = unsafePerformIO $ getCommand "glVertexAttribI1ui"

-- glVertexAttribI1uiEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI1uivEXT'. This command is an alias for 'glVertexAttribI1ui'.
glVertexAttribI1uiEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> m ()
glVertexAttribI1uiEXT v1 v2 = liftIO $ dyn4 ptr_glVertexAttribI1uiEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI1uiEXT #-}
ptr_glVertexAttribI1uiEXT :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribI1uiEXT = unsafePerformIO $ getCommand "glVertexAttribI1uiEXT"

-- glVertexAttribI1uiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI1uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribI1uiv v1 v2 = liftIO $ dyn201 ptr_glVertexAttribI1uiv v1 v2

{-# NOINLINE ptr_glVertexAttribI1uiv #-}
ptr_glVertexAttribI1uiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI1uiv = unsafePerformIO $ getCommand "glVertexAttribI1uiv"

-- glVertexAttribI1uivEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI1uiv'.
glVertexAttribI1uivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribI1uivEXT v1 v2 = liftIO $ dyn201 ptr_glVertexAttribI1uivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI1uivEXT #-}
ptr_glVertexAttribI1uivEXT :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI1uivEXT = unsafePerformIO $ getCommand "glVertexAttribI1uivEXT"

-- glVertexAttribI2i -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI2iv'.
glVertexAttribI2i
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> m ()
glVertexAttribI2i v1 v2 v3 = liftIO $ dyn672 ptr_glVertexAttribI2i v1 v2 v3

{-# NOINLINE ptr_glVertexAttribI2i #-}
ptr_glVertexAttribI2i :: FunPtr (GLuint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI2i = unsafePerformIO $ getCommand "glVertexAttribI2i"

-- glVertexAttribI2iEXT --------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI2ivEXT'. This command is an alias for 'glVertexAttribI2i'.
glVertexAttribI2iEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> m ()
glVertexAttribI2iEXT v1 v2 v3 = liftIO $ dyn672 ptr_glVertexAttribI2iEXT v1 v2 v3

{-# NOINLINE ptr_glVertexAttribI2iEXT #-}
ptr_glVertexAttribI2iEXT :: FunPtr (GLuint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI2iEXT = unsafePerformIO $ getCommand "glVertexAttribI2iEXT"

-- glVertexAttribI2iv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI2iv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @2@ elements of type @GLint@.
  -> m ()
glVertexAttribI2iv v1 v2 = liftIO $ dyn741 ptr_glVertexAttribI2iv v1 v2

{-# NOINLINE ptr_glVertexAttribI2iv #-}
ptr_glVertexAttribI2iv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI2iv = unsafePerformIO $ getCommand "glVertexAttribI2iv"

-- glVertexAttribI2ivEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI2iv'.
glVertexAttribI2ivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @2@ elements of type @GLint@.
  -> m ()
glVertexAttribI2ivEXT v1 v2 = liftIO $ dyn741 ptr_glVertexAttribI2ivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI2ivEXT #-}
ptr_glVertexAttribI2ivEXT :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI2ivEXT = unsafePerformIO $ getCommand "glVertexAttribI2ivEXT"

-- glVertexAttribI2ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI2uiv'.
glVertexAttribI2ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> m ()
glVertexAttribI2ui v1 v2 v3 = liftIO $ dyn109 ptr_glVertexAttribI2ui v1 v2 v3

{-# NOINLINE ptr_glVertexAttribI2ui #-}
ptr_glVertexAttribI2ui :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI2ui = unsafePerformIO $ getCommand "glVertexAttribI2ui"

-- glVertexAttribI2uiEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI2uivEXT'. This command is an alias for 'glVertexAttribI2ui'.
glVertexAttribI2uiEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> m ()
glVertexAttribI2uiEXT v1 v2 v3 = liftIO $ dyn109 ptr_glVertexAttribI2uiEXT v1 v2 v3

{-# NOINLINE ptr_glVertexAttribI2uiEXT #-}
ptr_glVertexAttribI2uiEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI2uiEXT = unsafePerformIO $ getCommand "glVertexAttribI2uiEXT"

-- glVertexAttribI2uiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI2uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @2@ elements of type @GLuint@.
  -> m ()
glVertexAttribI2uiv v1 v2 = liftIO $ dyn201 ptr_glVertexAttribI2uiv v1 v2

{-# NOINLINE ptr_glVertexAttribI2uiv #-}
ptr_glVertexAttribI2uiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI2uiv = unsafePerformIO $ getCommand "glVertexAttribI2uiv"

-- glVertexAttribI2uivEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI2uiv'.
glVertexAttribI2uivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @2@ elements of type @GLuint@.
  -> m ()
glVertexAttribI2uivEXT v1 v2 = liftIO $ dyn201 ptr_glVertexAttribI2uivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI2uivEXT #-}
ptr_glVertexAttribI2uivEXT :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI2uivEXT = unsafePerformIO $ getCommand "glVertexAttribI2uivEXT"

-- glVertexAttribI3i -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI3iv'.
glVertexAttribI3i
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> m ()
glVertexAttribI3i v1 v2 v3 v4 = liftIO $ dyn682 ptr_glVertexAttribI3i v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribI3i #-}
ptr_glVertexAttribI3i :: FunPtr (GLuint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI3i = unsafePerformIO $ getCommand "glVertexAttribI3i"

-- glVertexAttribI3iEXT --------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI3ivEXT'. This command is an alias for 'glVertexAttribI3i'.
glVertexAttribI3iEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> m ()
glVertexAttribI3iEXT v1 v2 v3 v4 = liftIO $ dyn682 ptr_glVertexAttribI3iEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribI3iEXT #-}
ptr_glVertexAttribI3iEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI3iEXT = unsafePerformIO $ getCommand "glVertexAttribI3iEXT"

-- glVertexAttribI3iv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI3iv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @3@ elements of type @GLint@.
  -> m ()
glVertexAttribI3iv v1 v2 = liftIO $ dyn741 ptr_glVertexAttribI3iv v1 v2

{-# NOINLINE ptr_glVertexAttribI3iv #-}
ptr_glVertexAttribI3iv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI3iv = unsafePerformIO $ getCommand "glVertexAttribI3iv"

-- glVertexAttribI3ivEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI3iv'.
glVertexAttribI3ivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @3@ elements of type @GLint@.
  -> m ()
glVertexAttribI3ivEXT v1 v2 = liftIO $ dyn741 ptr_glVertexAttribI3ivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI3ivEXT #-}
ptr_glVertexAttribI3ivEXT :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI3ivEXT = unsafePerformIO $ getCommand "glVertexAttribI3ivEXT"

-- glVertexAttribI3ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI3uiv'.
glVertexAttribI3ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> m ()
glVertexAttribI3ui v1 v2 v3 v4 = liftIO $ dyn83 ptr_glVertexAttribI3ui v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribI3ui #-}
ptr_glVertexAttribI3ui :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI3ui = unsafePerformIO $ getCommand "glVertexAttribI3ui"

-- glVertexAttribI3uiEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI3uivEXT'. This command is an alias for 'glVertexAttribI3ui'.
glVertexAttribI3uiEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> m ()
glVertexAttribI3uiEXT v1 v2 v3 v4 = liftIO $ dyn83 ptr_glVertexAttribI3uiEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribI3uiEXT #-}
ptr_glVertexAttribI3uiEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI3uiEXT = unsafePerformIO $ getCommand "glVertexAttribI3uiEXT"

-- glVertexAttribI3uiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI3uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @3@ elements of type @GLuint@.
  -> m ()
glVertexAttribI3uiv v1 v2 = liftIO $ dyn201 ptr_glVertexAttribI3uiv v1 v2

{-# NOINLINE ptr_glVertexAttribI3uiv #-}
ptr_glVertexAttribI3uiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI3uiv = unsafePerformIO $ getCommand "glVertexAttribI3uiv"

-- glVertexAttribI3uivEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI3uiv'.
glVertexAttribI3uivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @3@ elements of type @GLuint@.
  -> m ()
glVertexAttribI3uivEXT v1 v2 = liftIO $ dyn201 ptr_glVertexAttribI3uivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI3uivEXT #-}
ptr_glVertexAttribI3uivEXT :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI3uivEXT = unsafePerformIO $ getCommand "glVertexAttribI3uivEXT"

-- glVertexAttribI4bv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4bv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttribI4bv v1 v2 = liftIO $ dyn881 ptr_glVertexAttribI4bv v1 v2

{-# NOINLINE ptr_glVertexAttribI4bv #-}
ptr_glVertexAttribI4bv :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttribI4bv = unsafePerformIO $ getCommand "glVertexAttribI4bv"

-- glVertexAttribI4bvEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4bv'.
glVertexAttribI4bvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttribI4bvEXT v1 v2 = liftIO $ dyn881 ptr_glVertexAttribI4bvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4bvEXT #-}
ptr_glVertexAttribI4bvEXT :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttribI4bvEXT = unsafePerformIO $ getCommand "glVertexAttribI4bvEXT"

-- glVertexAttribI4i -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI4iv'.
glVertexAttribI4i
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @w@.
  -> m ()
glVertexAttribI4i v1 v2 v3 v4 v5 = liftIO $ dyn690 ptr_glVertexAttribI4i v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribI4i #-}
ptr_glVertexAttribI4i :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI4i = unsafePerformIO $ getCommand "glVertexAttribI4i"

-- glVertexAttribI4iEXT --------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI4ivEXT'. This command is an alias for 'glVertexAttribI4i'.
glVertexAttribI4iEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @w@.
  -> m ()
glVertexAttribI4iEXT v1 v2 v3 v4 v5 = liftIO $ dyn690 ptr_glVertexAttribI4iEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribI4iEXT #-}
ptr_glVertexAttribI4iEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI4iEXT = unsafePerformIO $ getCommand "glVertexAttribI4iEXT"

-- glVertexAttribI4iv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4iv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttribI4iv v1 v2 = liftIO $ dyn741 ptr_glVertexAttribI4iv v1 v2

{-# NOINLINE ptr_glVertexAttribI4iv #-}
ptr_glVertexAttribI4iv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI4iv = unsafePerformIO $ getCommand "glVertexAttribI4iv"

-- glVertexAttribI4ivEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4iv'.
glVertexAttribI4ivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttribI4ivEXT v1 v2 = liftIO $ dyn741 ptr_glVertexAttribI4ivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4ivEXT #-}
ptr_glVertexAttribI4ivEXT :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI4ivEXT = unsafePerformIO $ getCommand "glVertexAttribI4ivEXT"

-- glVertexAttribI4sv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4sv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttribI4sv v1 v2 = liftIO $ dyn883 ptr_glVertexAttribI4sv v1 v2

{-# NOINLINE ptr_glVertexAttribI4sv #-}
ptr_glVertexAttribI4sv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttribI4sv = unsafePerformIO $ getCommand "glVertexAttribI4sv"

-- glVertexAttribI4svEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4sv'.
glVertexAttribI4svEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttribI4svEXT v1 v2 = liftIO $ dyn883 ptr_glVertexAttribI4svEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4svEXT #-}
ptr_glVertexAttribI4svEXT :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttribI4svEXT = unsafePerformIO $ getCommand "glVertexAttribI4svEXT"

-- glVertexAttribI4ubv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4ubv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttribI4ubv v1 v2 = liftIO $ dyn393 ptr_glVertexAttribI4ubv v1 v2

{-# NOINLINE ptr_glVertexAttribI4ubv #-}
ptr_glVertexAttribI4ubv :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttribI4ubv = unsafePerformIO $ getCommand "glVertexAttribI4ubv"

-- glVertexAttribI4ubvEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4ubv'.
glVertexAttribI4ubvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttribI4ubvEXT v1 v2 = liftIO $ dyn393 ptr_glVertexAttribI4ubvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4ubvEXT #-}
ptr_glVertexAttribI4ubvEXT :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttribI4ubvEXT = unsafePerformIO $ getCommand "glVertexAttribI4ubvEXT"

-- glVertexAttribI4ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI4uiv'.
glVertexAttribI4ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> GLuint -- ^ @w@.
  -> m ()
glVertexAttribI4ui v1 v2 v3 v4 v5 = liftIO $ dyn913 ptr_glVertexAttribI4ui v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribI4ui #-}
ptr_glVertexAttribI4ui :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI4ui = unsafePerformIO $ getCommand "glVertexAttribI4ui"

-- glVertexAttribI4uiEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI4uivEXT'. This command is an alias for 'glVertexAttribI4ui'.
glVertexAttribI4uiEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> GLuint -- ^ @w@.
  -> m ()
glVertexAttribI4uiEXT v1 v2 v3 v4 v5 = liftIO $ dyn913 ptr_glVertexAttribI4uiEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribI4uiEXT #-}
ptr_glVertexAttribI4uiEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI4uiEXT = unsafePerformIO $ getCommand "glVertexAttribI4uiEXT"

-- glVertexAttribI4uiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttribI4uiv v1 v2 = liftIO $ dyn201 ptr_glVertexAttribI4uiv v1 v2

{-# NOINLINE ptr_glVertexAttribI4uiv #-}
ptr_glVertexAttribI4uiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI4uiv = unsafePerformIO $ getCommand "glVertexAttribI4uiv"

-- glVertexAttribI4uivEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4uiv'.
glVertexAttribI4uivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttribI4uivEXT v1 v2 = liftIO $ dyn201 ptr_glVertexAttribI4uivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4uivEXT #-}
ptr_glVertexAttribI4uivEXT :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI4uivEXT = unsafePerformIO $ getCommand "glVertexAttribI4uivEXT"

-- glVertexAttribI4usv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4usv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttribI4usv v1 v2 = liftIO $ dyn884 ptr_glVertexAttribI4usv v1 v2

{-# NOINLINE ptr_glVertexAttribI4usv #-}
ptr_glVertexAttribI4usv :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttribI4usv = unsafePerformIO $ getCommand "glVertexAttribI4usv"

-- glVertexAttribI4usvEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4usv'.
glVertexAttribI4usvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttribI4usvEXT v1 v2 = liftIO $ dyn884 ptr_glVertexAttribI4usvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4usvEXT #-}
ptr_glVertexAttribI4usvEXT :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttribI4usvEXT = unsafePerformIO $ getCommand "glVertexAttribI4usvEXT"

-- glVertexAttribIFormat -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexAttribIFormat
  :: MonadIO m
  => GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribIType](Graphics-GL-Groups.html#VertexAttribIType).
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexAttribIFormat v1 v2 v3 v4 = liftIO $ dyn914 ptr_glVertexAttribIFormat v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribIFormat #-}
ptr_glVertexAttribIFormat :: FunPtr (GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexAttribIFormat = unsafePerformIO $ getCommand "glVertexAttribIFormat"

-- glVertexAttribIFormatNV -----------------------------------------------------

glVertexAttribIFormatNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribIType](Graphics-GL-Groups.html#VertexAttribIType).
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexAttribIFormatNV v1 v2 v3 v4 = liftIO $ dyn915 ptr_glVertexAttribIFormatNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribIFormatNV #-}
ptr_glVertexAttribIFormatNV :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> IO ())
ptr_glVertexAttribIFormatNV = unsafePerformIO $ getCommand "glVertexAttribIFormatNV"

-- glVertexAttribIPointer ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttribPointer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribPointer.xhtml OpenGL 4.x>.
glVertexAttribIPointer
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glVertexAttribIPointer v1 v2 v3 v4 v5 = liftIO $ dyn916 ptr_glVertexAttribIPointer v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribIPointer #-}
ptr_glVertexAttribIPointer :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribIPointer = unsafePerformIO $ getCommand "glVertexAttribIPointer"

-- glVertexAttribIPointerEXT ---------------------------------------------------

-- | This command is an alias for 'glVertexAttribIPointer'.
glVertexAttribIPointerEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glVertexAttribIPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn916 ptr_glVertexAttribIPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribIPointerEXT #-}
ptr_glVertexAttribIPointerEXT :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribIPointerEXT = unsafePerformIO $ getCommand "glVertexAttribIPointerEXT"

-- glVertexAttribL1d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL1d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexAttribL1d v1 v2 = liftIO $ dyn895 ptr_glVertexAttribL1d v1 v2

{-# NOINLINE ptr_glVertexAttribL1d #-}
ptr_glVertexAttribL1d :: FunPtr (GLuint -> GLdouble -> IO ())
ptr_glVertexAttribL1d = unsafePerformIO $ getCommand "glVertexAttribL1d"

-- glVertexAttribL1dEXT --------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL1d'.
glVertexAttribL1dEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexAttribL1dEXT v1 v2 = liftIO $ dyn895 ptr_glVertexAttribL1dEXT v1 v2

{-# NOINLINE ptr_glVertexAttribL1dEXT #-}
ptr_glVertexAttribL1dEXT :: FunPtr (GLuint -> GLdouble -> IO ())
ptr_glVertexAttribL1dEXT = unsafePerformIO $ getCommand "glVertexAttribL1dEXT"

-- glVertexAttribL1dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL1dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexAttribL1dv v1 v2 = liftIO $ dyn882 ptr_glVertexAttribL1dv v1 v2

{-# NOINLINE ptr_glVertexAttribL1dv #-}
ptr_glVertexAttribL1dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL1dv = unsafePerformIO $ getCommand "glVertexAttribL1dv"

-- glVertexAttribL1dvEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL1dv'.
glVertexAttribL1dvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexAttribL1dvEXT v1 v2 = liftIO $ dyn882 ptr_glVertexAttribL1dvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribL1dvEXT #-}
ptr_glVertexAttribL1dvEXT :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL1dvEXT = unsafePerformIO $ getCommand "glVertexAttribL1dvEXT"

-- glVertexAttribL1i64NV -------------------------------------------------------

glVertexAttribL1i64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint64EXT -- ^ @x@.
  -> m ()
glVertexAttribL1i64NV v1 v2 = liftIO $ dyn917 ptr_glVertexAttribL1i64NV v1 v2

{-# NOINLINE ptr_glVertexAttribL1i64NV #-}
ptr_glVertexAttribL1i64NV :: FunPtr (GLuint -> GLint64EXT -> IO ())
ptr_glVertexAttribL1i64NV = unsafePerformIO $ getCommand "glVertexAttribL1i64NV"

-- glVertexAttribL1i64vNV ------------------------------------------------------

glVertexAttribL1i64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint64EXT -- ^ @v@ pointing to @1@ element of type @GLint64EXT@.
  -> m ()
glVertexAttribL1i64vNV v1 v2 = liftIO $ dyn918 ptr_glVertexAttribL1i64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL1i64vNV #-}
ptr_glVertexAttribL1i64vNV :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
ptr_glVertexAttribL1i64vNV = unsafePerformIO $ getCommand "glVertexAttribL1i64vNV"

-- glVertexAttribL1ui64ARB -----------------------------------------------------

glVertexAttribL1ui64ARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint64EXT -- ^ @x@.
  -> m ()
glVertexAttribL1ui64ARB v1 v2 = liftIO $ dyn919 ptr_glVertexAttribL1ui64ARB v1 v2

{-# NOINLINE ptr_glVertexAttribL1ui64ARB #-}
ptr_glVertexAttribL1ui64ARB :: FunPtr (GLuint -> GLuint64EXT -> IO ())
ptr_glVertexAttribL1ui64ARB = unsafePerformIO $ getCommand "glVertexAttribL1ui64ARB"

-- glVertexAttribL1ui64NV ------------------------------------------------------

glVertexAttribL1ui64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint64EXT -- ^ @x@.
  -> m ()
glVertexAttribL1ui64NV v1 v2 = liftIO $ dyn919 ptr_glVertexAttribL1ui64NV v1 v2

{-# NOINLINE ptr_glVertexAttribL1ui64NV #-}
ptr_glVertexAttribL1ui64NV :: FunPtr (GLuint -> GLuint64EXT -> IO ())
ptr_glVertexAttribL1ui64NV = unsafePerformIO $ getCommand "glVertexAttribL1ui64NV"

-- glVertexAttribL1ui64vARB ----------------------------------------------------

glVertexAttribL1ui64vARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint64EXT -- ^ @v@.
  -> m ()
glVertexAttribL1ui64vARB v1 v2 = liftIO $ dyn920 ptr_glVertexAttribL1ui64vARB v1 v2

{-# NOINLINE ptr_glVertexAttribL1ui64vARB #-}
ptr_glVertexAttribL1ui64vARB :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
ptr_glVertexAttribL1ui64vARB = unsafePerformIO $ getCommand "glVertexAttribL1ui64vARB"

-- glVertexAttribL1ui64vNV -----------------------------------------------------

glVertexAttribL1ui64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint64EXT -- ^ @v@ pointing to @1@ element of type @GLuint64EXT@.
  -> m ()
glVertexAttribL1ui64vNV v1 v2 = liftIO $ dyn920 ptr_glVertexAttribL1ui64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL1ui64vNV #-}
ptr_glVertexAttribL1ui64vNV :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
ptr_glVertexAttribL1ui64vNV = unsafePerformIO $ getCommand "glVertexAttribL1ui64vNV"

-- glVertexAttribL2d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL2d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexAttribL2d v1 v2 v3 = liftIO $ dyn228 ptr_glVertexAttribL2d v1 v2 v3

{-# NOINLINE ptr_glVertexAttribL2d #-}
ptr_glVertexAttribL2d :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL2d = unsafePerformIO $ getCommand "glVertexAttribL2d"

-- glVertexAttribL2dEXT --------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL2d'.
glVertexAttribL2dEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexAttribL2dEXT v1 v2 v3 = liftIO $ dyn228 ptr_glVertexAttribL2dEXT v1 v2 v3

{-# NOINLINE ptr_glVertexAttribL2dEXT #-}
ptr_glVertexAttribL2dEXT :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL2dEXT = unsafePerformIO $ getCommand "glVertexAttribL2dEXT"

-- glVertexAttribL2dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL2dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL2dv v1 v2 = liftIO $ dyn882 ptr_glVertexAttribL2dv v1 v2

{-# NOINLINE ptr_glVertexAttribL2dv #-}
ptr_glVertexAttribL2dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL2dv = unsafePerformIO $ getCommand "glVertexAttribL2dv"

-- glVertexAttribL2dvEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL2dv'.
glVertexAttribL2dvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL2dvEXT v1 v2 = liftIO $ dyn882 ptr_glVertexAttribL2dvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribL2dvEXT #-}
ptr_glVertexAttribL2dvEXT :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL2dvEXT = unsafePerformIO $ getCommand "glVertexAttribL2dvEXT"

-- glVertexAttribL2i64NV -------------------------------------------------------

glVertexAttribL2i64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> m ()
glVertexAttribL2i64NV v1 v2 v3 = liftIO $ dyn921 ptr_glVertexAttribL2i64NV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribL2i64NV #-}
ptr_glVertexAttribL2i64NV :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glVertexAttribL2i64NV = unsafePerformIO $ getCommand "glVertexAttribL2i64NV"

-- glVertexAttribL2i64vNV ------------------------------------------------------

glVertexAttribL2i64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint64EXT -- ^ @v@ pointing to @2@ elements of type @GLint64EXT@.
  -> m ()
glVertexAttribL2i64vNV v1 v2 = liftIO $ dyn918 ptr_glVertexAttribL2i64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL2i64vNV #-}
ptr_glVertexAttribL2i64vNV :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
ptr_glVertexAttribL2i64vNV = unsafePerformIO $ getCommand "glVertexAttribL2i64vNV"

-- glVertexAttribL2ui64NV ------------------------------------------------------

glVertexAttribL2ui64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> m ()
glVertexAttribL2ui64NV v1 v2 v3 = liftIO $ dyn922 ptr_glVertexAttribL2ui64NV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribL2ui64NV #-}
ptr_glVertexAttribL2ui64NV :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glVertexAttribL2ui64NV = unsafePerformIO $ getCommand "glVertexAttribL2ui64NV"

-- glVertexAttribL2ui64vNV -----------------------------------------------------

glVertexAttribL2ui64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint64EXT -- ^ @v@ pointing to @2@ elements of type @GLuint64EXT@.
  -> m ()
glVertexAttribL2ui64vNV v1 v2 = liftIO $ dyn920 ptr_glVertexAttribL2ui64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL2ui64vNV #-}
ptr_glVertexAttribL2ui64vNV :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
ptr_glVertexAttribL2ui64vNV = unsafePerformIO $ getCommand "glVertexAttribL2ui64vNV"

-- glVertexAttribL3d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL3d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexAttribL3d v1 v2 v3 v4 = liftIO $ dyn902 ptr_glVertexAttribL3d v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribL3d #-}
ptr_glVertexAttribL3d :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL3d = unsafePerformIO $ getCommand "glVertexAttribL3d"

-- glVertexAttribL3dEXT --------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL3d'.
glVertexAttribL3dEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexAttribL3dEXT v1 v2 v3 v4 = liftIO $ dyn902 ptr_glVertexAttribL3dEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribL3dEXT #-}
ptr_glVertexAttribL3dEXT :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL3dEXT = unsafePerformIO $ getCommand "glVertexAttribL3dEXT"

-- glVertexAttribL3dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL3dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL3dv v1 v2 = liftIO $ dyn882 ptr_glVertexAttribL3dv v1 v2

{-# NOINLINE ptr_glVertexAttribL3dv #-}
ptr_glVertexAttribL3dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL3dv = unsafePerformIO $ getCommand "glVertexAttribL3dv"

-- glVertexAttribL3dvEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL3dv'.
glVertexAttribL3dvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL3dvEXT v1 v2 = liftIO $ dyn882 ptr_glVertexAttribL3dvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribL3dvEXT #-}
ptr_glVertexAttribL3dvEXT :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL3dvEXT = unsafePerformIO $ getCommand "glVertexAttribL3dvEXT"

-- glVertexAttribL3i64NV -------------------------------------------------------

glVertexAttribL3i64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> m ()
glVertexAttribL3i64NV v1 v2 v3 v4 = liftIO $ dyn923 ptr_glVertexAttribL3i64NV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribL3i64NV #-}
ptr_glVertexAttribL3i64NV :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glVertexAttribL3i64NV = unsafePerformIO $ getCommand "glVertexAttribL3i64NV"

-- glVertexAttribL3i64vNV ------------------------------------------------------

glVertexAttribL3i64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint64EXT -- ^ @v@ pointing to @3@ elements of type @GLint64EXT@.
  -> m ()
glVertexAttribL3i64vNV v1 v2 = liftIO $ dyn918 ptr_glVertexAttribL3i64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL3i64vNV #-}
ptr_glVertexAttribL3i64vNV :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
ptr_glVertexAttribL3i64vNV = unsafePerformIO $ getCommand "glVertexAttribL3i64vNV"

-- glVertexAttribL3ui64NV ------------------------------------------------------

glVertexAttribL3ui64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> m ()
glVertexAttribL3ui64NV v1 v2 v3 v4 = liftIO $ dyn924 ptr_glVertexAttribL3ui64NV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribL3ui64NV #-}
ptr_glVertexAttribL3ui64NV :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glVertexAttribL3ui64NV = unsafePerformIO $ getCommand "glVertexAttribL3ui64NV"

-- glVertexAttribL3ui64vNV -----------------------------------------------------

glVertexAttribL3ui64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint64EXT -- ^ @v@ pointing to @3@ elements of type @GLuint64EXT@.
  -> m ()
glVertexAttribL3ui64vNV v1 v2 = liftIO $ dyn920 ptr_glVertexAttribL3ui64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL3ui64vNV #-}
ptr_glVertexAttribL3ui64vNV :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
ptr_glVertexAttribL3ui64vNV = unsafePerformIO $ getCommand "glVertexAttribL3ui64vNV"

-- glVertexAttribL4d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL4d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexAttribL4d v1 v2 v3 v4 v5 = liftIO $ dyn906 ptr_glVertexAttribL4d v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribL4d #-}
ptr_glVertexAttribL4d :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL4d = unsafePerformIO $ getCommand "glVertexAttribL4d"

-- glVertexAttribL4dEXT --------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL4d'.
glVertexAttribL4dEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexAttribL4dEXT v1 v2 v3 v4 v5 = liftIO $ dyn906 ptr_glVertexAttribL4dEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribL4dEXT #-}
ptr_glVertexAttribL4dEXT :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL4dEXT = unsafePerformIO $ getCommand "glVertexAttribL4dEXT"

-- glVertexAttribL4dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL4dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL4dv v1 v2 = liftIO $ dyn882 ptr_glVertexAttribL4dv v1 v2

{-# NOINLINE ptr_glVertexAttribL4dv #-}
ptr_glVertexAttribL4dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL4dv = unsafePerformIO $ getCommand "glVertexAttribL4dv"

-- glVertexAttribL4dvEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL4dv'.
glVertexAttribL4dvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL4dvEXT v1 v2 = liftIO $ dyn882 ptr_glVertexAttribL4dvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribL4dvEXT #-}
ptr_glVertexAttribL4dvEXT :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL4dvEXT = unsafePerformIO $ getCommand "glVertexAttribL4dvEXT"

-- glVertexAttribL4i64NV -------------------------------------------------------

glVertexAttribL4i64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> GLint64EXT -- ^ @w@.
  -> m ()
glVertexAttribL4i64NV v1 v2 v3 v4 v5 = liftIO $ dyn925 ptr_glVertexAttribL4i64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribL4i64NV #-}
ptr_glVertexAttribL4i64NV :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glVertexAttribL4i64NV = unsafePerformIO $ getCommand "glVertexAttribL4i64NV"

-- glVertexAttribL4i64vNV ------------------------------------------------------

glVertexAttribL4i64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint64EXT -- ^ @v@ pointing to @4@ elements of type @GLint64EXT@.
  -> m ()
glVertexAttribL4i64vNV v1 v2 = liftIO $ dyn918 ptr_glVertexAttribL4i64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL4i64vNV #-}
ptr_glVertexAttribL4i64vNV :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
ptr_glVertexAttribL4i64vNV = unsafePerformIO $ getCommand "glVertexAttribL4i64vNV"

-- glVertexAttribL4ui64NV ------------------------------------------------------

glVertexAttribL4ui64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> GLuint64EXT -- ^ @w@.
  -> m ()
glVertexAttribL4ui64NV v1 v2 v3 v4 v5 = liftIO $ dyn926 ptr_glVertexAttribL4ui64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribL4ui64NV #-}
ptr_glVertexAttribL4ui64NV :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glVertexAttribL4ui64NV = unsafePerformIO $ getCommand "glVertexAttribL4ui64NV"

-- glVertexAttribL4ui64vNV -----------------------------------------------------

glVertexAttribL4ui64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint64EXT -- ^ @v@ pointing to @4@ elements of type @GLuint64EXT@.
  -> m ()
glVertexAttribL4ui64vNV v1 v2 = liftIO $ dyn920 ptr_glVertexAttribL4ui64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL4ui64vNV #-}
ptr_glVertexAttribL4ui64vNV :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
ptr_glVertexAttribL4ui64vNV = unsafePerformIO $ getCommand "glVertexAttribL4ui64vNV"

-- glVertexAttribLFormat -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexAttribLFormat
  :: MonadIO m
  => GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribLType](Graphics-GL-Groups.html#VertexAttribLType).
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexAttribLFormat v1 v2 v3 v4 = liftIO $ dyn914 ptr_glVertexAttribLFormat v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribLFormat #-}
ptr_glVertexAttribLFormat :: FunPtr (GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexAttribLFormat = unsafePerformIO $ getCommand "glVertexAttribLFormat"

-- glVertexAttribLFormatNV -----------------------------------------------------

glVertexAttribLFormatNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribLType](Graphics-GL-Groups.html#VertexAttribLType).
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexAttribLFormatNV v1 v2 v3 v4 = liftIO $ dyn915 ptr_glVertexAttribLFormatNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribLFormatNV #-}
ptr_glVertexAttribLFormatNV :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> IO ())
ptr_glVertexAttribLFormatNV = unsafePerformIO $ getCommand "glVertexAttribLFormatNV"

-- glVertexAttribLPointer ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribPointer.xhtml OpenGL 4.x>.
glVertexAttribLPointer
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @size@ elements of type @a@.
  -> m ()
glVertexAttribLPointer v1 v2 v3 v4 v5 = liftIO $ dyn916 ptr_glVertexAttribLPointer v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribLPointer #-}
ptr_glVertexAttribLPointer :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribLPointer = unsafePerformIO $ getCommand "glVertexAttribLPointer"

