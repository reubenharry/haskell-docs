{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F32
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

module Graphics.GL.Functions.F32 (
  glVertexAttribLPointerEXT,
  glVertexAttribP1ui,
  glVertexAttribP1uiv,
  glVertexAttribP2ui,
  glVertexAttribP2uiv,
  glVertexAttribP3ui,
  glVertexAttribP3uiv,
  glVertexAttribP4ui,
  glVertexAttribP4uiv,
  glVertexAttribParameteriAMD,
  glVertexAttribPointer,
  glVertexAttribPointerARB,
  glVertexAttribPointerNV,
  glVertexAttribs1dvNV,
  glVertexAttribs1fvNV,
  glVertexAttribs1hvNV,
  glVertexAttribs1svNV,
  glVertexAttribs2dvNV,
  glVertexAttribs2fvNV,
  glVertexAttribs2hvNV,
  glVertexAttribs2svNV,
  glVertexAttribs3dvNV,
  glVertexAttribs3fvNV,
  glVertexAttribs3hvNV,
  glVertexAttribs3svNV,
  glVertexAttribs4dvNV,
  glVertexAttribs4fvNV,
  glVertexAttribs4hvNV,
  glVertexAttribs4svNV,
  glVertexAttribs4ubvNV,
  glVertexBindingDivisor,
  glVertexBlendARB,
  glVertexBlendEnvfATI,
  glVertexBlendEnviATI,
  glVertexFormatNV,
  glVertexP2ui,
  glVertexP2uiv,
  glVertexP3ui,
  glVertexP3uiv,
  glVertexP4ui,
  glVertexP4uiv,
  glVertexPointer,
  glVertexPointerEXT,
  glVertexPointerListIBM,
  glVertexPointervINTEL,
  glVertexStream1dATI,
  glVertexStream1dvATI,
  glVertexStream1fATI,
  glVertexStream1fvATI,
  glVertexStream1iATI,
  glVertexStream1ivATI,
  glVertexStream1sATI,
  glVertexStream1svATI,
  glVertexStream2dATI,
  glVertexStream2dvATI,
  glVertexStream2fATI,
  glVertexStream2fvATI,
  glVertexStream2iATI,
  glVertexStream2ivATI,
  glVertexStream2sATI,
  glVertexStream2svATI,
  glVertexStream3dATI,
  glVertexStream3dvATI,
  glVertexStream3fATI,
  glVertexStream3fvATI,
  glVertexStream3iATI,
  glVertexStream3ivATI,
  glVertexStream3sATI,
  glVertexStream3svATI,
  glVertexStream4dATI,
  glVertexStream4dvATI,
  glVertexStream4fATI,
  glVertexStream4fvATI,
  glVertexStream4iATI,
  glVertexStream4ivATI,
  glVertexStream4sATI,
  glVertexStream4svATI,
  glVertexWeightPointerEXT,
  glVertexWeightfEXT,
  glVertexWeightfvEXT,
  glVertexWeighthNV,
  glVertexWeighthvNV,
  glVideoCaptureNV,
  glVideoCaptureStreamParameterdvNV,
  glVideoCaptureStreamParameterfvNV,
  glVideoCaptureStreamParameterivNV,
  glViewport,
  glViewportArrayv,
  glViewportArrayvNV,
  glViewportArrayvOES,
  glViewportIndexedf,
  glViewportIndexedfNV,
  glViewportIndexedfOES,
  glViewportIndexedfv,
  glViewportIndexedfvNV,
  glViewportIndexedfvOES,
  glViewportPositionWScaleNV,
  glViewportSwizzleNV,
  glWaitSemaphoreEXT,
  glWaitSemaphoreui64NVX
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glVertexAttribLPointerEXT ---------------------------------------------------

-- | This command is an alias for 'glVertexAttribLPointer'.
glVertexAttribLPointerEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @size@ elements of type @a@.
  -> m ()
glVertexAttribLPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn916 ptr_glVertexAttribLPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribLPointerEXT #-}
ptr_glVertexAttribLPointerEXT :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribLPointerEXT = unsafePerformIO $ getCommand "glVertexAttribLPointerEXT"

-- glVertexAttribP1ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribP1ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @value@.
  -> m ()
glVertexAttribP1ui v1 v2 v3 v4 = liftIO $ dyn927 ptr_glVertexAttribP1ui v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP1ui #-}
ptr_glVertexAttribP1ui :: FunPtr (GLuint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexAttribP1ui = unsafePerformIO $ getCommand "glVertexAttribP1ui"

-- glVertexAttribP1uiv ---------------------------------------------------------

glVertexAttribP1uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribP1uiv v1 v2 v3 v4 = liftIO $ dyn928 ptr_glVertexAttribP1uiv v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP1uiv #-}
ptr_glVertexAttribP1uiv :: FunPtr (GLuint -> GLenum -> GLboolean -> Ptr GLuint -> IO ())
ptr_glVertexAttribP1uiv = unsafePerformIO $ getCommand "glVertexAttribP1uiv"

-- glVertexAttribP2ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribP2ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @value@.
  -> m ()
glVertexAttribP2ui v1 v2 v3 v4 = liftIO $ dyn927 ptr_glVertexAttribP2ui v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP2ui #-}
ptr_glVertexAttribP2ui :: FunPtr (GLuint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexAttribP2ui = unsafePerformIO $ getCommand "glVertexAttribP2ui"

-- glVertexAttribP2uiv ---------------------------------------------------------

glVertexAttribP2uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribP2uiv v1 v2 v3 v4 = liftIO $ dyn928 ptr_glVertexAttribP2uiv v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP2uiv #-}
ptr_glVertexAttribP2uiv :: FunPtr (GLuint -> GLenum -> GLboolean -> Ptr GLuint -> IO ())
ptr_glVertexAttribP2uiv = unsafePerformIO $ getCommand "glVertexAttribP2uiv"

-- glVertexAttribP3ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribP3ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @value@.
  -> m ()
glVertexAttribP3ui v1 v2 v3 v4 = liftIO $ dyn927 ptr_glVertexAttribP3ui v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP3ui #-}
ptr_glVertexAttribP3ui :: FunPtr (GLuint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexAttribP3ui = unsafePerformIO $ getCommand "glVertexAttribP3ui"

-- glVertexAttribP3uiv ---------------------------------------------------------

glVertexAttribP3uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribP3uiv v1 v2 v3 v4 = liftIO $ dyn928 ptr_glVertexAttribP3uiv v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP3uiv #-}
ptr_glVertexAttribP3uiv :: FunPtr (GLuint -> GLenum -> GLboolean -> Ptr GLuint -> IO ())
ptr_glVertexAttribP3uiv = unsafePerformIO $ getCommand "glVertexAttribP3uiv"

-- glVertexAttribP4ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribP4ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @value@.
  -> m ()
glVertexAttribP4ui v1 v2 v3 v4 = liftIO $ dyn927 ptr_glVertexAttribP4ui v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP4ui #-}
ptr_glVertexAttribP4ui :: FunPtr (GLuint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexAttribP4ui = unsafePerformIO $ getCommand "glVertexAttribP4ui"

-- glVertexAttribP4uiv ---------------------------------------------------------

glVertexAttribP4uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribP4uiv v1 v2 v3 v4 = liftIO $ dyn928 ptr_glVertexAttribP4uiv v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP4uiv #-}
ptr_glVertexAttribP4uiv :: FunPtr (GLuint -> GLenum -> GLboolean -> Ptr GLuint -> IO ())
ptr_glVertexAttribP4uiv = unsafePerformIO $ getCommand "glVertexAttribP4uiv"

-- glVertexAttribParameteriAMD -------------------------------------------------

glVertexAttribParameteriAMD
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> GLint -- ^ @param@.
  -> m ()
glVertexAttribParameteriAMD v1 v2 v3 = liftIO $ dyn491 ptr_glVertexAttribParameteriAMD v1 v2 v3

{-# NOINLINE ptr_glVertexAttribParameteriAMD #-}
ptr_glVertexAttribParameteriAMD :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glVertexAttribParameteriAMD = unsafePerformIO $ getCommand "glVertexAttribParameteriAMD"

-- glVertexAttribPointer -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttribPointer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttribPointer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribPointer.xhtml OpenGL 4.x>.
glVertexAttribPointer
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glVertexAttribPointer v1 v2 v3 v4 v5 v6 = liftIO $ dyn929 ptr_glVertexAttribPointer v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexAttribPointer #-}
ptr_glVertexAttribPointer :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribPointer = unsafePerformIO $ getCommand "glVertexAttribPointer"

-- glVertexAttribPointerARB ----------------------------------------------------

-- | This command is an alias for 'glVertexAttribPointer'.
glVertexAttribPointerARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexAttribPointerType](Graphics-GL-Groups.html#VertexAttribPointerType).
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glVertexAttribPointerARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn929 ptr_glVertexAttribPointerARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexAttribPointerARB #-}
ptr_glVertexAttribPointerARB :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribPointerARB = unsafePerformIO $ getCommand "glVertexAttribPointerARB"

-- glVertexAttribPointerNV -----------------------------------------------------

glVertexAttribPointerNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @fsize@.
  -> GLenum -- ^ @type@ of type [VertexAttribEnumNV](Graphics-GL-Groups.html#VertexAttribEnumNV).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(fsize,type,stride)@ elements of type @a@.
  -> m ()
glVertexAttribPointerNV v1 v2 v3 v4 v5 = liftIO $ dyn916 ptr_glVertexAttribPointerNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribPointerNV #-}
ptr_glVertexAttribPointerNV :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribPointerNV = unsafePerformIO $ getCommand "glVertexAttribPointerNV"

-- glVertexAttribs1dvNV --------------------------------------------------------

glVertexAttribs1dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glVertexAttribs1dvNV v1 v2 v3 = liftIO $ dyn227 ptr_glVertexAttribs1dvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs1dvNV #-}
ptr_glVertexAttribs1dvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glVertexAttribs1dvNV = unsafePerformIO $ getCommand "glVertexAttribs1dvNV"

-- glVertexAttribs1fvNV --------------------------------------------------------

glVertexAttribs1fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glVertexAttribs1fvNV v1 v2 v3 = liftIO $ dyn226 ptr_glVertexAttribs1fvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs1fvNV #-}
ptr_glVertexAttribs1fvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glVertexAttribs1fvNV = unsafePerformIO $ getCommand "glVertexAttribs1fvNV"

-- glVertexAttribs1hvNV --------------------------------------------------------

glVertexAttribs1hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @n@ elements of type @Half16NV@.
  -> m ()
glVertexAttribs1hvNV v1 v2 v3 = liftIO $ dyn930 ptr_glVertexAttribs1hvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs1hvNV #-}
ptr_glVertexAttribs1hvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttribs1hvNV = unsafePerformIO $ getCommand "glVertexAttribs1hvNV"

-- glVertexAttribs1svNV --------------------------------------------------------

glVertexAttribs1svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLshort -- ^ @v@ pointing to @count@ elements of type @GLshort@.
  -> m ()
glVertexAttribs1svNV v1 v2 v3 = liftIO $ dyn931 ptr_glVertexAttribs1svNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs1svNV #-}
ptr_glVertexAttribs1svNV :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
ptr_glVertexAttribs1svNV = unsafePerformIO $ getCommand "glVertexAttribs1svNV"

-- glVertexAttribs2dvNV --------------------------------------------------------

glVertexAttribs2dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @count*2@ elements of type @GLdouble@.
  -> m ()
glVertexAttribs2dvNV v1 v2 v3 = liftIO $ dyn227 ptr_glVertexAttribs2dvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs2dvNV #-}
ptr_glVertexAttribs2dvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glVertexAttribs2dvNV = unsafePerformIO $ getCommand "glVertexAttribs2dvNV"

-- glVertexAttribs2fvNV --------------------------------------------------------

glVertexAttribs2fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @count*2@ elements of type @GLfloat@.
  -> m ()
glVertexAttribs2fvNV v1 v2 v3 = liftIO $ dyn226 ptr_glVertexAttribs2fvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs2fvNV #-}
ptr_glVertexAttribs2fvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glVertexAttribs2fvNV = unsafePerformIO $ getCommand "glVertexAttribs2fvNV"

-- glVertexAttribs2hvNV --------------------------------------------------------

glVertexAttribs2hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @n@ elements of type @Half16NV@.
  -> m ()
glVertexAttribs2hvNV v1 v2 v3 = liftIO $ dyn930 ptr_glVertexAttribs2hvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs2hvNV #-}
ptr_glVertexAttribs2hvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttribs2hvNV = unsafePerformIO $ getCommand "glVertexAttribs2hvNV"

-- glVertexAttribs2svNV --------------------------------------------------------

glVertexAttribs2svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLshort -- ^ @v@ pointing to @count*2@ elements of type @GLshort@.
  -> m ()
glVertexAttribs2svNV v1 v2 v3 = liftIO $ dyn931 ptr_glVertexAttribs2svNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs2svNV #-}
ptr_glVertexAttribs2svNV :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
ptr_glVertexAttribs2svNV = unsafePerformIO $ getCommand "glVertexAttribs2svNV"

-- glVertexAttribs3dvNV --------------------------------------------------------

glVertexAttribs3dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @count*3@ elements of type @GLdouble@.
  -> m ()
glVertexAttribs3dvNV v1 v2 v3 = liftIO $ dyn227 ptr_glVertexAttribs3dvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs3dvNV #-}
ptr_glVertexAttribs3dvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glVertexAttribs3dvNV = unsafePerformIO $ getCommand "glVertexAttribs3dvNV"

-- glVertexAttribs3fvNV --------------------------------------------------------

glVertexAttribs3fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @count*3@ elements of type @GLfloat@.
  -> m ()
glVertexAttribs3fvNV v1 v2 v3 = liftIO $ dyn226 ptr_glVertexAttribs3fvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs3fvNV #-}
ptr_glVertexAttribs3fvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glVertexAttribs3fvNV = unsafePerformIO $ getCommand "glVertexAttribs3fvNV"

-- glVertexAttribs3hvNV --------------------------------------------------------

glVertexAttribs3hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @n@ elements of type @Half16NV@.
  -> m ()
glVertexAttribs3hvNV v1 v2 v3 = liftIO $ dyn930 ptr_glVertexAttribs3hvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs3hvNV #-}
ptr_glVertexAttribs3hvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttribs3hvNV = unsafePerformIO $ getCommand "glVertexAttribs3hvNV"

-- glVertexAttribs3svNV --------------------------------------------------------

glVertexAttribs3svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLshort -- ^ @v@ pointing to @count*3@ elements of type @GLshort@.
  -> m ()
glVertexAttribs3svNV v1 v2 v3 = liftIO $ dyn931 ptr_glVertexAttribs3svNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs3svNV #-}
ptr_glVertexAttribs3svNV :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
ptr_glVertexAttribs3svNV = unsafePerformIO $ getCommand "glVertexAttribs3svNV"

-- glVertexAttribs4dvNV --------------------------------------------------------

glVertexAttribs4dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glVertexAttribs4dvNV v1 v2 v3 = liftIO $ dyn227 ptr_glVertexAttribs4dvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs4dvNV #-}
ptr_glVertexAttribs4dvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glVertexAttribs4dvNV = unsafePerformIO $ getCommand "glVertexAttribs4dvNV"

-- glVertexAttribs4fvNV --------------------------------------------------------

glVertexAttribs4fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glVertexAttribs4fvNV v1 v2 v3 = liftIO $ dyn226 ptr_glVertexAttribs4fvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs4fvNV #-}
ptr_glVertexAttribs4fvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glVertexAttribs4fvNV = unsafePerformIO $ getCommand "glVertexAttribs4fvNV"

-- glVertexAttribs4hvNV --------------------------------------------------------

glVertexAttribs4hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @n@ elements of type @Half16NV@.
  -> m ()
glVertexAttribs4hvNV v1 v2 v3 = liftIO $ dyn930 ptr_glVertexAttribs4hvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs4hvNV #-}
ptr_glVertexAttribs4hvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttribs4hvNV = unsafePerformIO $ getCommand "glVertexAttribs4hvNV"

-- glVertexAttribs4svNV --------------------------------------------------------

glVertexAttribs4svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLshort -- ^ @v@ pointing to @count*4@ elements of type @GLshort@.
  -> m ()
glVertexAttribs4svNV v1 v2 v3 = liftIO $ dyn931 ptr_glVertexAttribs4svNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs4svNV #-}
ptr_glVertexAttribs4svNV :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
ptr_glVertexAttribs4svNV = unsafePerformIO $ getCommand "glVertexAttribs4svNV"

-- glVertexAttribs4ubvNV -------------------------------------------------------

glVertexAttribs4ubvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLubyte -- ^ @v@ pointing to @count*4@ elements of type @ColorUB@.
  -> m ()
glVertexAttribs4ubvNV v1 v2 v3 = liftIO $ dyn932 ptr_glVertexAttribs4ubvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs4ubvNV #-}
ptr_glVertexAttribs4ubvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> IO ())
ptr_glVertexAttribs4ubvNV = unsafePerformIO $ getCommand "glVertexAttribs4ubvNV"

-- glVertexBindingDivisor ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexBindingDivisor.xhtml OpenGL 4.x>.
glVertexBindingDivisor
  :: MonadIO m
  => GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexBindingDivisor v1 v2 = liftIO $ dyn4 ptr_glVertexBindingDivisor v1 v2

{-# NOINLINE ptr_glVertexBindingDivisor #-}
ptr_glVertexBindingDivisor :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexBindingDivisor = unsafePerformIO $ getCommand "glVertexBindingDivisor"

-- glVertexBlendARB ------------------------------------------------------------

glVertexBlendARB
  :: MonadIO m
  => GLint -- ^ @count@.
  -> m ()
glVertexBlendARB v1 = liftIO $ dyn13 ptr_glVertexBlendARB v1

{-# NOINLINE ptr_glVertexBlendARB #-}
ptr_glVertexBlendARB :: FunPtr (GLint -> IO ())
ptr_glVertexBlendARB = unsafePerformIO $ getCommand "glVertexBlendARB"

-- glVertexBlendEnvfATI --------------------------------------------------------

glVertexBlendEnvfATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLfloat -- ^ @param@.
  -> m ()
glVertexBlendEnvfATI v1 v2 = liftIO $ dyn0 ptr_glVertexBlendEnvfATI v1 v2

{-# NOINLINE ptr_glVertexBlendEnvfATI #-}
ptr_glVertexBlendEnvfATI :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glVertexBlendEnvfATI = unsafePerformIO $ getCommand "glVertexBlendEnvfATI"

-- glVertexBlendEnviATI --------------------------------------------------------

glVertexBlendEnviATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLint -- ^ @param@.
  -> m ()
glVertexBlendEnviATI v1 v2 = liftIO $ dyn58 ptr_glVertexBlendEnviATI v1 v2

{-# NOINLINE ptr_glVertexBlendEnviATI #-}
ptr_glVertexBlendEnviATI :: FunPtr (GLenum -> GLint -> IO ())
ptr_glVertexBlendEnviATI = unsafePerformIO $ getCommand "glVertexBlendEnviATI"

-- glVertexFormatNV ------------------------------------------------------------

glVertexFormatNV
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexFormatNV v1 v2 v3 = liftIO $ dyn126 ptr_glVertexFormatNV v1 v2 v3

{-# NOINLINE ptr_glVertexFormatNV #-}
ptr_glVertexFormatNV :: FunPtr (GLint -> GLenum -> GLsizei -> IO ())
ptr_glVertexFormatNV = unsafePerformIO $ getCommand "glVertexFormatNV"

-- glVertexP2ui ----------------------------------------------------------------

glVertexP2ui
  :: MonadIO m
  => GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLuint -- ^ @value@.
  -> m ()
glVertexP2ui v1 v2 = liftIO $ dyn19 ptr_glVertexP2ui v1 v2

{-# NOINLINE ptr_glVertexP2ui #-}
ptr_glVertexP2ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glVertexP2ui = unsafePerformIO $ getCommand "glVertexP2ui"

-- glVertexP2uiv ---------------------------------------------------------------

glVertexP2uiv
  :: MonadIO m
  => GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexP2uiv v1 v2 = liftIO $ dyn132 ptr_glVertexP2uiv v1 v2

{-# NOINLINE ptr_glVertexP2uiv #-}
ptr_glVertexP2uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glVertexP2uiv = unsafePerformIO $ getCommand "glVertexP2uiv"

-- glVertexP3ui ----------------------------------------------------------------

glVertexP3ui
  :: MonadIO m
  => GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLuint -- ^ @value@.
  -> m ()
glVertexP3ui v1 v2 = liftIO $ dyn19 ptr_glVertexP3ui v1 v2

{-# NOINLINE ptr_glVertexP3ui #-}
ptr_glVertexP3ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glVertexP3ui = unsafePerformIO $ getCommand "glVertexP3ui"

-- glVertexP3uiv ---------------------------------------------------------------

glVertexP3uiv
  :: MonadIO m
  => GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexP3uiv v1 v2 = liftIO $ dyn132 ptr_glVertexP3uiv v1 v2

{-# NOINLINE ptr_glVertexP3uiv #-}
ptr_glVertexP3uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glVertexP3uiv = unsafePerformIO $ getCommand "glVertexP3uiv"

-- glVertexP4ui ----------------------------------------------------------------

glVertexP4ui
  :: MonadIO m
  => GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLuint -- ^ @value@.
  -> m ()
glVertexP4ui v1 v2 = liftIO $ dyn19 ptr_glVertexP4ui v1 v2

{-# NOINLINE ptr_glVertexP4ui #-}
ptr_glVertexP4ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glVertexP4ui = unsafePerformIO $ getCommand "glVertexP4ui"

-- glVertexP4uiv ---------------------------------------------------------------

glVertexP4uiv
  :: MonadIO m
  => GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexP4uiv v1 v2 = liftIO $ dyn132 ptr_glVertexP4uiv v1 v2

{-# NOINLINE ptr_glVertexP4uiv #-}
ptr_glVertexP4uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glVertexP4uiv = unsafePerformIO $ getCommand "glVertexP4uiv"

-- glVertexPointer -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexPointer.xml OpenGL 2.x>.
glVertexPointer
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glVertexPointer v1 v2 v3 v4 = liftIO $ dyn133 ptr_glVertexPointer v1 v2 v3 v4

{-# NOINLINE ptr_glVertexPointer #-}
ptr_glVertexPointer :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexPointer = unsafePerformIO $ getCommand "glVertexPointer"

-- glVertexPointerEXT ----------------------------------------------------------

glVertexPointerEXT
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLsizei -- ^ @count@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride,count)@ elements of type @a@.
  -> m ()
glVertexPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn134 ptr_glVertexPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexPointerEXT #-}
ptr_glVertexPointerEXT :: FunPtr (GLint -> GLenum -> GLsizei -> GLsizei -> Ptr a -> IO ())
ptr_glVertexPointerEXT = unsafePerformIO $ getCommand "glVertexPointerEXT"

-- glVertexPointerListIBM ------------------------------------------------------

glVertexPointerListIBM
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glVertexPointerListIBM v1 v2 v3 v4 v5 = liftIO $ dyn135 ptr_glVertexPointerListIBM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexPointerListIBM #-}
ptr_glVertexPointerListIBM :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glVertexPointerListIBM = unsafePerformIO $ getCommand "glVertexPointerListIBM"

-- glVertexPointervINTEL -------------------------------------------------------

glVertexPointervINTEL
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @4@ elements of type @Ptr a@.
  -> m ()
glVertexPointervINTEL v1 v2 v3 = liftIO $ dyn136 ptr_glVertexPointervINTEL v1 v2 v3

{-# NOINLINE ptr_glVertexPointervINTEL #-}
ptr_glVertexPointervINTEL :: FunPtr (GLint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glVertexPointervINTEL = unsafePerformIO $ getCommand "glVertexPointervINTEL"

-- glVertexStream1dATI ---------------------------------------------------------

glVertexStream1dATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexStream1dATI v1 v2 = liftIO $ dyn569 ptr_glVertexStream1dATI v1 v2

{-# NOINLINE ptr_glVertexStream1dATI #-}
ptr_glVertexStream1dATI :: FunPtr (GLenum -> GLdouble -> IO ())
ptr_glVertexStream1dATI = unsafePerformIO $ getCommand "glVertexStream1dATI"

-- glVertexStream1dvATI --------------------------------------------------------

glVertexStream1dvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLdouble -- ^ @coords@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexStream1dvATI v1 v2 = liftIO $ dyn100 ptr_glVertexStream1dvATI v1 v2

{-# NOINLINE ptr_glVertexStream1dvATI #-}
ptr_glVertexStream1dvATI :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glVertexStream1dvATI = unsafePerformIO $ getCommand "glVertexStream1dvATI"

-- glVertexStream1fATI ---------------------------------------------------------

glVertexStream1fATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLfloat -- ^ @x@.
  -> m ()
glVertexStream1fATI v1 v2 = liftIO $ dyn0 ptr_glVertexStream1fATI v1 v2

{-# NOINLINE ptr_glVertexStream1fATI #-}
ptr_glVertexStream1fATI :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glVertexStream1fATI = unsafePerformIO $ getCommand "glVertexStream1fATI"

-- glVertexStream1fvATI --------------------------------------------------------

glVertexStream1fvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLfloat -- ^ @coords@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glVertexStream1fvATI v1 v2 = liftIO $ dyn101 ptr_glVertexStream1fvATI v1 v2

{-# NOINLINE ptr_glVertexStream1fvATI #-}
ptr_glVertexStream1fvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glVertexStream1fvATI = unsafePerformIO $ getCommand "glVertexStream1fvATI"

-- glVertexStream1iATI ---------------------------------------------------------

glVertexStream1iATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLint -- ^ @x@.
  -> m ()
glVertexStream1iATI v1 v2 = liftIO $ dyn58 ptr_glVertexStream1iATI v1 v2

{-# NOINLINE ptr_glVertexStream1iATI #-}
ptr_glVertexStream1iATI :: FunPtr (GLenum -> GLint -> IO ())
ptr_glVertexStream1iATI = unsafePerformIO $ getCommand "glVertexStream1iATI"

-- glVertexStream1ivATI --------------------------------------------------------

glVertexStream1ivATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLint -- ^ @coords@ pointing to @1@ element of type @GLint@.
  -> m ()
glVertexStream1ivATI v1 v2 = liftIO $ dyn143 ptr_glVertexStream1ivATI v1 v2

{-# NOINLINE ptr_glVertexStream1ivATI #-}
ptr_glVertexStream1ivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glVertexStream1ivATI = unsafePerformIO $ getCommand "glVertexStream1ivATI"

-- glVertexStream1sATI ---------------------------------------------------------

glVertexStream1sATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLshort -- ^ @x@.
  -> m ()
glVertexStream1sATI v1 v2 = liftIO $ dyn572 ptr_glVertexStream1sATI v1 v2

{-# NOINLINE ptr_glVertexStream1sATI #-}
ptr_glVertexStream1sATI :: FunPtr (GLenum -> GLshort -> IO ())
ptr_glVertexStream1sATI = unsafePerformIO $ getCommand "glVertexStream1sATI"

-- glVertexStream1svATI --------------------------------------------------------

glVertexStream1svATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLshort -- ^ @coords@ pointing to @1@ element of type @GLshort@.
  -> m ()
glVertexStream1svATI v1 v2 = liftIO $ dyn573 ptr_glVertexStream1svATI v1 v2

{-# NOINLINE ptr_glVertexStream1svATI #-}
ptr_glVertexStream1svATI :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glVertexStream1svATI = unsafePerformIO $ getCommand "glVertexStream1svATI"

-- glVertexStream2dATI ---------------------------------------------------------

glVertexStream2dATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexStream2dATI v1 v2 v3 = liftIO $ dyn575 ptr_glVertexStream2dATI v1 v2 v3

{-# NOINLINE ptr_glVertexStream2dATI #-}
ptr_glVertexStream2dATI :: FunPtr (GLenum -> GLdouble -> GLdouble -> IO ())
ptr_glVertexStream2dATI = unsafePerformIO $ getCommand "glVertexStream2dATI"

-- glVertexStream2dvATI --------------------------------------------------------

glVertexStream2dvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLdouble -- ^ @coords@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexStream2dvATI v1 v2 = liftIO $ dyn100 ptr_glVertexStream2dvATI v1 v2

{-# NOINLINE ptr_glVertexStream2dvATI #-}
ptr_glVertexStream2dvATI :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glVertexStream2dvATI = unsafePerformIO $ getCommand "glVertexStream2dvATI"

-- glVertexStream2fATI ---------------------------------------------------------

glVertexStream2fATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m ()
glVertexStream2fATI v1 v2 v3 = liftIO $ dyn576 ptr_glVertexStream2fATI v1 v2 v3

{-# NOINLINE ptr_glVertexStream2fATI #-}
ptr_glVertexStream2fATI :: FunPtr (GLenum -> GLfloat -> GLfloat -> IO ())
ptr_glVertexStream2fATI = unsafePerformIO $ getCommand "glVertexStream2fATI"

-- glVertexStream2fvATI --------------------------------------------------------

glVertexStream2fvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLfloat -- ^ @coords@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glVertexStream2fvATI v1 v2 = liftIO $ dyn101 ptr_glVertexStream2fvATI v1 v2

{-# NOINLINE ptr_glVertexStream2fvATI #-}
ptr_glVertexStream2fvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glVertexStream2fvATI = unsafePerformIO $ getCommand "glVertexStream2fvATI"

-- glVertexStream2iATI ---------------------------------------------------------

glVertexStream2iATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> m ()
glVertexStream2iATI v1 v2 v3 = liftIO $ dyn275 ptr_glVertexStream2iATI v1 v2 v3

{-# NOINLINE ptr_glVertexStream2iATI #-}
ptr_glVertexStream2iATI :: FunPtr (GLenum -> GLint -> GLint -> IO ())
ptr_glVertexStream2iATI = unsafePerformIO $ getCommand "glVertexStream2iATI"

-- glVertexStream2ivATI --------------------------------------------------------

glVertexStream2ivATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLint -- ^ @coords@ pointing to @2@ elements of type @GLint@.
  -> m ()
glVertexStream2ivATI v1 v2 = liftIO $ dyn143 ptr_glVertexStream2ivATI v1 v2

{-# NOINLINE ptr_glVertexStream2ivATI #-}
ptr_glVertexStream2ivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glVertexStream2ivATI = unsafePerformIO $ getCommand "glVertexStream2ivATI"

-- glVertexStream2sATI ---------------------------------------------------------

glVertexStream2sATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> m ()
glVertexStream2sATI v1 v2 v3 = liftIO $ dyn578 ptr_glVertexStream2sATI v1 v2 v3

{-# NOINLINE ptr_glVertexStream2sATI #-}
ptr_glVertexStream2sATI :: FunPtr (GLenum -> GLshort -> GLshort -> IO ())
ptr_glVertexStream2sATI = unsafePerformIO $ getCommand "glVertexStream2sATI"

-- glVertexStream2svATI --------------------------------------------------------

glVertexStream2svATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLshort -- ^ @coords@ pointing to @2@ elements of type @GLshort@.
  -> m ()
glVertexStream2svATI v1 v2 = liftIO $ dyn573 ptr_glVertexStream2svATI v1 v2

{-# NOINLINE ptr_glVertexStream2svATI #-}
ptr_glVertexStream2svATI :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glVertexStream2svATI = unsafePerformIO $ getCommand "glVertexStream2svATI"

-- glVertexStream3dATI ---------------------------------------------------------

glVertexStream3dATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexStream3dATI v1 v2 v3 v4 = liftIO $ dyn548 ptr_glVertexStream3dATI v1 v2 v3 v4

{-# NOINLINE ptr_glVertexStream3dATI #-}
ptr_glVertexStream3dATI :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexStream3dATI = unsafePerformIO $ getCommand "glVertexStream3dATI"

-- glVertexStream3dvATI --------------------------------------------------------

glVertexStream3dvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLdouble -- ^ @coords@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexStream3dvATI v1 v2 = liftIO $ dyn100 ptr_glVertexStream3dvATI v1 v2

{-# NOINLINE ptr_glVertexStream3dvATI #-}
ptr_glVertexStream3dvATI :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glVertexStream3dvATI = unsafePerformIO $ getCommand "glVertexStream3dvATI"

-- glVertexStream3fATI ---------------------------------------------------------

glVertexStream3fATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glVertexStream3fATI v1 v2 v3 v4 = liftIO $ dyn549 ptr_glVertexStream3fATI v1 v2 v3 v4

{-# NOINLINE ptr_glVertexStream3fATI #-}
ptr_glVertexStream3fATI :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexStream3fATI = unsafePerformIO $ getCommand "glVertexStream3fATI"

-- glVertexStream3fvATI --------------------------------------------------------

glVertexStream3fvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLfloat -- ^ @coords@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glVertexStream3fvATI v1 v2 = liftIO $ dyn101 ptr_glVertexStream3fvATI v1 v2

{-# NOINLINE ptr_glVertexStream3fvATI #-}
ptr_glVertexStream3fvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glVertexStream3fvATI = unsafePerformIO $ getCommand "glVertexStream3fvATI"

-- glVertexStream3iATI ---------------------------------------------------------

glVertexStream3iATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> m ()
glVertexStream3iATI v1 v2 v3 v4 = liftIO $ dyn582 ptr_glVertexStream3iATI v1 v2 v3 v4

{-# NOINLINE ptr_glVertexStream3iATI #-}
ptr_glVertexStream3iATI :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexStream3iATI = unsafePerformIO $ getCommand "glVertexStream3iATI"

-- glVertexStream3ivATI --------------------------------------------------------

glVertexStream3ivATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLint -- ^ @coords@ pointing to @3@ elements of type @GLint@.
  -> m ()
glVertexStream3ivATI v1 v2 = liftIO $ dyn143 ptr_glVertexStream3ivATI v1 v2

{-# NOINLINE ptr_glVertexStream3ivATI #-}
ptr_glVertexStream3ivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glVertexStream3ivATI = unsafePerformIO $ getCommand "glVertexStream3ivATI"

-- glVertexStream3sATI ---------------------------------------------------------

glVertexStream3sATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> m ()
glVertexStream3sATI v1 v2 v3 v4 = liftIO $ dyn583 ptr_glVertexStream3sATI v1 v2 v3 v4

{-# NOINLINE ptr_glVertexStream3sATI #-}
ptr_glVertexStream3sATI :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexStream3sATI = unsafePerformIO $ getCommand "glVertexStream3sATI"

-- glVertexStream3svATI --------------------------------------------------------

glVertexStream3svATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLshort -- ^ @coords@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glVertexStream3svATI v1 v2 = liftIO $ dyn573 ptr_glVertexStream3svATI v1 v2

{-# NOINLINE ptr_glVertexStream3svATI #-}
ptr_glVertexStream3svATI :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glVertexStream3svATI = unsafePerformIO $ getCommand "glVertexStream3svATI"

-- glVertexStream4dATI ---------------------------------------------------------

glVertexStream4dATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexStream4dATI v1 v2 v3 v4 v5 = liftIO $ dyn546 ptr_glVertexStream4dATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexStream4dATI #-}
ptr_glVertexStream4dATI :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexStream4dATI = unsafePerformIO $ getCommand "glVertexStream4dATI"

-- glVertexStream4dvATI --------------------------------------------------------

glVertexStream4dvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLdouble -- ^ @coords@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexStream4dvATI v1 v2 = liftIO $ dyn100 ptr_glVertexStream4dvATI v1 v2

{-# NOINLINE ptr_glVertexStream4dvATI #-}
ptr_glVertexStream4dvATI :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glVertexStream4dvATI = unsafePerformIO $ getCommand "glVertexStream4dvATI"

-- glVertexStream4fATI ---------------------------------------------------------

glVertexStream4fATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glVertexStream4fATI v1 v2 v3 v4 v5 = liftIO $ dyn547 ptr_glVertexStream4fATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexStream4fATI #-}
ptr_glVertexStream4fATI :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexStream4fATI = unsafePerformIO $ getCommand "glVertexStream4fATI"

-- glVertexStream4fvATI --------------------------------------------------------

glVertexStream4fvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLfloat -- ^ @coords@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glVertexStream4fvATI v1 v2 = liftIO $ dyn101 ptr_glVertexStream4fvATI v1 v2

{-# NOINLINE ptr_glVertexStream4fvATI #-}
ptr_glVertexStream4fvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glVertexStream4fvATI = unsafePerformIO $ getCommand "glVertexStream4fvATI"

-- glVertexStream4iATI ---------------------------------------------------------

glVertexStream4iATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @w@.
  -> m ()
glVertexStream4iATI v1 v2 v3 v4 v5 = liftIO $ dyn276 ptr_glVertexStream4iATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexStream4iATI #-}
ptr_glVertexStream4iATI :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexStream4iATI = unsafePerformIO $ getCommand "glVertexStream4iATI"

-- glVertexStream4ivATI --------------------------------------------------------

glVertexStream4ivATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLint -- ^ @coords@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexStream4ivATI v1 v2 = liftIO $ dyn143 ptr_glVertexStream4ivATI v1 v2

{-# NOINLINE ptr_glVertexStream4ivATI #-}
ptr_glVertexStream4ivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glVertexStream4ivATI = unsafePerformIO $ getCommand "glVertexStream4ivATI"

-- glVertexStream4sATI ---------------------------------------------------------

glVertexStream4sATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> GLshort -- ^ @w@.
  -> m ()
glVertexStream4sATI v1 v2 v3 v4 v5 = liftIO $ dyn587 ptr_glVertexStream4sATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexStream4sATI #-}
ptr_glVertexStream4sATI :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexStream4sATI = unsafePerformIO $ getCommand "glVertexStream4sATI"

-- glVertexStream4svATI --------------------------------------------------------

glVertexStream4svATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type [VertexStreamATI](Graphics-GL-Groups.html#VertexStreamATI).
  -> Ptr GLshort -- ^ @coords@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexStream4svATI v1 v2 = liftIO $ dyn573 ptr_glVertexStream4svATI v1 v2

{-# NOINLINE ptr_glVertexStream4svATI #-}
ptr_glVertexStream4svATI :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glVertexStream4svATI = unsafePerformIO $ getCommand "glVertexStream4svATI"

-- glVertexWeightPointerEXT ----------------------------------------------------

glVertexWeightPointerEXT
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexWeightPointerTypeEXT](Graphics-GL-Groups.html#VertexWeightPointerTypeEXT).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glVertexWeightPointerEXT v1 v2 v3 v4 = liftIO $ dyn133 ptr_glVertexWeightPointerEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVertexWeightPointerEXT #-}
ptr_glVertexWeightPointerEXT :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexWeightPointerEXT = unsafePerformIO $ getCommand "glVertexWeightPointerEXT"

-- glVertexWeightfEXT ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexWeightfvEXT'.
glVertexWeightfEXT
  :: MonadIO m
  => GLfloat -- ^ @weight@.
  -> m ()
glVertexWeightfEXT v1 = liftIO $ dyn85 ptr_glVertexWeightfEXT v1

{-# NOINLINE ptr_glVertexWeightfEXT #-}
ptr_glVertexWeightfEXT :: FunPtr (GLfloat -> IO ())
ptr_glVertexWeightfEXT = unsafePerformIO $ getCommand "glVertexWeightfEXT"

-- glVertexWeightfvEXT ---------------------------------------------------------

glVertexWeightfvEXT
  :: MonadIO m
  => Ptr GLfloat -- ^ @weight@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glVertexWeightfvEXT v1 = liftIO $ dyn44 ptr_glVertexWeightfvEXT v1

{-# NOINLINE ptr_glVertexWeightfvEXT #-}
ptr_glVertexWeightfvEXT :: FunPtr (Ptr GLfloat -> IO ())
ptr_glVertexWeightfvEXT = unsafePerformIO $ getCommand "glVertexWeightfvEXT"

-- glVertexWeighthNV -----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexWeighthvNV'.
glVertexWeighthNV
  :: MonadIO m
  => GLhalfNV -- ^ @weight@ of type @Half16NV@.
  -> m ()
glVertexWeighthNV v1 = liftIO $ dyn292 ptr_glVertexWeighthNV v1

{-# NOINLINE ptr_glVertexWeighthNV #-}
ptr_glVertexWeighthNV :: FunPtr (GLhalfNV -> IO ())
ptr_glVertexWeighthNV = unsafePerformIO $ getCommand "glVertexWeighthNV"

-- glVertexWeighthvNV ----------------------------------------------------------

glVertexWeighthvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @weight@ pointing to @1@ element of type @Half16NV@.
  -> m ()
glVertexWeighthvNV v1 = liftIO $ dyn106 ptr_glVertexWeighthvNV v1

{-# NOINLINE ptr_glVertexWeighthvNV #-}
ptr_glVertexWeighthvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glVertexWeighthvNV = unsafePerformIO $ getCommand "glVertexWeighthvNV"

-- glVideoCaptureNV ------------------------------------------------------------

glVideoCaptureNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> Ptr GLuint -- ^ @sequence_num@.
  -> Ptr GLuint64EXT -- ^ @capture_time@.
  -> m GLenum
glVideoCaptureNV v1 v2 v3 = liftIO $ dyn933 ptr_glVideoCaptureNV v1 v2 v3

{-# NOINLINE ptr_glVideoCaptureNV #-}
ptr_glVideoCaptureNV :: FunPtr (GLuint -> Ptr GLuint -> Ptr GLuint64EXT -> IO GLenum)
ptr_glVideoCaptureNV = unsafePerformIO $ getCommand "glVideoCaptureNV"

-- glVideoCaptureStreamParameterdvNV -------------------------------------------

glVideoCaptureStreamParameterdvNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glVideoCaptureStreamParameterdvNV v1 v2 v3 v4 = liftIO $ dyn465 ptr_glVideoCaptureStreamParameterdvNV v1 v2 v3 v4

{-# NOINLINE ptr_glVideoCaptureStreamParameterdvNV #-}
ptr_glVideoCaptureStreamParameterdvNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glVideoCaptureStreamParameterdvNV = unsafePerformIO $ getCommand "glVideoCaptureStreamParameterdvNV"

-- glVideoCaptureStreamParameterfvNV -------------------------------------------

glVideoCaptureStreamParameterfvNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glVideoCaptureStreamParameterfvNV v1 v2 v3 v4 = liftIO $ dyn466 ptr_glVideoCaptureStreamParameterfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glVideoCaptureStreamParameterfvNV #-}
ptr_glVideoCaptureStreamParameterfvNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glVideoCaptureStreamParameterfvNV = unsafePerformIO $ getCommand "glVideoCaptureStreamParameterfvNV"

-- glVideoCaptureStreamParameterivNV -------------------------------------------

glVideoCaptureStreamParameterivNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glVideoCaptureStreamParameterivNV v1 v2 v3 v4 = liftIO $ dyn314 ptr_glVideoCaptureStreamParameterivNV v1 v2 v3 v4

{-# NOINLINE ptr_glVideoCaptureStreamParameterivNV #-}
ptr_glVideoCaptureStreamParameterivNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glVideoCaptureStreamParameterivNV = unsafePerformIO $ getCommand "glVideoCaptureStreamParameterivNV"

-- glViewport ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glViewport.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glViewport.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glViewport.xhtml OpenGL 4.x>.
glViewport
  :: MonadIO m
  => GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glViewport v1 v2 v3 v4 = liftIO $ dyn738 ptr_glViewport v1 v2 v3 v4

{-# NOINLINE ptr_glViewport #-}
ptr_glViewport :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glViewport = unsafePerformIO $ getCommand "glViewport"

-- glViewportArrayv ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glViewportArray.xhtml OpenGL 4.x>.
glViewportArrayv
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @COMPSIZE(count)@ elements of type @GLfloat@.
  -> m ()
glViewportArrayv v1 v2 v3 = liftIO $ dyn226 ptr_glViewportArrayv v1 v2 v3

{-# NOINLINE ptr_glViewportArrayv #-}
ptr_glViewportArrayv :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glViewportArrayv = unsafePerformIO $ getCommand "glViewportArrayv"

-- glViewportArrayvNV ----------------------------------------------------------

-- | This command is an alias for 'glViewportArrayv'.
glViewportArrayvNV
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @COMPSIZE(count)@ elements of type @GLfloat@.
  -> m ()
glViewportArrayvNV v1 v2 v3 = liftIO $ dyn226 ptr_glViewportArrayvNV v1 v2 v3

{-# NOINLINE ptr_glViewportArrayvNV #-}
ptr_glViewportArrayvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glViewportArrayvNV = unsafePerformIO $ getCommand "glViewportArrayvNV"

-- glViewportArrayvOES ---------------------------------------------------------

-- | This command is an alias for 'glViewportArrayv'.
glViewportArrayvOES
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @COMPSIZE(count)@ elements of type @GLfloat@.
  -> m ()
glViewportArrayvOES v1 v2 v3 = liftIO $ dyn226 ptr_glViewportArrayvOES v1 v2 v3

{-# NOINLINE ptr_glViewportArrayvOES #-}
ptr_glViewportArrayvOES :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glViewportArrayvOES = unsafePerformIO $ getCommand "glViewportArrayvOES"

-- glViewportIndexedf ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glViewportIndexed.xhtml OpenGL 4.x>.
glViewportIndexedf
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @w@.
  -> GLfloat -- ^ @h@.
  -> m ()
glViewportIndexedf v1 v2 v3 v4 v5 = liftIO $ dyn907 ptr_glViewportIndexedf v1 v2 v3 v4 v5

{-# NOINLINE ptr_glViewportIndexedf #-}
ptr_glViewportIndexedf :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glViewportIndexedf = unsafePerformIO $ getCommand "glViewportIndexedf"

-- glViewportIndexedfNV --------------------------------------------------------

-- | This command is an alias for 'glViewportIndexedf'.
glViewportIndexedfNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @w@.
  -> GLfloat -- ^ @h@.
  -> m ()
glViewportIndexedfNV v1 v2 v3 v4 v5 = liftIO $ dyn907 ptr_glViewportIndexedfNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glViewportIndexedfNV #-}
ptr_glViewportIndexedfNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glViewportIndexedfNV = unsafePerformIO $ getCommand "glViewportIndexedfNV"

-- glViewportIndexedfOES -------------------------------------------------------

-- | This command is an alias for 'glViewportIndexedf'.
glViewportIndexedfOES
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @w@.
  -> GLfloat -- ^ @h@.
  -> m ()
glViewportIndexedfOES v1 v2 v3 v4 v5 = liftIO $ dyn907 ptr_glViewportIndexedfOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glViewportIndexedfOES #-}
ptr_glViewportIndexedfOES :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glViewportIndexedfOES = unsafePerformIO $ getCommand "glViewportIndexedfOES"

-- glViewportIndexedfv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glViewportIndexed.xhtml OpenGL 4.x>.
glViewportIndexedfv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glViewportIndexedfv v1 v2 = liftIO $ dyn394 ptr_glViewportIndexedfv v1 v2

{-# NOINLINE ptr_glViewportIndexedfv #-}
ptr_glViewportIndexedfv :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glViewportIndexedfv = unsafePerformIO $ getCommand "glViewportIndexedfv"

-- glViewportIndexedfvNV -------------------------------------------------------

-- | This command is an alias for 'glViewportIndexedfv'.
glViewportIndexedfvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glViewportIndexedfvNV v1 v2 = liftIO $ dyn394 ptr_glViewportIndexedfvNV v1 v2

{-# NOINLINE ptr_glViewportIndexedfvNV #-}
ptr_glViewportIndexedfvNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glViewportIndexedfvNV = unsafePerformIO $ getCommand "glViewportIndexedfvNV"

-- glViewportIndexedfvOES ------------------------------------------------------

-- | This command is an alias for 'glViewportIndexedfv'.
glViewportIndexedfvOES
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glViewportIndexedfvOES v1 v2 = liftIO $ dyn394 ptr_glViewportIndexedfvOES v1 v2

{-# NOINLINE ptr_glViewportIndexedfvOES #-}
ptr_glViewportIndexedfvOES :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glViewportIndexedfvOES = unsafePerformIO $ getCommand "glViewportIndexedfvOES"

-- glViewportPositionWScaleNV --------------------------------------------------

glViewportPositionWScaleNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @xcoeff@.
  -> GLfloat -- ^ @ycoeff@.
  -> m ()
glViewportPositionWScaleNV v1 v2 v3 = liftIO $ dyn229 ptr_glViewportPositionWScaleNV v1 v2 v3

{-# NOINLINE ptr_glViewportPositionWScaleNV #-}
ptr_glViewportPositionWScaleNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
ptr_glViewportPositionWScaleNV = unsafePerformIO $ getCommand "glViewportPositionWScaleNV"

-- glViewportSwizzleNV ---------------------------------------------------------

glViewportSwizzleNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @swizzlex@.
  -> GLenum -- ^ @swizzley@.
  -> GLenum -- ^ @swizzlez@.
  -> GLenum -- ^ @swizzlew@.
  -> m ()
glViewportSwizzleNV v1 v2 v3 v4 v5 = liftIO $ dyn57 ptr_glViewportSwizzleNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glViewportSwizzleNV #-}
ptr_glViewportSwizzleNV :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glViewportSwizzleNV = unsafePerformIO $ getCommand "glViewportSwizzleNV"

-- glWaitSemaphoreEXT ----------------------------------------------------------

glWaitSemaphoreEXT
  :: MonadIO m
  => GLuint -- ^ @semaphore@.
  -> GLuint -- ^ @numBufferBarriers@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @COMPSIZE(numBufferBarriers)@ elements of type @GLuint@.
  -> GLuint -- ^ @numTextureBarriers@.
  -> Ptr GLuint -- ^ @textures@ pointing to @COMPSIZE(numTextureBarriers)@ elements of type @GLuint@.
  -> Ptr GLenum -- ^ @srcLayouts@ pointing to @COMPSIZE(numTextureBarriers)@ elements of type [TextureLayout](Graphics-GL-Groups.html#TextureLayout).
  -> m ()
glWaitSemaphoreEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn749 ptr_glWaitSemaphoreEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glWaitSemaphoreEXT #-}
ptr_glWaitSemaphoreEXT :: FunPtr (GLuint -> GLuint -> Ptr GLuint -> GLuint -> Ptr GLuint -> Ptr GLenum -> IO ())
ptr_glWaitSemaphoreEXT = unsafePerformIO $ getCommand "glWaitSemaphoreEXT"

-- glWaitSemaphoreui64NVX ------------------------------------------------------

glWaitSemaphoreui64NVX
  :: MonadIO m
  => GLuint -- ^ @waitGpu@.
  -> GLsizei -- ^ @fenceObjectCount@.
  -> Ptr GLuint -- ^ @semaphoreArray@ pointing to @fenceObjectCount@ elements of type @GLuint@.
  -> Ptr GLuint64 -- ^ @fenceValueArray@ pointing to @fenceObjectCount@ elements of type @GLuint64@.
  -> m ()
glWaitSemaphoreui64NVX v1 v2 v3 v4 = liftIO $ dyn750 ptr_glWaitSemaphoreui64NVX v1 v2 v3 v4

{-# NOINLINE ptr_glWaitSemaphoreui64NVX #-}
ptr_glWaitSemaphoreui64NVX :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> Ptr GLuint64 -> IO ())
ptr_glWaitSemaphoreui64NVX = unsafePerformIO $ getCommand "glWaitSemaphoreui64NVX"

