{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F06
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

module Graphics.GL.Functions.F06 (
  glDeleteVertexArraysOES,
  glDeleteVertexShaderEXT,
  glDepthBoundsEXT,
  glDepthBoundsdNV,
  glDepthFunc,
  glDepthMask,
  glDepthRange,
  glDepthRangeArrayfvNV,
  glDepthRangeArrayfvOES,
  glDepthRangeArrayv,
  glDepthRangeIndexed,
  glDepthRangeIndexedfNV,
  glDepthRangeIndexedfOES,
  glDepthRangedNV,
  glDepthRangef,
  glDepthRangefOES,
  glDepthRangex,
  glDepthRangexOES,
  glDetachObjectARB,
  glDetachShader,
  glDetailTexFuncSGIS,
  glDisable,
  glDisableClientState,
  glDisableClientStateIndexedEXT,
  glDisableClientStateiEXT,
  glDisableDriverControlQCOM,
  glDisableIndexedEXT,
  glDisableVariantClientStateEXT,
  glDisableVertexArrayAttrib,
  glDisableVertexArrayAttribEXT,
  glDisableVertexArrayEXT,
  glDisableVertexAttribAPPLE,
  glDisableVertexAttribArray,
  glDisableVertexAttribArrayARB,
  glDisablei,
  glDisableiEXT,
  glDisableiNV,
  glDisableiOES,
  glDiscardFramebufferEXT,
  glDispatchCompute,
  glDispatchComputeGroupSizeARB,
  glDispatchComputeIndirect,
  glDrawArrays,
  glDrawArraysEXT,
  glDrawArraysIndirect,
  glDrawArraysInstanced,
  glDrawArraysInstancedANGLE,
  glDrawArraysInstancedARB,
  glDrawArraysInstancedBaseInstance,
  glDrawArraysInstancedBaseInstanceEXT,
  glDrawArraysInstancedEXT,
  glDrawArraysInstancedNV,
  glDrawBuffer,
  glDrawBuffers,
  glDrawBuffersARB,
  glDrawBuffersATI,
  glDrawBuffersEXT,
  glDrawBuffersIndexedEXT,
  glDrawBuffersNV,
  glDrawCommandsAddressNV,
  glDrawCommandsNV,
  glDrawCommandsStatesAddressNV,
  glDrawCommandsStatesNV,
  glDrawElementArrayAPPLE,
  glDrawElementArrayATI,
  glDrawElements,
  glDrawElementsBaseVertex,
  glDrawElementsBaseVertexEXT,
  glDrawElementsBaseVertexOES,
  glDrawElementsIndirect,
  glDrawElementsInstanced,
  glDrawElementsInstancedANGLE,
  glDrawElementsInstancedARB,
  glDrawElementsInstancedBaseInstance,
  glDrawElementsInstancedBaseInstanceEXT,
  glDrawElementsInstancedBaseVertex,
  glDrawElementsInstancedBaseVertexBaseInstance,
  glDrawElementsInstancedBaseVertexBaseInstanceEXT,
  glDrawElementsInstancedBaseVertexEXT,
  glDrawElementsInstancedBaseVertexOES,
  glDrawElementsInstancedEXT,
  glDrawElementsInstancedNV,
  glDrawMeshArraysSUN,
  glDrawMeshTasksIndirectNV,
  glDrawMeshTasksNV,
  glDrawPixels,
  glDrawRangeElementArrayAPPLE,
  glDrawRangeElementArrayATI,
  glDrawRangeElements,
  glDrawRangeElementsBaseVertex,
  glDrawRangeElementsBaseVertexEXT,
  glDrawRangeElementsBaseVertexOES,
  glDrawRangeElementsEXT,
  glDrawTexfOES,
  glDrawTexfvOES,
  glDrawTexiOES,
  glDrawTexivOES,
  glDrawTexsOES,
  glDrawTexsvOES,
  glDrawTextureNV
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Ptr
import Graphics.GL.Foreign
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )

-- glDeleteVertexArraysOES -----------------------------------------------------

-- | This command is an alias for 'glDeleteVertexArrays'.
glDeleteVertexArraysOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteVertexArraysOES v1 v2 = liftIO $ dyn200 ptr_glDeleteVertexArraysOES v1 v2

{-# NOINLINE ptr_glDeleteVertexArraysOES #-}
ptr_glDeleteVertexArraysOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteVertexArraysOES = unsafePerformIO $ getCommand "glDeleteVertexArraysOES"

-- glDeleteVertexShaderEXT -----------------------------------------------------

glDeleteVertexShaderEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glDeleteVertexShaderEXT v1 = liftIO $ dyn3 ptr_glDeleteVertexShaderEXT v1

{-# NOINLINE ptr_glDeleteVertexShaderEXT #-}
ptr_glDeleteVertexShaderEXT :: FunPtr (GLuint -> IO ())
ptr_glDeleteVertexShaderEXT = unsafePerformIO $ getCommand "glDeleteVertexShaderEXT"

-- glDepthBoundsEXT ------------------------------------------------------------

glDepthBoundsEXT
  :: MonadIO m
  => GLclampd -- ^ @zmin@ of type @ClampedFloat64@.
  -> GLclampd -- ^ @zmax@ of type @ClampedFloat64@.
  -> m ()
glDepthBoundsEXT v1 v2 = liftIO $ dyn224 ptr_glDepthBoundsEXT v1 v2

{-# NOINLINE ptr_glDepthBoundsEXT #-}
ptr_glDepthBoundsEXT :: FunPtr (GLclampd -> GLclampd -> IO ())
ptr_glDepthBoundsEXT = unsafePerformIO $ getCommand "glDepthBoundsEXT"

-- glDepthBoundsdNV ------------------------------------------------------------

glDepthBoundsdNV
  :: MonadIO m
  => GLdouble -- ^ @zmin@.
  -> GLdouble -- ^ @zmax@.
  -> m ()
glDepthBoundsdNV v1 v2 = liftIO $ dyn225 ptr_glDepthBoundsdNV v1 v2

{-# NOINLINE ptr_glDepthBoundsdNV #-}
ptr_glDepthBoundsdNV :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glDepthBoundsdNV = unsafePerformIO $ getCommand "glDepthBoundsdNV"

-- glDepthFunc -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDepthFunc.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDepthFunc.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDepthFunc.xhtml OpenGL 4.x>.
glDepthFunc
  :: MonadIO m
  => GLenum -- ^ @func@ of type [DepthFunction](Graphics-GL-Groups.html#DepthFunction).
  -> m ()
glDepthFunc v1 = liftIO $ dyn5 ptr_glDepthFunc v1

{-# NOINLINE ptr_glDepthFunc #-}
ptr_glDepthFunc :: FunPtr (GLenum -> IO ())
ptr_glDepthFunc = unsafePerformIO $ getCommand "glDepthFunc"

-- glDepthMask -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDepthMask.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDepthMask.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDepthMask.xhtml OpenGL 4.x>.
glDepthMask
  :: MonadIO m
  => GLboolean -- ^ @flag@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glDepthMask v1 = liftIO $ dyn198 ptr_glDepthMask v1

{-# NOINLINE ptr_glDepthMask #-}
ptr_glDepthMask :: FunPtr (GLboolean -> IO ())
ptr_glDepthMask = unsafePerformIO $ getCommand "glDepthMask"

-- glDepthRange ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDepthRange.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDepthRange.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDepthRange.xhtml OpenGL 4.x>.
glDepthRange
  :: MonadIO m
  => GLdouble -- ^ @n@.
  -> GLdouble -- ^ @f@.
  -> m ()
glDepthRange v1 v2 = liftIO $ dyn225 ptr_glDepthRange v1 v2

{-# NOINLINE ptr_glDepthRange #-}
ptr_glDepthRange :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glDepthRange = unsafePerformIO $ getCommand "glDepthRange"

-- glDepthRangeArrayfvNV -------------------------------------------------------

glDepthRangeArrayfvNV
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glDepthRangeArrayfvNV v1 v2 v3 = liftIO $ dyn226 ptr_glDepthRangeArrayfvNV v1 v2 v3

{-# NOINLINE ptr_glDepthRangeArrayfvNV #-}
ptr_glDepthRangeArrayfvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glDepthRangeArrayfvNV = unsafePerformIO $ getCommand "glDepthRangeArrayfvNV"

-- glDepthRangeArrayfvOES ------------------------------------------------------

glDepthRangeArrayfvOES
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glDepthRangeArrayfvOES v1 v2 v3 = liftIO $ dyn226 ptr_glDepthRangeArrayfvOES v1 v2 v3

{-# NOINLINE ptr_glDepthRangeArrayfvOES #-}
ptr_glDepthRangeArrayfvOES :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glDepthRangeArrayfvOES = unsafePerformIO $ getCommand "glDepthRangeArrayfvOES"

-- glDepthRangeArrayv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDepthRangeArray.xhtml OpenGL 4.x>.
glDepthRangeArrayv
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @COMPSIZE(count)@ elements of type @GLdouble@.
  -> m ()
glDepthRangeArrayv v1 v2 v3 = liftIO $ dyn227 ptr_glDepthRangeArrayv v1 v2 v3

{-# NOINLINE ptr_glDepthRangeArrayv #-}
ptr_glDepthRangeArrayv :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glDepthRangeArrayv = unsafePerformIO $ getCommand "glDepthRangeArrayv"

-- glDepthRangeIndexed ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDepthRangeIndexed.xhtml OpenGL 4.x>.
glDepthRangeIndexed
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @n@.
  -> GLdouble -- ^ @f@.
  -> m ()
glDepthRangeIndexed v1 v2 v3 = liftIO $ dyn228 ptr_glDepthRangeIndexed v1 v2 v3

{-# NOINLINE ptr_glDepthRangeIndexed #-}
ptr_glDepthRangeIndexed :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glDepthRangeIndexed = unsafePerformIO $ getCommand "glDepthRangeIndexed"

-- glDepthRangeIndexedfNV ------------------------------------------------------

glDepthRangeIndexedfNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glDepthRangeIndexedfNV v1 v2 v3 = liftIO $ dyn229 ptr_glDepthRangeIndexedfNV v1 v2 v3

{-# NOINLINE ptr_glDepthRangeIndexedfNV #-}
ptr_glDepthRangeIndexedfNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
ptr_glDepthRangeIndexedfNV = unsafePerformIO $ getCommand "glDepthRangeIndexedfNV"

-- glDepthRangeIndexedfOES -----------------------------------------------------

glDepthRangeIndexedfOES
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glDepthRangeIndexedfOES v1 v2 v3 = liftIO $ dyn229 ptr_glDepthRangeIndexedfOES v1 v2 v3

{-# NOINLINE ptr_glDepthRangeIndexedfOES #-}
ptr_glDepthRangeIndexedfOES :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
ptr_glDepthRangeIndexedfOES = unsafePerformIO $ getCommand "glDepthRangeIndexedfOES"

-- glDepthRangedNV -------------------------------------------------------------

glDepthRangedNV
  :: MonadIO m
  => GLdouble -- ^ @zNear@.
  -> GLdouble -- ^ @zFar@.
  -> m ()
glDepthRangedNV v1 v2 = liftIO $ dyn225 ptr_glDepthRangedNV v1 v2

{-# NOINLINE ptr_glDepthRangedNV #-}
ptr_glDepthRangedNV :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glDepthRangedNV = unsafePerformIO $ getCommand "glDepthRangedNV"

-- glDepthRangef ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDepthRange.xhtml OpenGL 4.x>.
glDepthRangef
  :: MonadIO m
  => GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glDepthRangef v1 v2 = liftIO $ dyn230 ptr_glDepthRangef v1 v2

{-# NOINLINE ptr_glDepthRangef #-}
ptr_glDepthRangef :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glDepthRangef = unsafePerformIO $ getCommand "glDepthRangef"

-- glDepthRangefOES ------------------------------------------------------------

-- | This command is an alias for 'glDepthRangef'.
glDepthRangefOES
  :: MonadIO m
  => GLclampf -- ^ @n@ of type @ClampedFloat32@.
  -> GLclampf -- ^ @f@ of type @ClampedFloat32@.
  -> m ()
glDepthRangefOES v1 v2 = liftIO $ dyn231 ptr_glDepthRangefOES v1 v2

{-# NOINLINE ptr_glDepthRangefOES #-}
ptr_glDepthRangefOES :: FunPtr (GLclampf -> GLclampf -> IO ())
ptr_glDepthRangefOES = unsafePerformIO $ getCommand "glDepthRangefOES"

-- glDepthRangex ---------------------------------------------------------------

glDepthRangex
  :: MonadIO m
  => GLfixed -- ^ @n@.
  -> GLfixed -- ^ @f@.
  -> m ()
glDepthRangex v1 v2 = liftIO $ dyn232 ptr_glDepthRangex v1 v2

{-# NOINLINE ptr_glDepthRangex #-}
ptr_glDepthRangex :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glDepthRangex = unsafePerformIO $ getCommand "glDepthRangex"

-- glDepthRangexOES ------------------------------------------------------------

glDepthRangexOES
  :: MonadIO m
  => GLfixed -- ^ @n@ of type @ClampedFixed@.
  -> GLfixed -- ^ @f@ of type @ClampedFixed@.
  -> m ()
glDepthRangexOES v1 v2 = liftIO $ dyn232 ptr_glDepthRangexOES v1 v2

{-# NOINLINE ptr_glDepthRangexOES #-}
ptr_glDepthRangexOES :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glDepthRangexOES = unsafePerformIO $ getCommand "glDepthRangexOES"

-- glDetachObjectARB -----------------------------------------------------------

-- | This command is an alias for 'glDetachShader'.
glDetachObjectARB
  :: MonadIO m
  => GLhandleARB -- ^ @containerObj@ of type @handleARB@.
  -> GLhandleARB -- ^ @attachedObj@ of type @handleARB@.
  -> m ()
glDetachObjectARB v1 v2 = liftIO $ dyn17 ptr_glDetachObjectARB v1 v2

{-# NOINLINE ptr_glDetachObjectARB #-}
ptr_glDetachObjectARB :: FunPtr (GLhandleARB -> GLhandleARB -> IO ())
ptr_glDetachObjectARB = unsafePerformIO $ getCommand "glDetachObjectARB"

-- glDetachShader --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDetachShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDetachShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDetachShader.xhtml OpenGL 4.x>.
glDetachShader
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @shader@.
  -> m ()
glDetachShader v1 v2 = liftIO $ dyn4 ptr_glDetachShader v1 v2

{-# NOINLINE ptr_glDetachShader #-}
ptr_glDetachShader :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glDetachShader = unsafePerformIO $ getCommand "glDetachShader"

-- glDetailTexFuncSGIS ---------------------------------------------------------

glDetailTexFuncSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @n@.
  -> Ptr GLfloat -- ^ @points@ pointing to @n*2@ elements of type @GLfloat@.
  -> m ()
glDetailTexFuncSGIS v1 v2 v3 = liftIO $ dyn233 ptr_glDetailTexFuncSGIS v1 v2 v3

{-# NOINLINE ptr_glDetailTexFuncSGIS #-}
ptr_glDetailTexFuncSGIS :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glDetailTexFuncSGIS = unsafePerformIO $ getCommand "glDetailTexFuncSGIS"

-- glDisable -------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnable.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml OpenGL 4.x>.
glDisable
  :: MonadIO m
  => GLenum -- ^ @cap@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glDisable v1 = liftIO $ dyn5 ptr_glDisable v1

{-# NOINLINE ptr_glDisable #-}
ptr_glDisable :: FunPtr (GLenum -> IO ())
ptr_glDisable = unsafePerformIO $ getCommand "glDisable"

-- glDisableClientState --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnableClientState.xml OpenGL 2.x>.
glDisableClientState
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glDisableClientState v1 = liftIO $ dyn5 ptr_glDisableClientState v1

{-# NOINLINE ptr_glDisableClientState #-}
ptr_glDisableClientState :: FunPtr (GLenum -> IO ())
ptr_glDisableClientState = unsafePerformIO $ getCommand "glDisableClientState"

-- glDisableClientStateIndexedEXT ----------------------------------------------

glDisableClientStateIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glDisableClientStateIndexedEXT v1 v2 = liftIO $ dyn19 ptr_glDisableClientStateIndexedEXT v1 v2

{-# NOINLINE ptr_glDisableClientStateIndexedEXT #-}
ptr_glDisableClientStateIndexedEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableClientStateIndexedEXT = unsafePerformIO $ getCommand "glDisableClientStateIndexedEXT"

-- glDisableClientStateiEXT ----------------------------------------------------

glDisableClientStateiEXT
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glDisableClientStateiEXT v1 v2 = liftIO $ dyn19 ptr_glDisableClientStateiEXT v1 v2

{-# NOINLINE ptr_glDisableClientStateiEXT #-}
ptr_glDisableClientStateiEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableClientStateiEXT = unsafePerformIO $ getCommand "glDisableClientStateiEXT"

-- glDisableDriverControlQCOM --------------------------------------------------

glDisableDriverControlQCOM
  :: MonadIO m
  => GLuint -- ^ @driverControl@.
  -> m ()
glDisableDriverControlQCOM v1 = liftIO $ dyn3 ptr_glDisableDriverControlQCOM v1

{-# NOINLINE ptr_glDisableDriverControlQCOM #-}
ptr_glDisableDriverControlQCOM :: FunPtr (GLuint -> IO ())
ptr_glDisableDriverControlQCOM = unsafePerformIO $ getCommand "glDisableDriverControlQCOM"

-- glDisableIndexedEXT ---------------------------------------------------------

-- | This command is an alias for 'glDisablei'.
glDisableIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glDisableIndexedEXT v1 v2 = liftIO $ dyn19 ptr_glDisableIndexedEXT v1 v2

{-# NOINLINE ptr_glDisableIndexedEXT #-}
ptr_glDisableIndexedEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableIndexedEXT = unsafePerformIO $ getCommand "glDisableIndexedEXT"

-- glDisableVariantClientStateEXT ----------------------------------------------

glDisableVariantClientStateEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glDisableVariantClientStateEXT v1 = liftIO $ dyn3 ptr_glDisableVariantClientStateEXT v1

{-# NOINLINE ptr_glDisableVariantClientStateEXT #-}
ptr_glDisableVariantClientStateEXT :: FunPtr (GLuint -> IO ())
ptr_glDisableVariantClientStateEXT = unsafePerformIO $ getCommand "glDisableVariantClientStateEXT"

-- glDisableVertexArrayAttrib --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml OpenGL 4.x>.
glDisableVertexArrayAttrib
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> m ()
glDisableVertexArrayAttrib v1 v2 = liftIO $ dyn4 ptr_glDisableVertexArrayAttrib v1 v2

{-# NOINLINE ptr_glDisableVertexArrayAttrib #-}
ptr_glDisableVertexArrayAttrib :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glDisableVertexArrayAttrib = unsafePerformIO $ getCommand "glDisableVertexArrayAttrib"

-- glDisableVertexArrayAttribEXT -----------------------------------------------

glDisableVertexArrayAttribEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> m ()
glDisableVertexArrayAttribEXT v1 v2 = liftIO $ dyn4 ptr_glDisableVertexArrayAttribEXT v1 v2

{-# NOINLINE ptr_glDisableVertexArrayAttribEXT #-}
ptr_glDisableVertexArrayAttribEXT :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glDisableVertexArrayAttribEXT = unsafePerformIO $ getCommand "glDisableVertexArrayAttribEXT"

-- glDisableVertexArrayEXT -----------------------------------------------------

glDisableVertexArrayEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glDisableVertexArrayEXT v1 v2 = liftIO $ dyn18 ptr_glDisableVertexArrayEXT v1 v2

{-# NOINLINE ptr_glDisableVertexArrayEXT #-}
ptr_glDisableVertexArrayEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glDisableVertexArrayEXT = unsafePerformIO $ getCommand "glDisableVertexArrayEXT"

-- glDisableVertexAttribAPPLE --------------------------------------------------

glDisableVertexAttribAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> m ()
glDisableVertexAttribAPPLE v1 v2 = liftIO $ dyn18 ptr_glDisableVertexAttribAPPLE v1 v2

{-# NOINLINE ptr_glDisableVertexAttribAPPLE #-}
ptr_glDisableVertexAttribAPPLE :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glDisableVertexAttribAPPLE = unsafePerformIO $ getCommand "glDisableVertexAttribAPPLE"

-- glDisableVertexAttribArray --------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnableVertexAttribArray.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glEnableVertexAttribArray.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml OpenGL 4.x>.
glDisableVertexAttribArray
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> m ()
glDisableVertexAttribArray v1 = liftIO $ dyn3 ptr_glDisableVertexAttribArray v1

{-# NOINLINE ptr_glDisableVertexAttribArray #-}
ptr_glDisableVertexAttribArray :: FunPtr (GLuint -> IO ())
ptr_glDisableVertexAttribArray = unsafePerformIO $ getCommand "glDisableVertexAttribArray"

-- glDisableVertexAttribArrayARB -----------------------------------------------

-- | This command is an alias for 'glDisableVertexAttribArray'.
glDisableVertexAttribArrayARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> m ()
glDisableVertexAttribArrayARB v1 = liftIO $ dyn3 ptr_glDisableVertexAttribArrayARB v1

{-# NOINLINE ptr_glDisableVertexAttribArrayARB #-}
ptr_glDisableVertexAttribArrayARB :: FunPtr (GLuint -> IO ())
ptr_glDisableVertexAttribArrayARB = unsafePerformIO $ getCommand "glDisableVertexAttribArrayARB"

-- glDisablei ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml OpenGL 4.x>.
glDisablei
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glDisablei v1 v2 = liftIO $ dyn19 ptr_glDisablei v1 v2

{-# NOINLINE ptr_glDisablei #-}
ptr_glDisablei :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisablei = unsafePerformIO $ getCommand "glDisablei"

-- glDisableiEXT ---------------------------------------------------------------

-- | This command is an alias for 'glDisablei'.
glDisableiEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glDisableiEXT v1 v2 = liftIO $ dyn19 ptr_glDisableiEXT v1 v2

{-# NOINLINE ptr_glDisableiEXT #-}
ptr_glDisableiEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableiEXT = unsafePerformIO $ getCommand "glDisableiEXT"

-- glDisableiNV ----------------------------------------------------------------

-- | This command is an alias for 'glDisablei'.
glDisableiNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glDisableiNV v1 v2 = liftIO $ dyn19 ptr_glDisableiNV v1 v2

{-# NOINLINE ptr_glDisableiNV #-}
ptr_glDisableiNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableiNV = unsafePerformIO $ getCommand "glDisableiNV"

-- glDisableiOES ---------------------------------------------------------------

-- | This command is an alias for 'glDisablei'.
glDisableiOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glDisableiOES v1 v2 = liftIO $ dyn19 ptr_glDisableiOES v1 v2

{-# NOINLINE ptr_glDisableiOES #-}
ptr_glDisableiOES :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableiOES = unsafePerformIO $ getCommand "glDisableiOES"

-- glDiscardFramebufferEXT -----------------------------------------------------

glDiscardFramebufferEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FramebufferTarget](Graphics-GL-Groups.html#FramebufferTarget).
  -> GLsizei -- ^ @numAttachments@.
  -> Ptr GLenum -- ^ @attachments@ pointing to @numAttachments@ elements of type [InvalidateFramebufferAttachment](Graphics-GL-Groups.html#InvalidateFramebufferAttachment).
  -> m ()
glDiscardFramebufferEXT v1 v2 v3 = liftIO $ dyn234 ptr_glDiscardFramebufferEXT v1 v2 v3

{-# NOINLINE ptr_glDiscardFramebufferEXT #-}
ptr_glDiscardFramebufferEXT :: FunPtr (GLenum -> GLsizei -> Ptr GLenum -> IO ())
ptr_glDiscardFramebufferEXT = unsafePerformIO $ getCommand "glDiscardFramebufferEXT"

-- glDispatchCompute -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDispatchCompute.xhtml OpenGL 4.x>.
glDispatchCompute
  :: MonadIO m
  => GLuint -- ^ @num_groups_x@.
  -> GLuint -- ^ @num_groups_y@.
  -> GLuint -- ^ @num_groups_z@.
  -> m ()
glDispatchCompute v1 v2 v3 = liftIO $ dyn109 ptr_glDispatchCompute v1 v2 v3

{-# NOINLINE ptr_glDispatchCompute #-}
ptr_glDispatchCompute :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glDispatchCompute = unsafePerformIO $ getCommand "glDispatchCompute"

-- glDispatchComputeGroupSizeARB -----------------------------------------------

glDispatchComputeGroupSizeARB
  :: MonadIO m
  => GLuint -- ^ @num_groups_x@.
  -> GLuint -- ^ @num_groups_y@.
  -> GLuint -- ^ @num_groups_z@.
  -> GLuint -- ^ @group_size_x@.
  -> GLuint -- ^ @group_size_y@.
  -> GLuint -- ^ @group_size_z@.
  -> m ()
glDispatchComputeGroupSizeARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn235 ptr_glDispatchComputeGroupSizeARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDispatchComputeGroupSizeARB #-}
ptr_glDispatchComputeGroupSizeARB :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glDispatchComputeGroupSizeARB = unsafePerformIO $ getCommand "glDispatchComputeGroupSizeARB"

-- glDispatchComputeIndirect ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDispatchComputeIndirect.xhtml OpenGL 4.x>.
glDispatchComputeIndirect
  :: MonadIO m
  => GLintptr -- ^ @indirect@ of type @BufferOffset@.
  -> m ()
glDispatchComputeIndirect v1 = liftIO $ dyn236 ptr_glDispatchComputeIndirect v1

{-# NOINLINE ptr_glDispatchComputeIndirect #-}
ptr_glDispatchComputeIndirect :: FunPtr (GLintptr -> IO ())
ptr_glDispatchComputeIndirect = unsafePerformIO $ getCommand "glDispatchComputeIndirect"

-- glDrawArrays ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawArrays.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawArrays.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawArrays.xhtml OpenGL 4.x>.
glDrawArrays
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawArrays v1 v2 v3 = liftIO $ dyn237 ptr_glDrawArrays v1 v2 v3

{-# NOINLINE ptr_glDrawArrays #-}
ptr_glDrawArrays :: FunPtr (GLenum -> GLint -> GLsizei -> IO ())
ptr_glDrawArrays = unsafePerformIO $ getCommand "glDrawArrays"

-- glDrawArraysEXT -------------------------------------------------------------

-- | This command is an alias for 'glDrawArrays'.
glDrawArraysEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawArraysEXT v1 v2 v3 = liftIO $ dyn237 ptr_glDrawArraysEXT v1 v2 v3

{-# NOINLINE ptr_glDrawArraysEXT #-}
ptr_glDrawArraysEXT :: FunPtr (GLenum -> GLint -> GLsizei -> IO ())
ptr_glDrawArraysEXT = unsafePerformIO $ getCommand "glDrawArraysEXT"

-- glDrawArraysIndirect --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawArraysIndirect.xhtml OpenGL 4.x>.
glDrawArraysIndirect
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indirect@.
  -> m ()
glDrawArraysIndirect v1 v2 = liftIO $ dyn238 ptr_glDrawArraysIndirect v1 v2

{-# NOINLINE ptr_glDrawArraysIndirect #-}
ptr_glDrawArraysIndirect :: FunPtr (GLenum -> Ptr a -> IO ())
ptr_glDrawArraysIndirect = unsafePerformIO $ getCommand "glDrawArraysIndirect"

-- glDrawArraysInstanced -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawArraysInstanced.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawArraysInstanced.xhtml OpenGL 4.x>.
glDrawArraysInstanced
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @instancecount@.
  -> m ()
glDrawArraysInstanced v1 v2 v3 v4 = liftIO $ dyn239 ptr_glDrawArraysInstanced v1 v2 v3 v4

{-# NOINLINE ptr_glDrawArraysInstanced #-}
ptr_glDrawArraysInstanced :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawArraysInstanced = unsafePerformIO $ getCommand "glDrawArraysInstanced"

-- glDrawArraysInstancedANGLE --------------------------------------------------

-- | This command is an alias for 'glDrawArraysInstanced'.
glDrawArraysInstancedANGLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawArraysInstancedANGLE v1 v2 v3 v4 = liftIO $ dyn239 ptr_glDrawArraysInstancedANGLE v1 v2 v3 v4

{-# NOINLINE ptr_glDrawArraysInstancedANGLE #-}
ptr_glDrawArraysInstancedANGLE :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawArraysInstancedANGLE = unsafePerformIO $ getCommand "glDrawArraysInstancedANGLE"

-- glDrawArraysInstancedARB ----------------------------------------------------

-- | This command is an alias for 'glDrawArraysInstanced'.
glDrawArraysInstancedARB
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawArraysInstancedARB v1 v2 v3 v4 = liftIO $ dyn239 ptr_glDrawArraysInstancedARB v1 v2 v3 v4

{-# NOINLINE ptr_glDrawArraysInstancedARB #-}
ptr_glDrawArraysInstancedARB :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawArraysInstancedARB = unsafePerformIO $ getCommand "glDrawArraysInstancedARB"

-- glDrawArraysInstancedBaseInstance -------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawArraysInstancedBaseInstance.xhtml OpenGL 4.x>.
glDrawArraysInstancedBaseInstance
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @instancecount@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawArraysInstancedBaseInstance v1 v2 v3 v4 v5 = liftIO $ dyn240 ptr_glDrawArraysInstancedBaseInstance v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawArraysInstancedBaseInstance #-}
ptr_glDrawArraysInstancedBaseInstance :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> GLuint -> IO ())
ptr_glDrawArraysInstancedBaseInstance = unsafePerformIO $ getCommand "glDrawArraysInstancedBaseInstance"

-- glDrawArraysInstancedBaseInstanceEXT ----------------------------------------

-- | This command is an alias for 'glDrawArraysInstancedBaseInstance'.
glDrawArraysInstancedBaseInstanceEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @instancecount@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawArraysInstancedBaseInstanceEXT v1 v2 v3 v4 v5 = liftIO $ dyn240 ptr_glDrawArraysInstancedBaseInstanceEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawArraysInstancedBaseInstanceEXT #-}
ptr_glDrawArraysInstancedBaseInstanceEXT :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> GLuint -> IO ())
ptr_glDrawArraysInstancedBaseInstanceEXT = unsafePerformIO $ getCommand "glDrawArraysInstancedBaseInstanceEXT"

-- glDrawArraysInstancedEXT ----------------------------------------------------

-- | This command is an alias for 'glDrawArraysInstanced'.
glDrawArraysInstancedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawArraysInstancedEXT v1 v2 v3 v4 = liftIO $ dyn239 ptr_glDrawArraysInstancedEXT v1 v2 v3 v4

{-# NOINLINE ptr_glDrawArraysInstancedEXT #-}
ptr_glDrawArraysInstancedEXT :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawArraysInstancedEXT = unsafePerformIO $ getCommand "glDrawArraysInstancedEXT"

-- glDrawArraysInstancedNV -----------------------------------------------------

-- | This command is an alias for 'glDrawArraysInstanced'.
glDrawArraysInstancedNV
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawArraysInstancedNV v1 v2 v3 v4 = liftIO $ dyn239 ptr_glDrawArraysInstancedNV v1 v2 v3 v4

{-# NOINLINE ptr_glDrawArraysInstancedNV #-}
ptr_glDrawArraysInstancedNV :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawArraysInstancedNV = unsafePerformIO $ getCommand "glDrawArraysInstancedNV"

-- glDrawBuffer ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawBuffer.xhtml OpenGL 4.x>.
glDrawBuffer
  :: MonadIO m
  => GLenum -- ^ @buf@ of type [DrawBufferMode](Graphics-GL-Groups.html#DrawBufferMode).
  -> m ()
glDrawBuffer v1 = liftIO $ dyn5 ptr_glDrawBuffer v1

{-# NOINLINE ptr_glDrawBuffer #-}
ptr_glDrawBuffer :: FunPtr (GLenum -> IO ())
ptr_glDrawBuffer = unsafePerformIO $ getCommand "glDrawBuffer"

-- glDrawBuffers ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawBuffers.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawBuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawBuffers.xhtml OpenGL 4.x>.
glDrawBuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to @n@ elements of type [DrawBufferMode](Graphics-GL-Groups.html#DrawBufferMode).
  -> m ()
glDrawBuffers v1 v2 = liftIO $ dyn241 ptr_glDrawBuffers v1 v2

{-# NOINLINE ptr_glDrawBuffers #-}
ptr_glDrawBuffers :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
ptr_glDrawBuffers = unsafePerformIO $ getCommand "glDrawBuffers"

-- glDrawBuffersARB ------------------------------------------------------------

-- | This command is an alias for 'glDrawBuffers'.
glDrawBuffersARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to @n@ elements of type [DrawBufferMode](Graphics-GL-Groups.html#DrawBufferMode).
  -> m ()
glDrawBuffersARB v1 v2 = liftIO $ dyn241 ptr_glDrawBuffersARB v1 v2

{-# NOINLINE ptr_glDrawBuffersARB #-}
ptr_glDrawBuffersARB :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
ptr_glDrawBuffersARB = unsafePerformIO $ getCommand "glDrawBuffersARB"

-- glDrawBuffersATI ------------------------------------------------------------

-- | This command is an alias for 'glDrawBuffers'.
glDrawBuffersATI
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to @n@ elements of type [DrawBufferMode](Graphics-GL-Groups.html#DrawBufferMode).
  -> m ()
glDrawBuffersATI v1 v2 = liftIO $ dyn241 ptr_glDrawBuffersATI v1 v2

{-# NOINLINE ptr_glDrawBuffersATI #-}
ptr_glDrawBuffersATI :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
ptr_glDrawBuffersATI = unsafePerformIO $ getCommand "glDrawBuffersATI"

-- glDrawBuffersEXT ------------------------------------------------------------

-- | This command is an alias for 'glDrawBuffers'.
glDrawBuffersEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to @n@ elements of type @GLenum@.
  -> m ()
glDrawBuffersEXT v1 v2 = liftIO $ dyn241 ptr_glDrawBuffersEXT v1 v2

{-# NOINLINE ptr_glDrawBuffersEXT #-}
ptr_glDrawBuffersEXT :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
ptr_glDrawBuffersEXT = unsafePerformIO $ getCommand "glDrawBuffersEXT"

-- glDrawBuffersIndexedEXT -----------------------------------------------------

glDrawBuffersIndexedEXT
  :: MonadIO m
  => GLint -- ^ @n@.
  -> Ptr GLenum -- ^ @location@ pointing to @n@ elements of type @GLenum@.
  -> Ptr GLint -- ^ @indices@ pointing to @n@ elements of type @GLint@.
  -> m ()
glDrawBuffersIndexedEXT v1 v2 v3 = liftIO $ dyn242 ptr_glDrawBuffersIndexedEXT v1 v2 v3

{-# NOINLINE ptr_glDrawBuffersIndexedEXT #-}
ptr_glDrawBuffersIndexedEXT :: FunPtr (GLint -> Ptr GLenum -> Ptr GLint -> IO ())
ptr_glDrawBuffersIndexedEXT = unsafePerformIO $ getCommand "glDrawBuffersIndexedEXT"

-- glDrawBuffersNV -------------------------------------------------------------

glDrawBuffersNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to @n@ elements of type @GLenum@.
  -> m ()
glDrawBuffersNV v1 v2 = liftIO $ dyn241 ptr_glDrawBuffersNV v1 v2

{-# NOINLINE ptr_glDrawBuffersNV #-}
ptr_glDrawBuffersNV :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
ptr_glDrawBuffersNV = unsafePerformIO $ getCommand "glDrawBuffersNV"

-- glDrawCommandsAddressNV -----------------------------------------------------

glDrawCommandsAddressNV
  :: MonadIO m
  => GLenum -- ^ @primitiveMode@.
  -> Ptr GLuint64 -- ^ @indirects@.
  -> Ptr GLsizei -- ^ @sizes@.
  -> GLuint -- ^ @count@.
  -> m ()
glDrawCommandsAddressNV v1 v2 v3 v4 = liftIO $ dyn243 ptr_glDrawCommandsAddressNV v1 v2 v3 v4

{-# NOINLINE ptr_glDrawCommandsAddressNV #-}
ptr_glDrawCommandsAddressNV :: FunPtr (GLenum -> Ptr GLuint64 -> Ptr GLsizei -> GLuint -> IO ())
ptr_glDrawCommandsAddressNV = unsafePerformIO $ getCommand "glDrawCommandsAddressNV"

-- glDrawCommandsNV ------------------------------------------------------------

glDrawCommandsNV
  :: MonadIO m
  => GLenum -- ^ @primitiveMode@.
  -> GLuint -- ^ @buffer@.
  -> Ptr GLintptr -- ^ @indirects@.
  -> Ptr GLsizei -- ^ @sizes@.
  -> GLuint -- ^ @count@.
  -> m ()
glDrawCommandsNV v1 v2 v3 v4 v5 = liftIO $ dyn244 ptr_glDrawCommandsNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawCommandsNV #-}
ptr_glDrawCommandsNV :: FunPtr (GLenum -> GLuint -> Ptr GLintptr -> Ptr GLsizei -> GLuint -> IO ())
ptr_glDrawCommandsNV = unsafePerformIO $ getCommand "glDrawCommandsNV"

-- glDrawCommandsStatesAddressNV -----------------------------------------------

glDrawCommandsStatesAddressNV
  :: MonadIO m
  => Ptr GLuint64 -- ^ @indirects@.
  -> Ptr GLsizei -- ^ @sizes@.
  -> Ptr GLuint -- ^ @states@.
  -> Ptr GLuint -- ^ @fbos@.
  -> GLuint -- ^ @count@.
  -> m ()
glDrawCommandsStatesAddressNV v1 v2 v3 v4 v5 = liftIO $ dyn245 ptr_glDrawCommandsStatesAddressNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawCommandsStatesAddressNV #-}
ptr_glDrawCommandsStatesAddressNV :: FunPtr (Ptr GLuint64 -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ())
ptr_glDrawCommandsStatesAddressNV = unsafePerformIO $ getCommand "glDrawCommandsStatesAddressNV"

-- glDrawCommandsStatesNV ------------------------------------------------------

glDrawCommandsStatesNV
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> Ptr GLintptr -- ^ @indirects@.
  -> Ptr GLsizei -- ^ @sizes@.
  -> Ptr GLuint -- ^ @states@.
  -> Ptr GLuint -- ^ @fbos@.
  -> GLuint -- ^ @count@.
  -> m ()
glDrawCommandsStatesNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn246 ptr_glDrawCommandsStatesNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawCommandsStatesNV #-}
ptr_glDrawCommandsStatesNV :: FunPtr (GLuint -> Ptr GLintptr -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ())
ptr_glDrawCommandsStatesNV = unsafePerformIO $ getCommand "glDrawCommandsStatesNV"

-- glDrawElementArrayAPPLE -----------------------------------------------------

glDrawElementArrayAPPLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawElementArrayAPPLE v1 v2 v3 = liftIO $ dyn237 ptr_glDrawElementArrayAPPLE v1 v2 v3

{-# NOINLINE ptr_glDrawElementArrayAPPLE #-}
ptr_glDrawElementArrayAPPLE :: FunPtr (GLenum -> GLint -> GLsizei -> IO ())
ptr_glDrawElementArrayAPPLE = unsafePerformIO $ getCommand "glDrawElementArrayAPPLE"

-- glDrawElementArrayATI -------------------------------------------------------

glDrawElementArrayATI
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawElementArrayATI v1 v2 = liftIO $ dyn247 ptr_glDrawElementArrayATI v1 v2

{-# NOINLINE ptr_glDrawElementArrayATI #-}
ptr_glDrawElementArrayATI :: FunPtr (GLenum -> GLsizei -> IO ())
ptr_glDrawElementArrayATI = unsafePerformIO $ getCommand "glDrawElementArrayATI"

-- glDrawElements --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawElements.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawElements.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawElements.xhtml OpenGL 4.x>.
glDrawElements
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> m ()
glDrawElements v1 v2 v3 v4 = liftIO $ dyn248 ptr_glDrawElements v1 v2 v3 v4

{-# NOINLINE ptr_glDrawElements #-}
ptr_glDrawElements :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glDrawElements = unsafePerformIO $ getCommand "glDrawElements"

-- glDrawElementsBaseVertex ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsBaseVertex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsBaseVertex.xhtml OpenGL 4.x>.
glDrawElementsBaseVertex
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsBaseVertex v1 v2 v3 v4 v5 = liftIO $ dyn249 ptr_glDrawElementsBaseVertex v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsBaseVertex #-}
ptr_glDrawElementsBaseVertex :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawElementsBaseVertex = unsafePerformIO $ getCommand "glDrawElementsBaseVertex"

-- glDrawElementsBaseVertexEXT -------------------------------------------------

-- | This command is an alias for 'glDrawElementsBaseVertex'.
glDrawElementsBaseVertexEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsBaseVertexEXT v1 v2 v3 v4 v5 = liftIO $ dyn249 ptr_glDrawElementsBaseVertexEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsBaseVertexEXT #-}
ptr_glDrawElementsBaseVertexEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawElementsBaseVertexEXT = unsafePerformIO $ getCommand "glDrawElementsBaseVertexEXT"

-- glDrawElementsBaseVertexOES -------------------------------------------------

-- | This command is an alias for 'glDrawElementsBaseVertex'.
glDrawElementsBaseVertexOES
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsBaseVertexOES v1 v2 v3 v4 v5 = liftIO $ dyn249 ptr_glDrawElementsBaseVertexOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsBaseVertexOES #-}
ptr_glDrawElementsBaseVertexOES :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawElementsBaseVertexOES = unsafePerformIO $ getCommand "glDrawElementsBaseVertexOES"

-- glDrawElementsIndirect ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsIndirect.xhtml OpenGL 4.x>.
glDrawElementsIndirect
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indirect@.
  -> m ()
glDrawElementsIndirect v1 v2 v3 = liftIO $ dyn250 ptr_glDrawElementsIndirect v1 v2 v3

{-# NOINLINE ptr_glDrawElementsIndirect #-}
ptr_glDrawElementsIndirect :: FunPtr (GLenum -> GLenum -> Ptr a -> IO ())
ptr_glDrawElementsIndirect = unsafePerformIO $ getCommand "glDrawElementsIndirect"

-- glDrawElementsInstanced -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsInstanced.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstanced.xhtml OpenGL 4.x>.
glDrawElementsInstanced
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> m ()
glDrawElementsInstanced v1 v2 v3 v4 v5 = liftIO $ dyn251 ptr_glDrawElementsInstanced v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsInstanced #-}
ptr_glDrawElementsInstanced :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glDrawElementsInstanced = unsafePerformIO $ getCommand "glDrawElementsInstanced"

-- glDrawElementsInstancedANGLE ------------------------------------------------

-- | This command is an alias for 'glDrawElementsInstanced'.
glDrawElementsInstancedANGLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawElementsInstancedANGLE v1 v2 v3 v4 v5 = liftIO $ dyn251 ptr_glDrawElementsInstancedANGLE v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsInstancedANGLE #-}
ptr_glDrawElementsInstancedANGLE :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glDrawElementsInstancedANGLE = unsafePerformIO $ getCommand "glDrawElementsInstancedANGLE"

-- glDrawElementsInstancedARB --------------------------------------------------

-- | This command is an alias for 'glDrawElementsInstanced'.
glDrawElementsInstancedARB
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawElementsInstancedARB v1 v2 v3 v4 v5 = liftIO $ dyn251 ptr_glDrawElementsInstancedARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsInstancedARB #-}
ptr_glDrawElementsInstancedARB :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glDrawElementsInstancedARB = unsafePerformIO $ getCommand "glDrawElementsInstancedARB"

-- glDrawElementsInstancedBaseInstance -----------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstancedBaseInstance.xhtml OpenGL 4.x>.
glDrawElementsInstancedBaseInstance
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indices@ pointing to @count@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawElementsInstancedBaseInstance v1 v2 v3 v4 v5 v6 = liftIO $ dyn252 ptr_glDrawElementsInstancedBaseInstance v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawElementsInstancedBaseInstance #-}
ptr_glDrawElementsInstancedBaseInstance :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLuint -> IO ())
ptr_glDrawElementsInstancedBaseInstance = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseInstance"

-- glDrawElementsInstancedBaseInstanceEXT --------------------------------------

-- | This command is an alias for 'glDrawElementsInstancedBaseInstance'.
glDrawElementsInstancedBaseInstanceEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indices@ pointing to @count@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawElementsInstancedBaseInstanceEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn252 ptr_glDrawElementsInstancedBaseInstanceEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawElementsInstancedBaseInstanceEXT #-}
ptr_glDrawElementsInstancedBaseInstanceEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLuint -> IO ())
ptr_glDrawElementsInstancedBaseInstanceEXT = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseInstanceEXT"

-- glDrawElementsInstancedBaseVertex -------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsInstancedBaseVertex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstancedBaseVertex.xhtml OpenGL 4.x>.
glDrawElementsInstancedBaseVertex
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsInstancedBaseVertex v1 v2 v3 v4 v5 v6 = liftIO $ dyn253 ptr_glDrawElementsInstancedBaseVertex v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawElementsInstancedBaseVertex #-}
ptr_glDrawElementsInstancedBaseVertex :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLint -> IO ())
ptr_glDrawElementsInstancedBaseVertex = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseVertex"

-- glDrawElementsInstancedBaseVertexBaseInstance -------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstancedBaseVertexBaseInstance.xhtml OpenGL 4.x>.
glDrawElementsInstancedBaseVertexBaseInstance
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @count@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLint -- ^ @basevertex@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawElementsInstancedBaseVertexBaseInstance v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn254 ptr_glDrawElementsInstancedBaseVertexBaseInstance v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glDrawElementsInstancedBaseVertexBaseInstance #-}
ptr_glDrawElementsInstancedBaseVertexBaseInstance :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLint -> GLuint -> IO ())
ptr_glDrawElementsInstancedBaseVertexBaseInstance = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseVertexBaseInstance"

-- glDrawElementsInstancedBaseVertexBaseInstanceEXT ----------------------------

-- | This command is an alias for 'glDrawElementsInstancedBaseVertexBaseInstance'.
glDrawElementsInstancedBaseVertexBaseInstanceEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @count@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLint -- ^ @basevertex@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawElementsInstancedBaseVertexBaseInstanceEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn254 ptr_glDrawElementsInstancedBaseVertexBaseInstanceEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glDrawElementsInstancedBaseVertexBaseInstanceEXT #-}
ptr_glDrawElementsInstancedBaseVertexBaseInstanceEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLint -> GLuint -> IO ())
ptr_glDrawElementsInstancedBaseVertexBaseInstanceEXT = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseVertexBaseInstanceEXT"

-- glDrawElementsInstancedBaseVertexEXT ----------------------------------------

-- | This command is an alias for 'glDrawElementsInstancedBaseVertex'.
glDrawElementsInstancedBaseVertexEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsInstancedBaseVertexEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn253 ptr_glDrawElementsInstancedBaseVertexEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawElementsInstancedBaseVertexEXT #-}
ptr_glDrawElementsInstancedBaseVertexEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLint -> IO ())
ptr_glDrawElementsInstancedBaseVertexEXT = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseVertexEXT"

-- glDrawElementsInstancedBaseVertexOES ----------------------------------------

-- | This command is an alias for 'glDrawElementsInstancedBaseVertex'.
glDrawElementsInstancedBaseVertexOES
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsInstancedBaseVertexOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn253 ptr_glDrawElementsInstancedBaseVertexOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawElementsInstancedBaseVertexOES #-}
ptr_glDrawElementsInstancedBaseVertexOES :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLint -> IO ())
ptr_glDrawElementsInstancedBaseVertexOES = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseVertexOES"

-- glDrawElementsInstancedEXT --------------------------------------------------

-- | This command is an alias for 'glDrawElementsInstanced'.
glDrawElementsInstancedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawElementsInstancedEXT v1 v2 v3 v4 v5 = liftIO $ dyn251 ptr_glDrawElementsInstancedEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsInstancedEXT #-}
ptr_glDrawElementsInstancedEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glDrawElementsInstancedEXT = unsafePerformIO $ getCommand "glDrawElementsInstancedEXT"

-- glDrawElementsInstancedNV ---------------------------------------------------

-- | This command is an alias for 'glDrawElementsInstanced'.
glDrawElementsInstancedNV
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawElementsInstancedNV v1 v2 v3 v4 v5 = liftIO $ dyn251 ptr_glDrawElementsInstancedNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsInstancedNV #-}
ptr_glDrawElementsInstancedNV :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glDrawElementsInstancedNV = unsafePerformIO $ getCommand "glDrawElementsInstancedNV"

-- glDrawMeshArraysSUN ---------------------------------------------------------

glDrawMeshArraysSUN
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @width@.
  -> m ()
glDrawMeshArraysSUN v1 v2 v3 v4 = liftIO $ dyn239 ptr_glDrawMeshArraysSUN v1 v2 v3 v4

{-# NOINLINE ptr_glDrawMeshArraysSUN #-}
ptr_glDrawMeshArraysSUN :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawMeshArraysSUN = unsafePerformIO $ getCommand "glDrawMeshArraysSUN"

-- glDrawMeshTasksIndirectNV ---------------------------------------------------

glDrawMeshTasksIndirectNV
  :: MonadIO m
  => GLintptr -- ^ @indirect@.
  -> m ()
glDrawMeshTasksIndirectNV v1 = liftIO $ dyn236 ptr_glDrawMeshTasksIndirectNV v1

{-# NOINLINE ptr_glDrawMeshTasksIndirectNV #-}
ptr_glDrawMeshTasksIndirectNV :: FunPtr (GLintptr -> IO ())
ptr_glDrawMeshTasksIndirectNV = unsafePerformIO $ getCommand "glDrawMeshTasksIndirectNV"

-- glDrawMeshTasksNV -----------------------------------------------------------

glDrawMeshTasksNV
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLuint -- ^ @count@.
  -> m ()
glDrawMeshTasksNV v1 v2 = liftIO $ dyn4 ptr_glDrawMeshTasksNV v1 v2

{-# NOINLINE ptr_glDrawMeshTasksNV #-}
ptr_glDrawMeshTasksNV :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glDrawMeshTasksNV = unsafePerformIO $ getCommand "glDrawMeshTasksNV"

-- glDrawPixels ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawPixels.xml OpenGL 2.x>.
glDrawPixels
  :: MonadIO m
  => GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glDrawPixels v1 v2 v3 v4 v5 = liftIO $ dyn255 ptr_glDrawPixels v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawPixels #-}
ptr_glDrawPixels :: FunPtr (GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glDrawPixels = unsafePerformIO $ getCommand "glDrawPixels"

-- glDrawRangeElementArrayAPPLE ------------------------------------------------

glDrawRangeElementArrayAPPLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawRangeElementArrayAPPLE v1 v2 v3 v4 v5 = liftIO $ dyn256 ptr_glDrawRangeElementArrayAPPLE v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawRangeElementArrayAPPLE #-}
ptr_glDrawRangeElementArrayAPPLE :: FunPtr (GLenum -> GLuint -> GLuint -> GLint -> GLsizei -> IO ())
ptr_glDrawRangeElementArrayAPPLE = unsafePerformIO $ getCommand "glDrawRangeElementArrayAPPLE"

-- glDrawRangeElementArrayATI --------------------------------------------------

glDrawRangeElementArrayATI
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawRangeElementArrayATI v1 v2 v3 v4 = liftIO $ dyn257 ptr_glDrawRangeElementArrayATI v1 v2 v3 v4

{-# NOINLINE ptr_glDrawRangeElementArrayATI #-}
ptr_glDrawRangeElementArrayATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> IO ())
ptr_glDrawRangeElementArrayATI = unsafePerformIO $ getCommand "glDrawRangeElementArrayATI"

-- glDrawRangeElements ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawRangeElements.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElements.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawRangeElements.xhtml OpenGL 4.x>.
glDrawRangeElements
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> m ()
glDrawRangeElements v1 v2 v3 v4 v5 v6 = liftIO $ dyn258 ptr_glDrawRangeElements v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawRangeElements #-}
ptr_glDrawRangeElements :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glDrawRangeElements = unsafePerformIO $ getCommand "glDrawRangeElements"

-- glDrawRangeElementsBaseVertex -----------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElementsBaseVertex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawRangeElementsBaseVertex.xhtml OpenGL 4.x>.
glDrawRangeElementsBaseVertex
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawRangeElementsBaseVertex v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn259 ptr_glDrawRangeElementsBaseVertex v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glDrawRangeElementsBaseVertex #-}
ptr_glDrawRangeElementsBaseVertex :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawRangeElementsBaseVertex = unsafePerformIO $ getCommand "glDrawRangeElementsBaseVertex"

-- glDrawRangeElementsBaseVertexEXT --------------------------------------------

-- | This command is an alias for 'glDrawRangeElementsBaseVertex'.
glDrawRangeElementsBaseVertexEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawRangeElementsBaseVertexEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn259 ptr_glDrawRangeElementsBaseVertexEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glDrawRangeElementsBaseVertexEXT #-}
ptr_glDrawRangeElementsBaseVertexEXT :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawRangeElementsBaseVertexEXT = unsafePerformIO $ getCommand "glDrawRangeElementsBaseVertexEXT"

-- glDrawRangeElementsBaseVertexOES --------------------------------------------

-- | This command is an alias for 'glDrawRangeElementsBaseVertex'.
glDrawRangeElementsBaseVertexOES
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawRangeElementsBaseVertexOES v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn259 ptr_glDrawRangeElementsBaseVertexOES v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glDrawRangeElementsBaseVertexOES #-}
ptr_glDrawRangeElementsBaseVertexOES :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawRangeElementsBaseVertexOES = unsafePerformIO $ getCommand "glDrawRangeElementsBaseVertexOES"

-- glDrawRangeElementsEXT ------------------------------------------------------

-- | This command is an alias for 'glDrawRangeElements'.
glDrawRangeElementsEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type [DrawElementsType](Graphics-GL-Groups.html#DrawElementsType).
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> m ()
glDrawRangeElementsEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn258 ptr_glDrawRangeElementsEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawRangeElementsEXT #-}
ptr_glDrawRangeElementsEXT :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glDrawRangeElementsEXT = unsafePerformIO $ getCommand "glDrawRangeElementsEXT"

-- glDrawTexfOES ---------------------------------------------------------------

-- | The vector equivalent of this command is 'glDrawTexfvOES'.
glDrawTexfOES
  :: MonadIO m
  => GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @width@.
  -> GLfloat -- ^ @height@.
  -> m ()
glDrawTexfOES v1 v2 v3 v4 v5 = liftIO $ dyn260 ptr_glDrawTexfOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawTexfOES #-}
ptr_glDrawTexfOES :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glDrawTexfOES = unsafePerformIO $ getCommand "glDrawTexfOES"

-- glDrawTexfvOES --------------------------------------------------------------

glDrawTexfvOES
  :: MonadIO m
  => Ptr GLfloat -- ^ @coords@ pointing to @5@ elements of type @GLfloat@.
  -> m ()
glDrawTexfvOES v1 = liftIO $ dyn44 ptr_glDrawTexfvOES v1

{-# NOINLINE ptr_glDrawTexfvOES #-}
ptr_glDrawTexfvOES :: FunPtr (Ptr GLfloat -> IO ())
ptr_glDrawTexfvOES = unsafePerformIO $ getCommand "glDrawTexfvOES"

-- glDrawTexiOES ---------------------------------------------------------------

-- | The vector equivalent of this command is 'glDrawTexivOES'.
glDrawTexiOES
  :: MonadIO m
  => GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @width@.
  -> GLint -- ^ @height@.
  -> m ()
glDrawTexiOES v1 v2 v3 v4 v5 = liftIO $ dyn261 ptr_glDrawTexiOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawTexiOES #-}
ptr_glDrawTexiOES :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glDrawTexiOES = unsafePerformIO $ getCommand "glDrawTexiOES"

-- glDrawTexivOES --------------------------------------------------------------

glDrawTexivOES
  :: MonadIO m
  => Ptr GLint -- ^ @coords@ pointing to @5@ elements of type @GLint@.
  -> m ()
glDrawTexivOES v1 = liftIO $ dyn46 ptr_glDrawTexivOES v1

{-# NOINLINE ptr_glDrawTexivOES #-}
ptr_glDrawTexivOES :: FunPtr (Ptr GLint -> IO ())
ptr_glDrawTexivOES = unsafePerformIO $ getCommand "glDrawTexivOES"

-- glDrawTexsOES ---------------------------------------------------------------

-- | The vector equivalent of this command is 'glDrawTexsvOES'.
glDrawTexsOES
  :: MonadIO m
  => GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> GLshort -- ^ @width@.
  -> GLshort -- ^ @height@.
  -> m ()
glDrawTexsOES v1 v2 v3 v4 v5 = liftIO $ dyn262 ptr_glDrawTexsOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawTexsOES #-}
ptr_glDrawTexsOES :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glDrawTexsOES = unsafePerformIO $ getCommand "glDrawTexsOES"

-- glDrawTexsvOES --------------------------------------------------------------

glDrawTexsvOES
  :: MonadIO m
  => Ptr GLshort -- ^ @coords@ pointing to @5@ elements of type @GLshort@.
  -> m ()
glDrawTexsvOES v1 = liftIO $ dyn48 ptr_glDrawTexsvOES v1

{-# NOINLINE ptr_glDrawTexsvOES #-}
ptr_glDrawTexsvOES :: FunPtr (Ptr GLshort -> IO ())
ptr_glDrawTexsvOES = unsafePerformIO $ getCommand "glDrawTexsvOES"

-- glDrawTextureNV -------------------------------------------------------------

glDrawTextureNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLuint -- ^ @sampler@.
  -> GLfloat -- ^ @x0@.
  -> GLfloat -- ^ @y0@.
  -> GLfloat -- ^ @x1@.
  -> GLfloat -- ^ @y1@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @s0@.
  -> GLfloat -- ^ @t0@.
  -> GLfloat -- ^ @s1@.
  -> GLfloat -- ^ @t1@.
  -> m ()
glDrawTextureNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn263 ptr_glDrawTextureNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glDrawTextureNV #-}
ptr_glDrawTextureNV :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glDrawTextureNV = unsafePerformIO $ getCommand "glDrawTextureNV"

