{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Types
-- Copyright   :  (c) Sven Panne 2009-2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- All types from the whole OpenGL registry.
--
--------------------------------------------------------------------------------

module Graphics.GL.Types (
  -- * Types from OpenGL itself.
  GLboolean,
  GLbyte,
  GLubyte,
  GLchar,
  GLshort,
  GLushort,
  GLint,
  GLuint,
  GLfixed,
  GLint64,
  GLuint64,
  GLsizei,
  GLenum,
  GLintptr,
  GLsizeiptr,
  GLsync,
  GLbitfield,
  GLhalf,
  GLfloat,
  GLclampf,
  GLdouble,
  GLclampd,
  GLDEBUGPROC, GLDEBUGPROCFunc, makeGLDEBUGPROC,
  GLvoid,

  -- * Pre-standard OpenGL types.
  GLcharARB,
  GLint64EXT,
  GLuint64EXT,
  GLintptrARB,
  GLsizeiptrARB,
  GLhalfARB,
  GLhalfNV,
  GLDEBUGPROCAMD, GLDEBUGPROCAMDFunc, makeGLDEBUGPROCAMD,
  GLDEBUGPROCARB, GLDEBUGPROCARBFunc, makeGLDEBUGPROCARB,
  GLDEBUGPROCKHR, GLDEBUGPROCKHRFunc, makeGLDEBUGPROCKHR,

  -- * Types from various extensions.
  GLclampx,
  GLhandleARB,
  GLvdpauSurfaceNV,
  GLeglImageOES,
  GLeglClientBufferEXT,
  GLVULKANPROCNVFunc,
  GLVULKANPROCNV,

  -- * Deprecated functions for @gl@ compatibility.
  mkGLDEBUGPROC,
  mkGLDEBUGPROCAMD,
  mkGLDEBUGPROCARB,
  mkGLDEBUGPROCKHR

) where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Numeric.Fixed
import Numeric.Half

--------------------------------------------------------------------------------

-- | 8bit boolean.
type GLboolean = Word8

-- | 8bit signed two\'s complement binary integer.
type GLbyte = Int8

-- | 8bit unsigned binary integer.
type GLubyte = Word8

-- | 8bit characters making up strings.
type GLchar = CChar

-- | 16bit signed two\'s complement binary integer.
type GLshort = Int16

-- | 16bit unsigned binary integer.
type GLushort = Word16

-- | 32bit signed two\'s complement binary integer.
type GLint = Int32

-- | 32bit unsigned binary integer.
type GLuint = Word32

-- | 32bit signed two\'s complement 16.16 scaled integer.
type GLfixed = Fixed

-- | 64bit signed two\'s complement binary integer.
type GLint64 = Int64

-- | 64bit unsigned binary integer.
type GLuint64 = Word64

-- | 32bit non-negative binary integer size.
type GLsizei = Int32

-- | 32bit enumerated binary integer value.
type GLenum = Word32

-- | Pointer-sized signed two\'s complement binary integer.
type GLintptr = CPtrdiff
-- NOTE: OpenGL ES uses khronos_intptr_t for this.

-- | Pointer-sized non-negative binary integer size.
type GLsizeiptr = CPtrdiff
-- NOTE: OpenGL ES uses khronos_ssize_t for this.

-- | Pointer-sized sync object handle.
type GLsync = Ptr ()

-- | 32bit bit field.
type GLbitfield = Word32

-- | 16bit half-precision floating-point value encoded in an unsigned scalar.
type GLhalf = Half

-- | 32bit floating-point value.
type GLfloat = Float

-- | 32bit floating-point value clamped to [0, 1].
type GLclampf = Float

-- | 64bit floating-point value.
type GLdouble = Double

-- | 64bit floating-point value clamped to [0, 1].
type GLclampd = Double

-- | A pointer to a debug callback.
type GLDEBUGPROC = FunPtr GLDEBUGPROCFunc

-- | Debug callback.
type GLDEBUGPROCFunc
  =  GLenum -- ^ @source@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @severity@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @message@.
  -> Ptr () -- ^ @userParam@.
  -> IO ()

-- | The storage associated with the resulting 'FunPtr' has to be released with
-- 'freeHaskellFunPtr' when it is no longer required.
foreign import CALLCONV "wrapper"
   makeGLDEBUGPROC :: GLDEBUGPROCFunc -> IO (FunPtr GLDEBUGPROCFunc)

-- | Not an actual GL type, though used in headers in the past.
type GLvoid = ()

type GLcharARB = CChar

type GLint64EXT = Int64

type GLuint64EXT = Word64

type GLintptrARB = CPtrdiff

type GLsizeiptrARB = CPtrdiff

type GLhalfARB = Half

type GLhalfNV = Half

type GLDEBUGPROCAMD = FunPtr GLDEBUGPROCAMDFunc

-- | Debug callback.
type GLDEBUGPROCAMDFunc
  =  GLuint -- ^ @id@.
  -> GLenum -- ^ @category@.
  -> GLenum -- ^ @severity@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @message@.
  -> Ptr () -- ^ @userParam@.
  -> IO ()

-- | The storage associated with the resulting 'FunPtr' has to be released with
-- 'freeHaskellFunPtr' when it is no longer required.
foreign import CALLCONV "wrapper"
  makeGLDEBUGPROCAMD :: GLDEBUGPROCAMDFunc -> IO (FunPtr GLDEBUGPROCAMDFunc)

type GLDEBUGPROCARB = GLDEBUGPROC

type GLDEBUGPROCARBFunc = GLDEBUGPROCFunc

makeGLDEBUGPROCARB :: GLDEBUGPROCARBFunc -> IO (FunPtr GLDEBUGPROCARBFunc)
makeGLDEBUGPROCARB = makeGLDEBUGPROC

type GLDEBUGPROCKHR = GLDEBUGPROC

type GLDEBUGPROCKHRFunc = GLDEBUGPROCFunc

makeGLDEBUGPROCKHR :: GLDEBUGPROCKHRFunc -> IO (FunPtr GLDEBUGPROCKHRFunc)
makeGLDEBUGPROCKHR = makeGLDEBUGPROC

type GLclampx = Fixed

#if HANDLE_IS_POINTER
type GLhandleARB = Ptr ()
#else
type GLhandleARB = Word32
#endif

type GLvdpauSurfaceNV = GLintptr

type GLeglImageOES = Ptr ()

type GLeglClientBufferEXT = Ptr ()

type GLVULKANPROCNV = FunPtr GLVULKANPROCNVFunc

type GLVULKANPROCNVFunc = IO ()

{-# DEPRECATED mkGLDEBUGPROC "Use 'makeGLDEBUGPROC' instead." #-}
mkGLDEBUGPROC :: GLDEBUGPROCFunc -> IO (FunPtr GLDEBUGPROCFunc)
mkGLDEBUGPROC = makeGLDEBUGPROC

{-# DEPRECATED mkGLDEBUGPROCAMD "Use 'makeGLDEBUGPROCAMD' instead." #-}
mkGLDEBUGPROCAMD :: GLDEBUGPROCAMDFunc -> IO (FunPtr GLDEBUGPROCAMDFunc)
mkGLDEBUGPROCAMD = makeGLDEBUGPROCAMD

{-# DEPRECATED mkGLDEBUGPROCARB "Use 'makekGLDEBUGPROCARB' instead." #-}
mkGLDEBUGPROCARB :: GLDEBUGPROCARBFunc -> IO (FunPtr GLDEBUGPROCARBFunc)
mkGLDEBUGPROCARB = makeGLDEBUGPROCARB

{-# DEPRECATED mkGLDEBUGPROCKHR "Use 'makeGLDEBUGPROCKHR' instead." #-}
mkGLDEBUGPROCKHR :: GLDEBUGPROCKHRFunc -> IO (FunPtr GLDEBUGPROCKHRFunc)
mkGLDEBUGPROCKHR = makeGLDEBUGPROCKHR
