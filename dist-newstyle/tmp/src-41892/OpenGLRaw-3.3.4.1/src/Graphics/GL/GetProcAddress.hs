{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.GetProcAddress
-- Copyright   :  (c) Sven Panne 2009-2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module offers a portable way to retrieve OpenGL functions and extension
-- entries, providing a portability layer upon platform-specific mechanisms
-- like @glXGetProcAddress@, @wglGetProcAddress@ or @NSAddressOfSymbol@.
--
-- Note that /finding/ an OpenGL entry point doesn't mean that it's actually
-- /usable:/ On most platforms entry points are context-independent, so you have
-- to check the available extensions and\/or OpenGL version, too.
--------------------------------------------------------------------------------

module Graphics.GL.GetProcAddress (
   -- * Unchecked retrieval
   getProcAddress,
   getProcAddressWithSuffixes,
   getExtension,
   -- * Checked retrieval
   getProcAddressChecked,
   getProcAddressWithSuffixesChecked,
   getExtensionChecked,
   -- * Version info and extensions
   getVersion, version,
   getExtensions, extensions
) where

#if !MIN_VERSION_base(4,8,0)
import Data.Functor( (<$>), (<$) )
#endif
import Control.Monad ( forM )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.ByteString.Unsafe ( unsafePackCString, unsafeUseAsCString )
import Data.Char ( isDigit )
import Data.Set ( Set, fromList )
import Data.Text ( pack, unpack )
import Data.Text.Encoding ( encodeUtf8, decodeUtf8 )
import Foreign.C.String ( CString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Error ( throwIf )
import Foreign.Ptr ( Ptr, nullPtr, castPtr, FunPtr, nullFunPtr )
import Foreign.Storable ( peek )
import Graphics.GL.Tokens
import Graphics.GL.Types
import System.IO.Unsafe ( unsafePerformIO )
import Text.ParserCombinators.ReadP

--------------------------------------------------------------------------------

-- | Retrieve an OpenGL function by name. Returns 'nullFunPtr' when no function
-- with the given name was found.
getProcAddress :: MonadIO m => String -> m (FunPtr a)
getProcAddress cmd = liftIO $ withUtf8String cmd hs_OpenGLRaw_getProcAddress

foreign import ccall unsafe "hs_OpenGLRaw_getProcAddress"
   hs_OpenGLRaw_getProcAddress :: CString -> IO (FunPtr a)

-- | Retrieve an OpenGL function by name. Throws an 'userError' when no function
-- with the given name was found.
getProcAddressChecked :: MonadIO m => String -> m (FunPtr a)
getProcAddressChecked cmd = liftIO $ check cmd $ getProcAddress cmd

-- | Retrieve an OpenGL function by name, trying a list of name suffixes in the
-- given order. Returns 'nullFunPtr' when no function with the given name plus
-- any of the suffixes was found.
getProcAddressWithSuffixes :: MonadIO m => String -> [String] -> m (FunPtr a)
getProcAddressWithSuffixes _ [] = return nullFunPtr
getProcAddressWithSuffixes cmd (x:xs) = do
   p <- getProcAddress (cmd ++ x)
   if p == nullFunPtr
      then getProcAddressWithSuffixes cmd xs
      else return p

-- | Retrieve an OpenGL function by name, trying a list of name suffixes in the
-- given order. Throws an 'userError' when no function with the given name plus
-- any of the suffixes was found.
getProcAddressWithSuffixesChecked :: MonadIO m
                                  => String -> [String] -> m (FunPtr a)
getProcAddressWithSuffixesChecked cmd suffixes =
   liftIO $ check cmd $ getProcAddressWithSuffixes cmd suffixes

-- | Retrieve an OpenGL function by name, additionally trying a list of all
-- known vendor suffixes. Returns 'nullFunPtr' when no function with the given
-- name plus any of the suffixes was found.
getExtension :: MonadIO m => String -> m (FunPtr a)
getExtension cmd = liftIO $ getProcAddressWithSuffixes cmd vendorSuffixes

-- | Retrieve an OpenGL function by name, additionally trying a list of all
-- known vendor suffixes. Throws an 'userError' when no function with the given
-- name plus any of the suffixes was found.
getExtensionChecked :: MonadIO m => String -> m (FunPtr a)
getExtensionChecked cmd =
  liftIO $ getProcAddressWithSuffixesChecked cmd vendorSuffixes

check :: String -> IO (FunPtr a) -> IO (FunPtr a)
check = throwIfNullFunPtr . ("unknown OpenGL command " ++)

-- This should really live in Foreign.Marshal.Error.
throwIfNullFunPtr :: String -> IO (FunPtr a) -> IO (FunPtr a)
throwIfNullFunPtr = throwIf (== nullFunPtr) . const

vendorSuffixes :: [String]
vendorSuffixes = [
   -- stuff already in the standard
   "",
   -- officially blessed stuff
   "ARB", "KHR", "OES",
   -- almost official stuff
   "EXT",
   -- random vendor stuff in decreasing order of number of extensions
   "NV", "SGIX", "AMD", "APPLE", "ATI", "SGIS", "ANGLE", "QCOM", "IMG", "SUN",
   "IBM", "ARM", "MESA", "INTEL", "HP", "SGI", "OML", "INGR", "3DFX", "WIN",
   "PGI", "NVX", "GREMEDY", "DMP", "VIV", "SUNX", "S3", "REND", "MESAX", "FJ",
   "ANDROID" ]

--------------------------------------------------------------------------------

-- | Retrieve the set of all available OpenGL extensions.
getExtensions :: MonadIO m => m (Set String)
getExtensions = liftIO $ Data.Set.fromList <$> do
  -- glGetStringi is only present from OpenGL 3.0 and OpenGL ES 3.0 onwards, but
  -- we can't simply retrieve its entry point and check that against nullFunPtr:
  -- Apart from WGL, entry points are context-independent, so even having an
  -- entry point which looks valid doesn't guarantee that it is actually
  -- supported. Therefore we need to check the OpenGL version number directly.
  getString <- makeGetString
  v <- getVersionWith getString
  if v >= (3, 0)
    then do getInteger <- makeGetInteger
            getStringi <- makeGetStringi
            numExtensions <- getInteger GL_NUM_EXTENSIONS
            forM [ 0 .. fromIntegral numExtensions - 1 ] $
              getStringi GL_EXTENSIONS
    else words <$> getString GL_EXTENSIONS

--------------------------------------------------------------------------------

-- | Retrieve the OpenGL version, split into major and minor version numbers.
getVersion :: MonadIO m => m (Int, Int)
getVersion = liftIO $ makeGetString >>= getVersionWith

getVersionWith :: (GLenum -> IO String) -> IO (Int, Int)
getVersionWith getString =
  runParser parseVersion (-1, -1) <$> getString GL_VERSION

runParser :: ReadP a -> a -> String -> a
runParser parser failed str =
  case readP_to_S parser str of
    [(v, "")] -> v
    _ -> failed

-- This does quite a bit more than we need for "normal" OpenGL, but at least it
-- documents the convoluted format of the version string in detail.
parseVersion :: ReadP (Int, Int)
parseVersion = do
  _prefix <-
    -- Too lazy to define a type for the API...
    ("CL" <$ string "OpenGL ES-CL ") <++  -- OpenGL ES 1.x Common-Lite
    ("CM" <$ string "OpenGL ES-CM ") <++  -- OpenGL ES 1.x Common
    ("ES" <$ string "OpenGL ES "   ) <++  -- OpenGL ES 2.x or 3.x
    ("GL" <$ string ""             )      -- OpenGL
  major <- read <$> munch1 isDigit
  minor <- char '.' >> read <$> munch1 isDigit
  _release <- (char '.' >> munch1 (/= ' ')) <++ return ""
  _vendorStuff <- (char ' ' >> get `manyTill` eof) <++ ("" <$ eof)
  return (major, minor)

--------------------------------------------------------------------------------
-- Graphics.GL.Foreign uses generated names, which are not
-- easily predictable, so we duplicate a few "foreign import"s below.

makeGetString :: IO (GLenum -> IO String)
makeGetString = do
  glGetString_ <- dynGLenumIOPtrGLubyte <$> getProcAddress "glGetString"
  return $ \name -> glGetString_ name >>= peekGLstring

foreign import CALLCONV "dynamic" dynGLenumIOPtrGLubyte
  :: FunPtr (GLenum -> IO (Ptr GLubyte))
  ->         GLenum -> IO (Ptr GLubyte)

makeGetStringi :: IO (GLenum -> GLuint -> IO String)
makeGetStringi = do
  glGetStringi_ <- dynGLenumGLuintIOPtrGLubyte <$> getProcAddress "glGetStringi"
  return $ \name index -> glGetStringi_ name index >>= peekGLstring

foreign import CALLCONV "dynamic" dynGLenumGLuintIOPtrGLubyte
  :: FunPtr (GLenum -> GLuint -> IO (Ptr GLubyte))
  ->         GLenum -> GLuint -> IO (Ptr GLubyte)

makeGetInteger :: IO (GLenum -> IO GLint)
makeGetInteger = do
  glGetIntegerv_ <- dynGLenumPtrGLintIOVoid <$> getProcAddress "glGetIntegerv"
  return $ \name -> alloca $ \p -> glGetIntegerv_ name p >> peek p

foreign import CALLCONV "dynamic" dynGLenumPtrGLintIOVoid
  :: FunPtr (GLenum -> Ptr GLint -> IO ())
  ->         GLenum -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------

-- Play safe, this is in line with OpenGL: Return something, but don't crash.
peekGLstring :: Ptr GLubyte -> IO String
peekGLstring = ptr (return "") (peekUtf8String . castPtr)

-- This should really be in Foreign.Ptr.
ptr :: b -> (Ptr a -> b) -> Ptr a -> b
ptr n f p | p == nullPtr = n
          | otherwise    = f p

--------------------------------------------------------------------------------

withUtf8String :: String -> (CString -> IO a) -> IO a
withUtf8String = unsafeUseAsCString . encodeUtf8 . pack . (++ "\0")

peekUtf8String :: CString -> IO String
peekUtf8String p = unpack . decodeUtf8 <$> unsafePackCString p

--------------------------------------------------------------------------------

-- | The set of all available OpenGL extensions. Note that in the presence of
-- multiple contexts with different capabilities, this might be wrong. Use
-- 'getExtensions' in those cases instead.
extensions :: Set String
extensions = unsafePerformIO getExtensions
{-# NOINLINE extensions #-}

-- | The OpenGL version, split into major and minor version numbers. Note that
-- in the presence of multiple contexts with different capabilities, this might
-- be wrong. Use 'getVersion' in those cases instead.
version :: (Int, Int)
version = unsafePerformIO getVersion
{-# NOINLINE version #-}
