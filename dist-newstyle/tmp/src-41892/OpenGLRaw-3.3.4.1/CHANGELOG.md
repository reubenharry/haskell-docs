3.3.4.1
-------
* Updated OpenGL registry to fdaa98e456.
* Relaxed upper version bounds for `bytestring` and `transformers`.
* Fixed OpenBSD build.

3.3.4.0
-------
* Updated OpenGL registry to 9b214157e.

3.3.3.0
-------
* Updated OpenGL registry to 696ac2296.
* Relaxed version bound of `fixed` package.

3.3.2.0
-------
* Updated OpenGL registry to 39e37f838.

3.3.1.0
-------
* Updated OpenGL registry to accf5ad3c.

3.3.0.2
-------
* Updated OpenGL registry to b7aee529f.
* Relaxed version bound of `containers` package.

3.3.0.1
-------
* Updated OpenGL registry to fe1a89f35.
* Relaxed version bound of `half` package.

3.3.0.0
-------
* Updated OpenGL registry to 6565c242f.

3.2.7.0
-------
* Updated OpenGL registry to 681c365c0.
* Added OpenGL 4.6 support.

3.2.6.0
-------
* Updated OpenGL registry to 89bede35b.

3.2.5.0
-------
* Updated OpenGL registry to 97558118d.

3.2.4.0
-------
* Updated OpenGL registry to r33312.

3.2.3.0
-------
* Updated OpenGL registry to r33189.

3.2.2.0
-------
* Updated OpenGL registry to r33080.

3.2.1.0
-------
* Updated OpenGL registry to r33061.
* Split the `Graphics.GL.Functions` implementation into more manageable chunks.

3.2.0.0
-------
* Updated OpenGL registry to r32749.

3.1.0.1
-------
* Updated warning flag magic for GHC 8.0.
* Updated OpenGL registry to r32363.

3.1.0.0
-------
* Changed the type of `GL_FALSE` and `GL_TRUE` to `GLboolean`, leading to fewer
  `fromIntegral` calls in user code.
* Added deprecated functions `mkGLDEBUGPROC`, `mkGLDEBUGPROCAMD`,
  `mkGLDEBUGPROCARB`, `mkGLDEBUGPROCKHR` for `gl` compatibility.
* Updated OpenGL registry to r32348.

3.0.0.0
-------
* Make the `OpenGLRaw` package even more similar to the `gl` package:
  * Use pattern synonyms for OpenGL enums.
  * Changed module name prefix from `Graphics.Rendering.OpenGL.Raw` to `Graphics.GL`.
  * Use slightly different type synonyms for GL type (introducing `Fixed` on the way):
    * `CDouble` => `Double` (for `GLclampd`, `GLdouble`)
    * `CFloat`  => `Float`  (for `GLclampf`, `GLfloat`)
    * `CInt`    => `Fixed`  (for `GLclampx`, `GLfixed`)
    * `CInt`    => `Int32`  (for `GLint`, `GLsizei`)
    * `CSChar`  => `Int8`   (for `GLbyte`)
    * `CShort`  => `Int16`  (for `GLshort`)
    * `CUChar`  => `Word8`  (for `GLboolean`, `GLubyte`)
    * `CUInt`   => `Word32` (for `GLbitfield`, `GLenum`, `GLhandleARB`, `GLuint`)
    * `CUShort` => `Word16` (for `GLushort`)

2.6.1.1
-------
* Relaxed upper version bound for `transformers`.

2.6.1.0
-------
* Updated OpenGL registry to r32258. Note that the major version was
  intentionally not bumped, because the signature change of
  `glClearNamedFramebufferfi` was a bug fix, see the corresponding
  [issue](https://www.khronos.org/bugzilla/show_bug.cgi?id=1394) on Khronos.

2.6.0.0
-------
* Use the `Half` type from the `half` package.
* Updated OpenGL registry to r32110.

2.5.5.0
-------
* Added retrieval of version info and extensions.
* Added extension predicates.
* Added documentation for vector equivalents and aliases.
* Consistently use UTF-8 as the encoding.

2.5.4.0
-------
* Added documentation for enumerant groups.

2.5.3.0
-------
* Updated OpenGL registry to r31903.
* Added `GL_EXT_multisampled_compatibility` tokens and functions (gles2 extension only).

2.5.2.1
-------
* Added CHANGELOG.md to distribution.

2.5.2.0
-------
* Updated OpenGL registry to r31811.
* Added `ARB_ES3_2_compatibility` extension.
* Added `ARB_gpu_shader_int64` extension.
* Added `ARB_parallel_shader_compile` extension.
* Added `ARB_sample_locations` extension.
* Added `ARB_texture_filter_minmax` extension.
* Added `INTEL_framebuffer_CMAA` extension.
* Added `NV_conservative_raster_dilate` extension.
