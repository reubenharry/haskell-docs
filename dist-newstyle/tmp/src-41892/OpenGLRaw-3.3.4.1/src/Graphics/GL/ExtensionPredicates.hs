{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.ExtensionPredicates
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Extension support predicates.
--
--------------------------------------------------------------------------------

module Graphics.GL.ExtensionPredicates where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Set ( member )
import Graphics.GL.GetProcAddress ( getExtensions, extensions )

-- | Is the <https://www.opengl.org/registry/specs/3DFX/3dfx_multisample.txt 3DFX_multisample> extension supported?
glGetThreeDFXMultisample :: MonadIO m => m Bool
glGetThreeDFXMultisample = getExtensions >>= (return . member "GL_3DFX_multisample")

-- | Is the <https://www.opengl.org/registry/specs/3DFX/3dfx_multisample.txt 3DFX_multisample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetThreeDFXMultisample' in those cases instead.
gl_3DFX_multisample :: Bool
gl_3DFX_multisample = member "GL_3DFX_multisample" extensions
{-# NOINLINE gl_3DFX_multisample #-}

-- | Is the <https://www.opengl.org/registry/specs/3DFX/tbuffer.txt 3DFX_tbuffer> extension supported?
glGetThreeDFXTbuffer :: MonadIO m => m Bool
glGetThreeDFXTbuffer = getExtensions >>= (return . member "GL_3DFX_tbuffer")

-- | Is the <https://www.opengl.org/registry/specs/3DFX/tbuffer.txt 3DFX_tbuffer> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetThreeDFXTbuffer' in those cases instead.
gl_3DFX_tbuffer :: Bool
gl_3DFX_tbuffer = member "GL_3DFX_tbuffer" extensions
{-# NOINLINE gl_3DFX_tbuffer #-}

-- | Is the <https://www.opengl.org/registry/specs/3DFX/texture_compression_FXT1.txt 3DFX_texture_compression_FXT1> extension supported?
glGetThreeDFXTextureCompressionFXT1 :: MonadIO m => m Bool
glGetThreeDFXTextureCompressionFXT1 = getExtensions >>= (return . member "GL_3DFX_texture_compression_FXT1")

-- | Is the <https://www.opengl.org/registry/specs/3DFX/texture_compression_FXT1.txt 3DFX_texture_compression_FXT1> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetThreeDFXTextureCompressionFXT1' in those cases instead.
gl_3DFX_texture_compression_FXT1 :: Bool
gl_3DFX_texture_compression_FXT1 = member "GL_3DFX_texture_compression_FXT1" extensions
{-# NOINLINE gl_3DFX_texture_compression_FXT1 #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/blend_minmax_factor.txt AMD_blend_minmax_factor> extension supported?
glGetAMDBlendMinmaxFactor :: MonadIO m => m Bool
glGetAMDBlendMinmaxFactor = getExtensions >>= (return . member "GL_AMD_blend_minmax_factor")

-- | Is the <https://www.opengl.org/registry/specs/AMD/blend_minmax_factor.txt AMD_blend_minmax_factor> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDBlendMinmaxFactor' in those cases instead.
gl_AMD_blend_minmax_factor :: Bool
gl_AMD_blend_minmax_factor = member "GL_AMD_blend_minmax_factor" extensions
{-# NOINLINE gl_AMD_blend_minmax_factor #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/debug_output.txt AMD_debug_output> extension supported?
glGetAMDDebugOutput :: MonadIO m => m Bool
glGetAMDDebugOutput = getExtensions >>= (return . member "GL_AMD_debug_output")

-- | Is the <https://www.opengl.org/registry/specs/AMD/debug_output.txt AMD_debug_output> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDDebugOutput' in those cases instead.
gl_AMD_debug_output :: Bool
gl_AMD_debug_output = member "GL_AMD_debug_output" extensions
{-# NOINLINE gl_AMD_debug_output #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/depth_clamp_separate.txt AMD_depth_clamp_separate> extension supported?
glGetAMDDepthClampSeparate :: MonadIO m => m Bool
glGetAMDDepthClampSeparate = getExtensions >>= (return . member "GL_AMD_depth_clamp_separate")

-- | Is the <https://www.opengl.org/registry/specs/AMD/depth_clamp_separate.txt AMD_depth_clamp_separate> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDDepthClampSeparate' in those cases instead.
gl_AMD_depth_clamp_separate :: Bool
gl_AMD_depth_clamp_separate = member "GL_AMD_depth_clamp_separate" extensions
{-# NOINLINE gl_AMD_depth_clamp_separate #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/draw_buffers_blend.txt AMD_draw_buffers_blend> extension supported?
glGetAMDDrawBuffersBlend :: MonadIO m => m Bool
glGetAMDDrawBuffersBlend = getExtensions >>= (return . member "GL_AMD_draw_buffers_blend")

-- | Is the <https://www.opengl.org/registry/specs/AMD/draw_buffers_blend.txt AMD_draw_buffers_blend> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDDrawBuffersBlend' in those cases instead.
gl_AMD_draw_buffers_blend :: Bool
gl_AMD_draw_buffers_blend = member "GL_AMD_draw_buffers_blend" extensions
{-# NOINLINE gl_AMD_draw_buffers_blend #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/framebuffer_multisample_advanced.txt AMD_framebuffer_multisample_advanced> extension supported?
glGetAMDFramebufferMultisampleAdvanced :: MonadIO m => m Bool
glGetAMDFramebufferMultisampleAdvanced = getExtensions >>= (return . member "GL_AMD_framebuffer_multisample_advanced")

-- | Is the <https://www.opengl.org/registry/specs/AMD/framebuffer_multisample_advanced.txt AMD_framebuffer_multisample_advanced> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDFramebufferMultisampleAdvanced' in those cases instead.
gl_AMD_framebuffer_multisample_advanced :: Bool
gl_AMD_framebuffer_multisample_advanced = member "GL_AMD_framebuffer_multisample_advanced" extensions
{-# NOINLINE gl_AMD_framebuffer_multisample_advanced #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/framebuffer_sample_positions.txt AMD_framebuffer_sample_positions> extension supported?
glGetAMDFramebufferSamplePositions :: MonadIO m => m Bool
glGetAMDFramebufferSamplePositions = getExtensions >>= (return . member "GL_AMD_framebuffer_sample_positions")

-- | Is the <https://www.opengl.org/registry/specs/AMD/framebuffer_sample_positions.txt AMD_framebuffer_sample_positions> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDFramebufferSamplePositions' in those cases instead.
gl_AMD_framebuffer_sample_positions :: Bool
gl_AMD_framebuffer_sample_positions = member "GL_AMD_framebuffer_sample_positions" extensions
{-# NOINLINE gl_AMD_framebuffer_sample_positions #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/gpu_shader_half_float.txt AMD_gpu_shader_half_float> extension supported?
glGetAMDGPUShaderHalfFloat :: MonadIO m => m Bool
glGetAMDGPUShaderHalfFloat = getExtensions >>= (return . member "GL_AMD_gpu_shader_half_float")

-- | Is the <https://www.opengl.org/registry/specs/AMD/gpu_shader_half_float.txt AMD_gpu_shader_half_float> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDGPUShaderHalfFloat' in those cases instead.
gl_AMD_gpu_shader_half_float :: Bool
gl_AMD_gpu_shader_half_float = member "GL_AMD_gpu_shader_half_float" extensions
{-# NOINLINE gl_AMD_gpu_shader_half_float #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/gpu_shader_int64.txt AMD_gpu_shader_int64> extension supported?
glGetAMDGPUShaderInt64 :: MonadIO m => m Bool
glGetAMDGPUShaderInt64 = getExtensions >>= (return . member "GL_AMD_gpu_shader_int64")

-- | Is the <https://www.opengl.org/registry/specs/AMD/gpu_shader_int64.txt AMD_gpu_shader_int64> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDGPUShaderInt64' in those cases instead.
gl_AMD_gpu_shader_int64 :: Bool
gl_AMD_gpu_shader_int64 = member "GL_AMD_gpu_shader_int64" extensions
{-# NOINLINE gl_AMD_gpu_shader_int64 #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/interleaved_elements.txt AMD_interleaved_elements> extension supported?
glGetAMDInterleavedElements :: MonadIO m => m Bool
glGetAMDInterleavedElements = getExtensions >>= (return . member "GL_AMD_interleaved_elements")

-- | Is the <https://www.opengl.org/registry/specs/AMD/interleaved_elements.txt AMD_interleaved_elements> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDInterleavedElements' in those cases instead.
gl_AMD_interleaved_elements :: Bool
gl_AMD_interleaved_elements = member "GL_AMD_interleaved_elements" extensions
{-# NOINLINE gl_AMD_interleaved_elements #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/multi_draw_indirect.txt AMD_multi_draw_indirect> extension supported?
glGetAMDMultiDrawIndirect :: MonadIO m => m Bool
glGetAMDMultiDrawIndirect = getExtensions >>= (return . member "GL_AMD_multi_draw_indirect")

-- | Is the <https://www.opengl.org/registry/specs/AMD/multi_draw_indirect.txt AMD_multi_draw_indirect> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDMultiDrawIndirect' in those cases instead.
gl_AMD_multi_draw_indirect :: Bool
gl_AMD_multi_draw_indirect = member "GL_AMD_multi_draw_indirect" extensions
{-# NOINLINE gl_AMD_multi_draw_indirect #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/name_gen_delete.txt AMD_name_gen_delete> extension supported?
glGetAMDNameGenDelete :: MonadIO m => m Bool
glGetAMDNameGenDelete = getExtensions >>= (return . member "GL_AMD_name_gen_delete")

-- | Is the <https://www.opengl.org/registry/specs/AMD/name_gen_delete.txt AMD_name_gen_delete> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDNameGenDelete' in those cases instead.
gl_AMD_name_gen_delete :: Bool
gl_AMD_name_gen_delete = member "GL_AMD_name_gen_delete" extensions
{-# NOINLINE gl_AMD_name_gen_delete #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/occlusion_query_event.txt AMD_occlusion_query_event> extension supported?
glGetAMDOcclusionQueryEvent :: MonadIO m => m Bool
glGetAMDOcclusionQueryEvent = getExtensions >>= (return . member "GL_AMD_occlusion_query_event")

-- | Is the <https://www.opengl.org/registry/specs/AMD/occlusion_query_event.txt AMD_occlusion_query_event> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDOcclusionQueryEvent' in those cases instead.
gl_AMD_occlusion_query_event :: Bool
gl_AMD_occlusion_query_event = member "GL_AMD_occlusion_query_event" extensions
{-# NOINLINE gl_AMD_occlusion_query_event #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/performance_monitor.txt AMD_performance_monitor> extension supported?
glGetAMDPerformanceMonitor :: MonadIO m => m Bool
glGetAMDPerformanceMonitor = getExtensions >>= (return . member "GL_AMD_performance_monitor")

-- | Is the <https://www.opengl.org/registry/specs/AMD/performance_monitor.txt AMD_performance_monitor> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDPerformanceMonitor' in those cases instead.
gl_AMD_performance_monitor :: Bool
gl_AMD_performance_monitor = member "GL_AMD_performance_monitor" extensions
{-# NOINLINE gl_AMD_performance_monitor #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/pinned_memory.txt AMD_pinned_memory> extension supported?
glGetAMDPinnedMemory :: MonadIO m => m Bool
glGetAMDPinnedMemory = getExtensions >>= (return . member "GL_AMD_pinned_memory")

-- | Is the <https://www.opengl.org/registry/specs/AMD/pinned_memory.txt AMD_pinned_memory> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDPinnedMemory' in those cases instead.
gl_AMD_pinned_memory :: Bool
gl_AMD_pinned_memory = member "GL_AMD_pinned_memory" extensions
{-# NOINLINE gl_AMD_pinned_memory #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/query_buffer_object.txt AMD_query_buffer_object> extension supported?
glGetAMDQueryBufferObject :: MonadIO m => m Bool
glGetAMDQueryBufferObject = getExtensions >>= (return . member "GL_AMD_query_buffer_object")

-- | Is the <https://www.opengl.org/registry/specs/AMD/query_buffer_object.txt AMD_query_buffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDQueryBufferObject' in those cases instead.
gl_AMD_query_buffer_object :: Bool
gl_AMD_query_buffer_object = member "GL_AMD_query_buffer_object" extensions
{-# NOINLINE gl_AMD_query_buffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/sample_positions.txt AMD_sample_positions> extension supported?
glGetAMDSamplePositions :: MonadIO m => m Bool
glGetAMDSamplePositions = getExtensions >>= (return . member "GL_AMD_sample_positions")

-- | Is the <https://www.opengl.org/registry/specs/AMD/sample_positions.txt AMD_sample_positions> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDSamplePositions' in those cases instead.
gl_AMD_sample_positions :: Bool
gl_AMD_sample_positions = member "GL_AMD_sample_positions" extensions
{-# NOINLINE gl_AMD_sample_positions #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/seamless_cubemap_per_texture.txt AMD_seamless_cubemap_per_texture> extension supported?
glGetAMDSeamlessCubemapPerTexture :: MonadIO m => m Bool
glGetAMDSeamlessCubemapPerTexture = getExtensions >>= (return . member "GL_AMD_seamless_cubemap_per_texture")

-- | Is the <https://www.opengl.org/registry/specs/AMD/seamless_cubemap_per_texture.txt AMD_seamless_cubemap_per_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDSeamlessCubemapPerTexture' in those cases instead.
gl_AMD_seamless_cubemap_per_texture :: Bool
gl_AMD_seamless_cubemap_per_texture = member "GL_AMD_seamless_cubemap_per_texture" extensions
{-# NOINLINE gl_AMD_seamless_cubemap_per_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/sparse_texture.txt AMD_sparse_texture> extension supported?
glGetAMDSparseTexture :: MonadIO m => m Bool
glGetAMDSparseTexture = getExtensions >>= (return . member "GL_AMD_sparse_texture")

-- | Is the <https://www.opengl.org/registry/specs/AMD/sparse_texture.txt AMD_sparse_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDSparseTexture' in those cases instead.
gl_AMD_sparse_texture :: Bool
gl_AMD_sparse_texture = member "GL_AMD_sparse_texture" extensions
{-# NOINLINE gl_AMD_sparse_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/stencil_operation_extended.txt AMD_stencil_operation_extended> extension supported?
glGetAMDStencilOperationExtended :: MonadIO m => m Bool
glGetAMDStencilOperationExtended = getExtensions >>= (return . member "GL_AMD_stencil_operation_extended")

-- | Is the <https://www.opengl.org/registry/specs/AMD/stencil_operation_extended.txt AMD_stencil_operation_extended> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDStencilOperationExtended' in those cases instead.
gl_AMD_stencil_operation_extended :: Bool
gl_AMD_stencil_operation_extended = member "GL_AMD_stencil_operation_extended" extensions
{-# NOINLINE gl_AMD_stencil_operation_extended #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/transform_feedback4.txt AMD_transform_feedback4> extension supported?
glGetAMDTransformFeedback4 :: MonadIO m => m Bool
glGetAMDTransformFeedback4 = getExtensions >>= (return . member "GL_AMD_transform_feedback4")

-- | Is the <https://www.opengl.org/registry/specs/AMD/transform_feedback4.txt AMD_transform_feedback4> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDTransformFeedback4' in those cases instead.
gl_AMD_transform_feedback4 :: Bool
gl_AMD_transform_feedback4 = member "GL_AMD_transform_feedback4" extensions
{-# NOINLINE gl_AMD_transform_feedback4 #-}

-- | Is the <https://www.opengl.org/registry/specs/AMD/vertex_shader_tessellator.txt AMD_vertex_shader_tessellator> extension supported?
glGetAMDVertexShaderTessellator :: MonadIO m => m Bool
glGetAMDVertexShaderTessellator = getExtensions >>= (return . member "GL_AMD_vertex_shader_tessellator")

-- | Is the <https://www.opengl.org/registry/specs/AMD/vertex_shader_tessellator.txt AMD_vertex_shader_tessellator> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAMDVertexShaderTessellator' in those cases instead.
gl_AMD_vertex_shader_tessellator :: Bool
gl_AMD_vertex_shader_tessellator = member "GL_AMD_vertex_shader_tessellator" extensions
{-# NOINLINE gl_AMD_vertex_shader_tessellator #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/aux_depth_stencil.txt APPLE_aux_depth_stencil> extension supported?
glGetAPPLEAuxDepthStencil :: MonadIO m => m Bool
glGetAPPLEAuxDepthStencil = getExtensions >>= (return . member "GL_APPLE_aux_depth_stencil")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/aux_depth_stencil.txt APPLE_aux_depth_stencil> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLEAuxDepthStencil' in those cases instead.
gl_APPLE_aux_depth_stencil :: Bool
gl_APPLE_aux_depth_stencil = member "GL_APPLE_aux_depth_stencil" extensions
{-# NOINLINE gl_APPLE_aux_depth_stencil #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/client_storage.txt APPLE_client_storage> extension supported?
glGetAPPLEClientStorage :: MonadIO m => m Bool
glGetAPPLEClientStorage = getExtensions >>= (return . member "GL_APPLE_client_storage")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/client_storage.txt APPLE_client_storage> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLEClientStorage' in those cases instead.
gl_APPLE_client_storage :: Bool
gl_APPLE_client_storage = member "GL_APPLE_client_storage" extensions
{-# NOINLINE gl_APPLE_client_storage #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/element_array.txt APPLE_element_array> extension supported?
glGetAPPLEElementArray :: MonadIO m => m Bool
glGetAPPLEElementArray = getExtensions >>= (return . member "GL_APPLE_element_array")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/element_array.txt APPLE_element_array> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLEElementArray' in those cases instead.
gl_APPLE_element_array :: Bool
gl_APPLE_element_array = member "GL_APPLE_element_array" extensions
{-# NOINLINE gl_APPLE_element_array #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/fence.txt APPLE_fence> extension supported?
glGetAPPLEFence :: MonadIO m => m Bool
glGetAPPLEFence = getExtensions >>= (return . member "GL_APPLE_fence")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/fence.txt APPLE_fence> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLEFence' in those cases instead.
gl_APPLE_fence :: Bool
gl_APPLE_fence = member "GL_APPLE_fence" extensions
{-# NOINLINE gl_APPLE_fence #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/float_pixels.txt APPLE_float_pixels> extension supported?
glGetAPPLEFloatPixels :: MonadIO m => m Bool
glGetAPPLEFloatPixels = getExtensions >>= (return . member "GL_APPLE_float_pixels")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/float_pixels.txt APPLE_float_pixels> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLEFloatPixels' in those cases instead.
gl_APPLE_float_pixels :: Bool
gl_APPLE_float_pixels = member "GL_APPLE_float_pixels" extensions
{-# NOINLINE gl_APPLE_float_pixels #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/flush_buffer_range.txt APPLE_flush_buffer_range> extension supported?
glGetAPPLEFlushBufferRange :: MonadIO m => m Bool
glGetAPPLEFlushBufferRange = getExtensions >>= (return . member "GL_APPLE_flush_buffer_range")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/flush_buffer_range.txt APPLE_flush_buffer_range> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLEFlushBufferRange' in those cases instead.
gl_APPLE_flush_buffer_range :: Bool
gl_APPLE_flush_buffer_range = member "GL_APPLE_flush_buffer_range" extensions
{-# NOINLINE gl_APPLE_flush_buffer_range #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/object_purgeable.txt APPLE_object_purgeable> extension supported?
glGetAPPLEObjectPurgeable :: MonadIO m => m Bool
glGetAPPLEObjectPurgeable = getExtensions >>= (return . member "GL_APPLE_object_purgeable")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/object_purgeable.txt APPLE_object_purgeable> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLEObjectPurgeable' in those cases instead.
gl_APPLE_object_purgeable :: Bool
gl_APPLE_object_purgeable = member "GL_APPLE_object_purgeable" extensions
{-# NOINLINE gl_APPLE_object_purgeable #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/rgb_422.txt APPLE_rgb_422> extension supported?
glGetAPPLERGB422 :: MonadIO m => m Bool
glGetAPPLERGB422 = getExtensions >>= (return . member "GL_APPLE_rgb_422")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/rgb_422.txt APPLE_rgb_422> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLERGB422' in those cases instead.
gl_APPLE_rgb_422 :: Bool
gl_APPLE_rgb_422 = member "GL_APPLE_rgb_422" extensions
{-# NOINLINE gl_APPLE_rgb_422 #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/row_bytes.txt APPLE_row_bytes> extension supported?
glGetAPPLERowBytes :: MonadIO m => m Bool
glGetAPPLERowBytes = getExtensions >>= (return . member "GL_APPLE_row_bytes")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/row_bytes.txt APPLE_row_bytes> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLERowBytes' in those cases instead.
gl_APPLE_row_bytes :: Bool
gl_APPLE_row_bytes = member "GL_APPLE_row_bytes" extensions
{-# NOINLINE gl_APPLE_row_bytes #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/specular_vector.txt APPLE_specular_vector> extension supported?
glGetAPPLESpecularVector :: MonadIO m => m Bool
glGetAPPLESpecularVector = getExtensions >>= (return . member "GL_APPLE_specular_vector")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/specular_vector.txt APPLE_specular_vector> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLESpecularVector' in those cases instead.
gl_APPLE_specular_vector :: Bool
gl_APPLE_specular_vector = member "GL_APPLE_specular_vector" extensions
{-# NOINLINE gl_APPLE_specular_vector #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/texture_range.txt APPLE_texture_range> extension supported?
glGetAPPLETextureRange :: MonadIO m => m Bool
glGetAPPLETextureRange = getExtensions >>= (return . member "GL_APPLE_texture_range")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/texture_range.txt APPLE_texture_range> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLETextureRange' in those cases instead.
gl_APPLE_texture_range :: Bool
gl_APPLE_texture_range = member "GL_APPLE_texture_range" extensions
{-# NOINLINE gl_APPLE_texture_range #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/transform_hint.txt APPLE_transform_hint> extension supported?
glGetAPPLETransformHint :: MonadIO m => m Bool
glGetAPPLETransformHint = getExtensions >>= (return . member "GL_APPLE_transform_hint")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/transform_hint.txt APPLE_transform_hint> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLETransformHint' in those cases instead.
gl_APPLE_transform_hint :: Bool
gl_APPLE_transform_hint = member "GL_APPLE_transform_hint" extensions
{-# NOINLINE gl_APPLE_transform_hint #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/vertex_array_object.txt APPLE_vertex_array_object> extension supported?
glGetAPPLEVertexArrayObject :: MonadIO m => m Bool
glGetAPPLEVertexArrayObject = getExtensions >>= (return . member "GL_APPLE_vertex_array_object")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/vertex_array_object.txt APPLE_vertex_array_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLEVertexArrayObject' in those cases instead.
gl_APPLE_vertex_array_object :: Bool
gl_APPLE_vertex_array_object = member "GL_APPLE_vertex_array_object" extensions
{-# NOINLINE gl_APPLE_vertex_array_object #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/vertex_array_range.txt APPLE_vertex_array_range> extension supported?
glGetAPPLEVertexArrayRange :: MonadIO m => m Bool
glGetAPPLEVertexArrayRange = getExtensions >>= (return . member "GL_APPLE_vertex_array_range")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/vertex_array_range.txt APPLE_vertex_array_range> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLEVertexArrayRange' in those cases instead.
gl_APPLE_vertex_array_range :: Bool
gl_APPLE_vertex_array_range = member "GL_APPLE_vertex_array_range" extensions
{-# NOINLINE gl_APPLE_vertex_array_range #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt APPLE_vertex_program_evaluators> extension supported?
glGetAPPLEVertexProgramEvaluators :: MonadIO m => m Bool
glGetAPPLEVertexProgramEvaluators = getExtensions >>= (return . member "GL_APPLE_vertex_program_evaluators")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/vertex_program_evaluators.txt APPLE_vertex_program_evaluators> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLEVertexProgramEvaluators' in those cases instead.
gl_APPLE_vertex_program_evaluators :: Bool
gl_APPLE_vertex_program_evaluators = member "GL_APPLE_vertex_program_evaluators" extensions
{-# NOINLINE gl_APPLE_vertex_program_evaluators #-}

-- | Is the <https://www.opengl.org/registry/specs/APPLE/ycbcr_422.txt APPLE_ycbcr_422> extension supported?
glGetAPPLEYCbCr422 :: MonadIO m => m Bool
glGetAPPLEYCbCr422 = getExtensions >>= (return . member "GL_APPLE_ycbcr_422")

-- | Is the <https://www.opengl.org/registry/specs/APPLE/ycbcr_422.txt APPLE_ycbcr_422> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetAPPLEYCbCr422' in those cases instead.
gl_APPLE_ycbcr_422 :: Bool
gl_APPLE_ycbcr_422 = member "GL_APPLE_ycbcr_422" extensions
{-# NOINLINE gl_APPLE_ycbcr_422 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/ES2_compatibility.txt ARB_ES2_compatibility> extension supported?
glGetARBES2Compatibility :: MonadIO m => m Bool
glGetARBES2Compatibility = getExtensions >>= (return . member "GL_ARB_ES2_compatibility")

-- | Is the <https://www.opengl.org/registry/specs/ARB/ES2_compatibility.txt ARB_ES2_compatibility> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBES2Compatibility' in those cases instead.
gl_ARB_ES2_compatibility :: Bool
gl_ARB_ES2_compatibility = member "GL_ARB_ES2_compatibility" extensions
{-# NOINLINE gl_ARB_ES2_compatibility #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/ES3_1_compatibility.txt ARB_ES3_1_compatibility> extension supported?
glGetARBES31Compatibility :: MonadIO m => m Bool
glGetARBES31Compatibility = getExtensions >>= (return . member "GL_ARB_ES3_1_compatibility")

-- | Is the <https://www.opengl.org/registry/specs/ARB/ES3_1_compatibility.txt ARB_ES3_1_compatibility> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBES31Compatibility' in those cases instead.
gl_ARB_ES3_1_compatibility :: Bool
gl_ARB_ES3_1_compatibility = member "GL_ARB_ES3_1_compatibility" extensions
{-# NOINLINE gl_ARB_ES3_1_compatibility #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/ES3_2_compatibility.txt ARB_ES3_2_compatibility> extension supported?
glGetARBES32Compatibility :: MonadIO m => m Bool
glGetARBES32Compatibility = getExtensions >>= (return . member "GL_ARB_ES3_2_compatibility")

-- | Is the <https://www.opengl.org/registry/specs/ARB/ES3_2_compatibility.txt ARB_ES3_2_compatibility> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBES32Compatibility' in those cases instead.
gl_ARB_ES3_2_compatibility :: Bool
gl_ARB_ES3_2_compatibility = member "GL_ARB_ES3_2_compatibility" extensions
{-# NOINLINE gl_ARB_ES3_2_compatibility #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/ES3_compatibility.txt ARB_ES3_compatibility> extension supported?
glGetARBES3Compatibility :: MonadIO m => m Bool
glGetARBES3Compatibility = getExtensions >>= (return . member "GL_ARB_ES3_compatibility")

-- | Is the <https://www.opengl.org/registry/specs/ARB/ES3_compatibility.txt ARB_ES3_compatibility> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBES3Compatibility' in those cases instead.
gl_ARB_ES3_compatibility :: Bool
gl_ARB_ES3_compatibility = member "GL_ARB_ES3_compatibility" extensions
{-# NOINLINE gl_ARB_ES3_compatibility #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/base_instance.txt ARB_base_instance> extension supported?
glGetARBBaseInstance :: MonadIO m => m Bool
glGetARBBaseInstance = getExtensions >>= (return . member "GL_ARB_base_instance")

-- | Is the <https://www.opengl.org/registry/specs/ARB/base_instance.txt ARB_base_instance> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBBaseInstance' in those cases instead.
gl_ARB_base_instance :: Bool
gl_ARB_base_instance = member "GL_ARB_base_instance" extensions
{-# NOINLINE gl_ARB_base_instance #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/bindless_texture.txt ARB_bindless_texture> extension supported?
glGetARBBindlessTexture :: MonadIO m => m Bool
glGetARBBindlessTexture = getExtensions >>= (return . member "GL_ARB_bindless_texture")

-- | Is the <https://www.opengl.org/registry/specs/ARB/bindless_texture.txt ARB_bindless_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBBindlessTexture' in those cases instead.
gl_ARB_bindless_texture :: Bool
gl_ARB_bindless_texture = member "GL_ARB_bindless_texture" extensions
{-# NOINLINE gl_ARB_bindless_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/blend_func_extended.txt ARB_blend_func_extended> extension supported?
glGetARBBlendFuncExtended :: MonadIO m => m Bool
glGetARBBlendFuncExtended = getExtensions >>= (return . member "GL_ARB_blend_func_extended")

-- | Is the <https://www.opengl.org/registry/specs/ARB/blend_func_extended.txt ARB_blend_func_extended> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBBlendFuncExtended' in those cases instead.
gl_ARB_blend_func_extended :: Bool
gl_ARB_blend_func_extended = member "GL_ARB_blend_func_extended" extensions
{-# NOINLINE gl_ARB_blend_func_extended #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/buffer_storage.txt ARB_buffer_storage> extension supported?
glGetARBBufferStorage :: MonadIO m => m Bool
glGetARBBufferStorage = getExtensions >>= (return . member "GL_ARB_buffer_storage")

-- | Is the <https://www.opengl.org/registry/specs/ARB/buffer_storage.txt ARB_buffer_storage> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBBufferStorage' in those cases instead.
gl_ARB_buffer_storage :: Bool
gl_ARB_buffer_storage = member "GL_ARB_buffer_storage" extensions
{-# NOINLINE gl_ARB_buffer_storage #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/cl_event.txt ARB_cl_event> extension supported?
glGetARBCLEvent :: MonadIO m => m Bool
glGetARBCLEvent = getExtensions >>= (return . member "GL_ARB_cl_event")

-- | Is the <https://www.opengl.org/registry/specs/ARB/cl_event.txt ARB_cl_event> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBCLEvent' in those cases instead.
gl_ARB_cl_event :: Bool
gl_ARB_cl_event = member "GL_ARB_cl_event" extensions
{-# NOINLINE gl_ARB_cl_event #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/clear_buffer_object.txt ARB_clear_buffer_object> extension supported?
glGetARBClearBufferObject :: MonadIO m => m Bool
glGetARBClearBufferObject = getExtensions >>= (return . member "GL_ARB_clear_buffer_object")

-- | Is the <https://www.opengl.org/registry/specs/ARB/clear_buffer_object.txt ARB_clear_buffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBClearBufferObject' in those cases instead.
gl_ARB_clear_buffer_object :: Bool
gl_ARB_clear_buffer_object = member "GL_ARB_clear_buffer_object" extensions
{-# NOINLINE gl_ARB_clear_buffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/clear_texture.txt ARB_clear_texture> extension supported?
glGetARBClearTexture :: MonadIO m => m Bool
glGetARBClearTexture = getExtensions >>= (return . member "GL_ARB_clear_texture")

-- | Is the <https://www.opengl.org/registry/specs/ARB/clear_texture.txt ARB_clear_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBClearTexture' in those cases instead.
gl_ARB_clear_texture :: Bool
gl_ARB_clear_texture = member "GL_ARB_clear_texture" extensions
{-# NOINLINE gl_ARB_clear_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/clip_control.txt ARB_clip_control> extension supported?
glGetARBClipControl :: MonadIO m => m Bool
glGetARBClipControl = getExtensions >>= (return . member "GL_ARB_clip_control")

-- | Is the <https://www.opengl.org/registry/specs/ARB/clip_control.txt ARB_clip_control> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBClipControl' in those cases instead.
gl_ARB_clip_control :: Bool
gl_ARB_clip_control = member "GL_ARB_clip_control" extensions
{-# NOINLINE gl_ARB_clip_control #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/color_buffer_float.txt ARB_color_buffer_float> extension supported?
glGetARBColorBufferFloat :: MonadIO m => m Bool
glGetARBColorBufferFloat = getExtensions >>= (return . member "GL_ARB_color_buffer_float")

-- | Is the <https://www.opengl.org/registry/specs/ARB/color_buffer_float.txt ARB_color_buffer_float> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBColorBufferFloat' in those cases instead.
gl_ARB_color_buffer_float :: Bool
gl_ARB_color_buffer_float = member "GL_ARB_color_buffer_float" extensions
{-# NOINLINE gl_ARB_color_buffer_float #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/compressed_texture_pixel_storage.txt ARB_compressed_texture_pixel_storage> extension supported?
glGetARBCompressedTexturePixelStorage :: MonadIO m => m Bool
glGetARBCompressedTexturePixelStorage = getExtensions >>= (return . member "GL_ARB_compressed_texture_pixel_storage")

-- | Is the <https://www.opengl.org/registry/specs/ARB/compressed_texture_pixel_storage.txt ARB_compressed_texture_pixel_storage> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBCompressedTexturePixelStorage' in those cases instead.
gl_ARB_compressed_texture_pixel_storage :: Bool
gl_ARB_compressed_texture_pixel_storage = member "GL_ARB_compressed_texture_pixel_storage" extensions
{-# NOINLINE gl_ARB_compressed_texture_pixel_storage #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/compute_shader.txt ARB_compute_shader> extension supported?
glGetARBComputeShader :: MonadIO m => m Bool
glGetARBComputeShader = getExtensions >>= (return . member "GL_ARB_compute_shader")

-- | Is the <https://www.opengl.org/registry/specs/ARB/compute_shader.txt ARB_compute_shader> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBComputeShader' in those cases instead.
gl_ARB_compute_shader :: Bool
gl_ARB_compute_shader = member "GL_ARB_compute_shader" extensions
{-# NOINLINE gl_ARB_compute_shader #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/compute_variable_group_size.txt ARB_compute_variable_group_size> extension supported?
glGetARBComputeVariableGroupSize :: MonadIO m => m Bool
glGetARBComputeVariableGroupSize = getExtensions >>= (return . member "GL_ARB_compute_variable_group_size")

-- | Is the <https://www.opengl.org/registry/specs/ARB/compute_variable_group_size.txt ARB_compute_variable_group_size> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBComputeVariableGroupSize' in those cases instead.
gl_ARB_compute_variable_group_size :: Bool
gl_ARB_compute_variable_group_size = member "GL_ARB_compute_variable_group_size" extensions
{-# NOINLINE gl_ARB_compute_variable_group_size #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/conditional_render_inverted.txt ARB_conditional_render_inverted> extension supported?
glGetARBConditionalRenderInverted :: MonadIO m => m Bool
glGetARBConditionalRenderInverted = getExtensions >>= (return . member "GL_ARB_conditional_render_inverted")

-- | Is the <https://www.opengl.org/registry/specs/ARB/conditional_render_inverted.txt ARB_conditional_render_inverted> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBConditionalRenderInverted' in those cases instead.
gl_ARB_conditional_render_inverted :: Bool
gl_ARB_conditional_render_inverted = member "GL_ARB_conditional_render_inverted" extensions
{-# NOINLINE gl_ARB_conditional_render_inverted #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/copy_buffer.txt ARB_copy_buffer> extension supported?
glGetARBCopyBuffer :: MonadIO m => m Bool
glGetARBCopyBuffer = getExtensions >>= (return . member "GL_ARB_copy_buffer")

-- | Is the <https://www.opengl.org/registry/specs/ARB/copy_buffer.txt ARB_copy_buffer> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBCopyBuffer' in those cases instead.
gl_ARB_copy_buffer :: Bool
gl_ARB_copy_buffer = member "GL_ARB_copy_buffer" extensions
{-# NOINLINE gl_ARB_copy_buffer #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/copy_image.txt ARB_copy_image> extension supported?
glGetARBCopyImage :: MonadIO m => m Bool
glGetARBCopyImage = getExtensions >>= (return . member "GL_ARB_copy_image")

-- | Is the <https://www.opengl.org/registry/specs/ARB/copy_image.txt ARB_copy_image> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBCopyImage' in those cases instead.
gl_ARB_copy_image :: Bool
gl_ARB_copy_image = member "GL_ARB_copy_image" extensions
{-# NOINLINE gl_ARB_copy_image #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/cull_distance.txt ARB_cull_distance> extension supported?
glGetARBCullDistance :: MonadIO m => m Bool
glGetARBCullDistance = getExtensions >>= (return . member "GL_ARB_cull_distance")

-- | Is the <https://www.opengl.org/registry/specs/ARB/cull_distance.txt ARB_cull_distance> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBCullDistance' in those cases instead.
gl_ARB_cull_distance :: Bool
gl_ARB_cull_distance = member "GL_ARB_cull_distance" extensions
{-# NOINLINE gl_ARB_cull_distance #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/debug_output.txt ARB_debug_output> extension supported?
glGetARBDebugOutput :: MonadIO m => m Bool
glGetARBDebugOutput = getExtensions >>= (return . member "GL_ARB_debug_output")

-- | Is the <https://www.opengl.org/registry/specs/ARB/debug_output.txt ARB_debug_output> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBDebugOutput' in those cases instead.
gl_ARB_debug_output :: Bool
gl_ARB_debug_output = member "GL_ARB_debug_output" extensions
{-# NOINLINE gl_ARB_debug_output #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/depth_buffer_float.txt ARB_depth_buffer_float> extension supported?
glGetARBDepthBufferFloat :: MonadIO m => m Bool
glGetARBDepthBufferFloat = getExtensions >>= (return . member "GL_ARB_depth_buffer_float")

-- | Is the <https://www.opengl.org/registry/specs/ARB/depth_buffer_float.txt ARB_depth_buffer_float> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBDepthBufferFloat' in those cases instead.
gl_ARB_depth_buffer_float :: Bool
gl_ARB_depth_buffer_float = member "GL_ARB_depth_buffer_float" extensions
{-# NOINLINE gl_ARB_depth_buffer_float #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/depth_clamp.txt ARB_depth_clamp> extension supported?
glGetARBDepthClamp :: MonadIO m => m Bool
glGetARBDepthClamp = getExtensions >>= (return . member "GL_ARB_depth_clamp")

-- | Is the <https://www.opengl.org/registry/specs/ARB/depth_clamp.txt ARB_depth_clamp> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBDepthClamp' in those cases instead.
gl_ARB_depth_clamp :: Bool
gl_ARB_depth_clamp = member "GL_ARB_depth_clamp" extensions
{-# NOINLINE gl_ARB_depth_clamp #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/depth_texture.txt ARB_depth_texture> extension supported?
glGetARBDepthTexture :: MonadIO m => m Bool
glGetARBDepthTexture = getExtensions >>= (return . member "GL_ARB_depth_texture")

-- | Is the <https://www.opengl.org/registry/specs/ARB/depth_texture.txt ARB_depth_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBDepthTexture' in those cases instead.
gl_ARB_depth_texture :: Bool
gl_ARB_depth_texture = member "GL_ARB_depth_texture" extensions
{-# NOINLINE gl_ARB_depth_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/direct_state_access.txt ARB_direct_state_access> extension supported?
glGetARBDirectStateAccess :: MonadIO m => m Bool
glGetARBDirectStateAccess = getExtensions >>= (return . member "GL_ARB_direct_state_access")

-- | Is the <https://www.opengl.org/registry/specs/ARB/direct_state_access.txt ARB_direct_state_access> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBDirectStateAccess' in those cases instead.
gl_ARB_direct_state_access :: Bool
gl_ARB_direct_state_access = member "GL_ARB_direct_state_access" extensions
{-# NOINLINE gl_ARB_direct_state_access #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/draw_buffers.txt ARB_draw_buffers> extension supported?
glGetARBDrawBuffers :: MonadIO m => m Bool
glGetARBDrawBuffers = getExtensions >>= (return . member "GL_ARB_draw_buffers")

-- | Is the <https://www.opengl.org/registry/specs/ARB/draw_buffers.txt ARB_draw_buffers> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBDrawBuffers' in those cases instead.
gl_ARB_draw_buffers :: Bool
gl_ARB_draw_buffers = member "GL_ARB_draw_buffers" extensions
{-# NOINLINE gl_ARB_draw_buffers #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/draw_buffers_blend.txt ARB_draw_buffers_blend> extension supported?
glGetARBDrawBuffersBlend :: MonadIO m => m Bool
glGetARBDrawBuffersBlend = getExtensions >>= (return . member "GL_ARB_draw_buffers_blend")

-- | Is the <https://www.opengl.org/registry/specs/ARB/draw_buffers_blend.txt ARB_draw_buffers_blend> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBDrawBuffersBlend' in those cases instead.
gl_ARB_draw_buffers_blend :: Bool
gl_ARB_draw_buffers_blend = member "GL_ARB_draw_buffers_blend" extensions
{-# NOINLINE gl_ARB_draw_buffers_blend #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/draw_elements_base_vertex.txt ARB_draw_elements_base_vertex> extension supported?
glGetARBDrawElementsBaseVertex :: MonadIO m => m Bool
glGetARBDrawElementsBaseVertex = getExtensions >>= (return . member "GL_ARB_draw_elements_base_vertex")

-- | Is the <https://www.opengl.org/registry/specs/ARB/draw_elements_base_vertex.txt ARB_draw_elements_base_vertex> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBDrawElementsBaseVertex' in those cases instead.
gl_ARB_draw_elements_base_vertex :: Bool
gl_ARB_draw_elements_base_vertex = member "GL_ARB_draw_elements_base_vertex" extensions
{-# NOINLINE gl_ARB_draw_elements_base_vertex #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/draw_indirect.txt ARB_draw_indirect> extension supported?
glGetARBDrawIndirect :: MonadIO m => m Bool
glGetARBDrawIndirect = getExtensions >>= (return . member "GL_ARB_draw_indirect")

-- | Is the <https://www.opengl.org/registry/specs/ARB/draw_indirect.txt ARB_draw_indirect> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBDrawIndirect' in those cases instead.
gl_ARB_draw_indirect :: Bool
gl_ARB_draw_indirect = member "GL_ARB_draw_indirect" extensions
{-# NOINLINE gl_ARB_draw_indirect #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/draw_instanced.txt ARB_draw_instanced> extension supported?
glGetARBDrawInstanced :: MonadIO m => m Bool
glGetARBDrawInstanced = getExtensions >>= (return . member "GL_ARB_draw_instanced")

-- | Is the <https://www.opengl.org/registry/specs/ARB/draw_instanced.txt ARB_draw_instanced> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBDrawInstanced' in those cases instead.
gl_ARB_draw_instanced :: Bool
gl_ARB_draw_instanced = member "GL_ARB_draw_instanced" extensions
{-# NOINLINE gl_ARB_draw_instanced #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/enhanced_layouts.txt ARB_enhanced_layouts> extension supported?
glGetARBEnhancedLayouts :: MonadIO m => m Bool
glGetARBEnhancedLayouts = getExtensions >>= (return . member "GL_ARB_enhanced_layouts")

-- | Is the <https://www.opengl.org/registry/specs/ARB/enhanced_layouts.txt ARB_enhanced_layouts> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBEnhancedLayouts' in those cases instead.
gl_ARB_enhanced_layouts :: Bool
gl_ARB_enhanced_layouts = member "GL_ARB_enhanced_layouts" extensions
{-# NOINLINE gl_ARB_enhanced_layouts #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/explicit_uniform_location.txt ARB_explicit_uniform_location> extension supported?
glGetARBExplicitUniformLocation :: MonadIO m => m Bool
glGetARBExplicitUniformLocation = getExtensions >>= (return . member "GL_ARB_explicit_uniform_location")

-- | Is the <https://www.opengl.org/registry/specs/ARB/explicit_uniform_location.txt ARB_explicit_uniform_location> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBExplicitUniformLocation' in those cases instead.
gl_ARB_explicit_uniform_location :: Bool
gl_ARB_explicit_uniform_location = member "GL_ARB_explicit_uniform_location" extensions
{-# NOINLINE gl_ARB_explicit_uniform_location #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/fragment_program.txt ARB_fragment_program> extension supported?
glGetARBFragmentProgram :: MonadIO m => m Bool
glGetARBFragmentProgram = getExtensions >>= (return . member "GL_ARB_fragment_program")

-- | Is the <https://www.opengl.org/registry/specs/ARB/fragment_program.txt ARB_fragment_program> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBFragmentProgram' in those cases instead.
gl_ARB_fragment_program :: Bool
gl_ARB_fragment_program = member "GL_ARB_fragment_program" extensions
{-# NOINLINE gl_ARB_fragment_program #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/fragment_shader.txt ARB_fragment_shader> extension supported?
glGetARBFragmentShader :: MonadIO m => m Bool
glGetARBFragmentShader = getExtensions >>= (return . member "GL_ARB_fragment_shader")

-- | Is the <https://www.opengl.org/registry/specs/ARB/fragment_shader.txt ARB_fragment_shader> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBFragmentShader' in those cases instead.
gl_ARB_fragment_shader :: Bool
gl_ARB_fragment_shader = member "GL_ARB_fragment_shader" extensions
{-# NOINLINE gl_ARB_fragment_shader #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/framebuffer_no_attachments.txt ARB_framebuffer_no_attachments> extension supported?
glGetARBFramebufferNoAttachments :: MonadIO m => m Bool
glGetARBFramebufferNoAttachments = getExtensions >>= (return . member "GL_ARB_framebuffer_no_attachments")

-- | Is the <https://www.opengl.org/registry/specs/ARB/framebuffer_no_attachments.txt ARB_framebuffer_no_attachments> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBFramebufferNoAttachments' in those cases instead.
gl_ARB_framebuffer_no_attachments :: Bool
gl_ARB_framebuffer_no_attachments = member "GL_ARB_framebuffer_no_attachments" extensions
{-# NOINLINE gl_ARB_framebuffer_no_attachments #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/framebuffer_object.txt ARB_framebuffer_object> extension supported?
glGetARBFramebufferObject :: MonadIO m => m Bool
glGetARBFramebufferObject = getExtensions >>= (return . member "GL_ARB_framebuffer_object")

-- | Is the <https://www.opengl.org/registry/specs/ARB/framebuffer_object.txt ARB_framebuffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBFramebufferObject' in those cases instead.
gl_ARB_framebuffer_object :: Bool
gl_ARB_framebuffer_object = member "GL_ARB_framebuffer_object" extensions
{-# NOINLINE gl_ARB_framebuffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/framebuffer_sRGB.txt ARB_framebuffer_sRGB> extension supported?
glGetARBFramebufferSRGB :: MonadIO m => m Bool
glGetARBFramebufferSRGB = getExtensions >>= (return . member "GL_ARB_framebuffer_sRGB")

-- | Is the <https://www.opengl.org/registry/specs/ARB/framebuffer_sRGB.txt ARB_framebuffer_sRGB> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBFramebufferSRGB' in those cases instead.
gl_ARB_framebuffer_sRGB :: Bool
gl_ARB_framebuffer_sRGB = member "GL_ARB_framebuffer_sRGB" extensions
{-# NOINLINE gl_ARB_framebuffer_sRGB #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/geometry_shader4.txt ARB_geometry_shader4> extension supported?
glGetARBGeometryShader4 :: MonadIO m => m Bool
glGetARBGeometryShader4 = getExtensions >>= (return . member "GL_ARB_geometry_shader4")

-- | Is the <https://www.opengl.org/registry/specs/ARB/geometry_shader4.txt ARB_geometry_shader4> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBGeometryShader4' in those cases instead.
gl_ARB_geometry_shader4 :: Bool
gl_ARB_geometry_shader4 = member "GL_ARB_geometry_shader4" extensions
{-# NOINLINE gl_ARB_geometry_shader4 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/get_program_binary.txt ARB_get_program_binary> extension supported?
glGetARBGetProgramBinary :: MonadIO m => m Bool
glGetARBGetProgramBinary = getExtensions >>= (return . member "GL_ARB_get_program_binary")

-- | Is the <https://www.opengl.org/registry/specs/ARB/get_program_binary.txt ARB_get_program_binary> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBGetProgramBinary' in those cases instead.
gl_ARB_get_program_binary :: Bool
gl_ARB_get_program_binary = member "GL_ARB_get_program_binary" extensions
{-# NOINLINE gl_ARB_get_program_binary #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/get_texture_sub_image.txt ARB_get_texture_sub_image> extension supported?
glGetARBGetTextureSubImage :: MonadIO m => m Bool
glGetARBGetTextureSubImage = getExtensions >>= (return . member "GL_ARB_get_texture_sub_image")

-- | Is the <https://www.opengl.org/registry/specs/ARB/get_texture_sub_image.txt ARB_get_texture_sub_image> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBGetTextureSubImage' in those cases instead.
gl_ARB_get_texture_sub_image :: Bool
gl_ARB_get_texture_sub_image = member "GL_ARB_get_texture_sub_image" extensions
{-# NOINLINE gl_ARB_get_texture_sub_image #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/gl_spirv.txt ARB_gl_spirv> extension supported?
glGetARBGlSpirv :: MonadIO m => m Bool
glGetARBGlSpirv = getExtensions >>= (return . member "GL_ARB_gl_spirv")

-- | Is the <https://www.opengl.org/registry/specs/ARB/gl_spirv.txt ARB_gl_spirv> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBGlSpirv' in those cases instead.
gl_ARB_gl_spirv :: Bool
gl_ARB_gl_spirv = member "GL_ARB_gl_spirv" extensions
{-# NOINLINE gl_ARB_gl_spirv #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/gpu_shader5.txt ARB_gpu_shader5> extension supported?
glGetARBGPUShader5 :: MonadIO m => m Bool
glGetARBGPUShader5 = getExtensions >>= (return . member "GL_ARB_gpu_shader5")

-- | Is the <https://www.opengl.org/registry/specs/ARB/gpu_shader5.txt ARB_gpu_shader5> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBGPUShader5' in those cases instead.
gl_ARB_gpu_shader5 :: Bool
gl_ARB_gpu_shader5 = member "GL_ARB_gpu_shader5" extensions
{-# NOINLINE gl_ARB_gpu_shader5 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/gpu_shader_fp64.txt ARB_gpu_shader_fp64> extension supported?
glGetARBGPUShaderFP64 :: MonadIO m => m Bool
glGetARBGPUShaderFP64 = getExtensions >>= (return . member "GL_ARB_gpu_shader_fp64")

-- | Is the <https://www.opengl.org/registry/specs/ARB/gpu_shader_fp64.txt ARB_gpu_shader_fp64> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBGPUShaderFP64' in those cases instead.
gl_ARB_gpu_shader_fp64 :: Bool
gl_ARB_gpu_shader_fp64 = member "GL_ARB_gpu_shader_fp64" extensions
{-# NOINLINE gl_ARB_gpu_shader_fp64 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/gpu_shader_int64.txt ARB_gpu_shader_int64> extension supported?
glGetARBGPUShaderInt64 :: MonadIO m => m Bool
glGetARBGPUShaderInt64 = getExtensions >>= (return . member "GL_ARB_gpu_shader_int64")

-- | Is the <https://www.opengl.org/registry/specs/ARB/gpu_shader_int64.txt ARB_gpu_shader_int64> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBGPUShaderInt64' in those cases instead.
gl_ARB_gpu_shader_int64 :: Bool
gl_ARB_gpu_shader_int64 = member "GL_ARB_gpu_shader_int64" extensions
{-# NOINLINE gl_ARB_gpu_shader_int64 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/half_float_pixel.txt ARB_half_float_pixel> extension supported?
glGetARBHalfFloatPixel :: MonadIO m => m Bool
glGetARBHalfFloatPixel = getExtensions >>= (return . member "GL_ARB_half_float_pixel")

-- | Is the <https://www.opengl.org/registry/specs/ARB/half_float_pixel.txt ARB_half_float_pixel> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBHalfFloatPixel' in those cases instead.
gl_ARB_half_float_pixel :: Bool
gl_ARB_half_float_pixel = member "GL_ARB_half_float_pixel" extensions
{-# NOINLINE gl_ARB_half_float_pixel #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/half_float_vertex.txt ARB_half_float_vertex> extension supported?
glGetARBHalfFloatVertex :: MonadIO m => m Bool
glGetARBHalfFloatVertex = getExtensions >>= (return . member "GL_ARB_half_float_vertex")

-- | Is the <https://www.opengl.org/registry/specs/ARB/half_float_vertex.txt ARB_half_float_vertex> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBHalfFloatVertex' in those cases instead.
gl_ARB_half_float_vertex :: Bool
gl_ARB_half_float_vertex = member "GL_ARB_half_float_vertex" extensions
{-# NOINLINE gl_ARB_half_float_vertex #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/imaging.txt ARB_imaging> extension supported?
glGetARBImaging :: MonadIO m => m Bool
glGetARBImaging = getExtensions >>= (return . member "GL_ARB_imaging")

-- | Is the <https://www.opengl.org/registry/specs/ARB/imaging.txt ARB_imaging> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBImaging' in those cases instead.
gl_ARB_imaging :: Bool
gl_ARB_imaging = member "GL_ARB_imaging" extensions
{-# NOINLINE gl_ARB_imaging #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/indirect_parameters.txt ARB_indirect_parameters> extension supported?
glGetARBIndirectParameters :: MonadIO m => m Bool
glGetARBIndirectParameters = getExtensions >>= (return . member "GL_ARB_indirect_parameters")

-- | Is the <https://www.opengl.org/registry/specs/ARB/indirect_parameters.txt ARB_indirect_parameters> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBIndirectParameters' in those cases instead.
gl_ARB_indirect_parameters :: Bool
gl_ARB_indirect_parameters = member "GL_ARB_indirect_parameters" extensions
{-# NOINLINE gl_ARB_indirect_parameters #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/instanced_arrays.txt ARB_instanced_arrays> extension supported?
glGetARBInstancedArrays :: MonadIO m => m Bool
glGetARBInstancedArrays = getExtensions >>= (return . member "GL_ARB_instanced_arrays")

-- | Is the <https://www.opengl.org/registry/specs/ARB/instanced_arrays.txt ARB_instanced_arrays> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBInstancedArrays' in those cases instead.
gl_ARB_instanced_arrays :: Bool
gl_ARB_instanced_arrays = member "GL_ARB_instanced_arrays" extensions
{-# NOINLINE gl_ARB_instanced_arrays #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/internalformat_query.txt ARB_internalformat_query> extension supported?
glGetARBInternalformatQuery :: MonadIO m => m Bool
glGetARBInternalformatQuery = getExtensions >>= (return . member "GL_ARB_internalformat_query")

-- | Is the <https://www.opengl.org/registry/specs/ARB/internalformat_query.txt ARB_internalformat_query> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBInternalformatQuery' in those cases instead.
gl_ARB_internalformat_query :: Bool
gl_ARB_internalformat_query = member "GL_ARB_internalformat_query" extensions
{-# NOINLINE gl_ARB_internalformat_query #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/internalformat_query2.txt ARB_internalformat_query2> extension supported?
glGetARBInternalformatQuery2 :: MonadIO m => m Bool
glGetARBInternalformatQuery2 = getExtensions >>= (return . member "GL_ARB_internalformat_query2")

-- | Is the <https://www.opengl.org/registry/specs/ARB/internalformat_query2.txt ARB_internalformat_query2> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBInternalformatQuery2' in those cases instead.
gl_ARB_internalformat_query2 :: Bool
gl_ARB_internalformat_query2 = member "GL_ARB_internalformat_query2" extensions
{-# NOINLINE gl_ARB_internalformat_query2 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/invalidate_subdata.txt ARB_invalidate_subdata> extension supported?
glGetARBInvalidateSubdata :: MonadIO m => m Bool
glGetARBInvalidateSubdata = getExtensions >>= (return . member "GL_ARB_invalidate_subdata")

-- | Is the <https://www.opengl.org/registry/specs/ARB/invalidate_subdata.txt ARB_invalidate_subdata> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBInvalidateSubdata' in those cases instead.
gl_ARB_invalidate_subdata :: Bool
gl_ARB_invalidate_subdata = member "GL_ARB_invalidate_subdata" extensions
{-# NOINLINE gl_ARB_invalidate_subdata #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/map_buffer_alignment.txt ARB_map_buffer_alignment> extension supported?
glGetARBMapBufferAlignment :: MonadIO m => m Bool
glGetARBMapBufferAlignment = getExtensions >>= (return . member "GL_ARB_map_buffer_alignment")

-- | Is the <https://www.opengl.org/registry/specs/ARB/map_buffer_alignment.txt ARB_map_buffer_alignment> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBMapBufferAlignment' in those cases instead.
gl_ARB_map_buffer_alignment :: Bool
gl_ARB_map_buffer_alignment = member "GL_ARB_map_buffer_alignment" extensions
{-# NOINLINE gl_ARB_map_buffer_alignment #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/map_buffer_range.txt ARB_map_buffer_range> extension supported?
glGetARBMapBufferRange :: MonadIO m => m Bool
glGetARBMapBufferRange = getExtensions >>= (return . member "GL_ARB_map_buffer_range")

-- | Is the <https://www.opengl.org/registry/specs/ARB/map_buffer_range.txt ARB_map_buffer_range> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBMapBufferRange' in those cases instead.
gl_ARB_map_buffer_range :: Bool
gl_ARB_map_buffer_range = member "GL_ARB_map_buffer_range" extensions
{-# NOINLINE gl_ARB_map_buffer_range #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/matrix_palette.txt ARB_matrix_palette> extension supported?
glGetARBMatrixPalette :: MonadIO m => m Bool
glGetARBMatrixPalette = getExtensions >>= (return . member "GL_ARB_matrix_palette")

-- | Is the <https://www.opengl.org/registry/specs/ARB/matrix_palette.txt ARB_matrix_palette> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBMatrixPalette' in those cases instead.
gl_ARB_matrix_palette :: Bool
gl_ARB_matrix_palette = member "GL_ARB_matrix_palette" extensions
{-# NOINLINE gl_ARB_matrix_palette #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/multi_bind.txt ARB_multi_bind> extension supported?
glGetARBMultiBind :: MonadIO m => m Bool
glGetARBMultiBind = getExtensions >>= (return . member "GL_ARB_multi_bind")

-- | Is the <https://www.opengl.org/registry/specs/ARB/multi_bind.txt ARB_multi_bind> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBMultiBind' in those cases instead.
gl_ARB_multi_bind :: Bool
gl_ARB_multi_bind = member "GL_ARB_multi_bind" extensions
{-# NOINLINE gl_ARB_multi_bind #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/multi_draw_indirect.txt ARB_multi_draw_indirect> extension supported?
glGetARBMultiDrawIndirect :: MonadIO m => m Bool
glGetARBMultiDrawIndirect = getExtensions >>= (return . member "GL_ARB_multi_draw_indirect")

-- | Is the <https://www.opengl.org/registry/specs/ARB/multi_draw_indirect.txt ARB_multi_draw_indirect> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBMultiDrawIndirect' in those cases instead.
gl_ARB_multi_draw_indirect :: Bool
gl_ARB_multi_draw_indirect = member "GL_ARB_multi_draw_indirect" extensions
{-# NOINLINE gl_ARB_multi_draw_indirect #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/multisample.txt ARB_multisample> extension supported?
glGetARBMultisample :: MonadIO m => m Bool
glGetARBMultisample = getExtensions >>= (return . member "GL_ARB_multisample")

-- | Is the <https://www.opengl.org/registry/specs/ARB/multisample.txt ARB_multisample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBMultisample' in those cases instead.
gl_ARB_multisample :: Bool
gl_ARB_multisample = member "GL_ARB_multisample" extensions
{-# NOINLINE gl_ARB_multisample #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/multitexture.txt ARB_multitexture> extension supported?
glGetARBMultitexture :: MonadIO m => m Bool
glGetARBMultitexture = getExtensions >>= (return . member "GL_ARB_multitexture")

-- | Is the <https://www.opengl.org/registry/specs/ARB/multitexture.txt ARB_multitexture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBMultitexture' in those cases instead.
gl_ARB_multitexture :: Bool
gl_ARB_multitexture = member "GL_ARB_multitexture" extensions
{-# NOINLINE gl_ARB_multitexture #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/occlusion_query.txt ARB_occlusion_query> extension supported?
glGetARBOcclusionQuery :: MonadIO m => m Bool
glGetARBOcclusionQuery = getExtensions >>= (return . member "GL_ARB_occlusion_query")

-- | Is the <https://www.opengl.org/registry/specs/ARB/occlusion_query.txt ARB_occlusion_query> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBOcclusionQuery' in those cases instead.
gl_ARB_occlusion_query :: Bool
gl_ARB_occlusion_query = member "GL_ARB_occlusion_query" extensions
{-# NOINLINE gl_ARB_occlusion_query #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/occlusion_query2.txt ARB_occlusion_query2> extension supported?
glGetARBOcclusionQuery2 :: MonadIO m => m Bool
glGetARBOcclusionQuery2 = getExtensions >>= (return . member "GL_ARB_occlusion_query2")

-- | Is the <https://www.opengl.org/registry/specs/ARB/occlusion_query2.txt ARB_occlusion_query2> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBOcclusionQuery2' in those cases instead.
gl_ARB_occlusion_query2 :: Bool
gl_ARB_occlusion_query2 = member "GL_ARB_occlusion_query2" extensions
{-# NOINLINE gl_ARB_occlusion_query2 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/parallel_shader_compile.txt ARB_parallel_shader_compile> extension supported?
glGetARBParallelShaderCompile :: MonadIO m => m Bool
glGetARBParallelShaderCompile = getExtensions >>= (return . member "GL_ARB_parallel_shader_compile")

-- | Is the <https://www.opengl.org/registry/specs/ARB/parallel_shader_compile.txt ARB_parallel_shader_compile> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBParallelShaderCompile' in those cases instead.
gl_ARB_parallel_shader_compile :: Bool
gl_ARB_parallel_shader_compile = member "GL_ARB_parallel_shader_compile" extensions
{-# NOINLINE gl_ARB_parallel_shader_compile #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/pipeline_statistics_query.txt ARB_pipeline_statistics_query> extension supported?
glGetARBPipelineStatisticsQuery :: MonadIO m => m Bool
glGetARBPipelineStatisticsQuery = getExtensions >>= (return . member "GL_ARB_pipeline_statistics_query")

-- | Is the <https://www.opengl.org/registry/specs/ARB/pipeline_statistics_query.txt ARB_pipeline_statistics_query> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBPipelineStatisticsQuery' in those cases instead.
gl_ARB_pipeline_statistics_query :: Bool
gl_ARB_pipeline_statistics_query = member "GL_ARB_pipeline_statistics_query" extensions
{-# NOINLINE gl_ARB_pipeline_statistics_query #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/pixel_buffer_object.txt ARB_pixel_buffer_object> extension supported?
glGetARBPixelBufferObject :: MonadIO m => m Bool
glGetARBPixelBufferObject = getExtensions >>= (return . member "GL_ARB_pixel_buffer_object")

-- | Is the <https://www.opengl.org/registry/specs/ARB/pixel_buffer_object.txt ARB_pixel_buffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBPixelBufferObject' in those cases instead.
gl_ARB_pixel_buffer_object :: Bool
gl_ARB_pixel_buffer_object = member "GL_ARB_pixel_buffer_object" extensions
{-# NOINLINE gl_ARB_pixel_buffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/point_parameters.txt ARB_point_parameters> extension supported?
glGetARBPointParameters :: MonadIO m => m Bool
glGetARBPointParameters = getExtensions >>= (return . member "GL_ARB_point_parameters")

-- | Is the <https://www.opengl.org/registry/specs/ARB/point_parameters.txt ARB_point_parameters> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBPointParameters' in those cases instead.
gl_ARB_point_parameters :: Bool
gl_ARB_point_parameters = member "GL_ARB_point_parameters" extensions
{-# NOINLINE gl_ARB_point_parameters #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/point_sprite.txt ARB_point_sprite> extension supported?
glGetARBPointSprite :: MonadIO m => m Bool
glGetARBPointSprite = getExtensions >>= (return . member "GL_ARB_point_sprite")

-- | Is the <https://www.opengl.org/registry/specs/ARB/point_sprite.txt ARB_point_sprite> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBPointSprite' in those cases instead.
gl_ARB_point_sprite :: Bool
gl_ARB_point_sprite = member "GL_ARB_point_sprite" extensions
{-# NOINLINE gl_ARB_point_sprite #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/polygon_offset_clamp.txt ARB_polygon_offset_clamp> extension supported?
glGetARBPolygonOffsetClamp :: MonadIO m => m Bool
glGetARBPolygonOffsetClamp = getExtensions >>= (return . member "GL_ARB_polygon_offset_clamp")

-- | Is the <https://www.opengl.org/registry/specs/ARB/polygon_offset_clamp.txt ARB_polygon_offset_clamp> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBPolygonOffsetClamp' in those cases instead.
gl_ARB_polygon_offset_clamp :: Bool
gl_ARB_polygon_offset_clamp = member "GL_ARB_polygon_offset_clamp" extensions
{-# NOINLINE gl_ARB_polygon_offset_clamp #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/program_interface_query.txt ARB_program_interface_query> extension supported?
glGetARBProgramInterfaceQuery :: MonadIO m => m Bool
glGetARBProgramInterfaceQuery = getExtensions >>= (return . member "GL_ARB_program_interface_query")

-- | Is the <https://www.opengl.org/registry/specs/ARB/program_interface_query.txt ARB_program_interface_query> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBProgramInterfaceQuery' in those cases instead.
gl_ARB_program_interface_query :: Bool
gl_ARB_program_interface_query = member "GL_ARB_program_interface_query" extensions
{-# NOINLINE gl_ARB_program_interface_query #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/provoking_vertex.txt ARB_provoking_vertex> extension supported?
glGetARBProvokingVertex :: MonadIO m => m Bool
glGetARBProvokingVertex = getExtensions >>= (return . member "GL_ARB_provoking_vertex")

-- | Is the <https://www.opengl.org/registry/specs/ARB/provoking_vertex.txt ARB_provoking_vertex> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBProvokingVertex' in those cases instead.
gl_ARB_provoking_vertex :: Bool
gl_ARB_provoking_vertex = member "GL_ARB_provoking_vertex" extensions
{-# NOINLINE gl_ARB_provoking_vertex #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/query_buffer_object.txt ARB_query_buffer_object> extension supported?
glGetARBQueryBufferObject :: MonadIO m => m Bool
glGetARBQueryBufferObject = getExtensions >>= (return . member "GL_ARB_query_buffer_object")

-- | Is the <https://www.opengl.org/registry/specs/ARB/query_buffer_object.txt ARB_query_buffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBQueryBufferObject' in those cases instead.
gl_ARB_query_buffer_object :: Bool
gl_ARB_query_buffer_object = member "GL_ARB_query_buffer_object" extensions
{-# NOINLINE gl_ARB_query_buffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/robustness.txt ARB_robustness> extension supported?
glGetARBRobustness :: MonadIO m => m Bool
glGetARBRobustness = getExtensions >>= (return . member "GL_ARB_robustness")

-- | Is the <https://www.opengl.org/registry/specs/ARB/robustness.txt ARB_robustness> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBRobustness' in those cases instead.
gl_ARB_robustness :: Bool
gl_ARB_robustness = member "GL_ARB_robustness" extensions
{-# NOINLINE gl_ARB_robustness #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/sample_locations.txt ARB_sample_locations> extension supported?
glGetARBSampleLocations :: MonadIO m => m Bool
glGetARBSampleLocations = getExtensions >>= (return . member "GL_ARB_sample_locations")

-- | Is the <https://www.opengl.org/registry/specs/ARB/sample_locations.txt ARB_sample_locations> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBSampleLocations' in those cases instead.
gl_ARB_sample_locations :: Bool
gl_ARB_sample_locations = member "GL_ARB_sample_locations" extensions
{-# NOINLINE gl_ARB_sample_locations #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/sample_shading.txt ARB_sample_shading> extension supported?
glGetARBSampleShading :: MonadIO m => m Bool
glGetARBSampleShading = getExtensions >>= (return . member "GL_ARB_sample_shading")

-- | Is the <https://www.opengl.org/registry/specs/ARB/sample_shading.txt ARB_sample_shading> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBSampleShading' in those cases instead.
gl_ARB_sample_shading :: Bool
gl_ARB_sample_shading = member "GL_ARB_sample_shading" extensions
{-# NOINLINE gl_ARB_sample_shading #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/sampler_objects.txt ARB_sampler_objects> extension supported?
glGetARBSamplerObjects :: MonadIO m => m Bool
glGetARBSamplerObjects = getExtensions >>= (return . member "GL_ARB_sampler_objects")

-- | Is the <https://www.opengl.org/registry/specs/ARB/sampler_objects.txt ARB_sampler_objects> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBSamplerObjects' in those cases instead.
gl_ARB_sampler_objects :: Bool
gl_ARB_sampler_objects = member "GL_ARB_sampler_objects" extensions
{-# NOINLINE gl_ARB_sampler_objects #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/seamless_cube_map.txt ARB_seamless_cube_map> extension supported?
glGetARBSeamlessCubeMap :: MonadIO m => m Bool
glGetARBSeamlessCubeMap = getExtensions >>= (return . member "GL_ARB_seamless_cube_map")

-- | Is the <https://www.opengl.org/registry/specs/ARB/seamless_cube_map.txt ARB_seamless_cube_map> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBSeamlessCubeMap' in those cases instead.
gl_ARB_seamless_cube_map :: Bool
gl_ARB_seamless_cube_map = member "GL_ARB_seamless_cube_map" extensions
{-# NOINLINE gl_ARB_seamless_cube_map #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/seamless_cubemap_per_texture.txt ARB_seamless_cubemap_per_texture> extension supported?
glGetARBSeamlessCubemapPerTexture :: MonadIO m => m Bool
glGetARBSeamlessCubemapPerTexture = getExtensions >>= (return . member "GL_ARB_seamless_cubemap_per_texture")

-- | Is the <https://www.opengl.org/registry/specs/ARB/seamless_cubemap_per_texture.txt ARB_seamless_cubemap_per_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBSeamlessCubemapPerTexture' in those cases instead.
gl_ARB_seamless_cubemap_per_texture :: Bool
gl_ARB_seamless_cubemap_per_texture = member "GL_ARB_seamless_cubemap_per_texture" extensions
{-# NOINLINE gl_ARB_seamless_cubemap_per_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/separate_shader_objects.txt ARB_separate_shader_objects> extension supported?
glGetARBSeparateShaderObjects :: MonadIO m => m Bool
glGetARBSeparateShaderObjects = getExtensions >>= (return . member "GL_ARB_separate_shader_objects")

-- | Is the <https://www.opengl.org/registry/specs/ARB/separate_shader_objects.txt ARB_separate_shader_objects> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBSeparateShaderObjects' in those cases instead.
gl_ARB_separate_shader_objects :: Bool
gl_ARB_separate_shader_objects = member "GL_ARB_separate_shader_objects" extensions
{-# NOINLINE gl_ARB_separate_shader_objects #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/shader_atomic_counters.txt ARB_shader_atomic_counters> extension supported?
glGetARBShaderAtomicCounters :: MonadIO m => m Bool
glGetARBShaderAtomicCounters = getExtensions >>= (return . member "GL_ARB_shader_atomic_counters")

-- | Is the <https://www.opengl.org/registry/specs/ARB/shader_atomic_counters.txt ARB_shader_atomic_counters> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBShaderAtomicCounters' in those cases instead.
gl_ARB_shader_atomic_counters :: Bool
gl_ARB_shader_atomic_counters = member "GL_ARB_shader_atomic_counters" extensions
{-# NOINLINE gl_ARB_shader_atomic_counters #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/shader_image_load_store.txt ARB_shader_image_load_store> extension supported?
glGetARBShaderImageLoadStore :: MonadIO m => m Bool
glGetARBShaderImageLoadStore = getExtensions >>= (return . member "GL_ARB_shader_image_load_store")

-- | Is the <https://www.opengl.org/registry/specs/ARB/shader_image_load_store.txt ARB_shader_image_load_store> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBShaderImageLoadStore' in those cases instead.
gl_ARB_shader_image_load_store :: Bool
gl_ARB_shader_image_load_store = member "GL_ARB_shader_image_load_store" extensions
{-# NOINLINE gl_ARB_shader_image_load_store #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/shader_objects.txt ARB_shader_objects> extension supported?
glGetARBShaderObjects :: MonadIO m => m Bool
glGetARBShaderObjects = getExtensions >>= (return . member "GL_ARB_shader_objects")

-- | Is the <https://www.opengl.org/registry/specs/ARB/shader_objects.txt ARB_shader_objects> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBShaderObjects' in those cases instead.
gl_ARB_shader_objects :: Bool
gl_ARB_shader_objects = member "GL_ARB_shader_objects" extensions
{-# NOINLINE gl_ARB_shader_objects #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/shader_storage_buffer_object.txt ARB_shader_storage_buffer_object> extension supported?
glGetARBShaderStorageBufferObject :: MonadIO m => m Bool
glGetARBShaderStorageBufferObject = getExtensions >>= (return . member "GL_ARB_shader_storage_buffer_object")

-- | Is the <https://www.opengl.org/registry/specs/ARB/shader_storage_buffer_object.txt ARB_shader_storage_buffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBShaderStorageBufferObject' in those cases instead.
gl_ARB_shader_storage_buffer_object :: Bool
gl_ARB_shader_storage_buffer_object = member "GL_ARB_shader_storage_buffer_object" extensions
{-# NOINLINE gl_ARB_shader_storage_buffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/shader_subroutine.txt ARB_shader_subroutine> extension supported?
glGetARBShaderSubroutine :: MonadIO m => m Bool
glGetARBShaderSubroutine = getExtensions >>= (return . member "GL_ARB_shader_subroutine")

-- | Is the <https://www.opengl.org/registry/specs/ARB/shader_subroutine.txt ARB_shader_subroutine> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBShaderSubroutine' in those cases instead.
gl_ARB_shader_subroutine :: Bool
gl_ARB_shader_subroutine = member "GL_ARB_shader_subroutine" extensions
{-# NOINLINE gl_ARB_shader_subroutine #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/shading_language_100.txt ARB_shading_language_100> extension supported?
glGetARBShadingLanguage100 :: MonadIO m => m Bool
glGetARBShadingLanguage100 = getExtensions >>= (return . member "GL_ARB_shading_language_100")

-- | Is the <https://www.opengl.org/registry/specs/ARB/shading_language_100.txt ARB_shading_language_100> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBShadingLanguage100' in those cases instead.
gl_ARB_shading_language_100 :: Bool
gl_ARB_shading_language_100 = member "GL_ARB_shading_language_100" extensions
{-# NOINLINE gl_ARB_shading_language_100 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/shading_language_include.txt ARB_shading_language_include> extension supported?
glGetARBShadingLanguageInclude :: MonadIO m => m Bool
glGetARBShadingLanguageInclude = getExtensions >>= (return . member "GL_ARB_shading_language_include")

-- | Is the <https://www.opengl.org/registry/specs/ARB/shading_language_include.txt ARB_shading_language_include> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBShadingLanguageInclude' in those cases instead.
gl_ARB_shading_language_include :: Bool
gl_ARB_shading_language_include = member "GL_ARB_shading_language_include" extensions
{-# NOINLINE gl_ARB_shading_language_include #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/shadow.txt ARB_shadow> extension supported?
glGetARBShadow :: MonadIO m => m Bool
glGetARBShadow = getExtensions >>= (return . member "GL_ARB_shadow")

-- | Is the <https://www.opengl.org/registry/specs/ARB/shadow.txt ARB_shadow> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBShadow' in those cases instead.
gl_ARB_shadow :: Bool
gl_ARB_shadow = member "GL_ARB_shadow" extensions
{-# NOINLINE gl_ARB_shadow #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/shadow_ambient.txt ARB_shadow_ambient> extension supported?
glGetARBShadowAmbient :: MonadIO m => m Bool
glGetARBShadowAmbient = getExtensions >>= (return . member "GL_ARB_shadow_ambient")

-- | Is the <https://www.opengl.org/registry/specs/ARB/shadow_ambient.txt ARB_shadow_ambient> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBShadowAmbient' in those cases instead.
gl_ARB_shadow_ambient :: Bool
gl_ARB_shadow_ambient = member "GL_ARB_shadow_ambient" extensions
{-# NOINLINE gl_ARB_shadow_ambient #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/sparse_buffer.txt ARB_sparse_buffer> extension supported?
glGetARBSparseBuffer :: MonadIO m => m Bool
glGetARBSparseBuffer = getExtensions >>= (return . member "GL_ARB_sparse_buffer")

-- | Is the <https://www.opengl.org/registry/specs/ARB/sparse_buffer.txt ARB_sparse_buffer> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBSparseBuffer' in those cases instead.
gl_ARB_sparse_buffer :: Bool
gl_ARB_sparse_buffer = member "GL_ARB_sparse_buffer" extensions
{-# NOINLINE gl_ARB_sparse_buffer #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/sparse_texture.txt ARB_sparse_texture> extension supported?
glGetARBSparseTexture :: MonadIO m => m Bool
glGetARBSparseTexture = getExtensions >>= (return . member "GL_ARB_sparse_texture")

-- | Is the <https://www.opengl.org/registry/specs/ARB/sparse_texture.txt ARB_sparse_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBSparseTexture' in those cases instead.
gl_ARB_sparse_texture :: Bool
gl_ARB_sparse_texture = member "GL_ARB_sparse_texture" extensions
{-# NOINLINE gl_ARB_sparse_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/spirv_extensions.txt ARB_spirv_extensions> extension supported?
glGetARBSpirvExtensions :: MonadIO m => m Bool
glGetARBSpirvExtensions = getExtensions >>= (return . member "GL_ARB_spirv_extensions")

-- | Is the <https://www.opengl.org/registry/specs/ARB/spirv_extensions.txt ARB_spirv_extensions> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBSpirvExtensions' in those cases instead.
gl_ARB_spirv_extensions :: Bool
gl_ARB_spirv_extensions = member "GL_ARB_spirv_extensions" extensions
{-# NOINLINE gl_ARB_spirv_extensions #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/stencil_texturing.txt ARB_stencil_texturing> extension supported?
glGetARBStencilTexturing :: MonadIO m => m Bool
glGetARBStencilTexturing = getExtensions >>= (return . member "GL_ARB_stencil_texturing")

-- | Is the <https://www.opengl.org/registry/specs/ARB/stencil_texturing.txt ARB_stencil_texturing> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBStencilTexturing' in those cases instead.
gl_ARB_stencil_texturing :: Bool
gl_ARB_stencil_texturing = member "GL_ARB_stencil_texturing" extensions
{-# NOINLINE gl_ARB_stencil_texturing #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/sync.txt ARB_sync> extension supported?
glGetARBSync :: MonadIO m => m Bool
glGetARBSync = getExtensions >>= (return . member "GL_ARB_sync")

-- | Is the <https://www.opengl.org/registry/specs/ARB/sync.txt ARB_sync> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBSync' in those cases instead.
gl_ARB_sync :: Bool
gl_ARB_sync = member "GL_ARB_sync" extensions
{-# NOINLINE gl_ARB_sync #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/tessellation_shader.txt ARB_tessellation_shader> extension supported?
glGetARBTessellationShader :: MonadIO m => m Bool
glGetARBTessellationShader = getExtensions >>= (return . member "GL_ARB_tessellation_shader")

-- | Is the <https://www.opengl.org/registry/specs/ARB/tessellation_shader.txt ARB_tessellation_shader> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTessellationShader' in those cases instead.
gl_ARB_tessellation_shader :: Bool
gl_ARB_tessellation_shader = member "GL_ARB_tessellation_shader" extensions
{-# NOINLINE gl_ARB_tessellation_shader #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_barrier.txt ARB_texture_barrier> extension supported?
glGetARBTextureBarrier :: MonadIO m => m Bool
glGetARBTextureBarrier = getExtensions >>= (return . member "GL_ARB_texture_barrier")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_barrier.txt ARB_texture_barrier> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureBarrier' in those cases instead.
gl_ARB_texture_barrier :: Bool
gl_ARB_texture_barrier = member "GL_ARB_texture_barrier" extensions
{-# NOINLINE gl_ARB_texture_barrier #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_border_clamp.txt ARB_texture_border_clamp> extension supported?
glGetARBTextureBorderClamp :: MonadIO m => m Bool
glGetARBTextureBorderClamp = getExtensions >>= (return . member "GL_ARB_texture_border_clamp")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_border_clamp.txt ARB_texture_border_clamp> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureBorderClamp' in those cases instead.
gl_ARB_texture_border_clamp :: Bool
gl_ARB_texture_border_clamp = member "GL_ARB_texture_border_clamp" extensions
{-# NOINLINE gl_ARB_texture_border_clamp #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_buffer_object.txt ARB_texture_buffer_object> extension supported?
glGetARBTextureBufferObject :: MonadIO m => m Bool
glGetARBTextureBufferObject = getExtensions >>= (return . member "GL_ARB_texture_buffer_object")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_buffer_object.txt ARB_texture_buffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureBufferObject' in those cases instead.
gl_ARB_texture_buffer_object :: Bool
gl_ARB_texture_buffer_object = member "GL_ARB_texture_buffer_object" extensions
{-# NOINLINE gl_ARB_texture_buffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_buffer_object_rgb32.txt ARB_texture_buffer_object_rgb32> extension supported?
glGetARBTextureBufferObjectRGB32 :: MonadIO m => m Bool
glGetARBTextureBufferObjectRGB32 = getExtensions >>= (return . member "GL_ARB_texture_buffer_object_rgb32")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_buffer_object_rgb32.txt ARB_texture_buffer_object_rgb32> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureBufferObjectRGB32' in those cases instead.
gl_ARB_texture_buffer_object_rgb32 :: Bool
gl_ARB_texture_buffer_object_rgb32 = member "GL_ARB_texture_buffer_object_rgb32" extensions
{-# NOINLINE gl_ARB_texture_buffer_object_rgb32 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_buffer_range.txt ARB_texture_buffer_range> extension supported?
glGetARBTextureBufferRange :: MonadIO m => m Bool
glGetARBTextureBufferRange = getExtensions >>= (return . member "GL_ARB_texture_buffer_range")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_buffer_range.txt ARB_texture_buffer_range> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureBufferRange' in those cases instead.
gl_ARB_texture_buffer_range :: Bool
gl_ARB_texture_buffer_range = member "GL_ARB_texture_buffer_range" extensions
{-# NOINLINE gl_ARB_texture_buffer_range #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_compression.txt ARB_texture_compression> extension supported?
glGetARBTextureCompression :: MonadIO m => m Bool
glGetARBTextureCompression = getExtensions >>= (return . member "GL_ARB_texture_compression")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_compression.txt ARB_texture_compression> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureCompression' in those cases instead.
gl_ARB_texture_compression :: Bool
gl_ARB_texture_compression = member "GL_ARB_texture_compression" extensions
{-# NOINLINE gl_ARB_texture_compression #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_compression_bptc.txt ARB_texture_compression_bptc> extension supported?
glGetARBTextureCompressionBPTC :: MonadIO m => m Bool
glGetARBTextureCompressionBPTC = getExtensions >>= (return . member "GL_ARB_texture_compression_bptc")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_compression_bptc.txt ARB_texture_compression_bptc> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureCompressionBPTC' in those cases instead.
gl_ARB_texture_compression_bptc :: Bool
gl_ARB_texture_compression_bptc = member "GL_ARB_texture_compression_bptc" extensions
{-# NOINLINE gl_ARB_texture_compression_bptc #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_compression_rgtc.txt ARB_texture_compression_rgtc> extension supported?
glGetARBTextureCompressionRGTC :: MonadIO m => m Bool
glGetARBTextureCompressionRGTC = getExtensions >>= (return . member "GL_ARB_texture_compression_rgtc")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_compression_rgtc.txt ARB_texture_compression_rgtc> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureCompressionRGTC' in those cases instead.
gl_ARB_texture_compression_rgtc :: Bool
gl_ARB_texture_compression_rgtc = member "GL_ARB_texture_compression_rgtc" extensions
{-# NOINLINE gl_ARB_texture_compression_rgtc #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_cube_map.txt ARB_texture_cube_map> extension supported?
glGetARBTextureCubeMap :: MonadIO m => m Bool
glGetARBTextureCubeMap = getExtensions >>= (return . member "GL_ARB_texture_cube_map")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_cube_map.txt ARB_texture_cube_map> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureCubeMap' in those cases instead.
gl_ARB_texture_cube_map :: Bool
gl_ARB_texture_cube_map = member "GL_ARB_texture_cube_map" extensions
{-# NOINLINE gl_ARB_texture_cube_map #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_cube_map_array.txt ARB_texture_cube_map_array> extension supported?
glGetARBTextureCubeMapArray :: MonadIO m => m Bool
glGetARBTextureCubeMapArray = getExtensions >>= (return . member "GL_ARB_texture_cube_map_array")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_cube_map_array.txt ARB_texture_cube_map_array> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureCubeMapArray' in those cases instead.
gl_ARB_texture_cube_map_array :: Bool
gl_ARB_texture_cube_map_array = member "GL_ARB_texture_cube_map_array" extensions
{-# NOINLINE gl_ARB_texture_cube_map_array #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_env_combine.txt ARB_texture_env_combine> extension supported?
glGetARBTextureEnvCombine :: MonadIO m => m Bool
glGetARBTextureEnvCombine = getExtensions >>= (return . member "GL_ARB_texture_env_combine")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_env_combine.txt ARB_texture_env_combine> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureEnvCombine' in those cases instead.
gl_ARB_texture_env_combine :: Bool
gl_ARB_texture_env_combine = member "GL_ARB_texture_env_combine" extensions
{-# NOINLINE gl_ARB_texture_env_combine #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_env_dot3.txt ARB_texture_env_dot3> extension supported?
glGetARBTextureEnvDot3 :: MonadIO m => m Bool
glGetARBTextureEnvDot3 = getExtensions >>= (return . member "GL_ARB_texture_env_dot3")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_env_dot3.txt ARB_texture_env_dot3> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureEnvDot3' in those cases instead.
gl_ARB_texture_env_dot3 :: Bool
gl_ARB_texture_env_dot3 = member "GL_ARB_texture_env_dot3" extensions
{-# NOINLINE gl_ARB_texture_env_dot3 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_filter_anisotropic.txt ARB_texture_filter_anisotropic> extension supported?
glGetARBTextureFilterAnisotropic :: MonadIO m => m Bool
glGetARBTextureFilterAnisotropic = getExtensions >>= (return . member "GL_ARB_texture_filter_anisotropic")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_filter_anisotropic.txt ARB_texture_filter_anisotropic> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureFilterAnisotropic' in those cases instead.
gl_ARB_texture_filter_anisotropic :: Bool
gl_ARB_texture_filter_anisotropic = member "GL_ARB_texture_filter_anisotropic" extensions
{-# NOINLINE gl_ARB_texture_filter_anisotropic #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_filter_minmax.txt ARB_texture_filter_minmax> extension supported?
glGetARBTextureFilterMinmax :: MonadIO m => m Bool
glGetARBTextureFilterMinmax = getExtensions >>= (return . member "GL_ARB_texture_filter_minmax")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_filter_minmax.txt ARB_texture_filter_minmax> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureFilterMinmax' in those cases instead.
gl_ARB_texture_filter_minmax :: Bool
gl_ARB_texture_filter_minmax = member "GL_ARB_texture_filter_minmax" extensions
{-# NOINLINE gl_ARB_texture_filter_minmax #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_float.txt ARB_texture_float> extension supported?
glGetARBTextureFloat :: MonadIO m => m Bool
glGetARBTextureFloat = getExtensions >>= (return . member "GL_ARB_texture_float")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_float.txt ARB_texture_float> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureFloat' in those cases instead.
gl_ARB_texture_float :: Bool
gl_ARB_texture_float = member "GL_ARB_texture_float" extensions
{-# NOINLINE gl_ARB_texture_float #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_gather.txt ARB_texture_gather> extension supported?
glGetARBTextureGather :: MonadIO m => m Bool
glGetARBTextureGather = getExtensions >>= (return . member "GL_ARB_texture_gather")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_gather.txt ARB_texture_gather> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureGather' in those cases instead.
gl_ARB_texture_gather :: Bool
gl_ARB_texture_gather = member "GL_ARB_texture_gather" extensions
{-# NOINLINE gl_ARB_texture_gather #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_mirror_clamp_to_edge.txt ARB_texture_mirror_clamp_to_edge> extension supported?
glGetARBTextureMirrorClampToEdge :: MonadIO m => m Bool
glGetARBTextureMirrorClampToEdge = getExtensions >>= (return . member "GL_ARB_texture_mirror_clamp_to_edge")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_mirror_clamp_to_edge.txt ARB_texture_mirror_clamp_to_edge> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureMirrorClampToEdge' in those cases instead.
gl_ARB_texture_mirror_clamp_to_edge :: Bool
gl_ARB_texture_mirror_clamp_to_edge = member "GL_ARB_texture_mirror_clamp_to_edge" extensions
{-# NOINLINE gl_ARB_texture_mirror_clamp_to_edge #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_mirrored_repeat.txt ARB_texture_mirrored_repeat> extension supported?
glGetARBTextureMirroredRepeat :: MonadIO m => m Bool
glGetARBTextureMirroredRepeat = getExtensions >>= (return . member "GL_ARB_texture_mirrored_repeat")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_mirrored_repeat.txt ARB_texture_mirrored_repeat> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureMirroredRepeat' in those cases instead.
gl_ARB_texture_mirrored_repeat :: Bool
gl_ARB_texture_mirrored_repeat = member "GL_ARB_texture_mirrored_repeat" extensions
{-# NOINLINE gl_ARB_texture_mirrored_repeat #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_multisample.txt ARB_texture_multisample> extension supported?
glGetARBTextureMultisample :: MonadIO m => m Bool
glGetARBTextureMultisample = getExtensions >>= (return . member "GL_ARB_texture_multisample")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_multisample.txt ARB_texture_multisample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureMultisample' in those cases instead.
gl_ARB_texture_multisample :: Bool
gl_ARB_texture_multisample = member "GL_ARB_texture_multisample" extensions
{-# NOINLINE gl_ARB_texture_multisample #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_rectangle.txt ARB_texture_rectangle> extension supported?
glGetARBTextureRectangle :: MonadIO m => m Bool
glGetARBTextureRectangle = getExtensions >>= (return . member "GL_ARB_texture_rectangle")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_rectangle.txt ARB_texture_rectangle> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureRectangle' in those cases instead.
gl_ARB_texture_rectangle :: Bool
gl_ARB_texture_rectangle = member "GL_ARB_texture_rectangle" extensions
{-# NOINLINE gl_ARB_texture_rectangle #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_rg.txt ARB_texture_rg> extension supported?
glGetARBTextureRG :: MonadIO m => m Bool
glGetARBTextureRG = getExtensions >>= (return . member "GL_ARB_texture_rg")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_rg.txt ARB_texture_rg> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureRG' in those cases instead.
gl_ARB_texture_rg :: Bool
gl_ARB_texture_rg = member "GL_ARB_texture_rg" extensions
{-# NOINLINE gl_ARB_texture_rg #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_rgb10_a2ui.txt ARB_texture_rgb10_a2ui> extension supported?
glGetARBTextureRGB10A2UI :: MonadIO m => m Bool
glGetARBTextureRGB10A2UI = getExtensions >>= (return . member "GL_ARB_texture_rgb10_a2ui")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_rgb10_a2ui.txt ARB_texture_rgb10_a2ui> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureRGB10A2UI' in those cases instead.
gl_ARB_texture_rgb10_a2ui :: Bool
gl_ARB_texture_rgb10_a2ui = member "GL_ARB_texture_rgb10_a2ui" extensions
{-# NOINLINE gl_ARB_texture_rgb10_a2ui #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_stencil8.txt ARB_texture_stencil8> extension supported?
glGetARBTextureStencil8 :: MonadIO m => m Bool
glGetARBTextureStencil8 = getExtensions >>= (return . member "GL_ARB_texture_stencil8")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_stencil8.txt ARB_texture_stencil8> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureStencil8' in those cases instead.
gl_ARB_texture_stencil8 :: Bool
gl_ARB_texture_stencil8 = member "GL_ARB_texture_stencil8" extensions
{-# NOINLINE gl_ARB_texture_stencil8 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_storage.txt ARB_texture_storage> extension supported?
glGetARBTextureStorage :: MonadIO m => m Bool
glGetARBTextureStorage = getExtensions >>= (return . member "GL_ARB_texture_storage")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_storage.txt ARB_texture_storage> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureStorage' in those cases instead.
gl_ARB_texture_storage :: Bool
gl_ARB_texture_storage = member "GL_ARB_texture_storage" extensions
{-# NOINLINE gl_ARB_texture_storage #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_storage_multisample.txt ARB_texture_storage_multisample> extension supported?
glGetARBTextureStorageMultisample :: MonadIO m => m Bool
glGetARBTextureStorageMultisample = getExtensions >>= (return . member "GL_ARB_texture_storage_multisample")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_storage_multisample.txt ARB_texture_storage_multisample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureStorageMultisample' in those cases instead.
gl_ARB_texture_storage_multisample :: Bool
gl_ARB_texture_storage_multisample = member "GL_ARB_texture_storage_multisample" extensions
{-# NOINLINE gl_ARB_texture_storage_multisample #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_swizzle.txt ARB_texture_swizzle> extension supported?
glGetARBTextureSwizzle :: MonadIO m => m Bool
glGetARBTextureSwizzle = getExtensions >>= (return . member "GL_ARB_texture_swizzle")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_swizzle.txt ARB_texture_swizzle> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureSwizzle' in those cases instead.
gl_ARB_texture_swizzle :: Bool
gl_ARB_texture_swizzle = member "GL_ARB_texture_swizzle" extensions
{-# NOINLINE gl_ARB_texture_swizzle #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_view.txt ARB_texture_view> extension supported?
glGetARBTextureView :: MonadIO m => m Bool
glGetARBTextureView = getExtensions >>= (return . member "GL_ARB_texture_view")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_view.txt ARB_texture_view> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTextureView' in those cases instead.
gl_ARB_texture_view :: Bool
gl_ARB_texture_view = member "GL_ARB_texture_view" extensions
{-# NOINLINE gl_ARB_texture_view #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/timer_query.txt ARB_timer_query> extension supported?
glGetARBTimerQuery :: MonadIO m => m Bool
glGetARBTimerQuery = getExtensions >>= (return . member "GL_ARB_timer_query")

-- | Is the <https://www.opengl.org/registry/specs/ARB/timer_query.txt ARB_timer_query> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTimerQuery' in those cases instead.
gl_ARB_timer_query :: Bool
gl_ARB_timer_query = member "GL_ARB_timer_query" extensions
{-# NOINLINE gl_ARB_timer_query #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/transform_feedback2.txt ARB_transform_feedback2> extension supported?
glGetARBTransformFeedback2 :: MonadIO m => m Bool
glGetARBTransformFeedback2 = getExtensions >>= (return . member "GL_ARB_transform_feedback2")

-- | Is the <https://www.opengl.org/registry/specs/ARB/transform_feedback2.txt ARB_transform_feedback2> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTransformFeedback2' in those cases instead.
gl_ARB_transform_feedback2 :: Bool
gl_ARB_transform_feedback2 = member "GL_ARB_transform_feedback2" extensions
{-# NOINLINE gl_ARB_transform_feedback2 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/transform_feedback3.txt ARB_transform_feedback3> extension supported?
glGetARBTransformFeedback3 :: MonadIO m => m Bool
glGetARBTransformFeedback3 = getExtensions >>= (return . member "GL_ARB_transform_feedback3")

-- | Is the <https://www.opengl.org/registry/specs/ARB/transform_feedback3.txt ARB_transform_feedback3> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTransformFeedback3' in those cases instead.
gl_ARB_transform_feedback3 :: Bool
gl_ARB_transform_feedback3 = member "GL_ARB_transform_feedback3" extensions
{-# NOINLINE gl_ARB_transform_feedback3 #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/transform_feedback_instanced.txt ARB_transform_feedback_instanced> extension supported?
glGetARBTransformFeedbackInstanced :: MonadIO m => m Bool
glGetARBTransformFeedbackInstanced = getExtensions >>= (return . member "GL_ARB_transform_feedback_instanced")

-- | Is the <https://www.opengl.org/registry/specs/ARB/transform_feedback_instanced.txt ARB_transform_feedback_instanced> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTransformFeedbackInstanced' in those cases instead.
gl_ARB_transform_feedback_instanced :: Bool
gl_ARB_transform_feedback_instanced = member "GL_ARB_transform_feedback_instanced" extensions
{-# NOINLINE gl_ARB_transform_feedback_instanced #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/transform_feedback_overflow_query.txt ARB_transform_feedback_overflow_query> extension supported?
glGetARBTransformFeedbackOverflowQuery :: MonadIO m => m Bool
glGetARBTransformFeedbackOverflowQuery = getExtensions >>= (return . member "GL_ARB_transform_feedback_overflow_query")

-- | Is the <https://www.opengl.org/registry/specs/ARB/transform_feedback_overflow_query.txt ARB_transform_feedback_overflow_query> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTransformFeedbackOverflowQuery' in those cases instead.
gl_ARB_transform_feedback_overflow_query :: Bool
gl_ARB_transform_feedback_overflow_query = member "GL_ARB_transform_feedback_overflow_query" extensions
{-# NOINLINE gl_ARB_transform_feedback_overflow_query #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/transpose_matrix.txt ARB_transpose_matrix> extension supported?
glGetARBTransposeMatrix :: MonadIO m => m Bool
glGetARBTransposeMatrix = getExtensions >>= (return . member "GL_ARB_transpose_matrix")

-- | Is the <https://www.opengl.org/registry/specs/ARB/transpose_matrix.txt ARB_transpose_matrix> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBTransposeMatrix' in those cases instead.
gl_ARB_transpose_matrix :: Bool
gl_ARB_transpose_matrix = member "GL_ARB_transpose_matrix" extensions
{-# NOINLINE gl_ARB_transpose_matrix #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/uniform_buffer_object.txt ARB_uniform_buffer_object> extension supported?
glGetARBUniformBufferObject :: MonadIO m => m Bool
glGetARBUniformBufferObject = getExtensions >>= (return . member "GL_ARB_uniform_buffer_object")

-- | Is the <https://www.opengl.org/registry/specs/ARB/uniform_buffer_object.txt ARB_uniform_buffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBUniformBufferObject' in those cases instead.
gl_ARB_uniform_buffer_object :: Bool
gl_ARB_uniform_buffer_object = member "GL_ARB_uniform_buffer_object" extensions
{-# NOINLINE gl_ARB_uniform_buffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_array_bgra.txt ARB_vertex_array_bgra> extension supported?
glGetARBVertexArrayBGRA :: MonadIO m => m Bool
glGetARBVertexArrayBGRA = getExtensions >>= (return . member "GL_ARB_vertex_array_bgra")

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_array_bgra.txt ARB_vertex_array_bgra> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBVertexArrayBGRA' in those cases instead.
gl_ARB_vertex_array_bgra :: Bool
gl_ARB_vertex_array_bgra = member "GL_ARB_vertex_array_bgra" extensions
{-# NOINLINE gl_ARB_vertex_array_bgra #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_array_object.txt ARB_vertex_array_object> extension supported?
glGetARBVertexArrayObject :: MonadIO m => m Bool
glGetARBVertexArrayObject = getExtensions >>= (return . member "GL_ARB_vertex_array_object")

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_array_object.txt ARB_vertex_array_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBVertexArrayObject' in those cases instead.
gl_ARB_vertex_array_object :: Bool
gl_ARB_vertex_array_object = member "GL_ARB_vertex_array_object" extensions
{-# NOINLINE gl_ARB_vertex_array_object #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_attrib_64bit.txt ARB_vertex_attrib_64bit> extension supported?
glGetARBVertexAttrib64Bit :: MonadIO m => m Bool
glGetARBVertexAttrib64Bit = getExtensions >>= (return . member "GL_ARB_vertex_attrib_64bit")

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_attrib_64bit.txt ARB_vertex_attrib_64bit> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBVertexAttrib64Bit' in those cases instead.
gl_ARB_vertex_attrib_64bit :: Bool
gl_ARB_vertex_attrib_64bit = member "GL_ARB_vertex_attrib_64bit" extensions
{-# NOINLINE gl_ARB_vertex_attrib_64bit #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt ARB_vertex_attrib_binding> extension supported?
glGetARBVertexAttribBinding :: MonadIO m => m Bool
glGetARBVertexAttribBinding = getExtensions >>= (return . member "GL_ARB_vertex_attrib_binding")

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_attrib_binding.txt ARB_vertex_attrib_binding> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBVertexAttribBinding' in those cases instead.
gl_ARB_vertex_attrib_binding :: Bool
gl_ARB_vertex_attrib_binding = member "GL_ARB_vertex_attrib_binding" extensions
{-# NOINLINE gl_ARB_vertex_attrib_binding #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_blend.txt ARB_vertex_blend> extension supported?
glGetARBVertexBlend :: MonadIO m => m Bool
glGetARBVertexBlend = getExtensions >>= (return . member "GL_ARB_vertex_blend")

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_blend.txt ARB_vertex_blend> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBVertexBlend' in those cases instead.
gl_ARB_vertex_blend :: Bool
gl_ARB_vertex_blend = member "GL_ARB_vertex_blend" extensions
{-# NOINLINE gl_ARB_vertex_blend #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt ARB_vertex_buffer_object> extension supported?
glGetARBVertexBufferObject :: MonadIO m => m Bool
glGetARBVertexBufferObject = getExtensions >>= (return . member "GL_ARB_vertex_buffer_object")

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_buffer_object.txt ARB_vertex_buffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBVertexBufferObject' in those cases instead.
gl_ARB_vertex_buffer_object :: Bool
gl_ARB_vertex_buffer_object = member "GL_ARB_vertex_buffer_object" extensions
{-# NOINLINE gl_ARB_vertex_buffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_program.txt ARB_vertex_program> extension supported?
glGetARBVertexProgram :: MonadIO m => m Bool
glGetARBVertexProgram = getExtensions >>= (return . member "GL_ARB_vertex_program")

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_program.txt ARB_vertex_program> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBVertexProgram' in those cases instead.
gl_ARB_vertex_program :: Bool
gl_ARB_vertex_program = member "GL_ARB_vertex_program" extensions
{-# NOINLINE gl_ARB_vertex_program #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_shader.txt ARB_vertex_shader> extension supported?
glGetARBVertexShader :: MonadIO m => m Bool
glGetARBVertexShader = getExtensions >>= (return . member "GL_ARB_vertex_shader")

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_shader.txt ARB_vertex_shader> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBVertexShader' in those cases instead.
gl_ARB_vertex_shader :: Bool
gl_ARB_vertex_shader = member "GL_ARB_vertex_shader" extensions
{-# NOINLINE gl_ARB_vertex_shader #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_type_10f_11f_11f_rev.txt ARB_vertex_type_10f_11f_11f_rev> extension supported?
glGetARBVertexType10f11f11fRev :: MonadIO m => m Bool
glGetARBVertexType10f11f11fRev = getExtensions >>= (return . member "GL_ARB_vertex_type_10f_11f_11f_rev")

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_type_10f_11f_11f_rev.txt ARB_vertex_type_10f_11f_11f_rev> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBVertexType10f11f11fRev' in those cases instead.
gl_ARB_vertex_type_10f_11f_11f_rev :: Bool
gl_ARB_vertex_type_10f_11f_11f_rev = member "GL_ARB_vertex_type_10f_11f_11f_rev" extensions
{-# NOINLINE gl_ARB_vertex_type_10f_11f_11f_rev #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_type_2_10_10_10_rev.txt ARB_vertex_type_2_10_10_10_rev> extension supported?
glGetARBVertexType2101010Rev :: MonadIO m => m Bool
glGetARBVertexType2101010Rev = getExtensions >>= (return . member "GL_ARB_vertex_type_2_10_10_10_rev")

-- | Is the <https://www.opengl.org/registry/specs/ARB/vertex_type_2_10_10_10_rev.txt ARB_vertex_type_2_10_10_10_rev> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBVertexType2101010Rev' in those cases instead.
gl_ARB_vertex_type_2_10_10_10_rev :: Bool
gl_ARB_vertex_type_2_10_10_10_rev = member "GL_ARB_vertex_type_2_10_10_10_rev" extensions
{-# NOINLINE gl_ARB_vertex_type_2_10_10_10_rev #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/viewport_array.txt ARB_viewport_array> extension supported?
glGetARBViewportArray :: MonadIO m => m Bool
glGetARBViewportArray = getExtensions >>= (return . member "GL_ARB_viewport_array")

-- | Is the <https://www.opengl.org/registry/specs/ARB/viewport_array.txt ARB_viewport_array> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBViewportArray' in those cases instead.
gl_ARB_viewport_array :: Bool
gl_ARB_viewport_array = member "GL_ARB_viewport_array" extensions
{-# NOINLINE gl_ARB_viewport_array #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/window_pos.txt ARB_window_pos> extension supported?
glGetARBWindowPos :: MonadIO m => m Bool
glGetARBWindowPos = getExtensions >>= (return . member "GL_ARB_window_pos")

-- | Is the <https://www.opengl.org/registry/specs/ARB/window_pos.txt ARB_window_pos> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetARBWindowPos' in those cases instead.
gl_ARB_window_pos :: Bool
gl_ARB_window_pos = member "GL_ARB_window_pos" extensions
{-# NOINLINE gl_ARB_window_pos #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/draw_buffers.txt ATI_draw_buffers> extension supported?
glGetATIDrawBuffers :: MonadIO m => m Bool
glGetATIDrawBuffers = getExtensions >>= (return . member "GL_ATI_draw_buffers")

-- | Is the <https://www.opengl.org/registry/specs/ATI/draw_buffers.txt ATI_draw_buffers> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATIDrawBuffers' in those cases instead.
gl_ATI_draw_buffers :: Bool
gl_ATI_draw_buffers = member "GL_ATI_draw_buffers" extensions
{-# NOINLINE gl_ATI_draw_buffers #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/element_array.txt ATI_element_array> extension supported?
glGetATIElementArray :: MonadIO m => m Bool
glGetATIElementArray = getExtensions >>= (return . member "GL_ATI_element_array")

-- | Is the <https://www.opengl.org/registry/specs/ATI/element_array.txt ATI_element_array> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATIElementArray' in those cases instead.
gl_ATI_element_array :: Bool
gl_ATI_element_array = member "GL_ATI_element_array" extensions
{-# NOINLINE gl_ATI_element_array #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/envmap_bumpmap.txt ATI_envmap_bumpmap> extension supported?
glGetATIEnvmapBumpmap :: MonadIO m => m Bool
glGetATIEnvmapBumpmap = getExtensions >>= (return . member "GL_ATI_envmap_bumpmap")

-- | Is the <https://www.opengl.org/registry/specs/ATI/envmap_bumpmap.txt ATI_envmap_bumpmap> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATIEnvmapBumpmap' in those cases instead.
gl_ATI_envmap_bumpmap :: Bool
gl_ATI_envmap_bumpmap = member "GL_ATI_envmap_bumpmap" extensions
{-# NOINLINE gl_ATI_envmap_bumpmap #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/fragment_shader.txt ATI_fragment_shader> extension supported?
glGetATIFragmentShader :: MonadIO m => m Bool
glGetATIFragmentShader = getExtensions >>= (return . member "GL_ATI_fragment_shader")

-- | Is the <https://www.opengl.org/registry/specs/ATI/fragment_shader.txt ATI_fragment_shader> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATIFragmentShader' in those cases instead.
gl_ATI_fragment_shader :: Bool
gl_ATI_fragment_shader = member "GL_ATI_fragment_shader" extensions
{-# NOINLINE gl_ATI_fragment_shader #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/map_object_buffer.txt ATI_map_object_buffer> extension supported?
glGetATIMapObjectBuffer :: MonadIO m => m Bool
glGetATIMapObjectBuffer = getExtensions >>= (return . member "GL_ATI_map_object_buffer")

-- | Is the <https://www.opengl.org/registry/specs/ATI/map_object_buffer.txt ATI_map_object_buffer> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATIMapObjectBuffer' in those cases instead.
gl_ATI_map_object_buffer :: Bool
gl_ATI_map_object_buffer = member "GL_ATI_map_object_buffer" extensions
{-# NOINLINE gl_ATI_map_object_buffer #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/meminfo.txt ATI_meminfo> extension supported?
glGetATIMeminfo :: MonadIO m => m Bool
glGetATIMeminfo = getExtensions >>= (return . member "GL_ATI_meminfo")

-- | Is the <https://www.opengl.org/registry/specs/ATI/meminfo.txt ATI_meminfo> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATIMeminfo' in those cases instead.
gl_ATI_meminfo :: Bool
gl_ATI_meminfo = member "GL_ATI_meminfo" extensions
{-# NOINLINE gl_ATI_meminfo #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/pixel_format_float.txt ATI_pixel_format_float> extension supported?
glGetATIPixelFormatFloat :: MonadIO m => m Bool
glGetATIPixelFormatFloat = getExtensions >>= (return . member "GL_ATI_pixel_format_float")

-- | Is the <https://www.opengl.org/registry/specs/ATI/pixel_format_float.txt ATI_pixel_format_float> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATIPixelFormatFloat' in those cases instead.
gl_ATI_pixel_format_float :: Bool
gl_ATI_pixel_format_float = member "GL_ATI_pixel_format_float" extensions
{-# NOINLINE gl_ATI_pixel_format_float #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/pn_triangles.txt ATI_pn_triangles> extension supported?
glGetATIPNTriangles :: MonadIO m => m Bool
glGetATIPNTriangles = getExtensions >>= (return . member "GL_ATI_pn_triangles")

-- | Is the <https://www.opengl.org/registry/specs/ATI/pn_triangles.txt ATI_pn_triangles> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATIPNTriangles' in those cases instead.
gl_ATI_pn_triangles :: Bool
gl_ATI_pn_triangles = member "GL_ATI_pn_triangles" extensions
{-# NOINLINE gl_ATI_pn_triangles #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/separate_stencil.txt ATI_separate_stencil> extension supported?
glGetATISeparateStencil :: MonadIO m => m Bool
glGetATISeparateStencil = getExtensions >>= (return . member "GL_ATI_separate_stencil")

-- | Is the <https://www.opengl.org/registry/specs/ATI/separate_stencil.txt ATI_separate_stencil> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATISeparateStencil' in those cases instead.
gl_ATI_separate_stencil :: Bool
gl_ATI_separate_stencil = member "GL_ATI_separate_stencil" extensions
{-# NOINLINE gl_ATI_separate_stencil #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/text_fragment_shader.txt ATI_text_fragment_shader> extension supported?
glGetATITextFragmentShader :: MonadIO m => m Bool
glGetATITextFragmentShader = getExtensions >>= (return . member "GL_ATI_text_fragment_shader")

-- | Is the <https://www.opengl.org/registry/specs/ATI/text_fragment_shader.txt ATI_text_fragment_shader> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATITextFragmentShader' in those cases instead.
gl_ATI_text_fragment_shader :: Bool
gl_ATI_text_fragment_shader = member "GL_ATI_text_fragment_shader" extensions
{-# NOINLINE gl_ATI_text_fragment_shader #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/texture_env_combine3.txt ATI_texture_env_combine3> extension supported?
glGetATITextureEnvCombine3 :: MonadIO m => m Bool
glGetATITextureEnvCombine3 = getExtensions >>= (return . member "GL_ATI_texture_env_combine3")

-- | Is the <https://www.opengl.org/registry/specs/ATI/texture_env_combine3.txt ATI_texture_env_combine3> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATITextureEnvCombine3' in those cases instead.
gl_ATI_texture_env_combine3 :: Bool
gl_ATI_texture_env_combine3 = member "GL_ATI_texture_env_combine3" extensions
{-# NOINLINE gl_ATI_texture_env_combine3 #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/texture_float.txt ATI_texture_float> extension supported?
glGetATITextureFloat :: MonadIO m => m Bool
glGetATITextureFloat = getExtensions >>= (return . member "GL_ATI_texture_float")

-- | Is the <https://www.opengl.org/registry/specs/ATI/texture_float.txt ATI_texture_float> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATITextureFloat' in those cases instead.
gl_ATI_texture_float :: Bool
gl_ATI_texture_float = member "GL_ATI_texture_float" extensions
{-# NOINLINE gl_ATI_texture_float #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/texture_mirror_once.txt ATI_texture_mirror_once> extension supported?
glGetATITextureMirrorOnce :: MonadIO m => m Bool
glGetATITextureMirrorOnce = getExtensions >>= (return . member "GL_ATI_texture_mirror_once")

-- | Is the <https://www.opengl.org/registry/specs/ATI/texture_mirror_once.txt ATI_texture_mirror_once> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATITextureMirrorOnce' in those cases instead.
gl_ATI_texture_mirror_once :: Bool
gl_ATI_texture_mirror_once = member "GL_ATI_texture_mirror_once" extensions
{-# NOINLINE gl_ATI_texture_mirror_once #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/vertex_array_object.txt ATI_vertex_array_object> extension supported?
glGetATIVertexArrayObject :: MonadIO m => m Bool
glGetATIVertexArrayObject = getExtensions >>= (return . member "GL_ATI_vertex_array_object")

-- | Is the <https://www.opengl.org/registry/specs/ATI/vertex_array_object.txt ATI_vertex_array_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATIVertexArrayObject' in those cases instead.
gl_ATI_vertex_array_object :: Bool
gl_ATI_vertex_array_object = member "GL_ATI_vertex_array_object" extensions
{-# NOINLINE gl_ATI_vertex_array_object #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/vertex_attrib_array_object.txt ATI_vertex_attrib_array_object> extension supported?
glGetATIVertexAttribArrayObject :: MonadIO m => m Bool
glGetATIVertexAttribArrayObject = getExtensions >>= (return . member "GL_ATI_vertex_attrib_array_object")

-- | Is the <https://www.opengl.org/registry/specs/ATI/vertex_attrib_array_object.txt ATI_vertex_attrib_array_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATIVertexAttribArrayObject' in those cases instead.
gl_ATI_vertex_attrib_array_object :: Bool
gl_ATI_vertex_attrib_array_object = member "GL_ATI_vertex_attrib_array_object" extensions
{-# NOINLINE gl_ATI_vertex_attrib_array_object #-}

-- | Is the <https://www.opengl.org/registry/specs/ATI/vertex_streams.txt ATI_vertex_streams> extension supported?
glGetATIVertexStreams :: MonadIO m => m Bool
glGetATIVertexStreams = getExtensions >>= (return . member "GL_ATI_vertex_streams")

-- | Is the <https://www.opengl.org/registry/specs/ATI/vertex_streams.txt ATI_vertex_streams> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetATIVertexStreams' in those cases instead.
gl_ATI_vertex_streams :: Bool
gl_ATI_vertex_streams = member "GL_ATI_vertex_streams" extensions
{-# NOINLINE gl_ATI_vertex_streams #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/422_pixels.txt EXT_422_pixels> extension supported?
glGetEXTFourTwoTwoPixels :: MonadIO m => m Bool
glGetEXTFourTwoTwoPixels = getExtensions >>= (return . member "GL_EXT_422_pixels")

-- | Is the <https://www.opengl.org/registry/specs/EXT/422_pixels.txt EXT_422_pixels> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTFourTwoTwoPixels' in those cases instead.
gl_EXT_422_pixels :: Bool
gl_EXT_422_pixels = member "GL_EXT_422_pixels" extensions
{-# NOINLINE gl_EXT_422_pixels #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/EGL_image_storage.txt EXT_EGL_image_storage> extension supported?
glGetEXTEglImageStorage :: MonadIO m => m Bool
glGetEXTEglImageStorage = getExtensions >>= (return . member "GL_EXT_EGL_image_storage")

-- | Is the <https://www.opengl.org/registry/specs/EXT/EGL_image_storage.txt EXT_EGL_image_storage> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTEglImageStorage' in those cases instead.
gl_EXT_EGL_image_storage :: Bool
gl_EXT_EGL_image_storage = member "GL_EXT_EGL_image_storage" extensions
{-# NOINLINE gl_EXT_EGL_image_storage #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/abgr.txt EXT_abgr> extension supported?
glGetEXTABGR :: MonadIO m => m Bool
glGetEXTABGR = getExtensions >>= (return . member "GL_EXT_abgr")

-- | Is the <https://www.opengl.org/registry/specs/EXT/abgr.txt EXT_abgr> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTABGR' in those cases instead.
gl_EXT_abgr :: Bool
gl_EXT_abgr = member "GL_EXT_abgr" extensions
{-# NOINLINE gl_EXT_abgr #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/bgra.txt EXT_bgra> extension supported?
glGetEXTBGRA :: MonadIO m => m Bool
glGetEXTBGRA = getExtensions >>= (return . member "GL_EXT_bgra")

-- | Is the <https://www.opengl.org/registry/specs/EXT/bgra.txt EXT_bgra> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTBGRA' in those cases instead.
gl_EXT_bgra :: Bool
gl_EXT_bgra = member "GL_EXT_bgra" extensions
{-# NOINLINE gl_EXT_bgra #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/bindable_uniform.txt EXT_bindable_uniform> extension supported?
glGetEXTBindableUniform :: MonadIO m => m Bool
glGetEXTBindableUniform = getExtensions >>= (return . member "GL_EXT_bindable_uniform")

-- | Is the <https://www.opengl.org/registry/specs/EXT/bindable_uniform.txt EXT_bindable_uniform> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTBindableUniform' in those cases instead.
gl_EXT_bindable_uniform :: Bool
gl_EXT_bindable_uniform = member "GL_EXT_bindable_uniform" extensions
{-# NOINLINE gl_EXT_bindable_uniform #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_color.txt EXT_blend_color> extension supported?
glGetEXTBlendColor :: MonadIO m => m Bool
glGetEXTBlendColor = getExtensions >>= (return . member "GL_EXT_blend_color")

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_color.txt EXT_blend_color> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTBlendColor' in those cases instead.
gl_EXT_blend_color :: Bool
gl_EXT_blend_color = member "GL_EXT_blend_color" extensions
{-# NOINLINE gl_EXT_blend_color #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_equation_separate.txt EXT_blend_equation_separate> extension supported?
glGetEXTBlendEquationSeparate :: MonadIO m => m Bool
glGetEXTBlendEquationSeparate = getExtensions >>= (return . member "GL_EXT_blend_equation_separate")

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_equation_separate.txt EXT_blend_equation_separate> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTBlendEquationSeparate' in those cases instead.
gl_EXT_blend_equation_separate :: Bool
gl_EXT_blend_equation_separate = member "GL_EXT_blend_equation_separate" extensions
{-# NOINLINE gl_EXT_blend_equation_separate #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_func_separate.txt EXT_blend_func_separate> extension supported?
glGetEXTBlendFuncSeparate :: MonadIO m => m Bool
glGetEXTBlendFuncSeparate = getExtensions >>= (return . member "GL_EXT_blend_func_separate")

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_func_separate.txt EXT_blend_func_separate> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTBlendFuncSeparate' in those cases instead.
gl_EXT_blend_func_separate :: Bool
gl_EXT_blend_func_separate = member "GL_EXT_blend_func_separate" extensions
{-# NOINLINE gl_EXT_blend_func_separate #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_minmax.txt EXT_blend_minmax> extension supported?
glGetEXTBlendMinmax :: MonadIO m => m Bool
glGetEXTBlendMinmax = getExtensions >>= (return . member "GL_EXT_blend_minmax")

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_minmax.txt EXT_blend_minmax> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTBlendMinmax' in those cases instead.
gl_EXT_blend_minmax :: Bool
gl_EXT_blend_minmax = member "GL_EXT_blend_minmax" extensions
{-# NOINLINE gl_EXT_blend_minmax #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_subtract.txt EXT_blend_subtract> extension supported?
glGetEXTBlendSubtract :: MonadIO m => m Bool
glGetEXTBlendSubtract = getExtensions >>= (return . member "GL_EXT_blend_subtract")

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_subtract.txt EXT_blend_subtract> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTBlendSubtract' in those cases instead.
gl_EXT_blend_subtract :: Bool
gl_EXT_blend_subtract = member "GL_EXT_blend_subtract" extensions
{-# NOINLINE gl_EXT_blend_subtract #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/clip_volume_hint.txt EXT_clip_volume_hint> extension supported?
glGetEXTClipVolumeHint :: MonadIO m => m Bool
glGetEXTClipVolumeHint = getExtensions >>= (return . member "GL_EXT_clip_volume_hint")

-- | Is the <https://www.opengl.org/registry/specs/EXT/clip_volume_hint.txt EXT_clip_volume_hint> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTClipVolumeHint' in those cases instead.
gl_EXT_clip_volume_hint :: Bool
gl_EXT_clip_volume_hint = member "GL_EXT_clip_volume_hint" extensions
{-# NOINLINE gl_EXT_clip_volume_hint #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/cmyka.txt EXT_cmyka> extension supported?
glGetEXTCMYKA :: MonadIO m => m Bool
glGetEXTCMYKA = getExtensions >>= (return . member "GL_EXT_cmyka")

-- | Is the <https://www.opengl.org/registry/specs/EXT/cmyka.txt EXT_cmyka> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTCMYKA' in those cases instead.
gl_EXT_cmyka :: Bool
gl_EXT_cmyka = member "GL_EXT_cmyka" extensions
{-# NOINLINE gl_EXT_cmyka #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/color_subtable.txt EXT_color_subtable> extension supported?
glGetEXTColorSubtable :: MonadIO m => m Bool
glGetEXTColorSubtable = getExtensions >>= (return . member "GL_EXT_color_subtable")

-- | Is the <https://www.opengl.org/registry/specs/EXT/color_subtable.txt EXT_color_subtable> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTColorSubtable' in those cases instead.
gl_EXT_color_subtable :: Bool
gl_EXT_color_subtable = member "GL_EXT_color_subtable" extensions
{-# NOINLINE gl_EXT_color_subtable #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/compiled_vertex_array.txt EXT_compiled_vertex_array> extension supported?
glGetEXTCompiledVertexArray :: MonadIO m => m Bool
glGetEXTCompiledVertexArray = getExtensions >>= (return . member "GL_EXT_compiled_vertex_array")

-- | Is the <https://www.opengl.org/registry/specs/EXT/compiled_vertex_array.txt EXT_compiled_vertex_array> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTCompiledVertexArray' in those cases instead.
gl_EXT_compiled_vertex_array :: Bool
gl_EXT_compiled_vertex_array = member "GL_EXT_compiled_vertex_array" extensions
{-# NOINLINE gl_EXT_compiled_vertex_array #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/convolution.txt EXT_convolution> extension supported?
glGetEXTConvolution :: MonadIO m => m Bool
glGetEXTConvolution = getExtensions >>= (return . member "GL_EXT_convolution")

-- | Is the <https://www.opengl.org/registry/specs/EXT/convolution.txt EXT_convolution> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTConvolution' in those cases instead.
gl_EXT_convolution :: Bool
gl_EXT_convolution = member "GL_EXT_convolution" extensions
{-# NOINLINE gl_EXT_convolution #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/coordinate_frame.txt EXT_coordinate_frame> extension supported?
glGetEXTCoordinateFrame :: MonadIO m => m Bool
glGetEXTCoordinateFrame = getExtensions >>= (return . member "GL_EXT_coordinate_frame")

-- | Is the <https://www.opengl.org/registry/specs/EXT/coordinate_frame.txt EXT_coordinate_frame> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTCoordinateFrame' in those cases instead.
gl_EXT_coordinate_frame :: Bool
gl_EXT_coordinate_frame = member "GL_EXT_coordinate_frame" extensions
{-# NOINLINE gl_EXT_coordinate_frame #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/copy_texture.txt EXT_copy_texture> extension supported?
glGetEXTCopyTexture :: MonadIO m => m Bool
glGetEXTCopyTexture = getExtensions >>= (return . member "GL_EXT_copy_texture")

-- | Is the <https://www.opengl.org/registry/specs/EXT/copy_texture.txt EXT_copy_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTCopyTexture' in those cases instead.
gl_EXT_copy_texture :: Bool
gl_EXT_copy_texture = member "GL_EXT_copy_texture" extensions
{-# NOINLINE gl_EXT_copy_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/cull_vertex.txt EXT_cull_vertex> extension supported?
glGetEXTCullVertex :: MonadIO m => m Bool
glGetEXTCullVertex = getExtensions >>= (return . member "GL_EXT_cull_vertex")

-- | Is the <https://www.opengl.org/registry/specs/EXT/cull_vertex.txt EXT_cull_vertex> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTCullVertex' in those cases instead.
gl_EXT_cull_vertex :: Bool
gl_EXT_cull_vertex = member "GL_EXT_cull_vertex" extensions
{-# NOINLINE gl_EXT_cull_vertex #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/EXT_debug_label.txt EXT_debug_label> extension supported?
glGetEXTDebugLabel :: MonadIO m => m Bool
glGetEXTDebugLabel = getExtensions >>= (return . member "GL_EXT_debug_label")

-- | Is the <https://www.opengl.org/registry/specs/EXT/EXT_debug_label.txt EXT_debug_label> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTDebugLabel' in those cases instead.
gl_EXT_debug_label :: Bool
gl_EXT_debug_label = member "GL_EXT_debug_label" extensions
{-# NOINLINE gl_EXT_debug_label #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/EXT_debug_marker.txt EXT_debug_marker> extension supported?
glGetEXTDebugMarker :: MonadIO m => m Bool
glGetEXTDebugMarker = getExtensions >>= (return . member "GL_EXT_debug_marker")

-- | Is the <https://www.opengl.org/registry/specs/EXT/EXT_debug_marker.txt EXT_debug_marker> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTDebugMarker' in those cases instead.
gl_EXT_debug_marker :: Bool
gl_EXT_debug_marker = member "GL_EXT_debug_marker" extensions
{-# NOINLINE gl_EXT_debug_marker #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/depth_bounds_test.txt EXT_depth_bounds_test> extension supported?
glGetEXTDepthBoundsTest :: MonadIO m => m Bool
glGetEXTDepthBoundsTest = getExtensions >>= (return . member "GL_EXT_depth_bounds_test")

-- | Is the <https://www.opengl.org/registry/specs/EXT/depth_bounds_test.txt EXT_depth_bounds_test> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTDepthBoundsTest' in those cases instead.
gl_EXT_depth_bounds_test :: Bool
gl_EXT_depth_bounds_test = member "GL_EXT_depth_bounds_test" extensions
{-# NOINLINE gl_EXT_depth_bounds_test #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/direct_state_access.txt EXT_direct_state_access> extension supported?
glGetEXTDirectStateAccess :: MonadIO m => m Bool
glGetEXTDirectStateAccess = getExtensions >>= (return . member "GL_EXT_direct_state_access")

-- | Is the <https://www.opengl.org/registry/specs/EXT/direct_state_access.txt EXT_direct_state_access> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTDirectStateAccess' in those cases instead.
gl_EXT_direct_state_access :: Bool
gl_EXT_direct_state_access = member "GL_EXT_direct_state_access" extensions
{-# NOINLINE gl_EXT_direct_state_access #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/draw_buffers2.txt EXT_draw_buffers2> extension supported?
glGetEXTDrawBuffers2 :: MonadIO m => m Bool
glGetEXTDrawBuffers2 = getExtensions >>= (return . member "GL_EXT_draw_buffers2")

-- | Is the <https://www.opengl.org/registry/specs/EXT/draw_buffers2.txt EXT_draw_buffers2> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTDrawBuffers2' in those cases instead.
gl_EXT_draw_buffers2 :: Bool
gl_EXT_draw_buffers2 = member "GL_EXT_draw_buffers2" extensions
{-# NOINLINE gl_EXT_draw_buffers2 #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/draw_instanced.txt EXT_draw_instanced> extension supported?
glGetEXTDrawInstanced :: MonadIO m => m Bool
glGetEXTDrawInstanced = getExtensions >>= (return . member "GL_EXT_draw_instanced")

-- | Is the <https://www.opengl.org/registry/specs/EXT/draw_instanced.txt EXT_draw_instanced> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTDrawInstanced' in those cases instead.
gl_EXT_draw_instanced :: Bool
gl_EXT_draw_instanced = member "GL_EXT_draw_instanced" extensions
{-# NOINLINE gl_EXT_draw_instanced #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/draw_range_elements.txt EXT_draw_range_elements> extension supported?
glGetEXTDrawRangeElements :: MonadIO m => m Bool
glGetEXTDrawRangeElements = getExtensions >>= (return . member "GL_EXT_draw_range_elements")

-- | Is the <https://www.opengl.org/registry/specs/EXT/draw_range_elements.txt EXT_draw_range_elements> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTDrawRangeElements' in those cases instead.
gl_EXT_draw_range_elements :: Bool
gl_EXT_draw_range_elements = member "GL_EXT_draw_range_elements" extensions
{-# NOINLINE gl_EXT_draw_range_elements #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/external_buffer.txt EXT_external_buffer> extension supported?
glGetEXTExternalBuffer :: MonadIO m => m Bool
glGetEXTExternalBuffer = getExtensions >>= (return . member "GL_EXT_external_buffer")

-- | Is the <https://www.opengl.org/registry/specs/EXT/external_buffer.txt EXT_external_buffer> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTExternalBuffer' in those cases instead.
gl_EXT_external_buffer :: Bool
gl_EXT_external_buffer = member "GL_EXT_external_buffer" extensions
{-# NOINLINE gl_EXT_external_buffer #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/fog_coord.txt EXT_fog_coord> extension supported?
glGetEXTFogCoord :: MonadIO m => m Bool
glGetEXTFogCoord = getExtensions >>= (return . member "GL_EXT_fog_coord")

-- | Is the <https://www.opengl.org/registry/specs/EXT/fog_coord.txt EXT_fog_coord> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTFogCoord' in those cases instead.
gl_EXT_fog_coord :: Bool
gl_EXT_fog_coord = member "GL_EXT_fog_coord" extensions
{-# NOINLINE gl_EXT_fog_coord #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/framebuffer_blit.txt EXT_framebuffer_blit> extension supported?
glGetEXTFramebufferBlit :: MonadIO m => m Bool
glGetEXTFramebufferBlit = getExtensions >>= (return . member "GL_EXT_framebuffer_blit")

-- | Is the <https://www.opengl.org/registry/specs/EXT/framebuffer_blit.txt EXT_framebuffer_blit> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTFramebufferBlit' in those cases instead.
gl_EXT_framebuffer_blit :: Bool
gl_EXT_framebuffer_blit = member "GL_EXT_framebuffer_blit" extensions
{-# NOINLINE gl_EXT_framebuffer_blit #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/framebuffer_multisample.txt EXT_framebuffer_multisample> extension supported?
glGetEXTFramebufferMultisample :: MonadIO m => m Bool
glGetEXTFramebufferMultisample = getExtensions >>= (return . member "GL_EXT_framebuffer_multisample")

-- | Is the <https://www.opengl.org/registry/specs/EXT/framebuffer_multisample.txt EXT_framebuffer_multisample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTFramebufferMultisample' in those cases instead.
gl_EXT_framebuffer_multisample :: Bool
gl_EXT_framebuffer_multisample = member "GL_EXT_framebuffer_multisample" extensions
{-# NOINLINE gl_EXT_framebuffer_multisample #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/framebuffer_multisample_blit_scaled.txt EXT_framebuffer_multisample_blit_scaled> extension supported?
glGetEXTFramebufferMultisampleBlitScaled :: MonadIO m => m Bool
glGetEXTFramebufferMultisampleBlitScaled = getExtensions >>= (return . member "GL_EXT_framebuffer_multisample_blit_scaled")

-- | Is the <https://www.opengl.org/registry/specs/EXT/framebuffer_multisample_blit_scaled.txt EXT_framebuffer_multisample_blit_scaled> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTFramebufferMultisampleBlitScaled' in those cases instead.
gl_EXT_framebuffer_multisample_blit_scaled :: Bool
gl_EXT_framebuffer_multisample_blit_scaled = member "GL_EXT_framebuffer_multisample_blit_scaled" extensions
{-# NOINLINE gl_EXT_framebuffer_multisample_blit_scaled #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/framebuffer_object.txt EXT_framebuffer_object> extension supported?
glGetEXTFramebufferObject :: MonadIO m => m Bool
glGetEXTFramebufferObject = getExtensions >>= (return . member "GL_EXT_framebuffer_object")

-- | Is the <https://www.opengl.org/registry/specs/EXT/framebuffer_object.txt EXT_framebuffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTFramebufferObject' in those cases instead.
gl_EXT_framebuffer_object :: Bool
gl_EXT_framebuffer_object = member "GL_EXT_framebuffer_object" extensions
{-# NOINLINE gl_EXT_framebuffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/framebuffer_sRGB.txt EXT_framebuffer_sRGB> extension supported?
glGetEXTFramebufferSRGB :: MonadIO m => m Bool
glGetEXTFramebufferSRGB = getExtensions >>= (return . member "GL_EXT_framebuffer_sRGB")

-- | Is the <https://www.opengl.org/registry/specs/EXT/framebuffer_sRGB.txt EXT_framebuffer_sRGB> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTFramebufferSRGB' in those cases instead.
gl_EXT_framebuffer_sRGB :: Bool
gl_EXT_framebuffer_sRGB = member "GL_EXT_framebuffer_sRGB" extensions
{-# NOINLINE gl_EXT_framebuffer_sRGB #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/geometry_shader4.txt EXT_geometry_shader4> extension supported?
glGetEXTGeometryShader4 :: MonadIO m => m Bool
glGetEXTGeometryShader4 = getExtensions >>= (return . member "GL_EXT_geometry_shader4")

-- | Is the <https://www.opengl.org/registry/specs/EXT/geometry_shader4.txt EXT_geometry_shader4> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTGeometryShader4' in those cases instead.
gl_EXT_geometry_shader4 :: Bool
gl_EXT_geometry_shader4 = member "GL_EXT_geometry_shader4" extensions
{-# NOINLINE gl_EXT_geometry_shader4 #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/gpu_program_parameters.txt EXT_gpu_program_parameters> extension supported?
glGetEXTGPUProgramParameters :: MonadIO m => m Bool
glGetEXTGPUProgramParameters = getExtensions >>= (return . member "GL_EXT_gpu_program_parameters")

-- | Is the <https://www.opengl.org/registry/specs/EXT/gpu_program_parameters.txt EXT_gpu_program_parameters> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTGPUProgramParameters' in those cases instead.
gl_EXT_gpu_program_parameters :: Bool
gl_EXT_gpu_program_parameters = member "GL_EXT_gpu_program_parameters" extensions
{-# NOINLINE gl_EXT_gpu_program_parameters #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/gpu_shader4.txt EXT_gpu_shader4> extension supported?
glGetEXTGPUShader4 :: MonadIO m => m Bool
glGetEXTGPUShader4 = getExtensions >>= (return . member "GL_EXT_gpu_shader4")

-- | Is the <https://www.opengl.org/registry/specs/EXT/gpu_shader4.txt EXT_gpu_shader4> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTGPUShader4' in those cases instead.
gl_EXT_gpu_shader4 :: Bool
gl_EXT_gpu_shader4 = member "GL_EXT_gpu_shader4" extensions
{-# NOINLINE gl_EXT_gpu_shader4 #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/histogram.txt EXT_histogram> extension supported?
glGetEXTHistogram :: MonadIO m => m Bool
glGetEXTHistogram = getExtensions >>= (return . member "GL_EXT_histogram")

-- | Is the <https://www.opengl.org/registry/specs/EXT/histogram.txt EXT_histogram> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTHistogram' in those cases instead.
gl_EXT_histogram :: Bool
gl_EXT_histogram = member "GL_EXT_histogram" extensions
{-# NOINLINE gl_EXT_histogram #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/index_array_formats.txt EXT_index_array_formats> extension supported?
glGetEXTIndexArrayFormats :: MonadIO m => m Bool
glGetEXTIndexArrayFormats = getExtensions >>= (return . member "GL_EXT_index_array_formats")

-- | Is the <https://www.opengl.org/registry/specs/EXT/index_array_formats.txt EXT_index_array_formats> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTIndexArrayFormats' in those cases instead.
gl_EXT_index_array_formats :: Bool
gl_EXT_index_array_formats = member "GL_EXT_index_array_formats" extensions
{-# NOINLINE gl_EXT_index_array_formats #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/index_func.txt EXT_index_func> extension supported?
glGetEXTIndexFunc :: MonadIO m => m Bool
glGetEXTIndexFunc = getExtensions >>= (return . member "GL_EXT_index_func")

-- | Is the <https://www.opengl.org/registry/specs/EXT/index_func.txt EXT_index_func> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTIndexFunc' in those cases instead.
gl_EXT_index_func :: Bool
gl_EXT_index_func = member "GL_EXT_index_func" extensions
{-# NOINLINE gl_EXT_index_func #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/index_material.txt EXT_index_material> extension supported?
glGetEXTIndexMaterial :: MonadIO m => m Bool
glGetEXTIndexMaterial = getExtensions >>= (return . member "GL_EXT_index_material")

-- | Is the <https://www.opengl.org/registry/specs/EXT/index_material.txt EXT_index_material> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTIndexMaterial' in those cases instead.
gl_EXT_index_material :: Bool
gl_EXT_index_material = member "GL_EXT_index_material" extensions
{-# NOINLINE gl_EXT_index_material #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/light_texture.txt EXT_light_texture> extension supported?
glGetEXTLightTexture :: MonadIO m => m Bool
glGetEXTLightTexture = getExtensions >>= (return . member "GL_EXT_light_texture")

-- | Is the <https://www.opengl.org/registry/specs/EXT/light_texture.txt EXT_light_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTLightTexture' in those cases instead.
gl_EXT_light_texture :: Bool
gl_EXT_light_texture = member "GL_EXT_light_texture" extensions
{-# NOINLINE gl_EXT_light_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/memory_object.txt EXT_memory_object> extension supported?
glGetEXTMemoryObject :: MonadIO m => m Bool
glGetEXTMemoryObject = getExtensions >>= (return . member "GL_EXT_memory_object")

-- | Is the <https://www.opengl.org/registry/specs/EXT/memory_object.txt EXT_memory_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTMemoryObject' in those cases instead.
gl_EXT_memory_object :: Bool
gl_EXT_memory_object = member "GL_EXT_memory_object" extensions
{-# NOINLINE gl_EXT_memory_object #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/memory_object_fd.txt EXT_memory_object_fd> extension supported?
glGetEXTMemoryObjectFd :: MonadIO m => m Bool
glGetEXTMemoryObjectFd = getExtensions >>= (return . member "GL_EXT_memory_object_fd")

-- | Is the <https://www.opengl.org/registry/specs/EXT/memory_object_fd.txt EXT_memory_object_fd> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTMemoryObjectFd' in those cases instead.
gl_EXT_memory_object_fd :: Bool
gl_EXT_memory_object_fd = member "GL_EXT_memory_object_fd" extensions
{-# NOINLINE gl_EXT_memory_object_fd #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/memory_object_win32.txt EXT_memory_object_win32> extension supported?
glGetEXTMemoryObjectWin32 :: MonadIO m => m Bool
glGetEXTMemoryObjectWin32 = getExtensions >>= (return . member "GL_EXT_memory_object_win32")

-- | Is the <https://www.opengl.org/registry/specs/EXT/memory_object_win32.txt EXT_memory_object_win32> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTMemoryObjectWin32' in those cases instead.
gl_EXT_memory_object_win32 :: Bool
gl_EXT_memory_object_win32 = member "GL_EXT_memory_object_win32" extensions
{-# NOINLINE gl_EXT_memory_object_win32 #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/multi_draw_arrays.txt EXT_multi_draw_arrays> extension supported?
glGetEXTMultiDrawArrays :: MonadIO m => m Bool
glGetEXTMultiDrawArrays = getExtensions >>= (return . member "GL_EXT_multi_draw_arrays")

-- | Is the <https://www.opengl.org/registry/specs/EXT/multi_draw_arrays.txt EXT_multi_draw_arrays> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTMultiDrawArrays' in those cases instead.
gl_EXT_multi_draw_arrays :: Bool
gl_EXT_multi_draw_arrays = member "GL_EXT_multi_draw_arrays" extensions
{-# NOINLINE gl_EXT_multi_draw_arrays #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/wgl_multisample.txt EXT_multisample> extension supported?
glGetEXTMultisample :: MonadIO m => m Bool
glGetEXTMultisample = getExtensions >>= (return . member "GL_EXT_multisample")

-- | Is the <https://www.opengl.org/registry/specs/EXT/wgl_multisample.txt EXT_multisample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTMultisample' in those cases instead.
gl_EXT_multisample :: Bool
gl_EXT_multisample = member "GL_EXT_multisample" extensions
{-# NOINLINE gl_EXT_multisample #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/packed_depth_stencil.txt EXT_packed_depth_stencil> extension supported?
glGetEXTPackedDepthStencil :: MonadIO m => m Bool
glGetEXTPackedDepthStencil = getExtensions >>= (return . member "GL_EXT_packed_depth_stencil")

-- | Is the <https://www.opengl.org/registry/specs/EXT/packed_depth_stencil.txt EXT_packed_depth_stencil> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTPackedDepthStencil' in those cases instead.
gl_EXT_packed_depth_stencil :: Bool
gl_EXT_packed_depth_stencil = member "GL_EXT_packed_depth_stencil" extensions
{-# NOINLINE gl_EXT_packed_depth_stencil #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/packed_float.txt EXT_packed_float> extension supported?
glGetEXTPackedFloat :: MonadIO m => m Bool
glGetEXTPackedFloat = getExtensions >>= (return . member "GL_EXT_packed_float")

-- | Is the <https://www.opengl.org/registry/specs/EXT/packed_float.txt EXT_packed_float> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTPackedFloat' in those cases instead.
gl_EXT_packed_float :: Bool
gl_EXT_packed_float = member "GL_EXT_packed_float" extensions
{-# NOINLINE gl_EXT_packed_float #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/packed_pixels.txt EXT_packed_pixels> extension supported?
glGetEXTPackedPixels :: MonadIO m => m Bool
glGetEXTPackedPixels = getExtensions >>= (return . member "GL_EXT_packed_pixels")

-- | Is the <https://www.opengl.org/registry/specs/EXT/packed_pixels.txt EXT_packed_pixels> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTPackedPixels' in those cases instead.
gl_EXT_packed_pixels :: Bool
gl_EXT_packed_pixels = member "GL_EXT_packed_pixels" extensions
{-# NOINLINE gl_EXT_packed_pixels #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/paletted_texture.txt EXT_paletted_texture> extension supported?
glGetEXTPalettedTexture :: MonadIO m => m Bool
glGetEXTPalettedTexture = getExtensions >>= (return . member "GL_EXT_paletted_texture")

-- | Is the <https://www.opengl.org/registry/specs/EXT/paletted_texture.txt EXT_paletted_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTPalettedTexture' in those cases instead.
gl_EXT_paletted_texture :: Bool
gl_EXT_paletted_texture = member "GL_EXT_paletted_texture" extensions
{-# NOINLINE gl_EXT_paletted_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/pixel_buffer_object.txt EXT_pixel_buffer_object> extension supported?
glGetEXTPixelBufferObject :: MonadIO m => m Bool
glGetEXTPixelBufferObject = getExtensions >>= (return . member "GL_EXT_pixel_buffer_object")

-- | Is the <https://www.opengl.org/registry/specs/EXT/pixel_buffer_object.txt EXT_pixel_buffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTPixelBufferObject' in those cases instead.
gl_EXT_pixel_buffer_object :: Bool
gl_EXT_pixel_buffer_object = member "GL_EXT_pixel_buffer_object" extensions
{-# NOINLINE gl_EXT_pixel_buffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/pixel_transform.txt EXT_pixel_transform> extension supported?
glGetEXTPixelTransform :: MonadIO m => m Bool
glGetEXTPixelTransform = getExtensions >>= (return . member "GL_EXT_pixel_transform")

-- | Is the <https://www.opengl.org/registry/specs/EXT/pixel_transform.txt EXT_pixel_transform> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTPixelTransform' in those cases instead.
gl_EXT_pixel_transform :: Bool
gl_EXT_pixel_transform = member "GL_EXT_pixel_transform" extensions
{-# NOINLINE gl_EXT_pixel_transform #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/point_parameters.txt EXT_point_parameters> extension supported?
glGetEXTPointParameters :: MonadIO m => m Bool
glGetEXTPointParameters = getExtensions >>= (return . member "GL_EXT_point_parameters")

-- | Is the <https://www.opengl.org/registry/specs/EXT/point_parameters.txt EXT_point_parameters> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTPointParameters' in those cases instead.
gl_EXT_point_parameters :: Bool
gl_EXT_point_parameters = member "GL_EXT_point_parameters" extensions
{-# NOINLINE gl_EXT_point_parameters #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/polygon_offset.txt EXT_polygon_offset> extension supported?
glGetEXTPolygonOffset :: MonadIO m => m Bool
glGetEXTPolygonOffset = getExtensions >>= (return . member "GL_EXT_polygon_offset")

-- | Is the <https://www.opengl.org/registry/specs/EXT/polygon_offset.txt EXT_polygon_offset> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTPolygonOffset' in those cases instead.
gl_EXT_polygon_offset :: Bool
gl_EXT_polygon_offset = member "GL_EXT_polygon_offset" extensions
{-# NOINLINE gl_EXT_polygon_offset #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/polygon_offset_clamp.txt EXT_polygon_offset_clamp> extension supported?
glGetEXTPolygonOffsetClamp :: MonadIO m => m Bool
glGetEXTPolygonOffsetClamp = getExtensions >>= (return . member "GL_EXT_polygon_offset_clamp")

-- | Is the <https://www.opengl.org/registry/specs/EXT/polygon_offset_clamp.txt EXT_polygon_offset_clamp> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTPolygonOffsetClamp' in those cases instead.
gl_EXT_polygon_offset_clamp :: Bool
gl_EXT_polygon_offset_clamp = member "GL_EXT_polygon_offset_clamp" extensions
{-# NOINLINE gl_EXT_polygon_offset_clamp #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/provoking_vertex.txt EXT_provoking_vertex> extension supported?
glGetEXTProvokingVertex :: MonadIO m => m Bool
glGetEXTProvokingVertex = getExtensions >>= (return . member "GL_EXT_provoking_vertex")

-- | Is the <https://www.opengl.org/registry/specs/EXT/provoking_vertex.txt EXT_provoking_vertex> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTProvokingVertex' in those cases instead.
gl_EXT_provoking_vertex :: Bool
gl_EXT_provoking_vertex = member "GL_EXT_provoking_vertex" extensions
{-# NOINLINE gl_EXT_provoking_vertex #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/raster_multisample.txt EXT_raster_multisample> extension supported?
glGetEXTRasterMultisample :: MonadIO m => m Bool
glGetEXTRasterMultisample = getExtensions >>= (return . member "GL_EXT_raster_multisample")

-- | Is the <https://www.opengl.org/registry/specs/EXT/raster_multisample.txt EXT_raster_multisample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTRasterMultisample' in those cases instead.
gl_EXT_raster_multisample :: Bool
gl_EXT_raster_multisample = member "GL_EXT_raster_multisample" extensions
{-# NOINLINE gl_EXT_raster_multisample #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/rescale_normal.txt EXT_rescale_normal> extension supported?
glGetEXTRescaleNormal :: MonadIO m => m Bool
glGetEXTRescaleNormal = getExtensions >>= (return . member "GL_EXT_rescale_normal")

-- | Is the <https://www.opengl.org/registry/specs/EXT/rescale_normal.txt EXT_rescale_normal> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTRescaleNormal' in those cases instead.
gl_EXT_rescale_normal :: Bool
gl_EXT_rescale_normal = member "GL_EXT_rescale_normal" extensions
{-# NOINLINE gl_EXT_rescale_normal #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/secondary_color.txt EXT_secondary_color> extension supported?
glGetEXTSecondaryColor :: MonadIO m => m Bool
glGetEXTSecondaryColor = getExtensions >>= (return . member "GL_EXT_secondary_color")

-- | Is the <https://www.opengl.org/registry/specs/EXT/secondary_color.txt EXT_secondary_color> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTSecondaryColor' in those cases instead.
gl_EXT_secondary_color :: Bool
gl_EXT_secondary_color = member "GL_EXT_secondary_color" extensions
{-# NOINLINE gl_EXT_secondary_color #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/semaphore.txt EXT_semaphore> extension supported?
glGetEXTSemaphore :: MonadIO m => m Bool
glGetEXTSemaphore = getExtensions >>= (return . member "GL_EXT_semaphore")

-- | Is the <https://www.opengl.org/registry/specs/EXT/semaphore.txt EXT_semaphore> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTSemaphore' in those cases instead.
gl_EXT_semaphore :: Bool
gl_EXT_semaphore = member "GL_EXT_semaphore" extensions
{-# NOINLINE gl_EXT_semaphore #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/semaphore_fd.txt EXT_semaphore_fd> extension supported?
glGetEXTSemaphoreFd :: MonadIO m => m Bool
glGetEXTSemaphoreFd = getExtensions >>= (return . member "GL_EXT_semaphore_fd")

-- | Is the <https://www.opengl.org/registry/specs/EXT/semaphore_fd.txt EXT_semaphore_fd> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTSemaphoreFd' in those cases instead.
gl_EXT_semaphore_fd :: Bool
gl_EXT_semaphore_fd = member "GL_EXT_semaphore_fd" extensions
{-# NOINLINE gl_EXT_semaphore_fd #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/semaphore_win32.txt EXT_semaphore_win32> extension supported?
glGetEXTSemaphoreWin32 :: MonadIO m => m Bool
glGetEXTSemaphoreWin32 = getExtensions >>= (return . member "GL_EXT_semaphore_win32")

-- | Is the <https://www.opengl.org/registry/specs/EXT/semaphore_win32.txt EXT_semaphore_win32> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTSemaphoreWin32' in those cases instead.
gl_EXT_semaphore_win32 :: Bool
gl_EXT_semaphore_win32 = member "GL_EXT_semaphore_win32" extensions
{-# NOINLINE gl_EXT_semaphore_win32 #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/separate_shader_objects.txt EXT_separate_shader_objects> extension supported?
glGetEXTSeparateShaderObjects :: MonadIO m => m Bool
glGetEXTSeparateShaderObjects = getExtensions >>= (return . member "GL_EXT_separate_shader_objects")

-- | Is the <https://www.opengl.org/registry/specs/EXT/separate_shader_objects.txt EXT_separate_shader_objects> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTSeparateShaderObjects' in those cases instead.
gl_EXT_separate_shader_objects :: Bool
gl_EXT_separate_shader_objects = member "GL_EXT_separate_shader_objects" extensions
{-# NOINLINE gl_EXT_separate_shader_objects #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/separate_specular_color.txt EXT_separate_specular_color> extension supported?
glGetEXTSeparateSpecularColor :: MonadIO m => m Bool
glGetEXTSeparateSpecularColor = getExtensions >>= (return . member "GL_EXT_separate_specular_color")

-- | Is the <https://www.opengl.org/registry/specs/EXT/separate_specular_color.txt EXT_separate_specular_color> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTSeparateSpecularColor' in those cases instead.
gl_EXT_separate_specular_color :: Bool
gl_EXT_separate_specular_color = member "GL_EXT_separate_specular_color" extensions
{-# NOINLINE gl_EXT_separate_specular_color #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/shader_framebuffer_fetch.txt EXT_shader_framebuffer_fetch> extension supported?
glGetEXTShaderFramebufferFetch :: MonadIO m => m Bool
glGetEXTShaderFramebufferFetch = getExtensions >>= (return . member "GL_EXT_shader_framebuffer_fetch")

-- | Is the <https://www.opengl.org/registry/specs/EXT/shader_framebuffer_fetch.txt EXT_shader_framebuffer_fetch> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTShaderFramebufferFetch' in those cases instead.
gl_EXT_shader_framebuffer_fetch :: Bool
gl_EXT_shader_framebuffer_fetch = member "GL_EXT_shader_framebuffer_fetch" extensions
{-# NOINLINE gl_EXT_shader_framebuffer_fetch #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/shader_framebuffer_fetch_non_coherent.txt EXT_shader_framebuffer_fetch_non_coherent> extension supported?
glGetEXTShaderFramebufferFetchNonCoherent :: MonadIO m => m Bool
glGetEXTShaderFramebufferFetchNonCoherent = getExtensions >>= (return . member "GL_EXT_shader_framebuffer_fetch_non_coherent")

-- | Is the <https://www.opengl.org/registry/specs/EXT/shader_framebuffer_fetch_non_coherent.txt EXT_shader_framebuffer_fetch_non_coherent> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTShaderFramebufferFetchNonCoherent' in those cases instead.
gl_EXT_shader_framebuffer_fetch_non_coherent :: Bool
gl_EXT_shader_framebuffer_fetch_non_coherent = member "GL_EXT_shader_framebuffer_fetch_non_coherent" extensions
{-# NOINLINE gl_EXT_shader_framebuffer_fetch_non_coherent #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/shader_image_load_store.txt EXT_shader_image_load_store> extension supported?
glGetEXTShaderImageLoadStore :: MonadIO m => m Bool
glGetEXTShaderImageLoadStore = getExtensions >>= (return . member "GL_EXT_shader_image_load_store")

-- | Is the <https://www.opengl.org/registry/specs/EXT/shader_image_load_store.txt EXT_shader_image_load_store> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTShaderImageLoadStore' in those cases instead.
gl_EXT_shader_image_load_store :: Bool
gl_EXT_shader_image_load_store = member "GL_EXT_shader_image_load_store" extensions
{-# NOINLINE gl_EXT_shader_image_load_store #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/shared_texture_palette.txt EXT_shared_texture_palette> extension supported?
glGetEXTSharedTexturePalette :: MonadIO m => m Bool
glGetEXTSharedTexturePalette = getExtensions >>= (return . member "GL_EXT_shared_texture_palette")

-- | Is the <https://www.opengl.org/registry/specs/EXT/shared_texture_palette.txt EXT_shared_texture_palette> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTSharedTexturePalette' in those cases instead.
gl_EXT_shared_texture_palette :: Bool
gl_EXT_shared_texture_palette = member "GL_EXT_shared_texture_palette" extensions
{-# NOINLINE gl_EXT_shared_texture_palette #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/stencil_clear_tag.txt EXT_stencil_clear_tag> extension supported?
glGetEXTStencilClearTag :: MonadIO m => m Bool
glGetEXTStencilClearTag = getExtensions >>= (return . member "GL_EXT_stencil_clear_tag")

-- | Is the <https://www.opengl.org/registry/specs/EXT/stencil_clear_tag.txt EXT_stencil_clear_tag> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTStencilClearTag' in those cases instead.
gl_EXT_stencil_clear_tag :: Bool
gl_EXT_stencil_clear_tag = member "GL_EXT_stencil_clear_tag" extensions
{-# NOINLINE gl_EXT_stencil_clear_tag #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/stencil_two_side.txt EXT_stencil_two_side> extension supported?
glGetEXTStencilTwoSide :: MonadIO m => m Bool
glGetEXTStencilTwoSide = getExtensions >>= (return . member "GL_EXT_stencil_two_side")

-- | Is the <https://www.opengl.org/registry/specs/EXT/stencil_two_side.txt EXT_stencil_two_side> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTStencilTwoSide' in those cases instead.
gl_EXT_stencil_two_side :: Bool
gl_EXT_stencil_two_side = member "GL_EXT_stencil_two_side" extensions
{-# NOINLINE gl_EXT_stencil_two_side #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/stencil_wrap.txt EXT_stencil_wrap> extension supported?
glGetEXTStencilWrap :: MonadIO m => m Bool
glGetEXTStencilWrap = getExtensions >>= (return . member "GL_EXT_stencil_wrap")

-- | Is the <https://www.opengl.org/registry/specs/EXT/stencil_wrap.txt EXT_stencil_wrap> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTStencilWrap' in those cases instead.
gl_EXT_stencil_wrap :: Bool
gl_EXT_stencil_wrap = member "GL_EXT_stencil_wrap" extensions
{-# NOINLINE gl_EXT_stencil_wrap #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/subtexture.txt EXT_subtexture> extension supported?
glGetEXTSubtexture :: MonadIO m => m Bool
glGetEXTSubtexture = getExtensions >>= (return . member "GL_EXT_subtexture")

-- | Is the <https://www.opengl.org/registry/specs/EXT/subtexture.txt EXT_subtexture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTSubtexture' in those cases instead.
gl_EXT_subtexture :: Bool
gl_EXT_subtexture = member "GL_EXT_subtexture" extensions
{-# NOINLINE gl_EXT_subtexture #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture.txt EXT_texture> extension supported?
glGetEXTTexture :: MonadIO m => m Bool
glGetEXTTexture = getExtensions >>= (return . member "GL_EXT_texture")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture.txt EXT_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTexture' in those cases instead.
gl_EXT_texture :: Bool
gl_EXT_texture = member "GL_EXT_texture" extensions
{-# NOINLINE gl_EXT_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture3D.txt EXT_texture3D> extension supported?
glGetEXTTexture3D :: MonadIO m => m Bool
glGetEXTTexture3D = getExtensions >>= (return . member "GL_EXT_texture3D")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture3D.txt EXT_texture3D> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTexture3D' in those cases instead.
gl_EXT_texture3D :: Bool
gl_EXT_texture3D = member "GL_EXT_texture3D" extensions
{-# NOINLINE gl_EXT_texture3D #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_array.txt EXT_texture_array> extension supported?
glGetEXTTextureArray :: MonadIO m => m Bool
glGetEXTTextureArray = getExtensions >>= (return . member "GL_EXT_texture_array")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_array.txt EXT_texture_array> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureArray' in those cases instead.
gl_EXT_texture_array :: Bool
gl_EXT_texture_array = member "GL_EXT_texture_array" extensions
{-# NOINLINE gl_EXT_texture_array #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_buffer_object.txt EXT_texture_buffer_object> extension supported?
glGetEXTTextureBufferObject :: MonadIO m => m Bool
glGetEXTTextureBufferObject = getExtensions >>= (return . member "GL_EXT_texture_buffer_object")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_buffer_object.txt EXT_texture_buffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureBufferObject' in those cases instead.
gl_EXT_texture_buffer_object :: Bool
gl_EXT_texture_buffer_object = member "GL_EXT_texture_buffer_object" extensions
{-# NOINLINE gl_EXT_texture_buffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_compression_latc.txt EXT_texture_compression_latc> extension supported?
glGetEXTTextureCompressionLATC :: MonadIO m => m Bool
glGetEXTTextureCompressionLATC = getExtensions >>= (return . member "GL_EXT_texture_compression_latc")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_compression_latc.txt EXT_texture_compression_latc> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureCompressionLATC' in those cases instead.
gl_EXT_texture_compression_latc :: Bool
gl_EXT_texture_compression_latc = member "GL_EXT_texture_compression_latc" extensions
{-# NOINLINE gl_EXT_texture_compression_latc #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_compression_rgtc.txt EXT_texture_compression_rgtc> extension supported?
glGetEXTTextureCompressionRGTC :: MonadIO m => m Bool
glGetEXTTextureCompressionRGTC = getExtensions >>= (return . member "GL_EXT_texture_compression_rgtc")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_compression_rgtc.txt EXT_texture_compression_rgtc> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureCompressionRGTC' in those cases instead.
gl_EXT_texture_compression_rgtc :: Bool
gl_EXT_texture_compression_rgtc = member "GL_EXT_texture_compression_rgtc" extensions
{-# NOINLINE gl_EXT_texture_compression_rgtc #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_compression_s3tc.txt EXT_texture_compression_s3tc> extension supported?
glGetEXTTextureCompressionS3TC :: MonadIO m => m Bool
glGetEXTTextureCompressionS3TC = getExtensions >>= (return . member "GL_EXT_texture_compression_s3tc")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_compression_s3tc.txt EXT_texture_compression_s3tc> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureCompressionS3TC' in those cases instead.
gl_EXT_texture_compression_s3tc :: Bool
gl_EXT_texture_compression_s3tc = member "GL_EXT_texture_compression_s3tc" extensions
{-# NOINLINE gl_EXT_texture_compression_s3tc #-}

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_cube_map.txt EXT_texture_cube_map> extension supported?
glGetEXTTextureCubeMap :: MonadIO m => m Bool
glGetEXTTextureCubeMap = getExtensions >>= (return . member "GL_EXT_texture_cube_map")

-- | Is the <https://www.opengl.org/registry/specs/ARB/texture_cube_map.txt EXT_texture_cube_map> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureCubeMap' in those cases instead.
gl_EXT_texture_cube_map :: Bool
gl_EXT_texture_cube_map = member "GL_EXT_texture_cube_map" extensions
{-# NOINLINE gl_EXT_texture_cube_map #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_env_combine.txt EXT_texture_env_combine> extension supported?
glGetEXTTextureEnvCombine :: MonadIO m => m Bool
glGetEXTTextureEnvCombine = getExtensions >>= (return . member "GL_EXT_texture_env_combine")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_env_combine.txt EXT_texture_env_combine> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureEnvCombine' in those cases instead.
gl_EXT_texture_env_combine :: Bool
gl_EXT_texture_env_combine = member "GL_EXT_texture_env_combine" extensions
{-# NOINLINE gl_EXT_texture_env_combine #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_env_dot3.txt EXT_texture_env_dot3> extension supported?
glGetEXTTextureEnvDot3 :: MonadIO m => m Bool
glGetEXTTextureEnvDot3 = getExtensions >>= (return . member "GL_EXT_texture_env_dot3")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_env_dot3.txt EXT_texture_env_dot3> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureEnvDot3' in those cases instead.
gl_EXT_texture_env_dot3 :: Bool
gl_EXT_texture_env_dot3 = member "GL_EXT_texture_env_dot3" extensions
{-# NOINLINE gl_EXT_texture_env_dot3 #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_filter_anisotropic.txt EXT_texture_filter_anisotropic> extension supported?
glGetEXTTextureFilterAnisotropic :: MonadIO m => m Bool
glGetEXTTextureFilterAnisotropic = getExtensions >>= (return . member "GL_EXT_texture_filter_anisotropic")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_filter_anisotropic.txt EXT_texture_filter_anisotropic> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureFilterAnisotropic' in those cases instead.
gl_EXT_texture_filter_anisotropic :: Bool
gl_EXT_texture_filter_anisotropic = member "GL_EXT_texture_filter_anisotropic" extensions
{-# NOINLINE gl_EXT_texture_filter_anisotropic #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_filter_minmax.txt EXT_texture_filter_minmax> extension supported?
glGetEXTTextureFilterMinmax :: MonadIO m => m Bool
glGetEXTTextureFilterMinmax = getExtensions >>= (return . member "GL_EXT_texture_filter_minmax")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_filter_minmax.txt EXT_texture_filter_minmax> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureFilterMinmax' in those cases instead.
gl_EXT_texture_filter_minmax :: Bool
gl_EXT_texture_filter_minmax = member "GL_EXT_texture_filter_minmax" extensions
{-# NOINLINE gl_EXT_texture_filter_minmax #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_integer.txt EXT_texture_integer> extension supported?
glGetEXTTextureInteger :: MonadIO m => m Bool
glGetEXTTextureInteger = getExtensions >>= (return . member "GL_EXT_texture_integer")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_integer.txt EXT_texture_integer> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureInteger' in those cases instead.
gl_EXT_texture_integer :: Bool
gl_EXT_texture_integer = member "GL_EXT_texture_integer" extensions
{-# NOINLINE gl_EXT_texture_integer #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_lod_bias.txt EXT_texture_lod_bias> extension supported?
glGetEXTTextureLODBias :: MonadIO m => m Bool
glGetEXTTextureLODBias = getExtensions >>= (return . member "GL_EXT_texture_lod_bias")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_lod_bias.txt EXT_texture_lod_bias> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureLODBias' in those cases instead.
gl_EXT_texture_lod_bias :: Bool
gl_EXT_texture_lod_bias = member "GL_EXT_texture_lod_bias" extensions
{-# NOINLINE gl_EXT_texture_lod_bias #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_mirror_clamp.txt EXT_texture_mirror_clamp> extension supported?
glGetEXTTextureMirrorClamp :: MonadIO m => m Bool
glGetEXTTextureMirrorClamp = getExtensions >>= (return . member "GL_EXT_texture_mirror_clamp")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_mirror_clamp.txt EXT_texture_mirror_clamp> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureMirrorClamp' in those cases instead.
gl_EXT_texture_mirror_clamp :: Bool
gl_EXT_texture_mirror_clamp = member "GL_EXT_texture_mirror_clamp" extensions
{-# NOINLINE gl_EXT_texture_mirror_clamp #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_object.txt EXT_texture_object> extension supported?
glGetEXTTextureObject :: MonadIO m => m Bool
glGetEXTTextureObject = getExtensions >>= (return . member "GL_EXT_texture_object")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_object.txt EXT_texture_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureObject' in those cases instead.
gl_EXT_texture_object :: Bool
gl_EXT_texture_object = member "GL_EXT_texture_object" extensions
{-# NOINLINE gl_EXT_texture_object #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_perturb_normal.txt EXT_texture_perturb_normal> extension supported?
glGetEXTTexturePerturbNormal :: MonadIO m => m Bool
glGetEXTTexturePerturbNormal = getExtensions >>= (return . member "GL_EXT_texture_perturb_normal")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_perturb_normal.txt EXT_texture_perturb_normal> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTexturePerturbNormal' in those cases instead.
gl_EXT_texture_perturb_normal :: Bool
gl_EXT_texture_perturb_normal = member "GL_EXT_texture_perturb_normal" extensions
{-# NOINLINE gl_EXT_texture_perturb_normal #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_sRGB.txt EXT_texture_sRGB> extension supported?
glGetEXTTextureSRGB :: MonadIO m => m Bool
glGetEXTTextureSRGB = getExtensions >>= (return . member "GL_EXT_texture_sRGB")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_sRGB.txt EXT_texture_sRGB> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureSRGB' in those cases instead.
gl_EXT_texture_sRGB :: Bool
gl_EXT_texture_sRGB = member "GL_EXT_texture_sRGB" extensions
{-# NOINLINE gl_EXT_texture_sRGB #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_sRGB_R8.txt EXT_texture_sRGB_R8> extension supported?
glGetEXTTextureSRGBR8 :: MonadIO m => m Bool
glGetEXTTextureSRGBR8 = getExtensions >>= (return . member "GL_EXT_texture_sRGB_R8")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_sRGB_R8.txt EXT_texture_sRGB_R8> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureSRGBR8' in those cases instead.
gl_EXT_texture_sRGB_R8 :: Bool
gl_EXT_texture_sRGB_R8 = member "GL_EXT_texture_sRGB_R8" extensions
{-# NOINLINE gl_EXT_texture_sRGB_R8 #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_sRGB_decode.txt EXT_texture_sRGB_decode> extension supported?
glGetEXTTextureSRGBDecode :: MonadIO m => m Bool
glGetEXTTextureSRGBDecode = getExtensions >>= (return . member "GL_EXT_texture_sRGB_decode")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_sRGB_decode.txt EXT_texture_sRGB_decode> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureSRGBDecode' in those cases instead.
gl_EXT_texture_sRGB_decode :: Bool
gl_EXT_texture_sRGB_decode = member "GL_EXT_texture_sRGB_decode" extensions
{-# NOINLINE gl_EXT_texture_sRGB_decode #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_shared_exponent.txt EXT_texture_shared_exponent> extension supported?
glGetEXTTextureSharedExponent :: MonadIO m => m Bool
glGetEXTTextureSharedExponent = getExtensions >>= (return . member "GL_EXT_texture_shared_exponent")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_shared_exponent.txt EXT_texture_shared_exponent> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureSharedExponent' in those cases instead.
gl_EXT_texture_shared_exponent :: Bool
gl_EXT_texture_shared_exponent = member "GL_EXT_texture_shared_exponent" extensions
{-# NOINLINE gl_EXT_texture_shared_exponent #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_snorm.txt EXT_texture_snorm> extension supported?
glGetEXTTextureSNorm :: MonadIO m => m Bool
glGetEXTTextureSNorm = getExtensions >>= (return . member "GL_EXT_texture_snorm")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_snorm.txt EXT_texture_snorm> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureSNorm' in those cases instead.
gl_EXT_texture_snorm :: Bool
gl_EXT_texture_snorm = member "GL_EXT_texture_snorm" extensions
{-# NOINLINE gl_EXT_texture_snorm #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_swizzle.txt EXT_texture_swizzle> extension supported?
glGetEXTTextureSwizzle :: MonadIO m => m Bool
glGetEXTTextureSwizzle = getExtensions >>= (return . member "GL_EXT_texture_swizzle")

-- | Is the <https://www.opengl.org/registry/specs/EXT/texture_swizzle.txt EXT_texture_swizzle> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTextureSwizzle' in those cases instead.
gl_EXT_texture_swizzle :: Bool
gl_EXT_texture_swizzle = member "GL_EXT_texture_swizzle" extensions
{-# NOINLINE gl_EXT_texture_swizzle #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/timer_query.txt EXT_timer_query> extension supported?
glGetEXTTimerQuery :: MonadIO m => m Bool
glGetEXTTimerQuery = getExtensions >>= (return . member "GL_EXT_timer_query")

-- | Is the <https://www.opengl.org/registry/specs/EXT/timer_query.txt EXT_timer_query> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTimerQuery' in those cases instead.
gl_EXT_timer_query :: Bool
gl_EXT_timer_query = member "GL_EXT_timer_query" extensions
{-# NOINLINE gl_EXT_timer_query #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/transform_feedback.txt EXT_transform_feedback> extension supported?
glGetEXTTransformFeedback :: MonadIO m => m Bool
glGetEXTTransformFeedback = getExtensions >>= (return . member "GL_EXT_transform_feedback")

-- | Is the <https://www.opengl.org/registry/specs/EXT/transform_feedback.txt EXT_transform_feedback> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTTransformFeedback' in those cases instead.
gl_EXT_transform_feedback :: Bool
gl_EXT_transform_feedback = member "GL_EXT_transform_feedback" extensions
{-# NOINLINE gl_EXT_transform_feedback #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/vertex_array.txt EXT_vertex_array> extension supported?
glGetEXTVertexArray :: MonadIO m => m Bool
glGetEXTVertexArray = getExtensions >>= (return . member "GL_EXT_vertex_array")

-- | Is the <https://www.opengl.org/registry/specs/EXT/vertex_array.txt EXT_vertex_array> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTVertexArray' in those cases instead.
gl_EXT_vertex_array :: Bool
gl_EXT_vertex_array = member "GL_EXT_vertex_array" extensions
{-# NOINLINE gl_EXT_vertex_array #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/vertex_array_bgra.txt EXT_vertex_array_bgra> extension supported?
glGetEXTVertexArrayBGRA :: MonadIO m => m Bool
glGetEXTVertexArrayBGRA = getExtensions >>= (return . member "GL_EXT_vertex_array_bgra")

-- | Is the <https://www.opengl.org/registry/specs/EXT/vertex_array_bgra.txt EXT_vertex_array_bgra> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTVertexArrayBGRA' in those cases instead.
gl_EXT_vertex_array_bgra :: Bool
gl_EXT_vertex_array_bgra = member "GL_EXT_vertex_array_bgra" extensions
{-# NOINLINE gl_EXT_vertex_array_bgra #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt EXT_vertex_attrib_64bit> extension supported?
glGetEXTVertexAttrib64Bit :: MonadIO m => m Bool
glGetEXTVertexAttrib64Bit = getExtensions >>= (return . member "GL_EXT_vertex_attrib_64bit")

-- | Is the <https://www.opengl.org/registry/specs/EXT/vertex_attrib_64bit.txt EXT_vertex_attrib_64bit> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTVertexAttrib64Bit' in those cases instead.
gl_EXT_vertex_attrib_64bit :: Bool
gl_EXT_vertex_attrib_64bit = member "GL_EXT_vertex_attrib_64bit" extensions
{-# NOINLINE gl_EXT_vertex_attrib_64bit #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/vertex_shader.txt EXT_vertex_shader> extension supported?
glGetEXTVertexShader :: MonadIO m => m Bool
glGetEXTVertexShader = getExtensions >>= (return . member "GL_EXT_vertex_shader")

-- | Is the <https://www.opengl.org/registry/specs/EXT/vertex_shader.txt EXT_vertex_shader> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTVertexShader' in those cases instead.
gl_EXT_vertex_shader :: Bool
gl_EXT_vertex_shader = member "GL_EXT_vertex_shader" extensions
{-# NOINLINE gl_EXT_vertex_shader #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/vertex_weighting.txt EXT_vertex_weighting> extension supported?
glGetEXTVertexWeighting :: MonadIO m => m Bool
glGetEXTVertexWeighting = getExtensions >>= (return . member "GL_EXT_vertex_weighting")

-- | Is the <https://www.opengl.org/registry/specs/EXT/vertex_weighting.txt EXT_vertex_weighting> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTVertexWeighting' in those cases instead.
gl_EXT_vertex_weighting :: Bool
gl_EXT_vertex_weighting = member "GL_EXT_vertex_weighting" extensions
{-# NOINLINE gl_EXT_vertex_weighting #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/win32_keyed_mutex.txt EXT_win32_keyed_mutex> extension supported?
glGetEXTWin32KeyedMutex :: MonadIO m => m Bool
glGetEXTWin32KeyedMutex = getExtensions >>= (return . member "GL_EXT_win32_keyed_mutex")

-- | Is the <https://www.opengl.org/registry/specs/EXT/win32_keyed_mutex.txt EXT_win32_keyed_mutex> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTWin32KeyedMutex' in those cases instead.
gl_EXT_win32_keyed_mutex :: Bool
gl_EXT_win32_keyed_mutex = member "GL_EXT_win32_keyed_mutex" extensions
{-# NOINLINE gl_EXT_win32_keyed_mutex #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/window_rectangles.txt EXT_window_rectangles> extension supported?
glGetEXTWindowRectangles :: MonadIO m => m Bool
glGetEXTWindowRectangles = getExtensions >>= (return . member "GL_EXT_window_rectangles")

-- | Is the <https://www.opengl.org/registry/specs/EXT/window_rectangles.txt EXT_window_rectangles> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTWindowRectangles' in those cases instead.
gl_EXT_window_rectangles :: Bool
gl_EXT_window_rectangles = member "GL_EXT_window_rectangles" extensions
{-# NOINLINE gl_EXT_window_rectangles #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/x11_sync_object.txt EXT_x11_sync_object> extension supported?
glGetEXTX11SyncObject :: MonadIO m => m Bool
glGetEXTX11SyncObject = getExtensions >>= (return . member "GL_EXT_x11_sync_object")

-- | Is the <https://www.opengl.org/registry/specs/EXT/x11_sync_object.txt EXT_x11_sync_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetEXTX11SyncObject' in those cases instead.
gl_EXT_x11_sync_object :: Bool
gl_EXT_x11_sync_object = member "GL_EXT_x11_sync_object" extensions
{-# NOINLINE gl_EXT_x11_sync_object #-}

-- | Is the <https://www.opengl.org/registry/specs/GREMEDY/frame_terminator.txt GREMEDY_frame_terminator> extension supported?
glGetGREMEDYFrameTerminator :: MonadIO m => m Bool
glGetGREMEDYFrameTerminator = getExtensions >>= (return . member "GL_GREMEDY_frame_terminator")

-- | Is the <https://www.opengl.org/registry/specs/GREMEDY/frame_terminator.txt GREMEDY_frame_terminator> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetGREMEDYFrameTerminator' in those cases instead.
gl_GREMEDY_frame_terminator :: Bool
gl_GREMEDY_frame_terminator = member "GL_GREMEDY_frame_terminator" extensions
{-# NOINLINE gl_GREMEDY_frame_terminator #-}

-- | Is the <https://www.opengl.org/registry/specs/GREMEDY/string_marker.txt GREMEDY_string_marker> extension supported?
glGetGREMEDYStringMarker :: MonadIO m => m Bool
glGetGREMEDYStringMarker = getExtensions >>= (return . member "GL_GREMEDY_string_marker")

-- | Is the <https://www.opengl.org/registry/specs/GREMEDY/string_marker.txt GREMEDY_string_marker> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetGREMEDYStringMarker' in those cases instead.
gl_GREMEDY_string_marker :: Bool
gl_GREMEDY_string_marker = member "GL_GREMEDY_string_marker" extensions
{-# NOINLINE gl_GREMEDY_string_marker #-}

-- | Is the <https://www.opengl.org/registry/specs/HP/convolution_border_modes.txt HP_convolution_border_modes> extension supported?
glGetHPConvolutionBorderModes :: MonadIO m => m Bool
glGetHPConvolutionBorderModes = getExtensions >>= (return . member "GL_HP_convolution_border_modes")

-- | Is the <https://www.opengl.org/registry/specs/HP/convolution_border_modes.txt HP_convolution_border_modes> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetHPConvolutionBorderModes' in those cases instead.
gl_HP_convolution_border_modes :: Bool
gl_HP_convolution_border_modes = member "GL_HP_convolution_border_modes" extensions
{-# NOINLINE gl_HP_convolution_border_modes #-}

-- | Is the <https://www.opengl.org/registry/specs/HP/image_transform.txt HP_image_transform> extension supported?
glGetHPImageTransform :: MonadIO m => m Bool
glGetHPImageTransform = getExtensions >>= (return . member "GL_HP_image_transform")

-- | Is the <https://www.opengl.org/registry/specs/HP/image_transform.txt HP_image_transform> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetHPImageTransform' in those cases instead.
gl_HP_image_transform :: Bool
gl_HP_image_transform = member "GL_HP_image_transform" extensions
{-# NOINLINE gl_HP_image_transform #-}

-- | Is the <https://www.opengl.org/registry/specs/HP/occlusion_test.txt HP_occlusion_test> extension supported?
glGetHPOcclusionTest :: MonadIO m => m Bool
glGetHPOcclusionTest = getExtensions >>= (return . member "GL_HP_occlusion_test")

-- | Is the <https://www.opengl.org/registry/specs/HP/occlusion_test.txt HP_occlusion_test> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetHPOcclusionTest' in those cases instead.
gl_HP_occlusion_test :: Bool
gl_HP_occlusion_test = member "GL_HP_occlusion_test" extensions
{-# NOINLINE gl_HP_occlusion_test #-}

-- | Is the <https://www.opengl.org/registry/specs/HP/texture_lighting.txt HP_texture_lighting> extension supported?
glGetHPTextureLighting :: MonadIO m => m Bool
glGetHPTextureLighting = getExtensions >>= (return . member "GL_HP_texture_lighting")

-- | Is the <https://www.opengl.org/registry/specs/HP/texture_lighting.txt HP_texture_lighting> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetHPTextureLighting' in those cases instead.
gl_HP_texture_lighting :: Bool
gl_HP_texture_lighting = member "GL_HP_texture_lighting" extensions
{-# NOINLINE gl_HP_texture_lighting #-}

-- | Is the <https://www.opengl.org/registry/specs/IBM/cull_vertex.txt IBM_cull_vertex> extension supported?
glGetIBMCullVertex :: MonadIO m => m Bool
glGetIBMCullVertex = getExtensions >>= (return . member "GL_IBM_cull_vertex")

-- | Is the <https://www.opengl.org/registry/specs/IBM/cull_vertex.txt IBM_cull_vertex> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetIBMCullVertex' in those cases instead.
gl_IBM_cull_vertex :: Bool
gl_IBM_cull_vertex = member "GL_IBM_cull_vertex" extensions
{-# NOINLINE gl_IBM_cull_vertex #-}

-- | Is the <https://www.opengl.org/registry/specs/IBM/multimode_draw_arrays.txt IBM_multimode_draw_arrays> extension supported?
glGetIBMMultimodeDrawArrays :: MonadIO m => m Bool
glGetIBMMultimodeDrawArrays = getExtensions >>= (return . member "GL_IBM_multimode_draw_arrays")

-- | Is the <https://www.opengl.org/registry/specs/IBM/multimode_draw_arrays.txt IBM_multimode_draw_arrays> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetIBMMultimodeDrawArrays' in those cases instead.
gl_IBM_multimode_draw_arrays :: Bool
gl_IBM_multimode_draw_arrays = member "GL_IBM_multimode_draw_arrays" extensions
{-# NOINLINE gl_IBM_multimode_draw_arrays #-}

-- | Is the <https://www.opengl.org/registry/specs/IBM/rasterpos_clip.txt IBM_rasterpos_clip> extension supported?
glGetIBMRasterposClip :: MonadIO m => m Bool
glGetIBMRasterposClip = getExtensions >>= (return . member "GL_IBM_rasterpos_clip")

-- | Is the <https://www.opengl.org/registry/specs/IBM/rasterpos_clip.txt IBM_rasterpos_clip> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetIBMRasterposClip' in those cases instead.
gl_IBM_rasterpos_clip :: Bool
gl_IBM_rasterpos_clip = member "GL_IBM_rasterpos_clip" extensions
{-# NOINLINE gl_IBM_rasterpos_clip #-}

-- | Is the <https://www.opengl.org/registry/specs/IBM/static_data.txt IBM_static_data> extension supported?
glGetIBMStaticData :: MonadIO m => m Bool
glGetIBMStaticData = getExtensions >>= (return . member "GL_IBM_static_data")

-- | Is the <https://www.opengl.org/registry/specs/IBM/static_data.txt IBM_static_data> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetIBMStaticData' in those cases instead.
gl_IBM_static_data :: Bool
gl_IBM_static_data = member "GL_IBM_static_data" extensions
{-# NOINLINE gl_IBM_static_data #-}

-- | Is the <https://www.opengl.org/registry/specs/IBM/texture_mirrored_repeat.txt IBM_texture_mirrored_repeat> extension supported?
glGetIBMTextureMirroredRepeat :: MonadIO m => m Bool
glGetIBMTextureMirroredRepeat = getExtensions >>= (return . member "GL_IBM_texture_mirrored_repeat")

-- | Is the <https://www.opengl.org/registry/specs/IBM/texture_mirrored_repeat.txt IBM_texture_mirrored_repeat> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetIBMTextureMirroredRepeat' in those cases instead.
gl_IBM_texture_mirrored_repeat :: Bool
gl_IBM_texture_mirrored_repeat = member "GL_IBM_texture_mirrored_repeat" extensions
{-# NOINLINE gl_IBM_texture_mirrored_repeat #-}

-- | Is the <https://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt IBM_vertex_array_lists> extension supported?
glGetIBMVertexArrayLists :: MonadIO m => m Bool
glGetIBMVertexArrayLists = getExtensions >>= (return . member "GL_IBM_vertex_array_lists")

-- | Is the <https://www.opengl.org/registry/specs/IBM/vertex_array_lists.txt IBM_vertex_array_lists> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetIBMVertexArrayLists' in those cases instead.
gl_IBM_vertex_array_lists :: Bool
gl_IBM_vertex_array_lists = member "GL_IBM_vertex_array_lists" extensions
{-# NOINLINE gl_IBM_vertex_array_lists #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_func_separate.txt INGR_blend_func_separate> extension supported?
glGetINGRBlendFuncSeparate :: MonadIO m => m Bool
glGetINGRBlendFuncSeparate = getExtensions >>= (return . member "GL_INGR_blend_func_separate")

-- | Is the <https://www.opengl.org/registry/specs/EXT/blend_func_separate.txt INGR_blend_func_separate> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetINGRBlendFuncSeparate' in those cases instead.
gl_INGR_blend_func_separate :: Bool
gl_INGR_blend_func_separate = member "GL_INGR_blend_func_separate" extensions
{-# NOINLINE gl_INGR_blend_func_separate #-}

-- | Is the <https://www.opengl.org/registry/specs/INGR/color_clamp.txt INGR_color_clamp> extension supported?
glGetINGRColorClamp :: MonadIO m => m Bool
glGetINGRColorClamp = getExtensions >>= (return . member "GL_INGR_color_clamp")

-- | Is the <https://www.opengl.org/registry/specs/INGR/color_clamp.txt INGR_color_clamp> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetINGRColorClamp' in those cases instead.
gl_INGR_color_clamp :: Bool
gl_INGR_color_clamp = member "GL_INGR_color_clamp" extensions
{-# NOINLINE gl_INGR_color_clamp #-}

-- | Is the <https://www.opengl.org/registry/specs/INGR/interlace_read.txt INGR_interlace_read> extension supported?
glGetINGRInterlaceRead :: MonadIO m => m Bool
glGetINGRInterlaceRead = getExtensions >>= (return . member "GL_INGR_interlace_read")

-- | Is the <https://www.opengl.org/registry/specs/INGR/interlace_read.txt INGR_interlace_read> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetINGRInterlaceRead' in those cases instead.
gl_INGR_interlace_read :: Bool
gl_INGR_interlace_read = member "GL_INGR_interlace_read" extensions
{-# NOINLINE gl_INGR_interlace_read #-}

-- | Is the <https://www.opengl.org/registry/specs/INTEL/blackhole_render.txt INTEL_blackhole_render> extension supported?
glGetINTELBlackholeRender :: MonadIO m => m Bool
glGetINTELBlackholeRender = getExtensions >>= (return . member "GL_INTEL_blackhole_render")

-- | Is the <https://www.opengl.org/registry/specs/INTEL/blackhole_render.txt INTEL_blackhole_render> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetINTELBlackholeRender' in those cases instead.
gl_INTEL_blackhole_render :: Bool
gl_INTEL_blackhole_render = member "GL_INTEL_blackhole_render" extensions
{-# NOINLINE gl_INTEL_blackhole_render #-}

-- | Is the <https://www.opengl.org/registry/specs/INTEL/conservative_rasterization.txt INTEL_conservative_rasterization> extension supported?
glGetINTELConservativeRasterization :: MonadIO m => m Bool
glGetINTELConservativeRasterization = getExtensions >>= (return . member "GL_INTEL_conservative_rasterization")

-- | Is the <https://www.opengl.org/registry/specs/INTEL/conservative_rasterization.txt INTEL_conservative_rasterization> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetINTELConservativeRasterization' in those cases instead.
gl_INTEL_conservative_rasterization :: Bool
gl_INTEL_conservative_rasterization = member "GL_INTEL_conservative_rasterization" extensions
{-# NOINLINE gl_INTEL_conservative_rasterization #-}

-- | Is the <https://www.opengl.org/registry/specs/INTEL/framebuffer_CMAA.txt INTEL_framebuffer_CMAA> extension supported?
glGetINTELFramebufferCmaa :: MonadIO m => m Bool
glGetINTELFramebufferCmaa = getExtensions >>= (return . member "GL_INTEL_framebuffer_CMAA")

-- | Is the <https://www.opengl.org/registry/specs/INTEL/framebuffer_CMAA.txt INTEL_framebuffer_CMAA> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetINTELFramebufferCmaa' in those cases instead.
gl_INTEL_framebuffer_CMAA :: Bool
gl_INTEL_framebuffer_CMAA = member "GL_INTEL_framebuffer_CMAA" extensions
{-# NOINLINE gl_INTEL_framebuffer_CMAA #-}

-- | Is the <https://www.opengl.org/registry/specs/INTEL/map_texture.txt INTEL_map_texture> extension supported?
glGetINTELMapTexture :: MonadIO m => m Bool
glGetINTELMapTexture = getExtensions >>= (return . member "GL_INTEL_map_texture")

-- | Is the <https://www.opengl.org/registry/specs/INTEL/map_texture.txt INTEL_map_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetINTELMapTexture' in those cases instead.
gl_INTEL_map_texture :: Bool
gl_INTEL_map_texture = member "GL_INTEL_map_texture" extensions
{-# NOINLINE gl_INTEL_map_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/INTEL/parallel_arrays.txt INTEL_parallel_arrays> extension supported?
glGetINTELParallelArrays :: MonadIO m => m Bool
glGetINTELParallelArrays = getExtensions >>= (return . member "GL_INTEL_parallel_arrays")

-- | Is the <https://www.opengl.org/registry/specs/INTEL/parallel_arrays.txt INTEL_parallel_arrays> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetINTELParallelArrays' in those cases instead.
gl_INTEL_parallel_arrays :: Bool
gl_INTEL_parallel_arrays = member "GL_INTEL_parallel_arrays" extensions
{-# NOINLINE gl_INTEL_parallel_arrays #-}

-- | Is the <https://www.opengl.org/registry/specs/INTEL/performance_query.txt INTEL_performance_query> extension supported?
glGetINTELPerformanceQuery :: MonadIO m => m Bool
glGetINTELPerformanceQuery = getExtensions >>= (return . member "GL_INTEL_performance_query")

-- | Is the <https://www.opengl.org/registry/specs/INTEL/performance_query.txt INTEL_performance_query> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetINTELPerformanceQuery' in those cases instead.
gl_INTEL_performance_query :: Bool
gl_INTEL_performance_query = member "GL_INTEL_performance_query" extensions
{-# NOINLINE gl_INTEL_performance_query #-}

-- | Is the <https://www.opengl.org/registry/specs/KHR/blend_equation_advanced.txt KHR_blend_equation_advanced> extension supported?
glGetKHRBlendEquationAdvanced :: MonadIO m => m Bool
glGetKHRBlendEquationAdvanced = getExtensions >>= (return . member "GL_KHR_blend_equation_advanced")

-- | Is the <https://www.opengl.org/registry/specs/KHR/blend_equation_advanced.txt KHR_blend_equation_advanced> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetKHRBlendEquationAdvanced' in those cases instead.
gl_KHR_blend_equation_advanced :: Bool
gl_KHR_blend_equation_advanced = member "GL_KHR_blend_equation_advanced" extensions
{-# NOINLINE gl_KHR_blend_equation_advanced #-}

-- | Is the <https://www.opengl.org/registry/specs/KHR/blend_equation_advanced.txt KHR_blend_equation_advanced_coherent> extension supported?
glGetKHRBlendEquationAdvancedCoherent :: MonadIO m => m Bool
glGetKHRBlendEquationAdvancedCoherent = getExtensions >>= (return . member "GL_KHR_blend_equation_advanced_coherent")

-- | Is the <https://www.opengl.org/registry/specs/KHR/blend_equation_advanced.txt KHR_blend_equation_advanced_coherent> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetKHRBlendEquationAdvancedCoherent' in those cases instead.
gl_KHR_blend_equation_advanced_coherent :: Bool
gl_KHR_blend_equation_advanced_coherent = member "GL_KHR_blend_equation_advanced_coherent" extensions
{-# NOINLINE gl_KHR_blend_equation_advanced_coherent #-}

-- | Is the <https://www.opengl.org/registry/specs/KHR/context_flush_control.txt KHR_context_flush_control> extension supported?
glGetKHRContextFlushControl :: MonadIO m => m Bool
glGetKHRContextFlushControl = getExtensions >>= (return . member "GL_KHR_context_flush_control")

-- | Is the <https://www.opengl.org/registry/specs/KHR/context_flush_control.txt KHR_context_flush_control> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetKHRContextFlushControl' in those cases instead.
gl_KHR_context_flush_control :: Bool
gl_KHR_context_flush_control = member "GL_KHR_context_flush_control" extensions
{-# NOINLINE gl_KHR_context_flush_control #-}

-- | Is the <https://www.opengl.org/registry/specs/KHR/debug.txt KHR_debug> extension supported?
glGetKHRDebug :: MonadIO m => m Bool
glGetKHRDebug = getExtensions >>= (return . member "GL_KHR_debug")

-- | Is the <https://www.opengl.org/registry/specs/KHR/debug.txt KHR_debug> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetKHRDebug' in those cases instead.
gl_KHR_debug :: Bool
gl_KHR_debug = member "GL_KHR_debug" extensions
{-# NOINLINE gl_KHR_debug #-}

-- | Is the <https://www.opengl.org/registry/specs/KHR/no_error.txt KHR_no_error> extension supported?
glGetKHRNoError :: MonadIO m => m Bool
glGetKHRNoError = getExtensions >>= (return . member "GL_KHR_no_error")

-- | Is the <https://www.opengl.org/registry/specs/KHR/no_error.txt KHR_no_error> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetKHRNoError' in those cases instead.
gl_KHR_no_error :: Bool
gl_KHR_no_error = member "GL_KHR_no_error" extensions
{-# NOINLINE gl_KHR_no_error #-}

-- | Is the <https://www.opengl.org/registry/specs/KHR/parallel_shader_compile.txt KHR_parallel_shader_compile> extension supported?
glGetKHRParallelShaderCompile :: MonadIO m => m Bool
glGetKHRParallelShaderCompile = getExtensions >>= (return . member "GL_KHR_parallel_shader_compile")

-- | Is the <https://www.opengl.org/registry/specs/KHR/parallel_shader_compile.txt KHR_parallel_shader_compile> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetKHRParallelShaderCompile' in those cases instead.
gl_KHR_parallel_shader_compile :: Bool
gl_KHR_parallel_shader_compile = member "GL_KHR_parallel_shader_compile" extensions
{-# NOINLINE gl_KHR_parallel_shader_compile #-}

-- | Is the <https://www.opengl.org/registry/specs/KHR/robustness.txt KHR_robustness> extension supported?
glGetKHRRobustness :: MonadIO m => m Bool
glGetKHRRobustness = getExtensions >>= (return . member "GL_KHR_robustness")

-- | Is the <https://www.opengl.org/registry/specs/KHR/robustness.txt KHR_robustness> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetKHRRobustness' in those cases instead.
gl_KHR_robustness :: Bool
gl_KHR_robustness = member "GL_KHR_robustness" extensions
{-# NOINLINE gl_KHR_robustness #-}

-- | Is the <https://www.opengl.org/registry/specs/KHR/shader_subgroup.txt KHR_shader_subgroup> extension supported?
glGetKHRShaderSubgroup :: MonadIO m => m Bool
glGetKHRShaderSubgroup = getExtensions >>= (return . member "GL_KHR_shader_subgroup")

-- | Is the <https://www.opengl.org/registry/specs/KHR/shader_subgroup.txt KHR_shader_subgroup> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetKHRShaderSubgroup' in those cases instead.
gl_KHR_shader_subgroup :: Bool
gl_KHR_shader_subgroup = member "GL_KHR_shader_subgroup" extensions
{-# NOINLINE gl_KHR_shader_subgroup #-}

-- | Is the <https://www.opengl.org/registry/specs/KHR/texture_compression_astc_hdr.txt KHR_texture_compression_astc_hdr> extension supported?
glGetKHRTextureCompressionASTCHDR :: MonadIO m => m Bool
glGetKHRTextureCompressionASTCHDR = getExtensions >>= (return . member "GL_KHR_texture_compression_astc_hdr")

-- | Is the <https://www.opengl.org/registry/specs/KHR/texture_compression_astc_hdr.txt KHR_texture_compression_astc_hdr> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetKHRTextureCompressionASTCHDR' in those cases instead.
gl_KHR_texture_compression_astc_hdr :: Bool
gl_KHR_texture_compression_astc_hdr = member "GL_KHR_texture_compression_astc_hdr" extensions
{-# NOINLINE gl_KHR_texture_compression_astc_hdr #-}

-- | Is the <https://www.opengl.org/registry/specs/KHR/texture_compression_astc_hdr.txt KHR_texture_compression_astc_ldr> extension supported?
glGetKHRTextureCompressionASTCLDR :: MonadIO m => m Bool
glGetKHRTextureCompressionASTCLDR = getExtensions >>= (return . member "GL_KHR_texture_compression_astc_ldr")

-- | Is the <https://www.opengl.org/registry/specs/KHR/texture_compression_astc_hdr.txt KHR_texture_compression_astc_ldr> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetKHRTextureCompressionASTCLDR' in those cases instead.
gl_KHR_texture_compression_astc_ldr :: Bool
gl_KHR_texture_compression_astc_ldr = member "GL_KHR_texture_compression_astc_ldr" extensions
{-# NOINLINE gl_KHR_texture_compression_astc_ldr #-}

-- | Is the <https://www.opengl.org/registry/specs/MESA/framebuffer_flip_y.txt MESA_framebuffer_flip_y> extension supported?
glGetMESAFramebufferFlipY :: MonadIO m => m Bool
glGetMESAFramebufferFlipY = getExtensions >>= (return . member "GL_MESA_framebuffer_flip_y")

-- | Is the <https://www.opengl.org/registry/specs/MESA/framebuffer_flip_y.txt MESA_framebuffer_flip_y> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetMESAFramebufferFlipY' in those cases instead.
gl_MESA_framebuffer_flip_y :: Bool
gl_MESA_framebuffer_flip_y = member "GL_MESA_framebuffer_flip_y" extensions
{-# NOINLINE gl_MESA_framebuffer_flip_y #-}

-- | Is the <https://www.opengl.org/registry/specs/MESA/pack_invert.txt MESA_pack_invert> extension supported?
glGetMESAPackInvert :: MonadIO m => m Bool
glGetMESAPackInvert = getExtensions >>= (return . member "GL_MESA_pack_invert")

-- | Is the <https://www.opengl.org/registry/specs/MESA/pack_invert.txt MESA_pack_invert> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetMESAPackInvert' in those cases instead.
gl_MESA_pack_invert :: Bool
gl_MESA_pack_invert = member "GL_MESA_pack_invert" extensions
{-# NOINLINE gl_MESA_pack_invert #-}

-- | Is the <https://www.opengl.org/registry/specs/MESA/program_binary_formats.txt MESA_program_binary_formats> extension supported?
glGetMESAProgramBinaryFormats :: MonadIO m => m Bool
glGetMESAProgramBinaryFormats = getExtensions >>= (return . member "GL_MESA_program_binary_formats")

-- | Is the <https://www.opengl.org/registry/specs/MESA/program_binary_formats.txt MESA_program_binary_formats> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetMESAProgramBinaryFormats' in those cases instead.
gl_MESA_program_binary_formats :: Bool
gl_MESA_program_binary_formats = member "GL_MESA_program_binary_formats" extensions
{-# NOINLINE gl_MESA_program_binary_formats #-}

-- | Is the <https://www.opengl.org/registry/specs/MESA/resize_buffers.txt MESA_resize_buffers> extension supported?
glGetMESAResizeBuffers :: MonadIO m => m Bool
glGetMESAResizeBuffers = getExtensions >>= (return . member "GL_MESA_resize_buffers")

-- | Is the <https://www.opengl.org/registry/specs/MESA/resize_buffers.txt MESA_resize_buffers> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetMESAResizeBuffers' in those cases instead.
gl_MESA_resize_buffers :: Bool
gl_MESA_resize_buffers = member "GL_MESA_resize_buffers" extensions
{-# NOINLINE gl_MESA_resize_buffers #-}

-- | Is the <https://www.opengl.org/registry/specs/MESA/tile_raster_order.txt MESA_tile_raster_order> extension supported?
glGetMESATileRasterOrder :: MonadIO m => m Bool
glGetMESATileRasterOrder = getExtensions >>= (return . member "GL_MESA_tile_raster_order")

-- | Is the <https://www.opengl.org/registry/specs/MESA/tile_raster_order.txt MESA_tile_raster_order> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetMESATileRasterOrder' in those cases instead.
gl_MESA_tile_raster_order :: Bool
gl_MESA_tile_raster_order = member "GL_MESA_tile_raster_order" extensions
{-# NOINLINE gl_MESA_tile_raster_order #-}

-- | Is the <https://www.opengl.org/registry/specs/MESA/window_pos.txt MESA_window_pos> extension supported?
glGetMESAWindowPos :: MonadIO m => m Bool
glGetMESAWindowPos = getExtensions >>= (return . member "GL_MESA_window_pos")

-- | Is the <https://www.opengl.org/registry/specs/MESA/window_pos.txt MESA_window_pos> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetMESAWindowPos' in those cases instead.
gl_MESA_window_pos :: Bool
gl_MESA_window_pos = member "GL_MESA_window_pos" extensions
{-# NOINLINE gl_MESA_window_pos #-}

-- | Is the <https://www.opengl.org/registry/specs/MESA/ycbcr_texture.txt MESA_ycbcr_texture> extension supported?
glGetMESAYCbCrTexture :: MonadIO m => m Bool
glGetMESAYCbCrTexture = getExtensions >>= (return . member "GL_MESA_ycbcr_texture")

-- | Is the <https://www.opengl.org/registry/specs/MESA/ycbcr_texture.txt MESA_ycbcr_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetMESAYCbCrTexture' in those cases instead.
gl_MESA_ycbcr_texture :: Bool
gl_MESA_ycbcr_texture = member "GL_MESA_ycbcr_texture" extensions
{-# NOINLINE gl_MESA_ycbcr_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/MESAX/texture_stack.txt MESAX_texture_stack> extension supported?
glGetMESAXTextureStack :: MonadIO m => m Bool
glGetMESAXTextureStack = getExtensions >>= (return . member "GL_MESAX_texture_stack")

-- | Is the <https://www.opengl.org/registry/specs/MESAX/texture_stack.txt MESAX_texture_stack> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetMESAXTextureStack' in those cases instead.
gl_MESAX_texture_stack :: Bool
gl_MESAX_texture_stack = member "GL_MESAX_texture_stack" extensions
{-# NOINLINE gl_MESAX_texture_stack #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/alpha_to_coverage_dither_control.txt NV_alpha_to_coverage_dither_control> extension supported?
glGetNVAlphaToCoverageDitherControl :: MonadIO m => m Bool
glGetNVAlphaToCoverageDitherControl = getExtensions >>= (return . member "GL_NV_alpha_to_coverage_dither_control")

-- | Is the <https://www.opengl.org/registry/specs/NV/alpha_to_coverage_dither_control.txt NV_alpha_to_coverage_dither_control> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVAlphaToCoverageDitherControl' in those cases instead.
gl_NV_alpha_to_coverage_dither_control :: Bool
gl_NV_alpha_to_coverage_dither_control = member "GL_NV_alpha_to_coverage_dither_control" extensions
{-# NOINLINE gl_NV_alpha_to_coverage_dither_control #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/bindless_multi_draw_indirect.txt NV_bindless_multi_draw_indirect> extension supported?
glGetNVBindlessMultiDrawIndirect :: MonadIO m => m Bool
glGetNVBindlessMultiDrawIndirect = getExtensions >>= (return . member "GL_NV_bindless_multi_draw_indirect")

-- | Is the <https://www.opengl.org/registry/specs/NV/bindless_multi_draw_indirect.txt NV_bindless_multi_draw_indirect> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVBindlessMultiDrawIndirect' in those cases instead.
gl_NV_bindless_multi_draw_indirect :: Bool
gl_NV_bindless_multi_draw_indirect = member "GL_NV_bindless_multi_draw_indirect" extensions
{-# NOINLINE gl_NV_bindless_multi_draw_indirect #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/bindless_multi_draw_indirect_count.txt NV_bindless_multi_draw_indirect_count> extension supported?
glGetNVBindlessMultiDrawIndirectCount :: MonadIO m => m Bool
glGetNVBindlessMultiDrawIndirectCount = getExtensions >>= (return . member "GL_NV_bindless_multi_draw_indirect_count")

-- | Is the <https://www.opengl.org/registry/specs/NV/bindless_multi_draw_indirect_count.txt NV_bindless_multi_draw_indirect_count> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVBindlessMultiDrawIndirectCount' in those cases instead.
gl_NV_bindless_multi_draw_indirect_count :: Bool
gl_NV_bindless_multi_draw_indirect_count = member "GL_NV_bindless_multi_draw_indirect_count" extensions
{-# NOINLINE gl_NV_bindless_multi_draw_indirect_count #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/bindless_texture.txt NV_bindless_texture> extension supported?
glGetNVBindlessTexture :: MonadIO m => m Bool
glGetNVBindlessTexture = getExtensions >>= (return . member "GL_NV_bindless_texture")

-- | Is the <https://www.opengl.org/registry/specs/NV/bindless_texture.txt NV_bindless_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVBindlessTexture' in those cases instead.
gl_NV_bindless_texture :: Bool
gl_NV_bindless_texture = member "GL_NV_bindless_texture" extensions
{-# NOINLINE gl_NV_bindless_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/blend_equation_advanced.txt NV_blend_equation_advanced> extension supported?
glGetNVBlendEquationAdvanced :: MonadIO m => m Bool
glGetNVBlendEquationAdvanced = getExtensions >>= (return . member "GL_NV_blend_equation_advanced")

-- | Is the <https://www.opengl.org/registry/specs/NV/blend_equation_advanced.txt NV_blend_equation_advanced> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVBlendEquationAdvanced' in those cases instead.
gl_NV_blend_equation_advanced :: Bool
gl_NV_blend_equation_advanced = member "GL_NV_blend_equation_advanced" extensions
{-# NOINLINE gl_NV_blend_equation_advanced #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/blend_equation_advanced.txt NV_blend_equation_advanced_coherent> extension supported?
glGetNVBlendEquationAdvancedCoherent :: MonadIO m => m Bool
glGetNVBlendEquationAdvancedCoherent = getExtensions >>= (return . member "GL_NV_blend_equation_advanced_coherent")

-- | Is the <https://www.opengl.org/registry/specs/NV/blend_equation_advanced.txt NV_blend_equation_advanced_coherent> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVBlendEquationAdvancedCoherent' in those cases instead.
gl_NV_blend_equation_advanced_coherent :: Bool
gl_NV_blend_equation_advanced_coherent = member "GL_NV_blend_equation_advanced_coherent" extensions
{-# NOINLINE gl_NV_blend_equation_advanced_coherent #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/blend_minmax_factor.txt NV_blend_minmax_factor> extension supported?
glGetNVBlendMinmaxFactor :: MonadIO m => m Bool
glGetNVBlendMinmaxFactor = getExtensions >>= (return . member "GL_NV_blend_minmax_factor")

-- | Is the <https://www.opengl.org/registry/specs/NV/blend_minmax_factor.txt NV_blend_minmax_factor> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVBlendMinmaxFactor' in those cases instead.
gl_NV_blend_minmax_factor :: Bool
gl_NV_blend_minmax_factor = member "GL_NV_blend_minmax_factor" extensions
{-# NOINLINE gl_NV_blend_minmax_factor #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/clip_space_w_scaling.txt NV_clip_space_w_scaling> extension supported?
glGetNVClipSpaceWScaling :: MonadIO m => m Bool
glGetNVClipSpaceWScaling = getExtensions >>= (return . member "GL_NV_clip_space_w_scaling")

-- | Is the <https://www.opengl.org/registry/specs/NV/clip_space_w_scaling.txt NV_clip_space_w_scaling> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVClipSpaceWScaling' in those cases instead.
gl_NV_clip_space_w_scaling :: Bool
gl_NV_clip_space_w_scaling = member "GL_NV_clip_space_w_scaling" extensions
{-# NOINLINE gl_NV_clip_space_w_scaling #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/command_list.txt NV_command_list> extension supported?
glGetNVCommandList :: MonadIO m => m Bool
glGetNVCommandList = getExtensions >>= (return . member "GL_NV_command_list")

-- | Is the <https://www.opengl.org/registry/specs/NV/command_list.txt NV_command_list> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVCommandList' in those cases instead.
gl_NV_command_list :: Bool
gl_NV_command_list = member "GL_NV_command_list" extensions
{-# NOINLINE gl_NV_command_list #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/compute_program5.txt NV_compute_program5> extension supported?
glGetNVComputeProgram5 :: MonadIO m => m Bool
glGetNVComputeProgram5 = getExtensions >>= (return . member "GL_NV_compute_program5")

-- | Is the <https://www.opengl.org/registry/specs/NV/compute_program5.txt NV_compute_program5> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVComputeProgram5' in those cases instead.
gl_NV_compute_program5 :: Bool
gl_NV_compute_program5 = member "GL_NV_compute_program5" extensions
{-# NOINLINE gl_NV_compute_program5 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/conditional_render.txt NV_conditional_render> extension supported?
glGetNVConditionalRender :: MonadIO m => m Bool
glGetNVConditionalRender = getExtensions >>= (return . member "GL_NV_conditional_render")

-- | Is the <https://www.opengl.org/registry/specs/NV/conditional_render.txt NV_conditional_render> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVConditionalRender' in those cases instead.
gl_NV_conditional_render :: Bool
gl_NV_conditional_render = member "GL_NV_conditional_render" extensions
{-# NOINLINE gl_NV_conditional_render #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/conservative_raster.txt NV_conservative_raster> extension supported?
glGetNVConservativeRaster :: MonadIO m => m Bool
glGetNVConservativeRaster = getExtensions >>= (return . member "GL_NV_conservative_raster")

-- | Is the <https://www.opengl.org/registry/specs/NV/conservative_raster.txt NV_conservative_raster> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVConservativeRaster' in those cases instead.
gl_NV_conservative_raster :: Bool
gl_NV_conservative_raster = member "GL_NV_conservative_raster" extensions
{-# NOINLINE gl_NV_conservative_raster #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/conservative_raster_dilate.txt NV_conservative_raster_dilate> extension supported?
glGetNVConservativeRasterDilate :: MonadIO m => m Bool
glGetNVConservativeRasterDilate = getExtensions >>= (return . member "GL_NV_conservative_raster_dilate")

-- | Is the <https://www.opengl.org/registry/specs/NV/conservative_raster_dilate.txt NV_conservative_raster_dilate> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVConservativeRasterDilate' in those cases instead.
gl_NV_conservative_raster_dilate :: Bool
gl_NV_conservative_raster_dilate = member "GL_NV_conservative_raster_dilate" extensions
{-# NOINLINE gl_NV_conservative_raster_dilate #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/conservative_raster_pre_snap.txt NV_conservative_raster_pre_snap> extension supported?
glGetNVConservativeRasterPreSnap :: MonadIO m => m Bool
glGetNVConservativeRasterPreSnap = getExtensions >>= (return . member "GL_NV_conservative_raster_pre_snap")

-- | Is the <https://www.opengl.org/registry/specs/NV/conservative_raster_pre_snap.txt NV_conservative_raster_pre_snap> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVConservativeRasterPreSnap' in those cases instead.
gl_NV_conservative_raster_pre_snap :: Bool
gl_NV_conservative_raster_pre_snap = member "GL_NV_conservative_raster_pre_snap" extensions
{-# NOINLINE gl_NV_conservative_raster_pre_snap #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/conservative_raster_pre_snap_triangles.txt NV_conservative_raster_pre_snap_triangles> extension supported?
glGetNVConservativeRasterPreSnapTriangles :: MonadIO m => m Bool
glGetNVConservativeRasterPreSnapTriangles = getExtensions >>= (return . member "GL_NV_conservative_raster_pre_snap_triangles")

-- | Is the <https://www.opengl.org/registry/specs/NV/conservative_raster_pre_snap_triangles.txt NV_conservative_raster_pre_snap_triangles> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVConservativeRasterPreSnapTriangles' in those cases instead.
gl_NV_conservative_raster_pre_snap_triangles :: Bool
gl_NV_conservative_raster_pre_snap_triangles = member "GL_NV_conservative_raster_pre_snap_triangles" extensions
{-# NOINLINE gl_NV_conservative_raster_pre_snap_triangles #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/copy_depth_to_color.txt NV_copy_depth_to_color> extension supported?
glGetNVCopyDepthToColor :: MonadIO m => m Bool
glGetNVCopyDepthToColor = getExtensions >>= (return . member "GL_NV_copy_depth_to_color")

-- | Is the <https://www.opengl.org/registry/specs/NV/copy_depth_to_color.txt NV_copy_depth_to_color> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVCopyDepthToColor' in those cases instead.
gl_NV_copy_depth_to_color :: Bool
gl_NV_copy_depth_to_color = member "GL_NV_copy_depth_to_color" extensions
{-# NOINLINE gl_NV_copy_depth_to_color #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/copy_image.txt NV_copy_image> extension supported?
glGetNVCopyImage :: MonadIO m => m Bool
glGetNVCopyImage = getExtensions >>= (return . member "GL_NV_copy_image")

-- | Is the <https://www.opengl.org/registry/specs/NV/copy_image.txt NV_copy_image> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVCopyImage' in those cases instead.
gl_NV_copy_image :: Bool
gl_NV_copy_image = member "GL_NV_copy_image" extensions
{-# NOINLINE gl_NV_copy_image #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/deep_texture3D.txt NV_deep_texture3D> extension supported?
glGetNVDeepTexture3D :: MonadIO m => m Bool
glGetNVDeepTexture3D = getExtensions >>= (return . member "GL_NV_deep_texture3D")

-- | Is the <https://www.opengl.org/registry/specs/NV/deep_texture3D.txt NV_deep_texture3D> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVDeepTexture3D' in those cases instead.
gl_NV_deep_texture3D :: Bool
gl_NV_deep_texture3D = member "GL_NV_deep_texture3D" extensions
{-# NOINLINE gl_NV_deep_texture3D #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/depth_buffer_float.txt NV_depth_buffer_float> extension supported?
glGetNVDepthBufferFloat :: MonadIO m => m Bool
glGetNVDepthBufferFloat = getExtensions >>= (return . member "GL_NV_depth_buffer_float")

-- | Is the <https://www.opengl.org/registry/specs/NV/depth_buffer_float.txt NV_depth_buffer_float> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVDepthBufferFloat' in those cases instead.
gl_NV_depth_buffer_float :: Bool
gl_NV_depth_buffer_float = member "GL_NV_depth_buffer_float" extensions
{-# NOINLINE gl_NV_depth_buffer_float #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/depth_clamp.txt NV_depth_clamp> extension supported?
glGetNVDepthClamp :: MonadIO m => m Bool
glGetNVDepthClamp = getExtensions >>= (return . member "GL_NV_depth_clamp")

-- | Is the <https://www.opengl.org/registry/specs/NV/depth_clamp.txt NV_depth_clamp> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVDepthClamp' in those cases instead.
gl_NV_depth_clamp :: Bool
gl_NV_depth_clamp = member "GL_NV_depth_clamp" extensions
{-# NOINLINE gl_NV_depth_clamp #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/draw_texture.txt NV_draw_texture> extension supported?
glGetNVDrawTexture :: MonadIO m => m Bool
glGetNVDrawTexture = getExtensions >>= (return . member "GL_NV_draw_texture")

-- | Is the <https://www.opengl.org/registry/specs/NV/draw_texture.txt NV_draw_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVDrawTexture' in those cases instead.
gl_NV_draw_texture :: Bool
gl_NV_draw_texture = member "GL_NV_draw_texture" extensions
{-# NOINLINE gl_NV_draw_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/draw_vulkan_image.txt NV_draw_vulkan_image> extension supported?
glGetNVDrawVulkanImage :: MonadIO m => m Bool
glGetNVDrawVulkanImage = getExtensions >>= (return . member "GL_NV_draw_vulkan_image")

-- | Is the <https://www.opengl.org/registry/specs/NV/draw_vulkan_image.txt NV_draw_vulkan_image> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVDrawVulkanImage' in those cases instead.
gl_NV_draw_vulkan_image :: Bool
gl_NV_draw_vulkan_image = member "GL_NV_draw_vulkan_image" extensions
{-# NOINLINE gl_NV_draw_vulkan_image #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/evaluators.txt NV_evaluators> extension supported?
glGetNVEvaluators :: MonadIO m => m Bool
glGetNVEvaluators = getExtensions >>= (return . member "GL_NV_evaluators")

-- | Is the <https://www.opengl.org/registry/specs/NV/evaluators.txt NV_evaluators> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVEvaluators' in those cases instead.
gl_NV_evaluators :: Bool
gl_NV_evaluators = member "GL_NV_evaluators" extensions
{-# NOINLINE gl_NV_evaluators #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/explicit_multisample.txt NV_explicit_multisample> extension supported?
glGetNVExplicitMultisample :: MonadIO m => m Bool
glGetNVExplicitMultisample = getExtensions >>= (return . member "GL_NV_explicit_multisample")

-- | Is the <https://www.opengl.org/registry/specs/NV/explicit_multisample.txt NV_explicit_multisample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVExplicitMultisample' in those cases instead.
gl_NV_explicit_multisample :: Bool
gl_NV_explicit_multisample = member "GL_NV_explicit_multisample" extensions
{-# NOINLINE gl_NV_explicit_multisample #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/fence.txt NV_fence> extension supported?
glGetNVFence :: MonadIO m => m Bool
glGetNVFence = getExtensions >>= (return . member "GL_NV_fence")

-- | Is the <https://www.opengl.org/registry/specs/NV/fence.txt NV_fence> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVFence' in those cases instead.
gl_NV_fence :: Bool
gl_NV_fence = member "GL_NV_fence" extensions
{-# NOINLINE gl_NV_fence #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/fill_rectangle.txt NV_fill_rectangle> extension supported?
glGetNVFillRectangle :: MonadIO m => m Bool
glGetNVFillRectangle = getExtensions >>= (return . member "GL_NV_fill_rectangle")

-- | Is the <https://www.opengl.org/registry/specs/NV/fill_rectangle.txt NV_fill_rectangle> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVFillRectangle' in those cases instead.
gl_NV_fill_rectangle :: Bool
gl_NV_fill_rectangle = member "GL_NV_fill_rectangle" extensions
{-# NOINLINE gl_NV_fill_rectangle #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/float_buffer.txt NV_float_buffer> extension supported?
glGetNVFloatBuffer :: MonadIO m => m Bool
glGetNVFloatBuffer = getExtensions >>= (return . member "GL_NV_float_buffer")

-- | Is the <https://www.opengl.org/registry/specs/NV/float_buffer.txt NV_float_buffer> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVFloatBuffer' in those cases instead.
gl_NV_float_buffer :: Bool
gl_NV_float_buffer = member "GL_NV_float_buffer" extensions
{-# NOINLINE gl_NV_float_buffer #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/fog_distance.txt NV_fog_distance> extension supported?
glGetNVFogDistance :: MonadIO m => m Bool
glGetNVFogDistance = getExtensions >>= (return . member "GL_NV_fog_distance")

-- | Is the <https://www.opengl.org/registry/specs/NV/fog_distance.txt NV_fog_distance> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVFogDistance' in those cases instead.
gl_NV_fog_distance :: Bool
gl_NV_fog_distance = member "GL_NV_fog_distance" extensions
{-# NOINLINE gl_NV_fog_distance #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/fragment_coverage_to_color.txt NV_fragment_coverage_to_color> extension supported?
glGetNVFragmentCoverageToColor :: MonadIO m => m Bool
glGetNVFragmentCoverageToColor = getExtensions >>= (return . member "GL_NV_fragment_coverage_to_color")

-- | Is the <https://www.opengl.org/registry/specs/NV/fragment_coverage_to_color.txt NV_fragment_coverage_to_color> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVFragmentCoverageToColor' in those cases instead.
gl_NV_fragment_coverage_to_color :: Bool
gl_NV_fragment_coverage_to_color = member "GL_NV_fragment_coverage_to_color" extensions
{-# NOINLINE gl_NV_fragment_coverage_to_color #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/fragment_program.txt NV_fragment_program> extension supported?
glGetNVFragmentProgram :: MonadIO m => m Bool
glGetNVFragmentProgram = getExtensions >>= (return . member "GL_NV_fragment_program")

-- | Is the <https://www.opengl.org/registry/specs/NV/fragment_program.txt NV_fragment_program> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVFragmentProgram' in those cases instead.
gl_NV_fragment_program :: Bool
gl_NV_fragment_program = member "GL_NV_fragment_program" extensions
{-# NOINLINE gl_NV_fragment_program #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/fragment_program2.txt NV_fragment_program2> extension supported?
glGetNVFragmentProgram2 :: MonadIO m => m Bool
glGetNVFragmentProgram2 = getExtensions >>= (return . member "GL_NV_fragment_program2")

-- | Is the <https://www.opengl.org/registry/specs/NV/fragment_program2.txt NV_fragment_program2> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVFragmentProgram2' in those cases instead.
gl_NV_fragment_program2 :: Bool
gl_NV_fragment_program2 = member "GL_NV_fragment_program2" extensions
{-# NOINLINE gl_NV_fragment_program2 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/framebuffer_mixed_samples.txt NV_framebuffer_mixed_samples> extension supported?
glGetNVFramebufferMixedSamples :: MonadIO m => m Bool
glGetNVFramebufferMixedSamples = getExtensions >>= (return . member "GL_NV_framebuffer_mixed_samples")

-- | Is the <https://www.opengl.org/registry/specs/NV/framebuffer_mixed_samples.txt NV_framebuffer_mixed_samples> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVFramebufferMixedSamples' in those cases instead.
gl_NV_framebuffer_mixed_samples :: Bool
gl_NV_framebuffer_mixed_samples = member "GL_NV_framebuffer_mixed_samples" extensions
{-# NOINLINE gl_NV_framebuffer_mixed_samples #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/framebuffer_multisample_coverage.txt NV_framebuffer_multisample_coverage> extension supported?
glGetNVFramebufferMultisampleCoverage :: MonadIO m => m Bool
glGetNVFramebufferMultisampleCoverage = getExtensions >>= (return . member "GL_NV_framebuffer_multisample_coverage")

-- | Is the <https://www.opengl.org/registry/specs/NV/framebuffer_multisample_coverage.txt NV_framebuffer_multisample_coverage> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVFramebufferMultisampleCoverage' in those cases instead.
gl_NV_framebuffer_multisample_coverage :: Bool
gl_NV_framebuffer_multisample_coverage = member "GL_NV_framebuffer_multisample_coverage" extensions
{-# NOINLINE gl_NV_framebuffer_multisample_coverage #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/geometry_program4.txt NV_geometry_program4> extension supported?
glGetNVGeometryProgram4 :: MonadIO m => m Bool
glGetNVGeometryProgram4 = getExtensions >>= (return . member "GL_NV_geometry_program4")

-- | Is the <https://www.opengl.org/registry/specs/NV/geometry_program4.txt NV_geometry_program4> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVGeometryProgram4' in those cases instead.
gl_NV_geometry_program4 :: Bool
gl_NV_geometry_program4 = member "GL_NV_geometry_program4" extensions
{-# NOINLINE gl_NV_geometry_program4 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/gpu_multicast.txt NV_gpu_multicast> extension supported?
glGetNVGPUMulticast :: MonadIO m => m Bool
glGetNVGPUMulticast = getExtensions >>= (return . member "GL_NV_gpu_multicast")

-- | Is the <https://www.opengl.org/registry/specs/NV/gpu_multicast.txt NV_gpu_multicast> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVGPUMulticast' in those cases instead.
gl_NV_gpu_multicast :: Bool
gl_NV_gpu_multicast = member "GL_NV_gpu_multicast" extensions
{-# NOINLINE gl_NV_gpu_multicast #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/gpu_program4.txt NV_gpu_program4> extension supported?
glGetNVGPUProgram4 :: MonadIO m => m Bool
glGetNVGPUProgram4 = getExtensions >>= (return . member "GL_NV_gpu_program4")

-- | Is the <https://www.opengl.org/registry/specs/NV/gpu_program4.txt NV_gpu_program4> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVGPUProgram4' in those cases instead.
gl_NV_gpu_program4 :: Bool
gl_NV_gpu_program4 = member "GL_NV_gpu_program4" extensions
{-# NOINLINE gl_NV_gpu_program4 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/gpu_program5.txt NV_gpu_program5> extension supported?
glGetNVGPUProgram5 :: MonadIO m => m Bool
glGetNVGPUProgram5 = getExtensions >>= (return . member "GL_NV_gpu_program5")

-- | Is the <https://www.opengl.org/registry/specs/NV/gpu_program5.txt NV_gpu_program5> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVGPUProgram5' in those cases instead.
gl_NV_gpu_program5 :: Bool
gl_NV_gpu_program5 = member "GL_NV_gpu_program5" extensions
{-# NOINLINE gl_NV_gpu_program5 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/gpu_shader5.txt NV_gpu_shader5> extension supported?
glGetNVGPUShader5 :: MonadIO m => m Bool
glGetNVGPUShader5 = getExtensions >>= (return . member "GL_NV_gpu_shader5")

-- | Is the <https://www.opengl.org/registry/specs/NV/gpu_shader5.txt NV_gpu_shader5> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVGPUShader5' in those cases instead.
gl_NV_gpu_shader5 :: Bool
gl_NV_gpu_shader5 = member "GL_NV_gpu_shader5" extensions
{-# NOINLINE gl_NV_gpu_shader5 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/half_float.txt NV_half_float> extension supported?
glGetNVHalfFloat :: MonadIO m => m Bool
glGetNVHalfFloat = getExtensions >>= (return . member "GL_NV_half_float")

-- | Is the <https://www.opengl.org/registry/specs/NV/half_float.txt NV_half_float> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVHalfFloat' in those cases instead.
gl_NV_half_float :: Bool
gl_NV_half_float = member "GL_NV_half_float" extensions
{-# NOINLINE gl_NV_half_float #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/internalformat_sample_query.txt NV_internalformat_sample_query> extension supported?
glGetNVInternalformatSampleQuery :: MonadIO m => m Bool
glGetNVInternalformatSampleQuery = getExtensions >>= (return . member "GL_NV_internalformat_sample_query")

-- | Is the <https://www.opengl.org/registry/specs/NV/internalformat_sample_query.txt NV_internalformat_sample_query> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVInternalformatSampleQuery' in those cases instead.
gl_NV_internalformat_sample_query :: Bool
gl_NV_internalformat_sample_query = member "GL_NV_internalformat_sample_query" extensions
{-# NOINLINE gl_NV_internalformat_sample_query #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/light_max_exponent.txt NV_light_max_exponent> extension supported?
glGetNVLightMaxExponent :: MonadIO m => m Bool
glGetNVLightMaxExponent = getExtensions >>= (return . member "GL_NV_light_max_exponent")

-- | Is the <https://www.opengl.org/registry/specs/NV/light_max_exponent.txt NV_light_max_exponent> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVLightMaxExponent' in those cases instead.
gl_NV_light_max_exponent :: Bool
gl_NV_light_max_exponent = member "GL_NV_light_max_exponent" extensions
{-# NOINLINE gl_NV_light_max_exponent #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/memory_attachment.txt NV_memory_attachment> extension supported?
glGetNVMemoryAttachment :: MonadIO m => m Bool
glGetNVMemoryAttachment = getExtensions >>= (return . member "GL_NV_memory_attachment")

-- | Is the <https://www.opengl.org/registry/specs/NV/memory_attachment.txt NV_memory_attachment> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVMemoryAttachment' in those cases instead.
gl_NV_memory_attachment :: Bool
gl_NV_memory_attachment = member "GL_NV_memory_attachment" extensions
{-# NOINLINE gl_NV_memory_attachment #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/mesh_shader.txt NV_mesh_shader> extension supported?
glGetNVMeshShader :: MonadIO m => m Bool
glGetNVMeshShader = getExtensions >>= (return . member "GL_NV_mesh_shader")

-- | Is the <https://www.opengl.org/registry/specs/NV/mesh_shader.txt NV_mesh_shader> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVMeshShader' in those cases instead.
gl_NV_mesh_shader :: Bool
gl_NV_mesh_shader = member "GL_NV_mesh_shader" extensions
{-# NOINLINE gl_NV_mesh_shader #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/multisample_coverage.txt NV_multisample_coverage> extension supported?
glGetNVMultisampleCoverage :: MonadIO m => m Bool
glGetNVMultisampleCoverage = getExtensions >>= (return . member "GL_NV_multisample_coverage")

-- | Is the <https://www.opengl.org/registry/specs/NV/multisample_coverage.txt NV_multisample_coverage> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVMultisampleCoverage' in those cases instead.
gl_NV_multisample_coverage :: Bool
gl_NV_multisample_coverage = member "GL_NV_multisample_coverage" extensions
{-# NOINLINE gl_NV_multisample_coverage #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/multisample_filter_hint.txt NV_multisample_filter_hint> extension supported?
glGetNVMultisampleFilterHint :: MonadIO m => m Bool
glGetNVMultisampleFilterHint = getExtensions >>= (return . member "GL_NV_multisample_filter_hint")

-- | Is the <https://www.opengl.org/registry/specs/NV/multisample_filter_hint.txt NV_multisample_filter_hint> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVMultisampleFilterHint' in those cases instead.
gl_NV_multisample_filter_hint :: Bool
gl_NV_multisample_filter_hint = member "GL_NV_multisample_filter_hint" extensions
{-# NOINLINE gl_NV_multisample_filter_hint #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/occlusion_query.txt NV_occlusion_query> extension supported?
glGetNVOcclusionQuery :: MonadIO m => m Bool
glGetNVOcclusionQuery = getExtensions >>= (return . member "GL_NV_occlusion_query")

-- | Is the <https://www.opengl.org/registry/specs/NV/occlusion_query.txt NV_occlusion_query> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVOcclusionQuery' in those cases instead.
gl_NV_occlusion_query :: Bool
gl_NV_occlusion_query = member "GL_NV_occlusion_query" extensions
{-# NOINLINE gl_NV_occlusion_query #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/packed_depth_stencil.txt NV_packed_depth_stencil> extension supported?
glGetNVPackedDepthStencil :: MonadIO m => m Bool
glGetNVPackedDepthStencil = getExtensions >>= (return . member "GL_NV_packed_depth_stencil")

-- | Is the <https://www.opengl.org/registry/specs/NV/packed_depth_stencil.txt NV_packed_depth_stencil> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVPackedDepthStencil' in those cases instead.
gl_NV_packed_depth_stencil :: Bool
gl_NV_packed_depth_stencil = member "GL_NV_packed_depth_stencil" extensions
{-# NOINLINE gl_NV_packed_depth_stencil #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/parameter_buffer_object.txt NV_parameter_buffer_object> extension supported?
glGetNVParameterBufferObject :: MonadIO m => m Bool
glGetNVParameterBufferObject = getExtensions >>= (return . member "GL_NV_parameter_buffer_object")

-- | Is the <https://www.opengl.org/registry/specs/NV/parameter_buffer_object.txt NV_parameter_buffer_object> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVParameterBufferObject' in those cases instead.
gl_NV_parameter_buffer_object :: Bool
gl_NV_parameter_buffer_object = member "GL_NV_parameter_buffer_object" extensions
{-# NOINLINE gl_NV_parameter_buffer_object #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/path_rendering.txt NV_path_rendering> extension supported?
glGetNVPathRendering :: MonadIO m => m Bool
glGetNVPathRendering = getExtensions >>= (return . member "GL_NV_path_rendering")

-- | Is the <https://www.opengl.org/registry/specs/NV/path_rendering.txt NV_path_rendering> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVPathRendering' in those cases instead.
gl_NV_path_rendering :: Bool
gl_NV_path_rendering = member "GL_NV_path_rendering" extensions
{-# NOINLINE gl_NV_path_rendering #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/path_rendering_shared_edge.txt NV_path_rendering_shared_edge> extension supported?
glGetNVPathRenderingSharedEdge :: MonadIO m => m Bool
glGetNVPathRenderingSharedEdge = getExtensions >>= (return . member "GL_NV_path_rendering_shared_edge")

-- | Is the <https://www.opengl.org/registry/specs/NV/path_rendering_shared_edge.txt NV_path_rendering_shared_edge> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVPathRenderingSharedEdge' in those cases instead.
gl_NV_path_rendering_shared_edge :: Bool
gl_NV_path_rendering_shared_edge = member "GL_NV_path_rendering_shared_edge" extensions
{-# NOINLINE gl_NV_path_rendering_shared_edge #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/pixel_data_range.txt NV_pixel_data_range> extension supported?
glGetNVPixelDataRange :: MonadIO m => m Bool
glGetNVPixelDataRange = getExtensions >>= (return . member "GL_NV_pixel_data_range")

-- | Is the <https://www.opengl.org/registry/specs/NV/pixel_data_range.txt NV_pixel_data_range> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVPixelDataRange' in those cases instead.
gl_NV_pixel_data_range :: Bool
gl_NV_pixel_data_range = member "GL_NV_pixel_data_range" extensions
{-# NOINLINE gl_NV_pixel_data_range #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/point_sprite.txt NV_point_sprite> extension supported?
glGetNVPointSprite :: MonadIO m => m Bool
glGetNVPointSprite = getExtensions >>= (return . member "GL_NV_point_sprite")

-- | Is the <https://www.opengl.org/registry/specs/NV/point_sprite.txt NV_point_sprite> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVPointSprite' in those cases instead.
gl_NV_point_sprite :: Bool
gl_NV_point_sprite = member "GL_NV_point_sprite" extensions
{-# NOINLINE gl_NV_point_sprite #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/present_video.txt NV_present_video> extension supported?
glGetNVPresentVideo :: MonadIO m => m Bool
glGetNVPresentVideo = getExtensions >>= (return . member "GL_NV_present_video")

-- | Is the <https://www.opengl.org/registry/specs/NV/present_video.txt NV_present_video> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVPresentVideo' in those cases instead.
gl_NV_present_video :: Bool
gl_NV_present_video = member "GL_NV_present_video" extensions
{-# NOINLINE gl_NV_present_video #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/primitive_restart.txt NV_primitive_restart> extension supported?
glGetNVPrimitiveRestart :: MonadIO m => m Bool
glGetNVPrimitiveRestart = getExtensions >>= (return . member "GL_NV_primitive_restart")

-- | Is the <https://www.opengl.org/registry/specs/NV/primitive_restart.txt NV_primitive_restart> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVPrimitiveRestart' in those cases instead.
gl_NV_primitive_restart :: Bool
gl_NV_primitive_restart = member "GL_NV_primitive_restart" extensions
{-# NOINLINE gl_NV_primitive_restart #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/query_resource.txt NV_query_resource> extension supported?
glGetNVQueryResource :: MonadIO m => m Bool
glGetNVQueryResource = getExtensions >>= (return . member "GL_NV_query_resource")

-- | Is the <https://www.opengl.org/registry/specs/NV/query_resource.txt NV_query_resource> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVQueryResource' in those cases instead.
gl_NV_query_resource :: Bool
gl_NV_query_resource = member "GL_NV_query_resource" extensions
{-# NOINLINE gl_NV_query_resource #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/query_resource_tag.txt NV_query_resource_tag> extension supported?
glGetNVQueryResourceTag :: MonadIO m => m Bool
glGetNVQueryResourceTag = getExtensions >>= (return . member "GL_NV_query_resource_tag")

-- | Is the <https://www.opengl.org/registry/specs/NV/query_resource_tag.txt NV_query_resource_tag> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVQueryResourceTag' in those cases instead.
gl_NV_query_resource_tag :: Bool
gl_NV_query_resource_tag = member "GL_NV_query_resource_tag" extensions
{-# NOINLINE gl_NV_query_resource_tag #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/register_combiners.txt NV_register_combiners> extension supported?
glGetNVRegisterCombiners :: MonadIO m => m Bool
glGetNVRegisterCombiners = getExtensions >>= (return . member "GL_NV_register_combiners")

-- | Is the <https://www.opengl.org/registry/specs/NV/register_combiners.txt NV_register_combiners> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVRegisterCombiners' in those cases instead.
gl_NV_register_combiners :: Bool
gl_NV_register_combiners = member "GL_NV_register_combiners" extensions
{-# NOINLINE gl_NV_register_combiners #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/register_combiners2.txt NV_register_combiners2> extension supported?
glGetNVRegisterCombiners2 :: MonadIO m => m Bool
glGetNVRegisterCombiners2 = getExtensions >>= (return . member "GL_NV_register_combiners2")

-- | Is the <https://www.opengl.org/registry/specs/NV/register_combiners2.txt NV_register_combiners2> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVRegisterCombiners2' in those cases instead.
gl_NV_register_combiners2 :: Bool
gl_NV_register_combiners2 = member "GL_NV_register_combiners2" extensions
{-# NOINLINE gl_NV_register_combiners2 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/representative_fragment_test.txt NV_representative_fragment_test> extension supported?
glGetNVRepresentativeFragmentTest :: MonadIO m => m Bool
glGetNVRepresentativeFragmentTest = getExtensions >>= (return . member "GL_NV_representative_fragment_test")

-- | Is the <https://www.opengl.org/registry/specs/NV/representative_fragment_test.txt NV_representative_fragment_test> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVRepresentativeFragmentTest' in those cases instead.
gl_NV_representative_fragment_test :: Bool
gl_NV_representative_fragment_test = member "GL_NV_representative_fragment_test" extensions
{-# NOINLINE gl_NV_representative_fragment_test #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/robustness_video_memory_purge.txt NV_robustness_video_memory_purge> extension supported?
glGetNVRobustnessVideoMemoryPurge :: MonadIO m => m Bool
glGetNVRobustnessVideoMemoryPurge = getExtensions >>= (return . member "GL_NV_robustness_video_memory_purge")

-- | Is the <https://www.opengl.org/registry/specs/NV/robustness_video_memory_purge.txt NV_robustness_video_memory_purge> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVRobustnessVideoMemoryPurge' in those cases instead.
gl_NV_robustness_video_memory_purge :: Bool
gl_NV_robustness_video_memory_purge = member "GL_NV_robustness_video_memory_purge" extensions
{-# NOINLINE gl_NV_robustness_video_memory_purge #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/sample_locations.txt NV_sample_locations> extension supported?
glGetNVSampleLocations :: MonadIO m => m Bool
glGetNVSampleLocations = getExtensions >>= (return . member "GL_NV_sample_locations")

-- | Is the <https://www.opengl.org/registry/specs/NV/sample_locations.txt NV_sample_locations> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVSampleLocations' in those cases instead.
gl_NV_sample_locations :: Bool
gl_NV_sample_locations = member "GL_NV_sample_locations" extensions
{-# NOINLINE gl_NV_sample_locations #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/scissor_exclusive.txt NV_scissor_exclusive> extension supported?
glGetNVScissorExclusive :: MonadIO m => m Bool
glGetNVScissorExclusive = getExtensions >>= (return . member "GL_NV_scissor_exclusive")

-- | Is the <https://www.opengl.org/registry/specs/NV/scissor_exclusive.txt NV_scissor_exclusive> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVScissorExclusive' in those cases instead.
gl_NV_scissor_exclusive :: Bool
gl_NV_scissor_exclusive = member "GL_NV_scissor_exclusive" extensions
{-# NOINLINE gl_NV_scissor_exclusive #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/shader_buffer_load.txt NV_shader_buffer_load> extension supported?
glGetNVShaderBufferLoad :: MonadIO m => m Bool
glGetNVShaderBufferLoad = getExtensions >>= (return . member "GL_NV_shader_buffer_load")

-- | Is the <https://www.opengl.org/registry/specs/NV/shader_buffer_load.txt NV_shader_buffer_load> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVShaderBufferLoad' in those cases instead.
gl_NV_shader_buffer_load :: Bool
gl_NV_shader_buffer_load = member "GL_NV_shader_buffer_load" extensions
{-# NOINLINE gl_NV_shader_buffer_load #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/shader_buffer_store.txt NV_shader_buffer_store> extension supported?
glGetNVShaderBufferStore :: MonadIO m => m Bool
glGetNVShaderBufferStore = getExtensions >>= (return . member "GL_NV_shader_buffer_store")

-- | Is the <https://www.opengl.org/registry/specs/NV/shader_buffer_store.txt NV_shader_buffer_store> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVShaderBufferStore' in those cases instead.
gl_NV_shader_buffer_store :: Bool
gl_NV_shader_buffer_store = member "GL_NV_shader_buffer_store" extensions
{-# NOINLINE gl_NV_shader_buffer_store #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/shader_subgroup_partitioned.txt NV_shader_subgroup_partitioned> extension supported?
glGetNVShaderSubgroupPartitioned :: MonadIO m => m Bool
glGetNVShaderSubgroupPartitioned = getExtensions >>= (return . member "GL_NV_shader_subgroup_partitioned")

-- | Is the <https://www.opengl.org/registry/specs/NV/shader_subgroup_partitioned.txt NV_shader_subgroup_partitioned> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVShaderSubgroupPartitioned' in those cases instead.
gl_NV_shader_subgroup_partitioned :: Bool
gl_NV_shader_subgroup_partitioned = member "GL_NV_shader_subgroup_partitioned" extensions
{-# NOINLINE gl_NV_shader_subgroup_partitioned #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/shader_thread_group.txt NV_shader_thread_group> extension supported?
glGetNVShaderThreadGroup :: MonadIO m => m Bool
glGetNVShaderThreadGroup = getExtensions >>= (return . member "GL_NV_shader_thread_group")

-- | Is the <https://www.opengl.org/registry/specs/NV/shader_thread_group.txt NV_shader_thread_group> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVShaderThreadGroup' in those cases instead.
gl_NV_shader_thread_group :: Bool
gl_NV_shader_thread_group = member "GL_NV_shader_thread_group" extensions
{-# NOINLINE gl_NV_shader_thread_group #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/shading_rate_image.txt NV_shading_rate_image> extension supported?
glGetNVShadingRateImage :: MonadIO m => m Bool
glGetNVShadingRateImage = getExtensions >>= (return . member "GL_NV_shading_rate_image")

-- | Is the <https://www.opengl.org/registry/specs/NV/shading_rate_image.txt NV_shading_rate_image> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVShadingRateImage' in those cases instead.
gl_NV_shading_rate_image :: Bool
gl_NV_shading_rate_image = member "GL_NV_shading_rate_image" extensions
{-# NOINLINE gl_NV_shading_rate_image #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/tessellation_program5.txt NV_tessellation_program5> extension supported?
glGetNVTessellationProgram5 :: MonadIO m => m Bool
glGetNVTessellationProgram5 = getExtensions >>= (return . member "GL_NV_tessellation_program5")

-- | Is the <https://www.opengl.org/registry/specs/NV/tessellation_program5.txt NV_tessellation_program5> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTessellationProgram5' in those cases instead.
gl_NV_tessellation_program5 :: Bool
gl_NV_tessellation_program5 = member "GL_NV_tessellation_program5" extensions
{-# NOINLINE gl_NV_tessellation_program5 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/texgen_emboss.txt NV_texgen_emboss> extension supported?
glGetNVTexgenEmboss :: MonadIO m => m Bool
glGetNVTexgenEmboss = getExtensions >>= (return . member "GL_NV_texgen_emboss")

-- | Is the <https://www.opengl.org/registry/specs/NV/texgen_emboss.txt NV_texgen_emboss> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTexgenEmboss' in those cases instead.
gl_NV_texgen_emboss :: Bool
gl_NV_texgen_emboss = member "GL_NV_texgen_emboss" extensions
{-# NOINLINE gl_NV_texgen_emboss #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/texgen_reflection.txt NV_texgen_reflection> extension supported?
glGetNVTexgenReflection :: MonadIO m => m Bool
glGetNVTexgenReflection = getExtensions >>= (return . member "GL_NV_texgen_reflection")

-- | Is the <https://www.opengl.org/registry/specs/NV/texgen_reflection.txt NV_texgen_reflection> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTexgenReflection' in those cases instead.
gl_NV_texgen_reflection :: Bool
gl_NV_texgen_reflection = member "GL_NV_texgen_reflection" extensions
{-# NOINLINE gl_NV_texgen_reflection #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_barrier.txt NV_texture_barrier> extension supported?
glGetNVTextureBarrier :: MonadIO m => m Bool
glGetNVTextureBarrier = getExtensions >>= (return . member "GL_NV_texture_barrier")

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_barrier.txt NV_texture_barrier> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTextureBarrier' in those cases instead.
gl_NV_texture_barrier :: Bool
gl_NV_texture_barrier = member "GL_NV_texture_barrier" extensions
{-# NOINLINE gl_NV_texture_barrier #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_env_combine4.txt NV_texture_env_combine4> extension supported?
glGetNVTextureEnvCombine4 :: MonadIO m => m Bool
glGetNVTextureEnvCombine4 = getExtensions >>= (return . member "GL_NV_texture_env_combine4")

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_env_combine4.txt NV_texture_env_combine4> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTextureEnvCombine4' in those cases instead.
gl_NV_texture_env_combine4 :: Bool
gl_NV_texture_env_combine4 = member "GL_NV_texture_env_combine4" extensions
{-# NOINLINE gl_NV_texture_env_combine4 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_expand_normal.txt NV_texture_expand_normal> extension supported?
glGetNVTextureExpandNormal :: MonadIO m => m Bool
glGetNVTextureExpandNormal = getExtensions >>= (return . member "GL_NV_texture_expand_normal")

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_expand_normal.txt NV_texture_expand_normal> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTextureExpandNormal' in those cases instead.
gl_NV_texture_expand_normal :: Bool
gl_NV_texture_expand_normal = member "GL_NV_texture_expand_normal" extensions
{-# NOINLINE gl_NV_texture_expand_normal #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_multisample.txt NV_texture_multisample> extension supported?
glGetNVTextureMultisample :: MonadIO m => m Bool
glGetNVTextureMultisample = getExtensions >>= (return . member "GL_NV_texture_multisample")

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_multisample.txt NV_texture_multisample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTextureMultisample' in those cases instead.
gl_NV_texture_multisample :: Bool
gl_NV_texture_multisample = member "GL_NV_texture_multisample" extensions
{-# NOINLINE gl_NV_texture_multisample #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_rectangle.txt NV_texture_rectangle> extension supported?
glGetNVTextureRectangle :: MonadIO m => m Bool
glGetNVTextureRectangle = getExtensions >>= (return . member "GL_NV_texture_rectangle")

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_rectangle.txt NV_texture_rectangle> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTextureRectangle' in those cases instead.
gl_NV_texture_rectangle :: Bool
gl_NV_texture_rectangle = member "GL_NV_texture_rectangle" extensions
{-# NOINLINE gl_NV_texture_rectangle #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_shader.txt NV_texture_shader> extension supported?
glGetNVTextureShader :: MonadIO m => m Bool
glGetNVTextureShader = getExtensions >>= (return . member "GL_NV_texture_shader")

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_shader.txt NV_texture_shader> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTextureShader' in those cases instead.
gl_NV_texture_shader :: Bool
gl_NV_texture_shader = member "GL_NV_texture_shader" extensions
{-# NOINLINE gl_NV_texture_shader #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_shader2.txt NV_texture_shader2> extension supported?
glGetNVTextureShader2 :: MonadIO m => m Bool
glGetNVTextureShader2 = getExtensions >>= (return . member "GL_NV_texture_shader2")

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_shader2.txt NV_texture_shader2> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTextureShader2' in those cases instead.
gl_NV_texture_shader2 :: Bool
gl_NV_texture_shader2 = member "GL_NV_texture_shader2" extensions
{-# NOINLINE gl_NV_texture_shader2 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_shader3.txt NV_texture_shader3> extension supported?
glGetNVTextureShader3 :: MonadIO m => m Bool
glGetNVTextureShader3 = getExtensions >>= (return . member "GL_NV_texture_shader3")

-- | Is the <https://www.opengl.org/registry/specs/NV/texture_shader3.txt NV_texture_shader3> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTextureShader3' in those cases instead.
gl_NV_texture_shader3 :: Bool
gl_NV_texture_shader3 = member "GL_NV_texture_shader3" extensions
{-# NOINLINE gl_NV_texture_shader3 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/transform_feedback.txt NV_transform_feedback> extension supported?
glGetNVTransformFeedback :: MonadIO m => m Bool
glGetNVTransformFeedback = getExtensions >>= (return . member "GL_NV_transform_feedback")

-- | Is the <https://www.opengl.org/registry/specs/NV/transform_feedback.txt NV_transform_feedback> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTransformFeedback' in those cases instead.
gl_NV_transform_feedback :: Bool
gl_NV_transform_feedback = member "GL_NV_transform_feedback" extensions
{-# NOINLINE gl_NV_transform_feedback #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/transform_feedback2.txt NV_transform_feedback2> extension supported?
glGetNVTransformFeedback2 :: MonadIO m => m Bool
glGetNVTransformFeedback2 = getExtensions >>= (return . member "GL_NV_transform_feedback2")

-- | Is the <https://www.opengl.org/registry/specs/NV/transform_feedback2.txt NV_transform_feedback2> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVTransformFeedback2' in those cases instead.
gl_NV_transform_feedback2 :: Bool
gl_NV_transform_feedback2 = member "GL_NV_transform_feedback2" extensions
{-# NOINLINE gl_NV_transform_feedback2 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/uniform_buffer_unified_memory.txt NV_uniform_buffer_unified_memory> extension supported?
glGetNVUniformBufferUnifiedMemory :: MonadIO m => m Bool
glGetNVUniformBufferUnifiedMemory = getExtensions >>= (return . member "GL_NV_uniform_buffer_unified_memory")

-- | Is the <https://www.opengl.org/registry/specs/NV/uniform_buffer_unified_memory.txt NV_uniform_buffer_unified_memory> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVUniformBufferUnifiedMemory' in those cases instead.
gl_NV_uniform_buffer_unified_memory :: Bool
gl_NV_uniform_buffer_unified_memory = member "GL_NV_uniform_buffer_unified_memory" extensions
{-# NOINLINE gl_NV_uniform_buffer_unified_memory #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/vdpau_interop.txt NV_vdpau_interop> extension supported?
glGetNVVDPAUInterop :: MonadIO m => m Bool
glGetNVVDPAUInterop = getExtensions >>= (return . member "GL_NV_vdpau_interop")

-- | Is the <https://www.opengl.org/registry/specs/NV/vdpau_interop.txt NV_vdpau_interop> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVVDPAUInterop' in those cases instead.
gl_NV_vdpau_interop :: Bool
gl_NV_vdpau_interop = member "GL_NV_vdpau_interop" extensions
{-# NOINLINE gl_NV_vdpau_interop #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/vdpau_interop2.txt NV_vdpau_interop2> extension supported?
glGetNVVDPAUInterop2 :: MonadIO m => m Bool
glGetNVVDPAUInterop2 = getExtensions >>= (return . member "GL_NV_vdpau_interop2")

-- | Is the <https://www.opengl.org/registry/specs/NV/vdpau_interop2.txt NV_vdpau_interop2> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVVDPAUInterop2' in those cases instead.
gl_NV_vdpau_interop2 :: Bool
gl_NV_vdpau_interop2 = member "GL_NV_vdpau_interop2" extensions
{-# NOINLINE gl_NV_vdpau_interop2 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_array_range.txt NV_vertex_array_range> extension supported?
glGetNVVertexArrayRange :: MonadIO m => m Bool
glGetNVVertexArrayRange = getExtensions >>= (return . member "GL_NV_vertex_array_range")

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_array_range.txt NV_vertex_array_range> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVVertexArrayRange' in those cases instead.
gl_NV_vertex_array_range :: Bool
gl_NV_vertex_array_range = member "GL_NV_vertex_array_range" extensions
{-# NOINLINE gl_NV_vertex_array_range #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_array_range2.txt NV_vertex_array_range2> extension supported?
glGetNVVertexArrayRange2 :: MonadIO m => m Bool
glGetNVVertexArrayRange2 = getExtensions >>= (return . member "GL_NV_vertex_array_range2")

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_array_range2.txt NV_vertex_array_range2> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVVertexArrayRange2' in those cases instead.
gl_NV_vertex_array_range2 :: Bool
gl_NV_vertex_array_range2 = member "GL_NV_vertex_array_range2" extensions
{-# NOINLINE gl_NV_vertex_array_range2 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt NV_vertex_attrib_integer_64bit> extension supported?
glGetNVVertexAttribInteger64Bit :: MonadIO m => m Bool
glGetNVVertexAttribInteger64Bit = getExtensions >>= (return . member "GL_NV_vertex_attrib_integer_64bit")

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_attrib_integer_64bit.txt NV_vertex_attrib_integer_64bit> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVVertexAttribInteger64Bit' in those cases instead.
gl_NV_vertex_attrib_integer_64bit :: Bool
gl_NV_vertex_attrib_integer_64bit = member "GL_NV_vertex_attrib_integer_64bit" extensions
{-# NOINLINE gl_NV_vertex_attrib_integer_64bit #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt NV_vertex_buffer_unified_memory> extension supported?
glGetNVVertexBufferUnifiedMemory :: MonadIO m => m Bool
glGetNVVertexBufferUnifiedMemory = getExtensions >>= (return . member "GL_NV_vertex_buffer_unified_memory")

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_buffer_unified_memory.txt NV_vertex_buffer_unified_memory> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVVertexBufferUnifiedMemory' in those cases instead.
gl_NV_vertex_buffer_unified_memory :: Bool
gl_NV_vertex_buffer_unified_memory = member "GL_NV_vertex_buffer_unified_memory" extensions
{-# NOINLINE gl_NV_vertex_buffer_unified_memory #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_program.txt NV_vertex_program> extension supported?
glGetNVVertexProgram :: MonadIO m => m Bool
glGetNVVertexProgram = getExtensions >>= (return . member "GL_NV_vertex_program")

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_program.txt NV_vertex_program> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVVertexProgram' in those cases instead.
gl_NV_vertex_program :: Bool
gl_NV_vertex_program = member "GL_NV_vertex_program" extensions
{-# NOINLINE gl_NV_vertex_program #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_program2_option.txt NV_vertex_program2_option> extension supported?
glGetNVVertexProgram2Option :: MonadIO m => m Bool
glGetNVVertexProgram2Option = getExtensions >>= (return . member "GL_NV_vertex_program2_option")

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_program2_option.txt NV_vertex_program2_option> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVVertexProgram2Option' in those cases instead.
gl_NV_vertex_program2_option :: Bool
gl_NV_vertex_program2_option = member "GL_NV_vertex_program2_option" extensions
{-# NOINLINE gl_NV_vertex_program2_option #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_program3.txt NV_vertex_program3> extension supported?
glGetNVVertexProgram3 :: MonadIO m => m Bool
glGetNVVertexProgram3 = getExtensions >>= (return . member "GL_NV_vertex_program3")

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_program3.txt NV_vertex_program3> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVVertexProgram3' in those cases instead.
gl_NV_vertex_program3 :: Bool
gl_NV_vertex_program3 = member "GL_NV_vertex_program3" extensions
{-# NOINLINE gl_NV_vertex_program3 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_program4.txt NV_vertex_program4> extension supported?
glGetNVVertexProgram4 :: MonadIO m => m Bool
glGetNVVertexProgram4 = getExtensions >>= (return . member "GL_NV_vertex_program4")

-- | Is the <https://www.opengl.org/registry/specs/NV/vertex_program4.txt NV_vertex_program4> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVVertexProgram4' in those cases instead.
gl_NV_vertex_program4 :: Bool
gl_NV_vertex_program4 = member "GL_NV_vertex_program4" extensions
{-# NOINLINE gl_NV_vertex_program4 #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/video_capture.txt NV_video_capture> extension supported?
glGetNVVideoCapture :: MonadIO m => m Bool
glGetNVVideoCapture = getExtensions >>= (return . member "GL_NV_video_capture")

-- | Is the <https://www.opengl.org/registry/specs/NV/video_capture.txt NV_video_capture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVVideoCapture' in those cases instead.
gl_NV_video_capture :: Bool
gl_NV_video_capture = member "GL_NV_video_capture" extensions
{-# NOINLINE gl_NV_video_capture #-}

-- | Is the <https://www.opengl.org/registry/specs/NV/viewport_swizzle.txt NV_viewport_swizzle> extension supported?
glGetNVViewportSwizzle :: MonadIO m => m Bool
glGetNVViewportSwizzle = getExtensions >>= (return . member "GL_NV_viewport_swizzle")

-- | Is the <https://www.opengl.org/registry/specs/NV/viewport_swizzle.txt NV_viewport_swizzle> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVViewportSwizzle' in those cases instead.
gl_NV_viewport_swizzle :: Bool
gl_NV_viewport_swizzle = member "GL_NV_viewport_swizzle" extensions
{-# NOINLINE gl_NV_viewport_swizzle #-}

-- | Is the <https://www.opengl.org/registry/specs/NVX/nvx_conditional_render.txt NVX_conditional_render> extension supported?
glGetNVXConditionalRender :: MonadIO m => m Bool
glGetNVXConditionalRender = getExtensions >>= (return . member "GL_NVX_conditional_render")

-- | Is the <https://www.opengl.org/registry/specs/NVX/nvx_conditional_render.txt NVX_conditional_render> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVXConditionalRender' in those cases instead.
gl_NVX_conditional_render :: Bool
gl_NVX_conditional_render = member "GL_NVX_conditional_render" extensions
{-# NOINLINE gl_NVX_conditional_render #-}

-- | Is the <https://www.opengl.org/registry/specs/NVX/gpu_memory_info.txt NVX_gpu_memory_info> extension supported?
glGetNVXGPUMemoryInfo :: MonadIO m => m Bool
glGetNVXGPUMemoryInfo = getExtensions >>= (return . member "GL_NVX_gpu_memory_info")

-- | Is the <https://www.opengl.org/registry/specs/NVX/gpu_memory_info.txt NVX_gpu_memory_info> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVXGPUMemoryInfo' in those cases instead.
gl_NVX_gpu_memory_info :: Bool
gl_NVX_gpu_memory_info = member "GL_NVX_gpu_memory_info" extensions
{-# NOINLINE gl_NVX_gpu_memory_info #-}

-- | Is the <https://www.opengl.org/registry/specs/NVX/gpu_multicast2.txt NVX_gpu_multicast2> extension supported?
glGetNVXGPUMulticast2 :: MonadIO m => m Bool
glGetNVXGPUMulticast2 = getExtensions >>= (return . member "GL_NVX_gpu_multicast2")

-- | Is the <https://www.opengl.org/registry/specs/NVX/gpu_multicast2.txt NVX_gpu_multicast2> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVXGPUMulticast2' in those cases instead.
gl_NVX_gpu_multicast2 :: Bool
gl_NVX_gpu_multicast2 = member "GL_NVX_gpu_multicast2" extensions
{-# NOINLINE gl_NVX_gpu_multicast2 #-}

-- | Is the <https://www.opengl.org/registry/specs/NVX/linked_gpu_multicast.txt NVX_linked_gpu_multicast> extension supported?
glGetNVXLinkedGPUMulticast :: MonadIO m => m Bool
glGetNVXLinkedGPUMulticast = getExtensions >>= (return . member "GL_NVX_linked_gpu_multicast")

-- | Is the <https://www.opengl.org/registry/specs/NVX/linked_gpu_multicast.txt NVX_linked_gpu_multicast> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVXLinkedGPUMulticast' in those cases instead.
gl_NVX_linked_gpu_multicast :: Bool
gl_NVX_linked_gpu_multicast = member "GL_NVX_linked_gpu_multicast" extensions
{-# NOINLINE gl_NVX_linked_gpu_multicast #-}

-- | Is the <https://www.opengl.org/registry/specs/NVX/progress_fence.txt NVX_progress_fence> extension supported?
glGetNVXProgressFence :: MonadIO m => m Bool
glGetNVXProgressFence = getExtensions >>= (return . member "GL_NVX_progress_fence")

-- | Is the <https://www.opengl.org/registry/specs/NVX/progress_fence.txt NVX_progress_fence> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetNVXProgressFence' in those cases instead.
gl_NVX_progress_fence :: Bool
gl_NVX_progress_fence = member "GL_NVX_progress_fence" extensions
{-# NOINLINE gl_NVX_progress_fence #-}

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_byte_coordinates.txt OES_byte_coordinates> extension supported?
glGetOESByteCoordinates :: MonadIO m => m Bool
glGetOESByteCoordinates = getExtensions >>= (return . member "GL_OES_byte_coordinates")

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_byte_coordinates.txt OES_byte_coordinates> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetOESByteCoordinates' in those cases instead.
gl_OES_byte_coordinates :: Bool
gl_OES_byte_coordinates = member "GL_OES_byte_coordinates" extensions
{-# NOINLINE gl_OES_byte_coordinates #-}

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_compressed_paletted_texture.txt OES_compressed_paletted_texture> extension supported?
glGetOESCompressedPalettedTexture :: MonadIO m => m Bool
glGetOESCompressedPalettedTexture = getExtensions >>= (return . member "GL_OES_compressed_paletted_texture")

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_compressed_paletted_texture.txt OES_compressed_paletted_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetOESCompressedPalettedTexture' in those cases instead.
gl_OES_compressed_paletted_texture :: Bool
gl_OES_compressed_paletted_texture = member "GL_OES_compressed_paletted_texture" extensions
{-# NOINLINE gl_OES_compressed_paletted_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_fixed_point.txt OES_fixed_point> extension supported?
glGetOESFixedPoint :: MonadIO m => m Bool
glGetOESFixedPoint = getExtensions >>= (return . member "GL_OES_fixed_point")

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_fixed_point.txt OES_fixed_point> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetOESFixedPoint' in those cases instead.
gl_OES_fixed_point :: Bool
gl_OES_fixed_point = member "GL_OES_fixed_point" extensions
{-# NOINLINE gl_OES_fixed_point #-}

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_query_matrix.txt OES_query_matrix> extension supported?
glGetOESQueryMatrix :: MonadIO m => m Bool
glGetOESQueryMatrix = getExtensions >>= (return . member "GL_OES_query_matrix")

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_query_matrix.txt OES_query_matrix> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetOESQueryMatrix' in those cases instead.
gl_OES_query_matrix :: Bool
gl_OES_query_matrix = member "GL_OES_query_matrix" extensions
{-# NOINLINE gl_OES_query_matrix #-}

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_read_format.txt OES_read_format> extension supported?
glGetOESReadFormat :: MonadIO m => m Bool
glGetOESReadFormat = getExtensions >>= (return . member "GL_OES_read_format")

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_read_format.txt OES_read_format> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetOESReadFormat' in those cases instead.
gl_OES_read_format :: Bool
gl_OES_read_format = member "GL_OES_read_format" extensions
{-# NOINLINE gl_OES_read_format #-}

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_single_precision.txt OES_single_precision> extension supported?
glGetOESSinglePrecision :: MonadIO m => m Bool
glGetOESSinglePrecision = getExtensions >>= (return . member "GL_OES_single_precision")

-- | Is the <https://www.opengl.org/registry/specs/OES/OES_single_precision.txt OES_single_precision> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetOESSinglePrecision' in those cases instead.
gl_OES_single_precision :: Bool
gl_OES_single_precision = member "GL_OES_single_precision" extensions
{-# NOINLINE gl_OES_single_precision #-}

-- | Is the <https://www.opengl.org/registry/specs/OML/interlace.txt OML_interlace> extension supported?
glGetOMLInterlace :: MonadIO m => m Bool
glGetOMLInterlace = getExtensions >>= (return . member "GL_OML_interlace")

-- | Is the <https://www.opengl.org/registry/specs/OML/interlace.txt OML_interlace> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetOMLInterlace' in those cases instead.
gl_OML_interlace :: Bool
gl_OML_interlace = member "GL_OML_interlace" extensions
{-# NOINLINE gl_OML_interlace #-}

-- | Is the <https://www.opengl.org/registry/specs/OML/resample.txt OML_resample> extension supported?
glGetOMLResample :: MonadIO m => m Bool
glGetOMLResample = getExtensions >>= (return . member "GL_OML_resample")

-- | Is the <https://www.opengl.org/registry/specs/OML/resample.txt OML_resample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetOMLResample' in those cases instead.
gl_OML_resample :: Bool
gl_OML_resample = member "GL_OML_resample" extensions
{-# NOINLINE gl_OML_resample #-}

-- | Is the <https://www.opengl.org/registry/specs/OML/subsample.txt OML_subsample> extension supported?
glGetOMLSubsample :: MonadIO m => m Bool
glGetOMLSubsample = getExtensions >>= (return . member "GL_OML_subsample")

-- | Is the <https://www.opengl.org/registry/specs/OML/subsample.txt OML_subsample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetOMLSubsample' in those cases instead.
gl_OML_subsample :: Bool
gl_OML_subsample = member "GL_OML_subsample" extensions
{-# NOINLINE gl_OML_subsample #-}

-- | Is the <https://www.opengl.org/registry/specs/OVR/multiview.txt OVR_multiview> extension supported?
glGetOVRMultiview :: MonadIO m => m Bool
glGetOVRMultiview = getExtensions >>= (return . member "GL_OVR_multiview")

-- | Is the <https://www.opengl.org/registry/specs/OVR/multiview.txt OVR_multiview> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetOVRMultiview' in those cases instead.
gl_OVR_multiview :: Bool
gl_OVR_multiview = member "GL_OVR_multiview" extensions
{-# NOINLINE gl_OVR_multiview #-}

-- | Is the <https://www.opengl.org/registry/specs/PGI/misc_hints.txt PGI_misc_hints> extension supported?
glGetPGIMiscHints :: MonadIO m => m Bool
glGetPGIMiscHints = getExtensions >>= (return . member "GL_PGI_misc_hints")

-- | Is the <https://www.opengl.org/registry/specs/PGI/misc_hints.txt PGI_misc_hints> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetPGIMiscHints' in those cases instead.
gl_PGI_misc_hints :: Bool
gl_PGI_misc_hints = member "GL_PGI_misc_hints" extensions
{-# NOINLINE gl_PGI_misc_hints #-}

-- | Is the <https://www.opengl.org/registry/specs/PGI/vertex_hints.txt PGI_vertex_hints> extension supported?
glGetPGIVertexHints :: MonadIO m => m Bool
glGetPGIVertexHints = getExtensions >>= (return . member "GL_PGI_vertex_hints")

-- | Is the <https://www.opengl.org/registry/specs/PGI/vertex_hints.txt PGI_vertex_hints> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetPGIVertexHints' in those cases instead.
gl_PGI_vertex_hints :: Bool
gl_PGI_vertex_hints = member "GL_PGI_vertex_hints" extensions
{-# NOINLINE gl_PGI_vertex_hints #-}

-- | Is the <https://www.opengl.org/registry/specs/REND/screen_coordinates.txt REND_screen_coordinates> extension supported?
glGetRENDScreenCoordinates :: MonadIO m => m Bool
glGetRENDScreenCoordinates = getExtensions >>= (return . member "GL_REND_screen_coordinates")

-- | Is the <https://www.opengl.org/registry/specs/REND/screen_coordinates.txt REND_screen_coordinates> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetRENDScreenCoordinates' in those cases instead.
gl_REND_screen_coordinates :: Bool
gl_REND_screen_coordinates = member "GL_REND_screen_coordinates" extensions
{-# NOINLINE gl_REND_screen_coordinates #-}

-- | Is the <https://www.opengl.org/registry/specs/S3/s3tc.txt S3_s3tc> extension supported?
glGetS3S3TC :: MonadIO m => m Bool
glGetS3S3TC = getExtensions >>= (return . member "GL_S3_s3tc")

-- | Is the <https://www.opengl.org/registry/specs/S3/s3tc.txt S3_s3tc> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetS3S3TC' in those cases instead.
gl_S3_s3tc :: Bool
gl_S3_s3tc = member "GL_S3_s3tc" extensions
{-# NOINLINE gl_S3_s3tc #-}

-- | Is the <https://www.opengl.org/registry/specs/SGI/color_matrix.txt SGI_color_matrix> extension supported?
glGetSGIColorMatrix :: MonadIO m => m Bool
glGetSGIColorMatrix = getExtensions >>= (return . member "GL_SGI_color_matrix")

-- | Is the <https://www.opengl.org/registry/specs/SGI/color_matrix.txt SGI_color_matrix> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIColorMatrix' in those cases instead.
gl_SGI_color_matrix :: Bool
gl_SGI_color_matrix = member "GL_SGI_color_matrix" extensions
{-# NOINLINE gl_SGI_color_matrix #-}

-- | Is the <https://www.opengl.org/registry/specs/SGI/color_table.txt SGI_color_table> extension supported?
glGetSGIColorTable :: MonadIO m => m Bool
glGetSGIColorTable = getExtensions >>= (return . member "GL_SGI_color_table")

-- | Is the <https://www.opengl.org/registry/specs/SGI/color_table.txt SGI_color_table> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIColorTable' in those cases instead.
gl_SGI_color_table :: Bool
gl_SGI_color_table = member "GL_SGI_color_table" extensions
{-# NOINLINE gl_SGI_color_table #-}

-- | Is the <https://www.opengl.org/registry/specs/SGI/texture_color_table.txt SGI_texture_color_table> extension supported?
glGetSGITextureColorTable :: MonadIO m => m Bool
glGetSGITextureColorTable = getExtensions >>= (return . member "GL_SGI_texture_color_table")

-- | Is the <https://www.opengl.org/registry/specs/SGI/texture_color_table.txt SGI_texture_color_table> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGITextureColorTable' in those cases instead.
gl_SGI_texture_color_table :: Bool
gl_SGI_texture_color_table = member "GL_SGI_texture_color_table" extensions
{-# NOINLINE gl_SGI_texture_color_table #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/detail_texture.txt SGIS_detail_texture> extension supported?
glGetSGISDetailTexture :: MonadIO m => m Bool
glGetSGISDetailTexture = getExtensions >>= (return . member "GL_SGIS_detail_texture")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/detail_texture.txt SGIS_detail_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISDetailTexture' in those cases instead.
gl_SGIS_detail_texture :: Bool
gl_SGIS_detail_texture = member "GL_SGIS_detail_texture" extensions
{-# NOINLINE gl_SGIS_detail_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/fog_func.txt SGIS_fog_function> extension supported?
glGetSGISFogFunction :: MonadIO m => m Bool
glGetSGISFogFunction = getExtensions >>= (return . member "GL_SGIS_fog_function")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/fog_func.txt SGIS_fog_function> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISFogFunction' in those cases instead.
gl_SGIS_fog_function :: Bool
gl_SGIS_fog_function = member "GL_SGIS_fog_function" extensions
{-# NOINLINE gl_SGIS_fog_function #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/generate_mipmap.txt SGIS_generate_mipmap> extension supported?
glGetSGISGenerateMipmap :: MonadIO m => m Bool
glGetSGISGenerateMipmap = getExtensions >>= (return . member "GL_SGIS_generate_mipmap")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/generate_mipmap.txt SGIS_generate_mipmap> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISGenerateMipmap' in those cases instead.
gl_SGIS_generate_mipmap :: Bool
gl_SGIS_generate_mipmap = member "GL_SGIS_generate_mipmap" extensions
{-# NOINLINE gl_SGIS_generate_mipmap #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/multisample.txt SGIS_multisample> extension supported?
glGetSGISMultisample :: MonadIO m => m Bool
glGetSGISMultisample = getExtensions >>= (return . member "GL_SGIS_multisample")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/multisample.txt SGIS_multisample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISMultisample' in those cases instead.
gl_SGIS_multisample :: Bool
gl_SGIS_multisample = member "GL_SGIS_multisample" extensions
{-# NOINLINE gl_SGIS_multisample #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/pixel_texture.txt SGIS_pixel_texture> extension supported?
glGetSGISPixelTexture :: MonadIO m => m Bool
glGetSGISPixelTexture = getExtensions >>= (return . member "GL_SGIS_pixel_texture")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/pixel_texture.txt SGIS_pixel_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISPixelTexture' in those cases instead.
gl_SGIS_pixel_texture :: Bool
gl_SGIS_pixel_texture = member "GL_SGIS_pixel_texture" extensions
{-# NOINLINE gl_SGIS_pixel_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/point_line_texgen.txt SGIS_point_line_texgen> extension supported?
glGetSGISPointLineTexgen :: MonadIO m => m Bool
glGetSGISPointLineTexgen = getExtensions >>= (return . member "GL_SGIS_point_line_texgen")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/point_line_texgen.txt SGIS_point_line_texgen> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISPointLineTexgen' in those cases instead.
gl_SGIS_point_line_texgen :: Bool
gl_SGIS_point_line_texgen = member "GL_SGIS_point_line_texgen" extensions
{-# NOINLINE gl_SGIS_point_line_texgen #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/point_parameters.txt SGIS_point_parameters> extension supported?
glGetSGISPointParameters :: MonadIO m => m Bool
glGetSGISPointParameters = getExtensions >>= (return . member "GL_SGIS_point_parameters")

-- | Is the <https://www.opengl.org/registry/specs/EXT/point_parameters.txt SGIS_point_parameters> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISPointParameters' in those cases instead.
gl_SGIS_point_parameters :: Bool
gl_SGIS_point_parameters = member "GL_SGIS_point_parameters" extensions
{-# NOINLINE gl_SGIS_point_parameters #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/sharpen_texture.txt SGIS_sharpen_texture> extension supported?
glGetSGISSharpenTexture :: MonadIO m => m Bool
glGetSGISSharpenTexture = getExtensions >>= (return . member "GL_SGIS_sharpen_texture")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/sharpen_texture.txt SGIS_sharpen_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISSharpenTexture' in those cases instead.
gl_SGIS_sharpen_texture :: Bool
gl_SGIS_sharpen_texture = member "GL_SGIS_sharpen_texture" extensions
{-# NOINLINE gl_SGIS_sharpen_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture4D.txt SGIS_texture4D> extension supported?
glGetSGISTexture4D :: MonadIO m => m Bool
glGetSGISTexture4D = getExtensions >>= (return . member "GL_SGIS_texture4D")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture4D.txt SGIS_texture4D> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISTexture4D' in those cases instead.
gl_SGIS_texture4D :: Bool
gl_SGIS_texture4D = member "GL_SGIS_texture4D" extensions
{-# NOINLINE gl_SGIS_texture4D #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_border_clamp.txt SGIS_texture_border_clamp> extension supported?
glGetSGISTextureBorderClamp :: MonadIO m => m Bool
glGetSGISTextureBorderClamp = getExtensions >>= (return . member "GL_SGIS_texture_border_clamp")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_border_clamp.txt SGIS_texture_border_clamp> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISTextureBorderClamp' in those cases instead.
gl_SGIS_texture_border_clamp :: Bool
gl_SGIS_texture_border_clamp = member "GL_SGIS_texture_border_clamp" extensions
{-# NOINLINE gl_SGIS_texture_border_clamp #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_color_mask.txt SGIS_texture_color_mask> extension supported?
glGetSGISTextureColorMask :: MonadIO m => m Bool
glGetSGISTextureColorMask = getExtensions >>= (return . member "GL_SGIS_texture_color_mask")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_color_mask.txt SGIS_texture_color_mask> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISTextureColorMask' in those cases instead.
gl_SGIS_texture_color_mask :: Bool
gl_SGIS_texture_color_mask = member "GL_SGIS_texture_color_mask" extensions
{-# NOINLINE gl_SGIS_texture_color_mask #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_edge_clamp.txt SGIS_texture_edge_clamp> extension supported?
glGetSGISTextureEdgeClamp :: MonadIO m => m Bool
glGetSGISTextureEdgeClamp = getExtensions >>= (return . member "GL_SGIS_texture_edge_clamp")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_edge_clamp.txt SGIS_texture_edge_clamp> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISTextureEdgeClamp' in those cases instead.
gl_SGIS_texture_edge_clamp :: Bool
gl_SGIS_texture_edge_clamp = member "GL_SGIS_texture_edge_clamp" extensions
{-# NOINLINE gl_SGIS_texture_edge_clamp #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_filter4.txt SGIS_texture_filter4> extension supported?
glGetSGISTextureFilter4 :: MonadIO m => m Bool
glGetSGISTextureFilter4 = getExtensions >>= (return . member "GL_SGIS_texture_filter4")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_filter4.txt SGIS_texture_filter4> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISTextureFilter4' in those cases instead.
gl_SGIS_texture_filter4 :: Bool
gl_SGIS_texture_filter4 = member "GL_SGIS_texture_filter4" extensions
{-# NOINLINE gl_SGIS_texture_filter4 #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_lod.txt SGIS_texture_lod> extension supported?
glGetSGISTextureLOD :: MonadIO m => m Bool
glGetSGISTextureLOD = getExtensions >>= (return . member "GL_SGIS_texture_lod")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_lod.txt SGIS_texture_lod> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISTextureLOD' in those cases instead.
gl_SGIS_texture_lod :: Bool
gl_SGIS_texture_lod = member "GL_SGIS_texture_lod" extensions
{-# NOINLINE gl_SGIS_texture_lod #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_select.txt SGIS_texture_select> extension supported?
glGetSGISTextureSelect :: MonadIO m => m Bool
glGetSGISTextureSelect = getExtensions >>= (return . member "GL_SGIS_texture_select")

-- | Is the <https://www.opengl.org/registry/specs/SGIS/texture_select.txt SGIS_texture_select> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGISTextureSelect' in those cases instead.
gl_SGIS_texture_select :: Bool
gl_SGIS_texture_select = member "GL_SGIS_texture_select" extensions
{-# NOINLINE gl_SGIS_texture_select #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/async.txt SGIX_async> extension supported?
glGetSGIXAsync :: MonadIO m => m Bool
glGetSGIXAsync = getExtensions >>= (return . member "GL_SGIX_async")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/async.txt SGIX_async> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXAsync' in those cases instead.
gl_SGIX_async :: Bool
gl_SGIX_async = member "GL_SGIX_async" extensions
{-# NOINLINE gl_SGIX_async #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/async_histogram.txt SGIX_async_histogram> extension supported?
glGetSGIXAsyncHistogram :: MonadIO m => m Bool
glGetSGIXAsyncHistogram = getExtensions >>= (return . member "GL_SGIX_async_histogram")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/async_histogram.txt SGIX_async_histogram> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXAsyncHistogram' in those cases instead.
gl_SGIX_async_histogram :: Bool
gl_SGIX_async_histogram = member "GL_SGIX_async_histogram" extensions
{-# NOINLINE gl_SGIX_async_histogram #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/async_pixel.txt SGIX_async_pixel> extension supported?
glGetSGIXAsyncPixel :: MonadIO m => m Bool
glGetSGIXAsyncPixel = getExtensions >>= (return . member "GL_SGIX_async_pixel")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/async_pixel.txt SGIX_async_pixel> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXAsyncPixel' in those cases instead.
gl_SGIX_async_pixel :: Bool
gl_SGIX_async_pixel = member "GL_SGIX_async_pixel" extensions
{-# NOINLINE gl_SGIX_async_pixel #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/blend_alpha_minmax.txt SGIX_blend_alpha_minmax> extension supported?
glGetSGIXBlendAlphaMinmax :: MonadIO m => m Bool
glGetSGIXBlendAlphaMinmax = getExtensions >>= (return . member "GL_SGIX_blend_alpha_minmax")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/blend_alpha_minmax.txt SGIX_blend_alpha_minmax> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXBlendAlphaMinmax' in those cases instead.
gl_SGIX_blend_alpha_minmax :: Bool
gl_SGIX_blend_alpha_minmax = member "GL_SGIX_blend_alpha_minmax" extensions
{-# NOINLINE gl_SGIX_blend_alpha_minmax #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/calligraphic_fragment.txt SGIX_calligraphic_fragment> extension supported?
glGetSGIXCalligraphicFragment :: MonadIO m => m Bool
glGetSGIXCalligraphicFragment = getExtensions >>= (return . member "GL_SGIX_calligraphic_fragment")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/calligraphic_fragment.txt SGIX_calligraphic_fragment> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXCalligraphicFragment' in those cases instead.
gl_SGIX_calligraphic_fragment :: Bool
gl_SGIX_calligraphic_fragment = member "GL_SGIX_calligraphic_fragment" extensions
{-# NOINLINE gl_SGIX_calligraphic_fragment #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/clipmap.txt SGIX_clipmap> extension supported?
glGetSGIXClipmap :: MonadIO m => m Bool
glGetSGIXClipmap = getExtensions >>= (return . member "GL_SGIX_clipmap")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/clipmap.txt SGIX_clipmap> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXClipmap' in those cases instead.
gl_SGIX_clipmap :: Bool
gl_SGIX_clipmap = member "GL_SGIX_clipmap" extensions
{-# NOINLINE gl_SGIX_clipmap #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/convolution_accuracy.txt SGIX_convolution_accuracy> extension supported?
glGetSGIXConvolutionAccuracy :: MonadIO m => m Bool
glGetSGIXConvolutionAccuracy = getExtensions >>= (return . member "GL_SGIX_convolution_accuracy")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/convolution_accuracy.txt SGIX_convolution_accuracy> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXConvolutionAccuracy' in those cases instead.
gl_SGIX_convolution_accuracy :: Bool
gl_SGIX_convolution_accuracy = member "GL_SGIX_convolution_accuracy" extensions
{-# NOINLINE gl_SGIX_convolution_accuracy #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/depth_texture.txt SGIX_depth_texture> extension supported?
glGetSGIXDepthTexture :: MonadIO m => m Bool
glGetSGIXDepthTexture = getExtensions >>= (return . member "GL_SGIX_depth_texture")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/depth_texture.txt SGIX_depth_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXDepthTexture' in those cases instead.
gl_SGIX_depth_texture :: Bool
gl_SGIX_depth_texture = member "GL_SGIX_depth_texture" extensions
{-# NOINLINE gl_SGIX_depth_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/flush_raster.txt SGIX_flush_raster> extension supported?
glGetSGIXFlushRaster :: MonadIO m => m Bool
glGetSGIXFlushRaster = getExtensions >>= (return . member "GL_SGIX_flush_raster")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/flush_raster.txt SGIX_flush_raster> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXFlushRaster' in those cases instead.
gl_SGIX_flush_raster :: Bool
gl_SGIX_flush_raster = member "GL_SGIX_flush_raster" extensions
{-# NOINLINE gl_SGIX_flush_raster #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/fog_offset.txt SGIX_fog_offset> extension supported?
glGetSGIXFogOffset :: MonadIO m => m Bool
glGetSGIXFogOffset = getExtensions >>= (return . member "GL_SGIX_fog_offset")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/fog_offset.txt SGIX_fog_offset> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXFogOffset' in those cases instead.
gl_SGIX_fog_offset :: Bool
gl_SGIX_fog_offset = member "GL_SGIX_fog_offset" extensions
{-# NOINLINE gl_SGIX_fog_offset #-}

-- | Is the <https://www.opengl.org/registry/specs/EXT/fragment_lighting.txt SGIX_fragment_lighting> extension supported?
glGetSGIXFragmentLighting :: MonadIO m => m Bool
glGetSGIXFragmentLighting = getExtensions >>= (return . member "GL_SGIX_fragment_lighting")

-- | Is the <https://www.opengl.org/registry/specs/EXT/fragment_lighting.txt SGIX_fragment_lighting> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXFragmentLighting' in those cases instead.
gl_SGIX_fragment_lighting :: Bool
gl_SGIX_fragment_lighting = member "GL_SGIX_fragment_lighting" extensions
{-# NOINLINE gl_SGIX_fragment_lighting #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/framezoom.txt SGIX_framezoom> extension supported?
glGetSGIXFramezoom :: MonadIO m => m Bool
glGetSGIXFramezoom = getExtensions >>= (return . member "GL_SGIX_framezoom")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/framezoom.txt SGIX_framezoom> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXFramezoom' in those cases instead.
gl_SGIX_framezoom :: Bool
gl_SGIX_framezoom = member "GL_SGIX_framezoom" extensions
{-# NOINLINE gl_SGIX_framezoom #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/igloo_interface.txt SGIX_igloo_interface> extension supported?
glGetSGIXIglooInterface :: MonadIO m => m Bool
glGetSGIXIglooInterface = getExtensions >>= (return . member "GL_SGIX_igloo_interface")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/igloo_interface.txt SGIX_igloo_interface> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXIglooInterface' in those cases instead.
gl_SGIX_igloo_interface :: Bool
gl_SGIX_igloo_interface = member "GL_SGIX_igloo_interface" extensions
{-# NOINLINE gl_SGIX_igloo_interface #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/instruments.txt SGIX_instruments> extension supported?
glGetSGIXInstruments :: MonadIO m => m Bool
glGetSGIXInstruments = getExtensions >>= (return . member "GL_SGIX_instruments")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/instruments.txt SGIX_instruments> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXInstruments' in those cases instead.
gl_SGIX_instruments :: Bool
gl_SGIX_instruments = member "GL_SGIX_instruments" extensions
{-# NOINLINE gl_SGIX_instruments #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/interlace.txt SGIX_interlace> extension supported?
glGetSGIXInterlace :: MonadIO m => m Bool
glGetSGIXInterlace = getExtensions >>= (return . member "GL_SGIX_interlace")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/interlace.txt SGIX_interlace> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXInterlace' in those cases instead.
gl_SGIX_interlace :: Bool
gl_SGIX_interlace = member "GL_SGIX_interlace" extensions
{-# NOINLINE gl_SGIX_interlace #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/ir_instrument1.txt SGIX_ir_instrument1> extension supported?
glGetSGIXIrInstrument1 :: MonadIO m => m Bool
glGetSGIXIrInstrument1 = getExtensions >>= (return . member "GL_SGIX_ir_instrument1")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/ir_instrument1.txt SGIX_ir_instrument1> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXIrInstrument1' in those cases instead.
gl_SGIX_ir_instrument1 :: Bool
gl_SGIX_ir_instrument1 = member "GL_SGIX_ir_instrument1" extensions
{-# NOINLINE gl_SGIX_ir_instrument1 #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/list_priority.txt SGIX_list_priority> extension supported?
glGetSGIXListPriority :: MonadIO m => m Bool
glGetSGIXListPriority = getExtensions >>= (return . member "GL_SGIX_list_priority")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/list_priority.txt SGIX_list_priority> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXListPriority' in those cases instead.
gl_SGIX_list_priority :: Bool
gl_SGIX_list_priority = member "GL_SGIX_list_priority" extensions
{-# NOINLINE gl_SGIX_list_priority #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/sgix_pixel_texture.txt SGIX_pixel_texture> extension supported?
glGetSGIXPixelTexture :: MonadIO m => m Bool
glGetSGIXPixelTexture = getExtensions >>= (return . member "GL_SGIX_pixel_texture")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/sgix_pixel_texture.txt SGIX_pixel_texture> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXPixelTexture' in those cases instead.
gl_SGIX_pixel_texture :: Bool
gl_SGIX_pixel_texture = member "GL_SGIX_pixel_texture" extensions
{-# NOINLINE gl_SGIX_pixel_texture #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/pixel_tiles.txt SGIX_pixel_tiles> extension supported?
glGetSGIXPixelTiles :: MonadIO m => m Bool
glGetSGIXPixelTiles = getExtensions >>= (return . member "GL_SGIX_pixel_tiles")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/pixel_tiles.txt SGIX_pixel_tiles> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXPixelTiles' in those cases instead.
gl_SGIX_pixel_tiles :: Bool
gl_SGIX_pixel_tiles = member "GL_SGIX_pixel_tiles" extensions
{-# NOINLINE gl_SGIX_pixel_tiles #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/polynomial_ffd.txt SGIX_polynomial_ffd> extension supported?
glGetSGIXPolynomialFFD :: MonadIO m => m Bool
glGetSGIXPolynomialFFD = getExtensions >>= (return . member "GL_SGIX_polynomial_ffd")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/polynomial_ffd.txt SGIX_polynomial_ffd> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXPolynomialFFD' in those cases instead.
gl_SGIX_polynomial_ffd :: Bool
gl_SGIX_polynomial_ffd = member "GL_SGIX_polynomial_ffd" extensions
{-# NOINLINE gl_SGIX_polynomial_ffd #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/reference_plane.txt SGIX_reference_plane> extension supported?
glGetSGIXReferencePlane :: MonadIO m => m Bool
glGetSGIXReferencePlane = getExtensions >>= (return . member "GL_SGIX_reference_plane")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/reference_plane.txt SGIX_reference_plane> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXReferencePlane' in those cases instead.
gl_SGIX_reference_plane :: Bool
gl_SGIX_reference_plane = member "GL_SGIX_reference_plane" extensions
{-# NOINLINE gl_SGIX_reference_plane #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/resample.txt SGIX_resample> extension supported?
glGetSGIXResample :: MonadIO m => m Bool
glGetSGIXResample = getExtensions >>= (return . member "GL_SGIX_resample")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/resample.txt SGIX_resample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXResample' in those cases instead.
gl_SGIX_resample :: Bool
gl_SGIX_resample = member "GL_SGIX_resample" extensions
{-# NOINLINE gl_SGIX_resample #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/scalebias_hint.txt SGIX_scalebias_hint> extension supported?
glGetSGIXScalebiasHint :: MonadIO m => m Bool
glGetSGIXScalebiasHint = getExtensions >>= (return . member "GL_SGIX_scalebias_hint")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/scalebias_hint.txt SGIX_scalebias_hint> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXScalebiasHint' in those cases instead.
gl_SGIX_scalebias_hint :: Bool
gl_SGIX_scalebias_hint = member "GL_SGIX_scalebias_hint" extensions
{-# NOINLINE gl_SGIX_scalebias_hint #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/shadow.txt SGIX_shadow> extension supported?
glGetSGIXShadow :: MonadIO m => m Bool
glGetSGIXShadow = getExtensions >>= (return . member "GL_SGIX_shadow")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/shadow.txt SGIX_shadow> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXShadow' in those cases instead.
gl_SGIX_shadow :: Bool
gl_SGIX_shadow = member "GL_SGIX_shadow" extensions
{-# NOINLINE gl_SGIX_shadow #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/shadow_ambient.txt SGIX_shadow_ambient> extension supported?
glGetSGIXShadowAmbient :: MonadIO m => m Bool
glGetSGIXShadowAmbient = getExtensions >>= (return . member "GL_SGIX_shadow_ambient")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/shadow_ambient.txt SGIX_shadow_ambient> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXShadowAmbient' in those cases instead.
gl_SGIX_shadow_ambient :: Bool
gl_SGIX_shadow_ambient = member "GL_SGIX_shadow_ambient" extensions
{-# NOINLINE gl_SGIX_shadow_ambient #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/sprite.txt SGIX_sprite> extension supported?
glGetSGIXSprite :: MonadIO m => m Bool
glGetSGIXSprite = getExtensions >>= (return . member "GL_SGIX_sprite")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/sprite.txt SGIX_sprite> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXSprite' in those cases instead.
gl_SGIX_sprite :: Bool
gl_SGIX_sprite = member "GL_SGIX_sprite" extensions
{-# NOINLINE gl_SGIX_sprite #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/subsample.txt SGIX_subsample> extension supported?
glGetSGIXSubsample :: MonadIO m => m Bool
glGetSGIXSubsample = getExtensions >>= (return . member "GL_SGIX_subsample")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/subsample.txt SGIX_subsample> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXSubsample' in those cases instead.
gl_SGIX_subsample :: Bool
gl_SGIX_subsample = member "GL_SGIX_subsample" extensions
{-# NOINLINE gl_SGIX_subsample #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/tag_sample_buffer.txt SGIX_tag_sample_buffer> extension supported?
glGetSGIXTagSampleBuffer :: MonadIO m => m Bool
glGetSGIXTagSampleBuffer = getExtensions >>= (return . member "GL_SGIX_tag_sample_buffer")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/tag_sample_buffer.txt SGIX_tag_sample_buffer> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXTagSampleBuffer' in those cases instead.
gl_SGIX_tag_sample_buffer :: Bool
gl_SGIX_tag_sample_buffer = member "GL_SGIX_tag_sample_buffer" extensions
{-# NOINLINE gl_SGIX_tag_sample_buffer #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/texture_env_add.txt SGIX_texture_add_env> extension supported?
glGetSGIXTextureAddEnv :: MonadIO m => m Bool
glGetSGIXTextureAddEnv = getExtensions >>= (return . member "GL_SGIX_texture_add_env")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/texture_env_add.txt SGIX_texture_add_env> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXTextureAddEnv' in those cases instead.
gl_SGIX_texture_add_env :: Bool
gl_SGIX_texture_add_env = member "GL_SGIX_texture_add_env" extensions
{-# NOINLINE gl_SGIX_texture_add_env #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/texture_coordinate_clamp.txt SGIX_texture_coordinate_clamp> extension supported?
glGetSGIXTextureCoordinateClamp :: MonadIO m => m Bool
glGetSGIXTextureCoordinateClamp = getExtensions >>= (return . member "GL_SGIX_texture_coordinate_clamp")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/texture_coordinate_clamp.txt SGIX_texture_coordinate_clamp> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXTextureCoordinateClamp' in those cases instead.
gl_SGIX_texture_coordinate_clamp :: Bool
gl_SGIX_texture_coordinate_clamp = member "GL_SGIX_texture_coordinate_clamp" extensions
{-# NOINLINE gl_SGIX_texture_coordinate_clamp #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/texture_lod_bias.txt SGIX_texture_lod_bias> extension supported?
glGetSGIXTextureLODBias :: MonadIO m => m Bool
glGetSGIXTextureLODBias = getExtensions >>= (return . member "GL_SGIX_texture_lod_bias")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/texture_lod_bias.txt SGIX_texture_lod_bias> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXTextureLODBias' in those cases instead.
gl_SGIX_texture_lod_bias :: Bool
gl_SGIX_texture_lod_bias = member "GL_SGIX_texture_lod_bias" extensions
{-# NOINLINE gl_SGIX_texture_lod_bias #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/texture_multi_buffer.txt SGIX_texture_multi_buffer> extension supported?
glGetSGIXTextureMultiBuffer :: MonadIO m => m Bool
glGetSGIXTextureMultiBuffer = getExtensions >>= (return . member "GL_SGIX_texture_multi_buffer")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/texture_multi_buffer.txt SGIX_texture_multi_buffer> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXTextureMultiBuffer' in those cases instead.
gl_SGIX_texture_multi_buffer :: Bool
gl_SGIX_texture_multi_buffer = member "GL_SGIX_texture_multi_buffer" extensions
{-# NOINLINE gl_SGIX_texture_multi_buffer #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/texture_scale_bias.txt SGIX_texture_scale_bias> extension supported?
glGetSGIXTextureScaleBias :: MonadIO m => m Bool
glGetSGIXTextureScaleBias = getExtensions >>= (return . member "GL_SGIX_texture_scale_bias")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/texture_scale_bias.txt SGIX_texture_scale_bias> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXTextureScaleBias' in those cases instead.
gl_SGIX_texture_scale_bias :: Bool
gl_SGIX_texture_scale_bias = member "GL_SGIX_texture_scale_bias" extensions
{-# NOINLINE gl_SGIX_texture_scale_bias #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/vertex_preclip.txt SGIX_vertex_preclip> extension supported?
glGetSGIXVertexPreclip :: MonadIO m => m Bool
glGetSGIXVertexPreclip = getExtensions >>= (return . member "GL_SGIX_vertex_preclip")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/vertex_preclip.txt SGIX_vertex_preclip> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXVertexPreclip' in those cases instead.
gl_SGIX_vertex_preclip :: Bool
gl_SGIX_vertex_preclip = member "GL_SGIX_vertex_preclip" extensions
{-# NOINLINE gl_SGIX_vertex_preclip #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/ycrcb.txt SGIX_ycrcb> extension supported?
glGetSGIXYCrCb :: MonadIO m => m Bool
glGetSGIXYCrCb = getExtensions >>= (return . member "GL_SGIX_ycrcb")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/ycrcb.txt SGIX_ycrcb> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXYCrCb' in those cases instead.
gl_SGIX_ycrcb :: Bool
gl_SGIX_ycrcb = member "GL_SGIX_ycrcb" extensions
{-# NOINLINE gl_SGIX_ycrcb #-}

-- | Is the <https://www.opengl.org/registry/specs/SGIX/ycrcba.txt SGIX_ycrcba> extension supported?
glGetSGIXYCrCbA :: MonadIO m => m Bool
glGetSGIXYCrCbA = getExtensions >>= (return . member "GL_SGIX_ycrcba")

-- | Is the <https://www.opengl.org/registry/specs/SGIX/ycrcba.txt SGIX_ycrcba> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSGIXYCrCbA' in those cases instead.
gl_SGIX_ycrcba :: Bool
gl_SGIX_ycrcba = member "GL_SGIX_ycrcba" extensions
{-# NOINLINE gl_SGIX_ycrcba #-}

-- | Is the <https://www.opengl.org/registry/specs/SUN/convolution_border_modes.txt SUN_convolution_border_modes> extension supported?
glGetSUNConvolutionBorderModes :: MonadIO m => m Bool
glGetSUNConvolutionBorderModes = getExtensions >>= (return . member "GL_SUN_convolution_border_modes")

-- | Is the <https://www.opengl.org/registry/specs/SUN/convolution_border_modes.txt SUN_convolution_border_modes> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSUNConvolutionBorderModes' in those cases instead.
gl_SUN_convolution_border_modes :: Bool
gl_SUN_convolution_border_modes = member "GL_SUN_convolution_border_modes" extensions
{-# NOINLINE gl_SUN_convolution_border_modes #-}

-- | Is the <https://www.opengl.org/registry/specs/SUN/global_alpha.txt SUN_global_alpha> extension supported?
glGetSUNGlobalAlpha :: MonadIO m => m Bool
glGetSUNGlobalAlpha = getExtensions >>= (return . member "GL_SUN_global_alpha")

-- | Is the <https://www.opengl.org/registry/specs/SUN/global_alpha.txt SUN_global_alpha> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSUNGlobalAlpha' in those cases instead.
gl_SUN_global_alpha :: Bool
gl_SUN_global_alpha = member "GL_SUN_global_alpha" extensions
{-# NOINLINE gl_SUN_global_alpha #-}

-- | Is the <https://www.opengl.org/registry/specs/SUN/mesh_array.txt SUN_mesh_array> extension supported?
glGetSUNMeshArray :: MonadIO m => m Bool
glGetSUNMeshArray = getExtensions >>= (return . member "GL_SUN_mesh_array")

-- | Is the <https://www.opengl.org/registry/specs/SUN/mesh_array.txt SUN_mesh_array> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSUNMeshArray' in those cases instead.
gl_SUN_mesh_array :: Bool
gl_SUN_mesh_array = member "GL_SUN_mesh_array" extensions
{-# NOINLINE gl_SUN_mesh_array #-}

-- | Is the <https://www.opengl.org/registry/specs/SUN/slice_accum.txt SUN_slice_accum> extension supported?
glGetSUNSliceAccum :: MonadIO m => m Bool
glGetSUNSliceAccum = getExtensions >>= (return . member "GL_SUN_slice_accum")

-- | Is the <https://www.opengl.org/registry/specs/SUN/slice_accum.txt SUN_slice_accum> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSUNSliceAccum' in those cases instead.
gl_SUN_slice_accum :: Bool
gl_SUN_slice_accum = member "GL_SUN_slice_accum" extensions
{-# NOINLINE gl_SUN_slice_accum #-}

-- | Is the <https://www.opengl.org/registry/specs/SUN/triangle_list.txt SUN_triangle_list> extension supported?
glGetSUNTriangleList :: MonadIO m => m Bool
glGetSUNTriangleList = getExtensions >>= (return . member "GL_SUN_triangle_list")

-- | Is the <https://www.opengl.org/registry/specs/SUN/triangle_list.txt SUN_triangle_list> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSUNTriangleList' in those cases instead.
gl_SUN_triangle_list :: Bool
gl_SUN_triangle_list = member "GL_SUN_triangle_list" extensions
{-# NOINLINE gl_SUN_triangle_list #-}

-- | Is the <https://www.opengl.org/registry/specs/SUN/vertex.txt SUN_vertex> extension supported?
glGetSUNVertex :: MonadIO m => m Bool
glGetSUNVertex = getExtensions >>= (return . member "GL_SUN_vertex")

-- | Is the <https://www.opengl.org/registry/specs/SUN/vertex.txt SUN_vertex> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSUNVertex' in those cases instead.
gl_SUN_vertex :: Bool
gl_SUN_vertex = member "GL_SUN_vertex" extensions
{-# NOINLINE gl_SUN_vertex #-}

-- | Is the <https://www.opengl.org/registry/specs/SUNX/constant_data.txt SUNX_constant_data> extension supported?
glGetSUNXConstantData :: MonadIO m => m Bool
glGetSUNXConstantData = getExtensions >>= (return . member "GL_SUNX_constant_data")

-- | Is the <https://www.opengl.org/registry/specs/SUNX/constant_data.txt SUNX_constant_data> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetSUNXConstantData' in those cases instead.
gl_SUNX_constant_data :: Bool
gl_SUNX_constant_data = member "GL_SUNX_constant_data" extensions
{-# NOINLINE gl_SUNX_constant_data #-}

-- | Is the <https://www.opengl.org/registry/specs/WIN/phong_shading.txt WIN_phong_shading> extension supported?
glGetWINPhongShading :: MonadIO m => m Bool
glGetWINPhongShading = getExtensions >>= (return . member "GL_WIN_phong_shading")

-- | Is the <https://www.opengl.org/registry/specs/WIN/phong_shading.txt WIN_phong_shading> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetWINPhongShading' in those cases instead.
gl_WIN_phong_shading :: Bool
gl_WIN_phong_shading = member "GL_WIN_phong_shading" extensions
{-# NOINLINE gl_WIN_phong_shading #-}

-- | Is the <https://www.opengl.org/registry/specs/WIN/specular_fog.txt WIN_specular_fog> extension supported?
glGetWINSpecularFog :: MonadIO m => m Bool
glGetWINSpecularFog = getExtensions >>= (return . member "GL_WIN_specular_fog")

-- | Is the <https://www.opengl.org/registry/specs/WIN/specular_fog.txt WIN_specular_fog> extension supported?
-- Note that in the presence of multiple contexts with different capabilities,
-- this might be wrong. Use 'glGetWINSpecularFog' in those cases instead.
gl_WIN_specular_fog :: Bool
gl_WIN_specular_fog = member "GL_WIN_specular_fog" extensions
{-# NOINLINE gl_WIN_specular_fog #-}
