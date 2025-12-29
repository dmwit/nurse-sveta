module Ms.Mendel.CXX.Raw where

import Foreign
import Foreign.C

newtype Boards = Boards (ForeignPtr Boards)
foreign import ccall "boards_new" boards_new :: Ptr CChar -> Ptr CChar -> IO (Ptr Boards)
foreign import ccall unsafe "&boards_delete" boards_delete_ptr :: FinalizerPtr Boards
foreign import ccall unsafe "boards_delete" boards_delete :: Ptr Boards -> IO ()
foreign import ccall unsafe "boards_size" boards_size :: Ptr Boards -> IO Int64

foreign import ccall "boards_dump" boards_dump :: Ptr Boards -> IO ()
foreign import ccall "boards_sketch" boards_sketch :: Ptr Boards -> IO ()

newtype PatternsTemplate = PatternsTemplate (ForeignPtr PatternsTemplate)
foreign import ccall "patterns_template_new" patterns_template_new :: Int64 -> Int64 -> Int64 -> IO (Ptr PatternsTemplate)
foreign import ccall unsafe "&patterns_template_delete" patterns_template_delete_ptr :: FinalizerPtr PatternsTemplate
foreign import ccall unsafe "patterns_template_delete" patterns_template_delete :: Ptr PatternsTemplate -> IO ()

foreign import ccall unsafe "patterns_template_conv_width" patterns_template_conv_width :: Ptr PatternsTemplate -> IO Int64
foreign import ccall unsafe "patterns_template_conv_height" patterns_template_conv_height :: Ptr PatternsTemplate -> IO Int64
foreign import ccall unsafe "patterns_template_size" patterns_template_size :: Ptr PatternsTemplate -> IO Int64

foreign import ccall unsafe "patterns_template_get_color_pattern" patterns_template_get_color_pattern :: Ptr PatternsTemplate -> Int64 -> Int64 -> Int64 -> Int64 -> IO Word8
foreign import ccall unsafe "patterns_template_get_shape_pattern" patterns_template_get_shape_pattern :: Ptr PatternsTemplate -> Int64 -> Int64 -> Int64 -> Int64 -> IO Word8
foreign import ccall unsafe "patterns_template_set_color_pattern" patterns_template_set_color_pattern :: Ptr PatternsTemplate -> Int64 -> Int64 -> Int64 -> Int64 -> Word8 -> IO ()
foreign import ccall unsafe "patterns_template_set_shape_pattern" patterns_template_set_shape_pattern :: Ptr PatternsTemplate -> Int64 -> Int64 -> Int64 -> Int64 -> Word8 -> IO ()

foreign import ccall "patterns_template_decode" patterns_template_decode :: Ptr CChar -> Int64 -> IO (Ptr PatternsTemplate)
foreign import ccall "patterns_template_encode" patterns_template_encode :: Ptr PatternsTemplate -> Ptr Int64 -> IO (Ptr CChar)
foreign import ccall unsafe "&patterns_template_encoding_delete" patterns_template_encoding_delete_ptr :: FinalizerPtr CChar
foreign import ccall unsafe "patterns_template_encoding_delete" patterns_template_encoding_delete :: Ptr CChar -> IO ()

foreign import ccall "patterns_template_dump" patterns_template_dump :: Ptr PatternsTemplate -> IO ()
foreign import ccall "patterns_template_sketch" patterns_template_sketch :: Ptr PatternsTemplate -> IO ()

newtype Patterns = Patterns (ForeignPtr Patterns)
foreign import ccall "patterns_new" patterns_new :: Ptr PatternsTemplate -> Word8 -> Word8 -> IO (Ptr Patterns)
foreign import ccall unsafe "&patterns_delete" patterns_delete_ptr :: FinalizerPtr Patterns
foreign import ccall unsafe "patterns_delete" patterns_delete :: Ptr Patterns -> IO ()

foreign import ccall unsafe "patterns_size" patterns_size :: Ptr Patterns -> IO Int64
foreign import ccall unsafe "patterns_conv_width" patterns_conv_width :: Ptr Patterns -> IO Int64
foreign import ccall unsafe "patterns_conv_height" patterns_conv_height :: Ptr Patterns -> IO Int64
foreign import ccall unsafe "patterns_mirroring_size" patterns_mirroring_size :: Ptr Patterns -> IO Int64
foreign import ccall unsafe "patterns_coloring_size" patterns_coloring_size :: Ptr Patterns -> IO Int64
foreign import ccall unsafe "patterns_replication_size" patterns_replication_size :: Ptr Patterns -> IO Int64

foreign import ccall "patterns_dump" patterns_dump :: Ptr Patterns -> IO ()
foreign import ccall "patterns_sketch" patterns_sketch :: Ptr Patterns -> IO ()

-- | like a 'Tensor', but mutable
newtype Scores = Scores Tensor
newtype Tensor = Tensor (ForeignPtr Tensor)
foreign import ccall unsafe "&tensor_delete" tensor_delete_ptr :: FinalizerPtr Tensor
foreign import ccall unsafe "tensor_delete" tensor_delete :: Ptr Tensor -> IO ()

foreign import ccall "float_tensor_to_cpu" float_tensor_to_cpu :: Ptr Tensor -> Ptr Float -> Int64 -> IO ()
foreign import ccall "int_tensor_to_cpu" int_tensor_to_cpu :: Ptr Tensor -> Ptr Int64 -> Int64 -> IO ()
foreign import ccall "tensor_clone" tensor_clone :: Ptr Tensor -> IO (Ptr Tensor)

foreign import ccall "tensor_tanh" tensor_tanh :: Ptr Tensor -> IO (Ptr Tensor)
foreign import ccall "tensor_add" tensor_add :: Ptr Tensor -> Ptr Tensor -> IO (Ptr Tensor)
foreign import ccall "tensor_scale" tensor_scale :: Float -> Ptr Tensor -> IO (Ptr Tensor)

foreign import ccall "scores_new" scores_new :: Ptr Float -> Int64 -> IO (Ptr Tensor)
foreign import ccall unsafe "scores_get" scores_get :: Ptr Tensor -> Int64 -> IO Float
foreign import ccall unsafe "scores_set" scores_set :: Ptr Tensor -> Int64 -> Float -> IO ()

foreign import ccall "match_full" match_full :: Ptr Boards -> Ptr Patterns -> IO (Ptr Tensor)
foreign import ccall "summarize_match" summarize_match :: Ptr Tensor -> IO (Ptr Tensor)
foreign import ccall "score" score :: Ptr Tensor -> Ptr Tensor -> IO (Ptr Tensor)

foreign import ccall "match_summary" match_summary :: Ptr Boards -> Ptr Patterns -> IO (Ptr Tensor)
foreign import ccall "evaluate" evaluate :: Ptr Boards -> Ptr Patterns -> Ptr Tensor -> IO (Ptr Tensor)
foreign import ccall "evaluate_sync" evaluate_sync :: Ptr Boards -> Ptr Patterns -> Ptr Tensor -> Ptr Float -> Int64 -> IO ()

foreign import ccall "tensor_dump" tensor_dump :: Ptr Tensor -> IO ()
foreign import ccall "tensor_sketch" tensor_sketch :: Ptr Tensor -> IO ()
