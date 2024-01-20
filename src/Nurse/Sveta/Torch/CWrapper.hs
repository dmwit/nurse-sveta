module Nurse.Sveta.Torch.CWrapper where

import Foreign
import Foreign.C

class CWrapper a where
	type Unwrapped a
	withUnwrapped :: a -> (Unwrapped a -> IO b) -> IO b

instance CWrapper (ForeignPtr a) where
	type Unwrapped (ForeignPtr a) = Ptr a
	withUnwrapped = withForeignPtr

instance CWrapper () where
	type Unwrapped () = ()
	withUnwrapped _ f = f ()

instance (CWrapper a, CWrapper b) => CWrapper (a, b) where
	type Unwrapped (a, b) = (Unwrapped a, Unwrapped b)
	withUnwrapped (wa, wb) f = withUnwrapped wa $ \a -> withUnwrapped wb $ \b -> f (a, b)

instance CWrapper a => CWrapper [a] where
	type Unwrapped [a] = [Unwrapped a]
	withUnwrapped = withMany withUnwrapped

newtype WrappedCString = WCS String
instance CWrapper WrappedCString where
	type Unwrapped WrappedCString = CString
	withUnwrapped (WCS s) = withCString s
