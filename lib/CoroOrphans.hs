module CoroOrphans where


import Control.Monad.Coroutine
import Control.Monad.Coroutine.Nested
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Trans
import Control.Monad.State.Class
import Control.Monad.Reader.Class


instance (Functor s', MonadState s m) => MonadState s (Coroutine s' m) where
  state f = lift (state f)

instance (Functor s', MonadReader r m) => MonadReader r (Coroutine s' m) where
  ask = lift ask
  reader f = lift $ reader f

