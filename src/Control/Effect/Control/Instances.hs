{-# LANGUAGE StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, TupleSections, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Control.Instances
  ( module Control.Monad.Base
  , module Control.Monad.Trans.Control
  ) where

import Control.Monad
import Control.Effect
import Control.Effect.Reader
import Control.Effect.Random
import Control.Effect.Error
import Control.Effect.Fail
import Control.Effect.Fresh
import Control.Effect.Lift
import Control.Effect.Writer
import Control.Effect.Resumable
import Control.Effect.Trace
import Control.Monad.Trans.Control
import Control.Monad.Base
import qualified Control.Effect.State.Strict as Strict
import qualified Control.Effect.State.Lazy as Lazy

instance MonadBase PureC PureC where liftBase = id

-- Reader

instance MonadBase b m => MonadBase b (ReaderC r m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadTransControl (ReaderC r) where
  type StT (ReaderC r) a = a
  liftWith f = ReaderC $ \r -> f (runReader r)
  restoreT   = ReaderC . const
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (ReaderC r m) where
  type StM (ReaderC r m) a = ComposeSt (ReaderC r) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

-- Error

instance MonadBase b m => MonadBase b (ErrorC e m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadTransControl (ErrorC e) where
  type StT (ErrorC e) a = Either e a
  liftWith f = ErrorC . liftM pure $ f runError
  restoreT   = ErrorC
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (ErrorC e m) where
  type StM (ErrorC e m) a = ComposeSt (ErrorC e) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

-- Fail (derived from Error)

deriving instance MonadTransControl FailC

-- Strict State

instance MonadBase b m => MonadBase b (Strict.StateC e m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadTransControl (Strict.StateC s) where
  type StT (Strict.StateC s) a = (s, a)
  liftWith f = Strict.StateC $ \s -> liftM (s,) (f (Strict.runState s))
  restoreT   = Strict.StateC . const
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (Strict.StateC s m) where
  type StM (Strict.StateC s m) a = ComposeSt (Strict.StateC s) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

-- Writer, Fresh, and Random (derived from strict State)

deriving instance MonadBase b m => MonadBase b (WriterC w m)
deriving instance MonadTransControl (WriterC w)
deriving instance MonadBaseControl b m => MonadBaseControl b (WriterC w m)

deriving instance MonadBase b m => MonadBase b (FreshC m)
deriving instance MonadTransControl FreshC
deriving instance MonadBaseControl b m => MonadBaseControl b (FreshC m)

deriving instance MonadBase b m => MonadBase b (RandomC g m)
deriving instance MonadTransControl (RandomC g)
deriving instance MonadBaseControl b m => MonadBaseControl b (RandomC g m)

-- Lazy State

instance MonadBase b m => MonadBase b (Lazy.StateC e m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadTransControl (Lazy.StateC s) where
  type StT (Lazy.StateC s) a = (s, a)
  liftWith f = Lazy.StateC $ \s -> liftM (s,) (f (Lazy.runState s))
  restoreT   = Lazy.StateC . const
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (Lazy.StateC s m) where
  type StM (Lazy.StateC s m) a = ComposeSt (Lazy.StateC s) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

-- Printed tracing
instance MonadBase b m => MonadBase b (TraceByPrintingC m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadTransControl TraceByPrintingC where
  type StT TraceByPrintingC a = a
  liftWith f = TraceByPrintingC $ f runTraceByPrinting
  restoreT   = TraceByPrintingC
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (TraceByPrintingC m) where
  type StM (TraceByPrintingC m) a = ComposeSt TraceByPrintingC m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

-- Ignored tracing
instance MonadBase b m => MonadBase b (TraceByIgnoringC m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadTransControl TraceByIgnoringC where
  type StT TraceByIgnoringC a = a
  liftWith f = TraceByIgnoringC $ f runTraceByIgnoring
  restoreT   = TraceByIgnoringC
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (TraceByIgnoringC m) where
  type StM (TraceByIgnoringC m) a = ComposeSt TraceByIgnoringC m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

-- Accumulated tracing
deriving instance MonadBase b m => MonadBase b (TraceByReturningC m)
deriving instance MonadTransControl TraceByReturningC
deriving instance MonadBaseControl b m => MonadBaseControl b (TraceByReturningC m)

-- Resumable exceptions (does this work??)
deriving instance MonadBase b m => MonadBase b (ResumableC err m)
deriving instance MonadTransControl (ResumableC err)
deriving instance MonadBaseControl b m => MonadBaseControl b (ResumableC err m)

-- Lift

instance MonadBase b m => MonadBase b (LiftC m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadTransControl LiftC where
  type StT LiftC a = a
  liftWith f = LiftC $ f runM
  restoreT   = LiftC
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (LiftC m) where
  type StM (LiftC m) a = ComposeSt LiftC m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

-- TODO: NonDet?
