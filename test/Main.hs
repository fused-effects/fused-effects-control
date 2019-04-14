{-# LANGUAGE FlexibleContexts, RankNTypes, TypeApplications #-}

module Main where

import           Control.Effect
import           Control.Effect.Control.Instances
import           Control.Effect.Reader
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Either
import           Test.Hspec

try :: forall e m a . (MonadBaseControl IO m, E.Exception e) => m a -> m (Either e a)
try m = liftBaseWith (\runInIO -> E.try (runInIO m)) >>= either (return . Left) (liftM Right . restoreM)

throwIO :: forall a m e . (MonadBase IO m, E.Exception e) => e -> m a
throwIO = liftBase . E.throwIO

main :: IO ()
main = hspec $
  describe "try + throwIO" $
    it "should return Left after throwIO" $ do
      res <- liftIO . runM . runReader "foo" . try @E.SomeException . throwIO @Int $ userError "dead"
      res `shouldSatisfy` isLeft
