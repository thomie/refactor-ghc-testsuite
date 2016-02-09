module Utils where

-- http://hackage.haskell.org/package/monad-loops
-- import Control.Monad.Loops
unfoldWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
unfoldWhileM p m = loop id
    where
        loop f = do
            x <- m
            if p x
                then loop (f . (x:))
                else return (f [])
