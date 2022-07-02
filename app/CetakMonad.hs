module CetakMonad where
import Control.Monad.State (StateT, MonadState (get), MonadTrans(lift))

cetakState :: StateT String IO ()
cetakState = do
                state <- get
                lift $ putStrLn (state)