{-# LANGUAGE UndecidableInstances #-}
module Minecraft.Network.Client.ClientActionResult
	( ClientActionResult (..)
	, ClientActionT
	, clientActionResultT
	, failedClientActionResultT
	, runClientActionT
	) where
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Maybe

data ClientActionResult a = ClientActionSuccessful a | ClientDisconnected String | ClientKicked String | UnexpectedClientDisconnect String
	deriving (Eq, Show)

clientActionResultT :: Monad m => ClientActionResult a -> ClientActionT m a
clientActionResultT = ClientActionT . return

failedClientActionResultT :: Monad m => ClientActionResult a -> ClientActionT m b
failedClientActionResultT = clientActionResultT . coerceActionResult

coerceActionResult :: ClientActionResult a -> ClientActionResult b
coerceActionResult (ClientDisconnected s) = (ClientDisconnected s)
coerceActionResult (ClientKicked s) = (ClientKicked s)
coerceActionResult (UnexpectedClientDisconnect s) = (UnexpectedClientDisconnect s)

coerceActionT :: Monad m => ClientActionT m a -> ClientActionT m b
coerceActionT (ClientActionT x) = ClientActionT (x >>= return . coerceActionResult)

tryClientActionResultM :: Monad m => (a -> ClientActionT m b) -> ClientActionResult a -> m (ClientActionResult b)
tryClientActionResultM f x = do
	case x of
		(ClientActionSuccessful y) -> runClientActionT $ f y
		_ -> return $ coerceActionResult x
	
instance Monad ClientActionResult where
	(ClientActionSuccessful x) >>= k = k x
	res >>= _ = coerceActionResult res
	
	(ClientActionSuccessful x) >> k = k
	res >> _ = coerceActionResult res
	
	return = ClientActionSuccessful
	fail s = UnexpectedClientDisconnect s

instance Functor ClientActionResult where
	fmap f (ClientActionSuccessful x) = ClientActionSuccessful $ f x
	fmap _ res = coerceActionResult res

newtype Monad m => ClientActionT m a
	= ClientActionT {runClientActionT :: m (ClientActionResult a)}

instance MonadTrans ClientActionT where
	lift = ClientActionT . (liftM ClientActionSuccessful)

instance Monad m => Monad (ClientActionT m) where
	fail s = ClientActionT $ return $ UnexpectedClientDisconnect s
	return = lift . return
	x >>= f = ClientActionT (runClientActionT x >>= tryClientActionResultM f)

instance MonadIO m => MonadIO (ClientActionT m) where
	liftIO = lift . liftIO