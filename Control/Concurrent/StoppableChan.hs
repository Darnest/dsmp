module Control.Concurrent.StoppableChan
	( SChan
	, newSChan
	, writeSChan
	, readSChan
	, isEmptySChan
	, getSChanContents
	, writeList2SChan
	, closeSChan
	, isClosedSChan
	) where

import Control.Concurrent.MVar
import Control.Monad
import System.IO.Unsafe (unsafeInterleaveIO)

data SChanItem a
	= SChanItem a (MVar (SChanItem a))
	| Closed
data SChan a = SChan (MVar (MVar (SChanItem a))) (MVar (Maybe (MVar (SChanItem a))))

newSChan :: IO (SChan a)
newSChan = do
	p <- newEmptyMVar
	m <- newMVar p
	n <- newMVar (Just p)
	return (SChan m n)

writeSChan :: SChan a -> a -> IO Bool
writeSChan (SChan _ m) a = do
	mr <- takeMVar m
	case mr of
		(Just n) -> do
			p <- newEmptyMVar
			putMVar m (Just p)
			putMVar n (SChanItem a p)
			return True
		Nothing -> do
			putMVar m mr
			return False

readSChan :: SChan a -> IO (Maybe a)
readSChan (SChan p _ ) = do
	m <- takeMVar p
	mr <- takeMVar m
	case mr of
		(SChanItem a n) -> do
			putMVar p n
			return (Just a)
		Closed -> do
			putMVar m mr
			putMVar p m
			return Nothing

closeSChan :: SChan a -> IO Bool
closeSChan (SChan _ m) = do
	mr <- takeMVar m
	case mr of
		(Just n) -> do
			p <- newEmptyMVar
			putMVar m Nothing
			putMVar n Closed
			return True
		Nothing -> do
			putMVar m mr
			return False

isEmptySChan :: SChan a -> IO Bool
isEmptySChan (SChan p _ ) = do
	m <- takeMVar p
	mr <- tryTakeMVar m
	putMVar p m
	case mr of
		(Just mmr) -> do
			putMVar m mmr
			case mmr of
				(SChanItem a n) -> return False
				Closed -> return True
		Nothing -> return True

isClosedSChan :: SChan a -> IO Bool
isClosedSChan (SChan _ m) = do
	mr <- readMVar m
	case mr of
		(Just n) -> return True
		Nothing -> return False

getSChanContents :: SChan a -> IO [a]
getSChanContents (SChan p _) = do
	m <- takeMVar p
	unsafeInterleaveIO $ readchan p m
	where
		readchan :: (MVar (MVar (SChanItem a))) -> (MVar (SChanItem a)) -> IO [a]
		readchan p m = do
			mr <- takeMVar m
			case mr of
				(SChanItem a n) -> do
					b <- unsafeInterleaveIO $ readchan p n
					return (a:b)
				Closed -> do
					putMVar m mr
					putMVar p m
					return []
		

writeList2SChan :: SChan a -> [a] -> IO ()
writeList2SChan schan list = do
	mapM_ (\x -> do
		writeSChan schan x) list