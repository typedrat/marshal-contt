# marshal-contt

[![Hackage Version](http://img.shields.io/hackage/v/marshal-contt.svg)](http://hackage.haskell.org/package/marshal-contt)

A ContT-based wrapper for the bracket-style Haskell-to-C marshalling functions in `base` and `bytestring`.

## Example:

```haskell
type Argv = [BS.ByteString]
type Env  = M.Map BS.ByteString BS.ByteString

allocStringArrayWith :: [BS.ByteString] -> ContT r IO (Ptr CString)
allocStringArrayWith = allocaArrayWith0' withCString

allocStringMapWith :: M.Map BS.ByteString BS.ByteString -> (Ptr CString -> IO a) -> IO a
allocStringMapWith = iallocaArrayWith0' (\(k, v) -> withCString $ k <> "=" <> v)

spawn :: FilePath -> [FileActions] -> Argv -> Env -> IO ProcessID
spawn path actions argv env = runContT return $ do
    cPath <- withCString path
    cAction <- withFileActions actions
    cArgv <- allocStringArrayWith argv
    cEnv <- marshalEnv env
    pidPtr <- alloca
    liftIO . throwErrnoIf_ (/= 0) path $ 
        posix_spawn pidPtr cPath cActions nullSpawnAttrs cArgv cEnv
    liftIO $ peek pidPtr

spawnp :: String -> [FileActions] -> Argv -> Env -> IO ProcessID
spawnp command actions argv env = runContT return $ do
    cPath <- withCString path
    cAction <- withFileActions actions
    cArgv <- allocStringArrayWith argv
    cEnv <- marshalEnv env
    pidPtr <- alloca
    liftIO . throwErrnoIf_ (/= 0) path $ 
        posix_spawnp pidPtr cPath cActions nullSpawnAttrs cArgv cEnv
    liftIO $ peek pidPtr
```
