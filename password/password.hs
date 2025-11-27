module Password where

newtype PwdOp a = PwdOp { run :: String -> (a, String) }

-- instance definitions for Functor, Applicative, and Monad
instance Functor PwdOp where
  fmap transform (PwdOp operation) = PwdOp $ \currentPwd ->
      let (result, newPwd) = operation currentPwd
      in (transform result, newPwd)

instance Applicative PwdOp where
  pure value = PwdOp $ \currentPwd -> (value, currentPwd)
  (PwdOp funcOp) <*> (PwdOp valOp) = PwdOp $ \currentPwd ->
      let (func, intermediatePwd) = funcOp currentPwd
          (value, finalPwd) = valOp intermediatePwd
      in (func value, finalPwd)

instance Monad PwdOp where
  return = pure
  (PwdOp operation) >>= bindFunc = PwdOp $ \currentPwd ->
      let (result, intermediatePwd) = operation currentPwd
          PwdOp nextOperation = bindFunc result
      in nextOperation intermediatePwd

setPassword :: String -> PwdOp ()
setPassword newPwd = PwdOp $ \_ -> ((), newPwd)

checkPassword :: String -> PwdOp Bool
checkPassword attempt = PwdOp $ \storedPwd -> (attempt == storedPwd, storedPwd)

runPwdOp :: PwdOp a -> a
runPwdOp (PwdOp operation) = fst (operation "")





