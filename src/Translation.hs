module Translation where

import qualified Environment as Env
import TigerTypes (Expression)

type IL = ()

translate :: Env.ValEnv -> Expression -> IL
translate _ _ = ()
