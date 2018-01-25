module Translation where

import qualified Environment as Env
import TigerTypes (Expression)

type IL = ()

type ValEnv = Env.Environment Expression

translate :: ValEnv -> Expression -> IL
translate _ _ = ()
