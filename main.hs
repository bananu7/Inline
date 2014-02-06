import System.Random
import Control.Monad.Trans.State
import Control.Monad.Trans
import Data.Map

type SysState = Int
data Sys = Sys {
    state :: SysState,
    gen :: StdGen
}

type Command = StateT Sys IO ()
type Commands = Map String Command

ls :: Command
ls = do
    liftIO $ putStrLn "ls"

--commands :: [Command]

loop = do
    inp <- getLine
    putStrLn inp
    loop

main = do
    let sys = Sys 0 (mkStdGen 42)
    putStrLn "Your goal is to gain root access."
    putStrLn "Have fun!"
    loop
    print "Thanks for playing :)"
