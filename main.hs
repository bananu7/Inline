import System.Random
import Control.Monad.Trans.State
import Control.Monad.Trans
import Data.Map
import Data.Maybe

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

noSuchCommand :: Command
noSuchCommand = liftIO $ putStrLn "Permission denied"

commands :: Commands
commands = fromList [
    ("ls", ls)
    ]

loop :: Sys -> IO ()
loop sys = do
    inp <- getLine

    let cmd = if isJust lookCmd then fromJust lookCmd
                                else noSuchCommand
              where lookCmd = Data.Map.lookup inp commands

    runStateT cmd sys
--    let sys' = fst result
 --   liftIO $ snd result
    loop sys

main = do
    let sys = Sys 0 (mkStdGen 42)
    putStrLn "You find yourself awake in a dark room. There is nothing"
    putStrLn "but an old computer terminal inside. Your goal is to gain"
    putStrLn "the root access."
    putStrLn "Have fun!"
    loop sys
    print "Thanks for playing :)"
