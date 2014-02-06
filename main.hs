import System.Random
import Control.Monad.Trans.State
import Control.Monad.Trans
import Data.Map
import Data.Maybe
import Data.List.Split

data Sys = Sys {
    files :: Map String String,
    root :: Bool,
    gen :: StdGen
}

type Params = [String]
type Command = Params -> StateT Sys IO ()
type Commands = Map String Command

ls :: Command
ls _ = do
    liftIO $ putStrLn "ls"

cat :: Command
cat params = do
    sys <- get

    let val = params!!0

    if (length params) > 1 then
        if (params!!1 == ">") && (length params > 2) then
            let fName = params!!2
                modFiles = insert fName val (files sys)
                modSys = sys { files = modFiles }
            in do
                put modSys
                return ()
        else 
            liftIO $ putStrLn "Error"
    else
        return ()

noSuchCommand :: Command
noSuchCommand _ = liftIO $ putStrLn "Permission denied"

commands :: Commands
commands = fromList [
    ("ls", ls),
    ("cat", cat)
    ]

loop :: Sys -> IO ()
loop sys = do
    putStr "$ "
    inp <- getLine
    let inpParsed = splitOn " " inp

    let cmd = if isJust lookCmd then fromJust lookCmd
                                else noSuchCommand
              where lookCmd = Data.Map.lookup (head inpParsed) commands

    sys' <- fmap snd $ runStateT (cmd (tail inpParsed)) sys
--    let sys' = fst result
 --   liftIO $ snd result
    loop sys'

main = do
    let sys = Sys (fromList []) False (mkStdGen 42)
    putStrLn "You find yourself awake in a dark room. There is nothing"
    putStrLn "but an old computer terminal inside. Your goal is to gain"
    putStrLn "the root access."
    putStrLn "Have fun!"
    loop sys
    print "Thanks for playing :)"
