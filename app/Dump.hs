import qualified System.Environment
import qualified Epic
import qualified GameMaster

main = do
  args <- System.Environment.getArgs
  let filename = case args of
        (filename:_) -> filename
        [] -> "GAME_MASTER.yaml"
  Epic.catch (
    do
      result <- GameMaster.load filename
      gameMaster <- result
      print gameMaster)
    (\ex -> putStrLn $ "oops: " ++ ex)
