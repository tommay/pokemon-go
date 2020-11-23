import qualified System.Environment
import qualified Epic
import qualified GameMaster

main = do
  args <- System.Environment.getArgs
  Epic.catch (
    do
      result <- GameMaster.load
      gameMaster <- result
      print gameMaster
    )
    $ putStrLn . ("oops: " ++)
