import qualified System.Environment
import qualified GameMaster
-- import           GameMaster (GameMaster)

main = do
  args <- System.Environment.getArgs
  let filename = case args of
        (filename:_) -> filename
        [] -> "GAME_MASTER.yaml"
  result <- GameMaster.load filename
  case result of
    Right (Just gameMaster) ->
      print gameMaster
    Right Nothing ->
      putStrLn "Got Nothing."
    Left exception ->
      print exception
