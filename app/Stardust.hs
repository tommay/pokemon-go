module Main where

import qualified Options.Applicative as O
import           Options.Applicative ((<|>), (<**>))
import           Data.Semigroup ((<>))

import qualified Discounts
import qualified Epic
import qualified GameMaster
import           GameMaster (GameMaster)
import qualified Powerups

import           Control.Monad (join, forM_)
import           Data.List as List
import qualified System.Exit as Exit

data Options = Options {
  level :: Float,
  isShadow :: Bool,
  isPurified :: Bool,
  isLucky :: Bool
}

getOptions :: IO Options
getOptions =
  let opts = Options <$> optLevel
        <*> optIsShadow <*> optIsPurified <*> optIsLucky
      optLevel = O.option O.auto
        (  O.long "level"
        <> O.short 'l'
        <> O.value 1.0
        <> O.showDefault
        <> O.metavar "LEVEL"
        <> O.help "Show stardust costs from LEVEL")
      optIsShadow = O.switch
        (  O.long "shadow"
        <> O.short 's'
        <> O.help "Show for shadow pokemon")
      optIsPurified = O.switch
        (  O.long "purified"
        <> O.short 'p'
        <> O.help "Show for purified pokemon")
      optIsLucky = O.switch
        (  O.long "lucky"
        <> O.short 'L'
        <> O.help "Show for lucy pokemon")
      options = O.info (opts <**> O.helper)
        (  O.fullDesc
        <> O.progDesc "Show stardust costs to get to various levels.")
      prefs = O.prefs O.showHelpOnEmpty
  in O.customExecParser prefs options

main =
  Epic.catch (
    do
      options <- getOptions
      let discounts = Discounts.new
            (isShadow options) (isPurified options) (isLucky options)
      gameMaster <- join $ GameMaster.load "GAME_MASTER.yaml"
      let levelsAndCosts = Powerups.levelsAndCosts gameMaster discounts
            $ level options
      forM_ levelsAndCosts $ \ (level, dust, candy) ->
        putStrLn $ show level ++ ": " ++ show dust ++ " " ++ show candy
    )
    $ Exit.die
