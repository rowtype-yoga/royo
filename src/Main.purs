module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Discord.DiscordJS (DiscordToken(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Node.Process (lookupEnv)
import Royo.Bot (runBot)
import Yoga.APIClient (YogaToken(..))

main ∷ Effect Unit
main = do
  maybeDiscordToken <- lookupEnv _DISCORD_TOKEN <#> map DiscordToken
  maybeYogaToken <- lookupEnv _YOGA_TOKEN <#> map YogaToken
  case maybeDiscordToken, maybeYogaToken of
    Nothing, _ -> Console.error $ _DISCORD_TOKEN <> " env variable missing"
    _, Nothing -> Console.error $ _YOGA_TOKEN <> " env variable missing"
    Just discordToken, Just yogaToken -> launchAff_ $ runBot yogaToken discordToken
  where
    _DISCORD_TOKEN = "DISCORD_TOKEN"

    _YOGA_TOKEN = "YOGA_TOKEN"

discord ∷ Effect Unit
discord = do
  maybeDiscordToken <- lookupEnv _DISCORD_TOKEN <#> map DiscordToken
  maybeYogaToken <- lookupEnv _YOGA_TOKEN <#> map YogaToken
  case maybeDiscordToken, maybeYogaToken of
    Nothing, _ -> Console.error $ _DISCORD_TOKEN <> " env variable missing"
    _, Nothing -> Console.error $ _YOGA_TOKEN <> " env variable missing"
    Just discordToken, Just yogaToken -> launchAff_ $ runBot yogaToken discordToken
  where
    _DISCORD_TOKEN = "DISCORD_TOKEN"

    _YOGA_TOKEN = "YOGA_TOKEN"

slack ∷ Effect Unit
slack = do
  maybeDiscordToken <- lookupEnv _DISCORD_TOKEN <#> map DiscordToken
  maybeYogaToken <- lookupEnv _YOGA_TOKEN <#> map YogaToken
  case maybeDiscordToken, maybeYogaToken of
    Nothing, _ -> Console.error $ _DISCORD_TOKEN <> " env variable missing"
    _, Nothing -> Console.error $ _YOGA_TOKEN <> " env variable missing"
    Just discordToken, Just yogaToken -> launchAff_ $ runBot yogaToken discordToken
  where
    _DISCORD_TOKEN = "SLACK_TOKEN"

    _YOGA_TOKEN = "YOGA_TOKEN"
