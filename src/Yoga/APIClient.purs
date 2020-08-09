module Yoga.APIClient where

import Prelude
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt, error, message, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (Foreign)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Simple.JSON (class ReadForeign, read, writeJSON)

fetch ∷ M.Fetch
fetch = M.fetch nodeFetch

type CompileRequest =
  { code ∷ String }

newtype YogaToken = YogaToken String

compileAndRun ∷ YogaToken -> CompileRequest -> Aff (Either CompileResult RunResult)
compileAndRun (YogaToken bearerToken) body = do
  response <-
    attempt
      $ fetch (M.URL "https://rowtype.yoga/api/compileAndRun")
          { method: M.postMethod
          , body: writeJSON body
          , headers:
            M.makeHeaders
              { "Authorization": "Bearer " <> bearerToken
              , "Content-Type": "application/json"
              }
          }
  case response of
    Left l ->
      pure
        ( Left
            { resultType: ""
            , result:
              [ { allSpans: []
                , errorCode: ""
                , errorLink: ""
                , filename: ""
                , message: message l
                , moduleName: Nothing
                , position:
                  { endColumn: 0
                  , endLine: 0
                  , startColumn: 0
                  , startLine: 0
                  }
                , suggestion: Nothing
                }
              ]
            }
        )
    Right r -> case M.statusCode r of
      200 -> M.json r >>= readAff <#> Right
      422 -> M.json r >>= readAff <#> Left
      code -> throwError (error $ "Unexpected response code " <> show code)

readAff ∷ ∀ a. ReadForeign a => Foreign -> Aff a
readAff = read >>> orThrow

orThrow ∷ ∀ a s. Show s => Either s a -> Aff a
orThrow = either (show >>> throw >>> liftEffect) pure

type CompileResult =
  { result ∷ Array ErrorOrWarning
  , resultType ∷ String
  }

type RunResult =
  { code ∷ Maybe Int, stdout ∷ String, stderr ∷ String }

type Suggestion =
  { replaceRange ∷ Position, replacement ∷ String }

type Span =
  { end ∷ Array Int
  , name ∷ String
  , start ∷ Array Int
  }

type Position =
  { endColumn ∷ Int
  , endLine ∷ Int
  , startColumn ∷ Int
  , startLine ∷ Int
  }

type ErrorOrWarning =
  { allSpans ∷ Array Span
  , errorCode ∷ String
  , errorLink ∷ String
  , filename ∷ String
  , message ∷ String
  , moduleName ∷ Maybe String
  , position ∷ Position
  , suggestion ∷ Maybe Suggestion
  }
