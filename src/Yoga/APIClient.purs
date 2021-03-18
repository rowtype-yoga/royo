module Yoga.APIClient where

import Prelude
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff, error, throwError)

type CompileRequest =
  { code ∷ String }

newtype YogaToken = YogaToken String

compileAndRun ∷ YogaToken -> CompileRequest -> Aff (Either CompileResult RunResult)
compileAndRun (YogaToken bearerToken) compileRequest = do
  response <-
    AX.request
      ( AX.defaultRequest
          { url = "https://rowtype.yoga/api/compileAndRun"
          , method = Left POST
          , content = Just (RequestBody.Json (encodeJson compileRequest))
          , responseFormat = ResponseFormat.json
          , headers =
            [ RequestHeader "Authorization" ("Bearer " <> bearerToken)
            , ContentType applicationJSON
            ]
          }
      )
  case response of
    Left (l ∷ AX.Error) ->
      pure
        ( Left
            { resultType: ""
            , result:
              [ { allSpans: []
                , errorCode: ""
                , errorLink: ""
                , filename: ""
                , message: AX.printError l
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
    Right { status, body } -> case status of
      StatusCode 200 -> Right <$> decodeAff body
      StatusCode 422 -> Left <$> decodeAff body
      code -> throwError (error $ "Unexpected response code " <> show code)

decodeAff ∷ ∀ a. DecodeJson a => Json -> Aff a
decodeAff body = either (throwError <<< error <<< printJsonDecodeError) pure (decodeJson body)
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
