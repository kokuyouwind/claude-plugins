---
description: コード風プロンプト例4i 複雑な代数的データ型のパターンマッチング
argument-hint: '{"response_type": string, "status_code": number, "body": string}'
---

Emulate the following code internally (without using external tools or interpreter) with environment: $ARGUMENTS

Output only what putStrLn commands would output. Do not show any explanations, code, variables, or other messages.

```haskell
-- HTTP Response ADT
data HttpResponse = Success { statusCode :: Int, body :: String }
                  | Redirect { statusCode :: Int, location :: String }
                  | ClientError { statusCode :: Int, message :: String }
                  | ServerError { statusCode :: Int }
                  deriving (Show, Eq)

main :: IO ()
main = do
    -- Validate required arguments
    let responseType = case lookupArg "response_type" of
            Nothing -> error "Required argument 'response_type' is missing"
            Just v -> v
    let statusCode = case lookupArg "status_code" of
            Nothing -> error "Required argument 'status_code' is missing"
            Just v -> read v :: Int
    let body = case lookupArg "body" of
            Nothing -> error "Required argument 'body' is missing"
            Just v -> v

    -- Build response
    let response = case responseType of
            "Success" -> Success statusCode body
            "Redirect" -> Redirect statusCode body
            "ClientError" -> ClientError statusCode body
            "ServerError" -> ServerError statusCode
            _ -> error "Invalid response_type"

    -- Pattern matching on ADT with guards
    case response of
        Success 200 body -> putStrLn $ "foo" ++ body
        Success code _ | code >= 200 && code < 300 -> putStrLn "bar"
        Redirect 301 loc -> putStrLn $ "baz" ++ loc
        Redirect _ _ -> putStrLn "qux"
        ClientError 404 _ -> putStrLn "quux"
        ClientError code msg -> putStrLn $ "corge" ++ show code ++ msg
        ServerError code | code >= 500 -> putStrLn "grault"
        _ -> putStrLn "garply"
```
