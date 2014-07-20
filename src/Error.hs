module Error where

data HlamError = ParseError String
               | TypeError String

instance Show HlamError where
    show (ParseError err) = "ParseError: " ++ err
    show (TypeError err) = "TypeError: " ++ err

