import Html

import           Prelude     hiding (div)

data Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap g (Reader f) = Reader $ g . f

instance Applicative (Reader r) where
    pure a = Reader $ const a
    (Reader g) <*> (Reader f) = Reader $ \r -> g r $ f r

instance Monad (Reader r) where
    return = pure
    (Reader g) >>= f = Reader $ \r -> runReader (f (g r)) r

ask = Reader id

main :: IO ()
main =
  putStrLn "what is your email address?" >>
    getLine >>= \email ->
      putStrLn . show $ runReader view email

view :: Reader Email Html
view = do
    page' <- page
    return $
        div
            [ page'
            ]

page :: Reader Email Html
page = do
    content' <- content
    return $
        div
            [ topNav
            , content'
            ]

topNav :: Html
topNav =
  div
    [ h1 [ "OurSite.com" ]
    ]

content :: Reader Email Html
content = do
    email <- ask
    right' <- right
    return $ 
        div
            [ h1 [ "Custom Content for " ++ email ]
            , left
            , right'
            ]

left :: Html
left =
  div
    [ p [ "this is the left side" ]
    ]

right :: Reader Email Html
right = do
    email <- ask
    article' <- article
    return $
        div
            [ article'
            ]

article :: Reader Email Html
article = do
    email <- ask
    widget' <- widget
    return $
        div
            [ p [ "this is an article" ]
            , widget'
            ]

widget :: Reader Email Html
widget = do
    email <- ask
    return $
        div
            [ p [ "Hey " ++ email ++ ", we've got a great offer for you!" ]
            ]