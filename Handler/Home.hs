module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,withSmallInput)

getHomeR :: Handler Html
getHomeR = do
    (formWidget1, formEnctype1) <- generateFormPost orestisForm
    (formWidget2, formEnctype2) <- generateFormPost kristinaForm
    (formWidget3, formEnctype3) <- generateFormPost todoForm

    t1 <- runDB $ selectList [TaskOwner ==. "orestis"] []
    t2 <- runDB $ selectList [TaskOwner ==. "kristina"] []
    t3 <- runDB $ selectList [TaskOwner ==. "none"] []

    defaultLayout $ do
        setTitle "Allee Diderot"
        $(widgetFile "homepage")


postAddTaskR :: Text -> Handler ()
postAddTaskR owner = do
    case owner of
        "orestis" -> do
            ((result, _), _) <- runFormPost orestisForm
            case result of
                FormSuccess (name, price) -> do
                    _ <-    runDB $ insert $ Task name price owner
                    redirect HomeR
                _ -> do
                    redirect HomeR
        "kristina" -> do
            ((result, _), _) <- runFormPost kristinaForm
            case result of
                FormSuccess (name, price) -> do
                    _ <-    runDB $ insert $ Task name price owner
                    redirect HomeR
                _ -> do
                    redirect HomeR
        _ -> do
            ((result, _), _) <- runFormPost todoForm
            case result of
                FormSuccess name -> do
                    _ <-    runDB $ insert $ Task name 0.0 "none"
                    redirect HomeR
                _ -> do
                    redirect HomeR


postDeleteAllR :: Handler ()
postDeleteAllR = do
    _ <- runDB $ deleteWhere ([] :: [Filter Task])
    redirect HomeR

postDeleteR :: Text -> Handler ()
postDeleteR taskId = do
  _ <- runDB $ deleteWhere [TaskName ==. taskId]
  redirect HomeR

-- Get total price from DB result
total :: [Entity Task] -> Double
total list =
    sum $ map (\(Entity _ t) -> taskPrice t) list

-- FORMS
orestisForm :: Form (Text, Double)
orestisForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (withSmallInput "What?") Nothing
    <*> areq doubleField (withSmallInput "How much?") Nothing

kristinaForm :: Form (Text, Double)
kristinaForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (withSmallInput "What?") Nothing
    <*> areq doubleField (withSmallInput "How much?") Nothing

todoForm :: Form Text
todoForm = renderBootstrap3 BootstrapBasicForm $
    areq textField (withSmallInput "What?") Nothing
