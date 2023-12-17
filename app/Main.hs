{-# LANGUAGE RecursiveDo #-}
module Main (main)
where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as UI
import Data.IORef
import Control.Monad (void, when, unless)
import Graphics.UI.Threepenny.Attributes as Attr
import Graphics.UI.Threepenny.Elements hiding (header)
import qualified Data.IntMap as IntMap
import Data.Bool (bool)
import Data.Bits (xor)

main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "static"
        } setup


section :: UI Element
section = UI.mkElement "section"
header :: UI Element
header = UI.mkElement "header"
footer :: UI Element
footer = UI.mkElement "footer"

data Task = Task
  { setCompleted :: Bool -> UI ()
  , setVisibility :: (Bool -> Bool) -> UI ()
  , deleteThis :: UI ()
  , getCompleted :: IO Bool
  }

dblclick :: Element -> Event ()
dblclick = void . domEvent "dblclick"

setup :: Window -> UI ()
setup window = do
  idCtr <- liftIO $ newIORef (0 :: Int)
  tasks <- liftIO $ newIORef mempty
  void $ pure window # set UI.title "TodoMVC"
  UI.addStyleSheet window "todomvc.css"
  -- Hidden if no completed items are left
  clearCompletedBtn <- button #. "clear-completed" # set text "Clear completed" # set style [("display", "none")]
  toggleAllCheckbox <- input #. "toggle-all" # set id_ "toggle-all" # set type_ "checkbox"
  numCompleted <- liftIO $ newIORef 0
  on UI.checkedChange toggleAllCheckbox $ \c -> do
    mapM (`setCompleted` c) =<< liftIO (readIORef tasks)
  let updateToggleAll = do
        current <- get checked toggleAllCheckbox
        newState <- liftIO $ (==) <$> (IntMap.size <$> readIORef tasks) <*> readIORef numCompleted
        when (current /= newState) $ void $ element toggleAllCheckbox # set checked newState
  on UI.click clearCompletedBtn $ \() -> do
    liftIO (readIORef tasks) >>= mapM_ (\t -> liftIO (getCompleted t) >>= flip when (deleteThis t))
    updateToggleAll
  let modifyNumCompleted f = do
        (oldNumCompleted, newNumCompleted) <- liftIO $ atomicModifyIORef numCompleted $ \c -> (f c, (c, f c))
        when ((oldNumCompleted == 0) || (newNumCompleted == 0)) $
          void $ element clearCompletedBtn # set style [("display", bool "none" "" (oldNumCompleted == 0))]
  todoCount <- UI.span #. "todo-count"
  let updateNumLeft = do
        total <- liftIO (IntMap.size <$> readIORef tasks)
        completed <- liftIO $ readIORef numCompleted
        let numLeft = total - completed
        void $ element todoCount # set html ("<strong>" ++ show numLeft ++ "</strong>" ++ " item" ++ bool "s" "" (numLeft == 1) ++ " left")
  todolist <- ul #. "todo-list"
  updateNumLeft
  mainSection <- -- This section should be hidden by default and shown when there are todos
           section #. "main"
           # set style [("display", "none")]
           #+ [ element toggleAllCheckbox
              , label # set for "toggle-all" # set text "Mark all as complete"
              , pure todolist
              ]
  currentFilterRef <- liftIO $ newIORef (const True)
  todoFooter <- -- This footer should be hidden by default and shown when there are todos
    footer #. "footer"
    # set style [("display", "none")]
    #+ [ -- This should be `0 items left` by default
         element todoCount
       , do
           currentFilterBtn <- liftIO $ newIORef undefined
           let filterBtn f lbl = do
                 el <- a # set text lbl
                 on UI.click el $ \() -> do
                   liftIO $ writeIORef currentFilterRef f
                   oldSelection <- liftIO $ readIORef currentFilterBtn
                   liftIO $ writeIORef currentFilterBtn el
                   void $ element oldSelection #. ""
                   void $ element el #. "selected"
                   mapM_ (`setVisibility` f) =<< liftIO (readIORef tasks)
                 pure el
           ul #. "filters"
             #+ [ li #+ [ do el <- filterBtn (const True) "All" #. "selected"
                             liftIO $ writeIORef currentFilterBtn el
                             pure el
                        ]
                , li #+ [ filterBtn not "Active" ]
                , li #+ [ filterBtn id "Completed" ]
                ]
       , element clearCompletedBtn
       ]
  void $ getBody window #+
    [ section #. "todoapp"
      #+ [ header #. "header"
           #+ [ h1 # set text "todos"
              , do
                  el <- input #. "new-todo"
                        # set (attr "placeholder") "What needs to be done?"
                        # set (attr "autofocus") "true"
                  on UI.keydown el $ \c -> when (c == 13) $ mdo
                    -- List items should get the class `editing` when editing and `completed` when marked as completed
                    itemId <- liftIO $ atomicModifyIORef idCtr (\i -> (succ i, i))
                    completed <- input #. "toggle" # set type_ "checkbox"
                    on UI.checkedChange completed (setCompleted_ True)
                    completedState <- liftIO $ newIORef False
                    descr <- get value el
                    void $ set value "" (pure el)
                    flip when (do void $ element mainSection # set style [("display","")]
                                  void $ element todoFooter # set style [("display","")])
                      . IntMap.null =<< liftIO (readIORef tasks)
                    let deleteMe = do
                          liftIO $ modifyIORef tasks (IntMap.delete itemId)
                          wasCompleted <- liftIO $ readIORef completedState
                          when wasCompleted $ modifyNumCompleted pred
                          unless wasCompleted updateNumLeft
                          flip when (do
                                        void $ element mainSection # set style [("display","none")]
                                        void $ element todoFooter # set style [("display","none")])
                            . IntMap.null
                            =<< liftIO (readIORef tasks)
                          delete item
                    let updateClass = void $ do
                          s <- liftIO $ readIORef completedState
                          element item #. bool "" "completed" s
                    lbl <- label # set text descr
                    on dblclick lbl $ \() -> do
                      void $ element item #. "editing"
                    item <- li #. ""
                      #+ [ UI.div #. "view"
                           #+ [ pure completed
                              , pure lbl
                              , do deleteBtn <- button #. "destroy"
                                   on UI.click deleteBtn $ \() -> deleteMe
                                   pure deleteBtn
                              ]
                         , do i <- input #. "edit" # set value descr
                              let doneEditing = do
                                    updateClass
                                    void $ (\t -> element lbl # set text t) =<< get value i
                              on UI.keydown i $ (`when` doneEditing) . (== 13)
                              on UI.blur i $ \() -> doneEditing
                              pure i
                         ]
                    void $ pure todolist #+ [ pure item ]
                    let setCompleted_ fromClick newState = do
                          wasCompleted <- liftIO $ readIORef completedState
                          liftIO $ writeIORef completedState newState
                          when (xor wasCompleted newState) $ do
                            setVisibility' =<< liftIO (readIORef currentFilterRef)
                            modifyNumCompleted (bool pred succ newState)
                            updateNumLeft
                            when fromClick updateToggleAll
                            unless fromClick $ do
                              void $ element completed # set checked newState
                            updateClass
                    let setVisibility' checkViz = do
                          visible <- checkViz <$> liftIO (readIORef completedState)
                          void $ element item # set style (bool [("display","none")] [("display","")] visible)
                    setVisibility' =<< liftIO (readIORef currentFilterRef)
                    liftIO $ modifyIORef tasks $ IntMap.insert itemId $ Task
                      { setCompleted = setCompleted_ False
                      , setVisibility = setVisibility'
                      , getCompleted = readIORef completedState
                      , deleteThis = deleteMe
                      }
                    updateToggleAll
                    updateNumLeft
                  pure el
              ]
         , element mainSection
         , element todoFooter
         ]
    , footer #. "info"
      #+ [ p # set text "Double-click to edit a todo"
         , p #+ [ string "Created by ", a # set href "http://aidy.dev" # set text "Adriaan Leijnse" ]
         , p #+ [ string "Part of ", a # set href "http://todomvc.com" # set text "TodoMVC" ]
         ]
    ]
