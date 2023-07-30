module Web.MVC

import Data.IORef
import Data.Queue

import public JS
import public Web.MVC.Cmd
import public Web.MVC.View
import public Text.HTML

||| Run (a part of) an interactive web page firing events of type
||| `e` and holding state of type `s`.
export covering
runController :
     {0 e,s  : Type}
  -> (ctrl   : e -> s -> (s, Cmd e))
  -> (onErr  : JSErr -> IO ())
  -> (initEv : e)
  -> (initST : s)
  -> IO ()
runController ctrl onErr initEv initST = Prelude.do
  state <- newIORef initST
  flag  <- newIORef False
  queue <- newIORef $ Queue.empty {a = e}

  let covering handle : e -> IO ()
      handle ev = Prelude.do

        -- Enqueue synchronously fired events if we are already handling
        -- an event
        False <- readIORef flag | True => modifyIORef queue (enqueue ev)

        -- Start handing the event and prevent others from currently
        -- being handled
        writeIORef flag True

        -- read current application state
        stOld <- readIORef state

        -- compute new application state and the command to run
        let (stNew, cmd) := ctrl ev stOld

        -- update application state
        writeIORef state stNew

        -- run the command by invoking it with this very event handler
        -- the command might fire one or more events synchronously. these
        -- will be enqueued and processed in a moment.
        ei <- runEitherT (run cmd (liftIO . handle))

        case ei of
          Left err => onErr err
          Right () => pure ()

        -- we are do with handling the current event so we set the flag
        -- back to false.
        writeIORef flag False

        -- we are now going to process the next enqueued command (if any)
        Just (ev2,q) <- dequeue <$> readIORef queue | Nothing => pure ()
        writeIORef queue q
        handle ev2

  handle initEv

export covering
runMVC :
     {0 e,s   : Type}
  -> (update  : e -> s -> s)
  -> (display : e -> s -> Cmd e)
  -> (onErr   : JSErr -> IO ())
  -> (initEv  : e)
  -> (initST  : s)
  -> IO ()
runMVC upd disp onErr =
  runController (\ev,st => let st2 := upd ev st in (st2, disp ev st2)) onErr
