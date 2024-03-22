module Web.MVC.Controller.File

import Data.List1
import Data.String
import Text.HTML.Extra
import Web.MVC
import Web.MVC.Controller
import Web.MVC.Controller.Validation

%default total

fakeBody : String -> String
fakeBody s =
  case [<] <>< forget (split ('\\' ==) s) of
    _ :< p => p
    _      => ""

fileUpdate : FileEv -> (String, Maybe File) -> (String, Maybe File)
fileUpdate (NameChanged s)   (_,f) = (s,f)
fileUpdate (FileChanged f s) _     = (fakeBody s, Just f)

parameters {0 i     : Type}
           {auto fe : FileEnv i}

  fileDisplay : i -> FileEv -> (String, Maybe File) -> Cmd FileEv
  fileDisplay u (NameChanged s)   _     =
    validate (inpRef $ fe.valEnv.inputID u) (fe.read s)
  fileDisplay u (FileChanged _ _) (s,_) =
     value (inpRef $ fe.valEnv.inputID u) s <+>
     validate (inpRef $ fe.valEnv.inputID u) (fe.read s)

  export %inline
  fileC : i -> Controller FileEv (String, Maybe File)
  fileC = updateDisp fileUpdate . fileDisplay

  fileWidget : i -> (String,Maybe File) -> Node FileEv
  fileWidget u ("",_) = file u Nothing
  fileWidget u (s,_)  =
    case fe.read s of
      Right b => file u (Just b)
      Left _  => file u Nothing

  ||| Specialized version of `input` for entering file names.
  export
  file : Editor i (String,Maybe File) FileEv Body
  file = E fileC fileWidget noInit (fe.read . fst) ini
    where
      ini : Maybe Body -> (String,Maybe File)
      ini = (,Nothing) . maybe "" interpolate
