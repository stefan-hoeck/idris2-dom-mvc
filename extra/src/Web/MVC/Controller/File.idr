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
           {auto cst : Cast i DomID}
           {auto ve  : ValEnv i}
           {auto fe  : FileEnv i}

  mandatoryBody : String -> Either String Body
  mandatoryBody = checkVal {i} fe.readBody

  fileDisplay : i -> FileEv -> (String, Maybe File) -> Cmd FileEv
  fileDisplay u (NameChanged s)   _     =
    validate (inpRef $ ve.inputID u) (mandatoryBody s)
  fileDisplay u (FileChanged _ _) (s,_) =
     value (inpRef $ ve.inputID u) s <+>
     validate (inpRef $ ve.inputID u) (mandatoryBody s)

  export %inline
  fileC : i -> Controller FileEv (String, Maybe File)
  fileC = updateDisp fileUpdate . fileDisplay

  fileWidget : i -> (String,Maybe File) -> Node FileEv
  fileWidget u ("",_) = file u Nothing
  fileWidget u (s,_)  =
    case mandatoryBody s of
      Right b => file u (Just b)
      Left _  => file u Nothing

  ||| Specialized version of `input` for entering file names.
  export
  file : Editor i (String,Maybe File) FileEv Body
  file = E fileC fileWidget noInit (mandatoryBody . fst) ini
    where
      ini : Maybe Body -> (String,Maybe File)
      ini = (,Nothing) . maybe "" interpolate
