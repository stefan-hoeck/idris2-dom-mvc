module Text.HTML.File

import Text.HTML
import Text.HTML.Class
import Text.HTML.DomID
import Text.HTML.Validation
import public Data.FilePath
import public Web.Internal.FileTypes

%default total


public export
data FileEv : Type where
  NameChanged : String -> FileEv
  FileChanged : File -> String -> FileEv

||| Environment for editing files plus their paths
public export
record FileEnv (i : Type) where
  [noHints]
  constructor FE
  fileID    : i -> i
  read      : String -> Either String Body
  browse    : String
  fileCls   : Class
  labelCls  : Class
  {auto valEnv : ValEnv i}

export %inline %hint
fileEnvToValEnv : FileEnv i => ValEnv i
fileEnvToValEnv @{fe} = fe.valEnv

fromInfo : InputInfo -> Maybe FileEv
fromInfo (MkInputInfo v [f] checked) = Just (FileChanged f v)
fromInfo _                           = Nothing

export
file : (fe : FileEnv i) => i -> Maybe Body -> Node FileEv
file uid (Just b) = NameChanged <$> vinp fe.fileCls Text uid "\{b}"
file uid Nothing  =
  cell fe.fileCls []
    [ NameChanged <$> vinp fe.fileCls Text (fe.valEnv.inputID uid) ""
    , label [forID (fe.fileID uid), cls Btn fe.labelCls] [Text fe.browse]
    , input
        [ ref $ fe.fileID uid
        , cls Widget fe.fileCls
        , type File
        , Event (Input fromInfo)
        ] []
    ]
