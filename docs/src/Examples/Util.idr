||| Reocurring fragments from the example applications
module Examples.Util

import Examples.CSS.Core
import public Text.CSS.Class
import Text.HTML

--------------------------------------------------------------------------------
--          Usefule Nodes
--------------------------------------------------------------------------------

export
lbl : (text: String) -> (class : Class) -> Node ev
lbl txt cl = label [classes [widgetLabel, cl]] [Text txt]
