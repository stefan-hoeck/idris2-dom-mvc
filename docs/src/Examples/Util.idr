||| Reocurring fragments from the example applications
module Examples.Util

import Examples.CSS.Core
import Text.HTML

--------------------------------------------------------------------------------
--          Usefule Nodes
--------------------------------------------------------------------------------

export
lbl : (text: String) -> (class : String) -> Node ev
lbl txt cl = label [classes [widgetLabel, cl]] [Text txt]
