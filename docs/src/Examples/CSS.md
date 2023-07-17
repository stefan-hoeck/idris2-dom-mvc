# CSS

The plan of *rhone-js* is to eventually come
'batteries included' and this means having a way
to programmatically declare (and change) the appearance
of a web page. The `Text.CSS` submodules therefore come
with a (so far incomplete) set of data types for
declaring CSS rules in a type-safe manner.

```idris
module Examples.CSS

import Data.String
import public Examples.CSS.Balls
import public Examples.CSS.Core
import public Examples.CSS.Fractals
import public Examples.CSS.MathGame
import public Examples.CSS.Performance
import public Examples.CSS.Reset
import Text.CSS

%default total
```
## IDs and Classes

The `rhone.html` document at the project root defines two
entry points for our single-page web page: A `style` element
in the header, where our CSS rules go, and the body element,
where the content of our web page goes. We typically refer
to HTML elements via `ElemRef` values
(defined in `Rhone.JS.ElemRef`), which come with
an ID and a tag to allow us to safely request the properly
typed element from the DOM.

## CSS Rules

Note: I'm by no means an expert, so
the CSS rules below might quite well seem horrible
to purists; suggestions of improvements are welcome.

Here are the core rules for laying out the web page (the details can
be found in the corresponding submodules).

```idris
export
allRules : String
allRules =  fastUnlines . map interpolate
         $  coreCSS
         ++ Balls.css
         ++ Fractals.css
         ++ MathGame.css
         ++ Performance.css
         ++ Reset.css
```

<!-- vi: filetype=idris2:syntax=markdown
-->
