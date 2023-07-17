module Examples.CSS.Core

import Examples.CSS.Colors
import Text.CSS
import Web.MVC.ElemRef

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

||| ID of the `<body>` element. The page content will
||| be placed here.
public export
contentDiv : ElemRef
contentDiv = Id Body "content"

||| The page consists of a static heading with a title an
||| (eventually) a short description of the project.
||| This is followed by a selection box, where visitors can
||| choose an example application.
|||
||| The example application will be dynamicall generated and
||| placed in to a `<div>` with ID `"example"`.
public export
exampleDiv : ElemRef
exampleDiv = Id Div "example"

||| ID of a `<style>` element in the page header.
||| The generated CSS rules will go here.
public export
appStyle : ElemRef
appStyle = Id Style "appstyle"

--------------------------------------------------------------------------------
--          Classes
--------------------------------------------------------------------------------

||| a clickable button
public export
btn : String
btn = "btn"

||| a text input
public export
textIn : String
textIn = "textin"

||| a select input
public export
selectIn : String
selectIn = "selectin"

||| an input widget
public export
widget : String
widget = "widget"

||| the main content, split into three rows:
||| a title, the example selector, and the
||| currently loaded example application
public export
contentList : String
contentList = "contentList"

||| the header row where the example selector
||| resides
public export
contentHeader : String
contentHeader = "contentHeader"

||| the row with the page title
public export
pageTitle : String
pageTitle = "pageTitle"

||| the select box used to choose an example
||| application
public export
exampleSelector : String
exampleSelector = "example_selector"

||| a label on the left of an input
||| widget.
public export
widgetLabel : String
widgetLabel = "widgetlabel"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

export
coreCSS : List (Rule 1)
coreCSS =
  [ elem Html
      [ height          $ perc 100]

  , elem Body
      [ backgroundColor black
      , color           base100
      , display         Flex
      , flexDirection   Column
      , fontFamily      "Helvetica, Arial, sans-serif"
      , height          $ perc 100
      , margin          $ px 0
      ]

  , class contentList
      [ alignSelf        Center
      , backgroundColor  darker_grey
      , display          Flex
      , flex             "1"
      , flexDirection    Column
      , justifyContent   FlexStart
      , padding          $ VH (px 0) (pt 20)
      , minWidth         $ perc 80
      ]

  , class pageTitle
      [ borderStyle      $ Bottom Solid
      , borderWidth      $ Bottom (px 5)
      , borderColor      $ Bottom base80
      , fontSize         XLarge
      , padding          $ VH (px 40) (px 0)
      , textAlign        Center
      ]

  , class contentHeader
      [ display              Grid
      , columnGap            $ px 10
      , gridTemplateColumns  [px 170, fr 1, fr 3]
      , borderStyle          $ Bottom Solid
      , borderWidth          $ Bottom (px 2)
      , borderColor          $ Bottom base80
      , padding              $ VH (px 30) (px 10)
      ]

  , class widget
      [ backgroundColor lighter_grey
      , borderRadius    $ px 8
      , borderStyle     $ All Solid
      , borderWidth     $ px 2
      , borderColor     $ All comp100
      , color           darker_grey
      , fontSize        Large
      , padding         $ px 3
      ]

  , sel [class widget, Hover]
      [ backgroundColor lightest_grey
      , borderColor     $ All comp60
      ]

  , sel [class widget, Active]
      [ backgroundColor lightest_grey
      , borderColor     $ All comp60
      ]

  , sel [class widget, FocusVisible]
      [ backgroundColor lightest_grey
      , borderColor     $ All comp60
      ]

  , sel [class widget, Disabled]
      [ backgroundColor light_grey
      , borderColor     $ All dark_grey
      ]

  , class textIn
      [ textAlign       End ]

  , class selectIn
      [ textAlign       End ]

  , class exampleSelector
      [ fontSize        Large
      , gridColumn      $ At 2
      ]

  , sel [class widget, Invalid]
      [ borderColor     $ All red ]

  , class widgetLabel [ fontSize Large ]
  ]
