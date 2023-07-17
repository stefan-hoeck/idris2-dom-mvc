module Text.CSS.Flexbox

import Data.List

%default total

public export
data FlexDirection =
    Row
  | RowReverse
  | Column
  | ColumnReverse

export
Interpolation FlexDirection where
  interpolate Row           = "row"
  interpolate RowReverse    = "row-reverse"
  interpolate Column        = "column"
  interpolate ColumnReverse = "column-reverse"

public export
data FlexAlign =
    Normal
  | Stretch
  | Center
  | Start
  | End
  | FlexStart
  | FlexEnd
  | Baseline
  | FirstBaseline
  | LastBaseline

export
Interpolation FlexAlign where
  interpolate Normal        = "normal"
  interpolate Stretch       = "stretch"
  interpolate Center        = "center"
  interpolate Start         = "start"
  interpolate End           = "end"
  interpolate FlexStart     = "flex-start"
  interpolate FlexEnd       = "flex-end"
  interpolate Baseline      = "baseline"
  interpolate FirstBaseline = "first baseline"
  interpolate LastBaseline  = "last baseline"

namespace FlexJustify
  public export
  data FlexJustify =
      Center
    | Start
    | End
    | FlexStart
    | FlexEnd
    | Left
    | Right
    | Normal
    | SpaceBetween
    | SpaceAround
    | SpaceEvenly
    | Stretch

  export
  Interpolation FlexJustify where
    interpolate Center       = "center"
    interpolate Start        = "start"
    interpolate End          = "end"
    interpolate FlexStart    = "flex-start"
    interpolate FlexEnd      = "flex-end"
    interpolate Left         = "left"
    interpolate Right        = "right"
    interpolate Normal       = "normal"
    interpolate SpaceBetween = "space-between"
    interpolate SpaceAround  = "space-around"
    interpolate SpaceEvenly  = "space-evenly"
    interpolate Stretch      = "stretch"

namespace FlexFlow
  public export
  data FlexFlow =
      Column
    | ColumnReverse
    | Inherit
    | Initial
    | Nowrap
    | Revert
    | RevertLayout
    | Row
    | RowReverse
    | Unset
    | Wrap
    | WrapReverse

  export
  Interpolation FlexFlow where
    interpolate Column        = "column"
    interpolate ColumnReverse = "column-reverse"
    interpolate Inherit       = "inherit"
    interpolate Initial       = "initial"
    interpolate Nowrap        = "nowrap"
    interpolate Revert        = "revert"
    interpolate RevertLayout  = "revert-layout"
    interpolate Row           = "row"
    interpolate RowReverse    = "row-reverse"
    interpolate Unset         = "unset"
    interpolate Wrap          = "wrap"
    interpolate WrapReverse   = "wrap-reverse"

export
Interpolation (List FlexFlow) where
  interpolate = concat . intersperse " " . map interpolate
