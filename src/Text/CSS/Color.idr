module Text.CSS.Color

import Data.Maybe
import Data.Refined
import Text.CSS.Percentage

%default total

--------------------------------------------------------------------------------
--          Color
--------------------------------------------------------------------------------

public export
data Color : Type where
  RGBA : (red,green,blue : Bits8) -> (transparency : Percentage) -> Color
  HSLA : (hue : Double) -> (sat,light,transparency : Percentage) -> Color

public export %inline
rgb : (red,green,blue : Bits8) -> Color
rgb r g b = RGBA r g b 100

public export %inline
hsl : (hue : Double) -> (sat,light : Percentage) -> Color
hsl h s l = HSLA h s l 100

public export
transparent : Color
transparent = RGBA 0 0 0 0

export
Interpolation Color where
  interpolate (RGBA r g b a) = "rgba(\{show r},\{show g},\{show b},\{a})"
  interpolate (HSLA h s l a) = "hsla(\{show h},\{s},\{l},\{a})"

export
Show Color where show = interpolate

--------------------------------------------------------------------------------
--          X11 Colors (https://www.w3.org/TR/css3-color/#svg-color)
--------------------------------------------------------------------------------

export
aliceblue : Color
aliceblue = rgb 240 248 255

export
antiquewhite : Color
antiquewhite = rgb 250 235 215

export
aqua : Color
aqua = rgb 0 255 255

export
aquamarine : Color
aquamarine = rgb 127 255 212

export
azure : Color
azure = rgb 240 255 255

export
beige : Color
beige = rgb 245 245 220

export
bisque : Color
bisque = rgb 255 228 196

export
blanchedalmond : Color
blanchedalmond = rgb 255 235 205

export
black : Color
black = rgb 0 0 0

export
blue : Color
blue = rgb 0 0 255

export
blueviolet : Color
blueviolet = rgb 138 43 226

export
brown : Color
brown = rgb 165 42 42

export
burlywood : Color
burlywood = rgb 222 184 135

export
cadetblue : Color
cadetblue = rgb 95 158 160

export
chartreuse : Color
chartreuse = rgb 127 255 0

export
chocolate : Color
chocolate = rgb 210 105 30

export
coral : Color
coral = rgb 255 127 80

export
cornflowerblue : Color
cornflowerblue = rgb 100 149 237

export
cornsilk : Color
cornsilk = rgb 255 248 220

export
crimson : Color
crimson = rgb 220 20 60

export
cyan : Color
cyan = rgb 0 255 255

export
darkblue : Color
darkblue = rgb 0 0 139

export
darkcyan : Color
darkcyan = rgb 0 139 139

export
darkgoldenrod : Color
darkgoldenrod = rgb 184 134 11

export
darkgray : Color
darkgray = rgb 169 169 169

export
darkgreen : Color
darkgreen = rgb 0 100 0

export
darkgrey : Color
darkgrey = rgb 169 169 169

export
darkkhaki : Color
darkkhaki = rgb 189 183 107

export
darkmagenta : Color
darkmagenta = rgb 139 0 139

export
darkolivegreen : Color
darkolivegreen = rgb 85 107 47

export
darkorange : Color
darkorange = rgb 255 140 0

export
darkorchid : Color
darkorchid = rgb 153 50 204

export
darkred : Color
darkred = rgb 139 0 0

export
darksalmon : Color
darksalmon = rgb 233 150 122

export
darkseagreen : Color
darkseagreen = rgb 143 188 143

export
darkslateblue : Color
darkslateblue = rgb 72 61 139

export
darkslategray : Color
darkslategray = rgb 47 79 79

export
darkslategrey : Color
darkslategrey = rgb 47 79 79

export
darkturquoise : Color
darkturquoise = rgb 0 206 209

export
darkviolet : Color
darkviolet = rgb 148 0 211

export
deeppink : Color
deeppink = rgb 255 20 147

export
deepskyblue : Color
deepskyblue = rgb 0 191 255

export
dimgray : Color
dimgray = rgb 105 105 105

export
dimgrey : Color
dimgrey = rgb 105 105 105

export
dodgerblue : Color
dodgerblue = rgb 30 144 255

export
firebrick : Color
firebrick = rgb 178 34 34

export
floralwhite : Color
floralwhite = rgb 255 250 240

export
forestgreen : Color
forestgreen = rgb 34 139 34

export
fuchsia : Color
fuchsia = rgb 255 0 255

export
gainsboro : Color
gainsboro = rgb 220 220 220

export
ghostwhite : Color
ghostwhite = rgb 248 248 255

export
gold : Color
gold = rgb 255 215 0

export
goldenrod : Color
goldenrod = rgb 218 165 32

export
gray : Color
gray = rgb 128 128 128

export
green : Color
green = rgb 0 128 0

export
greenyellow : Color
greenyellow = rgb 173 255 47

export
grey : Color
grey = rgb 128 128 128

export
honeydew : Color
honeydew = rgb 240 255 240

export
hotpink : Color
hotpink = rgb 255 105 180

export
indianred : Color
indianred = rgb 205 92 92

export
indigo : Color
indigo = rgb 75 0 130

export
ivory : Color
ivory = rgb 255 255 240

export
khaki : Color
khaki = rgb 240 230 140

export
lavender : Color
lavender = rgb 230 230 250

export
lavenderblush : Color
lavenderblush = rgb 255 240 245

export
lawngreen : Color
lawngreen = rgb 124 252 0

export
lemonchiffon : Color
lemonchiffon = rgb 255 250 205

export
lightblue : Color
lightblue = rgb 173 216 230

export
lightcoral : Color
lightcoral = rgb 240 128 128

export
lightcyan : Color
lightcyan = rgb 224 255 255

export
lightgoldenrodyellow : Color
lightgoldenrodyellow = rgb 250 250 210

export
lightgray : Color
lightgray = rgb 211 211 211

export
lightgreen : Color
lightgreen = rgb 144 238 144

export
lightgrey : Color
lightgrey = rgb 211 211 211

export
lightpink : Color
lightpink = rgb 255 182 193

export
lightsalmon : Color
lightsalmon = rgb 255 160 122

export
lightseagreen : Color
lightseagreen = rgb 32 178 170

export
lightskyblue : Color
lightskyblue = rgb 135 206 250

export
lightslategray : Color
lightslategray = rgb 119 136 153

export
lightslategrey : Color
lightslategrey = rgb 119 136 153

export
lightsteelblue : Color
lightsteelblue = rgb 176 196 222

export
lightyellow : Color
lightyellow = rgb 255 255 224

export
lime : Color
lime = rgb 0 255 0

export
limegreen : Color
limegreen = rgb 50 205 50

export
linen : Color
linen = rgb 250 240 230

export
magenta : Color
magenta = rgb 255 0 255

export
maroon : Color
maroon = rgb 128 0 0

export
mediumaquamarine : Color
mediumaquamarine = rgb 102 205 170

export
mediumblue : Color
mediumblue = rgb 0 0 205

export
mediumorchid : Color
mediumorchid = rgb 186 85 211

export
mediumpurple : Color
mediumpurple = rgb 147 112 219

export
mediumseagreen : Color
mediumseagreen = rgb 60 179 113

export
mediumslateblue : Color
mediumslateblue = rgb 123 104 238

export
mediumspringgreen : Color
mediumspringgreen = rgb 0 250 154

export
mediumturquoise : Color
mediumturquoise = rgb 72 209 204

export
mediumvioletred : Color
mediumvioletred = rgb 199 21 133

export
midnightblue : Color
midnightblue = rgb 25 25 112

export
mintcream : Color
mintcream = rgb 245 255 250

export
mistyrose : Color
mistyrose = rgb 255 228 225

export
moccasin : Color
moccasin = rgb 255 228 181

export
navajowhite : Color
navajowhite = rgb 255 222 173

export
navy : Color
navy = rgb 0 0 128

export
oldlace : Color
oldlace = rgb 253 245 230

export
olive : Color
olive = rgb 128 128 0

export
olivedrab : Color
olivedrab = rgb 107 142 35

export
orange : Color
orange = rgb 255 165 0

export
orangered : Color
orangered = rgb 255 69 0

export
orchid : Color
orchid = rgb 218 112 214

export
palegoldenrod : Color
palegoldenrod = rgb 238 232 170

export
palegreen : Color
palegreen = rgb 152 251 152

export
paleturquoise : Color
paleturquoise = rgb 175 238 238

export
palevioletred : Color
palevioletred = rgb 219 112 147

export
papayawhip : Color
papayawhip = rgb 255 239 213

export
peachpuff : Color
peachpuff = rgb 255 218 185

export
peru : Color
peru = rgb 205 133 63

export
pink : Color
pink = rgb 255 192 203

export
plum : Color
plum = rgb 221 160 221

export
powderblue : Color
powderblue = rgb 176 224 230

export
purple : Color
purple = rgb 128 0 128

export
red : Color
red = rgb 255 0 0

export
rosybrown : Color
rosybrown = rgb 188 143 143

export
royalblue : Color
royalblue = rgb 65 105 225

export
saddlebrown : Color
saddlebrown = rgb 139 69 19

export
salmon : Color
salmon = rgb 250 128 114

export
sandybrown : Color
sandybrown = rgb 244 164 96

export
seagreen : Color
seagreen = rgb 46 139 87

export
seashell : Color
seashell = rgb 255 245 238

export
sienna : Color
sienna = rgb 160 82 45

export
silver : Color
silver = rgb 192 192 192

export
skyblue : Color
skyblue = rgb 135 206 235

export
slateblue : Color
slateblue = rgb 106 90 205

export
slategray : Color
slategray = rgb 112 128 144

export
slategrey : Color
slategrey = rgb 112 128 144

export
snow : Color
snow = rgb 255 250 250

export
springgreen : Color
springgreen = rgb 0 255 127

export
steelblue : Color
steelblue = rgb 70 130 180

export
tan : Color
tan = rgb 210 180 140

export
teal : Color
teal = rgb 0 128 128

export
thistle : Color
thistle = rgb 216 191 216

export
tomato : Color
tomato = rgb 255 99 71

export
turquoise : Color
turquoise = rgb 64 224 208

export
violet : Color
violet = rgb 238 130 238

export
wheat : Color
wheat = rgb 245 222 179

export
white : Color
white = rgb 255 255 255

export
whitesmoke : Color
whitesmoke = rgb 245 245 245

export
yellow : Color
yellow = rgb 255 255 0

export
yellowgreen : Color
yellowgreen = rgb 154 205 50
