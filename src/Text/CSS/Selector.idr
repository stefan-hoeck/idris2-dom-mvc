module Text.CSS.Selector

import Data.List
import Data.String
import Text.CSS.Property
import Web.Dom

%default total

public export
data Combinator : Type where
  Descendant      : Combinator
  Child           : Combinator
  GeneralSibling  : Combinator
  AdjacentSibling : Combinator

export
Interpolation Combinator where
  interpolate Descendant      = ""
  interpolate Child           = ">"
  interpolate GeneralSibling  = "~"
  interpolate AdjacentSibling = "+"

public export
data Selector : Type where
  Star    : Selector
  Id      : String -> Selector
  Class   : String -> Selector
  Elem    : {str : _} -> (0 tpe : ElementType str t) -> Selector
  Complex : Selector -> Combinator -> Selector -> Selector
  Nil     : Selector
  (::)    : Selector -> Selector -> Selector

  ||| Matches when the user activates (for example clicks on) an element.
  Active : Selector
  ||| Matches both the :link and :visited states of a link.
  AnyLink : Selector
  ||| Matches an <input> element whose input value is empty.
  Blank : Selector
  ||| Matches a radio button or checkbox in the selected state.
  Checked : Selector
  ||| Matches the element, or an ancestor of the element, that is currently being displayed.
  Current : Selector
  ||| Matches the one or more UI elements that are the default among a set of similar elements.
  Default : Selector
  ||| Select an element based on its directionality (value of the HTML dir attribute or CSS direction property).
  Dir : Direction -> Selector
  ||| Matches user interface elements that are in an disabled state.
  Disabled : Selector
  ||| Matches an element that has no children except optionally white space.
  Empty : Selector
  ||| Matches user interface elements that are in an enabled state.
  Enabled : Selector
  ||| In Paged Media, matches the first page.
  First : Selector
  ||| Matches an element that is first among its siblings.
  FirstChild : Selector
  ||| Matches an element which is first of a certain type among its siblings.
  FirstOfType : Selector
  ||| Matches when an element has focus.
  Focus : Selector
  ||| Matches when an element has focus and the focus should be visible to the user.
  FocusVisible : Selector
  ||| Matches an element with focus plus an element with a descendent that has focus.
  FocusWithin : Selector
  ||| Matches the elements after the current element.
  Future : Selector
  ||| Matches when the user hovers over an element.
  Hover : Selector
  ||| Matches UI elements whose value is in an indeterminate state, usually checkboxes.
  Indeterminate : Selector
  ||| Matches an element with a range when its value is in-range.
  InRange : Selector
  ||| Matches an element, such as an <input>, in an invalid state.
  Invalid : Selector
  ||| Matches an element based on language (value of the HTML lang attribute).
  Lang : String -> Selector
  ||| Matches an element which is last among its siblings.
  LastChild : Selector
  ||| Matches an element of a certain type that is last among its siblings.
  LastOfType : Selector
  ||| In Paged Media, matches left-hand pages.
  Left : Selector
  ||| Matches unvisited links.
  Link : Selector
  ||| Matches links pointing to pages that are in the same site as the current document.
  LocalLink : Selector
  |||Matches elements from a list of siblings — the siblings are matched by a formula of the form an+b (e.g. 2n + 1 would match elements 1, 3, 5, 7, etc. All the odd ones.)
  NthChild : String -> Selector
  |||Matches elements from a list of siblings — the siblings are matched by a formula of the form an+b (e.g. 2n + 1 would match elements 1, 3, 5, 7, etc. All the odd ones.)
  NthOfType : String -> Selector
  ||| Matches elements from a list of siblings, counting backwards from the end. The siblings are matched by a formula of the form an+b (e.g. 2n + 1 would match the last element in the sequence, then two elements before that, then two elements before that, etc. All the odd ones, counting from the end.)
  NthLastChild : String -> Selector
  ||| Matches elements from a list of siblings that are of a certain type (e.g. <p> elements), counting backwards from the end. The siblings are matched by a formula of the form an+b (e.g. 2n + 1 would match the last element of that type in the sequence, then two elements before that, then two elements before that, etc. All the odd ones, counting from the end.)
  NthLastOfType : String -> Selector
  ||| Matches an element that has no siblings.
  OnlyChild : Selector
  ||| Matches an element that is the only one of its type among its siblings.
  OnlyOfType : Selector
  ||| Matches form elements that are not required.
  Optional : Selector
  ||| Matches an element with a range when its value is out of range.
  OutOfRange : Selector
  ||| Matches the elements before the current element.
  Past : Selector
  ||| Matches an input element that is showing placeholder text.
  PlaceholderShown : Selector
  ||| Matches an element representing an audio, video, or similar resource that is capable of being “played” or “paused”, when that element is “playing”.
  Playing : Selector
  ||| Matches an element representing an audio, video, or similar resource that is capable of being “played” or “paused”, when that element is “paused”.
  Paused : Selector
  ||| Matches an element if it is not user-alterable.
  ReadOnly : Selector
  ||| Matches an element if it is user-alterable.
  ReadWrite : Selector
  ||| Matches form elements that are required.
  Required : Selector
  ||| In Paged Media, matches right-hand pages.
  Right : Selector
  ||| Matches an element that is the root of the document.
  Root : Selector
  ||| Matches any element that is a scope element.
  Scope : Selector
  ||| Matches an element such as an <input> element, in a valid state.
  Valid : Selector
  ||| Matches an element if it is the target of the current URL (i.e. if it has an ID matching the current URL fragment).
  Target : Selector
  ||| Matches visited links.
  Visited : Selector

  After : Selector
  Backdrop : Selector
  Before : Selector
  Cue : Selector
  CueRegion : Selector
  FirstLetter : Selector
  FirstLine : Selector
  FileSelectorButton : Selector
  Marker : Selector
  Placeholder : Selector
  Selection : Selector


export
Interpolation Selector where
  interpolate Star                        = "*"
  interpolate (Id s)                      = "#\{s}"
  interpolate (Class s)                   = ".\{s}"
  interpolate (Elem {str} _)              = str
  interpolate []                          = ""
  interpolate (h::t)                      = interpolate h ++ interpolate t
  interpolate (Complex  s1 Descendant s2) = "\{s1} \{s2}"
  interpolate (Complex  s1 c s2)          = "\{s1} \{c} \{s2}"
  interpolate Active                      = ":active"
  interpolate AnyLink                     = ":any-link"
  interpolate Blank                       = ":blank"
  interpolate Checked                     = ":checked"
  interpolate Current                     = ":current"
  interpolate Default                     = ":default"
  interpolate (Dir x)                     = ":dir(\{x})"
  interpolate Disabled                    = ":disabled"
  interpolate Empty                       = ":empty"
  interpolate Enabled                     = ":enabled"
  interpolate First                       = ":first"
  interpolate FirstChild                  = ":first-child"
  interpolate FirstOfType                 = ":first-of-type"
  interpolate Focus                       = ":focus"
  interpolate FocusVisible                = ":focus-visible"
  interpolate FocusWithin                 = ":focus-within"
  interpolate Future                      = ":future"
  interpolate Hover                       = ":hover"
  interpolate Indeterminate               = ":indeterminate"
  interpolate InRange                     = ":in-range"
  interpolate Invalid                     = ":invalid"
  interpolate (Lang x)                    = ":lang(\{x})"
  interpolate LastChild                   = ":last-child"
  interpolate LastOfType                  = ":last-of-type"
  interpolate Left                        = ":left"
  interpolate Link                        = ":link"
  interpolate LocalLink                   = ":local-link"
  interpolate (NthChild x)                = ":nth-child(\{x})"
  interpolate (NthOfType x)               = ":nth-of-type(\#{x})"
  interpolate (NthLastChild x)            = ":nth-last-child(\#{x})"
  interpolate (NthLastOfType x)           = ":nth-last-of-type(\#{x})"
  interpolate OnlyChild                   = ":only-child"
  interpolate OnlyOfType                  = ":only-of-type"
  interpolate Optional                    = ":optional"
  interpolate OutOfRange                  = ":out-of-range"
  interpolate Past                        = ":past"
  interpolate PlaceholderShown            = ":placeholder-shown"
  interpolate Playing                     = ":playing"
  interpolate Paused                      = ":paused"
  interpolate ReadOnly                    = ":read-only"
  interpolate ReadWrite                   = ":read-write"
  interpolate Required                    = ":required"
  interpolate Right                       = ":right"
  interpolate Root                        = ":root"
  interpolate Scope                       = ":scope"
  interpolate Valid                       = ":valid"
  interpolate Target                      = ":target"
  interpolate Visited                     = ":visited"
  interpolate After                       = "::after"
  interpolate Backdrop                    = "::backdrop"
  interpolate Before                      = "::before"
  interpolate Cue                         = "::cue"
  interpolate CueRegion                   = "::cue-region"
  interpolate FirstLetter                 = "::first-letter"
  interpolate FirstLine                   = "::first-line"
  interpolate FileSelectorButton          = "::file-selector-button"
  interpolate Marker                      = "::marker"
  interpolate Placeholder                 = "::placeholder"
  interpolate Selection                   = "::selection"

export %inline
class : String -> Selector
class = Class

export
classes : List String -> Selector
classes []        = []
classes (x :: xs) = class x :: classes xs

export %inline
elem : {str : _} -> (0 tpe : ElementType str t) -> Selector
elem = Elem

export %inline
id : String -> Selector
id = Id
