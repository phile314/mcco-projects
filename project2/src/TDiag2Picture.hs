import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Picture    (Picture)
import CCO.Tree       (ATerm, Tree (toTree, fromTree), parser)
import CCO.Diag       (Diag)
import Control.Arrow  ((>>>), arr)
import Diag2Picture

main =
  ioWrap (parser >>> (component toTree :: Component ATerm Diag) >>> arr toPicture >>> arr fromTree >>> printer)
