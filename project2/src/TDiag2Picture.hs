import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Picture    (Picture)
import CCO.Tree       (ATerm, Tree (toTree), parser)
import Control.Arrow  ((>>>))

main = undefined
--  ioWrap (parser >>> (component toTree :: Component ATerm Diag_) >>> printer)
