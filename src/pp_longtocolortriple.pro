; docformat = 'rst'
;+
; :Author: Paulo Penteado (http://www.ppenteado.net), Aug/2015
;-


;+
; :Description:
;    Converts colors from long integers to RGB color triples. Input can be either scalar
;    or an array. The inverse of this function is
;    `pp_colortripletolong`.
;
; :Params:
;    icolors: in, required
;      A scalar or array of colors encoded as long integers.
; 
; :Returns:
;    An array with RGB color triples, one triple per input element. If input is an array,
;    output is an array with one extra dimension, of size 3, as the first dimension. If input
;    is scalar, output is simply a 3-element array.
;    
; :Examples:
; 
;   Convert black and white, from long integers to RGB vectors::
;
;     print,pp_longtocolortriple([0L,2L^24-1])
;     ;     0           0           0
;     ;     255         255         255
;
;
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Aug/2015
;-
function pp_longtocolortriple,icolors
compile_opt idl2,logical_predicate
colors=icolors
sz=size(colors,/dimensions)
if sz[0] eq 0 then ret=[byte(colors,0),byte(colors,1),byte(colors,2)] else begin
  ret=lonarr([3,sz])
  foreach color,colors,ic do ret[*,ic]=[byte(color,0),byte(color,1),byte(color,2)]
endelse
return,ret
end
