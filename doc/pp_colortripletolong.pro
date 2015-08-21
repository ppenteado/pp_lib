; docformat = 'rst'
;+
; :Author: Paulo Penteado (http://www.ppenteado.net), Aug/2015
;-


;+
; :Description:
;    Converts colors from RGB color triples to long integers. Input can be a single triple
;    or multiple triples, as an array of any dimension. The inverse of this function is
;    `pp_longtocolortriple`.
;
; :Params:
;    icolors: in, required
;      An array of color triples. The first dimension of the array must have size 3,
;      and be the color dimension.
;
; :Returns:
;    An array with colors encoded as long integers, one long per per input triple. Output
;    dimension is one less than input.
;
; :Examples:
;
;   Convert black and white, from RGB vectors to long integers::
;
;     print,pp_colortripletolong([[0,0,0],[255,255,255]])
;     ;           0    16777215
;
;
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Aug/2015
;-
function pp_colortripletolong,icolors
compile_opt idl2,logical_predicate
colors=0L>long(icolors)<255L
sz=size(colors,/dimensions)
if n_elements(sz) eq 1 then ret=colors[0]+colors[1]*2L^8+colors[2]*2L^16 else begin
  ret=lonarr(sz[1:-1])
  ret[*]=colors[0:-1:3]+colors[1:-1:3]*2L^8+colors[2:-1:3]*2L^16
endelse
return,ret
end
