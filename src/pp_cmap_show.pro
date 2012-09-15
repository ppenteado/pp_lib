; docformat = 'rst'
;+
; :Description:
;    Produces an iplot showing the color of each index in the given colormap array. Each bar is plotted with the x
;    location corresponding to its index. See example below.
;
; :Params:
;    cm : in, required
;      The colormap to display, in a 3xn array of RGB values, with each row being the 3 values for each color index.
;      It is the same format as returned by pp_cmap (see example below).
;    locs : in, optional
;      If given an array of n values, those are used for the locations of each color plotted, instead of their indexes. 
;
; :Examples:
;    To identify the colors made by pp_cmap::
;    
;      cmap=pp_cmap(6)
;      print,cmap
;      ;  0   0   0
;      ;255   0   0
;      ;255 255   0
;      ;0 255   0
;      ;0 255 255
;      ;0   0 255
;      ;255   0 255
;      ;127 127 127
;      ;255 255 255
;      pp_cmap_show,cm
;
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Dec/2009
;-
pro pp_cmap_show,cm,locs
if (n_elements(locs) ne 0) then begin
  sz=size(locs)
  for i=0,sz[1]-1 do iplot,locs[[i,i]],[0,1],over=(i ne 0),color=cm[*,i],thick=10
endif else begin
  sz=size(cm)
  for i=0,sz[2]-1 do iplot,[i,i],[0,1],over=(i ne 0),color=cm[*,i],thick=10
endelse
end
