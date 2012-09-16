; docformat = 'rst'
;+
; :Description:
;    Simple wrapper for the contour procedure, used to extract the contour path
;    points, and pack them into a list.
;
; :Returns:
;    A list where each element has the n x,y pairs of each contour level,
;    as a [2,x] array, or and empty array when the level has no points.  
;
; :Params:
;    z : in, required
;      Passed unchanged to contour.
;    x : in, optional
;      Passed unchanged to contour.
;    y : in, optional
;      Passed unchanged to contour.
;
; :Keywords:
;    _ref_extra : in, out, optional
;      Passed unchanged to/from contour.
;      
; :Requires: IDL 8.0 or higher, for the lists and empty arrays. 
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Aug/2010
;-
function pp_contour_paths,z,x,y,_ref_extra=_rex
compile_opt idl2, logical_predicate
case n_params() of
  1: contour,z,_strict_extra=_rex,path_info=path_info,path_xy=path_xy,/path_double
  2: contour,z,x_strict_extra=_rex,path_info=path_info,path_xy=path_xy,/path_double
  3: contour,z,x,y,_strict_extra=_rex,path_info=path_info,path_xy=path_xy,/path_double
endcase
nl=n_elements(path_info)
path_list=list(length=nl)
for i=0,nl-1 do if (path_info[i].n gt 0) then path_list[i]=path_xy[*,path_info[i].offset:path_info[i].offset+path_info[i].n-1]
return,path_list
end
