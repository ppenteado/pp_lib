; docformat = 'rst'

;+
; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), Feb/2016
;-
;+
; :Description:
;    Converts a connectivity array, such as one used by
;    `map_proj_forward <https://www.exelisvis.com/docs/MAP_PROJ_FORWARD.html#M_824365677_999199>`,
;     to a list of arrays. Each list element is an array with the indices of the corresponding
;     line/polygon.
;
; :Params:
;    conn: in, required
;      An array with a connectivity list, such as that used by 
;      `map_proj_forward <https://www.exelisvis.com/docs/MAP_PROJ_FORWARD.html#M_824365677_999199>`,
;      which starts with an integer m0 specifying the number of vertices belonging to the first
;      line/polygon, followed by the m0 indices belonging to that line/polygon, followed by 
;      an m1 integer specifying the number of indices belonging to the second line/polygon, 
;      followed by the m1 indices of the second line/polygon, and so on. 
;
; :Returns:
;   A list, where each element is the array of vertices belonging to that line/polygon
;   as specified by the input connectivity array.
;
; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), Feb/2016
;-
function pp_connectivity_list,conn
compile_opt idl2,logical_predicate
l=list()
curr=0LL
while curr lt n_elements(conn) do begin
  count=conn[curr]
  l.add,conn[curr+1:curr+count]
  curr+=count+1
endwhile
return,l
end
