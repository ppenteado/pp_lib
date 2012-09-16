; docformat = 'rst'
;+
; :Description:
;    A simple wrapper to histogram(), to create a new output keyword,
;    reverse_list, which provides equivalent data as reverse_indices, but parsed
;    into a list (with !null for empty bins). See the help on histogram() for
;    anything besides the reverse_list keyword.
;
; :Params:
;    array:  in, required
;      Passed on, unaltered, to histogram(), its only positional parameter. See
;      the help on histogram() for more details.
;
; :Keywords:
;    reverse_list : out, optional
;      A list of reverse indices: has the same number of elements as the output
;      of histogram(), where each element is an array containing the indices of
;      the elements of the input array that fall into the corresponding bin. If
;      a bin is empty, the corresponding list element is !null.
;    reverse_indices : out, optional
;      The reverse indices, as an array, unaltered from the output of
;      histogram(). See the help on histogram() for more details on
;      reverse_indices.
;    _ref_extra : in, out, optional
;      Any other keywords are passed on (strictly) to histogram(), and returned
;      unaltered. See the help on histogram() for these keywords.
;      
; :Examples:
;   Make up a few random values and see the list returned by histogram_pp()::
;   
;     vals=randomu(0L,15)*7d0
;     print,vals
;     ;  2.9119955      0.64375425       5.2948734       3.7079015       6.5130554       2.6845145       4.5774329      0.46789565       5.0586230       4.6980456
;     ;  2.6839095       4.4214430       6.1929501       3.6359147       4.5606301
;     h=histogram_pp(vals,reverse_indices=ri,reverse_list=rl)
;     print,h
;     ;      2           0           3           3           5           1           1
;     print,ri[ri[0]:ri[1]-1]
;     ;      1           7
;     foreach el,rl,i do print,i,' : ',el
;     ;      0 :            1           7
;     ;      1 : !NULL
;     ;      2 :            0           5          10
;     ;      3 :            3          11          13
;     ;      4 :            2           6           8           9          14
;     ;      5 :           12
;     ;      6 :            4
;      
; :Requires: IDL 8.0.
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), July/2010
;-
function histogram_pp,array,reverse_list=rl,reverse_hash=rh,$
 reverse_indices=ri,locations=locations,_ref_extra=_ex
compile_opt idl2, logical_predicate
h=histogram(array,_strict_extra=_ex,reverse_indices=ri,locations=locations)
if arg_present(rl) then begin
  rl=list(length=n_elements(h))
  foreach el,h,i do if (el gt 0L) then rl[i]=ri[ri[i]:ri[i+1]-1]
endif
if arg_present(rh) then begin
  rh=hash(locations)
  foreach el,h,i do if (el gt 0L) then rh[locations[i]]=ri[ri[i]:ri[i+1]-1]
endif 
return,h
end
