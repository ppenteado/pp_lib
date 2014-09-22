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
;    reverse_hash : out, optional
;      A hash of reverse indices: has the same number of elements as the output
;      of histogram(), where each element contains an array containing the indices of
;      the elements of the input array that fall into the corresponding bin. If
;      a bin is empty, the corresponding hash element is !null. The key for each element
;      is the bin location (as returned by the locations keyword).
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
;     vals=randomu(0L,15)*13d0
;     h=histogram_pp(vals,reverse_indices=ri,reverse_list=rl,locations=loc,reverse_hash=rh)
;     print,h
;     ;1           3           0           4           2           1           0           4
;     print,ri[ri[0]:ri[1]-1]
;     ;13
;     foreach el,rl,i do print,i,' : ',el
;     ;0 :           13
;     ;1 :            8          11          12
;     ;2 : !NULL
;     ;3 :            0           1           4           6
;     ;4 :            9          10
;     ;5 :            2
;     ;6 : !NULL
;     ;7 :            3           5           7          14
;     foreach el,loc do print,el,' : ',rh[el]
;     ;3.8679500 :           13
;     ;4.8679500 :            8          11          12
;     ;5.8679500 : !NULL
;     ;6.8679500 :            0           1           4           6
;     ;7.8679500 :            9          10
;     ;8.8679500 :            2
;     ;9.8679500 : !NULL
;     ;10.867950 :            3           5           7          14
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
  rh=!version.release ge '8.3' ? orderedhash(locations) : hash(locations)
  foreach el,h,i do if (el gt 0L) then rh[locations[i]]=ri[ri[i]:ri[i+1]-1]
endif 
return,h
end
