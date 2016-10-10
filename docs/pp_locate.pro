; docformat = 'rst'

;+
; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), May/2013
;-
;+
; :Description:
;    Locates all the occurrences of all different (unique) values in the input array, similarly
;    to what would be produced by running a where() on each unique value of `array`, but this is
;    much more efficient, since only one pass is done through the array.
;    
; :Returns:
;    A hash where the keys are the unique values present in `array`, and the values
;    are the indices where they occur. See the example below.
;
; :Params:
;    array : in, required
;      An array of any type that can be sorted (integers, reals, strings, etc).
;      If `array` is sorted, the `no_sort` keyword can be set, to avoid spending time
;      computing a sort() of `array`.
;      
; :Keywords:
;    histogram : out, optional
;      A hash where the keys are the unique values present in `array`, and the values
;      are the number of occurrences in `array`.
;      unique_values : out, optional
;      An array containing the unique values present in `array`.
;    sorted_values : out, optional
;      A sorted copy of `array`.
;    no_sort : in, optional
;      If set, pp_locate assumes `array` is ordered. By default, pp_locate does
;      not make this assumption, and will have to compute a sort() on `array`. If you
;      know `array` is sorted, this will save some time. If `no_sort` is set, but
;      `array` is not sorted, the results may be incorrect.
;    unique_indices : out, optional
;      The array of indices into the original array of the unique elements.
;    sort_indices : out, optional
;      The array of indices into `array` to make it ordered: sort_indices=sort(array).
;    reverse_sort : out, optional
;      An array with indices into the sorted version of array, to map the elements
;      back into the original order: while sorted_values=array[sort_indices],
;      sorted_values[reverse_sort]=array.
;      
; :Examples:
; 
;    Create an array containing several repetitions of a few strings, and locate
;    all the occurences of each unique string::
;
;      array=['a','j','kk','a','a','b','zrdc','29','b','29','-19','0']
;      loc=pp_locate(array,sorted_values=sarray,unique_values=uarray,histogram=h)
;      help,loc
;      ;LOC             HASH  <ID=140  NELEMENTS=8>
;      print,loc
;      ;zrdc:                      6
;      ;a:                      0                     3                     4
;      ;j:                      1
;      ;0:                     11
;      ;-19:                     10
;      ;b:                      5                     8
;      ;kk:                      2
;      ;29:                      7                     9
;      foreach el,uarray do print,el,':',h[el]
;      ;-19:                     1
;      ;0:           1
;      ;29:           2
;      ;a:           3
;      ;b:           2
;      ;j:           1
;      ;kk:           1
;      ;zrdc:           1
;
; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), May/2013
;-
function pp_locate,array,histogram=hh,$
  unique_values=uarray,sorted_values=sarray,no_sort=no_sort,unique_indices=auinds,$
  sort_indices=s,reverse_sort=sr,use_pointers=use_pointers,counts=counts
compile_opt idl2,logical_predicate

mhh=arg_present(hh)

use_pointers=n_elements(use_pointers) ? use_pointers : 0B
make_counts=arg_present(counts)

if mhh then hh=hash()

if n_elements(array) eq 0 then return,ret ;Get out if array is empty

if keyword_set(no_sort) then begin ;Sort if needed
  sarray=array
  s=l64indgen(n_elements(array))
endif else begin
  s=sort(array)
  sarray=array[s]
endelse

sr=(l64indgen(n_elements(array)))[s] ;sr maps sarray back into array: sarray=array[s] and array=sarray[sr]

;Find unique values in the array
uinds=uniq(sarray)
uarray=sarray[uinds]
auinds=s[uinds]

last=0ULL
ret=use_pointers ? {keys:uarray,values:ptrarr(n_elements(uarray))} : hash()
if make_counts then counts=use_pointers ? {keys:uarray,values:0LL} : hash()
foreach el,uinds,i do begin
  nels=el-last+1
  if mhh then hh.set,uarray[i],nels
  els=sr[last+l64indgen(nels)]
  if use_pointers then ret.values[i]=ptr_new(els) else ret.set,uarray[i],els
  if make_counts then begin
    if use_pointers then counts.values[i]=n_elements(els) else counts.set,uarray[i],n_elements(els)
  endif
  last=el+1
endforeach

return,ret
end