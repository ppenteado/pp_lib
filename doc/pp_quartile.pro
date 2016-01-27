; docformat = 'rst'
;+
; :Author: Paulo Penteado (http://www.ppenteado.net), Jan/2016
;-
;+
; :Description:
;    Given an array iarr, returns the value(s) in the array that correspond to
;    the given quartile(s). If `cut` is set, instead of the quartile values, returns
;     a copy of the array, with minimum and maximum values given by the two quartiles provided.
;
; :Params:
;    iarr: in, required
;      The array for which the quartile(s) will be calculated.
;    quart: in, required
;      The desired quartile(s). It can be a scalar or an array.
;
; :Keywords:
;    cut: in,optional
;      If set, `quart` must be a two-element `array`. Instead of returning quartile
;      values, the function will return a copy of the array, with all values below the
;      low quartile (the first element in `quart`) will be set to the array's value
;      at that quartile, and all values above the high quartile (the second element
;      in `quart`) will be set to the value of the high quartile.
;    nan: in, optional
;      If the array contains non-finite values (NaN or Infinity), this must be set
;      so that the quartiles are correctly calculated. By default, it is set. Setting
;      nan to zero saves time, but if the array contains non-finite values and nan
;      is zero, incorrect results may be returned.
;    sort: out,optional
;      An array with the indices to sort the input array: s=sort(iarr)
;    index: out,optional
;      An array with the index in `s` corresponding to the quartiles selected. Useful
;      to obtain indices in `iarr` that match some quartile level. See example
;      
; :Examples:
; 
;   Create an array with 10 values between 0 and 9 and check on some of its quartiles::
;   
;     a=reverse(dindgen(10))
;     print,pp_quartile(a,[0.2d0,0.8d0])
;     ;       2.0000000       8.0000000
;     print,pp_quartile(a,[0.2d0,0.8d0],/cut)
;     ;       8.0000000       8.0000000       7.0000000       6.0000000       5.0000000
;     ;       4.0000000       3.0000000       2.0000000       2.0000000       2.0000000
;     
;    Obtain the elements of a which are below the 50% quartile::
;    
;      a_50=pp_quartile(a,0.5d0,index=ind,sort=s)
;      print,a[s[0:ind]]
;      ;       0.0000000       1.0000000       2.0000000       3.0000000       4.0000000       5.0000000
;
; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>` ), Jan/2016
;-
function pp_quartile,iarr,quart,cut=cut,nan=nan,index=index,sort=s
compile_opt idl2,logical_predicate
  nan=n_elements(nan) ? nan : 1B
  if nan then begin
    w=where(finite(iarr))
    arr=iarr[w]
  endif else arr=iarr
  s=sort(arr)
  narr=n_elements(arr)
  index=0>round(narr*quart)<(narr-1)
  q=arr[s[index]]
  if nan then arr=iarr
  return,keyword_set(cut) ? q[1]<arr>q[0] : q
end
