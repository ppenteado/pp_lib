; docformat = 'rst'

;+
; :Author: Paulo Penteado (http://www,ppenteado.net), Jan/2013
;-
;+
; :Description:
;    Creates a structure that has a subset of the fields from the input structure.
; 
; :Returns:
;    A structure with a subset of the fields from the input structure,
;     specified by a string array with the field names, in the desired order.
;     If the order of the fields did not matter, this could be done easily with
;      a hash. 
;
; :Params:
;    structin : in, required
;      The input structure, from which a subset will be taken.
;    fields : in, required
;      A string array with each element specifying the name of a field of
;      structin to go into the output structure.
;      
; :Examples:
; 
;   Make a simple structure and subset it::
;   
;     structin={a:18,b:'something',c:[0,9],d:[-9.5d0,4.85d0,17d0]}
;     help,pp_structsubset(structin,['d','b'])
;     ;** Structure <a413a488>, 2 tags, length=40, data length=40, refs=1:
;     ;D               DOUBLE    Array[3]
;     ;B               STRING    'something'
;     print,pp_structsubset(structin,['d','b'])
;     ;{      -9.5000000       4.8500000       17.000000
;     ;something}
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Jan/2013
;-
function pp_structsubset,structin,fields
compile_opt idl2,logical_predicate
on_error,2
tn=tag_names(structin)
fi=strupcase(fields)
;Check if all field names are valid
nfi=n_elements(fi)
val=lonarr(nfi)
sfi=fi[sort(fi)]
ufi=sfi[uniq(sfi)]
if n_elements(ufi) ne nfi then begin
  vl=value_locate(ufi,fi)
  h=histogram(vl,binsize=1,min=0)
  w=where(h gt 1)
  message,string('There are repeated fields: ',strjoin(ufi[w],','))
endif
for i=0,nfi-1 do begin
  w=where(tn eq fi[i],count)
  val[i]+=count
endfor
w=where(val eq 0,count)
if count then message,string('Fields not found: ',strjoin(string(fi[w]),','),' (elements ',strjoin(strtrim(w,2),','),')')
structout=create_struct(fi[0],structin.(where(tn eq fi[0])))
for i=1,nfi-1 do structout=create_struct(structout,fi[i],structin.(where(tn eq fi[i])))
return,structout
end
