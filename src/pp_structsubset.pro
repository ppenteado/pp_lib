;+
; :Description:
;    Returns a structure with a subset of the fields from the input structure,
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
;   
;   
;
;
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Jan/2013
;-
function pp_structsubset,structin,fields
compile_opt idl2,logical_predicate
tn=tag_names(structin)
fi=strupcase(fields)
structout=create_struct(fi[0],structin.(where(tn eq fi[0])))
for i=1,n_elements(fi)-1 do structout=create_struct(structout,fi[1],structin.(where(tn eq fi[i])))
return,structout
end
