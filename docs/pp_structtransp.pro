; docformat = 'rst'

;+
; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), Feb/2013
;-
;+
  ; :Description:
  ;    Given a structure of arrays, creates its transpose (an array of structures). Useful
  ;    for the output of read_csv and read_ascii, to make a structure array where each element
  ;    is a row of the table (see example below).
  ;    
  ; :Returns:
  ;    An array of structures, where the fields in each element have the values from
  ;    the corresponding element of the same fields in the input structure (see eaxmple below).
  ;
  ; :Params:
  ;    strin: in, required
  ;      The structure to be transposed. See example below.
  ;      
  ; :Examples:
  ; 
  ;    Read the contents of IDL's example csv file and transpose them::
  ;    
  ;      c=read_csv_pp(file_which('ScatterplotData.csv'),n_table_header=1)
  ;      help,c
  ;      ;** Structure <edac6838>, 3 tags, length=3080, data length=3080, refs=1:
  ;      ;DISTANCE_FROM_TERMINUS__METERS_         LONG      Array[154]
  ;      ;MEAN_PARTICLE_SIZE__MM_                 DOUBLE    Array[154]
  ;      ;SEDIMENTATION_RATE__G_CM2YR_            DOUBLE    Array[154]
  ;      help,pp_structtransp(c)
  ;      ;<Expression>    STRUCT    = -> <Anonymous> Array[154]
  ;      help,(pp_structtransp(c))[0]
  ;      ;** Structure <ec40f7f8>, 3 tags, length=24, data length=20, refs=1:
  ;      ;DISTANCE_FROM_TERMINUS__METERS_         LONG                 0
  ;      ;MEAN_PARTICLE_SIZE__MM_                 DOUBLE         0.062000000
  ;      ;SEDIMENTATION_RATE__G_CM2YR_            DOUBLE           32.500000
  ;
  ;
  ;
  ; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), Feb/2013
  ;-
function pp_structtransp,strin
compile_opt idl2,logical_predicate
names=tag_names(strin)
ret=!null
for i=0,n_elements(names)-1 do ret=create_struct(ret,names[i],(strin.(i))[0])
ret=replicate(ret,n_elements(strin.(0)))
for i=0,n_elements(names)-1 do begin
  catch,err 
  if err then begin
    catch,/cancel
    ret[*].(i)=strin.(i).toarray()
  endif else ret[*].(i)=strin.(i)
endfor
return,ret
end
