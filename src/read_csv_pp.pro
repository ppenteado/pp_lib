; docformat = 'rst'

;+
; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), Feb/2013
;-
;+
; :Description:
;    A simple wrapper for read_csv_pp_strings, to assign field names based on either the file's column headers,
;    or a user-supplied string array.
;
; :Params:
;    filename: in, required, type=string
;      The name of the CSV file to read.
;
; :Keywords:
;    _ref_extra: in, out, optional
;      Any other arguments are simply passed to/from read_csv_pp_strings, unaltered.
;    field_names: in, optional, type=strarr(nfields)
;      A string array with the name to assign to each field (column) in the output. If provided, this
;      overrides the auto column names derived from the csv header line.
;      
; :Examples:
;    To read IDL's example csv file::
;    
;      c=read_csv_pp(file_which('ScatterplotData.csv'),n_table_header=1,header=h)
;      help,c
;      ;** Structure <ec157fb8>, 3 tags, length=3080, data length=3080, refs=1:
;      ;DISTANCE_FROM_TERMINUS__METERS_   LONG      Array[154]
;      ;MEAN_PARTICLE_SIZE__MM_           DOUBLE    Array[154]
;      ;SEDIMENTATION_RATE__G_CM2YR_      DOUBLE    Array[154]
;      ;print,h
;      ;Distance from Terminus (meters) Mean Particle size (mm) Sedimentation Rate (g/cm2yr)
;      
; :Requires: `pp_isnumber`, `read_csv_pp_strings`
;    
;    
;
; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), Feb/2013
;-
function read_csv_pp,filename,header=header,_ref_extra=ex,field_names=fn
compile_opt idl2,logical_predicate
c=read_csv_pp_strings(filename,_strict_extra=ex,header=header)
if (n_elements(header) eq 0) then begin
  header=[]
  foreach el,tag_names(c),i do header=[header,(c.(i))[0]]
endif
nt=n_elements(tag_names(c))
fn=n_elements(fn) eq nt ? idl_validname(fn,/convert_all) : idl_validname(header,/convert_all)
;Check for repeated column names
fno=strupcase(fn)
while 1 do begin
  names=hash()
  restart=0
  for i=0,n_elements(fno)-1 do begin
    if names.haskey(fno[i]) then begin
      fno[i]+='_2'
      restart=1
      break
    endif else names[fno[i]]=!null
  endfor
  if restart then continue else break
endwhile
fn=fno
ret=!null
foreach el,tag_names(c),i do ret=create_struct(ret,fn[i],c.(i))
return,ret
end
