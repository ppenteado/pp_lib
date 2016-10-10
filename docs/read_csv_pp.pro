; docformat = 'rst'

;+
; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), Feb/2013
;-
;+
; :Description:
;    A simple wrapper for `read_csv_pp_strings`, to assign field names based on either the file's column headers,
;    or a user-supplied string array. It uses `read_csv_pp_strings`, instead of IDL's read_csv,
;    due to its superior handling of columns types. 
;    
; :Returns:
;    A structure of arrays, where each field corresponds to a column read from the input file. 
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
;    blank: in, optional, default=0
;      If set, blank (empty / whitespace) strings are allowed in numeric columns: if a column
;       contains numbers and blank values, its type will be numeric, and any blanks will be replaced
;       with 0. When not set (default), a column containing blanks will be returned as strings.
;       Note that, due to the way the original read_csv operates, a colum consisting entirely of blanks
;       will be returned a string column.
;    transp: in, optional, default=0
;      If set, return the transpose of the default output - shorter than writing pp_structtransp(read_csv_pp()).
;      This is a structure array, where each element is a row in the file, instead of a structure with
;      array fields (one per column).
;    rows_for_testing: in, optional, default=100
;      The number of rows in the file to use when testing for column types. If set to 0, all rows all used.
;    types: in, out, optional
;      An array of type codes. If provided with values, these types are assumed for the columns, instead of trying
;       to determine the column types. If provided as an undefined array, the typecodes found for the file are returned
;       in this array.
;    nan: in, optional
;       If set, NaNs are allowed as floating-point numbers.
;    infinity: in, optional
;       If set, infinities are allowed as floating-point numbers.
;    integer: in, optional
;       If set, only integers are considered to be numeric types: floating-point numbers will remain
;       as strings.
;    trim: in, optional
;       If set, this keyword is passed to strtrim(), which gets applied to each element being tested
;       to determine if it is a number. 
;        
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
;    To read that same file, but get the result as a structure array (one element per table row)::
;    
;      c=read_csv_pp(file_which('ScatterplotData.csv'),n_table_header=1,header=h,/transp)
;      help,c
;      ;C               STRUCT    = -> <Anonymous> Array[154]
;      help,c[0]
;      ;** Structure <98322d88>, 3 tags, length=24, data length=20, refs=2:
;      ;   DISTANCE_FROM_TERMINUS__METERS_ LONG                 0
;      ;   MEAN_PARTICLE_SIZE__MM_         DOUBLE         0.062000000
;      ;   SEDIMENTATION_RATE__G_CM2YR_    DOUBLE           32.500000
;      
; :Requires: `pp_isnumber`, `read_csv_pp_strings`, `pp_structtransp`
;    
;    
;
; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), Feb/2013
;-
function read_csv_pp,filename,header=header,_ref_extra=ex,field_names=fn,blank=blank,transp=transp,$
  rows_for_testing=rows_for_testing,types=types,$
  nan=nan,infinity=infinity,integer=integer,trim=trim
compile_opt idl2,logical_predicate
c=read_csv_pp_strings(filename,_strict_extra=ex,header=header,blank=blank,$
  rows_for_testing=rows_for_testing,types=types,nan=nan,infinity=infinity,integer=integer,trim=trim)
if (n_elements(header) ne n_tags(c)) || (strtrim(strjoin(header),2) eq '') then begin
  header=strarr(n_tags(c))
  foreach el,tag_names(c),i do header[i]=(c.(i))[0]
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
return,keyword_set(transp) ? pp_structtransp(ret) : ret
end
