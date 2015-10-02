; docformat = 'rst'
;+
; :Author: Paulo Penteado (http://www.ppenteado.net), Mar/2015
;-
;+
; :Description:
;    Parses table data in a text file (or text array) as an array, with multiple options to specify different
;    file formats and processing to be applied to the file. The output can be a string array or an
;    array of structures.
;
; :Params:
;   file: in, required
;    A string with the file name to read. If `buffer` is set, this should be a string
;    array, where each element correponds to what would be a file line.
;
; :Keywords:
;    header: out, optional
;      The header line(s) from the text file, unparsed. The number of header lines
;      is set by `nheader`.
;    lines: out, optional
;      A string array, with one element per line in the input file.
;    splitlines: out, optional
;      A list where each element is a string array corresponding to one line of the
;      input file. Each element in the array is one column from that file line.
;    as_struct: in, optional, default=0
;      If set, the output is an array of structures, one structure per input line.
;    fieldnames: in, out, optional
;      The names for the structure fields returned when `as_struct` is set.
;      If this is not given, field names are taken from the last line of the file header
;    types: in, out, optional
;      A hash containing type specifications for each of the structure fields to be created
;      when `as_struct` is set. If not given, it will be determined by guessing from the
;      file's column contents.
;    trim: in, optional, default=2
;      Determines the type of leading/trailing trimming to be applied to the file lines. It is
;      passed to strtrim, which is applied to all file lines.
;    spacedelimited: in, optional, default=0
;      If set, the columns are assumed to be separated by any positive number of blank
;      spaces. If not set, the columns are assumed to be fixed length, equal to the
;      lengths used in the header line.
;    skipblank: in, optional, default=0
;      If set, blank lines in the file are skipped.
;    delimiter: in, optional
;      The character(s) used as column delimiter in the file (the columns are split
;      with strplit). If not given, the input columns are assumed to be separated by blank space.
;    stripquotes: in, optional, default=0
;      If set, table elements enclosed in quotes will have the quotes removed.
;    isinteger: out, optional
;      If provided, will return a list, with one element per column of the file. Each element
;      is an array that informs whether the corresponding column element in the input is an integer.
;      Most often used for debugging and finding anomalous values in the input. 
;    isfloat: out, optional
;      If provided, will return a list, with one element per column of the file. Each element
;      is an array that informs whether the corresponding column element in the input is a float.
;      Most often used for debugging and finding anomalous values in the input.
;    missingint: in, optional
;      If provided, any missing values in columns with integers will be filled with this value.
;    missingfloat: in,optional
;      If provided, any missing values in columns with floats will be filled with this value.
;    blank: in, optional
;      Passed to `pp_isnumber`. If set, blank strings are considered valid numbers.
;    buffer: in,optional, default=0
;      If set, the first argument (`file`) is taken as a string array of the file contents,
;      instead of a file name to be read.
;    nheader: in, optional, default=1
;      The number of header lines contained in the file. If `as_struct` is set and
;      field names are not provided, the last line on the header is used to determine
;      column names.
;      
;
; :Examples:
;
;    Read some example files provided with IDL, as structures::
;
;      file=filepath('ascii.txt',subdirectory=['examples','data'])
;      a=pp_parsetext(file,/skipblank,nheader=4,header=header,delimiter=',',$
;      /as_struct,fieldnames=['lon','lat','el','temp','dew','speed','dir'])
;      help,a
;      ;A               STRUCT    = -> <Anonymous> Array[15]
;      ;help,a[0]
;      ;** Structure <4023e918>, 7 tags, length=56, data length=56, refs=2:
;      ;LON             DOUBLE          -156.95000
;      ;LAT             DOUBLE           20.783300
;      ;EL              LONG64                       399
;      ;TEMP            LONG64                        68
;      ;DEW             LONG64                        64
;      ;SPEED           LONG64                        10
;      ;DIR             LONG64                        60
;      print,header
;      ;This file contains ASCII format weather data in a comma delimited table with comments prefaced by the "%" character. The columns represent:
;      ;Longitude, latitude, elevation (in feet), temperature (in degrees F),  dew point (in degrees  F), wind speed (knots), wind direction (degrees)
;
; :Requires: `pp_isnumber`, `pp_readtxt`
;
; :Todo:
;   Expand documentation, with more examples. This function has received many options
;   to be capable of parsing different kinds of text files I encounter, which means
;   its options make for a large variety of possibilities in file formats.
;
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Mar/2015
;
function pp_parsetext,file,header=header,lines=lines,splitlines=liness,as_struct=as_struct,$
fieldnames=fieldnames,types=types,trim=trim,spacedelimited=spacedelimited,skipblank=skipblank,$
delimiter=delimiter,stripquotes=stripquotes,isinteger=isinteger,isfloat=isfloat,$
missingint=missingint,missingfloat=missingfloat,blank=blank,buffer=buffer,nheader=nheader
compile_opt idl2,logical_predicate
trim=n_elements(trim) ? trim : 2
spacedelimited=keyword_set(spacedelimited)
stripquotes=keyword_set(stripquotes)
replaceints=n_elements(missingint)
replacefloats=n_elements(missingfloat)
blank=keyword_set(blank)
buffer=keyword_set(buffer)
delimiter=n_elements(delimiter) ? delimiter : !null

if buffer then lines=file else lines=pp_readtxt(file)
if keyword_set(skipblank) then begin
  lines=lines[where(strtrim(lines,2) ne '',/null)]
endif
;header=lines[0]
;lines=lines[1:-1]
nheader=n_elements(nheader) ? nheader : 1L
header=nheader ? lines[0:nheader-1] : !null
if header eq !null then begin
  ncol=n_elements(strsplit(lines[0],/extract))
  header='field_'+strtrim(sindgen(ncol))
endif
lines=lines[nheader:-1]
s=n_elements(delimiter) ? strsplit(header[-1],delimiter) : strsplit(header[-1])
e=[s[1:-1],max(strlen(lines))]
l=e-s
fn=n_elements(delimiter) ? strsplit(header[-1],delimiter,/extract) : strsplit(header[-1],/extract)

liness=spacedelimited ? transpose((strsplit(lines,/extract)).toarray()) : ( n_elements(delimiter) ? transpose((strsplit(lines,delimiter,/extract)).toarray()) : strmid(lines,s,l))
if stripquotes then begin
  w=where(stregex(liness,'"(.*)"',/boolean),count)
  if count then liness[w]=(stregex(liness[w],'"(.*)"',/subexpr,/extract))[-1,*]
  w=where(stregex(fn,'"(.*)"',/boolean),count)
  if count then fn[w]=(stregex(fn[w],'"(.*)"',/subexpr,/extract))[-1,*]
endif
;fieldnames=idl_validname(fn,/convert_all)
fieldnames=n_elements(fieldnames) ? idl_validname(fieldnames,/convert_all) : idl_validname(fn,/convert_all)
if trim then liness=strtrim(liness,trim)
isinteger=arg_present(isinteger) ? list() : !null
isfloat=arg_present(isfloat) ? list() : !null
if keyword_set(as_struct) then begin
  ret={}
  typeh=n_elements(types) ? types[*] : hash()
  foreach field,fieldnames,ifield do begin
    if ~typeh.haskey(field) then begin
      tmpi=reform(pp_isnumber(liness[ifield,*],/integer,blank=(blank or replaceints)))
      if replaceints then begin
        wi=where(liness[ifield,*] eq '',counti)
        liness[ifield,wi]=missingint
      endif
      if isinteger ne !null then isinteger.add,tmpi
      isint=array_equal(minmax(tmpi),[1,1])
      tmpf=reform(pp_isnumber(liness[ifield,*],/nan,/infinity,blank=(blank or replacefloats)))
      if replacefloats then begin
        wf=where(liness[ifield,*] eq '',countf)
        liness[ifield,wf]=missingfloat
      endif
      if isfloat ne !null then isfloat.add,tmpf
      isdouble=array_equal(minmax(tmpf),[1,1])
      case 1 of
        isint: typeh[field]=0LL
        isdouble: typeh[field]=0d0
        else: typeh[field]=''
      endcase
    endif
    ret=create_struct(ret,field,typeh[field])
  endforeach
  ret=replicate(ret,n_elements(lines))
  foreach field,fieldnames,ifield do begin
    ret.(ifield)=reform(liness[ifield,*])
  endforeach
endif else ret=liness
return,ret
end
