; docformat = 'rst rst'
;+
;-
;+
; :Description:
;    Parses a text file or a string array into a hash. The text is interpreted
;    as lines containing key=value pairs, and can contain comments. Values can
;    can be converted to integers or reals, if possible.
;    Values are returned as a (possibly empty) hash, where all keys are strings.
;    
;    The complement to this function is `pp_writepars`.
;
; :Params:
;    input : in, required
;      If `input_is_strings` is not set, this is a string with the name of a text
;      file to read and parse. Otherwise, it is a string array, interpreted in the
;      same way a file would be (each element corresponding to one file line).
;
; :Keywords:
;    numbers : in, optional, default=1
;      Sets the type of conversion for values which are numbers, if any. If set to
;      zero, no conversion is made (all values are returned as strings). If set to
;      1, values which are integers are returned as long64s, and values which are reals
;      are returned as doubles.
;    key_case : in, optional, default=0
;      If set to 1, keys are converted to all uppercase. If set to 2, keys are converted
;      to all lowcase.
;    nan : in, optional, default=1
;      Passed on to pp_isnumber. If set, NaNs (in several spellings) are accepted
;      when testing if a value is a real number.
;    infinity : in, optional, default=1
;      Passed on to pp_isnumber. If set, infinities (in several spellings) are accepted
;      when testing if a value is a real number.
;    input_is_strings : in, optional, default=0
;      If set, `input` is treated as a string array, where each element is a line
;      to parse. Otherwise, `input` is treated as the name of a text file to read
;      and parse.
;    comment_marker : in, optional, default='#'
;      The character to use to mark comments in the text to process. Anything in
;      a line following this marker is ingored (even if it is escaped or inside
;      quotes). If something different from deafult is given, it must be a valid
;      regular expression (that is, especial regex characters must be escaped). 
;    value_separator : in, optional, default='='
;      The character to use to separate keys from values. If anyhting other than
;      default is used, it should be kept in mind that this is the separator given
;      to strsplit. Thus it follows strsplit's rules.
;    comment_lines : out, optional
;      Provides a string array with all lines that were not put into the hash (comment
;      lines, or lines that were not recognized as containing a valid key=value).
;    
; :Examples:
; 
; Make up some content to read, in the form of a string array::
; 
;   example_string=['#some comments','a=1 #some other comments',' b = -9',"c = 'some string' #some more comments",'d=98.765d-6 #in some units']
;   foreach el,example_string do print,el
;   ;#some comments
;   ;a=1 #some other comments
;   ; b = -9
;   ;c = 'some string' #some more comments
;   ;d=98.765d-6 #in some units
;   
; Now parse it and chek the results::
; 
;   pars=pp_readpars(example_string,/input_is_string)
;   print,pars
;   ;c: 'some string'
;   ;a:                      1
;   ;b:                     -9
;   ;d:    9.8765000e-05
;   foreach el,pars do help,el
;   ;EL              STRING    = ''some string''
;   ;EL              LONG64    =                      1
;   ;EL              LONG64    =                     -9
;   ;EL              DOUBLE    =    9.8765000e-05
;   
; :Uses: pp_isnumber
; 
; :Todo:
;   Support values as arrays. For now, the value array would be returned as
;   a string, which could be parsed afterwards.
;   Support returning comments from the same lines as values. For now, comments
;   following values are discarded (comments in standalone lines, and lines not
;   recognized are valid key=value, can be retrieved with ``comment_lines``). 
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Feb/2011
;-
function pp_readpars,input,numbers=numbers,key_case=kcase,nan=nan,infinity=infinity,$
 input_is_strings=inpstr,comment_marker=comment,value_separator=value_separator,$
 comment_lines=comment_lines
compile_opt idl2, logical_predicate

;Defaults
numbers=n_elements(numbers) eq 1 ? numbers : 1B
kcase=n_elements(kcase) eq 1 ? kcase : 0B
inpstr=n_elements(inpstr) eq 1 ? inpstr : 0B
comment_marker=n_elements(comment_marker) eq 1 ? comment_marker : '#'
value_separator=n_elements(value_separator) eq 1 ? value_separator : '='


;Read the file into strings
if (inpstr) then lines=input else begin
  nlines=file_lines(input)
  lines=strarr(nlines)
  openr,unit,input,/get_lun
  readf,unit,lines
  free_lun,unit
endelse

;Parse the lines
ret=hash()
comment_lines=list()
lines=strtrim(lines,2)
;Locate the comments (everything following a '#', even if the '#' is quoted or escaped)
slines=strsplit(lines,comment_marker,/regex,/extract,/preserve_null)
foreach line,slines,i do begin
  if line[0] then begin ;Skip lines that are entirely comments, or are empty
    tmp=strsplit(line[0],value_separator,/extract,count=count)
    if (count ge 2) then begin ;Skip line if it does not have more than two parts separated by '='
      key=strtrim(tmp[0],2)
      case kcase of ;Change key case if selected
        1: key=strupcase(key)
        2: key=strlowcase(key)
        else:
      endcase
      value=strtrim(strjoin(tmp[1:-1],'='),2) ;Reassemble value if it was separated in pieces by '='
      case numbers of ;If value is to be converted to number (if possible)
        1 : value=pp_isnumber(value,/integer) ? long64(value) : pp_isnumber(value,infinity=infinity,nan=nan) ? double(value) : value
        2 : value=pp_isnumber(value,infinity=infinity,nan=nan) ? double(value) : value
        else :
      endcase
    endif else comment_lines.add,lines[i]
    ret[key]=value 
  endif else comment_lines.add,lines[i]
endforeach
comment_lines=comment_lines.toarray()
return,ret
end
