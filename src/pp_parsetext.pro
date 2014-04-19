function pp_parsetext,file,header=header,lines=lines,splitlines=liness,as_struct=as_struct,$
fieldnames=fieldnames,types=types,trim=trim,spacedelimited=spacedelimited,skipblank=skipblank,$
delimiter=delimiter,stripquotes=stripquotes
compile_opt idl2,logical_predicate
trim=n_elements(trim) ? trim : 2
spacedelimited=keyword_set(spacedelimited)
stripquotes=keyword_set(stripquotes)
lines=pp_readtxt(file)
if keyword_set(skipblank) then begin
  lines=lines[where(strtrim(lines,2) ne '',/null)]
endif
header=lines[0]
lines=lines[1:-1]
s=strsplit(header)
e=[s[1:-1],max(strlen(lines))]
l=e-s
fn=n_elements(delimiter) ? strsplit(header,delimiter,/extract) : strsplit(header,/extract)

liness=spacedelimited ? transpose((strsplit(lines,/extract)).toarray()) : ( n_elements(delimiter) ? transpose((strsplit(lines,delimiter,/extract)).toarray()) : strmid(lines,s,l))
if stripquotes then begin
  w=where(stregex(liness,'"(.*)"',/boolean),count)
  if count then liness[w]=(stregex(liness[w],'"(.*)"',/subexpr,/extract))[-1,*]
  w=where(stregex(fieldnames,'"(.*)"',/boolean),count)
  if count then fieldnames[w]=(stregex(fieldnames[w],'"(.*)"',/subexpr,/extract))[-1,*]
endif
fieldnames=idl_validname(fn,/convert_all)
if trim then liness=strtrim(liness,trim)
if keyword_set(as_struct) then begin
  ret={}
  typeh=n_elements(types) ? types[*] : hash()
  foreach field,fieldnames,ifield do begin
    if ~typeh.haskey(field) then begin
      isint=array_equal(minmax(pp_isnumber(liness[ifield,*],/integer)),[1,1])
      isdouble=array_equal(minmax(pp_isnumber(liness[ifield,*],/nan,/infinity)),[1,1])
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
