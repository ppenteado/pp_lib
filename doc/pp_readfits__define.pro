; docformat = 'rst rst'
;+
; :Author: Paulo Penteado (pp.penteado@gmail.com), Dec/2010
; 
; :Requires:
;   Several routines from the `idlastro library <http://idlastro.gsfc.nasa.gov>`.
;-

function pp_readfits::init,file
compile_opt idl2,logical_predicate

;Initialize fields
self.data=ptr_new(!null)
self.header=ptr_new(!null)
self.variables=hash()
self.descriptions=hash()

;Read the file
data=readfits(file,header)
if array_equal(data,-1L) then return,0

;Parse the header
foreach el,header do begin
  tmp=strsplit(el,'=',/extract)
  if (n_elements(tmp) ge 2) then begin
    key=strtrim(tmp[0],2)
    tmp=strsplit(strjoin(tmp[1:*],'='),'/',/extract)
    (self.variables)[key]=strtrim(strjoin(tmp[0:-2],'/'),2)
    (self.descriptions)[key]=strtrim(tmp[-1],2)
  endif 
endforeach

;Store things into self
self.file=file
*self.data=temporary(data)
*self.header=temporary(header)
return,1
end

pro pp_readfits::getproperty,file=file,data=data,header=header,variables=variables,$
 descriptions=descriptions
compile_opt idl2,logical_predicate
if arg_present(file) then file=self.file
if arg_present(data) then data=*self.data
if arg_present(header) then header=*self.header
if arg_present(variables) then variables=(self.variables)[*]
if arg_present(descriptions) then descriptions=(self.descriptions)[*]
end

function pp_readfits::_overloadBracketsRightSide, isRange, $
   sub1, sub2, sub3, sub4, sub5, sub6, sub7, sub8
compile_opt idl2,logical_predicate
;Initialize the return value as an empty hash (which will be returned if nothing
;is found).
ret=hash()
;Get out if provided a range or more than 1D
if (n_elements(isrange) ne 1) || (isrange[0] ne 0) then return,ret
;Find the matching keys, if any
keys=(self.variables.keys()).toarray()
foreach el,sub1 do begin
  w=where(strmatch(keys,el,/fold_case),nw)
  if (nw eq 1) then ret[keys[w]]=(self.variables)[keys[w]]
  if (nw gt 1) then ret+=(self.variables)[keys[w]]
endforeach
return,ret
end

pro pp_readfits__define
compile_opt idl2,logical_predicate
!null={pp_readfits,file:'',data:ptr_new(),header:ptr_new(),$
  variables:obj_new(),descriptions:obj_new(),$
  inherits IDL_Object}
end
