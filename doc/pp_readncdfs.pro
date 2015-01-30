; docformat = 'rst rst'
;
;+
; :Author: Paulo Penteado (http://www.ppenteado.net), 2006
;-
;+
; :Description:
;    Reads the dimensions and variables from a netcdf file into a structure or hash.
;
; :Params:
;    ncf: in, required
;      The name of the netCDF file to read.
;
; :Keywords:
;    long: in, optional
;      If set, the output contains an extra field, identifying the names of the dimensions
;      of each variable.
;    
;    verb: hidden
;    
;    hash: in, optional
;      If set, returns a hash instead of a structure.
;    
; :Examples:
; 
;   Read one of IDL's example files::
;     fp=filepath('sample.nc',subdirectory=['examples','data'])
;     fc=pp_readncdfs(fp)
;     ;help,fc
;     Structure <c88e1778>, 3 tags, length=393240, data length=393238, refs=1:
;     ;NCDFNAME        STRING    '/usr/local/exelis/idl84/examples/data/sample.nc'
;     ;D               STRUCT    -> <Anonymous> Array[1]
;     ;V               STRUCT    -> <Anonymous> Array[1]
;     help,fc.d
;     ;** Structure <c8192c18>, 3 tags, length=6, data length=6, refs=2:
;     ;X               INT            512
;     ;Y               INT            768
;     ;Z               INT              0
;     help,fc.v
;     ;** Structure <c8164ee8>, 1 tags, length=393216, data length=393216, refs=2:
;     ;IMAGE           BYTE      Array[768, 512]
;
; :Todo:
;   Process attributes.
;
; :Author: Paulo Penteado (http://www.ppenteado.net), 2006
;-
function pp_readncdfs,ncf,long=long,verb=verb,hash=hash
compile_opt idl2,logical_predicate
;if long is set, structure contains the dimension names for each variable
;Paulo Penteado (http://www.ppenteado.net)
verb=n_elements(verb) eq 1 ? verb : 0
if (n_elements(long) ne 1) then long=0
;on_error,2
if (n_elements(ncf) ne 1) then message,'invalid file name'
ncid=ncdf_open(ncf)
ninq=ncdf_inquire(ncid)

dimlist=strarr(ninq.ndims)
dim=intarr(ninq.ndims)
varlist=strarr(ninq.nvars)

exe2="ret={ncdfname:ncf,d:{"
for i=0,ninq.ndims-1 do begin
  ncdf_diminq,ncid,i,t1,t2
  dimlist[i]=t1
  dim[i]=t2
  exe=dimlist[i]+"=dim[i]"
;  print,exe
  res=execute(exe)
  exe2=exe2+dimlist[i]+":"+dimlist[i]
  if (i eq ninq.ndims-1) then exe2=exe2+"},v:{" else exe2=exe2+","
endfor
for i=0,ninq.nvars-1 do begin
  vari=ncdf_varinq(ncid,i)
  varlist[i]=vari.name
  exe="ncdf_varget,ncid,i,"+varlist[i]
  exe2=exe2+varlist[i]+":"+varlist[i]
  if (verb) then print,exe
  res=execute(exe)
  if (i ne ninq.nvars-1) then exe2=exe2+"," else exe2+='}'
endfor
  if (long) then begin
    exe='nams={'
    pdimns=ptrarr(ninq.nvars)
    for i=0,ninq.nvars-1 do begin
      vari=ncdf_varinq(ncid,i)
      dimns=strarr(n_elements(vari.dim))
      for j=0,n_elements(vari.dim)-1 do begin
        ncdf_diminq,ncid,vari.dim[j],name,size
        dimns[j]=name
      endfor
      pdimns[i]=ptr_new(dimns)
      exe+=strcompress(varlist[i],/rem)+':*(pdimns['+strcompress(string(i),/rem)+'])'
      if (i ne ninq.nvars-1) then exe+=','
    endfor
    exe+='}'
    res=execute(exe)
    exe2+=',n:nams'
  endif
  exe2=exe2+"}"
  if (verb) then print,exe2
  res=execute(exe2)
ncdf_close,ncid
if keyword_set(hash) then ret=!version.release gt '8.3' ? orderedhash(ret) : hash(ret)
return,ret
end
