; docformat = 'rst rst'
;
;+
; :Author: Paulo Penteado (http://www.ppenteado.net), 2007
;-
;+
; :Description:
;    Writes a netcdf file from a structure a in the long format returned by `pp_readncdfs`.
;
; :Params:
;    strin: in, required
;      Structure specifying everything about the file to be written. See the help of `pp_readncdfs` for information on the structure format.
;
; :Keywords:
;    double: in, optional
;      If set, makes the variables in double precision.
;    
; :Todo:
;   Process attributes.
;
; :Author: Paulo Penteado (http://www.ppenteado.net),
;-
pro pp_writencdf,strin,double=double
if (n_elements(double) ne 1) then double=0
a=strin
;variable and dimension names
at=strlowcase(tag_names(a))
adt=where((at eq 'dims')+(at eq 'd'))
avt=where((at eq 'vars')+(at eq 'v'))
ant=where((at eq 'nams')+(at eq 'n'))
an=strlowcase(tag_names(a.(avt)))
ad=strlowcase(tag_names(a.(adt)))
nid=ncdf_create(a.ncdfname,/clobber);make the file
;dimensions
dids=lonarr(n_elements(ad))
for i=0,n_elements(ad)-1 do dids[i]=ncdf_dimdef(nid,ad[i],(a.(adt)).(i))
;variables declaration
vids=lonarr(n_elements(an))
adnt=strlowcase(tag_names(a.(ant)))
for i=0,n_elements(an)-1 do begin
  w=where(adnt eq an[i])
  tmp=(a.(ant).(w))
  inds=lonarr(n_elements(tmp))
  for j=0,n_elements(tmp)-1 do inds[j]=where(ad eq tmp[j])
  dims=dids[inds]
  vids[i]=ncdf_vardef(nid,an[i],dims,double=double)
endfor
ncdf_control,nid,/endef
;put variables
for i=0,n_elements(an)-1 do ncdf_varput,nid,vids[i],a.(avt).(i)
ncdf_close,nid
print,'netcdf file ',a.ncdfname,' done'
end
