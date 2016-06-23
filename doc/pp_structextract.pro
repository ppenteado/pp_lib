function pp_structextract,strin,_ref_extra=ex,null=null
compile_opt idl2,logical_predicate
ret={}
tn=tag_names(strin)
pl=pp_locate(strupcase(ex))
exret=bytarr(n_elements(ex))
foreach t,tn,it do begin
  if ~pl.haskey(t) then ret=create_struct(ret,t,strin.(it)) else begin
    (scope_varfetch(t,level=0,/ref_extra))=strin.(it)
    exret[pl[t]]=1B
  endelse
endforeach

if keyword_set(null) then foreach eex,ex,iex do if ~exret[iex] then begin
  (scope_varfetch(eex,level=0,/ref_extra))=!null
endif
return,ret
end
