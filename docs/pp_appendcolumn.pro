function pp_appendcolumn,strin,colname,icolvalues
compile_opt idl2,logical_predicate
nr=n_elements(strin)
colvalues=n_elements(icolvalues) eq 1 ? replicate(icolvalues,nr) : icolvalues
nc=n_elements(colvalues)
if nr ne nc then return,!null
ret=replicate(create_struct(strin[0],colname,colvalues[0]),nr)
struct_assign,strin,ret
ret.(n_tags(strin))=colvalues
return,ret
end
