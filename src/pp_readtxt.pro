function pp_readtxt,file
compile_opt idl2,logical_predicate,hidden
lines=strarr(file_lines(file))
openr,lun,file,/get_lun
readf,lun,lines
free_lun,lun
return,lines
end
