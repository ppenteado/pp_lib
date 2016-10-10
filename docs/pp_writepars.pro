; docformat = 'rst rst'
;+
;-
;+
; :Description:
;    Writes a text parameter file from the given hash. The file will contain
;    one line per hash element, in the form key=value.
;    
;    The complement to this procedure is `pp_readpars`. For compatibility with
;    `pp_readpars`, header lines should be comment lines (start with the comment
;    marker), and the keys in the hash should be strings (if not they will get
;    converted to strings in the output file, and the conversion may not be done
;    in a desirable way). 
;
; :Params:
;    file : in, required
;      A string with the name of the file to create.
;    ipars : in, required
;      A hash with the values to write in the file.
;
; :Keywords:
;    formats : in, optional
;      Either a scalar string, specifying the format to use for all file lines,
;      or a hash of strings, with the same keys as the hash of values, each giving
;      the format to use for the correponding line.
;    header : in, optional
;      If given, its contents are printed on the top of the file. Useful for headers
;      (for compatibility with `pp_readpars`, header lines should be comments).
;    order : in, optional
;      An array or list of keys to the parameter hash, sepcifying the order in which
;      the elements whould be written to the file. If not provided, no ordering is made.
;      
; :Examples:
;   Make up a simple set of parameters and write them with a comment line::
;   
;     pars=hash()
;     pars['a']=9d0
;     pars['aa']=-17
;     pars['b']="'some string'"
;     pp_writepars,'pp_writepars_example.txt',pars,header='#some comment'
;     
;   Which produces a file with
;   
;   #some comment
;   
;   a=       9.0000000
;   
;   b='some string'
;   
;   aa=     -17
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Feb/2011
;-
pro pp_writepars,file,ipars,formats=iformats,order=order,header=header
compile_opt idl2, logical_predicate

if ~isa(ipars,'hash') then begin
  print,'ipars is not a hash; doing nothing'
  return
endif

;Defaults
order=(n_elements(order) eq 0) ? ipars.keys() : order

pars=ipars[order]

case n_elements(iformats) of
  0: formats=hash(pars.keys())
  n_elements(pars) : formats=iformats
  1: begin
    formats=hash()
    foreach el,pars,key do formats[key]=iformats  
  end
  else: format=iformats[order]
endcase

;Write the file
openw,unit,file,/get_lun
if n_elements(header) ne 0 then printf,unit,header
foreach value,pars,key do printf,unit,key,'=',value,format=formats[key]
free_lun,unit

end