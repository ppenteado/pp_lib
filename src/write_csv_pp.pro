; docformat = 'rst'
  
  ;+
  ; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), Feb/2013
  ;-
;+
  ; :Description:
  ;    A simple wrapper for write_csv, to write csv files using a structure's field names as column
  ;    titles (setting `titlesfromfields`), and accepting nested structures.
  ;
  ; :Params:
  ;    file: in, required, type=string
  ;      Passed to write_csv, specifies the name of the file to write.
  ;    data1: in, required
  ;      Passed to write_csv, after the variable has its structures flattened by a call to 
  ;      `pp_struct_unravel`.
  ;    data2: in, optional
  ;      Passed unaltered to write_csv.
  ;    data3: in, optional
  ;      Passed unaltered to write_csv.
  ;    data4: in, optional
  ;      Passed unaltered to write_csv.
  ;    data5: in, optional
  ;      Passed unaltered to write_csv.
  ;    data6: in, optional
  ;      Passed unaltered to write_csv.
  ;    data7: in, optional
  ;      Passed unaltered to write_csv.
  ;    data8: in, optional
  ;      Passed unaltered to write_csv.
  ;
  ; :Keywords:
  ;    titlesfromfields: in, optional
  ;      If set, the column titles in the csv file are made by the field names in data1.
  ;    _ref_extra: in, out, optional
  ;      Any other parameters are passed, unaltered, to / from write_csv.
  ;      
  ; :Examples:
  ;    Make a simple structure array and write it to a csv file::
  ;    
  ;      s={a:1,b:{c:2.5,d:-9,e:0},f:1.8}
  ;      s2=replicate(s,2)
  ;      s2[1].a=-1
  ;      s2[1].f=-1.8
  ;      write_csv_pp,'write_csv_pp_test.csv',s2,/titlesfromfields
  ;      
  ;    Which result in a file with:
  ;
  ;    ;A,B_C,B_D,B_E,F
  ;    
  ;    ;1,2.50000,-9,0,1.80000
  ;    
  ;    ;-1,2.50000,-9,0,-1.80000
  ;    
  ; :Requires: `pp_struct_unravel`
  ;
  ; :Author: Paulo Penteado (`http://www.ppenteado.net <http://www.ppenteado.net>`), Feb/2013
  ;-
pro write_csv_pp,file,data1,data2,data3,data4,data5,data6,data7,data8,titlesfromfields=tf,_ref_extra=ex
compile_opt idl2,logical_predicate
data0=data1 eq !null ? !null : pp_struct_unravel(data1)
if keyword_set(tf) then begin 
  header=tag_names(data0)
  write_csv,file,data0,data2,data3,data4,data5,data6,data7,data8,_strict_extra=ex,header=header
endif else write_csv,file,data0,data2,data3,data4,data5,data6,data7,data8,_strict_extra=ex
end
