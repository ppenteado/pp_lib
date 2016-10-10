; docformat = 'rst rst'
;
;+
; :Author: Paulo Penteado (http://www.ppenteado.net), Apr/2015
;-


;+
; :Description:
;    Retrieves a table from a mysql server, into an IDL array of structures.
;    Each element of the array corresponds to a row in the table. The field names are
;    the table column names.
;    The mysql server connection must have been already opened into a logical unit,
;    using Marc Buie's `openmysql`
;    (`http://www.boulder.swri.edu/~buie/idl/pro/openmysql.html <http://www.boulder.swri.edu/~buie/idl/pro/openmysql.html>`).
;
; :Params:
;    lun: in, required
;      The logical unit where a connection to mysql was opened with `openmysql`
;      (`http://www.boulder.swri.edu/~buie/idl/pro/openmysql.html <http://www.boulder.swri.edu/~buie/idl/pro/openmysql.html>`).
;    query: in, required, type=string
;      The query to be performed.
;      
; :Returns:
;    An array of structures, where each element of the array corresponds to a row
;    in the table. The field names are the table column names.
;
; :Keywords:
;    verbose: in, optional
;      If set, the mysql commands get printed to the terminal.
;    trimunderscore: in, optional
;      If set, column names ending with a _ have the _ removed. Useful to retrieve
;      data from tables created by `pp_structtomysql`, which appends an _ to the column
;      names.
;    trim: in, optional
;      If provided, is passed along to `strtrim`, which is applied to the string columns.
;
; :Examples:
;   Create a table in a mysql server (server parameters set in ~/.my.conf), then retrieve
;   the data from it. In this case, assuming that there is a database named pp_structtomysql,
;   and that the user has write access to it::
;   
;     outstruct=replicate({a:1,b:2d0,c:'def'},4)
;     outstruct.a=indgen(4)
;     outstruct.b=dindgen(4)^2
;     outstruct.c+='_'+strtrim(indgen(4),2)
;     print,outstruct
;     ;{       0       0.0000000 def_0}
;     ;{       1       1.0000000 def_1}
;     ;{       2       4.0000000 def_2}
;     ;{       3       9.0000000 def_3}
;     pp_structtomysql,outstruct,'pp_structtomysql_example',primary_key='c_',dbname='pp_structtomysql'
;     openmysql,lun,'pp_structtomysql'
;     instruct=pp_mysqlquery(lun,'select * from pp_structtomysql_example;')
;     print,instruct
;     ;{                     0                     0 def_0}
;     ;{                     1                     1 def_1}
;     ;{                     2                     4 def_2}
;     ;{                     3                     9 def_3}
;     free_lun,lun
;     
;   Note that in this example field b was converted to integers upon reading, because all numbers
;   were integers.
;     
;
; :Requires: `mysqlcmd`, from Marc Buie's library
; (`http://www.boulder.swri.edu/~buie/idl/ <http://www.boulder.swri.edu/~buie/idl/>`).
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Apr 2015
;-
function pp_mysqlquery,lun,query,verbose=verbose,ngood=ngood,$
  trimunderscore=trimunderscore,trim=trim
compile_opt idl2,logical_predicate
mysqlcmd,lun,query,result,nlines,debug=verbose

trim=n_elements(trim) ? trim : 2
hh=orderedhash()
if nlines gt 1 then begin
  tmp=transpose((strsplit(result,'	',/extract,/preserve_null)).toarray())
  heads=tmp[*,0]
  if keyword_set(trimunderscore) then begin
    w=where(heads.endswith('_'),/null,count)
    if count then foreach ww,w do heads[ww]=strmid(heads[ww],0,strlen(heads[ww])-1)
  endif
  ret=orderedhash()
  foreach field,heads,ifield do begin
    case 1 of
      pp_isnumber(tmp[ifield,1:-1],/integer,/blank,/all): hh[field]=reform(long64(tmp[ifield,1:-1]))
      pp_isnumber(tmp[ifield,1:-1],/nan,/infinity,/blank,/all): hh[field]=reform(double(tmp[ifield,1:-1]))
      else: hh[field]=reform(strtrim(tmp[ifield,1:-1],trim))
    endcase
  endforeach
  ret=pp_structtransp(hh.tostruct())
endif else ret=!null
return,ret
end
