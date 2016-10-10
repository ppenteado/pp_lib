; docformat = 'rst rst'
;
;+
; :Author: Paulo Penteado (http://www.ppenteado.net), Apr/2015
;-


;+
; :Description:
;    Helper function, to return the mysql variable type corresponding to an IDL
;    variable type.
;
; :Params:
;    var: in, required
;      The IDL variable whose mysql type is desired. 
;
; :Keywords:
;    minlen: in, optional
;      If the type is a string, by default the mysql type will have the length of
;      the longest string in `var`. If minlen is provided, the mysql length will be
;      the greatest among minlen and the longest string in `var`.
;    maxlen: in, optional
;      If the type is a string, by default the mysql type will have the length of
;      the longest string in `var`. If maxlen is provided, the mysql length will be
;      the smallest among maxlen and the longest string in `var`.
;
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Apr 2015
;-
function pp_structtomysql__typename,var,minlen=minlen,maxlen=maxlen
compile_opt idl2,logical_predicate,hidden

if isa(var,'struct') && pp_in('TYPE',tag_names(var)) && pp_in('VALUE',tag_names(var)) then begin
  return,var.type
endif

ret=typename(var)
case ret of
  'BYTE': ret='TINYINT UNSIGNED'
  'INT' : ret='SMALLINT'
  'UINT': ret='SMALLINT UNSIGNED'
  'LONG': ret='INT'
  'ULONG': ret='INT UNSIGNED'
  'LONG64': ret='BIGINT'
  'ULONG64': ret='BIGINT UNSIGNED'
  'FLOAT' : ret='FLOAT'
  'DOUBLE': ret='DOUBLE'
  else: begin
      len=max(strlen(var))
      if keyword_set(minlen) then len=len>minlen
      if keyword_set(maxlen) then len=len<maxlen
      len=1>len
      ret='VARCHAR('+strtrim(len,2)+')'
  end
endcase
return,ret
end


;+
; :Description:
;    Creates a table in a mysql server, from an IDL array of structures.
;    Each element of the array becomes a row in the table. The column names are
;    the structure field names, with a '_' appended (to avoid mysql reserved words).
;    The mysql server information (address, login, password, etc) is taken from ~/.my.conf.
;    This is handled by Marc Buie's `openmysql`
;    (`http://www.boulder.swri.edu/~buie/idl/pro/openmysql.html <http://www.boulder.swri.edu/~buie/idl/pro/openmysql.html>`). 
;
; :Params:
;    struct: in, required
;      The structure containing the data to be inserted in the mysql table.
;    tablename: in, required, type=string
;      The name of the table to be created.
;
; :Keywords:
;    dbname: in, required, type=string
;      The name of the database where the table is to be created.
;    username: in, optional, type=string
;      The username to use in mysql. Defaults to the current user.
;    verbose: in, optional
;      If set, the mysql commands get printed to the terminal.
;    primary_key: in, optional, type=string
;      If set, determines which key will be the table's primary key.
;    nodeletecsv: in, optional
;      During injection, the data is put into a csv file in the current directory
;      (pp_structtomysql_tmpfile.csv). If this keyword is set, the file is not deleted
;      at the end.
;    nodroptable: in, optional
;      By default, if the table to be created already exists, it is dropped, then
;      created with the data provided. If this keyword is set, dropping the table is
;      skipped, and this procedure will just attempt to insert the data into the table.
;      
;    generate_code_only: out, optional
;      If provided, this variable will contain a string array with the commands that
;      would have been issued to mysql. No mysql commands are executed.
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
; :Requires: `openmysql`, `mysqlquery`, from Marc Buie's library
; (`http://www.boulder.swri.edu/~buie/idl/ <http://www.boulder.swri.edu/~buie/idl/>`).
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Apr 2015
;-
pro pp_structtomysql,struct,tablename,dbname=dbname,username=username,$
  verbose=verbose,primary_key=primary_key,nodeletecsv=nodeletecsv,nodroptable=nodroptable,$
  generate_code_only=gencode
compile_opt idl2,logical_predicate

verbose=keyword_set(verbose)
codeonly=arg_present(gencode)
;open mysql connection
if ~codeonly then openmysql,lun,dbname,user=username

;delete table if it exists

if ~codeonly then begin
  mysqlquery,lun,"show tables like '"+tablename+"';",res,verbose=verbose
  if (res && (~keyword_set(nodroptable))) then mysqlquery,lun,'drop table '+tablename+';',/cmd,verbose=verbose
  mysqlquery,lun,"show tables like '"+tablename+"';",res,verbose=verbose
endif

if codeonly || (~res) then begin
  ;create table
  mcomm='create table '+tablename+' ('
  foreach tn,tag_names(struct[0]),itn do begin
    mcomm+=(itn eq 0 ? '' : ', ')+strupcase(tn)+'_ '+pp_structtomysql__typename(struct.(itn))
  endforeach
  if keyword_set(primary_key) then mcomm+=', primary key ( '+primary_key+') '
  mcomm+=');'
  if verbose then print,mcomm
  if ~codeonly then mysqlquery,lun,mcomm,/cmd,verbose=verbose else gencode=mcomm
endif

if codeonly then return

structs={}
foreach tag,tag_names(struct),itag do begin
  var=struct.(itag)
  if isa(var,'struct') && pp_in('TYPE',tag_names(var)) && pp_in('VALUE',tag_names(var)) then begin
    structs=create_struct(structs,tag,strjoin(var.value,string(10B)))
  endif else structs=create_struct(structs,tag,var)
endforeach

fn='pp_structtomysql_tmpfile.csv'
write_csv_pp,fn,structs,/titles
mcomm="LOAD DATA LOCAL INFILE '"+fn+"' INTO TABLE "+tablename
mcomm+=" FIELDS TERMINATED BY ',' ENCLOSED BY '"+'"'+"'"
mcomm+=" LINES TERMINATED BY '\n'"
mcomm+=" IGNORE 1 LINES;"
if verbose then print,mcomm
mysqlquery,lun,mcomm,/cmd
if (~keyword_set(nodeletecsv)) then file_delete,fn

free_lun,lun
end
