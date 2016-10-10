; docformat = 'rst'
;+
; :Description:
;   Tests a string to determine if it can be converted to a floating point number.
;   All tests are case-insensitive. String must be trimmed on both sides:
;   if it contains leading or trailing blanks, it is not considered a number.
;    
; :Returns:
;   True if str is a floating point number, or (optionally), NaN or Infinity.
;   
;   False otherwise, including if str is undefined or is blank (unless the
;   keyword blank is set).
;
; :Params:
;    istr : in, required
;      String or string array to test.
;
; :Keywords:
;    nan : in, optional, default=1
;      If set, NaN is allowed as a number.
;    infinity : in, optional, default=1
;      If set, Infinity is allowed as a number
;      (any of inf,infi,infin,infini,infinit,infinity).
;    trim : in, optional, default=1
;      If set, blanks on the string (on both sides) are ignored (a call is made
;      to strtrim).
;    integer : in, optional, default=0
;      If set, will test for integer numbers, instead of real numbers.
;    blank : in, optional, default=0
;      If set, blanks are allowed as numbers
;    all : in, optional, default=0
;      If set and the input is an array, pp_isnumber will return a single 0 or 1,
;      indicating whether every element in the array passed the test.  
;      
; :Uses:
; 
; :Examples:
; 
; Test some simple cases::
; 
;   print,pp_isnumber(['a','12','-1','1.8','.9','-.8e-3','NaN','infinity','a12'])
;   ;   0   1   1   1   1   1   1   1   0
;   ;print,pp_isnumber(['a','12','-1','1.8','.9','-.8e-3','NaN','infinity','a12'],nan=0,infinity=0)
;   ;   0   1   1   1   1   1   0   0   0
;   print,pp_isnumber(['a','12','-1','1.8','.9','-.8e-3','NaN','infinity','a12'],/integer)
;   ;   0   1   1   0   0   0   0   0   0
; 
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Aug/2009
;-
function pp_isnumber,istr,nan=nan,infinity=infinity,trim=trim,integer=integer,blank=blank,all=all
compile_opt idl2, logical_predicate
;
;Defaults
nan=n_elements(nan) eq 1 ? nan : 1B
infinity=n_elements(infinity) eq 1 ? infinity : 1B
trim=n_elements(trim) eq 1 ? trim : 1B
integer=n_elements(integer) eq 1 ? integer : 0B
blank=n_elements(blank) eq 1 ? blank : 0B

nstr=n_elements(istr)
if (nstr eq 0) then return,0
str=trim ? strtrim(istr,2) : istr

if (blank) then begin
  bl=strlen(str) eq 0
endif
if (integer) then begin ;Test for integer
  ;Regular expression to determine if something is an integer number
  intexpr='^[-+]?([0-9]+)$'
  ;Test for integer
  res=stregex(str,intexpr,/boolean)
endif else begin ;Test for floating point number
  ;Regular expression to determine if something is a floating point number
  fpexpr='^[-+]?(([0-9]*\.?[0-9]+)|([0-9]+\.?[0-9]*))([eEdD][-+]?[0-9]+)?$'
  ;Regular expression for NaN
  nanexpr='^[-+]?(nan)$'
  ;Regular expression for Infinity
  infexpr='^[-+]?(inf)i?(in)?(ini)?(init)?(inity)?$'
  ;Test for floating point
  res=stregex(str,fpexpr,/boolean)
  ;Test for NaN
  res=byte(res or (replicate(nan,nstr) and stregex(str,nanexpr,/boolean,/fold_case)))
  ;Test for Infinity
  res=byte(res or (replicate(infinity,nstr) and stregex(str,infexpr,/boolean,/fold_case)))
endelse
if blank then res=res or bl
if keyword_set(all) then res=array_equal(res,1)
return,res
end
