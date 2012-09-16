; docformat = 'rst rst'
;+
; :Description:
;    Returns a valid (if any) tool ID, from the supplied (if any) `id`. This is useful
;    inside routines that operate on iTools or Graphics, that have an input ID, but
;    want to revert to the current tool if that is not provided or not valid.
;    Also useful to retrieve a tool ID from a Graphics full identifier (as from the
;    getfullidentifier method).
;
; :Params:
;    id : in, optional
;      The ID to try to use to retrieve the actual tool ID.  If the
;      provided ID is valid, it is returned. Otherwise, if it is a Graphics full
;      identifier (as from the getfullidentifier method), the returned ID is the corresponding
;      tool ID. If it is not valid, the current tool ID (if any) is returned.
;      
; :Keywords:
;    tool out, optional
;      Returns the tool object associated with that id. 
;    
; :Examples:
; 
;    Make a couple of tools and see their IDs::
;    
;      p=plot(/test,identifier=p_identifier)
;      print,p_identifier
;      ;/TOOLS/GRAPHIC
;      print,p.getfullidentifier()
;      ;/TOOLS/GRAPHIC/WINDOW/VIEW_1/VISUALIZATION LAYER/DATA SPACE/PLOT
;      print,pp_gettoolid(p.getfullidentifier())
;      ;/TOOLS/GRAPHIC
;      iplot,identifier=ip_identifier
;      print,ip_identifier
;      ;/TOOLS/PLOT TOOL
;      print,pp_gettoolid()
;      ;/TOOLS/PLOT TOOL
;
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Feb/2010
;-
function pp_gettoolid,id,tool=ot
compile_opt idl2,logical_predicate
if (size(id,/type) ne 7) then ret=igetcurrent(tool=ot) else begin ;Size is used instead of ISA for compatibility with <8.0
  if stregex(id,'^/TOOLS/GRAPHIC(_[[:digit:]]+)?/?',/boolean) then $ ;Test for a Graphics identifier
   ret=stregex(id,'(^/TOOLS/GRAPHIC(_[[:digit:]]+)?)/?',/extract)
  catch,ierr ;Try out the identifier, and replace it with the one for the current window, if it is not valid
  if ierr then begin
    catch,/cancel
    ret=igetcurrent(tool=ot)
  endif
  isetcurrent,ret
  catch,/cancel
  ret=igetcurrent(tool=ot)
endelse
return,ret
end
