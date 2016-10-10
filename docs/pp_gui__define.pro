; docformat = 'rst rst'
;
; :Version: 201411
; 
; pp_gui is a class derived from Catalyst's toplevelbase, as a wrapper, to ease
; the creation of a graphical interface.
; 
; :Examples:
;    See the example application `pp_gui_example`. 
;
; :Requires: IDL 8.0, plus Catalyst and Coyote Libraries (`http://www.idlcoyote.com`)
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Nov/2014
;-



;+
; :Description:
;    A wrapper for toplevelbase's eventhandler, populated just with the handler
;    for the exit button, and to save the event value into self.values (a hash
;    indexed by the event object's name). Typically, the user will want to override
;    this method, to add whatever handling will be needed.
;
; :Params:
;    event: in, required
;      The event to be processed. See `http://www.exelisvis.com/docs/routines-120.html`.
;
; :Examples:
;    See the example application `pp_gui_example`.
;
;-
pro pp_gui::eventhandler,event
compile_opt idl2,logical_predicate
if self.verbose then begin
  print,'pp_gui event info:'
  foreach tn,tag_names(event),i do print,i,' ',tn,' ',typename(event.(i)),' ',event.(i)
  print,''
endif

if (tag_names(event)).hasvalue('VALUE') then begin
  event.id.getproperty,value=val
  self.values[event.name]=val
endif



case event.name of
  'exitbutton': begin
     obj_destroy,self
     return
  end
  else: break
endcase

self.updatewindow

end

pro pp_gui::updatewindow
compile_opt idl2,logical_predicate

return

end


;+
; :Description:
;    A standard setproperty method. Anything not handled here is passed on to 
;    toplevelbase.
;
; :Keywords:
;    _extra: in, optional
;      Passed on toplevelbase.
;    debugevent: in, optional
;      If set, the event handler will print event iformation everytime it is called.
;
;-
pro pp_gui::setproperty,_extra=_ex,debugevent=debugevent
compile_opt idl2,logical_predicate,hidden
  if n_elements(debugevent) then self.verbose=debugevent
  if (n_elements(ex) ne 0) then self.toplevelbase::setproperty,_extra=ex
end

;+
; :Description:
;    A standard getproperty method. Anything not handled here is passed on to
;    toplevelbase.
;
; :Keywords:
;    _ref_extra: out, optional
;      Passed on toplevelbase.
;    debugevent: out, optional
;      If set, the event handler will print event iformation everytime it is called.
;
;-
pro pp_gui::getproperty,_ref_extra=ex,debugevent=debugevent
compile_opt idl2,logical_predicate,hidden
  if arg_present(debugevent) then debugevent=self.verbose
  if (n_elements(ex) ne 0) then self.toplevelbase::getproperty,_extra=ex
end

;+
; :Description:
;    Wraps toplevelbase's init, creating a sekeleton for the graphical interface
;    and launching it. The elements of the interface are created in the method
;    `pp_gui::greategui`. Typically, the user will want to define the interface by
;    creating another creategui method.
;
;
; :Keywords:
;    debugevent: in, optional
;      If set, the eventhandler will print out event information everytime it is called.
;    _ref_extra: in, optional
;      Parameters passed down to Catalyst's toplevelbase.
;    xsize: in, optional, default=600
;      The x size of the window.
;    ysize: in, optional, default=400
;      The y size of the window.
;    title: in, optional, default='pp_gui'
;      The title for the window.
;    row: in, optional, default=1
;      Passed on to toplevelbase.
;    column: in, optional
;      Passed on to toplevelbase.
;    
; :Examples:
;    See the example application `pp_gui_example`.
;
;-
function pp_gui::init,debugevent=verbose,_ref_extra=ex,$
  xsize=xsize,ysize=ysize,title=title,row=row,column=column,mbar=mbar
compile_opt idl2,logical_predicate
self.values=hash()
self.verbose=keyword_set(verbose)
xsize=n_elements(xsize) ? xsize : 600
ysize=n_elements(ysize) ? ysize : 400
title=n_elements(title) ? title : 'pp_gui'
if (n_elements(row) eq 0) and (n_elements(column) eq 0) then column=1
ret=self.toplevelbase::init(xsize=xsize,ysize=ysize,title=title,row=row,column=column,$
  _strict_extra=ex,mbar=mbar)
self.values['mbar']=mbar
self.creategui
self.draw
self.updatewindow
return,ret
end


;+
; :Description:
;    This method is called by init, to populate the interface with the elements
;    the user will want. Typically, the user will override this method, to add all
;    the other elements.
;
;
; :Examples:
;    See the example application `pp_gui_example`.
;
;-
pro pp_gui::creategui
compile_opt idl2,logical_predicate
b1=buttonwidget(self,value='Exit',name='exitbutton')
end

;+
; :Description:
;    Just the class definition for pp_gui
;    
;
; :Examples:
;    See the example application `pp_gui_example`.
;
pro pp_gui__define
compile_opt idl2,logical_predicate
!null={pp_gui,inherits toplevelbase,values:obj_new(),verbose:0B}
end