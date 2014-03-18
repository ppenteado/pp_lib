; docformat = 'rst rst'
;+
;
; This object allows adding separate legend boxes to an iTool. Once a new legend box is created,
; items can be added to it. After there are 2 or more legend boxes, any additional legend items must
; be added with this object's additem method. See example below.
;
; :Examples:
;    Make some plot, with a legend::
;
;      iplot,[0,1],name='a',/insert_legend
;      
;    Make another plot, with its name, but do not add the legend yet (or it would go into the first legend box)::
;    
;      iplot,[0,0],name='b',/over
;      
;    Now create the second legend box, at location [0,0.5], adding the last plot to it - this must be done at a time
;    when there is a plot object selected (as is the case just after a plot is created)::
;    
;      leg=pp_ilegend([0d0,0.5d0])
;      
;    Now create another plot, and add it to the second legend box - the item to be added to the legend is the
;    currently selected one::
;    
;      iplot,[0,-1],name='c',/over
;      leg.additem
;      
;    The result should look like
;    
;      .. image:: pp_ilegend_ex.png
;      
;      
; :Version: 20140318
; 
; :Requires: IDL 8.0
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Mar/2014
;
;-

function pp_ilegend::init,loc,verbose=verbose
compile_opt idl2,logical_predicate
!null=igetcurrent(tool=ot)
loc=n_elements(loc) eq 2 ? double(loc) : [0d0,0d0]
ileg=idlitopinsertlegend()
r=ileg.doaction(ot,location=loc,_extra=_extra)
self.verbose=keyword_set(verbose)
if self.verbose then print,r
self.leg=ot.getselecteditems()
self.tool=ot
self.items=list()
if self.leg then return,1 else return,0
end

pro pp_ilegend::additem
compile_opt idl2,logical_predicate
sel=self.tool.getselecteditems()
if sel then begin
  self.items.add,sel
  self.leg.select,/additive
  ni=idlitopinsertlegenditem()
  r=ni.doaction(self.tool)
  if self.verbose then print,r
endif
end

pro pp_ilegend__define
compile_opt idl2,logical_predicate
!null={pp_ilegend,tool:obj_new(),leg:obj_new(),items:obj_new(),verbose:0B}
end