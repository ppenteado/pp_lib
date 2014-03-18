; docformat = 'rst rst'
;+
;
; This procedure allows editing some properties of an iTool legend that are not easiy accessible
; programatically. See example below.
; 
; At this time, only hiding/unhiding the legend box is implemented. 
;
; :Examples:
;    Make some plot, with a legend, then hide its box::
;
;      iplot,[0,1],name='a',/insert_legend
;      pp_ilegendedit,/hidebox
;      
;    Now unhide it::
;
;      pp_ilegendedit,hidebox=0
;      
;    Working with multiple legends:
;    
;    By defaut, pp_ilegendedit will act over every legend on thw current iTool. To limit its action
;    to a subset of the legends, use the optional argument specifying which legend(s) to edit, a a scalar
;    (edit one legend, identified by that index), or an array, where each element is the index of one
;    legend to edit. Legend indexes start at 0::
;    
;         iplot,[0,1],name='a',/insert_legend
;         iplot,[0,0],name='b',/over
;         leg=pp_ilegend([0d0,0.5d0])
;         pp_ilegendedit,1,/hidebox=; will act only on the second legend
;      
; :Version: 20140318
; 
; :Requires: IDL 8.0
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Mar/2014
;
;-


function pp_ilegendeditor::init
compile_opt idl2,logical_predicate,hidden
!null=igetcurrent(tool=ot)
if ot then begin
  l=igetid('legend*')
  self.leg=list()
  foreach ll,l do self.leg.add,ot.getbyidentifier(ll)
  self.tool=ot
  return,1
endif else return,0
end

pro pp_ilegendeditor::changeproperty,hidebox=hidebox,legend
compile_opt idl2,logical_predicate,hidden
oself=self
lu=n_elements(legend) ? legend : indgen(oself.leg.count())
foreach il,lu do begin
  self=(oself.leg)[il]
  if n_elements(hidebox) then self._opolygon.setproperty,hide=hidebox
endforeach
self=oself
self.tool.refreshcurrentwindow
end

pro pp_ilegendeditor__define
compile_opt idl2,logical_predicate,hidden
!null={pp_ilegendeditor,leg:obj_new(),tool:obj_new()}
end

pro pp_ilegendedit,_ref_extra=_extra,legend
compile_opt idl2,logical_predicate
i=pp_ilegendeditor()
i.changeproperty,_extra=_extra,legend
end