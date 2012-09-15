; docformat = 'rst'
;+
; :Description:
;    Similar to xyouts, puts a text into the current itool.
;
; :Params:
;    str : in, required
;      String with the text to put into the itool.
;    location : in, optional
;      Two element array with the x,y coordinates where to place the text, in normalized units
;      ([0.,0.], is in the middle of the itool). 
;
; :Keywords:
;    font_size : in, optional, default=24.
;      Font size to use in the text.
;    vnumber : in, optional, default=1
;      View number (panel number) in the itool where the text should be placed.
;    id : in, out, optional
;      Itool id to use. If not specified, the current is used and its id is returned.
;    _ref_extra : in, out, optional
;      Any other properties of the text to set (IDLitVisText properties).
;
; :Examples:
;   To make an X axis title with a different font from the tick labels::
;   
;     iplot,findgen(10),xtickfont_size=12.
;
;     pp_ixyouts,'X Axis',[0.0,-0.8],font_size=24.,color=[255,0,0]
;
; :Uses:
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Sep/2009
;-
pro pp_ixyouts, str,location,font_size=fs,vnumber=vn,id=id,_ref_extra=ex
compile_opt idl2
;
;Defaults
vn=n_elements(vn) eq 1 ? vn : 1 
location=n_elements(location) eq 0 ? [0.,0.] : location
fs=n_elements(fs) eq 0 ? 24. : fs
;Get the itool object
if (n_elements(id) eq 1) then itcurrent,id
id=itgetcurrent(tool=ot) 
if (~obj_valid(ot)) then message,'No valid itool found'
;Get the annotation layer object
idan=ot->findidentifiers("*TOOL*/WINDOW/VIEW_"+strcompress(string(vn),/rem)+"/ANNOTATION LAYER")
oan=ot->getbyidentifier(idan)
;Make a new text object
text=obj_new('idlitvistext',_STRING=str,$
 alignment=0.5,vertical_alignment=1.0,font_size=fs,location=location)
text->setproperty,strings=str,_strict_extra=ex
;Put the object into the annotation layer
oan->add,text
end
