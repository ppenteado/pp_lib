; docformat = 'rst'
;+
; :Description:
;    Changes the range of values spanned by the labels in an existing colorbar.
;
; :Params:
;    range : in, required
;      Two-element array with the range to be spanned by the colorbar.
;    np : in, optional
;      Number of tick marks to use.
;
; :Keywords:
;    format : in, optional, default='(F5.1)'
;      Format specification to use when making the new tick labels.
;    vnumber : in, optional, default=1
;      View number (panel number) in the itool where the text should be placed.
;    id : in, out, optional
;      Itool id to use. If not specified, the current is used and its id is returned.
;    _ref_extra : in, out, optional
;      Any other properties of the colorbar to set (IDLitVisColorbar properties).
;
; :Examples:
;   Make a sample plot::
; 
;     iplot,findgen(10),vert_colors=bindgen(10)*255/(9.),/insert_colorbar
;     pp_colorbar_range,[0.,5.],font_size=12.
;
; :Uses:
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Aug/2009
;-
pro pp_colorbar_range,range,np,format=format,vnumber=vn,id=id,_ref_extra=ex
compile_opt idl2
;Defaults
format=n_elements(format) eq 1 ? format : '(F5.1)'
vn=n_elements(vn) eq 1 ? vn : 1
;Get the itool object
if (n_elements(id) eq 1) then isetcurrent,id
id=igetcurrent(tool=ot)
if (~obj_valid(ot)) then message,'No valid itool found'
;Get the colorbar object
cb=ot->getbyidentifier(ot->findidentifiers("*TOOL*/WINDOW/VIEW_"+strcompress(string(vn),/rem)+"/ANNOTATION LAYER/COLORBAR"))
if (~obj_valid(cb)) then message,'No valid colorbar found'
;Get the colorbar range, tickvalues and tick text object
cb->getproperty,range=crange,ticktext=tt,tickvalues=tv
np=n_elements(np) eq 1 ? np : n_elements(tv) ;default number of ticks
if (np eq n_elements(tv)) then begin
  cb->setproperty,major=np+1
  cb->setproperty,tickvalues=findgen(np+1)
endif
;Make new tick values, in the old coordinates (range will not be changed)
newvalues=crange[0]+(dindgen(np)/(np-1d0))*(crange[1]-crange[0])
;Make new labels
newstrings=strtrim(string(range[0]+(dindgen(np)/(np-1d0))*(range[1]-range[0]),format=format),2)
;Update things
cb->setproperty,major=np
cb->setproperty,tickvalues=newvalues,_strict_extra=ex
tt->setproperty,strings=newstrings
ot->commitactions
end
