; docformat = 'rst'
;+
; :Description:
;    Returns a filled/open/dotted square object to use as a symbol in an itool. Object
;    is an IDLgrPolygon. If the object is edited even after the call to the itool,
;    the itool symbol will change (it may take clicking on the itool window to cause it to refresh).

;
; :Params:
;    sz : in, optional, default=1.0
;      Symbol size.
;
; :Keywords:
;    _ref_extra : in, out, optional
;      Passed to IDLgrPolygon init routine. Commonly useful keywords are:
;      
;        style: determines the type of circle (2 for filled, 1 for the contour only,
;        0 for points at the vertices only.
;        
;        thick: determines the thickness of the line to draw the contour.
;        
;        color: 3 element RGB color vector.
;
; :Examples:
;    To create a plot using filled red squares of size 5::
;
;      os=pp_osquare(5.0,color=[255,0,0])
;      iplot,indgen(10),sym_object=os
;
;    To change the color and thickness of the symbols in the existing plot::
; 
;      os->setproperty,color=[0,0,255],style=1,thick=1.5
;      
;    The object will keep exisiting even after the itool is closed. To get
;    rid of it, it has to be destroyed afterwards (if destroyed while the itool
;    still exists, it will cause a lot of errors)::
;    
;      obj_destroy,os
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Sep/2009
;-
function pp_osquare,sz,_ref_extra=ex
compile_opt idl2
sz=n_elements(sz) eq 1 ? sz : 1.
obj=obj_new('IDLgrPolygon',sz*[-0.5,0.5,0.5,-0.5],sz*[0.5,0.5,-0.5,-0.5],_strict_extra=ex)
return,obj
end
