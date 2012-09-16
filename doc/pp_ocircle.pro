; docformat = 'rst'
;+
; :Description:
;    Returns a filled/open/dotted circle object to use as a symbol in an itool. Object
;    is an IDLgrPolygon. If the object is edited even after the call to the itool,
;    the itool symbol will change (it may take clicking on the itool window to cause it to refresh).

;
; :Params:
;    sz : in, optional, default=1.0
;      Symbol size.
;
; :Keywords:
; 
;    np : in, optional, default=21
;      The number of points used in making the circle (defaults to 21), since it is
;      actually a polygon (similar to IDL's orb, which is a polyhedron "sphere").
; 
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
;    To create a plot using filled red circles of size 5::
;    
;      oc=pp_ocircle(5.0,color=[255,0,0])
;      iplot,indgen(10),sym_object=oc
;
;    To change the color, style and thickness of the symbols in the existing plot::
; 
;      oc->setproperty,color=[0,0,255],style=1,thick=1.5
;      
;    The object will keep exisiting even after the itool is closed. To get
;    rid of it, it has to be destroyed afterwards (if destroyed while the itool
;    still exists, it will cause a lot of errors)::
;    
;      obj_destroy,oc
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Sep/2009
;-
function pp_ocircle,sz,np=np,_ref_extra=ex
compile_opt idl2
sz=n_elements(sz) eq 1 ? sz : 1.
np=n_elements(np) eq 1 ? np : 21
r=sz/2d0 ;radius
ang=findgen(np)/(np-1d0)*2d0*!dpi ;angles
xpos=r*cos(ang)
ypos=r*sin(ang)
obj=obj_new('IDLgrPolygon',xpos,ypos,_strict_extra=ex)
return,obj
end
