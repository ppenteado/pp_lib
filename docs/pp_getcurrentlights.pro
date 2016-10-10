; docformat = 'rst rst'
;+
; :Description:
;    Retrieves one or both of the default lights made when a surface is created in
;    iTools or Graphics. These lights are one ambient light and one directional light.
;    By default, the directional light is returned.
;    The current graphic (iTool or Graphics) is used by default, if `id` is not
;    provided.
;
; :Keywords:
;    ambient : in, optional, default=0
;      If set, the ambient light is the one retrieved.
;    directional : in, optional, default=1
;      If set, the directional light is the one retrieved.
;    all : in, optional, default=0
;      If set, both lights are retrieved, in a two-element array, with the ambient
;      light first.
;    tool : out, optional
;      Returns the object associated with the graphic. Most useful to do a call
;      to tool.commitactions after a change in the light properites (see the example).
;    id : in, out, optional
;      The tool id of the graphic to operate on. This is processed by `pp_gettoolid`: 
;      If not provided, the current (if any) is used, and its ID is returned in this
;      variable. If one is provided but is not valid, the current one is obtained
;      instead, and returned in this variable. This can also be a Graphics identifer
;      (as from the getfullidentifier method), in which case this variable will be
;      altered to return the corresponding tool ID.
;      
; :Examples:
; 
;   Make a simple surface, retrieve its directional light and move it::
;   
;     surf=surface(dist(100))
;     
;   This should look like
;   
;      .. image:: pp_getcurrentlights_ex1.png
;   
;   Then the direction can be retireved and changed with::
;   
;     dir_light=pp_getcurrentlights(tool=ot,id=surf.getfullidentifier())
;     dir_light.getproperty,location=location
;     print,location
;     ;  -0.00010000000   0.00010000000   0.00010000000
;     dir_light.setproperty,location=-location
;     
;   Update the rendering to show the changes done::
;   
;     ot.commitactions
;     
;   Which should look like
;   
;      .. image:: pp_getcurrentlights_ex2.png
;      
; :Uses: pp_gettoolid
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Feb/2011
;-
function pp_getcurrentlights,ambient=ambient,directional=directional,all=all,tool=ot,id=id
compile_opt idl2,logical_predicate

;Defaults
all=(n_elements(all) eq 1) ? all : 0
ambient=all ? 1 : (n_elements(ambient) eq 1) ? ambient : 0
directional=all ? 1 : ambient ? 0 : (n_elements(directional) eq 1) ? directional : 1

;Get the current tool and id, if not provided
id=pp_gettoolid(id,tool=ot)

;Get the default lights
if (directional) then begin
  dir=ot.getbyidentifier(igetid('*LIGHT_1'))
endif
if (ambient) then begin
  amb=ot.getbyidentifier(igetid('*LIGHT'))
endif
ret=all ? [amb,dir] : directional ? dir : amb
return,ret

end
