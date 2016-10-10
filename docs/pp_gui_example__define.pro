; docformat = 'rst rst'
;
; Example application, to show how to use pp_gui (which uses the Catalyst Library)
; 
; Mostly, one would use this program just to read the source code, to see how
; to use pp_gui.
; 
; This application displays a 3D array as multiple images, by dragging on a slider
; to change the image being shown. The array's final dimension must be the band number.
; 
; :Examples:
;    Load data from IDL's example files, and lanuch this application to visualize
;    them::
;      fp=filepath('head.dat',subdirectory=['examples','data'])
;      h=read_binary(fp,data_dims=[80,100,57])
;      g=pp_gui_example(h)
;      
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Nov/2014
;
;-


;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    data: in, required
;      A 3D array with several images to be displayed. The images must be contiguous
;      over the first two dimensions. That is, the third dimension must be where the
;      different images are indexed.
;
; :Keywords:
;    _extra: in, optional
;      Any other parameters are passed on to pp_gui.
;
;-
function pp_gui_example::init,data,_extra=ex
compile_opt idl2,logical_predicate

;Store the data and set the window title
self.data=ptr_new(data)
sz=size(data,/dimensions)
self.nbands=sz[2]
title='pp_gui_example '+strjoin(strtrim(sz,2),'x')

;Initialize the object
ret=self.pp_gui::init(_extra=ex,title=title,column=1)
return,ret
end

pro pp_gui_example::creategui
compile_opt idl2,logical_predicate

;Add the other needed widgets
draw=selectabledrawwidget(self,initial_color='ivory',erase_window=1,name='draw')
sl=sliderwidget(self,name='band',value=0,min=0,max=self.nbands-1,title='Band',/drag)

;Initialize some objects and properties
sz=size(*self.data,/dimensions)
draw.setproperty,xsize=300>sz[0]<600,ysize=300>sz[1]<400
im=catimage(bytarr(2,2),name='imagefordraw')
draw.add,im
self.values['band']=0

;Finish adding widgets with pp_gui's creategui method
self.pp_gui::creategui

end


;+
; :Description:
;    This method gets called by the event handler after it has finished processing
;    the event. We use it to do whatever is necessary to update the widgets. In
;    this case, the only thing to do is to replace the image in the draw widget.
;
;-
pro pp_gui_example::updatewindow
compile_opt idl2,logical_predicate

im=self.get('imagefordraw',/recursive)
im.setproperty,image=(*self.data)[*,*,self.values['band']]
im.draw

end

;+
; :Description:
;    Just the class definition for pp_gui_example.
;    
;-
pro pp_gui_example__define
;Here we define the class. This must be the last routine in this file
compile_opt idl2,logical_predicate
  !null={pp_gui_example,inherits pp_gui,data:ptr_new(),nbands:0}
end
