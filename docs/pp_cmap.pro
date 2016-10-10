; docformat = 'rst'
;+
; :Description:
;    Returns a colormap with nc saturated colors equally spaced in hue, plus black, white and one grey.
;
; :Returns:
;    A (3,(nc+3)) array, where each row contains the HLS or RGB values for each color. The first color is black,
;    the last is white, the one before last is grey, and the equally spaced saturated colors lie in between, in hue
;    order. 
;
; :Params:
;    nc : in, required
;      Number of equally spaced saturated colors to make. 
;
; :Keywords:
;    lct : in, optional, default=0
;      If set, the colormap created is loaded with tvlct.
;    grey : in, optional, default=0.5d0
;      The intensity of the grey color to make.
;    hls : in, optional, default=0
;      If set, the colors returned are in HLS space, instead of RGB space.
;
; :Examples:
;    Make a plot with pure black, red, green and blue lines::
;    
;      cmap=pp_cmap(3)
;      for i=0,3 do iplot,randomu(seed,10),color=cmap[*,i],over=(i ne 0),thick=2.
;      
;    Make a plot with pure black, red, yellow, green, cyan, blue and magenta lines::
;
;      cmap=pp_cmap(6)
;      for i=0,6 do iplot,randomu(seed,10),color=cmap[*,i],over=(i ne 0),thick=2.
;      
;    Make an eps in direct graphics pre-IDL 7.1::
;    
;      cmap=pp_cmap(3)
;      olddev=!d.name ;keep a copy of the current device name
;      set_plot,'ps'
;      device,filename='pp_cmap.eps',/encapsulated,/color,decomposed=0
;      tvlct,old1,old2,old3,/get ;keep a copy of the current colormap
;      tvlct,cmap[0,*],cmap[1,*],cmap[2,*] ;change the colormap
;      plot,[0d0,9d0],[0d0,1d0],/nodata
;      for i=0,3 do oplot,randomu(seed,10),color=i,thick=2.
;      tvlct,old1,old2,old3 ;restore the old colormap
;      device,/close
;      set_plot,olddev ;restore the old device name
;      
;    Make an eps in direct graphics with truecolor (only available from IDL 7.1)::
;    
;      cmap=pp_cmap(3)
;      olddev=!d.name ;keep a copy of the current device name
;      set_plot,'ps'
;      device,filename='pp_cmap.eps',/encapsulated,/color,decomposed=1
;      plot,[0d0,9d0],[0d0,1d0],/nodata
;      for i=0,3 do oplot,randomu(seed,10),color=cmap[0,i]+cmap[1,i]*2L^8+cmap[2,i]*2L^16,thick=2.
;      device,/close
;      set_plot,olddev ;restore the old device name
;
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Dec/2009
;-
function pp_cmap,nc,lct=lct,grey=gr,hls=hls
compile_opt idl2

;Defaults
gr=n_elements(gr) eq 1 ? gr :0.5d0 ;Intensity of grey color
lct=n_elements(lct) eq 1 ? lct : 0 ;Load color map?
rgb=n_elements(hls) eq 1 ? ~hls : 1 ;Convert colors to rgb space?

cm=dblarr(3,nc+3)
cm[*]=0d0
cm[0,1:nc]=dindgen(nc)*360d0/nc       ;Hue
cm[1,*]=0.5                           ;Ligthness
cm[2,*]=1.0                           ;Saturation
cm[*,0]=[0d0,0d0,0d0]                 ;Black
cm[*,nc+2]=[0.0,1.0,0.0]              ;White
cm[*,nc+1]=[0d0,gr,0d0]               ;Grey with specified intensity
if (rgb) then begin ;Convert to RGB
  color_convert,cm[0,*],cm[1,*],cm[2,*],r,g,b,/hls_rgb
  cm=[r,g,b]
endif
if (lct) then tvlct,cm,hls=~rgb ;Load color map if requested
return,cm
end
