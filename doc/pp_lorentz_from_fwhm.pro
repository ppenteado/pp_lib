; docformat = 'rst rst'
;+
; :Author: Paulo Penteado (pp.penteado@gmail.com), Nov/2010
;-

;+
; :Description:
;    Evaluates a normalized Lorentzian distribution of mean zero and the provided
;    width at the provided locations (`x`). The width by the FWHM (`fwhm`).
;
; :Params:
;    x in, required
;      The locations where the Gaussian is to be evaluated.
;
; :Keywords:
;    fwhm : in, optional, default=1d0
;      Specifies the width of the Gaussian, as its Full Width Half Maximum (FWHM).
;      
; :Examples:
; 
;   Make a domain where the Lorentzian is to be evaluated::
;   
;     nx=201
;     x=(dindgen(nx)/(nx-1d0)-0.5d0)*5d0
;     
;   Make a Lorentzian with fwhm=1 and plot it::
;   
;     yl=pp_lorentz_from_fwhm(x,fwhm=1d0)
;     pl=plot(x,yl,color='blue',name='Lorentzian, FWHM=1d0',thick=2.)
;     
;   Now compare with a Gaussian (made with `pp_gauss_from_fwhm`)::
;   
;     yg=pp_gauss_from_fwhm(x,fwhm=1d0)
;     pg=plot(x,yg,color='red',name='Gaussian, FWHM=1d0',thick=2.,/over)
;     l=legend(target=[pg,pl],position=[0.5,0.5]) ;Identify the two lines
;
;   Save the result into the file shown below::
;     
;       pl.save,'pp_lorentz_from_fwhm.png',resolution=100
;   
;   .. image:: pp_lorentz_from_fwhm.png
;     
;
;-
function pp_lorentz_from_fwhm,x,fwhm=fwhm
compile_opt idl2, logical_predicate
;Defaults
fwhm=(n_elements(fwhm) eq 1) ? fwhm : 1d0
;Get gamma (hwhm) from fwhm
gamma=fwhm/2d0

;Evaluate the Lorentzian at each provided coordinate x
ret=(1d0/!dpi)*(gamma/(x^2+gamma^2))
return,ret
end
