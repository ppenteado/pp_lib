; docformat = 'rst rst'
;+
; :Author: Paulo Penteado (pp.penteado@gmail.com), Nov/2010
;-

;+
; :Description:
;    Evaluates a normalized Gaussian distribution of mean zero and the provided
;    width at the provided locations (`x`). The width can be provided from the
;    standard deviation (`sigma`) or the FWHM (`fwhm`).
;
; :Params:
;    x in, required
;      The locations where the Gaussian is to be evaluated.
;
; :Keywords:
;    fwhm : in, optional, default=1d0
;      Specifies the width of the Gaussian, as its Full Width Half Maximum (FWHM).
;      If both `sigma` and `fwhm` are provided, `fwhm` takes precedence.
;    sigma : in, optional, default=1d0
;      Specifies the width of the Gaussian, as its Full Width Half Maximum (FWHM).
;      If both `sigma` and `fwhm` are provided, `fwhm` takes precedence.
;      
; :Examples:
; 
;   Make a domain where the Gaussian is to be evaluated::
;   
;     nx=201
;     x=(dindgen(nx)/(nx-1d0)-0.5d0)*5d0
;     
;   Make a Gaussian with fwhm=1 and plot it::
;   
;     yg=pp_gauss_from_fwhm(x,fwhm=1d0)
;     pg=plot(x,yg,color='red',name='Gaussian, FWHM=1d0',thick=2.)
;     
;   Now compare with a Lorentzian (made with `pp_lorentz_from_fwhm`)::
;   
;     yl=pp_lorentz_from_fwhm(x,fwhm=1d0)
;     pl=plot(x,yl,color='blue',name='Lorentzian, FWHM=1d0',thick=2.,/over)
;     l=legend(target=[pg,pl],position=[0.5,0.5]) ;Identify the two lines
;
;   Save the result into the file shown below::
;     
;       pg.save,'pp_gauss_from_fwhm.png',resolution=100
;   
;   .. image:: pp_gauss_from_fwhm.png
;     
;
;-
function pp_gauss_from_fwhm,x,fwhm=fwhm,sigma=sigma
compile_opt idl2, logical_predicate
;Defaults
;Get sigma from fwhm, if fwhm was provided
fsfac=(2d0*sqrt(2d0*alog(2d0)))
sigma=n_elements(fwhm eq 1) ? fwhm/fsfac : sigma ;equivalent to the line below
;if (n_elements(fwhm eq 1) then sigma=fwhm/(2d0*sqrt(2d0*alog(2d0))) else sigma=sigma
;If reached this point with no width set, use sigma=1 as default
if (n_elements(sigma) ne 1) then sigma=1d0/fsfac
fwhm=sigma*fsfac

;Evaluate the Gaussian of standard deviation sigma and mean 0 at the coordinates given in x
ret=exp(-(x^2)/(2d0*sigma^2d0))
ret/=(sigma*sqrt(2d0*!dpi))
return,ret
end
