; docformat = 'rst'
;+
; :Author: Paulo Penteado (pp.penteado@gmail.com), Feb/2010
;-



;+
; :Description:
;    Examples to test and illustrate the different behaviors of pp_convol.
;    
;    Makes up a high resolution spectrum and convolves it to lower resolutions.    
;
; :Params:
;    width : in, optional, default=0.04
;      The width for the convolution kernels.
;
; :Examples:
;    Convolve spectra to different resolutions::
;    
;      .com pp_convol ;pp_convol_test is in pp_convol.pro, so it will not be found by itself
;      pp_convol_test,0.04
;      pp_convol_test,0.02
;      
;   .. image:: pp_convol_test_1.png
;   .. image:: pp_convol_test_2.png
;
; :Uses: pp_resample,pp_integral
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Feb/2010
;-
pro pp_convol_test,width
;Example and test case for pp_convol
compile_opt idl2

;Defaults
width=n_elements(width) eq 1 ? width : 0.04

;Make up a high resolution spectrum and plot it
nx=1001
x=dindgen(nx)/(nx-1d0)
noise=0.005
y=1d0+randomu(seed,nx,/normal)*noise
nlines=30
df=0.7d0
depths=(randomu(seed,nlines)-0.5d0)*df
wf=0.025
widths=randomu(seed,nlines)*wf
centers=randomu(seed,nlines)
for i=0,nlines-1 do y+=depths[i]*exp(-((x-centers[i])/widths[i])^2)
iplot,x,y,name='Original spectrum',/insert_legend,$
 title='Original at '+strtrim(string(x[1]-x[0]),2)+', convolved to '+strtrim(string(width),2)
;Convolve with Gaussians at lower resolution
yc0=pp_convol(y,x,/gaussian,width=width,/local)
iplot,x,yc0,/over,color=[255,0,0],name='Gaussian (literal)',/insert_legend
yc0=pp_convol(y,x,/gaussian,width=width)
iplot,x,yc0,/over,color=[0,0,255],name='Gaussian (sampled)',/insert_legend
yc0=pp_convol(y,x,/step,width=width,/local)
;Convolve with rectangular kernels at lower resolution
iplot,x,yc0,/over,color=[0,255,0],name='Step (literal)',/insert_legend
yc0=pp_convol(y,x,/step,width=width)
iplot,x,yc0,/over,color=[255,0,255],name='Step (sampled)',/insert_legend

end



;+
; :Description:
;    Convolves the provided y(x) function with either a rectangular or Gaussian
;    kernel of the given width, or the provided arbitrary kernel (not yet implemented).
;    
;    The x grid does not need to be regular, and, contrary to common practice, no resampling
;    is done to a regular grid to calculate the convolution (resampling is not needed). Also
;    contrary to common practice, for rectangular or Gaussian kernels the integral is done
;    analytically, not numerically. In the case of a Gaussian kernel, it extends from the Gaussian
;    center up to the point where the terms in the integral become 0 (at double precision).
;    
;    Also, this routine allows for both literal and sampled interpretations of the input function (set
;    with the local keyword). 
;
; :Returns:
;    The result of the convultion is given for the same x locations as the input,
;    there is no automatic resampling. If resampling is intended after the convolution,
;    pp_resampled is recommended, as it preserves the areas. 
;
; :Params:
;    x : in, required
;      An array of locations where the function is sampled. Must be ordered (increasing or decreasing).
;    y : in, required 
;      An array with the function values corresponding to the locations in x.
;
; :Keywords:
;    step : in, optional, default=1
;      If set, the convolution kernel is a rectangular function of the given width.
;    gaussian : in, optional, default=0 
;      If set, the convolution kernel is a Gaussian, of fwhm of the given width.
;    width : in, required
;      The width of the convolution kernel (for step or gaussian).
;    grid_tolerance : in, optional,default=1d-6
;      Not yet implemented. Specifies the maximum fractional variation in the input grid to accept it as uniform.
;      
;      If the input grid is found to be uniform, the integration can be done faster.
;    kernelx : in, optional
;      Not yet implemented. An array of the x locations (centered at x=0) of the arbitrary convolution kernel to use.
;      
;      Must be ordered in increasing x.
;    kernely : in, optional
;      Not yet implemented. An array of the the arbitrary convolution kernel's values to use.
;      Must make a function of unit area.
;    local : in, optional, default=0
;      If not set, the y values are interpreted as a measured sample, that is, as the average of the "flux" falling inside the region
;      centered at the corresponding x values: x locations are interpreted as the centers of bins. Since this interpretation is equivalent
;      to a literal interpretation of a function made of rectangular regions, in this method there are no interpolations (partial areas, if
;      any, are just the corresponding fractions of the rectangles).
;      
;      If set, interprets the input function literally.  
;    newton : in, optional, default=0
;      Passed on to pp_integral. If local is set and newton is set, the function integrations are done with int_tabulated, which
;      uses a 5 point Newton-Cotes formula. If local is not set, this has no effect.
;    lsquadratic : in, optional, default=0
;      Passed on to pp_integral. If the method requires interpolation, this is passed on to interpol,
;      to select 4 point quadratic interpolation.
;      
;      If none of lsquadratic, quadratic and spline are set, interpolation is linear. 
;    quadratic : in, optional, default=0
;      Passed on to pp_integral. If the method requires interpolation, this is passed on to interpol,
;      to select 3 point quadratic interpolation.
;      
;      If none of lsquadratic, quadratic and spline are set, interpolation is linear.
;    spline : in, optional, default=0
;      Passed on to pp_integral. If the method requires interpolation, this is passed on to interpol,
;      to select spline interpolation.
;      
;      If none of lsquadratic, quadratic and spline are set, interpolation is linear.
;    xinc : out, optional
;      Returns the x values, in increasing order.
;    yinc : out, optional
;      Returns the y values, in order of increasing x.
;    xstart : out, optional
;      If local is not set, returns the location where each input pixel starts. If local is set, this is ignored.
;    xend : out, optional
;      If local is not set, returns the location where each input pixel ends. If local is set, this is ignored.
;    xbox : out, optional
;      Used when not in local mode, to return the input x values in pairs of equal values, so that
;      plotting xbox,ybox shows the rectangles that are how the input values were interpreted.
;    ybox : out, optional
;      Used when not in local mode, to return the input y values in pairs of equal values, so that
;      plotting xbox,ybox shows the rectangles that are how the input values were interpreted.
;
; :Examples:
;    
;   Make a well-sampled input function y(x) with fwhm=1::
;   
;     x=(12d0*dindgen(1001)/1d3)-6d0
;     y=exp(-(x^2*4d0*alog(2d0)))*8d0
;     iplot,x,y,/isotropic,name='Original function of fwhm=1',/insert_legend,thick=2.
;
;   Do the convolution and look at the results::
;
;     yc1=pp_convol(y,x,/step,width=4d0)
;     iplot,x,yc1,/over,color=[255,0,0],name='Convolution width=4, rectangular kernel (sampled)',/insert_legend
;     yc2=pp_convol(y,x,/step,/local,width=4d0)
;     iplot,x,yc2,/over,color=[0,0,255],name='Convolution width=4, rectangular kernel (literal)',/insert_legend
;     yc3=pp_convol(y,x,/gaussian,width=4d0)
;     iplot,x,yc3,/over,color=[0,255,0],name='Convolution width=4, Gaussian kernel (sampled)',/insert_legend
;     yc4=pp_convol(y,x,/gaussian,width=4d0,/local)
;     iplot,x,yc4,/over,color=[255,0,255],name='Convolution width=4, Gaussian kernel (literal)',/insert_legend
;   
;   .. image:: pp_convol.png
;    
;   See also the example in pp_convol_test, for a comparison of the methods with a more realistic (spectrum-like) function.
; 
; :Todo: Implement arbitrary kernel, grid_tolerance, and quadratic and cubic Gaussian integrations.
;
; :Uses: pp_resample,pp_integral
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Feb/2010
;-
function pp_convol,y,x,step=step,gaussian=gaus,width=width,grid_tolerance=gtol,$
 kernelx=kx,kernely=ky,$
 local=local,newton=newton,$
 lsquadratic=lsquadratic,quadratic=quadratic,spline=spline,$
 xinc=ix,yinc=iy,xstart=xstart,xend=xend,xbox=xbox,ybox=ybox
compile_opt idl2

;Defaults
local=n_elements(local) eq 1 ? local : 0
step=n_elements(step) eq 1 ? step : 1
gaus=n_elements(gaus) eq 1 ? gaus : 0
nkx=n_elements(kx) & nky=n_elements(ky)
usekernel=((nkx eq nky) && (nkx gt 0L)) ? 1 : 0
if (gaus) then begin
  step=0
  usekernel=0
endif
if (usekernel) then begin
  step=0
  gaus=0
endif
gtol=n_elements(gtol) eq 1 ? gtol : 1d-6

;Check that the dimensions of x and y match and are acceptable
nx=n_elements(x) & ny=n_elements(y)
if (nx ne ny) || (nx eq 0) then message,'x and y provided must be arrays of the same length'

;Make ix and iy to work with, 1D and in increasing order (x is assumed sorted, but can be increasing or decreasing order)
if (x[nx-1] lt x[0]) then begin
  ix=reverse(reform(x)) & iy=reverse(reform(y))
endif else begin
  ix=reform(x) & iy=reform(y)
endelse

;Determine whether the input grid is regular
dmax=max(ix[1:*]-ix[0:nx-2],/abs,min=dmin)
regular=((dmax-dmin)/dmin le gtol) 

if (step) then begin ;Use a constant function as the kernel
  ;Calculate the locations of the start and end of the interval that will go into each output point
  xmin=x-width/2d0
  xmax=x+width/2d0
  ;Calculate the input function's integral over the proper regions
  areas=pp_integral(x,y,xmin=xmin,xmax=xmax,newton=newton,local=local,$
   lsquadratic=lsquadratic,quadratic=quadratic,spline=spline)
  ret=areas/width
endif

if (gaus) then begin ;Use a Gaussian function as the kernel
  maxwid1=27.3d0 ;In double precision, exp(-x^2) underflows between x=27.29d0 and x=27.3d0
  maxwid0=8.27d0 ;In double precision, gauss_pdf(x) saturates (becomes 1d0) between x=8.26d0 and x=8.27d0
  ;maxwid0=5.888d0 ;In double precision, erf(x) saturates (becomes 1d0) between x=5.8870 and x=5.888d0
  a=width^2/(4d0*alog(2d0)) ;The constant that will go into the exponential (exp(-x^2/a))
  g=1d0/sqrt(a*!dpi) ;Constant that multiplies the exponential in the Gaussian (g*exp(-x^2/a))
  wid0=sqrt(a/2d0)*maxwid0
  wid1=sqrt(a/2d0)*maxwid1
  ;Calculate the integral
  ;This is not numerical integration: it is analytical integration of a function made of a finite number of pieces
  if (local) then begin ;Literal interpretation of the function
    ;Calculate the locations of the start and end of the interval that will go into each output point
    minloc1=(value_locate(ix,ix-wid1))>0L
    maxloc1=(value_locate(ix,ix+wid1)+1L)<(nx-2L) 
    minloc0=(value_locate(ix,ix-wid0))>0L
    maxloc0=(value_locate(ix,ix+wid0)+1L)<(nx-2L)
    ;The integral is of y(x)*g(x-x0)*dx, with y(x)=c0+c1*x
    ;Terms to calculate c0 and c1, coefficients of y(x)=c0+c1*x
    y1=iy[1:nx-1] & y0=iy[0:nx-2] & x1=ix[1:nx-1] & x0=ix[0:nx-2]
    c0=(y0*x1-y1*x0)/(x1-x0)
    c1=(y1-y0)/(x1-x0)
    ;Integral of the first term (c0*g(x-x0)*dx)
    w=where((maxloc0 gt minloc0),nw)
    ret0=dblarr(nx)
    for i=0,nw-1 do begin
      j=w[i]
      xl=ix[j]
      rng=lindgen(maxloc0[j]-minloc0[j]+1)+minloc0[j]
      x0l=(x0[rng]-xl)/sqrt(a)
      x1l=(x1[rng]-xl)/sqrt(a)
      d=(c0+c1*xl)[rng]
      ret0[j]=total(d*(erf(x1l)-erf(x0l)))
      ;ret0[j]+=total(d*(gauss_pdf(x1l)-gauss_pdf(x0l)))
    endfor
    ;Integral of the second term (c1*x*g(x-x0)*dx)
    w=where((maxloc1 gt minloc1),nw)
    ret1=dblarr(nx)  
    for i=0,nw-1 do begin
      j=w[i]
      xl=ix[j]
      rng=lindgen(maxloc1[j]-minloc1[j]+1)+minloc1[j]
      x0l=(x0[rng]-xl)/sqrt(a)
      x1l=(x1[rng]-xl)/sqrt(a)
      ret1[j]+=total(c1[rng]*(exp(-x0l^2)-exp(-x1l^2)))
    endfor
    ret=g*0.5d0*(sqrt(!dpi*a)*ret0+a*ret1)
    ;ret=g*(ret0+0.5d0*a*ret1)
  endif else begin
    ;Calculate the locations of the start and end of the interval that will go into each output point
    xstart=[ix[0]-(ix[1]-ix[0])/2d0,(ix[0:nx-2]+ix[1:nx-1])/2d0]
    xend=[xstart[1:nx-1],ix[nx-1]+(ix[nx-1]-ix[nx-2])/2d0]
    minloc0=(value_locate(xstart,xstart-wid0))>0L
    maxloc0=(value_locate(xend,xend+wid0)+1L)<(nx-1L)
    ;The integral is of y(x)*g(x-x0)*dx, with y(x)=c0
    xsig=ix/sqrt(a/2d0) ;x normalized by the Gaussian standard deviation
    xstartsig=xstart/sqrt(a/2d0) ;x normalized by the Gaussian standard deviation
    xendsig=xend/sqrt(a/2d0) ;x normalized by the Gaussian standard deviation
    w=where((maxloc0 gt minloc0),nw)
    ret=dblarr(nx)
    for i=0L,nw-1 do begin
      j=w[i]
      ret[j]+=total(iy[minloc0[j]:maxloc0[j]]*(gauss_pdf(xendsig[minloc0[j]:maxloc0[j]]-xsig[j])-gauss_pdf(xstartsig[minloc0[j]:maxloc0[j]]-xsig[j])))
    endfor  
  endelse
endif

if (~local) then begin
  ;Calculate the start and en of each output point
  xstart=[ix[0]-(ix[1]-ix[0])/2d0,(ix[0:nx-2]+ix[1:nx-1])/2d0]
  xend=[xstart[1:nx-1],ix[nx-1]+(ix[nx-1]-ix[nx-2])/2d0]
;Create xbox and ybox arrays, with constant y values spanning the start and end of each x point
  if (arg_present(xbox) || arg_present(ybox)) then begin
    nix=n_elements(ix)
    xbox=dblarr(2L*nix) & ybox=dblarr(2L*nix)
    xbox[0:*:2]=xstart & xbox[1:*:2]=xend
    ybox[0:*:2]=iy & ybox[1:*:2]=iy
  endif
endif

return,ret
end
