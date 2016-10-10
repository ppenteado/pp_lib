; docformat = 'rst'
;+
; :Author: Paulo Penteado (pp.penteado@gmail.com), Feb/2010
;-
;+
; :Description:
;    Calculates the area under the y(x) curve provided. The function must be ordered in x, but it does not
;    matter whether the order is increasing or decreasing.
;    
;    There are two possible ways that the input function is interpreted, determined by the keyword local.
;    See the description of local below, since it may significantly alter the result.
;    
;    If the method selected requires interpolation, it can be linear, 3 or 4-point quadratic, or spline. This is determined
;    by setting the quadratic, lsquadratic, or spline keywords (linear if none of these 3 keywords is set).
;
; :Returns:
;    If xmin and xmax are not provided, returns the area under the curve over its whole extension.
;    
;    If xmin and xmax are scalars, returns the area between these x values. If they are arrays of n elements,
;    returns the n areas, calculated starting at each xmin element, and ending at the corresponding xmax element.
;    
;    Every value in xmax is must be larger than or equal to the correspoinding value in xmin.
;     
; :Params:
;    x : in, required
;      An array of locations where the function is sampled. Must be ordered (increasing or decreasing).
;    y : in, required 
;      An array with the function values corresponding to the locations in x.
; :Keywords:
;    xmin : in, out, optional
;      A scalar or array with the start of the range(s) where the area is to be calculated. If not provided,
;      the minimum location of the function is used. Must have the same number of elements as xmax, and every
;      element in xmin must be smaller than or equal to the corresponding element in xmax. If any value of xmin
;      is smaller than the beginning of the x range, it is clipped to the beginning.
;    xmax : in, out, optional
;      A scalar or array with the end of the range(s) where the area is to be calculated. If not provided,
;      the maximum location of the function is used. Must have the same number of elements as xmin, and every
;      element in xmax must be larger than or equal to the corresponding element in xmin. If any value of xmax
;      is larger than the end of the x range, it is clipped to the end.
;    cumulative : in, optional, default=0
;      If set, and xmin and xmax are not provided, the areas returned are the cumulative areas at the end of each x point,
;      starting at the first x point. If xmax and xmin are provided, this keyword is ignored.
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
;    newton : in, optional, default=0
;      If local is set and newton is set, the function integrations are done with int_tabulated, which
;      uses a 5 point Newton-Cotes formula. If not set, trapezoid integration is done. If local is not set, this has no effect.  
;    local : in, optional, default=0
;      Determines how the input function is interpreted, which determines how the areas are calculated. If set, the function is
;      interpreted literally, that is, as in a simple mathematical function: the y values are the local evaluation y(x).
;      
;      If not set, the y values are interpreted as a measured sample, that is, as the average of the "flux" falling inside the region
;      centered at the corresponding x values: x locations are interpreted as the centers of bins. Since this interpretation is equivalent
;      to a literal interpretation of a function made of rectangular regions, in this method there are no interpolations (partial areas, if
;      any, are just the corresponding fractions of the rectangles).
;    lsquadratic : in, optional, default=0
;      If the method requires interpolation, this is passed on to interpol, to select 4 point quadratic interpolation.
;      
;      If none of lsquadratic, quadratic and spline are set, interpolation is linear. 
;    quadratic : in, optional, default=0
;      If the method requires interpolation, this is passed on to interpol, to select 3 point quadratic interpolation.
;      
;      If none of lsquadratic, quadratic and spline are set, interpolation is linear.
;    spline : in, optional, default=0
;      If the method requires interpolation, this is passed on to interpol, to select spline interpolation.
;      
;      If none of lsquadratic, quadratic and spline are set, interpolation is linear.
;      
;
; :Examples:
; 
;   Make up a simple constant function to show the difference between local and non-local modes::
;   
;     x=[0d0,1d0]
;     y=[2d0,2d0]
;     print,pp_integral(x,y,xmin=xmin,xmax=xmax)
;     ;4.0000000
;     print,xmin,xmax
;     ;-0.50000000       1.5000000
;     ;(the x locations are considered the middle of the bins, so the bins extend beyond the range min(x),max(x)) 
;     print,pp_integral(x,y,xmin=lxmin,xmax=lxmax,/local)
;     ;2.0000000
;     print,lxmin,lxmax
;     ;0.0000000       1.0000000
;     
;   Make up a well sampled function, so that the difference between local and non-local is small::
;   
;     x=dindgen(10001)*!dpi/1d4
;     y=sin(x)
;     a=pp_integral(x,y,xmin=[0d0,0d0],xmax=!dpi*[0.5d0,1d0])
;     b=pp_integral(x,y,xmin=[0d0,0d0],xmax=!dpi*[0.5d0,1d0],/local)
;     print,a
;     ;1.0003142       2.0000000
;     print,b
;     ;1.0000000       2.0000000
;     print,a-b
;     ;0.00031410992  -4.9348009e-08
;     
;   
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Feb/2010
;-
function pp_integral,x,y,xmin=xmin,xmax=xmax,cumulative=cumulative,$
 xinc=ix,yinc=iy,xstart=xstart,xend=xend,xbox=xbox,ybox=ybox,$
 newton=newton,local=local,$
 lsquadratic=lsquadratic,quadratic=quadratic,spline=spline
compile_opt idl2

;Check that the dimensions of x and y match and are acceptable
nx=n_elements(x) & ny=n_elements(y)
if (nx ne ny) || (nx eq 0) then message,'x and y provided must be arrays of the same length'

;Check that the dimensions of xmin and xmax match and are acceptable
nxmin=n_elements(xmin) & nxmax=n_elements(xmax)
if (nxmin ne nxmax) then message,'xmin and xmax provided must be arrays of the same length'

;Defaults
local=n_elements(local) eq 1 ? local : 0
newton=local ? 0 : n_elements(newton) eq 1 ? newton : 0
cumulative=n_elements(cumulative) eq 1 ? cumulative : 0

;Make ix and iy to work with, 1D and in increasing order (x is assumed sorted, but can be increasing or decreasing order)
if (x[nx-1] lt x[0]) then begin
  ix=reverse(reform(x)) & iy=reverse(reform(y))
endif else begin
  ix=reform(x) & iy=reform(y)
endelse

if (~local) then begin ;Input function is histogram-like (rectangle rule)
  ;Calculate the x start and end points for the tabulated function
  xstart=[ix[0]-(ix[1]-ix[0])/2d0,(ix[0:nx-2]+ix[1:nx-1])/2d0]
  xend=[xstart[1:nx-1],ix[nx-1]+(ix[nx-1]-ix[nx-2])/2d0]
  ;Default interval
  xmin=n_elements(xmin) gt 0 ? xmin : cumulative ? replicate(xstart[0],nx) : xstart[0]
  xmax=n_elements(xmax) gt 0 ? xmax : cumulative ? xend : xend[nx-1]
  ;Clip the edges of the interval if they exceeed the input function's range 
  xmin=xstart[0]>xmin<xend[nx-1]
  xmax=xstart[0]>xmax<xend[nx-1]
  nlocs=n_elements(xmin)
  minloc=value_locate(xstart,xmin)
  maxloc=(value_locate(xend,xmax)+1)<(nx-1)
  ;Area of the partial rectangles at the edges
  ret=(xend[minloc]-xmin)*iy[minloc]+(xmax-xend[maxloc])*iy[maxloc]
  ;Area where min and max fall in the same rectangle 
  w=where((maxloc-minloc) eq 0L,nw)
  if (nw gt 0L) then ret[w]=(xmax[w]-xmin[w])*iy[maxloc[w]]
  ;Area of the full rectangles contained in the output intervals
  w=where((maxloc-minloc) gt 0L,nw)
  if (nw gt 0L) then begin
    areas=total((xend-xstart)*iy,/cumulative) ;The cumulative area of the rectangles
    ret[w]+=areas[maxloc[w]]-areas[minloc[w]]
  endif
endif else begin ;Input function is interpreted literally
  ;Default interval
  xmin=n_elements(xmin) gt 0 ? xmin : cumulative ? replicate(ix[0],nx) : ix[0]
  xmax=n_elements(xmax) gt 0 ? xmax : cumulative ? ix : ix[nx-1]
  ;Clip the edges of the interval if they exceeed the input function's range
  xmin=ix[0]>xmin<ix[nx-1]
  xmax=ix[0]>xmax<ix[nx-1]
  minloc=value_locate(ix,xmin)
  maxloc=value_locate(ix,xmax)
  ymins=interpol(iy,ix,xmin,lsquadratic=lsquadratic,quadratic=quadratic,spline=spline)
  ymaxs=interpol(iy,ix,xmax,lsquadratic=lsquadratic,quadratic=quadratic,spline=spline)
  if (newton) then begin ;Use int_tabulated, that does a 5 point Newton-Cotes integration
    nlocs=n_elements(xmin)
    ret=dblarr(nlocs)
    for i=0L,nlocs-1 do begin
      xtmp=[xmin[i],ix[minloc[i]:maxloc[i]:minloc[i] le maxloc[i] ? 1L : -1L],xmax[i]]
      ytmp=[ymins[i],iy[minloc[i]:maxloc[i]:minloc[i] le maxloc[i] ? 1L : -1L],ymaxs[i]]
      ret[i]=int_tabulated(xtmp,ytmp,/double)
    endfor
  endif else begin ;Use trapezoidal integration
    ;Area of the partial trapezoids at the edges
    ret=(ix[(minloc+1)<(nx-1)]-xmin)*(ymins+iy[(minloc+1)<(nx-1)])*0.5d0
    w=where(ix[minloc] eq xmin,nw)
;    if (nw gt 0L) then ret[w]=0d0
    ret+=(xmax-ix[maxloc])*(ymaxs+iy[maxloc])*0.5d0
    ;Area where min and max fall in the same trapezoid 
    w=where((maxloc-minloc) eq 0L,nw)
    if (nw gt 0L) then ret[w]=(xmax[w]-xmin[w])*(ymaxs[w]+ymins[w])*0.5d0
    ;Area of the full trapezoids contained in the output intervals
    w=where((maxloc-minloc) gt 1L,nw)
    if (nw gt 0L) then begin
      areas=[0d0,total((ix[1:*]-ix[0:nx-1])*(iy[1:*]+iy[0:nx-1])*0.5d0,/cumulative)] ;The cumulative area of the trapezoids
      ret[w]+=areas[maxloc[w]]-areas[(minloc[w]+1)<(nx-1)]
    endif
  endelse
endelse

;Create xbox and ybox arrays, with constant y values spanning the start and end of each x point
if (~local && (arg_present(xbox) || arg_present(ybox))) then begin
  nix=n_elements(ix)
  xbox=dblarr(2L*nix) & ybox=dblarr(2L*nix)
  xbox[0:*:2]=xstart & xbox[1:*:2]=xend
  ybox[0:*:2]=iy & ybox[1:*:2]=iy
endif
 
return,ret
end
