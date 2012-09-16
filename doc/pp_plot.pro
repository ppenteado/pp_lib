; docformat = 'rst'
;+
; :Author: Paulo Penteado (pp.penteado@gmail.com), May/2010
; :History: Improved on algorithm to decide on tick locations (created
; pp_plot_decideintervals). Jul/2011
; :Version: 20110722
;-



pro pp_plot_decideintervals,dprange,dticks,dint,decide=decide,newrange=newrange,dminor=dminor,newdticks=newdticks;New algorithm
compile_opt idl2,logical_predicate,hidden
  intervals=[2d0,1d0,1d0,0.5d0];,0.25d0] ;Possible interval multipliers to use
  intrats=[0.2d0,0.1d0,1d0,0.5d0];,0.25d0] ;Possible normalized interval multipliers
  minors=[4,10,10,5]
  nintervals=n_elements(intervals)
  ints=[6,5,4,3] ;Possible number of intervals
  nints=n_elements(ints)
  mindiff=!values.d_infinity
  for i=0,nints-1 do begin ;Try every number of intervals, to find out which one gives nicer intervals
    dints=double(dprange[1]-dprange[0])/ints[i]
    al=alog10(dints)
    dints=al ge 0d0 ? 1d1^((al mod 1)-1d0) : 1d1^(al mod 1)
    tmp=min(abs(dints-intrats),minloc)
    if ((tmp-mindiff) lt -(machar(/double)).eps) then begin
      mindiff=tmp
      minint=intervals[minloc]
      sints=ints[i]
      dint=intrats[minloc]*1d1^ceil(al)
      dminor=minors[minloc]
    endif
    ;print,ints[i],minint,tmp,sints,dint
  endfor
  newrange=dint*[floor(dprange[0]/dint),ceil(dprange[1]/dint)]
  dticks=sints+1d0
  newdticks=(newrange[1]-newrange[0])/dint
end

;+
; :Hidden: Intended to be called only by `pp_plot`
; :Description: Calculates the tick values, numbers, intervals and data ranges for `pp_plot` below.
;-
pro pp_plot_maketicks,dprange,drange,d,dtickinterval,dticks,dint,dstyle,dtickv,det,yaxis=yaxis,decide=decide,dminor=dminor
compile_opt idl2,hidden
  ;Defaults  
  yaxis=n_elements(yaxis) eq 1 ? yaxis : 0B

  print,decide ? "pp_plot: setting tick locations (with pp_plot_maketicks), instead of using plot's default" $
   : "pp_plot: setting tick locations (with pp_plot_decideintervals), instead of using plot's default"
  
  ;Get the syle flags
  dexact=(dstyle and 1) eq 1
  dextend=(dstyle and 2) eq 2
  dzero=(dstyle and 16) eq 16
  ;Set number of ticks and interval size to match
  ;Determine the data and plot ranges
  dprange=array_equal([0d0,0d0],drange) ? [min(d,max=dmax,/nan),dmax] : drange
  if (yaxis && ~dzero && ~dexact) then dprange[0]<=0d0  
  if (dtickinterval ne 0d0) then begin ;Interval size takes precedence over number
    dticks=floor((dprange[1]-dprange[0])/dtickinterval)
    dint=dtickinterval
  endif else begin
    if (dticks eq 0) then begin
      if (~decide) then begin ;Old algorithm to decide on tick positions
        dticks=3 ;Default number of ticks to use, if interval was also not provided
        dint=(dprange[1]-dprange[0])/dticks
      endif else begin
        pp_plot_decideintervals,dprange,dticks,dint,decide=decide,newrange=newrange,dminor=dminor,newdticks=newdticks;New algorithm
        dticks--
      endelse
    endif
  endelse
  if (~dexact) then begin
    if (~decide) then begin
      dprange+=[-dint,dint] ;If range is not exact, add one interval to each side
      dint=(dprange[1]-dprange[0])/dticks
    endif else begin
      dprange=newrange
      dticks=newdticks
    endelse
  endif
  if (~decide) then begin
    if (dextend) then dprange+=[-dint,dint]*0.5d0 else $;If extend range is set, add a half interval to each side
    dtickv=dprange[0]+dindgen(dticks+1)*dint
  endif else begin
    dtickv=dprange[0]+dindgen(dticks+1)*dint
    if (dextend) then dprange+=([-dint,dint]/dminor)*21d0/34d0    
  endelse
  if ~((det and 1) eq 1) then dtickv=dtickv[1:*] 
  if ~((det and 2) eq 2) then dtickv=dtickv[0:n_elements(dtickv)-2]
  dticks=n_elements(dtickv)-1
end

;+
; :Hidden: Intended to be called only by `pp_plot`
; :Description:
;   Uses the state of `multiplot_pp` to determine `xendticks` and `yendticks`. Does
;   nothing to these values if a valid `multiplot_pp` state is not found.
;   
;   This code is encapsulated into this routine to keep the code insulate from the
;   common variables used by `multiplot_pp`.   
;-
pro pp_plot_get_xyendticks,xendticks,yendticks
compile_opt idl2,logical_predicate,hidden

common multiplot_pp $
  ,nplots $ ; [# of plots along x, # of plots along y]
  ,nleft $  ; # of plots remaining---like the first element of !p.multi
  ,pdotmulti $  ; saved value of !p.multi
  ,margins $  ; calculated margins based on !p.multi or pmulti
  ,pposition $  ; saved value of !p.position
  ,colmajor $ ; flag for column major order
  ,noerase $  ; saved value of !p.noerase
  ,xtickname $  ; Original value
  ,ytickname $  ; Original value
  ,xtickformat $; Original value
  ,ytickformat,$  ; Original value
  ontop,onright ;Whether the current plot will be on the bottom line or the left column

;Get out if the multiplot variables were not defined
if n_elements(ontop) eq 0 then return

;Set x|yendticks according to the plot location on the grid
xendticks=onright ? 3 : 1
yendticks=ontop ? 3 : 1

print,'pp_plot: setting xendticks to ',strtrim(xendticks,2),' and yendticks to ',strtrim(yendticks,2)

end


;+
; :Description:
;    Wrapper to IDL's plot procedure, to allow supressing the first and/or last tick values, so 
;    that they do not overlap with neighbouring plots (particularly if using `multiplot_pp`).
;
; :Params:
;    x : in, required
;      Passed to IDL's plot, see its help. Used to determine the x range if xrange is not provided (or the
;      y range if y is not provided).
;    y : in, optional
;      Passed to IDL's plot, see its help. Used to determine the y range if yrange is not provided.
;
; :Keywords:
;    use_multiplot : in, optional, default=0
;      If set, `xendticks` and `yendticks` are determined by the plot's position
;      in the grid set by `multiplot_pp`: Plots not on the top row get the top
;      y tick suppressed (`yendticks`=1), plots not on the right column get the right
;      x tick suppressed (`xendticks`=1).
;    calculate_ticks : in, optional, default=0
;      If set and tick values are not provided, the tick values are defined here,
;      instead of letting IDL's plot do it. Useful to keep plots with ticks suppressed
;      (in which case auto tick locations are calculated here) consistent with those
;      made with no suppresion (which otherwise would have their ticks determined by IDL's
;      plot).
;    xendticks : in, optional, default=3
;      Its value determines the kind of supression of the end ticks for the x axis:
;      
;      0 - No end ticks are printed (both the first and last are supressed).
;      
;      1 - Only the last is supressed, the first tick is left untouched.
;      
;      2 - Only the first is supressed, the last tick is left untouched.
;      
;      3  - No supression, this routine just passes its arguments to plot, with no changes. 
;    yendticks : in, optional, default=3
;      Its value determines the kind of supression of the end ticks for the y axis:
;      
;      0 - No end ticks are printed (both the first and last are supressed).
;      
;      1 - Only the last is supressed, the first tick is left untouched.
;      
;      2 - Only the first is supressed, the last tick is left untouched.
;      
;      3  - No supression, this routine just passes its arguments to plot, with no changes.
;    _extra : in, optional
;      Any other keywords are just passed to plot, unaltered.
;    xrange : in, optional
;      Passed to IDL's plot, see its help.
;    xtickv : in, optional
;      Passed to IDL's plot, see its help.
;    xticks : in, optional
;      Passed to IDL's plot, see its help.
;    xtickinterval : in, optional
;      Passed to IDL's plot, see its help.
;    xstyle : in, optional
;      Passed to IDL's plot, see its help.
;    xtick_get : out, optional
;      Passed to IDL's plot, see its help.
;    yrange : in, optional
;      Passed to IDL's plot, see its help.
;    ytickv : in, optional
;      Passed to IDL's plot, see its help.
;    yticks : in, optional
;      Passed to IDL's plot, see its help.
;    ytickinterval : in, optional
;      Passed to IDL's plot, see its help.
;    ystyle : in, optional
;      Passed to IDL's plot, see its help.
;    ytick_get : out, optional
;      Passed to IDL's plot, see its help.
;      
; :Examples:
;   To make a plot supressing both end marks on x, but supress only the bottom mark on y::
;   
;     pp_plot,dindgen(10),dindgen(10)*2,xendticks=0,yendticks=2,xstyle=1,ystyle=1,xticks=5,xminor=4
;     ;pp_plot: setting tick locations (with pp_plot_maketicks), instead of using plot's default
;     ;pp_plot: setting tick locations (with pp_plot_maketicks), instead of using plot's default
;   
;   To make multiplots (using `multiplot_pp`, a variation of IDLastro's multiplot) where the end marks do not overlap::
;   
;      multiplot_pp,[2,2]
;      pp_plot,[0,1],[0,1],xstyle=1,ystyle=1,/use_multiplot,/calculate_ticks
;      ;pp_plot: setting xendticks to 1 and yendticks to 3
;      ;pp_plot: setting tick locations (with pp_plot_maketicks), instead of using plot's default
;      ;pp_plot: setting tick locations (with pp_plot_maketicks), instead of using plot's default
;      multiplot_pp
;      pp_plot,[0,1],[0,1],xstyle=1,ystyle=1,/use_multiplot,/calculate_ticks
;      ;pp_plot: setting xendticks to 3 and yendticks to 3
;      ;pp_plot: setting tick locations (with pp_plot_maketicks), instead of using plot's default
;      ;pp_plot: setting tick locations (with pp_plot_maketicks), instead of using plot's default
;      multiplot_pp
;      pp_plot,[0,1],[0,1],xstyle=1,ystyle=1,/use_multiplot,/calculate_ticks
;      ;pp_plot: setting xendticks to 1 and yendticks to 1
;      ;pp_plot: setting tick locations (with pp_plot_maketicks), instead of using plot's default
;      ;pp_plot: setting tick locations (with pp_plot_maketicks), instead of using plot's default
;      multiplot_pp
;      pp_plot,[0,1],[0,1],xstyle=1,ystyle=1,/use_multiplot,/calculate_ticks
;      ;pp_plot: setting xendticks to 3 and yendticks to 1
;      ;pp_plot: setting tick locations (with pp_plot_maketicks), instead of using plot's default
;      ;pp_plot: setting tick locations (with pp_plot_maketicks), instead of using plot's default
;      
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), May/2010
; 
; :History:
;    Added the keywords `use_multiplot` and `calculate_ticks`. Apr/2011.
;-
pro pp_plot,x,y,xendticks=xet,yendticks=yet,_extra=ex, $
 xrange=ixrange,xtickv=ixtickv,xticks=xticks,xtickinterval=ixtickinterval,xstyle=xstyle,xtick_get=xtick_get,xminor=xminor, $
 yrange=iyrange,ytickv=iytickv,yticks=yticks,ytickinterval=iytickinterval,ystyle=ystyle,ytick_get=ytick_get,yminor=yminor, $
 use_multiplot=usemult,calculate_ticks=calcticks
compile_opt idl2

;Defaults
xet=n_elements(xet) eq 1 ? xet : 3
yet=n_elements(yet) eq 1 ? yet : 3
usemult=n_elements(usemult) eq 1 ? usemult : 0B
calcticks=n_elements(calcticks) eq 1 ? calcticks : 0B

;Auto set xendticks and yendticks according to what multiplot_pp is doing
if usemult then pp_plot_get_xyendticks,xet,yet
  
;Do nothing special if xet and yet are both 3 and no ticks are set here 
if ((xet eq 3) && (yet eq 3) && (~ calcticks)) then begin
  plot,x,y,_extra=ex, $
   xrange=ixrange,xtickv=ixtickv,xticks=xticks,xtickinterval=ixtickinterval,xstyle=xstyle,xtick_get=xtick_get, $
   yrange=iyrange,ytickv=iytickv,yticks=yticks,ytickinterval=iytickinterval,ystyle=ystyle,ytick_get=ytick_get
  return
endif

;Get the relevant keywords to use
if (xet ne 3)||calcticks then begin
  xtickv=n_elements(ixtickv) ne 0 ? double(ixtickv) : !x.tickv
  xticks=n_elements(xticks) eq 1 ? xticks : !x.ticks
  xtickinterval=n_elements(ixtickinterval) eq 1 ? double(ixtickinterval) : !x.tickinterval
  xstyle=n_elements(xstyle) eq 1 ? xstyle : !x.style
  xsupress=(xstyle and 4) eq 4
  xrange=n_elements(ixrange) eq 2 ? double(ixrange) : !x.range
  xt0=array_equal(xtickv,replicate(0,n_elements(xtickv)))
  xminor=n_elements(xminor) eq 1 ? xminor : !x.minor
endif
if (yet ne 3)||calcticks then begin
  ytickv=n_elements(iytickv) ne 0 ? double(iytickv) : !y.tickv
  yticks=n_elements(yticks) eq 1 ? yticks : !y.ticks
  ytickinterval=n_elements(iytickinterval) eq 1 ? double(iytickinterval) : !y.tickinterval
  ystyle=n_elements(ystyle) eq 1 ? ystyle : !y.style
  ysupress=(ystyle and 4) eq 4
  yrange=n_elements(iyrange) eq 2 ? double(iyrange) : !y.range
  yt0=array_equal(ytickv,replicate(0,n_elements(ytickv)))
  yminor=n_elements(yminor) eq 1 ? yminor : !y.minor
endif

;Make the xticks if they were not provided
if (n_elements(y) gt 0) then begin ;If both x and y were provided
  ;X axis
  if (calcticks || (xet ne 3))&&xt0 then $
   pp_plot_maketicks,xprange,xrange,double(x),xtickinterval,xticks,xint,xstyle,xtickv,xet,decide=calcticks,dminor=xminor
  ;Y axis
  if (calcticks || (yet ne 3))&&yt0 then $
   pp_plot_maketicks,yprange,yrange,double(y),ytickinterval,yticks,yint,ystyle,ytickv,yet,/yaxis,decide=calcticks,dminor=yminor
endif else begin ;If only x is provided, in which case it is interpreted as y, and x is an index array.
  ;X axis
  if (calcticks || (xet ne 3))&&xt0 then $
   pp_plot_maketicks,xprange,xrange,dindgen(n_elements(x)),xtickinterval,xticks,xint,xstyle,xtickv,xet,decide=calcticks,dminor=xminor
  ;Y axis
  if (calcticks || (yet ne 3))&&yt0 then $
   pp_plot_maketicks,yprange,yrange,double(x),ytickinterval,yticks,yint,ystyle,ytickv,yet,/yaxis,decide=calcticks,dminor=yminor
endelse

if calcticks||(((xet ne 3) && (yet ne 3))) then $ ;If both x and y ticks were set here
 plot,x,y,_extra=ex, $
 xrange=n_elements(xprange) eq 2 ? xprange : xrange,xtickv=xtickv,xticks=xticks,xtickinterval=xtickinterval,xtick_get=xtick_get,xminor=xminor,xstyle=(xstyle or 1) and (not 2), $
 yrange=n_elements(yprange) eq 2 ? yprange : yrange,ytickv=ytickv,yticks=yticks,ytickinterval=ytickinterval,ytick_get=ytick_get,yminor=yminor,ystyle=(ystyle or 1) and (not 2)
if ((xet ne 3) && (yet eq 3)) then $ ;If only x ticks were set here
 plot,x,y,_extra=ex, $
 xrange=n_elements(xprange) eq 2 ? xprange : xrange,xtickv=xtickv,xticks=xticks,xtickinterval=xtickinterval,xtick_get=xtick_get,xminor=xminor,xstyle=(xstyle or 1) and (not 2), $
 yrange=iyrange,ytickv=iytickv,yticks=yticks,ytickinterval=iytickinterval,ystyle=ystyle,ytick_get=ytick_get
if ((xet eq 3) && (yet ne 3)) then $ ;If only y ticks were set here
 plot,x,y,_extra=ex, $
 xrange=ixrange,xtickv=ixtickv,xticks=xticks,xtickinterval=ixtickinterval,xstyle=xstyle,xtick_get=xtick_get, $
 yrange=n_elements(yprange) eq 2 ? yprange : yrange,ytickv=ytickv,yticks=yticks,ytickinterval=ytickinterval,ytick_get=ytick_get,yminor=yminor,ystyle=(ystyle or 1) and (not 2)


end

