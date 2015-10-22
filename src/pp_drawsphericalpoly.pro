; docformat = 'rst'
;+
; :Author: Paulo Penteado (http://www.ppenteado.net), Aug/2015
;-


;+
; :Hidden:
; 
; :Author: Paulo Penteado (http://www.ppenteado.net), Aug/2015
;-
pro pp_drawsphericalpoly_cg,paths,colors,_ref_extra=ex,$
  irgbt
compile_opt idl2,logical_predicate,hidden

if n_elements(irgbt) then begin
  rgbt=pp_colortripletolong(irgbt)
  foreach p,paths,ip do cgpolygon,p[0,*],p[1,*],color=rgbt[colors[ip]],_strict_extra=ex
endif else begin
  foreach p,paths,ip do cgpolygon,p[0,*],p[1,*],color=colors[ip],_strict_extra=ex
endelse

end

;+
; :Hidden:
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Aug/2015
;-
pro pp_drawsphericalpoly_direct,paths,colors,_ref_extra=ex,$
  irgbt,stackmap=stackm,original_image=origim,maxstack=maxstack,$
  stacklist=stacklist,stackcount=stackc,verbose=verbose,do_stack=do_stack,$
  weights=weights,stackweights=stackw,doweight=dow,stackindex=stacki,doi=doi,pcount=pcount
  compile_opt idl2,logical_predicate,hidden


if do_stack then begin
  if dow && (n_elements(weights) ne n_elements(colors)) then weights=replicate(1d0,n_elements(colors))
  origim=tvrd()
  mapim=tvrd(channel=0)
  szm=size(mapim,/dimensions)
  stackc=lon64arr(szm)
  if do_stack then pcount=lon64arr(n_elements(colors))
  if do_stack eq 1 then begin
    maxstack=n_elements(maxstack) ? maxstack : n_elements(paths)
    stackm=dblarr([maxstack,szm])+!values.d_nan
    if dow then stackw=stackm
    if doi then stacki=stackm
  endif else begin
    stackm=objarr(szm)
    foreach s,stackm,is do stackm[is]=list()
    if dow then begin
      stackw=objarr(szm)
      foreach s,stackw,is do stackw[is]=list()
    endif
    if doi then begin
      stacki=objarr(szm)
      foreach s,stacki,is do stacki[is]=list()
    endif
  endelse
  foreach p,paths,ip do begin
    erase
    polyfill,p[0,*],p[1,*],/data,_strict_extra=ex
    if verbose && ~(ip mod verbose) then print,ip
    tmp=tvrd(channel=0)
    w=where(tmp,wc)
    pcount[ip]=wc
    if wc then begin
      cip=colors[ip]
      if dow then wip=weights[ip]
      if do_stack eq 1 then begin
        foreach pt,w do if stackc[pt] lt maxstack then stackm[pt*maxstack+stackc[pt]]=cip
        if dow then foreach pt,w do if stackc[pt] lt maxstack then stackw[pt*maxstack+stackc[pt]]=wip
        if doi then foreach pt,w do if stackc[pt] lt maxstack then stacki[pt*maxstack+stackc[pt]]=pt
      endif
      if do_stack eq 2 then begin
        foreach pt,w do (stackm[pt]).add,cip
        if dow then foreach pt,w do (stackw[pt]).add,wip
        if doi then foreach pt,w do (stacki[pt]).add,pt
      endif
      stackc[w]+=1
    endif
  endforeach
  return
endif
  
if n_elements(irgbt) then begin
  device,get_decomposed=dec
  if dec then begin
    rgbt=pp_colortripletolong(irgbt)
    foreach p,paths,ip do polyfill,p[0,*],p[1,*],color=rgbt[colors[ip]],/data,_strict_extra=ex
  endif else begin
    tvlct,r,g,b,/get
    tvlct,transpose(irgbt)
    foreach p,paths,ip do polyfill,p[0,*],p[1,*],color=colors[ip],/data,_strict_extra=ex
    tvlct,r,g,b
  endelse
endif else begin
  foreach p,paths,ip do polyfill,p[0,*],p[1,*],color=colors[ip],/data,_strict_extra=ex
endelse

end

;+
; :Hidden:
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Aug/2015
;-
pro pp_drawsphericalpoly_itool,paths,colors,_ref_extra=ex,$
  irgbt,polygon=poly,connectivity=conn,x=x,y=y,graphic=graphic
compile_opt idl2,logical_predicate,hidden

xy=paths.toarray(dimension=2)
conn=lonarr(n_elements(paths)+n_elements(xy)/2LL)
count=0LL
cols=lonarr(3,n_elements(xy)/2LL)
countc=0LL
foreach p,paths,ip do begin
  np=n_elements(p)/2LL
  conn[count]=np
  conn[count+1]=l64indgen(np)+countc
  count+=np+1
  cols[0,countc]=n_elements(irgbt) ? irgbt[*,colors[ip]]#replicate(1d0,np) : colors[*,ip]#replicate(1d0,np)
  countc+=np
endforeach
x=reform(xy[0,*])
y=reform(xy[1,*])

if keyword_set(graphic) then poly=polygon(x,y,connectivity=conn,vert_colors=cols,_strict_extra=ex,/data) else begin
  ipolygon,transpose([[x],[y]]),connectivity=conn,vert_colors=cols,_strict_extra=ex,/data,/visualization,object=poly
endelse

end


;+
; :Description:
;    Draws polygons on a map, with the polygon sides resampled to a large number
;    of vertices, so that the result resembles a spherical polygon (a polygon where
;    the sides are great circle arcs). Polygons can be drawn on Coyote Graphics,
;    iTools, Function Graphics or Direct Graphics. Se examples below.
;    Makes use of `pp_sphericalpath` to calculate the polygonal approximation to
;    the spherical polygon.
;
; :Params:
;    lons: in, required
;      An array of longitudes for the vertices which are to be connected by a path made of great circles.
;      Must be in degrees. Multiple polygons are supported in two different ways: 1) If all N polygons have 
;      the same number of vertices (M), lons can be given as a [M,N] array. 2) For arbitrary numbers of vertices,
;      lons is given as a list, where each list element is an array of vertices for one polygon.
;    lats: in, required
;      An array of latitudes for the vertices which are to be connected by a path made of great circles.
;      Must be in degrees. Multiple polygons are supported in two different ways: 1) If all N polygons have 
;      the same number of vertices (M), lats can be given as a [M,N] array. 2) For arbitrary numbers of vertices,
;      lons is given as a list, where each list element is an array of vertices for one polygon.
;    colors: in, required
;      An array with the color to be used to draw/fill the polygons. If `rgb_table`
;      is not given, this array is assumed to contain the colors in the system used
;      by the kind of plotting selected: either a [3,M] array of color triplets, one triple
;      per each of the M polygons, or a long integer array, with one long-integer-coded
;      color for each of the M polygons.
;      If `rgb_table` is given, then colors can be an array of any numerical type, and
;      the polygon colors will be determined by mapping the values in `colors` to
;      values in the 256-value colortable specified by `rgb_table`.
;
; :Keywords:
;    _ref_extra: in, out, optional
;      Any extra arguments are passed to the polygon plotting routine: cgpolygon if
;      `cg` is selected, ipolygon if `itool` is selected, polygon() if `graphics` is
;      selected, or polyfill, if `direct` is selected.
;    rgb_table: in, optional
;      The color table to be used to map intensities into colors. This can be either
;      a scalar, which will be used to select one of IDL's predefined colortables
;      (0 is grayscale), or a [3,256] array of color triples, or a 256-element array
;      of long integers.
;    cg: in, optional, default=0
;      If set, plotting is made with Coyote Graphics' cgpolygon.
;    graphics: in, optional, default=1
;      If set, plotting is made with IDL's Function Graphics' polygon().
;    itool: in, optional, default=0
;      If set, plotting is made with IDL's iTools' ipolygon.
;    direct: in, optional, default=0
;      If set, plotting is made with IDL's Direct Graphics' polyfill.
;    maxlength: in, optional
;      Passed on to `pp_sphericalpath`, determines the maximum length of the polygon
;      sides used for plotting, in degrees.
;    nsegments: in, optional
;      Passed on to `pp_sphericalpath`, determines the number of segments to use for
;      the polygon sides.
;    polygon: out, optional
;      If Function Graphics or iTools are being used for plotting, returns the polygon
;      object created with them.
;    x: out, optional
;      If Function Graphics or iTools are being used for plotting, arrays of x and y points
;      get created, one for each vertex of all polygons plotted. This keyword returns
;      the x coordinates of the vertices created.
;    y: out, optional
;      If Function Graphics or iTools are being used for plotting, arrays of x and y points
;      get created, one for each vertex of all polygons plotted. This keyword returns
;      the y coordinates of the vertices created.
;    connectivity: out, optional
;      If Function Graphics or iTools are being used for plotting, arrays of x and y points
;      get created, one for each vertex of all polygons plotted. This keyword returns
;      the connectivity array which specifies which vertices belong to each polygon. See IDL's help
;      on polygon() for more details.
;    fill: in, optional, default=0.
;      If Coyote Graphics' cgpolygon is being used, this keyword determines if
;      the polygons are drawn just as outlines, or should be filled (outlines and
;      fills share the same colors).
;      
;    :Examples:
;    
;      First, let's create some data to plot::
;      
;        ;create several rectangles
;        lons=dblarr(4,10)
;        for i=0,9 do lons[*,i]=[-85d0,-65d0,-55d0,-75d0]+i*25d0
;        lats=dblarr(4,10)
;        for i=0,9 do lats[*,i]=[-65d0,55d0,45d0,-75d0]
;        ;set their colors
;        colors=dindgen(10)
;      Now, plot the rectangles on a Graphics map::
;      
;        m=map('mollweide')
;        pp_drawsphericalpoly,lons,lats,colors,rgb_table=13,linestyle='none'
;      .. image:: pp_drawsphericalpoly_ex1.png
;      
;      Plot the rectangles on an imap::
;      
;        imap,map_projection='sinusoidal'
;        pp_drawsphericalpoly,lons,lats,colors,rgb_table=13,linestyle='none',/itool
;      .. image:: pp_drawsphericalpoly_ex2.png
;      
;      Plot the rectangles on a Coyote Graphics map::
;      
;        m=cgmap('robinson',/erase,/isotropic,/window)
;        cgmap_grid,map=m,/box,/addcmd
;        cgloadct,13
;        pp_drawsphericalpoly,lons,lats,bytscl(colors),/cg,/fill,map=m,/addcmd
;      .. image:: pp_drawsphericalpoly_ex3.png
;      
;      Plot the rectangles on a Direct Graphics map::
;      
;        map_set,0d0,0d0,/cylindrical,/isotropic,/grid,/label
;        pp_drawsphericalpoly,lons,lats,colors,rgb_table=13,/direct
;      .. image:: pp_drawsphericalpoly_ex4.png
;      
; :Requires:
;   If Coyote Graphics are to be used, the `Coyote Library <http://www.idlcoyote.com/documents/programs.php#COYOTE_LIBRARY_DOWNLOAD>` 
;   needs to be installed.
;   
;   Also needed are `pp_sphericalpath`, `pp_longtocolortriple`, `pp_colortripletolong`, 
;   and `tessellateshapes_pp`, from `pp_lib <http://www.ppenteado.net/idl/pp_lib.html>`).
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Aug/2015
;-
pro pp_drawsphericalpoly,lons,lats,colors,_ref_extra=ex,$
  rgb_table=rgbt,$
  cg=cg,graphics=graphics,itool=itool,direct=direct,$
  maxlength=maxlength,nsegments=nsegments,polygon=polygon,$
  x=x,y=y,connectivity=conn,fill=fill,$
  stackmap=stackm,original_image=origim,maxstack=maxstack,$
  stacklist=stacklist,stackcount=stackc,verbose=verbose,do_stack=do_stack,$
  weights=weights,stackweights=stackw,stackindex=stacki,pcount=pcount
compile_opt idl2,logical_predicate

verbose=n_elements(verbose) ? verbose : 0
do_stack=n_elements(do_stack) ? do_stack : 0

;Force the _tessellateshapes method in tessellateshapes_pp to be compiled after
;IDL's native method. 
r=routine_info()
if total(strmatch(r,'IDLITVISPOLYGON__DEFINE')) eq 0 then begin
  resolve_routine,'IDLITVISPOLYGON__DEFINE',/compile_full_file
endif
resolve_routine,'TESSELLATESHAPES_PP',/compile_full_file

;Default parameters
cg=keyword_set(cg)
itool=keyword_set(itool)
graphics=keyword_set(graphics)
direct=keyword_set(direct)
if do_stack then begin
  direct=1
  itool=0
  graphics=0
  cg=0
  dow=arg_present(stackw)
  doi=arg_present(stacki)
endif else begin
  dow=0
  doi=0
endelse

;Get the spherical polygons
paths=pp_sphericalpath(lons,lats,maxlength=maxlength,nsegments=nsegments,/open)

;Map the colors, if a map is set
if n_elements(rgbt) then begin
  if n_elements(rgbt) eq 1 then begin
    loadct,rgbt
    tvlct,irgbt,/get
    irgbt=transpose(irgbt)
  endif else begin
    sz=size(rgbt,/dimensions)
    if n_elements(sz) eq 2 then irgbt=rgbt else begin
      irgbt=pp_longtocolortriple(rgbt)
    endelse
  endelse
  ;Map the input colors into the [0,255] range
  icolors=bytscl(colors)
endif else begin
  icolors=colors  
endelse

;Call the drawing function
case 1 of
  (cg): pp_drawsphericalpoly_cg,paths,icolors,_strict_extra=ex,irgbt,fill=fill
  (itool): pp_drawsphericalpoly_itool,paths,icolors,_strict_extra=ex,irgbt;,fill=fill
  (direct): pp_drawsphericalpoly_direct,paths,icolors,_strict_extra=ex,irgbt,$
    stackmap=stackm,original_image=origim,maxstack=maxstack,$
    stacklist=stacklist,stackcount=stackc,verbose=verbose,do_stack=do_stack,weights=weights,$
    stackweights=stackw,doweight=dow,stackindex=stacki,doi=doi,pcount=pcount
  else: pp_drawsphericalpoly_itool,paths,icolors,_strict_extra=ex,irgbt,polygon=polygon,$
    x=x,y=y,connectivity=conn,/graphic
endcase

end
