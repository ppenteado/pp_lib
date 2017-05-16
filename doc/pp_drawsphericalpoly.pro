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
  weights=weights,stackweights=stackw,doweight=dow,stackindex=stacki,doi=doi,$
  pcount=pcount,e_map=e_map,map_structure=mapstr,image_mapstr=image_mapstr,$
  xsize=xsize,ysize=ysize
  compile_opt idl2,logical_predicate,hidden


if n_elements(mapstr) then begin
  lcolors=list(colors,/extract)
  ip=0
  while ip lt n_elements(paths) do begin
    p=paths[ip]
    xy=map_proj_forward(reform(p[0,*]),reform(p[1,*]),map_structure=mapstr,polygons=polygons)
    if n_elements(xy) lt 6 then begin
      ;print,ip
      paths.remove,ip
      lcolors.remove,ip
      continue
    endif
    if n_elements(polygons) ne n_elements(p[0,*])+1 then begin
      po=pp_connectivity_list(polygons)
      if n_elements(po) ne 1 then begin
        paths.remove,ip
        oc=lcolors[ip]
        lcolors.remove,ip
        ipo=ip
        foreach ppo,po do if n_elements(ppo) ge 3 then begin
          lcolors.add,oc,ip
          paths.add,xy[*,ppo],ip++
        endif
      endif else begin
        paths[ip]=xy
      endelse
    endif else paths[ip]=xy
    ip++
  endwhile
  colors=lcolors.toarray()
endif

if do_stack then begin
  oldmap=!map
  if n_elements(e_map) then begin
     p0lon=0 & p0lat=0 & mrot=0
    _e_map=pp_structextract(e_map,p0lat=p0lat,p0lon=p0lon,rot=mrot)
    olddev=!d.name
    set_plot,'z'
    origim=tvrd()
    ;device,get_decomposed=origdec
    eh={fill:1,color:cgcolor('red')}
    xsize=n_elements(xsize) ? xsize : 640
    ysize=n_elements(ysize) ? ysize : 480
    device,set_resolution=[xsize,ysize]
    map_set,p0lat,p0lon,mrot,/noborder,_strict_extra=_e_map,e_horizon=eh
  endif else begin
    eh={fill:1,color:cgcolor('red')}
    map_set,/noborder,/isotropic,/cylindrical,e_horizon=eh
  endelse
  if dow && (n_elements(weights) ne n_elements(colors)) then weights=replicate(1d0,n_elements(colors))
  
  mapim=tvrd(channel=0)
  maskrgb=tvrd()
  mask=maskrgb eq cgcolor('red')
  nmask=total(mask,/integer) 
  szm=size(mapim,/dimensions)
  stackc=lon64arr(szm)
  if do_stack then pcount=lon64arr(n_elements(colors))
  if do_stack eq 1 then begin
    maxstack=n_elements(maxstack) ? maxstack : n_elements(paths)
    stackm=dblarr([maxstack,szm])+!values.d_nan
    if dow then stackw=stackm
    if doi then stacki=lonarr([maxstack,szm])-1
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
  print,'n paths: ',n_elements(paths)
  foreach p,paths,ip do begin
    erase
    polyfill,p[0,*],p[1,*],/data,_strict_extra=ex,color=cgcolor('blue')
    if verbose && ~(ip mod verbose) then print,ip
    tmprgb=tvrd()
    tmp=mask and (tmprgb eq cgcolor('blue')) 
    w=where(tmp,wc)
    if wc gt nmask/2 then begin
      ;tmp=mask and (not tmp)
      w=where(tmp,wc)
    endif
    pcount[ip]=wc
    if wc then begin
      cip=colors[ip]
      if dow then wip=weights[ip]
      if do_stack eq 1 then begin
        foreach pt,w do if stackc[pt] lt maxstack then stackm[pt*maxstack+stackc[pt]]=cip
        if dow then foreach pt,w do if stackc[pt] lt maxstack then stackw[pt*maxstack+stackc[pt]]=wip
        if doi then foreach pt,w do if stackc[pt] lt maxstack then stacki[pt*maxstack+stackc[pt]]=ip
      endif
      if do_stack eq 2 then begin
        foreach pt,w do (stackm[pt]).add,cip
        if dow then foreach pt,w do (stackw[pt]).add,wip
        if doi then foreach pt,w do (stacki[pt]).add,ip
      endif
      stackc[w]+=1
    endif
  endforeach
  print,'done with the paths'
  if n_elements(olddev) then begin
    device,set_resolution=size(origim,/dimensions);,decomposed=origdec
    tv,origim
    set_plot,olddev
  endif
  image_mapstr=!map
  !map=oldmap
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

if (!version.release ge '8.2.3') then xy=paths.toarray(dimension=2) else begin
  nxy=0LL & foreach p,paths do nxy+=(size(p,/dimensions))[1]
  xy=dblarr(2,nxy)
  count=0LL
  foreach p,paths do begin
    xy[count]=p[*]
    count+=n_elements(p)
  endforeach
endelse
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

if keyword_set(graphic) then begin
  ;poly=polygon(x,y,connectivity=conn,vert_colors=cols,_strict_extra=ex,/data)
  ;The following lines were adapted from IDL's polygon.pro, because polygons do not have the property
  ;map_interpolate registered, so the idlitvispolygon has to be created in here.
  ;Without map_interpolate, ipolygon.pro's line 137 will cause points accross sides of the map to be connected
  add2vis=1
  iPolygon, transpose([[x],[y]]), $
    DATA=1,$;data, DEVICE=device, NORMAL=normal, RELATIVE=relative, TARGET=target, $
    OBJECT=oPolygon, VISUALIZATION=add2vis, SHADING=1, $
    ;COLOR=color, LINESTYLE=linestyle, THICK=thick, _EXTRA=ex
    map_interpolate=0,connectivity=conn,vert_colors=cols,_strict_extra=ex
    Graphic__define
    oGraphic = OBJ_NEW('Polygon', oPolygon)
    ;Done with the code from polygon.pro
    poly=oGraphic
endif else begin
  ipolygon,transpose([[x],[y]]),connectivity=conn,vert_colors=cols,_strict_extra=ex,$
    /data,/visualization,object=poly,map_interpolate=0
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
;    ilons: in, required
;      An array of longitudes for the vertices which are to be connected by a path made of great circles.
;      Must be in degrees. Multiple polygons are supported in two different ways: 1) If all N polygons have 
;      the same number of vertices (M), lons can be given as a [M,N] array. 2) For arbitrary numbers of vertices,
;      lons is given as a list, where each list element is an array of vertices for one polygon.
;    ilats: in, required
;      An array of latitudes for the vertices which are to be connected by a path made of great circles.
;      Must be in degrees. Multiple polygons are supported in two different ways: 1) If all N polygons have 
;      the same number of vertices (M), lats can be given as a [M,N] array. 2) For arbitrary numbers of vertices,
;      lons is given as a list, where each list element is an array of vertices for one polygon.
;    icolors: in, required
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
;    do_stack: in, optional, default=0
;      If set to 1, instead of drawing the polygons on the graphics device, a z-buffer will be used
;      to draw polygons in an invisible direct graphics window, which are used to generate a map of
;      where each polygon falls (`stackmap`). This will probably be more clearly explained by the
;      examples below. Note that setting do_stack to a value higher than 1 is not the same as setting
;      it to 1: other values turn on alternative stack algorithms, which at this time are still
;      experimental and thus not yet documented.
;    stackmap: out, optional
;      if `do_stack` is turned on, this argument returns the stacked map generated. If the algorithm
;      selected is `do_stack`=1, this will be an array of dimensions [`maxstack`,`xsize`,`ysize`], where
;      [`xsize`,`ysize`] are the dimensions of the map created, and `maxstack` specifies the maximum number
;      layers the map stack can have. At a given location [x,y] in the map, the values stackmap[*,x,y]
;      are all the values present in `icolors` that fell onto that location on the map. The argument
;      `stackcount` is often useful, as it records how many layers were stacked at each location in the
;      map. This will probably be more clearly explained by the examples below.
;    stackcount: out, optional
;      if `do_stack` is turned on, this argument returns the count of stacked layers in the `stackmap`
;      generated. At a given location [x,y] in the map, stackcount[x,y] is the number of polygons that
;      fell on that map location (the number of layers in `stackmap` at that location). This will probably
;      be more clearly explained by the examples below.
;    maxstack: in, optional
;      Specifies the first dimension of the `stackmap` array, which is the maximum number of layers being
;      tracked falling on each location on the map. If not set, it defaults to the number of polygons provided
;      in `ilons`, `ilats`, `icolors`. If there are many polygons, it is probably wise to specify a smaller
;      value for maxstack, to avoid using too much memory. Use `stackcount` to check that at no location in the
;      map there were polygons missed because that location had more overlapping layers than could fit into
;      `stackmap`: if `stackcount` is everywhere smaller than or equal to `maxstack`, no polygons were lost in
;      `stackmap`.
;    stackindex: out, optional
;      If `do_stack` is turned on, this argument returns the an array similar to `stackmap`, but its values
;      are the indices to of the polygons that fall on each location, instead of being the intensities (`icolors`).
;    xsize: in, optional, default=640
;      If `do_stack` is set to 1, this specifies the width of the map to generate (see `stackmap`).  
;    ysize: in, optional, default=480
;      If `do_stack` is set to 1, this specifies the height of the map to generate (see `stackmap`).
;    e_map: in, optional
;      If `do_stack` is set to 1, set this argument to a structure containing any parameters to be passed
;      to map_set, which is used to define the map projection used to make `stackmap`. This will probably
;      be more clearly explained by the examples below.
;    image_mapstr: out, optional
;      If `do_stack` is set to 1, this argument will return the map structure created by map_set, which is
;      used to define the map projection used to make `stackmap`.
;    pcount: out, optional
;      If `do_stack` is turned on, this argument returns the an array with the number of map pixels covered
;      by each polygon.
;    weights: in, optional
;    stackweights: out, optional
;    verbose: in, optional
;    
;      
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
;      Now, let's make some data that spans the tricky region around the pole::
;
;        ;make up the coordinates for the coordinates of 3 rectangular fields of view near the north pole:
;        lons=[[20d0,240d0,260d0,0d0],[80d0,180d0,200d0,60d0],[50d0,210d0,230d0,30d0]]
;        lats=[[62d0,60d0,60d0,62d0],[62d0,60d0,60d0,62d0],[65d0,67d0,67d0,65d0]]
;        ;make up some pixel values to determine the color used to fill the 3 rectangles
;        pixvals=dindgen(3)
;
;      Create an imap to plot these polygons::
;
;        imap,map_proj='orthographic',center_lat=60d0
;
;      Draw the polygons::
;
;        pp_drawsphericalpoly,lons,lats,pixvals,rgb_table=13,/itool
;      .. image:: pp_drawsphericalpoly_ex5.png
;      
;      Now, an example with overlapping polygons, making an overlap map and taking the
;      mean of the values on overlap::
;        
;        lats=[[62d0,60d0,60d0,62d0],[62d0,60d0,60d0,62d0],[70d0,72d0,72d0,70d0],[80d0,87d0,87d0,80d0]]
;        lons=[[40d0,220d0,250d0,0d0],[80d0,180d0,200d0,60d0],[50d0,200d0,240d0,20d0],[70d0,70d0,95d0,95d0]]
;        pixvals=[0d0,-1d0,-3d0,2d0]
;        
;      First, take a look at the overlayed polygons::
;        
;        m=map('orthographic',center_lat=30d0)
;      .. image:: pp_drawsphericalpoly_ex6.png
;        
;      Now, make the stack map::
;      
;        pp_drawsphericalpoly,lons,lats,pixvals,rgb_table=13
;        limit=[-90,-180,90,180]
;        e_map={cylindrical:1,noborder:1,xmargin:0,ymargin:0,limit:limit,isotropic:1}
;        pp_drawsphericalpoly,lons,lats,pixvals,do_stack=1,stackc=stackc,e_map=e_map,xsize=3000,ysize=1500,maxstack=4,stackm=stackm
;        
;      Look at the coverage map - an array where each value is the number of polygons that fell onto that place on the map::
;      
;        im0=image(stackc,map_projection='equirectangular',grid_units=2,image_location=limit[[1,0]],image_dimensions=[limit[3]-limit[1],limit[2]-limit[0]],dimensions=[900,500],color='cyan',aspect_ratio=5.,limit=[30,-180,90,180])
;        im1=image(stackc,map_projection='orthographic',center_lat=30,grid_units=2,image_location=limit[[1,0]],image_dimensions=[limit[3]-limit[1],limit[2]-limit[0]],,dimensions=[900,500],color='cyan',rgb_table=13)
;      .. image:: pp_drawsphericalpoly_ex7.png
;      .. image:: pp_drawsphericalpoly_ex8.png
;        
;      Make an average image from stackm, by taking then mean over the stack (first) dimension::
;      
;        stackmean=mean(stackm,dimension=1,/nan)
;        im2=image(stackmean,map_projection='orthographic',center_lat=90,grid_units=2,image_location=limit[[1,0]],image_dimensions=[limit[3]-limit[1],limit[2]-limit[0]],dimensions=[900,500],color='cyan',rgb_table=13)
;      .. image:: pp_drawsphericalpoly_ex9.png
;        
;        
;
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
pro pp_drawsphericalpoly,ilons,ilats,icolors,_ref_extra=ex,$
  rgb_table=rgbt,$
  cg=cg,graphics=graphics,itool=itool,direct=direct,$
  maxlength=maxlength,nsegments=nsegments,polygon=polygon,$
  x=x,y=y,connectivity=conn,fill=fill,$
  stackmap=stackm,original_image=origim,maxstack=maxstack,$
  stacklist=stacklist,stackcount=stackc,verbose=verbose,do_stack=do_stack,$
  weights=weights,stackweights=stackw,stackindex=stacki,pcount=pcount,no_fix_lon=no_fix_lon,$
  map_structure=mapstr,image_mapstr=image_mapstr,xsize=xsize,ysize=ysize,e_map=e_map,$
  bmin=bmin,bmax=bmax,bnan=bnan,btop=btop
compile_opt idl2,logical_predicate



if (size(ilons,/n_dimensions) eq 1) and ~isa(ilons,'list') then begin
  lons=[[ilons],[ilons]]
  lats=[[ilats],[ilats]]
endif else begin
  lons=ilons
  lats=ilats
endelse

nic=n_elements(icolors)
if size(lons,/n_dimensions) eq 2 then begin
  np=(size(lons,/dimensions))[1]
endif else np=n_elements(lons)

if nic eq 0 then begin
  colors=dindgen(np)
  rgbt=0
endif else begin
  colors=icolors
endelse

verbose=n_elements(verbose) ? verbose : 0
do_stack=n_elements(do_stack) ? do_stack : 0
no_fix_lon=keyword_set(no_fix_lon)

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
paths=pp_sphericalpath(lons,lats,maxlength=maxlength,nsegments=nsegments,no_fix_lon=no_fix_lon);,/open)
if ~no_fix_lon then foreach p,paths,ip do begin
  p[0,*]=(p[0,*]+360d0) mod 360
  w=where(p[0,*] gt 180d0,wc)
  if wc then begin
    p[0,w]-=360d0
    paths[ip]=p
  endif
;  if n_elements(maxlength) then begin 
;
;    pdiffs=p[*,1:-1]-p[*,0:-2]
;    pdiffs=sqrt(total(pdiffs^2,1))
;    w=where(pdiffs gt maxlength*10d0,wc)
;;  if wc then begin
;;    print,'found ',wc,' points to split polygon'
;;    p0=p[*,0:w[0]]
;;    p1=p[*,w[0]+1:-1]
;;    paths[ip]=p0
;;    paths.add,p1,ip+1
;;    sz=size(colors,/n_dim)
;;    colors=sz eq 1 ? [colors[0:ip],colors[ip:-1]] : [[colors[*,0:ip]],[colors[*,ip:-1]]]  
;;  endif
;  ;s=sort(pdiffs)
;  ;pds=pdiffs[s]
;  ;w=where((pds[1:-1] gt 10d0*pds[0:-2]) and (pds[0:-2] gt 0d0),wc)
;  endif
endforeach

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
  iicolors=isa(colors,'byte') ? colors : bytscl(colors,min=bmin,max=bmax,nan=bnan,top=btop)
endif else begin
  iicolors=colors  
endelse

;Call the drawing function
case 1 of
  (cg): pp_drawsphericalpoly_cg,paths,iicolors,_strict_extra=ex,irgbt,fill=fill
  (itool): pp_drawsphericalpoly_itool,paths,iicolors,_strict_extra=ex,irgbt;,fill=fill
  (direct): pp_drawsphericalpoly_direct,paths,iicolors,_strict_extra=ex,irgbt,$
    stackmap=stackm,original_image=origim,maxstack=maxstack,$
    stacklist=stacklist,stackcount=stackc,verbose=verbose,do_stack=do_stack,weights=weights,$
    stackweights=stackw,doweight=dow,stackindex=stacki,doi=doi,pcount=pcount,map_structure=mapstr,$
    image_mapstr=image_mapstr,xsize=xsize,ysize=ysize,e_map=e_map
  else: pp_drawsphericalpoly_itool,paths,iicolors,_strict_extra=ex,irgbt,polygon=polygon,$
    x=x,y=y,connectivity=conn,/graphic
endcase

end
