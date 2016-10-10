; docformat = 'rst rst'
;
;+
; :Author: Paulo Penteado (http://www.ppenteado.net), Nov/2014
;-
;+
; :Description:
;    Given a set of lon/lat points on a sphere, this function calculates vertices of
;    a path connecting the input vertices, placed along great circles connecting the
;    input vertices. The path is a polygon approximation of the curved path formed by
;    the great circles.
;    
; :Returns:
;    A 2D array [2,N] with a set of longitudes/latitudes for the N vertices of the path, for each path.
;    The number of vertices along each edge is determined by `maxlength`
;    or `nsegments`. If `open` is set, the last point is not connected to the first point.
;    If multiple polygons were given as input, the ouput is a list, where each list element is a [2,N] array
;    with lon/lat for each polygon (N needs not to be the same for all polygons).
;
; :Params:
;    lons: in, required
;      An array of longitudes for the vertices which are to be connected by a path made of great circles.
;      Must be in degrees, unless `radians` is set.
;      Multiple polygons are supported in two different ways: 1) If all N polygons have the same number of
;      vertices (M), lons can be given as a [M,N] array. 2) For arbitrary numbers of vertices, lons is given
;      as a list, where each list element is an array of vertices for one polygon. With multiple polygons, the
;      ouput is a list, where each list element has latitudes and longitudes for one polygon.
;    lats: in, required
;      An array of latitudes for the vertices which are to be connected by a path made of great circles.
;      Must be in degrees, unless `radians` is set.
;      Multiple polygons are supported in two different ways: 1) If all N polygons have the same number of
;      vertices (M), lats can be given as a [M,N] array. 2) For arbitrary numbers of vertices, lats is given
;      as a list, where each list element is an array of vertices for one polygon. With multiple polygons, the
;      ouput is a list, where each list element has latitudes and longitudes for one polygon.
;
; :Keywords:
;    maxlength: in, optional, default=0.1
;      If set, specifies the maximum angular length of each segment of the polygon.
;    nsegments: in, optional
;      If set, specifies a fixed number of vertices to use in for the line connecting each pair
;      of input points. If both `maxlength` and `nsegments` are set, `nsegments` takes precendence.
;    open: in, optional, default=0
;      If set, the output path does not connect the last input point to the first input point.
;    _ref_extra: in, out, optional
;      Anything else is passed along to map_2points, which is used to create the paths connecting the
;      pairs of input points. One commonly used option would be the radians keyword, to use radians,
;      instead of degrees, for `lon` and `lat`. See the IDL help for map_2points' options.
;
; :Examples:
;   Create a rectangle in lon/lat::
;     lons=[30d0,120d0,120d0,30d0]
;     lats=[70d0,70d0,10d0,10d0]
;   Create a closed path connecting these points::
;     path=pp_sphericalpath(lons,lats)
;   Create a base map to plot the results (from IDL's imap example)::
;     file = FILEPATH('avhrr.png', SUBDIRECTORY=['examples','data'])
;     data = READ_PNG(file, r, g, b)
;     IMAP, data, LIMIT=[-90,-180,90,180], $
;     MAP_PROJECTION='Mollweide', RGB_TABLE=[[r],[g],[b]], $
;       IMAGE_TRANSPARENCY=50, GRID_UNITS=2, $
;       IMAGE_LOCATION=[-180,-90], IMAGE_DIMENSIONS=[360,180]
;   Plot the input points connected by straight lines::
;     iplot,lons[[0,1,2,3,0]],lats[[0,1,2,3,0]],color='red',/overplot
;   Plot the path created by pp_sphericalpath::
;     iplot,/overplot,color='green',path,thick=3.
;   The result should look like
;     .. image:: pp_sphericalpath_example_1.png
;     
;   The green path, produced with the output of `pp_sphericalpath` looks
;   like a curve, made by the 4 great circles connecting these points. In reality,
;   the path is made of 2367 points, separated by 0.1 degrees.
;   This can be verified by making the same plot on a gnomonic projection, which has
;   the property that great circles are straight lines::
;     imap,data,limit=[0,20,80,130],map_projection='mollweide',rgb_table=[[r],[g],[b]],$
;     grid_units=2,image_transparency=50,image_location=[-180,-90],$
;     image_dimensions=[360,180],center_latitude=50,center_longitude=75
;     iplot,/overplot,color='green',path,thick=3.
;     
;   The result should look like
;     .. image:: pp_sphericalpath_example_2.png
;     
;   Where the green path is rendered as straight lines, since great circles map
;   into straight lines in a gnomonic projection.
;
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Nov/2014
;-
function pp_sphericalpath,lons,lats,maxlength=maxlength,nsegments=nsegments,$
  open=open,_ref_extra=ex,no_fix_lon=no_fix_lon
compile_opt idl2,logical_predicate

;Defaults
no_fix_lon=keyword_set(no_fix_lon)
open=keyword_set(open)
slats=size(lats,/dimensions)
slons=size(lons,/dimensions)
if ~array_equal(slats,slons) then message,'lons must have same dimensions as lats'

listres=1B
if isa(lons,'list') && isa (lats,'list') then begin
  npolys=n_elements(lons)  
endif else begin
  npo=slats[0]
  npolys=n_elements(lats)/npo
  if npolys eq 1 then listres=0B
endelse
lres=list()

for ipoly=0,npolys-1 do begin

if isa(lons,'list') && isa (lats,'list') then begin
    olons=lons[ipoly] & olats=lats[ipoly]
  endif else begin
    olons=lons[ipoly*npo:(ipoly+1)*npo-1]
    olats=lats[ipoly*npo:(ipoly+1)*npo-1]
  endelse

  npoints=n_elements(olons)
  ;Check arguments
  if npoints lt 2 then message,'lons must have at least two elements'
  if n_elements(olats) ne npoints then message,'lats must have the same number of elements as lons'
  
  ;Make set of vertices
  np=open ? npoints : npoints+1
  ilons=open ? olons : [olons,olons[0]]
  ilats=open ? olats : [olats,olats[0]]
  
  ;Create output
  ret=ptrarr(np-1)
  count=lon64arr(np)
  maxlength=n_elements(maxlength) ? maxlength : 0.1d0
    for i=0LL,np-2LL do begin    
      if n_elements(nsegments) then begin
        if nsegments ge 1 then tmp=map_2points(ilons[i],ilats[i],ilons[i+1],ilats[i+1],npath=nsegments+1,_strict_extra=ex) else begin
          tmp=map_2points(ilons[i],ilats[i],ilons[i+1],ilats[i+1],npath=2,_strict_extra=ex)
          countt=n_elements(tmp)/2LL
          tmp=i eq 0 ? tmp[*,[0,countt-1]] : tmp[*,[countt-1]]
        endelse
      endif else tmp=map_2points(ilons[i],ilats[i],ilons[i+1],ilats[i+1],dpath=maxlength,_strict_extra=ex)
      if ~no_fix_lon then begin
        tmp[0,*]=(tmp[0,*]+360d0) mod 360
        w=where(tmp[0,*] gt 180d0,wc)
        if wc then tmp[0,w]-=360d0
      endif
      count[i+1]=n_elements(tmp)/2LL
      ret[i]=ptr_new(tmp)
    endfor
  res=dblarr(2,total(count,/integer))
  count=total(count,/cumulative,/integer)
  for itmp=0LL,np-2LL do begin
    res[count[itmp]*2LL]=(*(ret[itmp]))[*]
  endfor
  if listres then lres.add,res
endfor
if listres then res=lres
return,res
end
