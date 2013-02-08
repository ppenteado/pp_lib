; docformat = 'rst'
;+
; :Author: Paulo Penteado (http://www,ppenteado.net),
;-
;+
; :Description:
;    Makes x and y coordinates to make a histogram plot that looks the usual way,
;    with lines connecting the histogram counts to y=0 at each side of each bin. 
;
; :Params:
;    locations : in, required
;      The coordinate of the start of each bin (like what is returned by IDL's histogram().
;      See example below. 
;    histogram_counts: in, required
;      The count for each bin. See example below.
;
; :Keywords:
;    x : out, required
;      The x coordinate to use in the plot. See example below.
;    y : out, required
;      The y coordinate to use in the plot. See example below.
;      
; :Examples:
;    Create an array with 10000 Gaussian-distributed doubles and plot a histogram::
;    
;      values=randomu(seed,10000,/normal,/double)
;      hist=histogram(values,min=-3d0,max=3d0,binsize=0.1,locations=loc)
;      pp_histogramlines,loc,hist,x=x,y=y
;      iplot,x,y,color='red',name='Gaussian',insert_legend=[0.3,0.5],thick=2.
;
;    Now add a histogram for 10000 Gamma(1)-distributed doubles::
;   
;      values=randomu(seed,10000,gamma=1d0,/double)
;      hist=histogram(values,min=-3d0,max=3d0,binsize=0.1,locations=loc)
;      pp_histogramlines,loc,hist,x=x,y=y
;      iplot,x,y,color='blue',name='Gamma(1)',/insert_legend,/over,thick=2.
;      
;    Which would result in
;    
;    .. image:: pp_histogramlines.png
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Jan/2013
;-
pro pp_histogramlines,locations,histogram_counts,x=x,y=y
compile_opt idl2,logical_predicate
nlocs=n_elements(locations)
binsize=(locations[-1]-locations[0])/(nlocs-1d0)
x=dblarr(4*nlocs)
y=x
x[0:*:4]=locations
x[1:*:4]=locations
x[2:*:4]=locations+binsize
x[3:*:4]=locations+binsize
y[1:*:4]=histogram_counts
y[2:*:4]=histogram_counts
end
