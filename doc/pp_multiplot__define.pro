; docformat = 'rst rst'
;+
;
;    pp_multiplot objects hold a grid of plots (from the plot function, of IDL
;    Graphics, not the plot procedure from direct graphics) with no empty space between lines
;    and columns, to use a single vertical/horizontal axis for each line/column
;    of plots. This was inspired by the functionality provided by `multiplot` from IDLAstro
;    (`http://idlastro.gsfc.nasa.gov/ftp/pro/plot/multiplot.pro <http://idlastro.gsfc.nasa.gov/ftp/pro/plot/multiplot.pro>`)
;    for direct graphics.
;    
;    But contrary to multiplot, this system works for Graphics (instead of direct
;    graphics), allows for lines/columns of different heights/widths,
;    provides global margins and axes titles, and provides a synchronization
;    mechanism to keep the plots in the same line/column (or all) with the same
;    axes ranges and properties. Also, like in `pp_plot`, there is the option
;    of suppressing the first/last tick marks of the axes, to avoid their overlap
;    with the neighboring plots (enabled by default, depending on the plot's
;    location on the grid).
;    
;    A pp_multiplot object makes a window to hold the grid, and will contain all the
;    plots in the grid (made with the plot method provided here). The individual
;    plot objects can be retrieved (for instance, to set/get their properties),
;    and axes properties can be set (and synchronized) for entire lines/columns
;    (or all plots in the grid) at a time. The init/get/set methods pass any
;    extra properties to the window that contains the plot - the window class is
;    used here, not inherited.
;
;    See the documentation on the public methods
;    `pp_multiplot::init`, `pp_multiplot::plot`, `pp_multiplot::image`, 
;    `pp_multiplot::contour`, `pp_multiplot::getproperty`,
;    `pp_multiplot::setproperty`, `pp_multiplot::sync_axes`, ,`pp_multiplot::close`,
;    `pp_multiplot::save`, and `pp_multiplot::getposition`, for details on their use.
;
; :Examples:
;    Make a simple 2x2 grid with equal sizes::
;
;      m=pp_multiplot(multi_layout=[2,2],global_xtitle='Test X axis title',global_ytitle='Test Y axis title')
;      
;    Now populate the grid with plots using plot()'s test data::
;    
;      ;Since multi_index was not provided, this will occupy the first free location in the grid:
;      p0=m.plot(/test,color='red',thick=2.)
;      p1=m.plot(/test,color='blue',linestyle=1) ;Second location, since multi_index was omitted
;      p2=m.plot(/test,multi_index=2,symbol='circle') ;Third location, explicitly set with multi_index
;      
;    The result should look like
;    
;      .. image:: pp_multiplot_ex1.png
;    
;    Now, the x and y ranges on the second plot will be changed::
;    
;      p1.xrange=[50,100]
;      p1.yrange=[0,1]
;      m.sync_axes,1 ;Update the axes on the plot, taking the second one as reference.
;      
;    Now the plot will have a different x axis on the second column, and a different
;    y axis on the first line: .. image:: pp_multiplot_ex2.png
;    
;    These new ranges will automatically be used when a new plot is put on that
;    line/column::
;    
;      p3=m.plot(/test,multi_index=3) ;Fourth location, explicitly set with multi_index
;      
;    Now, save the output to a vector pdf, and close the window in the usual Graphics way::
;      
;      m.save,'pp_multiplot_ex3.pdf'
;      m.close
;      
;    The file would look like
;    
;      .. image:: pp_multiplot_ex3.png
;    
;    A more complicated example, with a 2x3 grid with variable widths and heights::
;    
;      m=pp_multiplot(multi_layout=[2,3],lineheights=[100,200,100],columnwidths=[100,200],/absolute_dims)
;      p0=m.plot(/test,color='red')
;      p1=m.plot(/test,color='blue',xrange=[50,100])
;      p2=m.plot(/test,color='green',ycolor='magenta',propagate=2) ;Make ycolor extend only to the second line
;      p3=m.plot(/test,linestyle='dotted')
;      p4=m.plot(/test,symbol='square')
;      p5=m.plot(/test,symbol='circle')
;      
;    Which would look like
;    
;     .. image:: pp_multiplot_ex4.png
;      
;    Now to retrieve some y properties of each line::
;    
;      print,m.yranges
;      ;     -0.82532734      0.93990618
;      ;     -0.82532734      0.93990618
;      ;     -0.82532734      0.93990618
;      print,m.ycolor
;      ;!NULL
;      ;magenta
;      ;!NULL
;      
;    Now to make the second column have red x axes::
;    
;      xcolor=m.xcolor
;      print,xcolor
;      ;!NULL
;      ;!NULL
;      xcolor[1]='red'
;      m.xcolor=xcolor
;      
;    Now to make all plots have larger y tick marks::
;    
;      m.yticklen=0.2
;      
;    The final result would be 
;    
;     .. image:: pp_multiplot_ex5.png
;     
;    Save this window to a low resolution bitmap (same used in the documentation)
;    and close it::
;    
;      m.save,'pp_multiplot_ex5.png',resolution=100
;      m.close
;      
;    A simple example, with plots made with a set of default properties, so that
;    it is not necessary to repeat them for each plot::
;    
;      props={color:'red',symbol:'square',sym_filled:1}
;      m=pp_multiplot(multi_layout=[1,3],graphproperties=props)
;      p1=m.plot(/test)
;      p2=m.plot(/test)
;      p3=m.plot(/test,color='blue')
;      
;    The result would be
;
;     .. image:: pp_multiplot_ex6.png
;     
;    Now, we will change the yrange in one of the panels. This will cause the
;    axis tick labels to get recalculated, resulting in overlapping labels. This
;    would happen even if the new yrange was identical to the current range, beacuse
;    setting the range triggers a recalculation of the ticks::
;    
;       p2.yrange=[-1,2]
;       
;    Which looks like
;    
;    .. image:: pp_multiplot_ex7.png
;    
;    The fix to that is a call to the `updateranges` method::
;    
;      m.updateranges
;      
;    Which will make the plot look like
;    
;    .. image:: pp_multiplot_ex8.png
;       
;
; :Version: 20101027
; 
; :Requires: IDL 8.0
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Sep/2010
;-

;+
; :Description:
;    Creates an object to hold a grid of plots (from the plot function, of IDL
;    Graphics, not the plot procedure from direct graphics) with no empty space between lines
;    and columns, to use a single vertical/horizontal axis for each line/column
;    of plots. See the description of the class `pp_multiplot` for details
;    and examples.
;
; :Keywords:
; 
;    multi_layout: in, required
;      The layout of the grid to create, as a 2-element array [ncolumns, nlines],
;      with the number of columns and the number of lines for the grid.
;    title: in, optional
;      The title for the entire plot (passed on to the window object).
;    global_xtitle: in, optional
;      The common title for all the x axes, so that a same title does not have to
;      be shown repeatedly below every column.
;    global_ytitle: in, optional
;      The common title for all the y axes, so that a same title does not have to
;      be shown repeatedly on the left of every line.
;    global_margin: in, optional
;      The margins, in normalized units (range 0 to 1) to be given around the
;      grid. Provided as a 4-element array, for [left, bottom, right, top] margins.
;      Defaults to [0.125d0,0.15d0,0.005d0,0.1d0].
;    columnwidths: in, optional
;      If set to an array with the same number of elements as the number of grid
;      columns, specifies the width to use for each plot column. If not provided,
;      equal widths are used. The units are arbitrary, as only their relative values
;      is used, unless `absolute_dims` is set, in which case the units are the same
;      as window()'s dimensions. 
;    lineheights: in, optional
;      If set to an array with the same number of elements as the number of grid
;      lines, specifies the height to use for each plot column. If not provided,
;      equal heights are used. The units are arbitrary, as only their relative values
;      is used, unless `absolute_dims` is set, in which case the units are the same
;      as window()'s dimensions.
;    absolute_dims: in, optional, default=0
;      If set, causes `columnwidths` and `lineheights` to be interpreted as absolute,
;      rather than relative values. In that case, their units are the same as those
;      used by window()'s dimensions. 
;    _REF_EXTRA: in, out, optional
;      Any extra properties are just passed to window() (the init method of the
;      window class). The most common to be used is probably going to be dimensions.
;      See the help on window() for more information.
;    graphproperties: in, optional
;      Use this keyword to provide a set of graphic keywords to be passed to all
;      individual graphs by default. It should be in the form of a structure, with
;      each field containing the value for the keyword of corresponding name.
;      If an individual graph's creation specifies a value for a keyword given in
;      graphproperties, it will take precendece over the graphproperties value.
;      See examples above. 
;      
;-
function pp_multiplot::init,_REF_EXTRA=ex,$
 multi_layout=mlayout,title=gtitle,global_xtitle=gxtitle,global_ytitle=gytitle,$
 global_margin=gmargin,columnwidths=cwidths,lineheights=lheights,absolute_dims=absolute,$
 xgap=xgap,ygap=ygap,xsupressdivision=xsupressdivision,ysupressdivision=ysupressdivision,$
 graphproperties=graphproperties,xtickratio=xtickratio,ytickratio=ytickratio
compile_opt idl2, logical_predicate

if (n_elements(mlayout) ne 2) then begin
  print,'multi_layout must be a 2-element integer array'
  return,0
endif

;Defaults
;Are the widths and heigths provided to be interpreted as absolute or relative dimensions? 
absolute=(n_elements(absolute) eq 1) ? absolute : 0

;Grid shape
self.mlayout=round(mlayout)
ncolumns=self.mlayout[0]
nlines=self.mlayout[1]
self.ncolumns=ncolumns
self.nlines=nlines

;Initialize lists
self.oplots=list(length=self.mlayout[0]*self.mlayout[1])
;Set equal widths and heights by default
if (n_elements(cwidths) eq ncolumns) then self.cwidths=list(cwidths/total(cwidths),/extract) $
 else self.cwidths=list(replicate(1d0/ncolumns,ncolumns),/extract)
if (n_elements(lheights) eq nlines) then self.lheights=list(lheights/total(lheights),/extract) $
 else self.lheights=list(replicate(1d0/nlines,nlines),/extract)
self.xranges=list(length=ncolumns)
self.yranges=list(length=nlines)
keynames=['COLOR','GRIDSTYLE','LOG','MAJOR','MINOR','SUBTICKLEN','TEXT_COLOR',$
 'TEXT_POS','THICK','TICKDIR','TICKFONT_NAME','TICKFONT_SIZE','TICKFONT_STYLE',$
 'TICKFORMAT','TICKINTERVAL','TICKLAYOUT','TICKLEN','TICKNAME','TICKUNITS','TICKVALUES',$
 'TITLE','TRANSPARENCY']
self.xproperties=list()
for i=0,ncolumns-1 do self.xproperties.add,hash('X'+keynames)
self.yproperties=list()
for i=0,nlines-1 do self.yproperties.add,hash('Y'+keynames)
self.xendticks=list(length=nlines*ncolumns)
self.yendticks=list(length=nlines*ncolumns)

;Process optional parameters
;Default margins
self.global_margin=[0.125d0,0.15d0,0.005d0,0.1d0]
if (n_elements(gmargin) eq 4) then self.global_margin=gmargin
if (n_elements(xranges) ne 0) then self.setproperty,xranges=xranges
if (n_elements(yranges) ne 0) then self.setproperty,yranges=yranges
if (n_elements(ygap) ne 0) then self.ygap=ygap
if (n_elements(xgap) ne 0) then self.xgap=xgap

;Create the window to contain the graphics
if absolute then begin
  if (n_elements(dimensions) ne 2) then begin
    ;Compute window dimensions to give the proper margins in addition to the line/column dimensions 
    totalw=total(cwidths)/(1d0-total((self.global_margin)[[0,2]]))
    totalh=total(lheights)/(1d0-total((self.global_margin)[[1,3]]))
    dimensions=[totalw,totalh]
  ;Or take the absolute dimensions given for the window and ignore relative margins
  endif else self.global_margin=[0d0,0d0,0d0,0d0]
  self.owindow=window(_extra=ex,title=gtitle,dimensions=dimensions)
endif else self.owindow=window(_extra=ex,title=gtitle)

;Parameters that depend on the window's existence
if (n_elements(gxtitle) eq 1) then self.setproperty,global_xtitle=gxtitle
if (n_elements(gytitle) eq 1) then self.setproperty,global_ytitle=gytitle
if (n_elements(gtitle) ne 0) then self.setproperty,title=gtitle
if (n_elements(xsupressdivision) ne 0) then self.xsupressdivision=xsupressdivision
if (n_elements(ysupressdivision) ne 0) then self.ysupressdivision=ysupressdivision
self.xtickratio=n_elements(xtickratio) ? xtickratio : 0.2d0
self.ytickratio=n_elements(ytickratio) ? ytickratio : 0.2d0

self.graphproperties=n_elements(graphproperties) ? hash(graphproperties) : hash()

return,isa(self.owindow,'graphicswin') || isa(self.owindow,'graphicsbuffer')
end


;+
; :Description:
;    A wrapper for plot(), which creates the plot in the proper place and with
;    the right properties (particularly x/y ranges) in the multiplot grid, adding
;    the object to the list of plots contained by the pp_multiplot object.
;    
; :Returns:
;    The plot object created in the multiplot grid. This is a regular object of
;    IDL's plot class, and can be manipulated in the usual way. The method
;    `pp_multiplot::sync_axes` can be used to synchronize axes properties across
;    a line/column or all plots, after one plot has been changed (programmatically
;    or interactively). 
;
; :Params:
;    arg1: in, optional
;      The first argument to be passed on to plot(). See the help on plot()
;      for details.
;    arg2: in, optional
;      The second argument to be passed on to plot(). See the help on plot()
;      for details.
;    arg3: in, optional
;      The third argument to be passed on to plot(). See the help on plot()
;      for details.
;
; :Keywords:
;    TEST: in, optional, default=0
;      Passed on to plot(), to make plot() used the test data instead.
;    multi_index: in, optional
;      The index of the position to place the plot in the grid. Count starts at
;      0, on the top left, proceeding left-to-right, then top-to-bottom, up to
;      nlines*ncolumns-1.
;    xrange: in, optional
;      A 2-element array with the minimum and maximum to use for the x axis. If
;      not provided, the xrange set for the current column is used, or, if no range
;      was set for the column, the plot is created with the default range
;      (determined by plot()), and that range is subsequently set for the column
;      The other plots in the column have their ranges updated, if necessary. 
;    yrange: in, optional
;      A 2-element array with the minimum and maximum to use for the y axis. If
;      not provided, the xrange set for the current line is used, or, if no range
;      was set for the line, the plot is created with the default range
;      (determined by plot()), and that range is subsequently set for the line
;      The other plots in the line have their ranges updated, if necessary.
;    propagate: in, optional, default=1
;      Determines the mode of propagating the axes properties (range and endticks
;      not included) of this plot. If 0, no propagation of properties is done.
;      If 1, properties are propagated to all plots in the grid.
;      If 2, properties are propagated to all plots in the same line/column.
;    xendticks: in, optional
;      The mode set for suppressing the first/last x tick labels of the plot: 0
;      means that both the first and last labels are suppressed, 1 means that only
;      the last label is suppressed, 2 means that only the first label is suppressed,
;      and 3 means that neither is suppressed. The default for each plot depends
;      on its location on the multiplot grid (its mindex): Plots have their last
;      label suppressed (xendticks=1), except for those that fall on the rightmost
;      column (xendticks=3).
;    yendticks: in, optional
;      The mode set for suppressing the first/last y tick labels of the plot: 0
;      means that both the first and last labels are suppressed, 1 means that only
;      the last label is suppressed, 2 means that only the first label is suppressed,
;      and 3 means that neither is suppressed. The default for each plot depends
;      on its location on the multiplot grid (its mindex): Plots have their last
;      label suppressed (yendticks=1), except for those that fall on the top
;      line (yendticks=3).
;    _EXTRA: in, optional
;      Any keywords not handled by this method are passed on to plot(). See the
;      help on plot() for details.
;      
;      The following properties are axes properties, that can be made common across
;      columns (x) or lines (y) with the keyword `propagate`. See the help on plot
;      objects for details:
;      
;      [XY]COLOR, [XY]GRIDSTYLE, [XY]LOG, [XY]MAJOR, [XY]MINOR, [XY]SUBTICKLEN, [XY]TEXT_COLOR, 
;      [XY]TEXT_POS, [XY]THICK, [XY]TICKDIR, [XY]TICKFONT_NAME, [XY]TICKFONT_SIZE, [XY]TICKFONT_STYLE, 
;      [XY]TICKFORMAT, [XY]TICKINTERVAL, [XY]TICKLAYOUT, [XY]TICKLEN, [XY]TICKNAME, [XY]TICKUNITS, 
;      [XY]TICKVALUES, [XY]TITLE, [XY]TRANSPARENCY
;
;-
function pp_multiplot::plot,arg1, arg2, arg3, TEST=test, _EXTRA=ex,$
 multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
 xendticks=xendticks,yendticks=yendticks
compile_opt idl2, logical_predicate
graphic_to_do='plot'
case n_params() of
  0: ret=self.do_graphic(graphic_to_do=graphic_to_do, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks)
  1: ret=self.do_graphic(graphic_to_do=graphic_to_do,arg1, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks)
  2: ret=self.do_graphic(graphic_to_do=graphic_to_do,arg1, arg2, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks)
  3: ret=self.do_graphic(graphic_to_do=graphic_to_do,arg1, arg2, arg3, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks)
endcase
return,ret
end


;+
; :Description:
;    A wrapper for contour(), which creates the image in the proper place and with
;    the right properties (particularly x/y ranges) in the multiplot grid, adding
;    the object to the list of plots contained by the pp_multiplot object.
;    
; :Returns:
;    The contour object created in the multiplot grid. This is a regular object of
;    IDL's image class, and can be manipulated in the usual way. The method
;    `pp_multiplot::sync_axes` can be used to synchronize axes properties across
;    a line/column or all plots, after one plot has been changed (programmatically
;    or interactively). 
;
; :Params:
;    arg1: in, optional
;      The first argument to be passed on to plot(). See the help on plot()
;      for details.
;    arg2: in, optional
;      The second argument to be passed on to plot(). See the help on plot()
;      for details.
;    arg3: in, optional
;      The third argument to be passed on to plot(). See the help on plot()
;      for details.
;
; :Keywords:
;    TEST: in, optional, default=0
;      Passed on to plot(), to make plot() used the test data instead.
;    multi_index: in, optional
;      The index of the position to place the plot in the grid. Count starts at
;      0, on the top left, proceeding left-to-right, then top-to-bottom, up to
;      nlines*ncolumns-1.
;    xrange: in, optional
;      A 2-element array with the minimum and maximum to use for the x axis. If
;      not provided, the xrange set for the current column is used, or, if no range
;      was set for the column, the plot is created with the default range
;      (determined by plot()), and that range is subsequently set for the column
;      The other plots in the column have their ranges updated, if necessary. 
;    yrange: in, optional
;      A 2-element array with the minimum and maximum to use for the y axis. If
;      not provided, the xrange set for the current line is used, or, if no range
;      was set for the line, the plot is created with the default range
;      (determined by plot()), and that range is subsequently set for the line
;      The other plots in the line have their ranges updated, if necessary.
;    propagate: in, optional, default=1
;      Determines the mode of propagating the axes properties (range and endticks
;      not included) of this plot. If 0, no propagation of properties is done.
;      If 1, properties are propagated to all plots in the grid.
;      If 2, properties are propagated to all plots in the same line/column.
;    xendticks: in, optional
;      The mode set for suppressing the first/last x tick labels of the plot: 0
;      means that both the first and last labels are suppressed, 1 means that only
;      the last label is suppressed, 2 means that only the first label is suppressed,
;      and 3 means that neither is suppressed. The default for each plot depends
;      on its location on the multiplot grid (its mindex): Plots have their last
;      label suppressed (xendticks=1), except for those that fall on the rightmost
;      column (xendticks=3).
;    yendticks: in, optional
;      The mode set for suppressing the first/last y tick labels of the plot: 0
;      means that both the first and last labels are suppressed, 1 means that only
;      the last label is suppressed, 2 means that only the first label is suppressed,
;      and 3 means that neither is suppressed. The default for each plot depends
;      on its location on the multiplot grid (its mindex): Plots have their last
;      label suppressed (yendticks=1), except for those that fall on the top
;      line (yendticks=3).
;    _EXTRA: in, optional
;      Any keywords not handled by this method are passed on to plot(). See the
;      help on plot() for details.
;      
;      The following properties are axes properties, that can be made common across
;      columns (x) or lines (y) with the keyword `propagate`. See the help on plot
;      objects for details:
;      
;      [XY]COLOR, [XY]GRIDSTYLE, [XY]LOG, [XY]MAJOR, [XY]MINOR, [XY]SUBTICKLEN, [XY]TEXT_COLOR, 
;      [XY]TEXT_POS, [XY]THICK, [XY]TICKDIR, [XY]TICKFONT_NAME, [XY]TICKFONT_SIZE, [XY]TICKFONT_STYLE, 
;      [XY]TICKFORMAT, [XY]TICKINTERVAL, [XY]TICKLAYOUT, [XY]TICKLEN, [XY]TICKNAME, [XY]TICKUNITS, 
;      [XY]TICKVALUES, [XY]TITLE, [XY]TRANSPARENCY
;
;-
function pp_multiplot::contour,arg1, arg2, arg3, TEST=test, _EXTRA=ex,$
 multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
 xendticks=xendticks,yendticks=yendticks
compile_opt idl2, logical_predicate
graphic_to_do='contour'
case n_params() of
  0: ret=self.do_graphic(graphic_to_do=graphic_to_do, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks)
  1: ret=self.do_graphic(graphic_to_do=graphic_to_do,arg1, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks)
  2: ret=self.do_graphic(graphic_to_do=graphic_to_do,arg1, arg2, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks)
  3: ret=self.do_graphic(graphic_to_do=graphic_to_do,arg1, arg2, arg3, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks)
endcase
return,ret
end

;+
; :Description:
;    A wrapper for image(), which creates the image in the proper place and with
;    the right properties (particularly x/y ranges) in the multiplot grid, adding
;    the object to the list of plots contained by the pp_multiplot object.
;    
; :Returns:
;    The image object created in the multiplot grid. This is a regular object of
;    IDL's image class, and can be manipulated in the usual way. The method
;    `pp_multiplot::sync_axes` can be used to synchronize axes properties across
;    a line/column or all plots, after one plot has been changed (programmatically
;    or interactively). 
;
; :Params:
;    arg1: in, optional
;      The first argument to be passed on to plot(). See the help on plot()
;      for details.
;    arg2: in, optional
;      The second argument to be passed on to plot(). See the help on plot()
;      for details.
;    arg3: in, optional
;      The third argument to be passed on to plot(). See the help on plot()
;      for details.
;
; :Keywords:
;    TEST: in, optional, default=0
;      Passed on to plot(), to make plot() used the test data instead.
;    multi_index: in, optional
;      The index of the position to place the plot in the grid. Count starts at
;      0, on the top left, proceeding left-to-right, then top-to-bottom, up to
;      nlines*ncolumns-1.
;    xrange: in, optional
;      A 2-element array with the minimum and maximum to use for the x axis. If
;      not provided, the xrange set for the current column is used, or, if no range
;      was set for the column, the plot is created with the default range
;      (determined by plot()), and that range is subsequently set for the column
;      The other plots in the column have their ranges updated, if necessary. 
;    yrange: in, optional
;      A 2-element array with the minimum and maximum to use for the y axis. If
;      not provided, the xrange set for the current line is used, or, if no range
;      was set for the line, the plot is created with the default range
;      (determined by plot()), and that range is subsequently set for the line
;      The other plots in the line have their ranges updated, if necessary.
;    propagate: in, optional, default=1
;      Determines the mode of propagating the axes properties (range and endticks
;      not included) of this plot. If 0, no propagation of properties is done.
;      If 1, properties are propagated to all plots in the grid.
;      If 2, properties are propagated to all plots in the same line/column.
;    xendticks: in, optional
;      The mode set for suppressing the first/last x tick labels of the plot: 0
;      means that both the first and last labels are suppressed, 1 means that only
;      the last label is suppressed, 2 means that only the first label is suppressed,
;      and 3 means that neither is suppressed. The default for each plot depends
;      on its location on the multiplot grid (its mindex): Plots have their last
;      label suppressed (xendticks=1), except for those that fall on the rightmost
;      column (xendticks=3).
;    yendticks: in, optional
;      The mode set for suppressing the first/last y tick labels of the plot: 0
;      means that both the first and last labels are suppressed, 1 means that only
;      the last label is suppressed, 2 means that only the first label is suppressed,
;      and 3 means that neither is suppressed. The default for each plot depends
;      on its location on the multiplot grid (its mindex): Plots have their last
;      label suppressed (yendticks=1), except for those that fall on the top
;      line (yendticks=3).
;    _EXTRA: in, optional
;      Any keywords not handled by this method are passed on to plot(). See the
;      help on plot() for details.
;      
;      The following properties are axes properties, that can be made common across
;      columns (x) or lines (y) with the keyword `propagate`. See the help on plot
;      objects for details:
;      
;      [XY]COLOR, [XY]GRIDSTYLE, [XY]LOG, [XY]MAJOR, [XY]MINOR, [XY]SUBTICKLEN, [XY]TEXT_COLOR, 
;      [XY]TEXT_POS, [XY]THICK, [XY]TICKDIR, [XY]TICKFONT_NAME, [XY]TICKFONT_SIZE, [XY]TICKFONT_STYLE, 
;      [XY]TICKFORMAT, [XY]TICKINTERVAL, [XY]TICKLAYOUT, [XY]TICKLEN, [XY]TICKNAME, [XY]TICKUNITS, 
;      [XY]TICKVALUES, [XY]TITLE, [XY]TRANSPARENCY
;
;-
function pp_multiplot::image,arg1, arg2, arg3, TEST=test, _EXTRA=ex,$
 multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
 xendticks=xendticks,yendticks=yendticks,axis_style=axs,aspect_ratio=ar
compile_opt idl2, logical_predicate
graphic_to_do='image'
axs=n_elements(axs) ? axs : 2
ar=n_elements(ar) ? ar : 0
case n_params() of
  0: ret=self.do_graphic(graphic_to_do=graphic_to_do, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks,axis_style=axs,aspect_ratio=ar)
  1: ret=self.do_graphic(graphic_to_do=graphic_to_do,arg1, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks,axis_style=axs,aspect_ratio=ar)
  2: ret=self.do_graphic(graphic_to_do=graphic_to_do,arg1, arg2, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks,axis_style=axs,aspect_ratio=ar)
  3: ret=self.do_graphic(graphic_to_do=graphic_to_do,arg1, arg2, arg3, TEST=test, _EXTRA=ex,$
   multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
   xendticks=xendticks,yendticks=yendticks,axis_style=axs,aspect_ratio=ar)
endcase
return,ret
end


;+
; :Description:
;    A wrapper for plot(), image() and contour(), which creates the plot in the
;    proper place and with the right properties (particularly x/y ranges) in the
;    multiplot grid, adding the object to the list of plots contained by the
;    pp_multiplot object.
;    
; :Returns:
;    The plot object created in the multiplot grid. This is a regular object of
;    IDL's plot class, and can be manipulated in the usual way. The method
;    `pp_multiplot::sync_axes` can be used to synchronize axes properties across
;    a line/column or all plots, after one plot has been changed (programmatically
;    or interactively). 
;
; :Params:
;    arg1: in, optional
;      The first argument to be passed on to plot(). See the help on plot()
;      for details.
;    arg2: in, optional
;      The second argument to be passed on to plot(). See the help on plot()
;      for details.
;    arg3: in, optional
;      The third argument to be passed on to plot(). See the help on plot()
;      for details.
;
; :Keywords:
;    TEST: in, optional, default=0
;      Passed on to plot(), to make plot() used the test data instead.
;    multi_index: in, optional
;      The index of the position to place the plot in the grid. Count starts at
;      0, on the top left, proceeding left-to-right, then top-to-bottom, up to
;      nlines*ncolumns-1.
;    xrange: in, optional
;      A 2-element array with the minimum and maximum to use for the x axis. If
;      not provided, the xrange set for the current column is used, or, if no range
;      was set for the column, the plot is created with the default range
;      (determined by plot()), and that range is subsequently set for the column
;      The other plots in the column have their ranges updated, if necessary. 
;    yrange: in, optional
;      A 2-element array with the minimum and maximum to use for the y axis. If
;      not provided, the xrange set for the current line is used, or, if no range
;      was set for the line, the plot is created with the default range
;      (determined by plot()), and that range is subsequently set for the line
;      The other plots in the line have their ranges updated, if necessary.
;    propagate: in, optional, default=1
;      Determines the mode of propagating the axes properties (range and endticks
;      not included) of this plot. If 0, no propagation of properties is done.
;      If 1, properties are propagated to all plots in the grid.
;      If 2, properties are propagated to all plots in the same line/column.
;    xendticks: in, optional
;      The mode set for suppressing the first/last x tick labels of the plot: 0
;      means that both the first and last labels are suppressed, 1 means that only
;      the last label is suppressed, 2 means that only the first label is suppressed,
;      and 3 means that neither is suppressed. The default for each plot depends
;      on its location on the multiplot grid (its mindex): Plots have their last
;      label suppressed (xendticks=1), except for those that fall on the rightmost
;      column (xendticks=3).
;    yendticks: in, optional
;      The mode set for suppressing the first/last y tick labels of the plot: 0
;      means that both the first and last labels are suppressed, 1 means that only
;      the last label is suppressed, 2 means that only the first label is suppressed,
;      and 3 means that neither is suppressed. The default for each plot depends
;      on its location on the multiplot grid (its mindex): Plots have their last
;      label suppressed (yendticks=1), except for those that fall on the top
;      line (yendticks=3).
;    _EXTRA: in, optional
;      Any keywords not handled by this method are passed on to plot(). See the
;      help on plot() for details.
;      
;      The following properties are axes properties, that can be made common across
;      columns (x) or lines (y) with the keyword `propagate`. See the help on plot
;      objects for details:
;      
;      [XY]COLOR, [XY]GRIDSTYLE, [XY]LOG, [XY]MAJOR, [XY]MINOR, [XY]SUBTICKLEN, [XY]TEXT_COLOR, 
;      [XY]TEXT_POS, [XY]THICK, [XY]TICKDIR, [XY]TICKFONT_NAME, [XY]TICKFONT_SIZE, [XY]TICKFONT_STYLE, 
;      [XY]TICKFORMAT, [XY]TICKINTERVAL, [XY]TICKLAYOUT, [XY]TICKLEN, [XY]TICKNAME, [XY]TICKUNITS, 
;      [XY]TICKVALUES, [XY]TITLE, [XY]TRANSPARENCY
;
;-
function pp_multiplot::do_graphic,graphic_to_do=graphic_to_do,$
 arg1, arg2, arg3, TEST=test, _EXTRA=ex,$
 multi_index=mindex,xrange=xrange,yrange=yrange,propagate=propagate,$
 xendticks=xendticks,yendticks=yendticks
compile_opt idl2, logical_predicate

;Defaults
mindex=n_elements(mindex) eq 1 ? 0>mindex<(self.mlayout[0]*self.mlayout[1]-1) : self.mindex
self.mindex=mindex ;The index of the current location in the multiplot matrix
;Propagate [xy]properties over nothing (0), all (1), line/column (2)
propagate=(n_elements(propagate) eq 1) ? propagate : 1

;Determine the location for the plot
position=self.getposition(mindex,bottom=bottom,left=left,top=top,right=right,column=column,line=line)

;By default, supress end x/y ticks in plots that are in the middle of the columns/lines
if n_elements(xendticks) ne 1 then begin
  xendticks=right ? 3 : 1
  fixxticks=right ? 0 : 1
endif else fixxticks=0
if n_elements(yendticks) ne 1 then begin
  yendticks=top ? 3 : 1
  fixyticks=top ? 0 : 1
endif else fixyticks=0
;xendticks=n_elements(xendticks) eq 1 ? xendticks : right ? 3 : 1
;yendticks=n_elements(yendticks) eq 1 ? yendticks : top ? 3 : 1   

;Update the [xy]range, if provided
if (n_elements(xrange) eq 2) then begin
  newxranges=(self.xranges)[*]
  newxranges[column]=xrange
  self.setproperty,xranges=newxranges
endif
if (n_elements(yrange) eq 2) then begin
  newyranges=(self.yranges)[*]
  newyranges[line]=yrange
  self.setproperty,yranges=newyranges
endif

;Process the axes properties in the extra parameters
ex=(self.process_extras_plot(hash(ex),mindex,propagate)).tostruct()
;Combine extras with default properties
if self.graphproperties then begin
  exh=hash(ex)
  exh=self.graphproperties+exh
  ex=exh.tostruct()
endif

;Create the plot object
self.owindow.select,/clear ;Just to make this window the current one
if !version.release ge '8.2' then begin
  case n_params() of
    0:ret=call_function(graphic_to_do,TEST=test, _EXTRA=ex,position=position,/current,$
     xshowtext=bottom*1,yshowtext=left*1,xrange=(self.xranges)[column],yrange=(self.yranges)[line])
    1:ret=call_function(graphic_to_do,arg1,TEST=test, _EXTRA=ex,position=position,/current,$
     xshowtext=bottom*1,yshowtext=left*1,xrange=(self.xranges)[column],yrange=(self.yranges)[line])
    2:ret=call_function(graphic_to_do,arg1,arg2,TEST=test, _EXTRA=ex,position=position,/current,$
     xshowtext=bottom*1,yshowtext=left*1,xrange=(self.xranges)[column],yrange=(self.yranges)[line])
    3:ret=call_function(graphic_to_do,arg1,arg2,arg3,TEST=test, _EXTRA=ex,position=position,/current,$
     xshowtext=bottom*1,yshowtext=left*1,xrange=(self.xranges)[column],yrange=(self.yranges)[line])
  endcase
  axes=ret.axes
  axes[2].showtext=0
  axes[3].showtext=0
endif else begin
  case n_params() of
    0:ret=call_function(graphic_to_do,TEST=test, _EXTRA=ex,position=position,/current,$
      xtickfont_size=bottom*12,ytickfont_size=left*12,xrange=(self.xranges)[column],yrange=(self.yranges)[line])
    1:ret=call_function(graphic_to_do,arg1,TEST=test, _EXTRA=ex,position=position,/current,$
      xtickfont_size=bottom*12,ytickfont_size=left*12,xrange=(self.xranges)[column],yrange=(self.yranges)[line])
    2:ret=call_function(graphic_to_do,arg1,arg2,TEST=test, _EXTRA=ex,position=position,/current,$
      xtickfont_size=bottom*12,ytickfont_size=left*12,xrange=(self.xranges)[column],yrange=(self.yranges)[line])
    3:ret=call_function(graphic_to_do,arg1,arg2,arg3,TEST=test, _EXTRA=ex,position=position,/current,$
      xtickfont_size=bottom*12,ytickfont_size=left*12,xrange=(self.xranges)[column],yrange=(self.yranges)[line])
  endcase  
endelse

;Set the [xy]range, if necessary
if (~isa((self.xranges)[column])) then (self.xranges)[column]=ret.xrange
if (~isa((self.yranges)[line])) then (self.yranges)[line]=ret.yrange
if ~array_equal((self.xranges)[column],ret.xrange) then (self.xranges)[column]=ret.xrange
if ~array_equal((self.yranges)[line],ret.yrange) then (self.yranges)[line]=ret.yrange




;Remove the first/last labels, if necessary
self.setendticks,xendticks,ret,'x'
self.setendticks,yendticks,ret,'y'

(self.oplots)[mindex]=ret
(self.xendticks)[mindex]=xendticks
(self.yendticks)[mindex]=yendticks
;Change the default grid index to the next one 
if mindex lt (self.mlayout[0]*self.mlayout[1]-1) then self.mindex++

if self.xsupressdivision then begin
  if ~left then ret['axis1'].hide=1
  if ~right then ret['axis3'].hide=1
endif

if self.ysupressdivision then begin
  if ~bottom then ret['axis0'].hide=1
  if ~top then ret['axis2'].hide=1
endif

return,ret
end

;+
; :Description:
;    Processes the extra parameters passed to `pp_multiplot::plot`, to pick those
;    that specify axes properties. Axes properties are given a special treatment,
;    as they might be kept synchronized across lines/column/all plots. 
;    
;  :Hidden: This routine is to be used only by `pp_multiplot::plot`.
;  
;  :Private: This routine is to be used only by `pp_multiplot::plot`.
;
; :Params:
;    extras: in, required
;      Extra parameters passed to `pp_multiplot::plot`, converted to a hash, to be processed here.
;    mindex: in, required
;      The index of the plot being processed in the multiplot grid (0 to nlines*ncolumns-1).
;    propagate: in, required
;      If 0, no propagation of properties is done. If 1, properties are propagated
;      to all plots. If 2, properties are propagated to all
;      plots in the same line/column.
;      
;  :Returns:
;    A hash with the extra parameters that should be passed to plot(), which
;    includes those that were not altered here, and those that were processed
;    based on the x/y properties for that position in the grid.
;
;-
function pp_multiplot::process_extras_plot,extras,mindex,propagate
compile_opt idl2, logical_predicate,hidden

if (n_elements(extras) eq 0) then return,extras ;Get out now if nothing has to be done

column=mindex mod self.ncolumns
line=mindex/self.ncolumns

;Get the hashes of the currently set properties
currentx=((self.xproperties)[column])[*]
currenty=((self.yproperties)[line])[*]
to_update=hash() ;Indices of the plots that will have to be updated
foreach el,extras,i do begin ;Parse each extra argument, if it is an x/y property
  if currentx.haskey(i) then begin ;If it is one of the x properties to process
    if isa(el,'LIST') then foreach props,self.xproperties,j do ((self.xproperties)[j])[i]=el[j] else begin
      case propagate of
       ;Do not propagate property
       0 : currentx[i]=el
       ;Propagate property to all plots
       1 : begin
         foreach props,self.xproperties,j do ((self.xproperties)[j])[i]=el
         to_update=hash(indgen(self.nlines*self.ncolumns),self.oplots)
       end
       ;Propagate property over the plot's column
       2 : begin
         ((self.xproperties)[column])[i]=el
         to_update[column+indgen(self.nlines)*self.ncolumns]=(self.oplots)[column+indgen(self.nlines)*self.ncolumns]
       end
     endcase  
    endelse
  endif
  if currenty.haskey(i) then begin ;If it is one of the y properties to process
    if isa(el,'LIST') then foreach props,self.yproperties,j do ((self.yproperties)[j])[i]=el[j] else begin
      case propagate of
       ;Do not propagate property
       0 : currenty[i]=el
       ;Propagate property to all plots
       1 : begin
         foreach props,self.yproperties,j do ((self.yproperties)[j])[i]=el
         to_update=hash(indgen(self.nlines*self.ncolumns),self.oplots)
       end
       ;Propagate property over the plot's line
       2 : begin
         ((self.yproperties)[line])[i]=el
         to_update[line*self.ncolumns+indgen(self.ncolumns)]=(self.oplots)[line*self.ncolumns:(line+1)*self.ncolumns-1]
       end
     endcase  
    endelse
  endif  
endforeach

;Update the properties in the plot objects where it is necessary
foreach el,to_update,i do if isa(el) then $
 el.setproperty,_extra=(((self.xproperties)[i mod self.ncolumns])+((self.yproperties)[i/self.ncolumns])).tostruct()
 
;Prepare the new extras, which includes the x/y properties for that plot, and the other (untouched) properties

ret=extras[*]
foreach el,(self.xproperties)[column],i do if isa(el) then ret[i]=el
foreach el,(self.yproperties)[line],i do if isa(el) then ret[i]=el

return,ret
end

;+
; :Description:
;    Processes the extra parameters passed to `pp_multiplot::getproperty` and
;    `pp_multiplot::setproperty`, to pick those that specify axes properties.
;    Axes properties are given a special treatment, as they might be kept
;    synchronized across lines/column/all plots.
;    
;  :Hidden: This routine is to be used only by `pp_multiplot::getproperty` and 
;  `pp_multiplot::setproperty`.
;  
;  :Private: This routine is to be used only by `pp_multiplot::getproperty` and 
;  `pp_multiplot::setproperty`.
;
; :Params:
;    extras: in, required
;      Extra parameters that were passed to the caller, converted to a hash, to be
;      processed here.
;
; :Keywords:
;    get: out, optional
;      If provided, a variable where the values of the x/y properties that were
;      processed here are returned.
;    set: out, optional
;      This informs whether this routine encountered and processed any x/y properties,
;      so that `pp_multiplot::setproperty` can know that an update on plot properties
;      might be needed.
;      
;      The other keywords present here are the x/y properties that this routine processes,
;      and which must be present in the argument list so that their values can be passed
;      back trough `pp_multiplot::getproperty`, due to the way _ref_extra works.
;      
; :Returns:
;   A hash of the properties that were not processed here, which should be passed
;   on to the call of window's get/set property.
;
;-
function pp_multiplot::process_extras_properties,extras,get=get,set=set,$
  XCOLOR=xcolor,XGRIDSTYLE=xgridstyle,XLOG=xlog,XMAJOR=xmajor,XMINOR=xminor,XSUBTICKLEN=xsubticklen,$
  XTEXT_COLOR=xtext_color,XTEXT_POS=xtext_pos,XTHICK=xthick,XTICKDIR=xtickdir,XTICKFONT_NAME=xtickfont_NAME,$
  XTICKFONT_SIZE=xtickfont_size,XTICKFONT_STYLE=xtickfont_style,XTICKFORMAT=xtickformat,$
  XTICKINTERVAL=xtickinterval,XTICKLAYOUT=xticklayout,XTICKLEN=xticklen,XTICKNAME=xtickname,$
  XTICKUNITS=xtickunits,XTICKVALUES=xtickvalues,XTITLE=xtitle,XTRANSPARENCY=xtransparency,$
  YCOLOR=ycolor,YGRIDSTYLE=ygridstyle,YLOG=ylog,YMAJOR=ymajor,YMINOR=yminor,YSUBTICKLEN=ysubticklen,$
  YTEXT_COLOR=ytext_color,YTEXT_POS=ytext_pos,YTHICK=ythick,YTICKDIR=ytickdir,YTICKFONT_NAME=ytickfont_name,$
  YTICKFONT_SIZE=ytickfont_size,YTICKFONT_STYLE=ytickfont_style,YTICKFORMAT=ytickformat,$
  YTICKINTERVAL=ytickinterval,YTICKLAYOUT=yticklayout,YTICKLEN=yticklen,YTICKNAME=ytickname,$
  YTICKUNITS=ytickunits,YTICKVALUES=ytickvalues,YTITLE=ytitle,YTRANSPARENCY=ytransparency
compile_opt idl2, logical_predicate,hidden

retr=arg_present(get)
if retr then get=hash() else begin
  oldx=list()
  foreach column,self.xproperties,i do oldx.add,column[*]
  oldy=list()
  foreach column,self.yproperties,i do oldy.add,column[*]
endelse
set=0

if (n_elements(extras) eq 0) then return,hash() ;Get out if there is nothing to process

foreach value,extras,key do begin
  if ((self.xproperties)[0]).haskey(key) then begin
    set=1
    extras.remove,key
    if retr then begin;If properties are being retrieved
      get[key]=list()
      foreach column,self.xproperties do (get[key]).add,column[key]
    endif else begin ;If properties are being set
      if (n_elements(value) eq self.ncolumns) && isa(value,'LIST') then $
       foreach el,value,i do ((self.xproperties)[i])[key]=el else $ ;If a list was provided
       foreach el,self.xproperties do el[key]=value ;If a single value must be replicated for all columns
    endelse
  endif
  if ((self.yproperties)[0]).haskey(key) then begin
    set=1
    extras.remove,key
    if retr then begin;If properties are being retrieved
      get[key]=list()
      foreach column,self.yproperties do (get[key]).add,column[key]
    endif else begin ;If properties are being set
      if (n_elements(value) eq self.nlines) && isa(value,'LIST') then $
       foreach el,value,i do ((self.yproperties)[i])[key]=el else $ ;If a list was provided
       foreach el,self.yproperties do el[key]=value ;If a single value must be replicated for all lines
    endelse
  endif
endforeach

;Update any properties that have changed
if ~retr then begin
  foreach el,self.xproperties,column do if (el ne oldx[column]) then $
   foreach opl,(self.oplots)[indgen(self.nlines)*self.ncolumns+column] do $
   opl.setproperty,_extra=el.tostruct()
  foreach el,self.yproperties,line do if (el ne oldy[line]) then $
   foreach opl,(self.oplots)[indgen(self.ncolumns)+(self.ncolumns*line)] do $
   opl.setproperty,_extra=el.tostruct()
endif

;Send the retrieved properties back
if retr then begin
  if arg_present(xcolor) then xcolor=get['XCOLOR']
  if arg_present(xgridstyle) then xgridstyle=get['XGRIDSTYLE']
  if arg_present(xlog) then xlog=get['XLOG']
  if arg_present(xmajor) then xmajor=get['XMAJOR']
  if arg_present(xminor) then xminor=get['XMINOR']
  if arg_present(xsubticklen) then xsubticklen=get['XSUBTICKLEN']
  if arg_present(xtext_color) then xtext_color=get['XTEXT_COLOR']
  if arg_present(xtext_pos) then xtext_pos=get['XTEXT_POS']
  if arg_present(xthick) then xthick=get['XTHICK']
  if arg_present(xtickdir) then xtickdir=get['XTICKDIR']
  if arg_present(xtickfont_name) then xtickfont_name=get['XTICKFONT_NAME']
  if arg_present(xtickfont_size) then xtickfont_size=get['XTICKFONT_SIZE']
  if arg_present(xtickfont_style) then xtickfont_style=get['XTICKFONT_STYLE']
  if arg_present(xtickformat) then xtickformat=get['XTICKFORMAT']
  if arg_present(xtickinterval) then xticklayout=get['XTICKLAYOUT']
  if arg_present(xticklayout) then xticklayout=get['XTICKLAYOUT']
  if arg_present(xticklen) then xticklen=get['XTICKLEN']
  if arg_present(xtickname) then xtickname=get['XTICKNAME']
  if arg_present(xtickunits) then xtickunits=get['XTICKUNITS']
  if arg_present(xtickvalues) then xtickvalues=get['XTICKVALUES']
  if arg_present(xtitle) then xtitle=get['XTITLE']
  if arg_present(xtransparency) then xtransparency=get['XTRANSPARENCY']
  
  if arg_present(ycolor) then ycolor=get['YCOLOR']
  if arg_present(ygridstyle) then ygridstyle=get['YGRIDSTYLE']
  if arg_present(ylog) then ylog=get['YLOG']
  if arg_present(ymajor) then ymajor=get['YMAJOR']
  if arg_present(yminor) then yminor=get['YMINOR']
  if arg_present(ysubticklen) then ysubticklen=get['YSUBTICKLEN']
  if arg_present(ytext_color) then ytext_color=get['YTEXT_COLOR']
  if arg_present(ytext_pos) then ytext_pos=get['YTEXT_POS']
  if arg_present(ythick) then ythick=get['YTHICK']
  if arg_present(ytickdir) then ytickdir=get['YTICKDIR']
  if arg_present(ytickfont_name) then ytickfont_name=get['YTICKFONT_NAME']
  if arg_present(ytickfont_size) then ytickfont_size=get['YTICKFONT_SIZE']
  if arg_present(ytickfont_style) then ytickfont_style=get['YTICKFONT_STYLE']
  if arg_present(ytickformat) then ytickformat=get['YTICKFORMAT']
  if arg_present(ytickinterval) then yticklayout=get['YTICKLAYOUT']
  if arg_present(yticklayout) then yticklayout=get['YTICKLAYOUT']
  if arg_present(yticklen) then yticklen=get['YTICKLEN']
  if arg_present(ytickname) then ytickname=get['YTICKNAME']
  if arg_present(ytickunits) then ytickunits=get['YTICKUNITS']
  if arg_present(ytickvalues) then ytickvalues=get['YTICKVALUES']
  if arg_present(ytitle) then ytitle=get['YTITLE']
  if arg_present(ytransparency) then ytransparency=get['YTRANSPARENCY']
endif

ret=extras[*]
return,ret
end

;+
; :Description:
;    Updates the tick labels in the given plot object, to suppress the first, last
;    or both tick labels in the plot.
;    
;  :Hidden: This routine is to be used only by methods of `pp_multiplot`.
;  
;  :Private: This routine is to be used only by methods of `pp_multiplot`.
;
; :Params:
;    endticks: in, required
;      The mode to use for tick suppression. Like in `pp_plot`, 1 suppresses only
;      the last tick, 2 suppresses only the first tick, and 0 suppresses both.
;    opl: in, required
;      The plot object to act upon when updating the tick labels.
;    ax: in, required
;      The axis to act upon in the given plot object: 'x' or 'y'
;
;-
pro pp_multiplot::setendticks,endticks,opl,ax
compile_opt idl2,logical_predicate,hidden

;If necessary, revise x/yendticks
if (ax eq 'x') && (endticks eq 1) then begin
  xr=opl.xrange
  xtv=opl.xtickv
  nt=n_elements(xtv)
  ti=abs(xtv[-1]-xtv[-2])
  if ((abs(xr[1]-xtv[-1]))<(abs(xr[1]-xtv[0]))) gt self.xtickratio*ti*(nt/3d0) then endticks=3
endif
if (ax eq 'y') && (endticks eq 1) then begin
  xr=opl.yrange
  xtv=opl.ytickv
  nt=n_elements(xtv)
  ti=abs(xtv[-1]-xtv[-2])
  if ((abs(xr[1]-xtv[-1]))<(abs(xr[1]-xtv[0]))) gt self.ytickratio*ti*(nt/3d0) then endticks=3
endif


if (endticks ne 3) then begin
  axes=opl[ax+'axis']
  if axes eq obj_new() then axes=[]
  foreach el, axes do if isa(el.tickname) then begin
    tn=el.tickname
    range=ax eq 'x' ? el.xrange : el.yrange
    if range[1] ge range[0] then begin
      r0=0 & r1=-1
    endif else begin
      r0=-1 & r1=0
    endelse
    if ((endticks eq 0)||(endticks eq 2)) then tn[r0]=''
    if ((endticks eq 0)||(endticks eq 1)) then tn[r1]=''
    el.tickname=tn
  endif
endif
end

;+
; :Description:
;    Calculates the position parameters for a plot in the grid, given its index,
;    and the parameters set in the fields of self. For use of plot objects, use
;    `pp_multiplot::plot` directly, which already uses this method to compute the
;    location in the grid. This method can be useful if one wants to add another
;    type of Graphic (not a plot object) into the grid, such as an image or a contour.
;
; :Params:
;    mindex: in, required
;      The grid index of the plot being created, from 0 to ncolumns*nlines-1. 
;
; :Keywords:
;    bottom: out
;      Indicates whether this plot is in the bottom line of the grid.
;    left: out
;      Indicates whether this plot is in the left column of the grid.
;    top: out
;      Indicates whether this plot is in the top line of the grid.
;    right: out
;      Indicates whether this plot is in the right column of the grid.
;    column: out
;      The column where this plot lies on the grid (starting from 0).
;    line: out
;      The line where this plot lies on the grid (starting from 0).
;
;  :Returns:
;    The position array to be passed on to plot, to create this plot on the proper
;    place in the multiplot window. Contains [x0,y0,x1,y1], where the first two
;    refer to the lower-left corner, and the last two refer to the top-right corner,
;    with x counting from the left, and y counting from the bottom.
;-
function pp_multiplot::getposition,mindex,bottom=bottom,left=left,top=top,right=right,$
 column=column,line=line
compile_opt idl2,logical_predicate
;Determine the location for the plot
column=mindex mod self.ncolumns
line=mindex/self.ncolumns
;Parameters that determine if the axes labels are going to be drawn
left=column eq 0 ;Is this plot on the left column?
right=column eq (self.ncolumns-1) ;Is this plot on the right column?
bottom=line eq (self.nlines-1) ;Is this plot on the bottom line?
top=line eq 0 ;Is this plot on the bottom line?
fullwidth=1d0-self.global_margin[0]-self.global_margin[2]
fullheight=1d0-self.global_margin[1]-self.global_margin[3]
shiftsx=self.global_margin[0]+([0d0,total(self.cwidths.toarray(),/cumulative)])*fullwidth
shiftsy=self.global_margin[1]+(1d0-[0d0,total(self.lheights.toarray(),/cumulative)])*fullheight
return,[shiftsx[column]+(left ? 0d0 : self.xgap/2d0),shiftsy[line+1]+(bottom ? 0d0 : self.ygap/2d0),$
  shiftsx[column+1]-(right ? 0d0 : self.xgap/2d0),shiftsy[line]-(top ? 0d0 : self.ygap/2d0)]

end

;+
; :Description:
;    Retrieves properties from pp_multiplot objects. Since pp_multiplot inherits
;    from IDL_Object, these properties can be accessed with the dot (.) operator.
;    The extra properties not handled here are passed on to the getproperty method
;    of the window object (which contains the plots). 
;
; :Keywords:
;    multi_layout: out, optional
;      The layout of the grid, as a 2-element array with the number of
;      columns and the number of lines for the grid.
;    global_xtitle: out, optional
;      The common title for all the x axes.
;    global_ytitle: out, optional
;      The common title for all the y axes.
;    global_margin: out, optional
;      The margins, in normalized units (range 0 to 1) around the grid. Returned
;      as a 4-element array, for [left, bottom, right, top] margins.
;    window: out, optional
;      The window object, which contains the plots.
;    title: out, optional
;      The title for the entire plot (which resides in the window object).
;    xranges: out, optional
;      A list, with one element for each column, each being a 2-element array
;      with the minimum and maximum of the x axes for the plots on each column.    
;    yranges: out,optional
;      A list, with one element for each line, each being a 2-element array
;      with the minimum and maximum of the y axes for the plots on each line.    
;    xproperties: out,optional
;      A list, with one element for each column, each being a hash with the x
;      axes' properties that have been set for the plots on each column. On columns
;      with no properties set, the hash is empty.
;    yproperties: out,optional
;      A list, with one element for each line, each being a hash with the y
;      axes' properties that have been set for the plots on each line. On lines
;      with no properties set, the hash is empty.
;    xendticks: out, optional
;      A list, with one element for each column, each being the mode set for
;      suppressing the first/last x tick labels of the corresponding plot: 0
;      means that both the first and last labels are suppressed, 1 means that only
;      the last label is suppressed, 2 means that only the first label is suppressed,
;      and 3 means that neither is suppressed. The default for each plot depends
;      on its location on the multiplot grid (its mindex): Plots have their last
;      label suppressed (xendticks=1), except for those that fall on the rightmost
;      column (xendticks=3).
;    yendticks: out, optional
;      A list, with one element for each column, each being the mode set for
;      suppressing the first/last y tick labels of the corresponding plot: 0
;      means that both the first and last labels are suppressed, 1 means that only
;      the last label is suppressed, 2 means that only the first label is suppressed,
;      and 3 means that neither is suppressed. The default for each plot depends
;      on its location on the multiplot grid (its mindex): Plots have their last
;      label suppressed (yendticks=1), except for those that fall on the top
;      line (yendticks=3).
;    _ref_extra: out, optional
;      Any keywords not handled by this method are passed on to the getproperty
;      method of the window object. See the help on window objects for details.
;      
;      The following properties are axes properties, that can be made common across
;      columns (x) or lines (y). They are from the plot objects, and are returned
;      as a list, with one element for each column (x) or line (y). See the help
;      on plot objects for details:
;      
;      [XY]COLOR, [XY]GRIDSTYLE, [XY]LOG, [XY]MAJOR, [XY]MINOR, [XY]SUBTICKLEN, [XY]TEXT_COLOR, 
;      [XY]TEXT_POS, [XY]THICK, [XY]TICKDIR, [XY]TICKFONT_NAME, [XY]TICKFONT_SIZE, [XY]TICKFONT_STYLE, 
;      [XY]TICKFORMAT, [XY]TICKINTERVAL, [XY]TICKLAYOUT, [XY]TICKLEN, [XY]TICKNAME, [XY]TICKUNITS, 
;      [XY]TICKVALUES, [XY]TITLE, [XY]TRANSPARENCY
;
;-
pro pp_multiplot::getproperty,_ref_extra=ex,$
 multi_layout=mlayout,global_xtitle=gxtitle,global_ytitle=gytitle,$
 global_margin=gmargin,window=owindow,$
 title=otitle,xranges=xranges,yranges=yranges,$
 xproperties=xproperties,yproperties=yproperties,xendticks=xendticks,yendticks=yendticks,$
 xgap=xgap,ygap=ygap
compile_opt idl2, logical_predicate
if arg_present(gxtitle) then gxtitle=self.global_xtitle
if arg_present(gytitle) then gytitle=self.global_ytitle
if arg_present(gmargin) then gmargin=self.global_margin
if arg_present(mlayout) then mlayout=self.mlayout
if arg_present(owindow) then owindow=self.owindow
if arg_present(xranges) then xranges=(self.xranges)[*]
if arg_present(yranges) then yranges=(self.yranges)[*]
if arg_present(xendticks) then xendticks=(self.xendticks)[*]
if arg_present(yendticks) then yendticks=(self.yendticks)[*]
;Return only the x/y properties that have been set
if arg_present(xproperties) then xproperties=(self.xproperties)[where(self.xproperties ne !null)]
if arg_present(yproperties) then yproperties=(self.yproperties)[where(self.yproperties ne !null)]
if arg_present(xgap) then xgap=self.xgap
if arg_present(ygap) then ygap=self.ygap

;Filter and process the [x/y]properties from the extras
extras=hash(ex)
ex=self.process_extras_properties(extras,get=props,_extra=ex)
ex=(ex.keys()).toarray() ;Extras to pass on

;Get all other properties from the window object
;This block is to provide the right title object, due to a bug in IDL 8.0's igetid that breaks title in window objects.
if arg_present(otitle) then begin
  tool=self.owindow.gettool()
  id=tool.findidentifiers('*TITLE')
  otitle=tool.getbyidentifier(id)
endif
if (n_elements(ex) ne 0) then self.owindow.getproperty,_extra=ex

end

;+
; :Description:
;    Sets properties for pp_multiplot objects. Since pp_multiplot inherits
;    from IDL_Object, these properties can be accessed with the dot (.) operator.
;    The extra properties not handled here are passed on to the setproperty method
;    of the window object (which contains the plots). 
;
; :Keywords:
;    global_xtitle: in, optional
;      The common title for all the x axes.
;    global_ytitle: in, optional
;      The common title for all the y axes.
;    title: in, optional
;      The title for the entire plot (which resides in the window object).
;    xranges: in, optional
;      The minimum and maximum of the x axes for the plots on each column. Provided
;      as either a list, with one element for each column, each being a 2-element
;      array, or a 2-element array, to be applied for all columns, or as a [2,ncolumns]
;      array, with the minimum and maximum for each column.    
;    yranges: in,optional
;      The minimum and maximum of the y axes for the plots on each line. Provided
;      as either a list, with one element for each line, each being a 2-element
;      array, or a 2-element array, to be applied for all lines, or as a [2,nlines]
;      array, with the minimum and maximum for each line.
;    xproperties: in,optional
;      The x axes' properties that are set for the plots on each column. Provided
;      as either a list, with one element for each column, each being a hash with
;      the properties/values as keys/values, or a single hash, to be applied for
;      all columns.     
;    yproperties: in,optional
;      The y axes' properties that are set for the plots on each line. Provided
;      as either a list, with one element for each line, each being a hash with
;      the properties/values as keys/values, or a single hash, to be applied for
;      all lines.
;    xendticks: in, optional
;     The modes for suppressing the first/last x tick labels of the corresponding
;     plots: 0 means that both the first and last labels are suppressed, 1 means
;     that only the last label is suppressed, 2 means that only the first label
;     is suppressed, and 3 means that neither is suppressed. Provided as either
;     a nlines*ncolumns array, with one element for each plot, or as a scalar, to
;     be applied the same for all plots. 
;    yendticks: in, optional
;     The modes for suppressing the first/last y tick labels of the corresponding
;     plots: 0 means that both the first and last labels are suppressed, 1 means
;     that only the last label is suppressed, 2 means that only the first label
;     is suppressed, and 3 means that neither is suppressed. Provided as either
;     a nlines*ncolumns array, with one element for each plot, or as a scalar, to
;     be applied the same for all plots.
;    _extra: in, optional
;      Any keywords not handled by this method are passed on to the getproperty
;      method of the window object. See the help on window objects for details.
;      
;      The following properties are axes properties, that can be made common across
;      columns (x) or lines (y). They are for the plot objects, and are set
;      as either a list, with one element for each column (x) or line (y), or a
;      single property, to be replicated for all plots in the column/line. See
;      the help on plot objects for details:
;      
;      [XY]COLOR, [XY]GRIDSTYLE, [XY]LOG, [XY]MAJOR, [XY]MINOR, [XY]SUBTICKLEN, [XY]TEXT_COLOR, 
;      [XY]TEXT_POS, [XY]THICK, [XY]TICKDIR, [XY]TICKFONT_NAME, [XY]TICKFONT_SIZE, [XY]TICKFONT_STYLE, 
;      [XY]TICKFORMAT, [XY]TICKINTERVAL, [XY]TICKLAYOUT, [XY]TICKLEN, [XY]TICKNAME, [XY]TICKUNITS, 
;      [XY]TICKVALUES, [XY]TITLE, [XY]TRANSPARENCY
;      
;-
pro pp_multiplot::setproperty,_extra=ex,$
 global_xtitle=gxtitle,global_ytitle=gytitle,$
 title=title,xranges=xranges,yranges=yranges,$
 xproperties=xproperties,yproperties=yproperties,xendticks=xendticks,yendticks=yendticks,$
 xgap=xgap,ygap=ygap
compile_opt idl2, logical_predicate
if (n_elements(gxtitle) ne 0) then begin ;Set the global x title
  if obj_valid(self.global_xtitle) then self.global_xtitle.string=gxtitle else begin
    self.owindow.select,/clear ;Just to make this window the current one
    self.global_xtitle=text(0.5,0.025,gxtitle,alignment=0.5)
  endelse
endif
if (n_elements(gytitle) ne 0) then begin ;Set the global y title
  if obj_valid(self.global_ytitle) then self.global_ytitle.string=gytitle else begin
    self.owindow.select,/clear ;Just to make this window the current one
    self.global_ytitle=text(0.05,0.5,gytitle,alignment=0.5,baseline=[0.,1.0,0.],updir=[-1.,0.,0.])
  endelse
endif

;Set [xy]ranges
if ((n_elements(xranges) eq self.ncolumns) && isa(xranges,'list')) then self.xranges=xranges[*]
if (n_elements(xranges) eq 2*self.ncolumns) then for i=0,self.ncolumns-1 do (self.xranges)[i]=xranges[*,i]
if ((n_elements(xranges) eq 2) && (~isa(xranges,'list'))) then for i=0,self.ncolumns-1 do (self.xranges)[i]=xranges
;Update ranges if necessary
if (n_elements(xranges) ne 0) then foreach el,self.oplots,i do if isa(el) then begin
  column=i mod self.ncolumns
  if ~array_equal(el.xrange,xranges[column]) then begin
    el.xrange=(self.xranges)[column]
    self.setendticks,(self.xendticks)[i],el,'x'
    self.setendticks,(self.yendticks)[i],el,'y'
  endif
endif

if ((n_elements(yranges) eq self.nlines) && isa(yranges,'list')) then self.yranges=yranges[*]
if (n_elements(yranges) eq 2*self.nlines) then for i=0,self.nlines-1 do (self.yranges)[i]=yranges[*,i]
if ((n_elements(yranges) eq 2) && (~isa(yranges,'list'))) then for i=0,self.nlines-1 do self.yranges[i]=yranges
if (n_elements(yranges) ne 0) then foreach el,self.oplots,i do if isa(el) then el.yrange=(self.yranges)[i/self.ncolumns] 
;Update ranges if necessary
if (n_elements(yranges) ne 0) then foreach el,self.oplots,i do if isa(el) then begin
  line=i/self.ncolumns
  if ~array_equal(el.yrange,yranges[line]) then begin
    el.yrange=(self.yranges)[line]
    self.setendticks,(self.xendticks)[i],el,'x'
    self.setendticks,(self.yendticks)[i],el,'y'
  endif
endif

;Set [xy]endticks
oldxendticks=(self.xendticks)[*]
if (n_elements(xendticks) eq self.ncolumns*self.nlines) then (self.xendticks)[*]=xendticks[*]
if ((n_elements(xendticks) eq 1) && (~isa(xendticks,'list'))) then (self.xendticks)[*]=xendticks
;Update ranges if necessary
if (n_elements(xendticks) ne 0) then begin
  w=where(self.xendticks ne oldxendticks,/null)
  foreach el,(self.oplots)[w],i do if isa(el) then begin
    self.setendticks,(self.xendticks)[i],el,'x'
  endif
endif

oldyendticks=(self.yendticks)[*]
if (n_elements(yendticks) eq self.ncolumns*self.nlines) then (self.yendticks)[*]=yendticks[*]
if ((n_elements(yendticks) eq 1) && (~isa(yendticks,'list'))) then (self.yendticks)[*]=yendticks
;Update ranges if necessary
if (n_elements(yendticks) ne 0) then begin
  w=where(self.yendticks ne oldyendticks,/null)
  foreach el,(self.oplots)[w],i do if isa(el) then begin
    self.setendticks,(self.yendticks)[i],el,'y'
  endif
endif


;Set [xy]properties
oldx=list()
foreach column,self.xproperties,i do oldx.add,column[*]
oldy=list()
foreach column,self.yproperties,i do oldy.add,column[*]

;Filter and process the [x/y]properties from the extras
;Get around bug in IDL 8.0's hash init()
extras=hash() & if (n_elements(ex) ne 0) then begin
  tn=tag_names(ex)
  for i=0,n_tags(ex)-1 do extras[tn[i]]=ex.(i)
endif
ex=self.process_extras_properties(extras,set=set)
ex=ex.tostruct() ;Extras to pass on

;If the properties were provided in a list (one element per column/line)
if ((n_elements(xproperties) eq self.ncolumns) && isa(xproperties,'list')) then $
 foreach el,xproperties,i do (self.xproperties)[i]=el
if ((n_elements(yproperties) eq self.nlines) && isa(yproperties,'list')) then $
 foreach el,yproperties,i do (self.yproperties)[i]=el
;If the properties were provided in a hash (same properties for all columns/lines)
if (n_elements(xproperties) eq 1) && isa(xproperties,'hash') then foreach el,self.xproperties do el+=xproperties
if (n_elements(yproperties) eq 1) && isa(yproperties,'hash') then foreach el,self.yproperties do el+=yproperties 
;Update properties if necessary
if (n_elements(xproperties) ne 0)||(n_elements(yproperties) ne 0)||set then $
 foreach el,self.oplots,i do if isa(el) then begin
  column=i mod self.ncolumns
  line=i/self.ncolumns
  if ((self.xproperties)[column] ne oldx[column]) or ((self.yproperties)[line] ne oldy[line]) then $
   el.setproperty,_extra=((self.xproperties)[column]+(self.yproperties)[line]).tostruct()
end



;Pass on all other properties to the window object
;The following block is to avoid window's title property, which is broken in IDL 8.0
if (n_elements(title) ne 0) then begin
  self.getproperty,title=otitle
  if ~isa(oTitle) then $ ;Make a title if there is none
   itext,title,target=self.owindow->getfullidentifier(),/title else $
   otitle.setproperty,string=title ;Or just change the title's string if there a title already
   otitle.updatescene
endif
if (n_elements(ex) ne 0) then self.owindow.setproperty,_extra=ex

end

;+
; :Description:
;    Synchronizes the axes, across a line and column, or over the whole grid,
;    so that they have the same x/y ranges, and, optionally, the same set
;    x/y axes properties.
;
; :Params:
;    mindex: in, optional
;      If provided, uses the plot at position mindex (count starts from 0) as the
;      reference for the axes. If not provided, the currently selected plot is used.
;      If the index is not provided and none are selected, the first valid plot
;      is used.
;
; :Keywords:
;    layout: in, optional, default=0
;      If set, not only the ranges are synchronized, but also the x/y axes layouts,
;      from those properties that have been set (in xproperties and xyproperties).
;    all: in, optional, default=0
;      If set, synchronization is carried out on all plots in the grid, instead of
;      only to those at the same line and same column as the plot used as reference.
;      
; :Examples:
;    See the documentation on the method `pp_multiplot::plot` for examples.
;
;-
pro pp_multiplot::sync_axes,mindex,layout=layout,all=all
compile_opt idl2, logical_predicate

;Defaults
layout=n_elements(layout) eq 1 ? layout : 0
all=n_elements(all) eq 1 ? all : 0

if (n_elements(mindex) ne 1) then begin ;Get the ranges from the current plot
  sel=self.owindow.getselecteditems()
  if isa(sel) then begin
    sel.getproperty,xrange=xr,yrange=yr
    dss=(sel.getdataspace()).getfullidentifier()+'/'
    foreach el,self.oplots,i do if isa(el) then begin
      ds=stregex(el.getfullidentifier(),'.*(/DATA SPACE).*/',/extract)
      if dss eq ds then break
    endif
    mindex=i
  endif
endif
if (n_elements(mindex) eq 0) then mindex=0
;Get the ranges from the given index
mindex=0>mindex<(self.ncolumns*self.nlines-1)
column=mindex mod self.ncolumns
line=mindex/self.ncolumns
print,'sync_axes: Updating axes to those of index '+strtrim(mindex,2)
if (n_elements(mindex) eq 1) then begin
  if isa((self.oplots)[mindex]) then begin
    xr=((self.oplots)[mindex]).xrange
    yr=((self.oplots)[mindex]).yrange
    self.setendticks,(self.xendticks)[mindex],(self.oplots)[mindex],'x'
    self.setendticks,(self.yendticks)[mindex],(self.oplots)[mindex],'y'
  endif else begin
    print,'sync_axes: Selected index does not yet contain a plot; doing nothing'
    return
  endelse
endif
;Update the ranges
newxranges=(self.xranges)[*]
newyranges=(self.yranges)[*]
if all then begin ;Sync all plots
  newxranges[*]=xr
  newyranges[*]=yr  
endif else begin ;Sync only plots in the current line and column
  newxranges[column]=xr
  newyranges[line]=yr
endelse
self.setproperty,xranges=newxranges,yranges=newyranges

;Update the other properties, if this option was set
if layout then begin
  props=(self.xproperties)[column]+(self.yproperties)[line]
  props=(props[where(props ne !null)]).tostruct()
  if (~all) then begin ;Sync only plots in the current line and column
    to_update=hash(indgen(self.ncolumns)+self.ncolumns*line)
    to_update+=hash(indgen(self.nlines)*self.ncolumns+self.ncolumns*line)
    foreach el,(self.oplots)[(to_update.keys()).toarray()] do el.setproperty,_extra=props
  ;Sync all plots
  endif else foreach el,self.oplots do if isa(el) then el.setproperty,_extra=props
endif

end

pro pp_multiplot::decideintervals,dprange,dticks,dint,decide=decide,newrange=newrange,dminor=dminor,newdticks=newdticks;New algorithm
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
; :Description:
;    This method should be called after one or more plots in the multiplot had
;    its axes changed in a way that caused its ticks to be recomputed (setting the
;    range, for instance), so that the end ticks get fixed.
;
;-
pro pp_multiplot::updateranges,x=x,y=y
compile_opt idl2,logical_predicate

if keyword_set(x) then begin
  dint=0d0
  dticks=0
  xranges=(self.xranges)[*]
  foreach xr,xranges,ix do begin
    self.decideintervals,xr,dtick,dint,newrange=nr
    xranges[ix]=nr
  endforeach
  self.setproperty,xranges=xranges
endif

if keyword_set(x) then begin
  dint=0d0
  dticks=0
  xranges=(self.xranges)[*]
  foreach xr,xranges,ix do begin
    self.decideintervals,xr,dtick,dint,newrange=nr
    xranges[ix]=nr
  endforeach
  self.setproperty,yranges=yranges
endif

foreach el,self.oplots,iel do if isa(el) then begin
  el['yaxis'].tickname=''
  el['xaxis'].tickname=''
  self.setendticks,self.xendticks[iel],el,'x'
  self.setendticks,self.yendticks[iel],el,'y'
endif
end

;+
; :Description:
;    Simple wrapper for window::close, to make pp_multiplot objects
;    look almost like they inherited the window class they use. For more details,
;    see the help on the close method of IDL's Graphics.
;-
pro pp_multiplot::close
compile_opt idl2,logical_predicate
  self.owindow.close
end

;+
; :Description:
;    Simple wrapper for window::save, to make pp_multiplot objects
;    look almost like they inherited the window class they use. For more details,
;    see the help on the save method of IDL's Graphics.
;
; :Params:
;    filename, in, required
;      Passed on to window::save, the name for the file to create. Its extension
;      determines the type of file to be produced. See the help window::save for
;      more details.
;
; :Keywords:
;    _REF_EXTRA, in, out, optional
;      Any keywords are passed on, unaltered, to window::save. See the
;      help on the window::save for details.
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), 2010
;-
pro pp_multiplot::save,filename, _REF_EXTRA=_extra
compile_opt idl2,logical_predicate
  self.owindow.save,filename, _strict_extra=_extra
end

;+
; :Description:
;    Class definition for `pp_multiplot`.  
;-
pro pp_multiplot__define
compile_opt idl2, logical_predicate
!null={pp_multiplot, inherits idl_object, $
 owindow:obj_new(),$ ;The window is contained, instead of inherited, because of the convoluted way window objects are initialized.
 oplots:list(),$ ;Where the plots will be contained
 cwidths:list(),lheights:list(),mlayout:intarr(2),ncolumns:0,nlines:0,$ ;Specify the grid shape
 ;Global formatting
 global_title:'', global_xtitle:obj_new(), global_ytitle:obj_new(),$
 global_margin:dblarr(4),$
 mindex:0,$ ;Current position in the grid (0 to nlines*ncolumns)
 xranges:list(),yranges:list(),$ ;x/y ranges for each column/line in the grid
 xproperties:list(),yproperties:list(),$ ;x/y axes roperties for each column/line in the grid
 xendticks:list(),yendticks:list(),$ ;x/y endticks for each plot in the grid
 xgap:0d0,ygap:0d0,$ ;x/y gap between plots
 xsupressdivision:0B,ysupressdivision:0B,$ ;Supress the lines between plots in x/y
 graphproperties:obj_new(),$ ;Default properties for individual graphs
 xtickratio:0d0,ytickratio:0d0};Parameter to decide if the last tick on an axis should not be auto suppressed
end
