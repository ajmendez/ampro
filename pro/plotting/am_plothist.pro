PRO am_plothist, inputarr, xhist, yhist, $
                 index=index, $        ; index to show the results
                 BIN=bin, $            ; autobin if not passed
                 weight=inputweight, $ ; per point weighting
                 histmin=histmin, $    ; [opt] min limits for histogram
                 histmax=histmax, $    ; [opt] max  -- both min/max default to xrange
                 
                 ;; How to make the histograms
                 noplot=NoPlot, $             ; No Plot
                 overplot=overplot, $         ; Over plot this hist
                 rotate=rotate, $             ; Rotate this plot 
                 Boxplot = boxplot, $         ; Add lines between each bin
                 boxcolor=boxcolor, $         ;
                 Fill=Fill, $                 ; Fill in the polygon
                 Fcolor=Fcolor, $             ;
                 FLine=Fline, $               ; Fills in with a set of lines
                 Fspacing=Fspacing, $         ;
                 Forientation=Forientation, $ ;
                 Fpattern=Fpattern, $         ;
                 
                 ;; How to make the plot
                 xstyle=xstyle, xlog=xlog, xrange=xrange, xtitle=xtitle, $
                 ystyle=ystyle, ylog=ylog, yrange=yrange, ytitle=ytitle, $
                 axiscolor=axiscolor, $
                 color=color, $
                 
                 PSYM = psym, $
                 autobin=autobin, $ ;
                 halfbin=halfbin, $ ; CHECK
                 Peak=Peak, $
                 noNaN=noNan, $
                 
                 _EXTRA = _extra
  ;; by default does nan checks.
  
  On_error,2
  compile_opt idl2

  ;; a simple little hack to get nice dict support
  IF size(inputarr, /type) EQ 8 THEN BEGIN
    xrange = inputarr.xr
    xtitle = inputarr.pname
    bin = inputarr.bin
    arr = inputarr.x
  ENDIF ELSE BEGIN
    arr = inputarr
  ENDELSE
  
  ;; add indexing
  IF n_elements(index) EQ 0 THEN $
     index = lindgen(n_elements(inputarr))
  arr = arr[index]
  IF n_elements(inputweight) GT 0 THEN $
     weight = inputweight[index]
  

  ;; Check that we are ready to run
  narr = n_elements(arr)
  arrmin = min( arr, MAX = arrmax)
  IF n_params() LT 1 THEN message, 'ERROR - missing array input'
  IF narr LT 2 THEN message, 'ERROR - Input array must contain at least 2 elements'
  IF arrmin EQ arrmax THEN message, 'ERROR - Input array must contain distinct values'
  rotate = keyword_set(rotate)
  overplot = keyword_set(overplot)
  fill = keyword_set(fill)
  fline = keyword_set(fline)
  boxplot = keyword_set(boxplot)
  

  ;; Determining how to calculate bin size:
  IF n_elements(bin) EQ 0 THEN BEGIN
    bin = (NOT keyword_set(autobin)) ? $
           1.0 : $
           (arrmax-arrmin)/sqrt(narr)
  ENDIF
  bin = float(abs(bin)) ;; ensure that we are positive and a float
  
  
  ;; check histmin/histmax :: defaults to xrange, or yrange if set and rotated.
  tmprange = (n_elements(xrange) GT 0 AND NOT rotate) ? xrange : [arrmin, arrmax]
  tmprange = (n_elements(yrange) GT 0 AND rotate) ? yrange : tmprange
  histmin = (n_elements(histmin) EQ 0) ? min(tmprange) : float(abs(histmin))
  histmax = (n_elements(histmax) EQ 0) ? max(tmprange) : float(abs(histmax))


  ;; enable weights
  IF n_elements(weight) EQ 0 THEN weight = fltarr(n_elements(arr))+1.0
  IF n_elements(weight) NE n_elements(arr) THEN message, 'ERROR - weights and array dont match'
  
  
  ;; Compute the histogram and abcissa.    
  ;; Determine if a half bin shift is 
  ;; desired (default for integer data)
  halfbin = keyword_set(halfbin)
  IF halfbin THEN BEGIN 
    dtype = size(arr, /type)
    halfbin = (dtype NE 4) and (dtype NE 5) ;Non-integer data?
  ENDIF


  ;; Defaults to doing nan check
  IF NOT keyword_set(noNAN) THEN BEGIN
    good = where(finite(arr), ngoods)
    IF ngoods EQ 0 THEN message, 'ERROR - Input array contains no finite values'
    y = (halfbin) ? $
        round( arr[good] / bin ) : $
        floor( arr[good] / bin )
  ENDIF ELSE BEGIN
    y = (halfbin) ? $
        round( arr / bin ) : $
        floor( arr / bin )
  ENDELSE


  ;;Determine number in each bin:
  ;; yhist = histogram( y )
  ;; N_hist = N_elements( yhist )
  ;; ;;Positions of each bin:
  ;; xhist = lindgen( N_hist ) * bin + min(y*bin)  
  ;; if not halfbin then xhist = xhist + 0.5*bin
  
  ;; MENDEZ :: Handle weights.
  yhist = im_hist1d(arr, weight, binsize=bin, obin=xhist, $
                    histmin=histmin, histmax=histmax)
  n_hist = n_elements(yhist)


  ;; renormalizing the peak?
  IF keyword_set(Peak) THEN yhist = yhist * (Peak / float(max(yhist)))
  






  ;; If not doing a plot, exit here.
  IF keyword_set(NoPlot) THEN return
 





  ;; OK LETS DO THIS PLOT SHIT...
  IF n_elements(PSYM) EQ 0 THEN psym = 10 ;Default histogram plotting
  color = (n_elements(COLOR) EQ 0) ? !p.color : djs_icolor(color)
  axiscolor = (n_elements(AXISCOLOR) EQ 0) ? !p.color : djs_icolor(axiscolor)
  boxcolor = (n_elements(boxcolor) EQ 0) ? color : djs_icolor(boxcolor)
  Fillcolor = (n_elements(fcolor) EQ 0) ? color : djs_icolor(fColor)
  Fspacing = (n_elements(FSpacing) EQ 0) ? 0.0 : Fspacing
  Forientation = (n_elements(forientation) EQ 0) ? 0 : Forientation

  IF rotate THEN BEGIN
    oldx = xhist
    oldy = yhist
    xhist = oldy
    yhist = oldx
    
    IF n_elements(ystyle) EQ 0 THEN ystyle = 1
    IF n_elements(YRANGE) EQ 0 THEN yrange = [yhist[0]-bin, yhist[N_hist-1]+bin ]
    IF n_elements(xrange) EQ 0 THEN BEGIN
      xrange = [0, 0] ;; autorange
      ;; xhist = xhist > 1
    ENDIF
    
  ENDIF ELSE BEGIN
    IF n_elements(xstyle) EQ 0 THEN xstyle = 1
    IF n_elements(XRANGE) EQ 0 THEN xrange = [xhist[0]-bin, xhist[N_hist-1]+bin ]
    IF n_elements(yrange) EQ 0 THEN yrange = [0, 0]
  ENDELSE
  
  ;; setup xlog/ylog, or it is done below in the plot
  IF overplot THEN BEGIN ;if overplotting, was original plot a log?
    IF N_elements(xlog) EQ 0 THEN xlog = !X.type
    IF N_elements(ylog) EQ 0 THEN ylog = !Y.type
  ENDIF 
  
  ;; If we are doing a log plot cut off the edges and the sort.
  IF keyword_set(ylog) THEN BEGIN 
    ymin = min(yhist) GT 1 ? 1 : 0.01
    IF n_elements(yrange) GT 0 THEN ymin = ymin < yrange[0] 
    ;; ydata contains the y-positions where the lines should be linked.
    ydata = [ymin, yhist>ymin, ymin] 
  ENDIF ELSE BEGIN
    ydata = [0, yhist, 0]
  ENDELSE
  
  ;; xdata contains the y-positions where the lines should be linked.
  xdata = (rotate) ? $
          [xhist[0], xhist, xhist[n_hist-1]] : $
          [xhist[0] - bin, xhist, xhist[n_hist-1]+ bin]
  IF keyword_set(xlog) THEN BEGIN
    xmin = (min(xhist) EQ 0 AND keyword_set(xlog)) ? 0.001 : min(xrange)
    xrange[0] = xrange[0]>xmin
  ENDIF
  
  
  ;; JRM;;;;;
  xra_set = keyword_set(XRANGE)?1:0
  xst_set = keyword_set(xstyle)?1:0
  yst_set = keyword_set(ystyle)?1:0
  ;; JRM;;;;;
 
  
  IF NOT overplot THEN BEGIN
    plot, xdata, ydata, /nodata, PSYM=psym, color=axiscolor, $
          xstyle=xstyle, xlog=xlog, xrange=xrange, xtitle=xtitle, $
          ystyle=ystyle, ylog=ylog, yrange=yrange, ytitle=ytitle, $
          _EXTRA = _extra
    IF N_elements(xlog) EQ 0 THEN xlog = !X.type
    IF N_elements(ylog) EQ 0 THEN ylog = !Y.type
  ENDIF


  ;; Lets make a histogram:
  xcrange = (xlog) ? 10^!X.CRANGE : !X.CRANGE
  ycrange = (ylog) ? 10^!Y.CRANGE : !Y.CRANGE
  xcrange = xcrange[sort(xcrange)]
  ycrange = ycrange[sort(ycrange)]


  
  
  ;; apply an offset to either x or y depending on the axis we are plotting
  IF rotate THEN BEGIN
    Xfill = transpose([[Xhist],[Xhist]])
    Xfill = reform(Xfill, n_elements(Xfill))
    Xfill = (xlog) ? $
            [xcrange[0]/10, xfill, xcrange[0]/10] : $
            [xrange[0], xfill, xcrange[0]]
    
    Yfill = transpose([[Yhist-bin/2.0],[Yhist+bin/2.0]])
    Yfill = reform(Yfill, n_elements(Yfill))
    Yfill = [Yfill[0], Yfill, Yfill[n_elements(Yfill)-1]]
  ENDIF ELSE BEGIN
    Xfill = transpose([[Xhist-bin/2.0],[Xhist+bin/2.0]])
    Xfill = reform(Xfill, n_elements(Xfill))
    Xfill = [Xfill[0], Xfill, Xfill[n_elements(Xfill)-1]]
    
    Yfill = transpose([[Yhist],[Yhist]])
    Yfill = reform(Yfill, n_elements(Yfill))
    Yfill = (ylog) ? $
            [ycrange[0]/10, Yfill, ycrange[0]/10] : $
            [ycrange[0], Yfill, ycrange[0]]
  ENDELSE
  Xfill = Xfill > XCRANGE[0] < XCRANGE[1] ; Make sure within plot range
  Yfill = Yfill > YCRANGE[0] < YCRANGE[1]
  

  ;; fill it in
  IF fill THEN BEGIN 
    IF fline THEN BEGIN ;; orientation forces lines
      polyfill, Xfill, Yfill, color=Fillcolor, $
                line_fill=Fline, spacing=Fspacing, orient=Forientation, $
                _extra=extra
    ENDIF ELSE BEGIN
      polyfill, Xfill, Yfill, color=Fillcolor, _extra=extra
    ENDELSE
  ENDIF
  
  ;; Outline / draw
  oplot, xfill, yfill, psym=psym, color=color, _extra=extra
  
  
  ;; Make histogram boxes by drawing lines in data color.
  IF boxplot THEN BEGIN
    FOR j =0 ,N_Elements(xhist)-1 DO BEGIN
      IF NOT rotate THEN BEGIN
        PlotS, [xhist[j], xhist[j]]-bin/2, $
               [YCRange[0], yhist[j] < Ycrange[1]], $
               Color=boxColor,noclip=0, _Extra=extra
      ENDIF ELSE BEGIN
        PlotS, [xcrange[0], xhist[j]<xcrange[1]], $
               [yhist[j], yhist[j]]-bin/2, $
               Color=boxColor, noclip=0, _Extra=_extra
      ENDELSE
    ENDFOR
  ENDIF
END

  
















PRO check_plothist, _extra=extra
  
  array =  randomn(-299822L, 10000)
  range = [-3, 3]
  yrange = 10d^[-1, 3.1]
  bin = .25
  
  k = 0
  erase
  
  ;; second plot rotated
  plothist,  array, bin=bin, $
             xs=1, xrange=range, $
             /fill, $
             title='PLOTHIST, linear', $
             /noerase, position=subplot(3, 4, k++, gap=0.2)
  im_plothist, array, bin=bin, xs=1, xrange=range, $
               histmin=range[0], histmax=range[1], $
               color=djs_icolor('green'), thick=2, /overplot
  

  plothist,  array, bin=bin, $
             xs=1, xrange=range, $
             /ylog, yrange=yrange, $
             /fill, $
             title='PLOTHIST, log', $
             /noerase, position=subplot(3, 4, k++, gap=0.2)

  plothist,  array, L, V, bin=bin, /rotate, $
             ys=1, yrange=range, $
             /fill, $
             title='PLOTHIST, linear, rot', $
             /noerase, position=subplot(3, 4, k++, gap=0.2)
  
  plothist,  array, L, V, bin=bin, /rotate, $
             /xlog, xrange=yrange, $
             ys=1, yrange=range, $
             /fill, $
             title='PLOTHIST, log, rot', $
             /noerase, position=subplot(3, 4, k++, gap=0.2)



    ;; my new third plot linear
  am_plothist, array, L, V, bin=bin, /fill, $
               /fline, fspacing=0.1, forient=45, $
               weight=weight, $
               xs=1, xrange=range, $
               title='AM PLOTHIST: linear', $
               /noerase, position=subplot(3, 4, k++, gap=0.2)
  im_plothist, array,  bin=bin, xs=1, xrange=range, $
               histmin=range[0], histmax=range[1], $
               color=djs_icolor('green'), thick=2, /overplot
  
  am_plothist, array, L, V, bin=bin, /fill, $
               weight=weight, $
               xs=1, xrange=range, $
               /ylog, yrange=yrange, $
               title='AM PLOTHIST', $
               /noerase, position=subplot(3, 4, k++, gap=0.2)

  am_plothist, array, L, V, bin=bin, /fill, $
               weight=weight, $
               /rotate, $
               ys=1, yrange=range, $
               title='AM PLOTHIST', $
               /noerase, position=subplot(3, 4, k++, gap=0.2)

  am_plothist, array, L, V, bin=bin, /fill, $
               weight=weight, $
               /rotate, /xlog, xrange=yrange, $
               ys=1, yrange=range, $
               title='AM PLOTHIST', $
               /noerase, position=subplot(3, 4, k++, gap=0.2)


  scale = 1000.0
  
  yrange /=  scale
  
  weight = fltarr(n_elements(array))+1/scale
  am_plothist, array, L, V, bin=bin, /fill, $
               weight=weight, $
               xs=1, xrange=range, $
               title='AM PLOTHIST: linear', $
               /noerase, position=subplot(3, 4, k++, gap=0.2)
  im_plothist, array, weight=weight, bin=bin, xs=1, xrange=range, $
               histmin=range[0], histmax=range[1], $
               color=djs_icolor('green'), thick=2, /overplot
  
  am_plothist, array, L, V, bin=bin, /fill, $
               weight=weight, $
               xs=1, xrange=range, $
               /ylog, yrange=yrange, $
               title='AM PLOTHIST', $
               /noerase, position=subplot(3, 4, k++, gap=0.2)

  am_plothist, array, L, V, bin=bin, /fill, $
               weight=weight, $
               /rotate, $
               ys=1, yrange=range, $
               title='AM PLOTHIST', $
               /noerase, position=subplot(3, 4, k++, gap=0.2)

  am_plothist, array, L, V, bin=bin, /fill, $
               weight=weight, $
               /rotate, /xlog, $
               xrange=yrange, $
               ys=1, yrange=range, $
               title='AM PLOTHIST', $
               /noerase, position=subplot(3, 4, k++, gap=0.2)



  
END




