;; this makes it easier to plot the polygon.  might not be the
;;fastest, but it works
PRO ppoly, filename, color=color, cweight=cweight, overplot=overplot, poly=poly, _extra=extra
  read_mangle_polygons, filename, poly
  
  IF keyword_set(cweight) THEN BEGIN
    ;; am_plotfaves, /x11
    ;; cgLoadCT, 6,  /reverse
    weight = poly.weight
    ii = where(weight GE 0, nii)
    range = minmax(weight[ii])
    ;; print, range
    colors = ((255-43)*((weight-range[0])/(range[1]-range[0]))+43)
  ENDIF
  
  plot_poly, poly, xr=xr, yr=yr, /noplot
  IF NOT keyword_set(overplot) THEN BEGIN
    plot, [0], /nodata, $
          title=filename, $
          xs=2, xr=reverse(xr), xtitle='dec', $
          ys=2, yr=yr, ytitle='ra', $
          position=subplot(1, 1, 0, egap=0.07),  _extra=extra
  ENDIF
    
  
  IF keyword_set(cweight) THEN BEGIN
    cgLoadCT, 6,  /reverse
    FOR i=0, nii-1 DO BEGIN
      plot_poly, poly[ii[i]], color=colors[ii[i]], $
                 xr=xr, yr=yr, $
                 ;; over=(i NE  0), $
                 /over, /fill, $
                 _extra=extra
    ENDFOR

    cgColorBar, /vert, /right, $
                bottom=43, $
                title='fraction', yticklen=0.025, $
                range=range, $
                ;; format='(f5.2)', $
                ;; divisions=4, minor=4, $
                position=subsubplot(subplot(1, 1, 0, egap=0.07), $
                                    subplot(1, 10, 9, /def) ), $
                /noerase
  ENDIF ELSE BEGIN
    plot_poly, poly, color=djs_icolor(color), /over,  _extra=extra
  ENDELSE
  
END
