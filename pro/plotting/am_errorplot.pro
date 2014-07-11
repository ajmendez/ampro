;; Make Errors bars located at some percent of the width and height of
;; a plot. RENAME?
PRO am_errorplot, errors, P=P, errthick=errthick, _extra=extra
  IF n_elements(errthick) EQ 0 THEN errthick=0.9*!P.thick
  IF n_elements(p) EQ 0 THEN p = 0.93
  IF max(errors) GT 0 THEN BEGIN
    IF !X.crange[0] GT !X.crange[1] THEN xsign=-1 ELSE xsign=1
    IF !Y.crange[0] GT !Y.crange[1] THEN ysign=-1 ELSE ysign=1
    X = leftin(!X.crange, P=P)
    Y = topin(!Y.crange, P=P)
    Xerror = errors[0] > 0      ;either errorvalue if greater than 0
    Yerror = errors[1] > 0
    
    oploterror, X-xsign*xerror, Y+ysign*yerror, Xerror, Yerror, $
                hatlength=!D.X_VSIZE/150., errthick=errthick, _extra=extra
    IF xerror EQ 0 OR yerror EQ 0 THEN $
       oplot, [X-xsign*xerror], [Y+ysign*yerror], psym=1, symsize=0.2, _extra=extra
  ENDIF    
END
;;Unit test of the errorbarplot
PRO errorplot_test, combined
  cat = combined[where(combined.good EQ 2)]
  data = create_data(cat, subset=['C','A'])
  multicontour, data, cat
END

