;+
; PURPOSE:
;    Erase plot and make a title
; EXAMPLE:
;    am_plottitle,'title', [xpercent=0.5, ypercent=0.99, /noerase]
; MODIFICATION HISTORY:
;    [2011.09.01] Mendez
;-

PRO am_plottitle, title, xpercent=xpercent, ypercent=ypercent, $
                  overplot=overplot, _extra=extra
  IF n_elements(xpercent) EQ 0 THEN xpercent = 0.5
  IF n_elements(ypercent) EQ 0 THEN ypercent = 0.99
  IF NOT keyword_set(overplot) THEN $
     plot, [0], /nodata, xs=4, ys=4, _extra=extra
  xyouts, xpercent, ypercent, title, align=0.5, _extra=extra
END

