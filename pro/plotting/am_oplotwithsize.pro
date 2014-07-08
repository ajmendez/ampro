;; am_OplotWithSize -- Drop in replacement for oplot that allows for arrays
;; of color, psym and symsize.  This is not optimized for speed.  Can pass 
;; additional oplot keywords. Assumes x,y have same size.
;; optional ::
;;  index=index -- plot only a subset
;;  symscale=[minSymSize, maxSymSize] -- scale points
;;
PRO am_oplotwithsize, x, y, index=index, $
                      color=color, cscale=cscale, $
					  psym=psym, $
					  symsize=symsize, symscale=symscale, $
					  _extra=extra
  nx = n_elements(x)
  IF n_elements(symsize) EQ 0 THEN symsize = 1
  IF n_elements(symsize) EQ 1 THEN ptsize = replicate(symsize, nx) ELSE ptsize = symsize
  IF n_elements(psym) EQ 0 THEN p = 6 ELSE p = psym
  IF n_elements(p) EQ 1 THEN p = replicate(p, nx)
  IF n_elements(color) EQ 0 THEN c = !P.color ELSE c = color
  IF n_elements(c) EQ 1 THEN c = replicate(djs_icolor(c), nx)
  IF n_elements(index) EQ 0 THEN index = lindgen(nx)
  
  
  ;; splog, 'numbers', n_elements(x), n_elements(y), n_elements(symsize), n_elements(index)
  
  IF n_elements(symscale) ne 0 and min(ptsize) NE max(ptsize) THEN BEGIN
    ptsize = ptsize - min(ptsize[index])
    ptsize /= float(max(ptsize[index]))
    ptsize =  symscale[1]*ptsize + symscale[0]
  ENDIF

  IF n_elements(cscale) ne 0 and min(c) NE max(c) THEN BEGIN
    c -= min(c[index])
    c /= float(max(c[index]))
    c =  cscale[1]*c + cscale[0]
  ENDIF
  
  FOR i=0L, n_elements(index)-1 DO BEGIN
    oplot, [x[index[i]]], [y[index[i]]], $
           symsize=ptsize[index[i]], $
		   psym=p[index[i]], $
		   color=c[index[i]],$
           _extra=extra
  ENDFOR
END


PRO _test
  x = randomu(seed, 100)
  y = randomu(seed, 100)
  c = randomu(seed, 100)
  s = randomu(seed, 100)
  
  ;; http://www.exelisvis.com/docs/LoadingDefaultColorTables.html
  loadct, 3                     ; Red Temp
  plot, [0], /nodata
  am_oplotwithsize, x, y, symsize=s, symscale=[2, 6], $
                    color=c, cscale=[32, 80], psym=symcat(16)
END


