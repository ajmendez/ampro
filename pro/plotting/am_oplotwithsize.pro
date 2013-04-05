PRO am_oplotwithsize, x, y, index=index, symsize=symsize, _extra=extra
  IF n_elements(symsize) EQ 0 THEN symsize = 1
  IF n_elements(symsize) EQ 1 THEN ptsize = replicate(symsize, n_elements(x)) ELSE ptsize = symsize
  IF n_elements(index) EQ 0 THEN index = lindgen(n_elements(x))
  
  ;; splog, 'numbers', n_elements(x), n_elements(y), n_elements(symsize), n_elements(index)
  
  ;; IF min(ptsize) NE max(ptsize) THEN BEGIN
  ;;   ptsize = ptsize - min(ptsize[index])
  ;;   ptsize /= float(max(ptsize[index]))
  ;;   ptsize =  3.0*ptsize + 0.5
  ;; ENDIF
  
  FOR i=0L, n_elements(index)-1 DO BEGIN
    oplot, [x[index[i]]], [y[index[i]]], $
           symsize=ptsize[index[i]], $
           _extra=extra
  ENDFOR
END
