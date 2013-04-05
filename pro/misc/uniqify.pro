;; similar to x[uniq(x,sort(x))] but preserves order.
FUNCTION uniqify, x,  sort=sort
  IF keyword_set(sort) THEN BEGIN
    return, x[uniq(x,sort(x))]
  ENDIF

  FOR i=0L, n_elements(x)-1 DO BEGIN
    IF i EQ 0 THEN BEGIN
      y = x[0]
    ENDIF ELSE BEGIN
      ii = where(x[i] EQ y, nii)
      IF nii EQ 0 THEN y = [y, x[i]]
    ENDELSE
  ENDFOR
  return, y
END
