;; matches series of check values (check) to an array(array), and
;; returns an bool array of size n_elements(array) if matches OR of
;;all of the checks.
FUNCTION strm, array, checks, ngood, invert=invert, index=iigood
  value = (keyword_set(invert)) ? 0 : 1
  isgood = bytarr(n_elements(array))
  FOR i=0, n_elements(checks)-1 DO BEGIN
    ii = where(strmatch(array, '*'+checks[i]+'*', /fold) EQ value,  nii)
    IF nii GT 0 THEN isgood[ii] = 1
  ENDFOR
  iigood = where(isgood EQ 1, ngood)
  return, isgood
END
