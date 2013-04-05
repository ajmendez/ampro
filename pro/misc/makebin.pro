;; returns nbins+1 array of bins in range. roger roger.
;; the keyword bin, allows you to pass the bin size for nbins, which
;; then will create an array that evenly spaced at bin length.
FUNCTION makebin, nbins, range, bin=bin
  n = abs((range[1]-range[0])/double(nbins))
  sign = (range[1] GT range[0]) ? 1d: -1d
  IF n GT 1000 OR nbins GT 10000 THEN message, 'something failed.'
  if keyword_set(bin) then return, double(range[0]) + sign*dindgen(n+2)*nbins
  return, double(range[0] + (range[1]-range[0])*dindgen(nbins+1)/double(nbins))
END
