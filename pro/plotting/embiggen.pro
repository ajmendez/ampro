;+
; PURPOSE:
;  Embiggen(enlargen) a range by some percent
; EXAMPLE:
;  yr=[0,1]
;  print, embiggen(yr,0.1)
; MODIFICATION HISTORY:
;    [2011.04.27] Mendez
;-
FUNCTION embiggen, range, pembiggen, onlytop=onlytop,  onlybottom=onlybottom
  delta = (range[1]-range[0])

  erange = [-1, 1]/2d
  IF (keyword_set(onlytop)) THEN erange = [0.0, 1.0]
  IF (keyword_set(onlybottom)) THEN erange = [-1.0, 0.0]

  newrange = range + erange*double(delta)*double(pembiggen)
  return, newrange
END

