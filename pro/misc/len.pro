;+
; len.pro -- the same thing as n_elements, but this matches python so
; there we go.
;-
FUNCTION len, x
  return, n_elements(x)
END

  
