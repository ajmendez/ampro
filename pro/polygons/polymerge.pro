;+
; PURPOSE:
;    merges two polygons. not the most efficient method.
; MODIFICATION HISTORY:
;    [2011.08.25] Mendez
;-

;;[2011.08.25] Mendez copied this from goddard with  modifications
function polygon_overlap,poly1,poly2,newpoly=newpoly,minstr=minstr,str=str
  if(n_elements(minstr) eq 0) then minstr=0.
  
  newncaps=poly1.ncaps+poly2.ncaps
  if(n_tags(newpoly) eq 0) then begin
    newpoly=construct_polygon(nelem=1,ncaps=newncaps)
    noreturn=1
  endif
  newpoly.ncaps=newncaps
  newpoly.weight=poly1.weight
  (*newpoly.caps)[0L:poly1.ncaps-1L]=(*poly1.caps)[*]
  (*newpoly.caps)[poly1.ncaps:poly1.ncaps+poly2.ncaps-1L]=(*poly2.caps)[*]
  newpoly.use_caps=(poly1.use_caps+ishft(poly2.use_caps,poly1.ncaps))
  list=lindgen(newncaps)
  set_use_caps,newpoly,list,/allow_neg_doubles
  newpoly.str=garea(newpoly)
  str=newpoly.str

  return, newpoly
end


FUNCTION polymerge, poly1, poly2
  IF n_elements(poly2) EQ 0 THEN $
     return, poly1
  
  k = 0L
  N = n_elements(poly1)*n_elements(poly2)
  ;; mergedpolys = replicate(poly1[0], N)
  mergedpolys = construct_polygon(nelem=N)
  
  nprint = long(N/20.0)
  splog, 'Number of Polygons: n1*n2 = ', N
  FOR i=0L, n_elements(poly1)-1 DO BEGIN
    FOR j=0L, n_elements(poly2)-1 DO BEGIN
      poly = polygon_overlap(poly1[i],poly2[j])
      mergedpolys[k++] = poly
      ;; IF n_elements(mergedpoly) EQ 0 THEN BEGIN
      ;;   mergedpoly = poly
      ;; ENDIF ELSE BEGIN
      ;;   mergedpoly = struct_append(mergedpoly,poly)
      ;; ENDELSE
      IF (k MOD nprint) EQ 0 THEN splog, 'finished: '+number_formatter(k/float(N)*100, d=1)+'%'
    ENDFOR
  ENDFOR
  ;; return, mergedpoly
  
  ;; there are a bunch of zero polygons, so remove them
  ii = where(mergedpolys.str GT 0, nii, complement=jj, ncomplement=njj)
  splog, 'Number of Overlaping Polygons: ', nii
  IF nii EQ 0 THEN message, 'No polygons overlap'
  IF njj GT 0 THEN destruct_polygon, mergedpolys[jj]
  return, mergedpolys[ii]
  
END


