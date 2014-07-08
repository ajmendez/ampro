PRO write_poly, corners, outname
  npoly = n_elements(corners[*, 0, 0])
  
  openw, lun, outname, /get_lun
  printf, lun, strtrim(string(npoly),2)+' polygons'
  printf, lun, 'unit d'
  FOR ii=0L, npoly-1 DO BEGIN
    printf, lun, $
           corners[ii,0,0],corners[ii,0,1], $
           corners[ii,1,0],corners[ii,1,1], $
           corners[ii,2,0],corners[ii,2,1], $
           corners[ii,3,0],corners[ii,3,1], $
           format='(f13.7,1x,f13.7,1x,f13.7,1x,f13.7,1x,f13.7,1x,f13.7,1x,f13.7,1x,f13.7,1x,f13.7)'
  ENDFOR
  free_lun, lun
END

PRO write_weight, corners, outname
  npoly = size(corners, /dim)
  openw, lun, outname, /get_lun
  FOR i=0L, npoly[0]-1 DO BEGIN
    printf, lun, $
            corners[i, 4, 0], $ ; weight
            format='(f13.7)' 
  ENDFOR
  free_lun, lun
END

PRO write_reg, corners, outname, labels=labels
  ic = [0, 1, 2, 3, 0]
  npoly = size(corners, /dim)
  openw, lun, outname, /get_lun
  FOR i=0L, npoly[0]-1 DO BEGIN
    printf, lun, 'fk5; polygon( ', format='( (A0), $)'
    FOR k=0, n_elements(ic)-1 DO BEGIN
      j =  ic[k]
      printf, lun, corners[i, j, 0], format='((F13.7),$)' 
      printf, lun, corners[i, j, 1], format='((F13.7),$)' 
    ENDFOR
    IF n_elements(labels) GT 0 THEN BEGIN
      printf, lun, ' ) # text={'+strtrim(labels[i])+'}'
    ENDIF ELSE BEGIN
      printf, lun, ' ) '
    ENDELSE
    
  ENDFOR
  free_lun, lun
END





PRO build_square_poly, corners, outname, $
                       makereg=makereg, labels=labels
  outname = repstr(outname, '.ply', '')
  

  IF keyword_set(makereg) THEN BEGIN
    write_reg, corners, outname+'.reg', labels=labels
    return
  ENDIF
  
  write_poly, corners, outname+'_base.ply'
  write_weight, corners, '/tmp/weight.list'
  
  
  ;; opt = ' -m1e-08s '
  opt = ' -m1e-6s '
  ;; tmp = opt + outname + ' ' + outname
  
  ;; am_spawn, 'poly2poly -iv4 -vn ' + tmp
  am_spawn, 'weight -z /tmp/weight.list -iv4 -vn '+ $
            opt + outname+'_base.ply ' + outname+'_weight.ply'
  
  ;; am_spawn, 'pixelize ' + tmp
  am_spawn, 'snap -S -a1e-8s -b1e-8s -t1e-8s ' + opt + outname+'_weight.ply '+$
            outname+'_snap.ply'
;  am_spawn, 'balkanize ' + opt + outname+'_snap.ply '+$
;            outname+'_balkanize.ply'
  am_spawn, 'sed -i "s/snapped/snapped\nbalkanized/g" '+outname+'_snap.ply'
  am_spawn, 'unify ' + opt + outname+'_snap.ply '+$
            outname+'_unify.ply'
  
  read_mangle_polygons,outname+'_unify.ply',iracpoly, hdr=hdr
  iracpoly = set_poly_str(iracpoly)
  write_mangle_polygons,outname+'_final.ply',iracpoly, hdr=hdr
end
