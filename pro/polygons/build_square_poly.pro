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





PRO build_square_poly, corners, outname
  write_poly, corners, outname
  write_weight, corners, '/tmp/weight.list'
  
  
  opt = ' -m1e-08s '
  tmp = opt + outname + ' ' + outname
  
  ;; am_spawn, 'poly2poly -iv4 -vn ' + tmp
  am_spawn, 'weight -z /tmp/weight.list -iv4 -vn '+tmp
  ;; am_spawn, 'pixelize ' + tmp
  am_spawn, 'snap ' + tmp
  am_spawn, 'balkanize ' + tmp
  am_spawn, 'unify ' + tmp
  
  read_mangle_polygons,outname,iracpoly, hdr=hdr
  iracpoly = set_poly_str(iracpoly)
  write_mangle_polygons,outname,iracpoly, hdr=hdr
end
