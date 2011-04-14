FUNCTION test, nrandom,  idl=idl, seed=seed
  IF n_elements(seed) EQ 0 THEN seed = -232425L
  IF n_elements(nrandom) EQ 0 THEN nrandom = 10000
  
  array = randomn(seed, nrandom, nrandom)
  
  dim = 2

  IF keyword_set(idl) THEN BEGIN
    medarr = median(array, dimension=dim, /even)
  ENDIF ELSE BEGIN
    dimvec = size(array, /dim)
    ndim = n_elements(dimvec)

    newdimvec = dimvec[ where(lindgen(ndim)+1 NE dim) ]
    newsize = N_elements(array) / dimvec[dim-1]
    medarr = reform(fltarr(newsize), newdimvec)

    
    soname = filepath('libtest.'+idlutils_so_ext(), $
                      root_dir=getenv('AMPRO_DIR'), $
                      subdirectory='lib')
    retval = call_external(soname, 'arrmedian', $
                           ndim, dimvec, float(array), long(dim), medarr)
  ENDELSE
  
  return,  medarr
END


FUNCTION testrun,  npoint, niter=niter, _extra=extra
  reportnum, niter=niter, nprint=20,  /start
  FOR i=0, niter-1 DO BEGIN
    a = test(npoint, _extra=extra)
    reportnum, i
  ENDFOR
  reportnum,  /finish,  tfinish=tfinish
  return, tfinish
END




PRO check_test
  
  niter = 100
  npoints = [10, 20, 50, 75, 100, 150, 200, 400, 500]
  npoints = [10, 20, 50, 75, 100]
  
  times = dblarr(n_elements(npoints), 2)
  FOR i=0, n_elements(npoints)-1 DO BEGIN
    times[i, 0] = testrun(npoints[i], niter=niter)
    times[i, 1] = testrun(npoints[i], niter=niter, /idl)
  ENDFOR
  
  plot, npoints, times[*, 0], psym=-1,  yr=minmax(times), $
        position=subplot(2, 1, 0, egap=0.3)
  oplot, npoints, times[*, 1], psym=-1,  color=djs_icolor('red')
  
  plot, npoints,  times[*, 0]/times[*, 1],  psym=-4, $
        position=subplot(2, 1, 1, egap=0.3),  /noerase
  
        
END

