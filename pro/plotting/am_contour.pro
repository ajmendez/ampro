;; Make a contour plot of the data.  
PRO am_contour, XData, YData, cat, $
                cdata=cdata_all, $
                index=index, $
                overplot=overplot, $
                rainbowplot=rainbowplot, $
                symbol=ssymbol, $
                darkencontour=darkencontour, lightencontour=lightencontour, $
                darkenpoints=darkenpoints, lightenpoints=lightenpoints, $
                outpath=outpath, pathinfo=pathinfo, pathxy=pathxy, pathn=pathn, $
                smoothlen=smoothlen, npix=npix, $
                weightdata=weightdata,$
                highlightgreen=highlightgreen, $
                nopoints=nopoints, nocontours=nocontours, $
                noaxis=noaxis, nowhite=nowhite, $
                noerror=noerror, $
                _extra=extra

  ;; Create a color dat 
  IF n_elements(cdata_all) NE 0 THEN cdata = cdata_all
  IF n_elements(cdata) EQ 0 THEN $
     cdata = (create_cset(cat, ['entire'], /combine, _extra=extra)).(0)
  ;; Allow for indexed subsets to be plotted -----------------------------------
  IF n_elements(index) GT 0 THEN BEGIN
    IF n_elements(cdata) NE 0 THEN BEGIN  
      ;; index should be strictly smaller than before, so ok.
      ii = intersect(cdata.index, index, nii)
      IF nii EQ 0 THEN stop
      cdata.index = -999
      cdata.n = nii
      cdata.index = ii
      FOR i=0, n_tags(cdata.c)-1 DO BEGIN
        jj = intersect(cdata.c.(i).index, index, njj)
        cdata.c.(i).index = -999
        cdata.c.(i).n = njj
        IF njj GT 0 THEN cdata.c.(i).index = jj
      ENDFOR
    ENDIF
  ENDIF 

  ;; Create default color data if not defined ----------------------------------
  ncomponents = n_tags(cdata.c)
  
  ;; Setup plotting enviroment if not defined ----------------------------------
  IF NOT keyword_set(overplot) THEN BEGIN
    IF NOT keyword_set(noaxis) THEN style=1 ELSE style=1+4
    plot, [0],[0], /nodata, $
          xs=style, xr=XData.xr, xtitle=XData.pName, xtickv=XData.ticks, $
          ys=style, yr=Ydata.xr, ytitle=Ydata.pName, ytickv=YData.ticks, $
          _extra=extra
  ENDIF
  
  ;IF n_elements(symbol) EQ 0 THEN symbol=[16,0.4] ;;default value, set here and in create_cset
  ; IF n_elements(smoothlen) EQ 0 THEN smoothlen=0.5
  ; if n_elements(npix) eq 0 then npix = 10
  ;;npix can also be defined here.
  ; IF n_elements(smoothlen) EQ 0 THEN smoothlen=1
  ; if n_elements(npix) eq 0 then npix = 15
  IF n_elements(smoothlen) EQ 0 THEN smoothlen=1.
  if n_elements(npix) eq 0 then npix = 16

  ;; TODO FIX ------------------------------------------------------------------
  sw = lindgen(ncomponents) ; the switch for plot order
  IF keyword_set(highlightgreen) THEN BEGIN
    cNames = strlowcase(tag_names(cdata.c))
    g = where(cNames EQ 'green', ng)
    IF ng GT 0 THEN BEGIN
      b  = where(cNames EQ 'blue',nb) 
      IF nb GT 0 THEN BEGIN
        ex = complement([g,b], n_elements(cNames),nex)
        IF nex GT 0 THEN BEGIN
          sw = [b,ex,g]
        ENDIF ELSE sw = [b,g]
      ENDIF ELSE BEGIN
        ex = complement(g, n_elements(cNames),nex)
        IF nex GT 0 THEN BEGIN
          sw = [ex,g]
        ENDIF ELSE  sw = [g]
      ENDELSE
    ENDIF ELSE BEGIN
      print, 'am_contour: No green to highlight'
    ENDELSE
  ENDIF

;; stop

    

  

  ;; Pixelated weight map. -----------------------------------------------------
  IF keyword_set(rainbowplot) THEN BEGIN
    npix = 20
    image = dblarr(npix,npix,3)
    FOR k=0,ncomponents-1 DO BEGIN
      j=sw[k]
      component = cdata.c.(j)
      IF component.n GT 0 THEN begin
        colorindex = component.index[lindgen(component.n)]
        IF keyword_set(weightdata) THEN $
           weight=cdata.weight[colorindex]
        am_scatterplot, XData.X[colorindex],YData.X[colorindex], $
                        weight=weight, $
                        xnpix=npix, ynpix=npix,$
                        xr=!X.crange, yr=!Y.crange, $
                        /nocontours, /nogreyscale, /overplot, cumimage=grayvalue
        image[*,*,j]=grayvalue
      ENDIF
    ENDFOR
    ;; Handle Purple/ALl components
    IF ncomponents LT 3 THEN image[*,*,2] = image[*,*,0]
    IF ncomponents LT 2 THEN image[*,*,1] = image[*,*,0]
    
    oplotimage, byte((max(image)-image)*256d), $
                imgxrange=!X.crange, imgyrange=!Y.crange, range=[0,1]
    return
  ENDIF
  
  ;; Plot Outliers, or everything if not whitening -----------------------------
  IF NOT keyword_set(nopoints) THEN BEGIN
    FOR k=0,ncomponents-1 DO BEGIN
      j=sw[k]
      component = cdata.c.(j)
      IF component.n GT 0 THEN BEGIN
        colorindex = component.index[lindgen(component.n)]
        IF keyword_set(weightdata) THEN $
           weight=cdata.weight[colorindex]
        
        color = djs_icolor(component.color)
        IF keyword_set(lightenpoints) THEN color = djs_icolor('light '+component.color)
        IF keyword_set(darkenpoints) THEN color = djs_icolor('dark '+component.color)

        IF NOT keyword_set(nowhite) THEN BEGIN
          am_scatterplot, XData.X[colorindex], YData.X[colorindex],$
                          smoothlen=smoothlen, xnpix=npix, ynpix=npix, $
                          weight=weight,$
                          xr=!X.crange, yr=!Y.crange, $
                          color=color, $
                          /nocontours, /overplot, /nogreyscale, $
                          /internal_weight, /nopoint, $
                          /outliers, ioutliers=ioutliers, _extra=extra
        ENDIF ELSE BEGIN
          ioutliers = lindgen(n_elements(colorindex))
        ENDELSE
        IF n_elements(ioutliers) NE 0 THEN BEGIN
          IF min(ioutliers) GE 0 THEN BEGIN
            IF keyword_set(weightdata) THEN BEGIN
              weight = cdata.weight[colorindex[ioutliers]]
              ioutliers = ioutliers[where(weight ne 0)]
            ENDIF
            IF n_elements(component.symbol) GT 0 THEN symbol = component.symbol
            IF n_elements(ssymbol) GT 0 THEN symbol = ssymbol
            if symbol[0] eq 4 then psym=symbol[0] else psym=symcat(symbol[0])
            oplot, XData.X[colorindex[ioutliers]], $
                   YData.X[colorindex[ioutliers]], $
                   color=color, $
                   psym=psym, symsize=symbol[1], $
                   _extra=extra
          ENDIF
        ENDIF
      ENDIF
    ENDFOR
  ENDIF

  ;; White out area between contours -------------------------------------------
  IF NOT keyword_set(nowhite) THEN BEGIN
    FOR k=0,ncomponents-1 DO BEGIN
      j=sw[k]
      component = cdata.c.(j)
      IF component.n GT 0 THEN BEGIN
        colorindex = component.index[lindgen(component.n)]
        IF keyword_set(weightdata) THEN $
           weight = cdata.weight[colorindex]
        am_scatterplot, XData.X[colorindex], YData.X[colorindex], $
                        smoothlen=smoothlen, xnpix=npix, ynpix=npix, $
                        weight=weight, $
                        xr=!X.crange, yr=!Y.crange, $
                        color=djs_icolor(component.color), $
                        /nogreyscale, /overplot, /white, $
                        /internal_weight, _extra=extra
      ENDIF
    ENDFOR
  ENDIF
  

  ;; Add the colorful contours -------------------------------------------------
  IF NOT keyword_set(nocontours) THEN BEGIN
    FOR k=0,ncomponents-1 DO BEGIN
      j=sw[k]
      component = cdata.c.(j)
      IF component.n GT 0 THEN BEGIN
        colorindex = component.index[lindgen(component.n)]
        IF keyword_set(weightdata) THEN $
           weight = cdata.weight[colorindex]
        IF keyword_set(darkencontour) THEN ccolor=djs_icolor('dark '+ component.color)
        IF keyword_set(lightencontour) THEN ccolor=djs_icolor('light '+ component.color)
        
        
        IF keyword_set(outpath) THEN BEGIN
          am_scatterplot, XData.X[colorindex], YData.X[colorindex], $
                          smoothlen=smoothlen, xnpix=npix, ynpix=npix, $
                          weight=weight, $
                          xr=!X.crange, yr=!Y.crange, $
                          color=djs_icolor(component.color), $
                          ccolor=ccolor, $
                          /outpath, pathinfo=info, pathxy=xy,$
                          /nogreyscale, /overplot, $
                          /internal_weight, /noplot, _extra=extra
          IF n_elements(pathinfo) EQ 0 THEN BEGIN
            pathinfo = info
            pathxy = xy
            pathn = n_elements(info)
          ENDIF ELSE BEGIN
            FOR i=0, n_elements(info)-1 DO BEGIN
              info[I].offset += n_elements(pathxy[0,*])
            ENDFOR
            pathn = [pathn, n_elements(info)]
            pathxy = [transpose([transpose(pathxy[0,*]), transpose(xy[0,*])]), $
                      transpose([reform(pathxy[1,*]), reform(xy[1,*])])]
            pathinfo = struct_append(pathinfo,info)
          ENDELSE
          ;; print, pathn
          ;; struct_print, pathinfo

          ;; print, size(pathxy, /dim)
        ENDIF ELSE BEGIN
          am_scatterplot, XData.X[colorindex], YData.X[colorindex], $
                          smoothlen=smoothlen, xnpix=npix, ynpix=npix, $
                          weight=weight, $
                          xr=!X.crange, yr=!Y.crange, $
                          color=djs_icolor(component.color), $
                          ccolor=ccolor, $
                          /nogreyscale, /overplot, $
                          /internal_weight, _extra=extra
        ENDELSE
      ENDIF
    ENDFOR
  ENDIF
  ;; IF keyword_set(outpath) THEN stop

  ;; Add error bars to the plot ------------------------------------------------
  IF NOT keyword_set(noerror) THEN begin
    IF (XData.error NE -999) OR (YData.error NE -999) THEN BEGIN
      am_errorplot, [XData.error, YData.error], _extra=extra
    ENDIF
  ENDIF


    
END

PRO am_contour_test, combined, _extra=extra
  cat = combined[where(combined.good EQ 2)]
  data = create_data(cat)
  cset = create_cset(cat, subset='purple')
  cdata = cset.(0)
  am_contour, data.c, data.a, cat, cdata=cdata, _extra=extra
END

