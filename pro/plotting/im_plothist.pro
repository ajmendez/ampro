;+
; NAME:
;   IM_PLOTHIST
;
; PURPOSE:
;   Generate a histogram plot.
;
; INPUTS: 
;
; OPTIONAL INPUTS: 
;
; KEYWORD PARAMETERS: 
;    cumulative - plot the cumulative distribution
;
; OUTPUTS: 
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; MODIFICATION HISTORY:
;    J. Moustakas, 2007-Apr-02, NYU - written, based on
;      HOGG_PLOTHIST and PLOTHIST
;
; Copyright (C) 2007, John Moustakas
; 
; This program is free software; you can redistribute it and/or modify 
; it under the terms of the GNU General Public License as published by 
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
; 
; This program is distributed in the hope that it will be useful, but 
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details. 
;-

pro im_plothist, arr, xhist, yhist, bin=bin, edge=edge, weight=weight, peak=peak,$
  psym=psym, cumulative=cumulative, normfactor=normfactor, noplot=noplot, $
  overplot=overplot, fill=fill, fcolor=fcolor, fline=fline, fspacing=fspacing, $
  fpattern=fpattern, forientation=forientation, fraction=fraction, _extra=extra

    ndata = n_elements(arr)
    if (n_elements(weight) eq 0L) then weight = dblarr(ndata)+1.0

    if (n_elements(bin) eq 0L) then $
      bin = (max(arr)-min(arr))/double(ceil(0.3D*sqrt(ndata))) else $
        bin = float(abs(bin))

    yhist = im_hist1d(arr,weight,binsize=bin,obin=xhist,binedge=edge,_extra=extra)
    n_hist = n_elements(yhist)

    if keyword_set(fraction) then normfactor = total(yhist)
    if (n_elements(normfactor) ne 0L) then yhist = yhist/float(normfactor)
  if (n_elements(peak) ne 0L) then yhist = yhist*(peak/float(max(yhist)))

    if keyword_set(noplot) then return

    if (n_elements(psym) eq 0L) then psym = 10
    
    if keyword_set(overplot) then begin
       if keyword_set(cumulative) then begin
          yhist = total(yhist,/cumulative)/total(yhist)
          djs_oplot, xhist, yhist, psym=psym, _extra=extra 
       endif else begin
          djs_oplot, [xhist[0]-bin,xhist,xhist[n_hist-1]+bin], [0,yhist,0], $ 
            psym=psym, _extra=extra
       endelse
    endif else begin
       if keyword_set(cumulative) then begin
          yhist = total(yhist,/cumulative)/total(yhist)
          djs_plot, xhist, yhist, psym=psym, _extra=extra 
       endif else begin
          djs_plot, [xhist[0]-bin,xhist,xhist[n_hist-1]+bin], [0,yhist,0],  $ 
            psym=psym, _extra=extra
       endelse
    endelse

    if keyword_set(fill) then begin

       xfill = transpose([[Xhist-bin/2.0],[Xhist+bin/2.0]])
       xfill = reform(xfill, n_elements(xfill))
       xfill = [xfill[0], xfill, xfill[n_elements(xfill)-1]]
       yfill = transpose([[yhist],[yhist]])
       yfill = reform(yfill, n_elements(yfill))
       yfill = [0, yfill, 0]
       ;; [2011.04.29] Mendez fix for reversed axis
       xcrange = !X.crange[sort(!X.crange)]
       ycrange = !Y.crange[sort(!Y.crange)]
       ;; [2012.06.20] Mendez fix for log plots with fill
       IF !Y.type THEN ycrange = 10.0^ycrange
       
       xfill = xfill > XCRANGE[0] < XCRANGE[1] ;Make sure within plot range
       yfill = yfill > YCRANGE[0] < YCRANGE[1]

       if keyword_set(Fcolor) then Fc = Fcolor else Fc = !P.Color
       if keyword_set(Fline) then begin
          if keyword_set(Fspacing) then Fs = Fspacing else Fs = 0
          if keyword_set(Forientation) then Fo = Forientation else Fo = 0
          polyfill, xfill,yfill, color=Fc, /line_fill, spacing=Fs, orient=Fo
       endif else begin
          if keyword_set(Fpattern) then begin
             polyfill, xfill,yfill, color=Fc, pattern=Fpattern
          endif else begin
             polyfill, xfill,yfill, color=Fc
          endelse
       endelse

; because the POLYFILL can erase/overwrite parts of the originally plotted
; histogram, we need to replot it here.
       if keyword_set(cumulative) then $
         djs_plot, xhist, yhist, psym=psym, _extra=extra  else $
           djs_oplot, [xhist[0]-bin,xhist,xhist[n_hist-1]+bin], [0,yhist,0], $
         psym=psym, _extra=extra
    endif

return    
end
