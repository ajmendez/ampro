;+
; PURPOSE:
;    switch between a standard plot and a ps/eps plot
; EXAMPLE:
;   psplot,'figure.ps.gz'
;     ... plot commands
;   psplot, /finish
; MODIFICATION HISTORY:
;    [2011.04.21] MENDEZ: added some checks on the filename
;-

PRO psplot, outps, $
            xsize=xsize, ysize=ysize,  $
            xoffset=xoffset, yoffset=yoffset, $
            encap=encap, finish=finish, quiet=quiet, landscape=landscape
  COMMON psplot_internal, compress, psname

  IF n_elements(xsize) EQ 0 THEN xsize = 16
  IF n_elements(ysize) EQ 0 THEN ysize = 16
  IF n_elements(yoffset) EQ 0 THEN yoffset = (keyword_set(landscape)) ? ysize : 8.0
  IF n_elements(xoffset) EQ 0 THEN xoffset = 1
  
  IF n_elements(outps) GT 0 THEN BEGIN 
    finish = 0
    IF n_elements(encap) EQ 0 THEN encap = (strmatch(outps, '*.eps*') EQ 1) ?1:0
    IF strmatch(outps, '*.gz') THEN compress = 1 ELSE compress = 0
    psname = outps
  ENDIF ELSE BEGIN
    finish = 1
  ENDELSE
  ;; IF n_elements(outps) EQ 0 THEN finish=1 else finish=0
  ;; if not keyword_set(quiet) then quiet = 0
	
  IF NOT keyword_set(finish) THEN BEGIN
    set_plot,'ps'
    ;;ps_open, file=expand_path(outps), font=1, /color, /portrait
    device, filename=expand_path(repstr(psname, '.gz', '')), $
            yoffset=yoffset, ysize=ysize, $
            xoffset=xoffset, xsize=xsize, $
            scale_factor=1.2,$
            /HELVETICA, font_size=8, $
            /color, preview=encap, $
            ;; output="%%Title: Graphics produced by ME", $
            encapsulated=encap, landscape=landscape
    ;; fix: [2012.06.12] at some point, I should fix that banner thing
    ;; spawn,'cat '+filename+$                            ; replace irritating
    ;; '| sed "s|Graphics produced by IDL|'+filename+$  ; IDL plot banner
    ;; '|" >  idltemp.ps; mv idltemp.ps '+filename      ; with the file name 

    action = 'Started'
  ENDIF ELSE BEGIN
    ;; outfile = fstat(!D.UNIT)
    device, /close
    set_plot,'x'
    !P.Background = djs_icolor('white')
    !P.Color = djs_icolor('black')
    verb = (keyword_Set(silent))? '':'-v'
    IF keyword_set(compress) THEN $
       spawn, 'gzip '+verb+' -f '+ expand_path(repstr(psname, '.gz', ''))
    action = 'Saved to'
  ENDELSE
  IF NOT keyword_set(silent) THEN $
     splog, Action+': '+ expand_path(psname)

END

