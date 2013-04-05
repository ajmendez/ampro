; A copy of IM_PLOTFAVES.pro:  Favorite settings for different plot and the sort.

pro am_plotfaves, postscript=postscript, keynote=keynote, x11=x11, restore=restore, $
                  xthick=xthick, ythick=ythick, thick=thick, charthick=charthick, $
                  charsize=charsize, color=color, background=backgorund ;, extras=extras

    ; Defaults
  IF NOT keyword_set(x11) THEN BEGIN
    if (n_elements(xthick) eq 0)     then !x.thick = 2.0 else !x.thick = xthick
    if (n_elements(ythick) eq 0)     then !y.thick = 2.0 else !y.thick = ythick
    if (n_elements(thick) eq 0)      then !p.thick = 2.0 else !p.thick = thick
    if (n_elements(charthick) eq 0)  then !p.charthick = 1.5 else !p.charthick = charthick
    if (n_elements(charsize) eq 0)   then !p.charsize = 1.8 else !p.charsize = charsize
  ENDIF
  if (n_elements(background) eq 0) then !p.background = djs_icolor('white') else !p.background=background
  if (n_elements(color) eq 0)      then !p.color = djs_icolor('black') else !p.color=color
    
    
  if keyword_set(postscript) then begin
    if (n_elements(xthick) eq 0)    then !x.thick = 4.0 else !x.thick = xthick
    if (n_elements(ythick) eq 0)    then !y.thick = 4.0 else !y.thick = ythick
    if (n_elements(thick) eq 0)     then !p.thick = 4.0 else !p.thick = thick
    if (n_elements(charthick) eq 0) then !p.charthick = 3.0 else !p.charthick = charthick
    if (n_elements(charsize) eq 0)  then !p.charsize = 1.8 else !p.charsize = charsize
    !P.font = 0
  endif
  
  if keyword_set(keynote) then begin
    if (n_elements(xthick) eq 0)    then !x.thick = 7.0 else !x.thick = xthick
    if (n_elements(ythick) eq 0)    then !y.thick = 7.0 else !y.thick = ythick
    if (n_elements(thick) eq 0)     then !p.thick = 7.0 else !p.thick = thick
    if (n_elements(charthick) eq 0) then !p.charthick = 6.0 else !p.charthick = charthick
    if (n_elements(charsize) eq 0)  then !p.charsize = 1.8 else !p.charsize = charsize
  endif

  if keyword_set(restore) then cleanplot, /silent
  
  return
end
