FUNCTION structmap, smap, item, strict=strict, exact=exact, default=default
  FOR i=0, n_elements(item)-1 DO BEGIN
    IF keyword_set(exact) THEN BEGIN
      ;; [2013.04.28] Need an /exact key
      ii = where(strlowcase(tag_names(smap)) EQ strlowcase(item[i]), nii)
    ENDIF ELSE BEGIN
      ;; [2011.12.06] this was a bit too open, /strict to tighten this up
      IF keyword_set(strict) THEN $
         l = min(strlen([item[i]])) ELSE $
            l = min(strlen([tag_names(smap), item[i]]))
      ii = where(strcmp(strlowcase(tag_names(smap)), strlowcase(item[i]), l) EQ 1, nii)
    ENDELSE
    
    ;; clean up  -- this needs some debugging
    IF nii EQ 0 AND n_elements(default) GT 0 THEN return, default
    IF nii EQ 0 THEN $
       message, 'check sample('+item[i]+') '+$
                'and colormap: ('+strjoin(strlowcase(tag_names(smap)), ', ')+')'
    out = ( n_elements(out) GT 0 ) ? [out, smap.(ii)] : smap.(ii)
  ENDFOR
  return, out
END
