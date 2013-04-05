;+
; NAME:
;       LEGEND -- MENDEZ
; PURPOSE:
;       Create an annotation legend for a plot.
;       Mendez was here cleaning up things and adding polys, and symsizes
; EXPLANATION:
;       This procedure makes a legend for a plot.  The legend can contain
;       a mixture of symbols, linestyles, Hershey characters (vectorfont),
;       and filled polygons (usersym).  A test procedure, legendtest.pro,
;       shows legend's capabilities.  Placement of the legend is controlled
;       with keywords like /right, /top, and /center or by using a position
;       keyword for exact placement (position=[x,y]) or via mouse (/position).
; CALLING SEQUENCE:
;       LEGEND [,items][,keyword options]
; EXAMPLES:
;       The call:
;               legend,['Plus sign','Asterisk','Period'],psym=[1,2,3]
;         produces:
;               -----------------
;               |               |
;               |  + Plus sign  |
;               |  * Asterisk   |
;               |  . Period     |
;               |               |
;               -----------------
;         Each symbol is drawn with a plots command, so they look OK.
;         Other examples are given in optional output keywords.
;
;       lines = indgen(6)                       ; for line styles
;       items = 'linestyle '+strtrim(lines,2)   ; annotations
;       legend,items,linestyle=lines            ; vertical legend---upper left
;       items = ['Plus sign','Asterisk','Period']
;       sym = [1,2,3]
;       legend,items,psym=sym                   ; ditto except using symbols
;       legend,items,psym=sym,/horizontal       ; horizontal format
;       legend,items,psym=sym,box=0             ; sans border
;       legend,items,psym=sym,delimiter='='     ; embed '=' betw psym & text
;       legend,items,psym=sym,margin=2          ; 2-character margin
;       legend,items,psym=sym,position=[x,y]    ; upper left in data coords
;       legend,items,psym=sym,pos=[x,y],/norm   ; upper left in normal coords
;       legend,items,psym=sym,pos=[x,y],/device ; upper left in device coords
;       legend,items,psym=sym,/position         ; interactive position
;       legend,items,psym=sym,/right            ; at upper right
;       legend,items,psym=sym,/bottom           ; at lower left
;       legend,items,psym=sym,/center           ; approximately near center
;       legend,items,psym=sym,number=2          ; plot two symbols, not one
;       legend,items,/fill,psym=[8,8,8],colors=[10,20,30]; 3 filled squares
; INPUTS:
;       items = text for the items in the legend, a string array.
;               For example, items = ['diamond','asterisk','square'].
;               You can omit items if you don't want any text labels.
; OPTIONAL INPUT KEYWORDS:
;
;       linestyle = array of linestyle numbers  If linestyle[i] < 0, then omit
;               ith symbol or line to allow a multi-line entry.     If 
;               linestyle = -99 then text will be left-justified.  
;       psym = array of plot symbol numbers.  If psym[i] is negative, then a
;               line connects pts for ith item.  If psym[i] = 8, then the
;               procedure usersym is called with vertices define in the
;               keyword usersym.   If psym[i] = 88, then use the previously
;               defined user symbol
;       vectorfont = vector-drawn characters for the sym/line column, e.g.,
;               ['!9B!3','!9C!3','!9D!3'] produces an open square, a checkmark,
;               and a partial derivative, which might have accompanying items
;               ['BOX','CHECK','PARTIAL DERIVATIVE'].
;               There is no check that !p.font is set properly, e.g., -1 for
;               X and 0 for PostScript.  This can produce an error, e.g., use
;               !20 with PostScript and !p.font=0, but allows use of Hershey
;               *AND* PostScript fonts together.
;       N. B.: Choose any of linestyle, psym, and/or vectorfont.  If none is
;               present, only the text is output.  If more than one
;               is present, all need the same number of elements, and normal
;               plot behaviour occurs.
;               By default, if psym is positive, you get one point so there is
;               no connecting line.  If vectorfont[i] = '',
;               then plots is called to make a symbol or a line, but if
;               vectorfont[i] is a non-null string, then xyouts is called.
;       /help = flag to print header
;       /horizontal = flag to make the legend horizontal
;       /vertical = flag to make the legend vertical (D=vertical)
;       box = flag to include/omit box around the legend (D=include)
;		  outline_color = color of box outline (D = !P.color)
;       clear = flag to clear the box area before drawing the legend
;       delimiter = embedded character(s) between symbol and text (D=none)
;       colors = array of colors for plot symbols/lines (D=!P.color)
;       font = scalar font graphics keyword (-1,0 or 1) for text
;       textcolors = array of colors for text (D=!P.color)
;       margin = margin around text measured in characters and lines
;       spacing = line spacing (D=bit more than character height)
;       pspacing = psym spacing (D=3 characters) (when number of symbols is
;             greater than 1)
;       charsize = just like !p.charsize for plot labels
;       charthick = just like !p.charthick for plot labels
;       thick = array of line thickness numbers (D = !P.thick), if used, then 
;               linestyle must also be specified
;       position = data coordinates of the /top (D) /left (D) of the legend
;       normal = use normal coordinates for position, not data
;       device = use device coordinates for position, not data
;       number = number of plot symbols to plot or length of line (D=1)
;       usersym = 2-D array of vertices, cf. usersym in IDL manual. 
;             (/USERSYM =square, default is to use existing USERSYM definition)
;       /fill = flag to fill the usersym
;       /left_legend = flag to place legend snug against left side of plot
;                 window (D)
;       /right_legend = flag to place legend snug against right side of plot
;               window.    If /right,pos=[x,y], then x is position of RHS and
;               text runs right-to-left.
;       /top_legend = flag to place legend snug against top of plot window (D)
;       /bottom = flag to place legend snug against bottom of plot window
;               /top,pos=[x,y] and /bottom,pos=[x,y] produce same positions.
;
;       If LINESTYLE, PSYM, VECTORFONT, THICK, COLORS, or TEXTCOLORS are
;       supplied as scalars, then the scalar value is set for every line or
;       symbol in the legend.
; Outputs:
;       legend to current plot device
; OPTIONAL OUTPUT KEYWORDS:
;       corners = 4-element array, like !p.position, of the normalized
;         coords for the box (even if box=0): [llx,lly,urx,ury].
;         Useful for multi-column or multi-line legends, for example,
;         to make a 2-column legend, you might do the following:
;           c1_items = ['diamond','asterisk','square']
;           c1_psym = [4,2,6]
;           c2_items = ['solid','dashed','dotted']
;           c2_line = [0,2,1]
;           legend,c1_items,psym=c1_psym,corners=c1,box=0
;           legend,c2_items,line=c2_line,corners=c2,box=0,pos=[c1[2],c1[3]]
;           c = [c1[0]<c2[0],c1[1]<c2[1],c1[2]>c2[2],c1[3]>c2[3]]
;           plots,[c[0],c[0],c[2],c[2],c[0]],[c[1],c[3],c[3],c[1],c[1]],/norm
;         Useful also to place the legend.  Here's an automatic way to place
;         the legend in the lower right corner.  The difficulty is that the
;         legend's width is unknown until it is plotted.  In this example,
;         the legend is plotted twice: the first time in the upper left, the
;         second time in the lower right.
;           legend,['1','22','333','4444'],linestyle=indgen(4),corners=corners
;                       ; BOGUS LEGEND---FIRST TIME TO REPORT CORNERS
;           xydims = [corners[2]-corners[0],corners[3]-corners[1]]
;                       ; SAVE WIDTH AND HEIGHT
;           chdim=[!d.x_ch_size/float(!d.x_size),!d.y_ch_size/float(!d.y_size)]
;                       ; DIMENSIONS OF ONE CHARACTER IN NORMALIZED COORDS
;           pos = [!x.window[1]-chdim[0]-xydims[0] $
;                       ,!y.window[0]+chdim[1]+xydims[1]]
;                       ; CALCULATE POSITION FOR LOWER RIGHT
;           plot,findgen(10)    ; SIMPLE PLOT; YOU DO WHATEVER YOU WANT HERE.
;           legend,['1','22','333','4444'],linestyle=indgen(4),pos=pos
;                       ; REDO THE LEGEND IN LOWER RIGHT CORNER
;         You can modify the pos calculation to place the legend where you
;         want.  For example to place it in the upper right:
;           pos = [!x.window[1]-chdim[0]-xydims[0],!y.window[1]-xydims[1]]
; Common blocks:
;       none
; Procedure:
;       If keyword help is set, call doc_library to print header.
;       See notes in the code.  Much of the code deals with placement of the
;       legend.  The main problem with placement is not being
;       able to sense the length of a string before it is output.  Some crude
;       approximations are used for centering.
; Restrictions:
;       Here are some things that aren't implemented.
;       - An orientation keyword would allow lines at angles in the legend.
;       - An array of usersyms would be nice---simple change.
;       - An order option to interchange symbols and text might be nice.
;       - Somebody might like double boxes, e.g., with box = 2.
;       - Another feature might be a continuous bar with ticks and text.
;       - There are no guards to avoid writing outside the plot area.
;       - There is no provision for multi-line text, e.g., '1st line!c2nd line'
;         Sensing !c would be easy, but !c isn't implemented for PostScript.
;         A better way might be to simply output the 2nd line as another item
;         but without any accompanying symbol or linestyle.  A flag to omit
;         the symbol and linestyle is linestyle[i] = -1.
;       - There is no ability to make a title line containing any of titles
;         for the legend, for the symbols, or for the text.
; Side Effects:
; Modification history:
;       write, 24-25 Aug 92, F K Knight (knight@ll.mit.edu)
;       allow omission of items or omission of both psym and linestyle, add
;         corners keyword to facilitate multi-column legends, improve place-
;         ment of symbols and text, add guards for unequal size, 26 Aug 92, FKK
;       add linestyle(i)=-1 to suppress a single symbol/line, 27 Aug 92, FKK
;       add keyword vectorfont to allow characters in the sym/line column,
;         28 Aug 92, FKK
;       add /top, /bottom, /left, /right keywords for automatic placement at
;         the four corners of the plot window.  The /right keyword forces
;         right-to-left printing of menu. 18 Jun 93, FKK
;       change default position to data coords and add normal, data, and
;         device keywords, 17 Jan 94, FKK
;       add /center keyword for positioning, but it is not precise because
;         text string lengths cannot be known in advance, 17 Jan 94, FKK
;       add interactive positioning with /position keyword, 17 Jan 94, FKK
;       allow a legend with just text, no plotting symbols.  This helps in
;         simply describing a plot or writing assumptions done, 4 Feb 94, FKK
;       added thick, symsize, and clear keyword Feb 96, W. Landsman HSTX
;               David Seed, HR Wallingford, d.seed@hrwallingford.co.uk
;       allow scalar specification of keywords, Mar 96, W. Landsman HSTX
;       added charthick keyword, June 96, W. Landsman HSTX
;       Made keyword names  left,right,top,bottom,center longer,
;                                 Aug 16, 2000, Kim Tolbert
;       Added ability to have regular text lines in addition to plot legend 
;       lines in legend.  If linestyle is -99 that item is left-justified.
;       Previously, only option for no sym/line was linestyle=-1, but then text
;       was lined up after sym/line column.    10 Oct 2000, Kim Tolbert
;       Make default value of thick = !P.thick  W. Landsman  Jan. 2001
;       Don't overwrite existing USERSYM definition  W. Landsman Mar. 2002
;	     Added outline_color BT 24 MAY 2004
;       Pass font keyword to xyouts commands.  M. Fitzgerald, Sep. 2005
;       Default spacing, pspacing should be relative to charsize. M. Perrin, July 2007
;       Don't modify position keyword  A. Kimball/ W. Landsman Jul 2007
;       Small update to Jul 2007 for /NORMAL coords.  W. Landsman Aug 2007
;       [2011.12.12] Mendez symcat
;       [2012.12.12] Mendez 
;-

;; checks each property to match and grabs a default
FUNCTION legend_check, nitems, property, default, n=nproperty
  compile_opt idl2
  on_error,2
  
  name = scope_varname(property, level=-1)
  nproperty = n_elements(property) 
  CASE nproperty OF 
    0      : p = replicate(default, nitems)
    1      : p = replicate(property, nitems)
    nitems : p = property
    ELSE   : message, 'ERROR - '+name+' property did not have 0, 1, or '+strtrim(nitems, 2)
  ENDCASE
  return, p
END


pro legend, items, $
            PSYM=psymi, $
            SYMSIZE=symsizei, $
            LINESTYLE=linestylei, $
            THICK=thicki, $
            
            COLORS = colorsi, $
            TEXTCOLORS=textcolorsi, $
            VECTORFONT=vectorfonti, $
            
            ;; polygons
            fill=filli, $ ; ok now make a square polygon
            nofilloutline=nofilloutlinei, $
            ;;fcolor is handled by color
            fline=flinei, $
            fspacing=fspacingi, $
            forientation=forientationi, $
            ocolors=ocolorsi, $ ; outline color of the things
            ;; fpattern=fpatterni
            
            ;; ??
            ;; USERSYM=usersym, $         [2012.08.17] REMOVED   

            ;; location options
            HORIZONTAL=horizontal,$
            VERTICAL=vertical, $
            TOP_LEGEND=top, $
            CENTER_LEGEND=center, $
            BOTTOM_LEGEND=bottom, $
            LEFT_LEGEND=left, $
            RIGHT_LEGEND=right, $
            POSITION=position, $
            CORNERS = corners, $
            
            ;; legned box options
            BOX = box, $
            OUTLINE_COLOR=outline_color, $
            CLEAR=clear, $
            CLRCOLOR=clrcolor, $
            MARGIN=margin, $
            
            ;; properties for all of the legend items
            PSPACING=pspacing, $
            SPACING=spacing, $
            CHARTHICK=charthick, $
            CHARSIZE = charsize, $
            NUMBER=number, $ ;; number of plot symbols
            FONT=font, $
            
            
            ;; Plot specifics
            DELIMITER=delimiter, $
            DEVICE=device, $
            DATA=data, $
            NORMAL=normal, $
            
            HELP = help
  compile_opt idl2
  on_error,2
  ;;  ------- Help
  IF keyword_set(help) OR n_elements(items) EQ 0 THEN BEGIN
    doc_library,'legend'
    return
  ENDIF

  ;;; ---- Defaults for each item.
  n = n_elements(items)
  psym       = legend_check(n, psymi, 0,  n=npsym)
  colors     = legend_check(n, colorsi, !P.color) ; modifies psym
  symsize    = legend_check(n, symsizei, 1)       ; modified psym
  textcolors = legend_check(n, textcolorsi, !P.color)
  linestyle  = legend_check(n, linestylei, 0, n=nlinestyle)
  thick      = legend_check(n, thicki, !P.thick,  n=nthick)
  vectorfont = legend_check(n, vectorfonti, '',  n=nvectorfont)
  ;; polygon stuff

  fill         = legend_check(n, filli,  0,  n=nfill)

  fline        = legend_check(n, flinei,  0,  n=nline)
  fspacing     = legend_check(n, fspacingi, 0.0)
  forientation = legend_check(n, forientationi,  0.0)
  ocolors      = legend_check(n, ocolorsi, !P.color)
  ;; fpattern     = legend_check(n, fpatterni)
  nofilloutline= legend_check(n, nofilloutlinei, 0)

  
  ;;       =====>> SET DEFAULTS FOR OTHER OPTIONS.
  ;; fill = keyword_set(fill)
  box = keyword_set(box)
  clear = keyword_set(clear)

  
  if n_elements(margin) eq 0 then margin = 0.5
  if n_elements(delimiter) eq 0 then delimiter = ''
  if n_elements(charsize) eq 0 then charsize = !p.charsize
  if n_elements(charthick) eq 0 then charthick = !p.charthick
  if charsize eq 0 then charsize = 1
  if n_elements(number) eq 0 then number = 1
  if n_elements(outline_color) EQ 0 then outline_color = !P.Color
  IF n_elements(clrcolor) EQ 0 THEN clrcolor = !P.background
  if n_elements(usersym) eq 1 then usersym = 2*[[0,0],[0,1],[1,1],[1,0],[0,0]]-1

  ;; handle text colors
  colors = djs_icolor(colors)
  textcolors = djs_icolor(textcolors)
  ocolors = djs_icolor(ocolors)
  clrcolor = djs_icolor(clrcolor)
  outline_color = djs_icolor(outline_color)
  
  ;; ---CHOOSE VERTICAL OR HORIZONTAL ORIENTATION.
  ;; default vert, 
  vertical = keyword_set(vertical)
  horizontal = keyword_set(horizontal)
  IF keyword_set(horizontal) THEN vertical = NOT horizontal

  top = keyword_set(top)
  bottom = keyword_set(bottom)
  left = keyword_set(left)
  right = keyword_set(right)
  center = keyword_set(center)
  

  ;;       =====>> INITIALIZE SPACING
  if n_elements(spacing) eq 0 then spacing = 1.2*charsize
  if n_elements(pspacing) eq 0 then pspacing = 3*charsize
  xspacing = !d.x_ch_size/float(!d.x_size) * (spacing > charsize)
  yspacing = !d.y_ch_size/float(!d.y_size) * (spacing > charsize)
  
  ;; setup some nice flags and the rest of spacing
  ltor = 1                      ; flag for left-to-right
  IF left THEN ltor = left eq 1
  IF right THEN ltor = right ne 1
  ttob = 1                      ; flag for top-to-bottom
  IF top THEN ttob = top eq 1
  IF bottom THEN ttob = bottom ne 1
  
  xalign = ltor ne 1            ; x alignment: 1 or 0
  yalign = -0.5*ttob + 1        ; y alignment: 0.5 or 1
  xsign = 2*ltor - 1            ; xspacing direction: 1 or -1
  ysign = 2*ttob - 1            ; yspacing direction: 1 or -1
  if not ttob then yspacing = -yspacing
  if not ltor then xspacing = -xspacing



  ;;       =====>> INITIALIZE POSITIONS: FIRST CALCULATE X OFFSET FOR TEXT
  xt = 0
  if vertical then begin        ; CALC OFFSET FOR TEXT START
    for i = 0, n-1 do BEGIN
      ;; some spacing :: EXPAND
      expand = (psym[i] EQ 0) ? 1 : 2
      ;; more space for vectorfont
      num = (psym[i] EQ 0 AND nvectorfont GT 0) ? (number + 1) > 3 : number
      ;; more space for connecting line
      IF psym[i] LT 0 THEN num = number > 2
      thisxt = (expand*pspacing*(num-1)*xspacing)
      xt = (ltor) ? thisxt > xt : thisxt < xt
      ;; NOW xt IS AN X OFFSET TO ALIGN ALL TEXT ENTRIES.
      ;; nexpand has the number and expansion for the items
    endfor
  endif
  

  ;;       =====>> INITIALIZE POSITIONS: SECOND LOCATE BORDER
  if !x.window[0] eq !x.window[1] then $
     plot, [0],  /nodata, xstyle=4, ystyle=4, /noerase
  
  ;;       next line takes care of weirdness with small windows
  pos = [min(!x.window),min(!y.window),max(!x.window),max(!y.window)]
  case n_elements(position) of
    0: begin
      if ltor then px = pos[0] else px = pos[2]
      if ttob then py = pos[3] else py = pos[1]
      if center then begin
        if not right and not left then px = (pos[0] + pos[2])/2. - xt
        if not top and not bottom then py = (pos[1] + pos[3])/2. + n*yspacing
      endif
      nposition = [px,py] + [xspacing,-yspacing]
    end
    1: begin                    ; interactive
      message,/inform,'Place mouse at upper left corner and click any mouse button.'
      cursor,x,y,/normal
      nposition = [x,y]
    end
    2: begin                    ; convert upper left corner to normal coordinates
      if keyword_set(data) then $
         nposition = convert_coord(position,/to_norm) $
      else if keyword_set(device) then $
         nposition = convert_coord(position,/to_norm,/device) $
      else if not keyword_set(normal) then $
         nposition = convert_coord(position,/to_norm) else nposition= position
    end
    else: message,'Position keyword can have 0, 1, or 2 elements only. '
  endcase

  yoff = 0.25*yspacing*ysign                            ; VERT. OFFSET FOR SYM/LINE.
  x0 = nposition[0] + (margin)*xspacing                 ; INITIAL X & Y POSITIONS
  y0 = nposition[1] - margin*yspacing + yalign*yspacing ; WELL, THIS WORKS!





  ;;       =====>> OUTPUT TEXT FOR LEGEND, ITEM BY ITEM.
  ;;       =====>> FOR EACH ITEM, PLACE SYM/LINE, THEN DELIMITER,
  ;;       =====>> THEN TEXT---UPDATING X & Y POSITIONS EACH TIME.
  ;;       =====>> THERE ARE A NUMBER OF EXCEPTIONS DONE WITH IF STATEMENTS.
  ;;
  polygon = (nfill NE 0 OR nline NE 0)
  symline =  (npsym NE 0 OR nlinestyle NE 0) ;; flag that we are adding a psym/line
  
  for iclr = 0,clear do begin
    y = y0                               ; STARTING Y POSITIONS
    x = x0                               ; starting x
    xend = (ltor) ? 0 : 1                ; SAVED WIDTH FOR DRAWING BOX
    ii = (ttob) ? [0,n-1,1] : [n-1,0,-1] ; set direction of to iterate over
    ii = (ttob OR ltor) ? [0,n-1,1] : [n-1,0,-1] ; set direction of to iterate over
    
    for i = ii[0],ii[1],ii[2] do BEGIN
      isvectorfont = vectorfont[i] EQ ''
      if vertical then x = x0 else y = y0 ; RESET EITHER X OR Y
      x = x + xspacing                    ; UPDATE X & Y POSITIONS
      y = y - yspacing
      
      ;; we need to plot one of the following or just going to text
      IF (nlinestyle NE 0 OR npsym NE 0 OR nvectorfont NE 0 OR nfill NE 0 OR nline NE 0) THEN BEGIN
        num = ((psym[i] eq 0) and (isvectorfont)) ? (number + 1) > 3 : number
        IF polygon THEN num = number
        if psym[i] lt 0 then num = number > 2 ; TO SHOW CONNECTING LINE
        expand = (psym[i] eq 0) ? 1 : 2
        xp = x + expand*pspacing*indgen(num)*xspacing
        yp = y + intarr(num)
        
        ;; need some space for an vector font.
        if isvectorfont then yp = yp + yoff
        
        if (psym[i] gt 0) and (num eq 1) and vertical then xp = x + xt/2.

        ;; if we are going to expose a line
        if psym[i] eq 0 then BEGIN
          ;; IF (num eq 1) and vertical then xp = x + xt/2.
          xp = [min(xp),max(xp)] ; TO EXPOSE LINESTYLES
          yp = [min(yp),max(yp)] ; DITTO
        endif
        
        ;; either we want a filled square or a square with lines
        IF fill[i] OR fline[i] THEN BEGIN
          tmp = (2*[[0,0],[0,1],[1,1],[1,0],[0,0]]-1)
          a = xp[0] + tmp[0, *]*xspacing/1.5
          b = yp[0] + tmp[1, *]*xspacing/1.5*(!D.x_vsize/float(!D.y_vsize))
          ;; some how stupidly  orient forces line_full so shit.
          IF fill[i] THEN BEGIN
            polyfill, a, b, color=colors[i], /norm
          ENDIF ELSE BEGIN
            polyfill, a, b, color=colors[i], /norm, /line_fill, $
                      spacing=fspacing[i], $
                      orientation=forientation[i]
          ENDELSE
          IF NOT nofilloutline[i] THEN $
             tmp = (n_elements(ocolorsi) EQ 0) ? colors[i] : ocolors[i]
             plots, a, b, color=tmp, /norm
        ENDIF


        if NOT isvectorfont THEN BEGIN
          xyouts, xp, yp, vectorfont[i], /norm, $
                  width=width, color=colors[i], $
                  size=charsize, align=xalign, charthick=charthick,$
                  font=font
          xt = xt > width
          xp = xp + width/2.
        endif else begin
          if symline and (linestyle[i] ge 0) AND NOT fill[i] then BEGIN
            ;; only use symcat if needed
            tmpPsym = (abs(psym[i]) GT 8) ? symcat(psym[i],  thick=thick[i]) : psym[i]
            
            IF psym[i] LT 0 AND number EQ 1 THEN BEGIN
              plots, xp, yp, /normal, $
                     color=colors[i], linestyle=linestyle[i], thick=thick[i]
              plots, [mean(xp)], [mean(yp)], /normal, $
                     color=colors[i], linestyle=linestyle[i], thick=thick[i], $
                     psym=tmpPsym, symsize=symsize[i]
              xp[1] -=  xt
              
            ENDIF ELSE BEGIN
              plots, xp, yp, /normal, $
                     color=colors[i], linestyle=linestyle[i], thick=thick[i], $
                     psym=tmpPsym, symsize=symsize[i]
            ENDELSE
          ENDIF

        endelse
        ;; if vertical then x = x + xt else if ltor then x = max(xp) else x = min(xp)

        x = (ltor) ? max(xp) : min(xp)
        IF vertical THEN x +=  xt
        
        if symline OR polygon then x = x + xspacing
      ENDIF

      ;; TEXT ONLY!!!
      ;; IF vertical AND isvectorfont and symline and (linestyle[i] eq -99) then x=x0 + xspacing

      xyouts, x, y, delimiter, width=width, /norm, $
              color=textcolors[i], size=charsize, align=xalign, $
              charthick=charthick, font=font
      x = x + width*xsign
      if width ne 0 then x = x + 0.5*xspacing
      xyouts, x, y, items[i], width=width, /norm, $
              color=textcolors[i], size=charsize, align=xalign, $
              charthick=charthick,font=font
      x = x + width*xsign
      if not vertical and (i lt (n-1)) then x = x+2*xspacing ; ADD INTER-ITEM SPACE
      xfinal = (x + xspacing*margin)
      xend = (ltor) ? xfinal > xend : xfinal < xend ; UPDATE END X
    ENDFOR



    if (iclr lt clear ) then begin
      ;;       =====>> CLEAR AREA
      x = nposition[0]
      y = nposition[1]
      bottom = (vertical) ? n : 1
      ywidth = - (2*margin+bottom-0.5)*yspacing
      corners = [x,y+ywidth,xend,y]
      polyfill, [x, xend, xend, x, x],$
                y + [0, 0, ywidth, ywidth, 0], $
                /norm, color=clrcolor
    endif else BEGIN
      ;;       =====>> OUTPUT BORDER
      x = nposition[0]
      y = nposition[1]
      bottom = (vertical) ? n : 1
      ywidth = - (2*margin+bottom-0.5)*yspacing
      corners = [x,y+ywidth,xend,y]
      if box then $
         plots, [x, xend, xend, x, x],$
                y + [0, 0, ywidth, ywidth, 0],$
                /norm, color = outline_color
               
      return
    endelse
  endfor
end



PRO legend_test
  plot, [0], /nodata
  legend, ['alpha', 'something else', 'a really long bit of text'],  $
          psym=[23, 29, 16], $
          color=['red', 'blue', 'green'], $
          /horizontal, /box, /clear,  clrcolor='grey', $
          charsize=1.4, charthick=1.2, $
          /top, /left

  legend, ['alpha', 'something else', 'a really long bit of text'],  $
          fill=[1, 0, 0], $
          fline=[0, 1, 1], $
          fspacing=[0.1, 0.06, 0.1], $
          forientation=[0, 30, 60], $
          color=['red', 'blue', 'green'], $
          /horizontal, /box, /clear,  clrcolor='light grey', $
          charsize=1.4, charthick=1.2, $
          /center, /left

  legend, ['alpha', 'something else', 'a really long bit of text'],  $
          fill=[1, 1, 0], $
          psym=[0, 0, 16], $
          fspacing=[0.1, 0.05, 0.1], $
          forientation=[0, 10, 20], $
          color=['red', 'blue', 'green'], $
          /horizontal, /box, /clear,  clrcolor='grey', $
          charsize=1.4, charthick=1.2, $
          /bottom, /left


  legend, ['1', '2', 'a really long bit of text'],  $
          /fline, /nofilloutline, $
          fspacing=0.1, forient=135+45*lindgen(3), $
          color=['red', 'blue', 'green'], $
          /box, /clear, clrcolor='yellow',$
          charsize=1.4, charthick=1.2, $
          /center, /vert




  legend, ['alpha', 'something else', 'a really long bit of text'],  $
          psym=[23, 29, 16], $
          color=['red', 'blue', 'green'], $
          /box, /clear, clrcolor='orange',$
          charsize=1.4, charthick=1.2, $
          /right, /vertical,  /top
  
  legend, ['alpha', 'something else', 'a really long bit of text'],  $
          fill=[1, 1, 0], $
          psym=[0, 0, 16], $
          color=['red', 'blue', 'green'], $
          /box, /clear, clrcolor='orange',$
          charsize=1.4, charthick=1.2, $
          /right, /vertical,  /center
  
  legend, ['alpha', 'something else', 'a really long bit of text'],  $
          fill=[0, 0, 1], $
          fline=[1, 1, 0], $
          fspacing=[0.1, 0.05, 0.1], $
          forient=[0, 45, 90], $
          ;; psym=[23, 29, 16], $
          color=['red', 'blue', 'green'], $
          /box, /clear, clrcolor='orange',$
          charsize=1.4, charthick=1.2, $
          /right, /vertical, /bottom
END


PRO legend_pstest
  psplot, 'check_legend.ps.gz',  xsize=18,  ysize=9
  legend_test
  psplot, /finish
END
