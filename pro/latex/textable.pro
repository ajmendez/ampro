; ------------------ Get Column Sizes ------------------------------------------
;   Problem with Groups, but will fix later
FUNCTION textable_colsize, data, RowNames, headerNames, groups, groupnames, seperator, groupsizes=groupsizes
  offset = (n_elements(RowNames) NE 0) ? 1 : 0
  colsizes = lonarr(n_elements(data[0,*])+offset) ; extra for name col
  ;; Header Sizes --------------------------------------------------------------
  IF (n_elements(groups) NE 0) THEN gsize = strlen(groupnames[0]) ELSE gsize=0
  IF (n_elements(headernames) NE 0) THEN hsize=strlen(headernames[0]) ELSE hsize=0
  IF (n_elements(RowNames) NE 0) THEN colsizes[0] = max([hsize, gsize, strlen(RowNames)]) + strlen(seperator)

  
  ;; Each column, get size -----------------------------------------------------
  FOR i=0,n_elements(data[0,*])-1 DO BEGIN
    base = max(strlen(data[*, i]))
    IF (n_elements(headerNames) NE 0) THEN $
       header = max(strlen(headerNames[i+offset])) ELSE $
          header = .0
    colsizes[i+offset] = max([base, header])
  ENDFOR
  IF (n_elements(groups) NE 0) THEN BEGIN
    groupsizes=lonarr(n_elements(groups))
    groupstart = 1
    for i=0, n_elements(groups)-1 do begin
      coltotal = total(colsizes[groupstart:groupstart+groups[i]-1])
      while coltotal lt (strlen(groupnames[i+1]) + strlen(seperator)) do begin
        colsizes[groupstart:groupstart+groups[i]-1]++ ; make each column larger by 1
        coltotal = total(colsizes[groupstart:groupstart+groups[i]-1])
      endwhile
      groupsizes[i] = coltotal
      groupstart += groups[i]
    endfor
  ENDIF
  
  return, colsizes
END


; ------------------ Returns the FORTRAN Format --------------------------------
function idlformatrow, numbers, seperator
    tmpstr = string(numbers, format='(("(A",(I0),")"))')
    return, '(' + strjoin(tmpstr, ',"'+seperator+'",') + ')'
end


; ------------------ Returns the Multicolumn Groups ----------------------------
function texmulticolumn, name, number
  if (n_elements(number) eq 0) then number = lonarr(n_elements(name))+1
  return, string('\multicolumn{'+string(number,format='(I0)')+'}{c}{'+name+'}',format='(A0)')
END

; ------------------ Makes the grouped headers ------------------------------------
FUNCTION texgroupheader, groupnames, groups
  return, texmulticolumn(groupnames,groups)
END

FUNCTION texheader, HeaderNames, seperator=seperator
  return, '\colhead{'+HeaderNames+'}'
END

FUNCTION texrowhead, name, number, seperator=seperator
  return, name+string(seperator, format='(A'+number_formatter((number-strlen(name))>0)+')')
END





; ------------------ LaTeX Table Maker -----------------------------------------
;   Makes a Nice Table using Number_Formatter accepts any sort of data.
;   require data
;   rownames, headernames, optional
;   if defined groups, need groupnames of same length
;   groups : an array of nColumns  to group together so group=[1,2]
;                    --> [first] , [second, third]
;   groupnames : array of names to give each group
;                    --> groupnames[0] , [groupnames[1]
;   
;   
pro textable, data, $                    ; strarr[nRows, nColumns] of the Data
              RowNames=RowNames, $       ; strarray[nRows] of the Row Name
              HeaderNames=HeaderNames, $ ; strarr[nColumns] of each colum
              RowHeader=RowHeader,$      ; str of the name of the RowName column
              groups=groups, $           ; intarray[nGroups] of nCols to group
              groupnames=groupnames, $   ; strarr[nGroups] of Group Names
              horzLine=horzLine, $       ; boolarr[nRows] adds line after T
              prepend=prepend, $         ; strarr comments added before table
              postpend=postpend, $       ; strarr comments added after table
              label=label, $             ; str the label to add to the caption
              caption=caption,  $        ; str the table caption
              notes=notes,  $            ; strarr LatexNotes \tablenotetext{a}{}
              filename=filename, $       ; Full path to write table
              fullsize=fullsize          ; keyword : use deluxetable*
  
  indent='  '
  lineend = ' \\'
  hline = '\hline'
  seperator = ' & '
  headerbracket = ['\tablehead{','  }'] ; including spaces to match lineend
  columnbracket = ['\colhead{','}']
  enviromentname = (keyword_set(fullsize)) ? 'deluxetable*' : 'deluxetable'
  
  
  IF (N_elements(groups) NE 0) THEN BEGIN
    GH  = texmulticolumn(groupnames,groups)
    IF (n_elements(headernames) EQ 0) THEN endwith = headerbracket[1] ELSE endwith= ''
    GH[n_elements(GH)-1] += endwith
    groupstrings = [headerbracket[0], GH]

  ENDIF
  IF (n_elements(headernames) NE 0) THEN BEGIN
    IF (n_elements(groups) EQ 0) THEN startwith = headerbracket[0] ELSE startwith = ''
    NH =  columnbracket[0]+HeaderNames+columnbracket[1]
    NH[n_elements(NH)-1] += headerbracket[1]
    IF (n_elements(rownames) NE 0) THEN BEGIN
      IF (n_elements(rowheader) NE 0) THEN BEGIN
        startwith = startwith + number_formatter(rowheader)
      ENDIF
      headerstrings = [startwith, NH] 
    ENDIF ELSE BEGIN
      NH[0] = headerbracket[0] + NH[0]
      headerstrings = NH
    ENDELSE
  ENDIF
  datastrings = number_formatter(data)
  colsizes = textable_colsize(datastrings, RowNames, headerstrings, $
                              groups, groupstrings, seperator, groupsizes=groupsizes)
  IF (n_elements(rownames) NE 0) THEN BEGIN
    DataFormat = idlformatrow(colsizes[1:*], seperator)
    colsizes[n_elements(colsizes)-1] += strlen(lineend)
    HeaderFormat = idlformatrow(colsizes[1:*], seperator)
    NameFormat = idlformatrow(colsizes[0], seperator)
  ENDIF ELSE BEGIN
    DataFormat = idlformatrow(colsizes, seperator)
    colsizes[n_elements(colsizes)-1] += strlen(lineend)
    headerFormat = idlformatrow(colsizes, seperator)
  ENDELSE
    
  
  
  ; ------------------ Determine number of Coumn Lines -----------------------
  tmpstr = (n_elements(RowNames) NE 0) ? 'l' : ''
  if (n_elements(groups) ne 0) then begin
    for k=0, n_elements(groups)-1 do tmpstr+=strjoin(replicate('r',groups[k]))
  endif else begin
    tmpstr = tmpstr + strjoin(replicate('r',n_elements(data[0,*])))
  ENDELSE
  colnum = n_elements(data[0,*])
  IF (n_elements(rownames) NE 0) THEN colnum++




  ; Start Printing some fancy tables -------------------------------------------
  IF (n_elements(filename) GT 0) THEN splog, /noname, filename=filename
  FOR i=0, n_elements(prepend)-1 DO $
     splog, /noname, '%'+prepend[i]
  splog, /noname, '\begin{'+enviromentname+'}{'+tmpstr+'}  % Generated by textable.pro: '+systime()
  splog, /noname, indent + '\tablecolumns{'+number_formatter(colnum)+'}'
  splog, /noname, indent + '\tablewidth{0pt}'
  dlabel = (n_elements(label) NE 0) ? ' \label{table:'+label+'}' : ''
  dcaption = (n_elements(caption) NE 0) ? '\tablecaption{'+caption+dlabel+'}' : ''
  IF dcaption NE '' THEN $
     splog, /noname, indent + '\tablecaption{'+caption+dlabel+'}'




  ; ------------------ Add Groups, and Headers if there ----------------------
  if (n_elements(groups) ne 0) then BEGIN
    groupformat = idlformatrow(groupsizes+((groups-1)>0)*strlen(seperator), seperator)
    tmpstr = texrowhead(groupstrings[0], colsizes[0], seperator=seperator)
    groupstr = groupstrings[1:*]
    IF (n_elements(headernames) NE 0) THEN tmpstr2 = lineend ELSE tmpstr2 = ''
    splog, /noname, indent + tmpstr + string(groupstr, format=groupformat) + tmpstr2
  endif
  if (n_elements(headernames) ne 0) then BEGIN
    ;; this is broken for things without rownames. FIXME
    IF (n_elements(rownames) NE 0) THEN BEGIN
      tmpstr = texrowhead(headerstrings[0], colsizes[0], seperator=seperator) 
      headerstr = headerstrings[1:*]
      splog, /noname, indent + tmpstr + string(headerstr, format=HeaderFormat)
    ENDIF ELSE BEGIN
      splog, /noname, indent + string(headerstrings, format=HeaderFormat)
    ENDELSE
  ENDIF

  
  ; ------------------ Print the Data ----------------------------------------
  splog, /noname, indent+'\startdata'
  for i=0, n_elements(data[*,0])-1 do begin
    ;; if (n_elements(rownames) ne 0) then $
    ;;    tmpstr = string(rownames[i]+seperator, format=NameFormat) else tmpstr=''
    tmpstr =  ( n_elements(rownames) NE 0 ) ? string(rownames[i]+seperator, format=NameFormat) : ''
    lineending = (i LT n_elements(data[*, 0])-1) ? lineend : '   '
    splog, /noname, indent + tmpstr + string(number_formatter(data[i,*]),format=dataformat) + lineending
    ;; if horzline array is defined then add a hline after the row.
    IF n_elements(horzLine) GT 0 && horzline[i] EQ 1 THEN $
       splog, /noname, indent+hline
  endfor
  splog, /noname, indent+'\enddata'
  FOR i=0, n_elements(notes)-1 DO $
     splog, /noname, indent+ notes[i]
  splog, /noname, '\end{'+enviromentname+'}'
  ; ------------------ Anything else? ----------------------------------------
  FOR i=0, n_elements(postpend)-1 DO $
     splog, /noname, '%'+postpend[i]
  
  IF (n_elements(filename) NE 0) THEN splog, /noname, /close
  print
END



