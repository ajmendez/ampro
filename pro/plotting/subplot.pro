function subplot, row, col, N, $
                  GAP=gap, XGAP=xgap, YGAP=ygap, EGAP=eGAP, $
                  XOFFSET=oxoffset, YOFFSET=oyoffset,  $
                  defaults=defaults
  IF keyword_set(defaults) THEN BEGIN
    IF n_elements(defaults) NE 4 THEN $
       defaults = [0, 0, 1, 1]
  ENDIF
  IF n_elements(defaults) EQ 0 THEN $
     defaults = [0.05, $ ; x start
                 0.05, $ ; y start
                 0.91, $ ; x end
                 0.92  ] ; y end
  ;; N starts with 0
  IF n_elements(n) EQ 0 THEN n = 0
  ;; N is the array of numbers for the plot locations
  if n_elements(GAP) eq 0 then begin
    if n_elements(xgap) eq 0 then xgap = 0d
    if n_elements(ygap) eq 0 then ygap = 0d
  endif ELSE begin
    if n_elements(xgap) eq 0 then xgap = double(gap)
    if n_elements(ygap) eq 0 then ygap = double(gap)
  endelse
  if n_elements(egap) eq 0 then egap = 0d
  if n_elements(xgap) eq 0 then xgap = 0d
  if n_elements(ygap) eq 0 then ygap = 0d
  
  if n_elements(egap) eq 1 then egap=[egap, egap]
  if n_elements(xgap) eq 1 then xgap=[xgap, xgap]
  if n_elements(ygap) eq 1 then ygap=[ygap, ygap]
  
  
  if n_elements(oxoffset) eq 0 then xoffset = 0d ELSE xoffset = oxoffset
  xoffset = xoffset + double(defaults[0]) + double(egap[0])/2d ; redefine zero
  if n_elements(oyoffset) eq 0 then yoffset = 0d ELSE yoffset = oyoffset
  yoffset = yoffset - double(defaults[1]) - double(egap[1])/2d
  
  RowLeft = min(N MOD col)
  RowRight = max(N MOD col)
  Nbar = (row*col-1) - N 
  ColTop = max( (Nbar-(Nbar MOD col))/col )
  ColBot = min( (Nbar-(Nbar MOD col))/col )
  RowPercent = (double(defaults[2]) - double(egap[1]))/double(row)
  ColPercent = (double(defaults[3]) - double(egap[0]))/double(col)
  

  X0Location = double(RowLeft)*(ColPercent) + $
               double(RowRight-RowLeft+1)*ColPercent*xgap[0]/2d
  Y0Location = double(Colbot)*(RowPercent) + $
               double(ColTop-ColBot+1)*RowPercent*ygap[0]/2d
  X1Location = double(RowRight+1)*(ColPercent) - $
               double(RowRight-RowLeft+1)*ColPercent*xgap[1]/2d
  Y1Location = double(ColTop+1)*(RowPercent) - $
               double(coltop-colbot+1)*RowPercent*ygap[1]/2d

  return, [X0Location + xoffset, $
           Y0Location - yoffset, $
           X1Location + xoffset, $
           Y1Location - yoffset]
end


pro subplot_info, row, col
  print,indgen(col, row)
end

pro subplot_box, Rows, Columns, N, $
                 BORDERCOLOR=bordercolor, FILLCOLOR=fillcolor, $
                 BOX=box, _extra=extra
  Box = subplot(Rows,Columns, N, _extra=extra)
  if n_elements(fillcolor) ne 0 THEN $
     polyfill, [Box[0], Box[2], Box[2], Box[0], Box[0]], $
               [Box[1], Box[1], Box[3], Box[3], Box[1]], color=FillColor,/normal
  if n_elements(BorderColor) ne 0 THEN $
     plots,  [Box[0], Box[2], Box[2], Box[0], Box[0]], $
             [Box[1], Box[1], Box[3], Box[3], Box[1]], color=djs_icolor('black'),/normal
end

pro subplot_grid, Rows, Columns, EGAP=egap, GAP=gap
  if not keyword_set(egap) then  egap = 0.0d
  if not keyword_set(gap) then gap = 0.0d
  print, 'Example Grid with Rows: ' + number_formatter(Rows) + $
         ' and Columns: ' + number_formatter(Columns)
  print, '  With EdgeGap = ' + number_formatter(egap) + ' = ' + $
         number_formatter(100d*egap) + '% of plot '
  print, '  With xgap = ygap = gap = ' + number_formatter(gap) + ' = ' +  $
         number_formatter(100d*gap) + '% of box'  
  erase
  for j=0,Rows*Columns-1 do begin
    subplot_box, Rows, Columns, [j], $
                 BorderColor=djs_icolor('red'), $
                 fillcolor=djs_icolor('grey'), box=box, gap=gap, egap=egap
    ;; plot, findgen(2),  position=box, /noerase, title='xxxXXXXx'
    xyouts,mean([box[0],box[2]]), mean([box[1],box[3]]), $
           number_formatter(j), alignment=0.5, color=djs_icolor('black'),/normal
	endfor
end
