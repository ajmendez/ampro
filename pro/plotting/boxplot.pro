pro boxplot, xrange, yrange, fillcolor=fillcolor, bordercolor=bordercolor
  xr = xrange[sort(xrange)]
  yr = yrange[sort(yrange)]
  box = [xr[0],yr[0],xr[1],yr[1]]
  print, box
  if n_elements(fillcolor) ne 0 then $
    polyfill, [Box[0], Box[2], Box[2], Box[0], Box[0]], $
              [Box[1], Box[1], Box[3], Box[3], Box[1]], $
              color=djs_icolor(FillColor)
  if n_elements(bordercolor) ne 0 then $
    plots,  [Box[0], Box[2], Box[2], Box[0], Box[0]], $
            [Box[1], Box[1], Box[3], Box[3], Box[1]], $
              color=djs_icolor(bordercolor)
end

    