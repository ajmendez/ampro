;From some Range, and P=Percent, allows one to return Y-Data location so that
; it is some percent from the top.
;  Can run it by  topin(range,p=<% of screen>),  or topin(% of screen)
function topin, range, percent=percent
    if n_elements(percent) eq 0 then percent = 0.1
    if n_elements(range) eq 1 then percent = range
    if n_elements(range) eq 1 then range = !Y.crange
    
    if range[0] lt range[1] then sign = 1 else sign=-1
    return, range[1] - percent*sign*abs(range[1]-range[0])
end