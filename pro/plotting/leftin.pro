;From some Range, and P=Percent, allows one to return X-Data location so that
; it is some percent from the left
;  Can run it by  lefin(range,p=<% of screen>),  or leftin(% of screen)
function leftin, range, percent=percent
    if n_elements(percent) eq 0 then percent = 0.1
    if n_elements(range) eq 1 then percent = range
    if n_elements(range) eq 1 then range = !X.crange
    
    if range[0] lt range[1] then sign = 1 else sign=-1
    return, range[0] + percent*sign*abs(range[1]-range[0])
end