pro path
p=strsplit(!PATH,':', /extract)
for i=0,n_elements(p)-1 do  print,i,p[i]
end