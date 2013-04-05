;+
; NAME: 
;     venn
;
; PURPOSE:
;     Make a venn diagram
;
; CALLING SEQUENCE:
;     venn, A, I
;
; INPUTS:
;     A -- Area vector [A1,A2,A3]
;     I -- Intersection vector [I12 I13 I23 I123]
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;  ??? converted from matlab land
;  [2011.11.15] Mendez Modified this.
;
;
;-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Venn Rotation function,  probably could have used rot, but this is an
;; afterthought of a madman.
;;  A is the input x,y
;;  angle is the angle in degrees
FUNCTION venn_rotate, A,  angle
  B = dblarr(2)
  B[0] = A[0]*Cos(angle*!DtoR) - A[1]*Sin(angle*!DtoR)
  B[1] = A[0]*Sin(angle*!DtoR) + A[1]*Cos(angle*!DtoR)
  return,  B
END


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plotting routine for the venn diagram. It does a decent job, but
;; should should generally specity xr/yr to be sure that it is square.
;;   oy : circle x array
;;   oy : circle y array
;;   r  : circle radius array
;;
;; /overplot to overplot on a set of axes
;; orientations=[deg_1, deg_2, deg_3] for the /line_fill orientations
;; /fill, /outline to fill and outline things
;; _extra for plot/ellipse/oplot commands
pro plot_venn, ox, oy, R, $
               offset=offset, $
               overplot=overplot, $
               orientations=orientations, $
               colors=colors, ocolors=outlinecolors, $
               rotate=rotate, $
               xr=xr, yr=yr, $
               outline=outline, fill=fill, _extra=extra
  IF n_elements(offset) EQ 0 THEN offset = [0, 0.]
  x = ox+offset[0]
  y = oy+offset[1]

  IF n_elements(rotate) GT 0 THEN BEGIN
    FOR i=0, n_elements(x)-1 DO BEGIN
      tmp = venn_rotate([X[i], Y[i]], rotate)
      X[i] = tmp[0]
      Y[i] = tmp[1]
    ENDFOR
  ENDIF

  ; xr = minmax([x-r, x+r])
  ; yr = minmax([y-r, y+r])
  IF n_elements(xr) EQ 0 THEN $
     xr = embiggen(minmax([x-r, x+r, y-r, y+r]), 0.1)
  IF n_elements(yr) EQ 0 THEN $
     yr=xr

  IF NOT keyword_set(overplot) THEN $
     plot, [0], [0], xs=4, ys=4, xr=xr, yr=yr, _extra=extra

  ;; print,  xr
  
  IF n_elements(orientations) EQ 1 THEN $
     orientations = replicate(orientations, n_elements(x))
  IF n_elements(colors) EQ 1 THEN $
     colors = replicate(colors, n_elements(x))
  IF n_elements(outlinecolors) EQ 1 THEN $
     outlinecolors = replicate(colors, n_elements(x))

  nplots = (keyword_set(outline))? 1 : 0
  FOR j=0, nplots DO BEGIN
    FOR i=0,n_elements(x)-1 DO BEGIN
      IF n_elements(orientations) GT 0 THEN $
         orient = orientations[i]
      IF j AND n_elements(outlinecolors) GT 0 THEN $
         color = outlinecolors[i] ELSE $
            color = (n_elements(colors) GT 0) ? colors[i] : 'black'
      ellipse,R[i], R[i], 0,0,360, x[i], y[i], $
              fill=keyword_set(fill)-j > 0, $
              orient=orient, $
              color=djs_icolor(color), /data, _extra=extra
    ENDFOR
  ENDFOR
end







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Area of intersection of two circles with radius r1, and r2, and distance d 
;;  between them.
function areaIntersect2Circ, r1, r2, d
  alpha = 2*acos( (d^2 + r1^2 - r2^2) / (2*r1*d) );
  beta  = 2*acos( (d^2 + r2^2 - r1^2) / (2*r2*d) );
  A = (0.5)*( r1^2 * (alpha - sin(alpha)) + r2^2 * (beta - sin(beta)) )
  return, A
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns an estimate of the distance between two circles with radii Ra and Rb
;;  with an area of intersection I
function circPairDist, rA, rB, I
  D = makebin(500,1.1*[0,rA+rB])
  intersectArea = areaIntersect2Circ(rA,rB,D)
  ii = where(finite(intersectArea) EQ 1, nii)
  IF nii LE 0 THEN message, 'intersected area not found, check inputs'
  distance = interpol(D[ii], intersectArea[ii], I)
  return, distance
  ;; dA = abs(I-intersectArea)/double(I)
  ;; Probably should replace the below with value_locate
  ;; ii = where(da lt 0.001 AND finite(da) EQ 1, nii)
  ;; if nii gt 0 then return, median(D[ii]) else return, rA+rB
end









;; Calculate the points of intersection of three circles Adapted from Ref. [2]
;; d = [d12 d13 d23]
;; x = [x12; x13; x23]
;; y = [y12; y13; y23]
;;    Symbol    Meaning
;;      T         theta
;;      p         prime
;;      pp        double prime
function intersect3C, r, d
    x = dblarr(3)
    y = dblarr(3)
    NaN = !VALUES.D_NAN
    sinTp = NaN
    cosTp = NaN
    
    
    ;; Step 1. Check whether circles 1 and 2 intersect by testing d[0]
    ; if not ( ((r[0]-r[1]) lt d[0]) and  $
    ;          ((r[0]+r[1]) gt d[0]) ) then begin
    ;     x = NaN*dblarr(3)
    ;     y = NaN*dblarr(3)
    ;     return, [x, y, sinTp, cosTp]
    ; endif

    ;; Step 2. Calculate the coordinates of the relevant intersection point of circles 1 and 2:
    x[0] = (r[0]^2 - r[1]^2 + d[0]^2)/(2*d[0]);
    y[0] = 0.5/d[0] * sqrt( 2*d[0]^2*(r[0]^2 + r[1]^2) - (r[0]^2 - r[1]^2)^2 - d[0]^4 )

    ;; Step 3. Calculate the values of the sines and cosines of the angles tp and tpp:
    cosTp  =  (d[0]^2 + d[1]^2 - d[2]^2) / (2 * d[0] * d[1])
    cosTpp = -(d[0]^2 + d[2]^2 - d[1]^2) / (2 * d[0] * d[2])
    sinTp  =  (sqrt(1 - cosTp^2))
    sinTpp =  (sqrt(1 - cosTpp^2))

    ;; Step 4. Check that circle 3 is placed so as to form a circular triangle.
    cond1 = (x[0] - d[1]*cosTp)^2 + (y[0] - d[1]*sinTp)^2 lt r[2]^2
    cond2 = (x[0] - d[1]*cosTp)^2 + (y[0] + d[1]*sinTp)^2 gt r[2]^2
    if  ~(cond1 && cond2) then begin
        x = NaN*dblarr(3)
        y = NaN*dblarr(3)
        return, [x, y, sinTp, cosTp]
    endif

    ;; Step 5: Calculate the values of the coordinates of the relevant intersection points involving
    ;;  circle 3
    xp13  =  (r[0]^2 - r[2]^2 + d[1]^2) / (2 * d[1])
    yp13  = -0.5 / d[1] * sqrt( 2 * d[1]^2 * (r[0]^2 + r[2]^2) - (r[0]^2 - r[2]^2)^2 - d[1]^4 )

    x[1]   =  xp13*cosTp - yp13*sinTp
    y[1]   =  xp13*sinTp + yp13*cosTp

    xpp23 =  (r[1]^2 - r[2]^2 + d[2]^2) / (2 * d[2])
    ypp23 =  0.5 / d[2] * sqrt( 2 * d[2]^2 * (r[1]^2 + r[2]^2) - (r[1]^2 - r[2]^2)^2 - d[2]^4 )

    x[2] = xpp23*cosTpp - ypp23*sinTpp + d[0]
    y[2] = xpp23*sinTpp + ypp23*cosTpp

    return, [x, y, sinTp, cosTp]
end


;; Area of common intersection of three circles This algorithm is taken from [2].
;;  Symbol    Meaning
;;    T         theta
;;    p         prime
;;    pp        double prime
function areaIntersect3Circ, r, d 
    ; [r1 r2 r3] = deal(r[0], r[1], r[2]);
    ; [d12 d13 d23] = deal(d[0], d[1], d[2]);
    A=0d
    x=0d
    y=0d
    c=0d
    trngArea = 0d
    
    ;; Intersection points
    tmp = intersect3C(r,d);
    x = tmp[0:2]
    y = tmp[3:5]
    sinTp = tmp[6]
    cosTp = tmp[7]
    
    ;; if max(finite(x, /nan)) eq 1 or max(finite(y, /nan)) EQ 1 then begin
    if min(finite(x)) eq 0 or min(finite(y)) EQ 0 then begin
        A = 0 ;; No three circle intersection
        return, [A, x, y, c, trngArea]
    end
    
    ;; Step 6. Use the coordinates of the intersection points to calculate the chord lengths c1,
    ;;  c2, c3:
    i1 = [0, 0, 1];
    i2 = [1, 2, 2];
    c = sqrt((x[i1]-x[i2])^2 + (y[i1]-y[i2])^2)

    ;; Step 7: Check whether more than half of circle 3 is included in the circular triangle, so
    ;;  as to choose the correct expression for the area
    lhs = d[1] * sinTp;
    rhs = y[1] + (y[2] - y[1])/(x[2] - x[1])*(d[1]*cosTp - x[1]);
    sign = (lhs lt rhs)? [-1, -1, 1] : [-1, -1, -1]
    
    ;; Calculate the area of the three circular segments.
    ca = (r^2)*asin((c/2)/r) + ((sign*c)/4.)*sqrt(4*r^2 - c^2);

    trngArea = (1/4.)*sqrt( (c[0]+c[1]+c[2])*(c[1]+c[2]-c[0])*(c[0]+c[2]-c[1])*(c[0]+c[1]-c[2]) );
    A = trngArea + total(ca);
    
    return, [A, x, y, c, trngArea]
end


;; From locations pos, calculate the differences in their area.
;; NEEDS I, I0R,D,
function threeCircleAreaError, pos;, IX=I, I0=I0, D=D, R=R
  common venn, Venn_I0, Venn_I, Venn_D, Venn_R
    I0 = Venn_I0 & I = Venn_I & D = Venn_D & R = Venn_R
    
    x3 = pos[0]
    y3 = pos[1]
    
    ;; Calculate distances
    d[1] = sqrt(x3^2 + y3^2)        ; %d13
    d[2] = sqrt((x3-d[0])^2 + y3^2) ; %d23
    
    ;; Calculate intersections
    ;;   Note: were only moving the third circle, so I12 is not changing
    I[1:2] = areaIntersect2Circ(r[0:1], r[[2,2]], d[1:2])   ; I13 and I23
    ;; I[3]  = (areaIntersect3Circ(r, d))[0]                   ; I123
    tmp    = areaIntersect3Circ(r, d)                       ; I123
    I[3] = tmp[0] ; removed [2011.11.15]
    
    ;; Replace 0 (no intersection) with infinite error
    ;; ii = where(I eq 0, nii)
    ;; if nii ne 0 then  I[ii] = !Values.D_infinity
    ;; [2011.11.15] added back in

    
    ;; Error
    err = total(abs((I-I0)/I0));
    
    Venn_I = I & Venn_D = D & Venn_R = R
    return, err
end


PRO venn_debug, A, I, vennequalfix=vennequalfix, silent=silent, _extra=extra
  IF NOT keyword_set(vennequalfix) THEN return
  
  comp = { a: {ind:0, comp:[0, 1]}, $
           b: {ind:1, comp:[0, 2]}, $
           c: {ind:2, comp:[1, 2]}, $
           d: {ind:3, comp:[0, 1, 2]} }
  FOR j=0, n_tags(comp)-1 DO BEGIN
    IF min(a[comp.(j).comp]) LE i[comp.(j).ind] THEN BEGIN
      IF NOT keyword_set(silent) THEN $
         splog, 'Issue with exactly same numbers in intersection, lowering I['+$
                number_formatter(comp.(j).ind)+']='+$
                number_formatter(I[comp.(j).ind])+$
                ' by 0.5 % (multiplying by 0.995) -- '+$
                'This is a visualization tool, small errors, and human perception errors are expected.'
      I[comp.(j).ind] *= 0.995
    ENDIF
  ENDFOR
END

    




pro venn, Ain, Iin, x=x, y=y, r=r,  $
          silent=silent, noplot=noplot, $
          ignoreerror=ignoreerror, $
          _extra=extra
  A0 = Ain
  I0 = Iin
  if n_elements(A0) ne 3 and n_elements(I0) ne 4 then $
     message, "Support only for 3 intersecting circles."
  
  ;; [2011.11.15] testing edge cases
  venn_debug, A0, I0, silent=silent, _extra=extra


  nCirc = n_elements(A0)
  
  x = dblarr(nCirc)
  y = dblarr(nCirc)
  d = dblarr(nCirc)
  I = dblarr(4)
  A = dblarr(nCirc)
  Z = dblarr(7)

  ;; Do the 2 circle pair.
  r = sqrt(A0/!PI)
  d[0] = circPairDist(r[0], r[1], I0[0])
  x[1] = d[0]
  I[0] = areaIntersect2Circ(r[0], r[1], d[0]);
  
  ;; Pairwise distances for remaining pairs 1&3 and 2&3
  d[1] = circPairDist(r[0], r[2], I0[1]); %d13
  d[2] = circPairDist(r[1], r[2], I0[2]); %d23
  
  ;; Check triangle inequality
  ; Ds = d[sort(d)]
  ; if Ds[n_elements(Ds)-1] gt Ds[0] + Ds[1] then message, 'Triangle inequality not satisfied'
  
  ;; Guess the initial position of the third circle using the law of cosines
  alpha = acos( (d[0]^2 + d[1]^2 - d[2]^2)  / (2 * d[0] * d[1]) )
  x[2] = d[1]*cos(alpha)
  y[2] = d[1]*sin(alpha)

  ;; Minimize total intersection area error by moving the third circle
  ; pos = fminsearch(@threeCircleAreaError, [x[2] y[2]]);
  
  ; fa = {IX:I, I0:I0, R:R, D:D}
  common venn, Venn_I0, Venn_I, Venn_D, Venn_R
  Venn_I0 = I0 & Venn_I = I & Venn_D = D & Venn_R = R
  pos = mpfit('threeCircleAreaError', [x[2],y[2]], status=s);, functargs=fa)
  x[2] = pos[0]
  y[2] = pos[1]
  I = Venn_I & D = Venn_D & R = Venn_R
  
  I[3] = (areaIntersect3Circ(D, R))[0]

  ; print, 'status: ',s
  IF NOT keyword_Set(silent) THEN BEGIN 
    splog, 'A: ', double(!PI*Venn_R^2)
    splog, 'A0:', double(A0)
    splog, 'I: ', double(I)
    splog, 'I0:', double(I0)
    splog 
  ENDIF

  IF ( min(finite(I)) EQ 0 OR min(finite(A)) EQ 0 OR $
       min(finite(x)) EQ 0 OR min(finite(y)) EQ 0 ) AND $
     ( NOT keyword_set(ignoreerror) ) THEN $
        message, 'Non-finite solution found, probably non-physical input parameters, please check'


  IF NOT keyword_set(noplot) THEN $
     plot_venn, x,y,venn_R,  _extra=extra
  
end


    


pro test_venn,  _extra=extra
  ;; A = double([350,300,275])
  ;; I = double([100,80,60,20])
  ;; venn, A, I
  
  A = double([2,1.5,1])
  I = double([.9,.8,.6,.4])
  venn, A, I,  _extra=extra
end
