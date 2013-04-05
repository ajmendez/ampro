;; makes a position 4 vector such that it has been positioned
;; position2 within position1 ... inception
FUNCTION subsubplot, pos1, pos2
  newpos = [pos1[0] + (pos1[2]-pos1[0])*pos2[0], $
            pos1[1] + (pos1[3]-pos1[1])*pos2[1], $
            pos1[0] + (pos1[2]-pos1[0])*pos2[2], $
            pos1[1] + (pos1[3]-pos1[1])*pos2[3] ]
  return, newpos
END

