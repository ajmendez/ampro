;+
; NAME:
;   AM_MWRFITS
;
; PURPOSE:
;   Simple wrapper on MWRFITS with some bells and whistles. -- with
;   some modifications by Alex.  if outfile has .gz in the name assume
;   that you probbabily wanted it all gziped up
;
; INPUTS: 
;   outstruct - output structure of image to write out
;   outfile - output file name
;
; OPTIONAL INPUTS: 
;   hdr - output FITS-style header
;
; KEYWORD PARAMETERS: 
;   silent - suppress messages to STDOUT
;   append - append to an existing structure (default is to use
;     /CREATE) 
;   clobber - overwrite existing output file, otherwise exit
;
; OUTPUTS: 
;   Writes out a FITS file.
;
; MODIFICATION HISTORY:
;   J. Moustakas, 2009 Mar 18, NYU
;   jm09nov10ucsd - added /CLOBBER keyword
;   A. Mendez, 2013, May, 13, UCSD
;
; Copyright (C) 2009, John Moustakas
; 
; This program is free software; you can redistribute it and/or modify 
; it under the terms of the GNU General Public License as published by 
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
; 
; This program is distributed in the hope that it will be useful, but 
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details. 
;-

pro am_mwrfits, outstruct, filename, hdr, $
                silent=silent, $
                append=append, $
                clobber=clobber
  on_error, 1
  outfile = repstr(filename, '.gz')
  gzip = strmatch(filename, '*.gz')
  
  IF keyword_set(append) THEN BEGIN
    IF (file_test(outfile) EQ 0) THEN $
       message, 'Output file '+outfile+' does not exist!'
    IF (keyword_set(silent) EQ 0) THEN splog, 'Appending to ' + filename
    mwrfits, outstruct, outfile, hdr, /silent
  ENDIF ELSE BEGIN
    IF file_test(outfile) AND NOT keyword_set(clobber) THEN $
       message, 'File already exists [/clobber?]: '+outfile
    IF (keyword_set(silent) EQ 0) THEN splog, 'Writing '+ filename
    mwrfits, outstruct, outfile, hdr, /create
  ENDELSE
  
  IF (keyword_set(gzip)) THEN spawn, 'gzip -f '+outfile, /sh
end
    
