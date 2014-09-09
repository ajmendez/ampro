;+
; NAME:
; 
;	LS
;	
; CATEGORY:
; 
;       Stupid little convenience routines.
;   
; PURPOSE:
; 
;	List the contents of the current directory, like the Unix 'ls'
;       command.
;	
; CALLING SEQUENCE:
; 
;       LS[,NAME]
;
;       NAME - An optional string specifying the names of the files to
;              be listed.  Wild cards are allowed. For example,
;              ls,'*.pro' will list all files ending in .pro.
;
; MODIFICATION HISTORY:
;
;       David L. Windt, Bell Labs, November 1989
;       windt@bell-labs.com
;
;       February, 1998 - Now works under Windows and MacOS, making use
;                        of FINDFILE.  But the old DIR keyword is gone.
;       
;-
pro ls,name,dir=dir
on_error,2
if (n_params() eq 0) then begin
    if (!version.os_family eq 'unix') then name='' else name='*'
endif

if !version.os_family eq 'unix' then begin
    command='ls '+name
    spawn,command
endif else begin
    ff=findfile(name)
    more,ff
endelse
end