

PRO _read, unit, splog=splog, filename=filename, name=name
  IF keyword_set(filename) THEN $
     openr, unit, filename, /get_lun
  IF n_elements(name) EQ 0 THEN name = ''
  
  x =  ' '
  WHILE ~EOF(unit) DO BEGIN
    readf, unit, x
    IF keyword_set(splog) THEN $
       splog, x ELSE $
          print, name, x
  ENDWHILE

  IF keyword_set(filename) THEN free_lun, unit
END


PRO am_spawn, arg, quiet=quiet
  on_error, 2
  
  command = arg
  IF keyword_set(noerr) THEN command += ' 2>&1 ' 
  IF keyword_set(quiet) THEN command += ' > /dev/null '
  
  spawn, command, pid=pid, unit=unit, exit_status=exit_status
  
  IF NOT keyword_set(quiet) THEN BEGIN
    ;; Determine a nice name to show the user
    help,  calls=calls
    fname = (str_sep(calls[1], ' '))[0]+': '
    FOR i=0, n_elements(calls)-3 DO fname =  ' ' + fname

    ;; update the data to the user
    print
    splog, 'Running ['+number_formatter(pid)+'] : ', command    
    _read, unit, name=fname
  ENDIF
  
  ;; grab the exit_status
  free_lun, unit,  exit_status=exit_status
  
  IF exit_status NE 0 THEN BEGIN
    splog, command, ' :: '
    splog, ' Failed with exit code: ', exit_status, prelog=''
    message, 'Failed to run : '+command
  ENDIF
     
   
END


PRO _check
  am_spawn,'snap ~/Desktop/tmp.ply ~/Desktop/tmp2.ply'
END
  

PRO check_spawn
  FOR i=0, 100 DO BEGIN
    print, i
    am_spawn, 'ls'
  ENDFOR
  

  ;; am_spawn, 'ls'
  ;; am_spawn, 'ls', /quiet
  ;; am_spawn, 'ls -lah'
  ;; am_check
  ;; am_spawn, 'cd psdifn'
  
END