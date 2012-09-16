pro pp_dos2unix,file,inverse=inv
;
;+
;pro pp_dos2unix,file,inv=inv
;Similar to the console application dos2unix, converts the newlines of a text file between
;CR+LF (Windows) and LF (Linux).
;file is a string with the file name, which gets overwritten
;If inverse is set, writes the file with CR+LF instead of just LF
;Written by Paulo Penteado (pp.penteado@gmail.com), Aug/2009
;-
;
inv=n_elements(inv) eq 1 ? inv : 0 ;default
;read the file into an array, which does not contain newlines
nlines=file_lines(file)
lines=strarr(nlines)
openr,unit,file,/get_lun
readf,unit,lines
free_lun,unit
;add the proper newline to each line
lines=inv ? lines+string(13B)+string(10B) : lines+string(10B)
;write the file, using unformatted write so that no newlines are created after each line
openw,unit,file,/get_lun
writeu,unit,lines
free_lun,unit
end
