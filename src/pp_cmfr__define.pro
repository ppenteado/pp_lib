; docformat = 'rst'
;+
;    Object to read and interpret OBSFLUX-type files created by cmfgen:
;    
;    Uses pp_cmfread to do the low level reading and parsing,
;    and returns a structure with the names and values read (see that function for details).
;    pp_cmfread is more general, works with any file with a
;    format like OBSFLUX or obs_fin (see the function for details).
;    
;    The object pp_cmfr uses pp_cmfread, then tries to interpret the variable names
;    to match with them with the physical quantities expected to be in the file.
;     
;    The interesting methods are getall, and getvariable (see below).
;    
; :Uses: pp_cmfread
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Aug/2009
;-


;+
; :Description:
;    Returns a structure containing all the 6 scalars and 9 vectors read from the object (NaN for those not found).
;
; :Keywords:
;    desc: out, optional
;      A string array with the pairs of tag names and their descriptors in the result.
;
; :Examples:
;    To read everything from the file 'OBSFLUX'::
;
;      oobs=obj_new('pp_cmfr','OBSFLUX') ;reads 'OBSFLUX', though the file name is the same as the default
;      all=oobs->getall(desc=desc)
;      print,desc,format='(A10," ",A)'
;      ;    S_LINE Total Line luminosity
;      ; S_DIELIMP Total Dielectronic and Implicit Recombination Luminosity
;      ;    S_MECH Total Mechanical Luminosity
;      ;   S_SHOCK Total Shock Luminosity
;      ;   S_XRAYH X-ray Luminosity (> 1 keV)
;      ;   S_XRAYL X-ray Luminosity (> 0.1 keV)
;      ;      FREQ Continuum Frequencies
;      ;    INTENS Observed intensity (Janskys)
;      ;       LUM Luminosity
;      ;   DIELIMP Dielectronic and Implicit Recombination Line Emission
;      ;      LINE Line Emission
;      ;      MECH Mechanical Luminosity
;      ;       RAD Total Radiative Luminosity
;      ;     SHOCK Total Schock Luminosity
;      ;   RADMECH Total (Rad. + Mech.) Luminosity
;      iplot,all.freq,all.intens ;plot the intensities
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Aug/2009
;-
function pp_cmfr::getall,desc=desc
compile_opt idl2

;assemble the result
ret={s_line:self.s_line,s_dielimp:self.s_dielimp,s_mech:self.s_mech,$
 s_shock:self.s_shock,s_xrayh:self.s_xrayh,s_xrayl:self.s_xrayl,$
 freq:*self.freq,intens:*self.intens,lum:*self.lum,dielimp:*self.dielimp,$
 line:*self.line,mech:*self.mech,rad:*self.rad,shock:*self.shock,radmech:*self.radmech}

if (arg_present(desc)) then desc=*self.desc
return,ret
end

;+
; :Description:
;    Returns the variables selected from the corresponding keywords (see description below)
;    
; :Keywords:
;    desc : out, optional
;      An array string with the pairs of tag names and their descriptors in the result.
;    s_line : out, optional
;      Total Line luminosity.
;    s_dielimp : out, optional
;      Total Dielectronic and Implicit Recombination Luminosity.
;    s_mech : out, optional
;      Total Mechanical Luminosity.
;    s_shock : out, optional
;      Total Shock Luminosity.
;    s_xrayh : out, optional
;      X-ray Luminosity (> 1 keV).
;    s_xrayl : out, optional
;      X-ray Luminosity (> 0.1 keV).
;    freq : out, optional
;      Continuum Frequencies.
;    intens : out, optional
;      Observed intensity (Janskys).
;    lum : out, optional
;      Luminosity.
;    dielimp : out, optional
;      Dielectronic and Implicit Recombination Line Emission.
;    line : out, optional
;      Line Emission.
;    mech : out, optional
;      Mechanical Luminosity.
;    rad : out, optional
;      Total Radiative Luminosity.
;    shock : out, optional
;      Total Schock Luminosity.
;    radmech : out, optional
;      Total (Rad. + Mech.) Luminosity.
;      
; :Examples:
;    To obtain the frequencies and intensities from a file 'OBSFLUX' and plot them::
;
;      oobs=obj_new('pp_cmfr') ;the default file name is 'OBSFLUX', so it is not necessary to provide it
;      oobs->getvariable,intens=intens,freq=freq
;      iplot,freq,intens
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Aug/2009
;-
pro pp_cmfr::getvariable,desc=desc,s_line=s_line,s_dielimp=s_dielimp,s_mech=s_mech,$
 s_shock=s_shock,s_xrayh=s_xrayh,s_xrayl=s_xrayl,freq=freq,intens=intens,lum=lum,$
 dielimp=dielimp,line=line,mech=mech,rad=rad,shock=shock,radmech=radmech
compile_opt idl2

if (arg_present(desc)) then desc=*self.desc
if (arg_present(s_line)) then s_line=self.sline
if (arg_present(s_dielimp)) then s_dielimp=self.dielimp
if (arg_present(s_mech)) then s_mech=self.s_mech
if (arg_present(s_shock)) then s_shock=self.s_shock
if (arg_present(s_xrayh)) then s_xrayh=self.s_xrayh
if (arg_present(s_xrayl)) then s_xrayl=self.s_xrayl
if (arg_present(freq)) then freq=*self.freq
if (arg_present(intens)) then intens=*self.intens
if (arg_present(lum)) then lum=*self.lum
if (arg_present(dielimp)) then dielimp=*self.dielimp
if (arg_present(line)) then line=*self.line
if (arg_present(rad)) then rad=*self.rad
if (arg_present(shock)) then shock=*self.shock
if (arg_present(radmech)) then radmech=*self.radmech

end

function pp_cmfr::init,file ;initialization of the object
compile_opt idl2,hidden
tmp=pp_cmfread(file) ;reads and parses the content
;try to find the 6 scalars and 9 arrays the file should have if it is OBSFLUX
for i=0,n_tags(self.dind)-1 do self.dind.(i)=-1 ;mark everything as not found to begin with
;scalars:
if (tmp.nscal gt 0) then begin
  names=strlowcase(tmp.scal.name)
  for i=0,tmp.nscal-1 do begin
    if strmatch(names[i],'total line luminosity') then begin
      self.s_line=tmp.scal[i].value & self.dind.s_line=i & continue & end
    if strmatch(names[i],'total dielectronic and implicit recombination luminosity') then begin
      self.s_dielimp=tmp.scal[i].value & self.dind.s_dielimp=i & continue & end
    if strmatch(names[i],'total mechanical luminosity') then begin
      self.s_mech=tmp.scal[i].value & self.dind.s_mech=i & continue & end
    if strmatch(names[i],'total shock luminosity') then begin
      self.s_shock=tmp.scal[i].value & self.dind.s_shock=i & continue & end
    if strmatch(names[i],'x-ray luminosity (> 0.1 kev)') then begin
      self.s_xrayl=tmp.scal[i].value & self.dind.s_xrayl=i & continue & end
    if strmatch(names[i],'x-ray luminosity (> 1 kev)') then begin
      self.s_xrayh=tmp.scal[i].value & self.dind.s_xrayh=i & continue & end
  endfor
endif
;arrays:
if (tmp.narr gt 0) then begin
  names=strlowcase(tmp.arr.name)
  for i=0,tmp.narr-1 do begin
    if strmatch(names[i],'continuum frequencies*') then begin
      self.freq=tmp.arr[i].values & self.dind.freq=i & continue & end
    if strmatch(names[i],'observed intensity (janskys)') then begin
      self.intens=tmp.arr[i].values & self.dind.intens=i & continue & end
    if strmatch(names[i],'luminosity') then begin
      self.lum=tmp.arr[i].values & self.dind.lum=i & continue & end
    if strmatch(names[i],'dielectronic and implicit recombination line emission') then begin
      self.dielimp=tmp.arr[i].values & self.dind.dielimp=i & continue & end
    if strmatch(names[i],'line emission') then begin
      self.line=tmp.arr[i].values & self.dind.line=i & continue & end
    if strmatch(names[i],'mechanical luminosity') then begin
      self.mech=tmp.arr[i].values & self.dind.mech=i & continue & end
    if strmatch(names[i],'total radiative luminosity') then begin
      self.rad=tmp.arr[i].values & self.dind.rad=i & continue & end
    if strmatch(names[i],'total schock luminosity') then begin
      self.shock=tmp.arr[i].values & self.dind.shock=i & continue & end
    if strmatch(names[i],'total (rad. + mech.) luminosity') then begin
      self.radmech=tmp.arr[i].values & self.dind.radmech=i & continue & end
  endfor
endif
;put NaN in the unused pointers, if any
if (not ptr_valid(self.freq)) then self.freq=ptr_new(!values.d_nan)
if (not ptr_valid(self.intens)) then self.intens=ptr_new(!values.d_nan)
if (not ptr_valid(self.lum)) then self.lum=ptr_new(!values.d_nan)
if (not ptr_valid(self.dielimp)) then self.dielimp=ptr_new(!values.d_nan)
if (not ptr_valid(self.line)) then self.line=ptr_new(!values.d_nan)
if (not ptr_valid(self.mech)) then self.mech=ptr_new(!values.d_nan)
if (not ptr_valid(self.rad)) then self.rad=ptr_new(!values.d_nan)
if (not ptr_valid(self.shock)) then self.shock=ptr_new(!values.d_nan)
if (not ptr_valid(self.radmech)) then self.radmech=ptr_new(!values.d_nan)

;assemble the descriptor

tags=tag_names(self.dind)
ntags=n_elements(tags)
desc=strarr(2,ntags)
for i=0,ntags-1 do begin
  desc[0,i]=tags[i]
  ind=self.dind.(i)
  if (ind ne -1) then desc[1,i]=strpos(tags[i],'S_') eq 0 ? tmp.scal[ind].name : tmp.arr[ind].name
endfor

self.desc=ptr_new(desc,/no_copy)
self.cont=ptr_new(tmp,/no_copy)
return,1
end

pro pp_cmfr::cleanup ;clears the heap variables created by the object
compile_opt idl2,hidden
for i=0,(*self.cont).narr-1 do ptr_free,(*self.cont).arr[i].values
ptr_free,self.cont
end

;+
; :Description:
;    Object to read and interpret OBSFLUX-type files created by cmfgen:
;    
;    Uses pp_cmfread to do the low level reading and parsing,
;    and returns a structure with the names and values read (see that function for details).
;    pp_cmfread is more general, works with any file with a
;    format like OBSFLUX or obs_fin (see the function for details).
;    
;    The object pp_cmfr uses pp_cmfread, then tries to interpret the variable names
;    to match with them with the physical quantities expected to be in the file.
;     
;    The interesting methods are getall, and getvariable (see below).
;    
; :Uses: pp_cmfread
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Aug/2009
;-
pro pp_cmfr__define ;definition of the object that reads and interprets OBSFLUX files
compile_opt idl2
  void={PP_CMFR,cont:ptr_new(),$ ;structure with the file contents
   ;fields for the scalars (all must start with s_, for the use of getall)
   s_line:!values.d_nan,$ ;"Total Line luminosity"
   s_dielimp:!values.d_nan,$ ;"Total Dielectronic and Implicit Recombination Luminosity"
   s_mech:!values.d_nan,$ ;"Total Mechanical Luminosity"
   s_shock:!values.d_nan,$ ;"Total Shock Luminosity"
   s_xrayl:!values.d_nan,$ ;"X-ray Luminosity (> 0.1 keV)"
   s_xrayh:!values.d_nan,$ ;"X-ray Luminosity (> 1 keV)"
   ;fields for the vectors:
   freq:ptr_new(),$ ;"Continuum Frequencies"
   intens:ptr_new(),$ ;"Observed intensity (Janskys)"
   lum:ptr_new(),$ ;"Luminosity"
   dielimp:ptr_new(),$ ;"Dielectronic and Implicit Recombination Line Emission"
   line:ptr_new(),$ ;"Line Emission"
   mech:ptr_new(),$ ;"Mechanical Luminosity"
   rad:ptr_new(), $ ;"Total Radiative Luminosity"
   shock:ptr_new(),$ ;"Total Schock Luminosity"
   radmech:ptr_new(),$ ;"Total (Rad. + Mech.) Luminosity"
   ;indexes to descriptors:
   dind:{pp_cmfr_dind,s_line:-1,s_dielimp:-1,s_mech:-1,s_shock:-1,s_xrayh:-1,s_xrayl:-1,$
    freq:-1,intens:-1,lum:-1,dielimp:-1,line:-1,mech:-1,rad:-1,shock:-1,radmech:-1},$
   desc:ptr_new()} ;descriptors for the field names
end