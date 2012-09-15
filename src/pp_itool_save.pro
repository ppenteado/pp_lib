; docformat = 'rst'
;+
; :Description:
;    Saves an itool to and isv and/or an eps file, optionally closing the itool afterwards.
;    Some eps options can be chosen (CMYK colorspace and vector/bitmap use).
;
; :Params:
;    fname : in, optional
;      Name of the file to make, to be used for the isv and the eps files.
;      That is, if name is 'itoolfile', both 'itoolfile.isv' and 'itoolfile.eps' are made.
;
; :Keywords:
;    isvfile : in, optional
;      Name of the isv file to make. Is ignored if argument file is provided.
;    epsfile : in, optional
;      Name of the eps file to make. Is ignored if argument file is provided.
;    iclose : in, optional, default=0
;      If set, the itool is closed after saving.
;    cmyk : in, optional, default=0
;      If set, the eps is saved in CMYK colorspace.
;    novec : in, optional, default=0
;      If set, the eps file is made a bitmap, instead of vector.
;    id : in, out, optional
;      Itool id to use. If not specified, the current is used and its id is returned.
;
; :Examples:
;   To make an iplot that does not show up in the screen and is saved and closed automatically:;;
;   
;   iplot,findgen(10),user_interface='none',/no_saveprompt,/disable_splash
;   
;   pp_itool_save,'pp_iplotexample',/iclose
;   
;
; :Uses:
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Mar/2009
;-
pro pp_itool_save,fname,isvfile=isvfile,epsfile=epsfile,iclose=iclose,cmyk=cmyk,novec=novec,id=id
compile_opt idl2

;Defaults
cmyk=n_elements(cmyk) eq 1 ? cmyk : 0
iclose=n_elements(iclose) eq 1 ? iclose : 0
novec=n_elements(novec) eq 1 ? novec : 0
if (n_elements(fname) eq 1) then begin
  isvfile=fname+'.isv'
  epsfile=fname+'.eps'
endif

;Get the itool object
if (n_elements(id) eq 1) then itcurrent,id
id=itgetcurrent(tool=ot) 
if (~obj_valid(ot)) then message,'No valid itool found'
;Set the postscript options
psi=ot->findidentifiers('*post*')
pst=ot->getbyidentifier(psi)
pst->setproperty,graphics_format=~novec,color_model=cmyk
ot->commitactions
if (float(!version.release) ge 7.1) then begin
;Mostly for use by old programs, since From IDL 7.1, this program is largely unnecessary
  if (n_elements(isvfile) eq 1) then isave,isvfile
  if (n_elements(epsfile) eq 1) then isave,epsfile
endif else begin ;For IDL <7.1
  if (n_elements(isvfile) eq 1) then begin ;Save isv file
    idsave=ot->findidentifiers('*save',/operations)
    osave=ot->getbyidentifier(idsave) ;Get Save operator object reference
    osave->setproperty,show_execution_ui=0,filename=isvfile
;Save a copy of some properties that may be changed when the itool is saved
    ids=ot->findidentifiers("*TOOL*/WINDOW/VIEW*/VISUALIZATION LAYER")
    nids=n_elements(ids)
    vrp=ptrarr(nids)
    ov=objarr(nids)
    for i=0,nids-1 do begin
      ov[i]=ot->getbyidentifier(ids[i])
      ov[i]->getproperty,viewplane_rect=vr
      vrp[i]=ptr_new(vr)
    endfor
    idsa=ot->findidentifiers("*TOOL*/WINDOW/VIEW*/ANNOTATION LAYER/TEXT")
    if (idsa[0] ne '') then begin
      nidsa=n_elements(idsa)
      arp=ptrarr(nidsa)
      oa=objarr(nidsa)
    endif else nidsa=0
    for i=0,nidsa-1 do begin
      oa[i]=ot->getbyidentifier(idsa[i])
      oa[i]->getproperty,font_size=lfs
      arp[i]=ptr_new(lfs)
    endfor
    void=ot->doaction(idsave)
;Restore the properties that could have changed                 
    for i=0,nids-1 do ov[i]->setproperty,viewplane_rect=*vrp[i]
    for i=0,nidsa-1 do oa[i]->setproperty,font_size=*arp[i]
    ot->commitactions
  endif
  if (n_elements(epsfile) eq 1) then begin ;Save eps file
    idexp=ot->findidentifiers('*export*',/operations)
    oexp=ot->getbyidentifier(idexp)
    oexp->setproperty,show_execution_ui=0
    oexp->setproperty,filename=epsfile
    oexp->setproperty,source=1
    void=ot->doaction(idexp)
  endif
endelse
if (iclose) then itdelete

end

