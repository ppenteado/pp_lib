; docformat = 'rst'
;+
; :Description:
;    Calculates rise, transit and set times, plus the time above the horizon (sky time) for the given
;    equatorial coordinates, location and date.
;    
;    GDL-compatible. All dependencies are routines from idlastro (http://idlastro.gsfc.nasa.gov/).
;    
; :Returns:
;    A structure with the fields transit, sky, rise and set, each with the same dimensions as ra,dec, with
;    the times for transit, above the horizon, rise and set, respectively, in civil hours. Those points that are never
;    above the horizon have sky=0 and NaN for rise and set. Those points that are always above the horizon have sky=24
;    and NaN for rise and set.
;
; :Params:
;    ra : in, required
;      Right ascension(s) of the point(s) to calculate the visibility for. Units: hours. May be a scalar or arrar,
;      and must have the same number of elements as dec.
;    dec : in, required
;      Declination(s) of the point(s) to calculate the visibility for. Units: degrees. May be a scalar or arrar,
;      and must have the same number of elements as ra.
;
; :Keywords:
;    obsname : in, optional
;      String with the observatory name, as understood by idlastro's observatory procedure. If provided, supersedes lat, lon and tz.
;    lat : in, optional
;      Latitude of the observing location, in degrees (positive is north). 
;    lon : in, optional
;      Longitude of the observing location, in degrees (positive is east, contrary to that returned by observatory).
;    tz : in, optional
;      Time zone of the observing location, in hours (positive is east, contrary to that returned by observatory).
;    day : in, required
;      Day of the month for the observation.
;    month : in, required
;      Month for the observation.
;    year : in, required
;      Year for the observation.
;    prev_day : in, optional, default=0
;      By default, returned times of day (for transit, rise and set) are relative to local midnight, so times before
;      midnight are negative. If this keyword is set, times before midnight are returned as the time for the previous day,
;      instead.
;    extra : in, optional, default=0
;      If set, extra data is output (sunrise/sunset times, target altitude at sunrise/sunet, etc).
;      
; :Examples:
; 
;   Make up some coordinates and calculate the times::
;   
;     ra=[22d0+3d0/60d0+18d0/3600d0,0d0,19d0]
;     dec=[-9d0-40d0/60d0-34d0/3600d0,-87d0,80d0]
;     r=pp_rise_set(ra,dec,obsname='eso',day=0,month=06,year=2010,/prev)
;     print,r.transit
;       6.2173759       8.1019913       3.1540115
;     print,r.sky
;       12.765635       24.000000       0.0000000
;     print,r.rise
;       23.834559             NaN             NaN
;     print,r.set
;       12.600193             NaN             NaN
;       
; :Uses: observatory, ten, ct2lst, jdcnv  
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Jun/2010
;-
function pp_rise_set,ra,dec,obsname=obsname,lat=lat,lon=lon,tz=tz,day=day,$
  month=month,year=year,prev_day=prev,extra=extra
compile_opt idl2,logical_predicate

;Defaults
extra=keyword_set(extra)
prev=n_elements(prev) eq 1 ? prev : 0
if (n_elements(day) ne 1) || (n_elements(month) ne 1) || (n_elements(year) ne 1) then $
 message,'Time must be provided by day,month,year'
if (n_elements(obsname) eq 1) then begin
  observatory,obsname,obs
  lat=obs.latitude
  lon=-obs.longitude
  tz=-obs.tz
endif else if (n_elements(lat) ne 1) || (n_elements(lon) ne 1) || (n_elements(tz) ne 1) then $
 message,'Location must be provided by either obsname, or lat,lon,tz'

;Find the transit time
;Find the LST at 0h for the day
ct2lst,lst,lon,tz,0d0,day,month,year
;Find the LST at 0h for the previous day
ct2lst,lst1,lon,tz,0d0,day-1,month,year
;Sidereal to Civil time factor
scfac=1d0+(lst-lst1)/24d0
;Transit time in sidereal hours after midnight
transit_sid=ra-lst
;Transit time in civil hours after midnight
transit_civ=transit_sid*scfac

;Rise/set/sky time
rise=double(ra) & rise[*]=0d0 & set=rise & sky=rise
;Those that are never above the horizon
w=lat gt 0d0 ? where(dec lt (-90d0+lat),nw) : where(dec gt 90d0+lat,nw)
if (nw gt 0) then begin
  rise[w]=!values.d_nan & set[w]=!values.d_nan & sky[w]=0d0
endif
;Those that are always above the horizon
w=lat gt 0d0 ? where(dec gt (90d0-lat),nw) : where(dec lt -90d0-lat,nw)
if (nw gt 0) then begin
  rise[w]=!values.d_nan & set[w]=!values.d_nan & sky[w]=24d0
endif
;Those that rise and set
w=where(finite(rise),nw)
if (nw gt 0) then begin
  ;Find the hour angle of set time
  rdec=dec[w]*!dpi/180d0 & rlat=lat[w]*!dpi/180d0
  tmp=dcomplex(-sin(rlat)*sin(rdec)/(cos(rlat)*cos(rdec)))
  ha=acos(real_part(tmp))*12d0/!dpi
  ;Time in the sky (civil hours)
  sky[w]=2d0*ha*scfac
  ;Rise time (civil hours after midnight)
  rise[w]=transit_civ-ha*scfac
  ;Set time (civil hours after midnight)
  set[w]=transit_civ+ha*scfac
endif
;Convert negative times to previous day
if (prev) then begin
  rise=(rise+24d0) mod 24
  set=(set+24d0) mod 24
  transit_civ=(transit_civ+24d0) mod 24
endif

if extra then begin
  jd=julday(month,day,year)
  sunpos,jd,sunra,sundec
  sunra/=15d0
  sun=pp_rise_set(sunra,sundec,obsname=obsname,lat=lat,lon=lon,tz=tz,day=day,$
    month=month,year=year,prev_day=prev)
  sun=create_struct(sun,'ra',sunra,'dec',sundec)
  eq2hor,ra*15d0,dec,jd+(sun.rise+tz+12d0)/24d0,altr,azr,lat=lat,lon=lon
  eq2hor,ra*15d0,dec,jd+(sun.set+tz+12d0)/24d0,alts,azs,lat=lat,lon=lon
  target_nosun={sunset_alt:alts,sunrise_alt:altr};,no_sun_hours:hours}
endif
;Pack the results
ret={transit:transit_civ,sky:sky,rise:rise,set:set}


if extra then begin
  if n_elements(obsname) eq 0 then obsname=''
  ret=create_struct(ret,'jd',jd,'sun',sun,'target',target_nosun,'month',$
    month,'day',day,'year',year,'ra',ra,'dec',dec,'obsname',obsname,'lat',lat,'lon',lon,'tz',tz)

endif

return,ret

end
