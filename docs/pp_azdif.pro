; docformat = 'rst'

;+
; :Description:
;    Given the angle between two directions (`dphase`) with angle from vertical
;    given by `dinc`, `dema`, returns the azimuth difference between them. All inputs
;    and the output are in degrees. This function is vectorized, so the 3 input arguments
;    can be of any dimension (must be the same for all 3), and that will be the dimension of the
;    result.
;    This function is complementary to `pp_phase_angle`.
;    
;
; :Params:
;    dphase : in, required
;      The angle between the two directions (phase angle), in degrees.
;    dinc : in, required
;      The angle between one of the directions and the vertical (incidence angle), in degrees.
;    dema : in, required
;      The angle between the other direction and the vertical (emission angle), in degrees.
;
; :Examples:
;    Taking incidence and emission in the same plane::
;
;      print,pp_azdif(30d0,40d0,70d0)
;      ;0.0000000
;      print,pp_azdif(110d0,40d0,70d0)
;      ;180.00001
;      
;    Taking incidence and emission at the horizon::
;
;      print,pp_azdif(110d0,90d0,90d0)
;      ;110.00000
;
; :Uses:
;
; :Author: Paulo Penteado (pp.penteado@gmail.com), Sep/2009
;-
function pp_azdif,dphase,dinc,dema
compile_opt idl2
;Convert input to radians
ema=dema*!dpi/180d0
inc=dinc*!dpi/180d0
phase=dphase*!dpi/180d0
;Get the necessary sines and cosines
s1=sin(ema)
s2=sin(inc)
c1=cos(ema)
c2=cos(inc)
cp=cos(phase)
;Get the cosine of the azimuth difference
tmp=(cp-(c1*c2))/(s1*s2)
;Treat special cases (incidence or emission is at zenith or nadir)
w=where(s1*s2 eq 0d0,nw)
if (nw gt 0) then tmp[w]=1d0
;Use the complex arc cosine to avoid NaNs due to lack of precision
rdeltaphi=double(acos(complex(tmp,0d0)))
return,rdeltaphi*180d0/!dpi
end