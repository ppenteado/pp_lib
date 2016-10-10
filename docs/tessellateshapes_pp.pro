; docformat = 'rst'
;+
; :Author: Paulo Penteado (http://www.ppenteado.net), based on the original code
; that comes with IDL.
; 
; :History:
;    Just a function to overwrite IDL's _tessellateshapes, to preserve vertex
;    colors when the vertices are tessellated into a different number of vertices.
;    This file is compiled by `pp_drawshericalpoly`, to make sure this method is compiled
;    after the native _tessellateshapes.
; 
;-

;+
; :Description:
;    Just a function to overwrite IDL's _tessellateshapes, to preserve vertex
;    colors when the vertices are tessellated into a different number of vertices.
;    This file is compiled by `pp_drawshericalpoly`, to make sure this method is compiled
;    after the native _tessellateshapes. The changes are at lines 117-120.
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Aug/2015
;-
function IDLitVisPolygon::_TessellateShapes, $
  data, connectivity, shapes, vertColors, $
    MAP_STRUCTURE=sMap, $
    POLYGONS=polygonbuffer, $
    VERT_COLORS=colorbuffer

    compile_opt idl2, hidden

    nShape = N_ELEMENTS(shapes)
    hasMap = N_TAGS(sMap) gt 0

    polygons = !NULL
    nvert = 0L
    npoly = 0L

    ndimension = N_ELEMENTS(data[*,0])
    if (hasMap && ndimension gt 2) then begin
      ndimension = 2
      data = data[0:1,*]
    endif

    ; Create buffers that are perhaps slightly larger than required.
    ; This is much more efficient than concatenating arrays.
    nmaxvert = N_ELEMENTS(connectivity)
    vertbuffer = DBLARR(ndimension*nmaxvert, /NOZERO)
    nmaxpoly = 2*nmaxvert
    polygonbuffer = LONARR(nmaxpoly, /NOZERO)
    
    nvertinput = N_ELEMENTS(data)/ndimension
    dim = SIZE(vertColors, /DIMENSIONS)
    hasAlpha = dim[0] eq 4
    hasColors = dim[[1]] eq nvertinput
    if (hasColors) then begin
      colorbuffer = BYTARR((3+hasAlpha)*nmaxvert, /NOZERO)
    endif

    for i=0,nShape-1 do begin

        ; Pull out the individual shape and its vertices.
        polyshape1 = (i lt nShape-1) ? $
            connectivity[shapes[i]:shapes[i+1]-1] : $
            connectivity[shapes[i]:*]
        idx = 0
        nsubvert = 0
        while (idx lt N_ELEMENTS(polyshape1)) do begin
            n1 = polyshape1[idx]
            if (n1 eq -1) then $
                break
            if (n1 eq 0) then begin
              idx++
              continue
            endif
            ; Concat the individual shape parts and create a new
            ; connectivity array for the tessellator to use.
            datasub = data[*, polyshape1[idx+1:idx+n1]]
            polysub = [n1, LINDGEN(n1)+nsubvert]
            vert1 = (idx gt 0) ? [[TEMPORARY(vert1)], [TEMPORARY(datasub)]] : $
              TEMPORARY(datasub)
            polygons1 = (idx gt 0) ? $
              [TEMPORARY(polygons1), TEMPORARY(polysub)] : TEMPORARY(polysub)
            if (hasColors) then begin
              colorsub = vertColors[*,polyshape1[idx+1:idx+n1]]
              color1 = (idx gt 0) ? [[TEMPORARY(color1)], [TEMPORARY(colorsub)]] : $
                TEMPORARY(colorsub)
            endif
            idx += n1 + 1
            nsubvert += n1
        endwhile

        if (N_ELEMENTS(vert1) le 1) then $
                continue

        if (hasMap) then begin
          ; Map_Proj_Forward fails for polygons with vertices that are more
          ; than 180 degrees east of the map center. Constrain these vertices
          ; so they are <= 180 degrees east of the center.
            w = WHERE(vert1[0,*] gt sMap.p0lon + 180, /NULL)
            if (ISA(w)) then begin
              vert1[0,w] = (sMap.p0lon + 180) < vert1[0,w]
            endif
            vert1 = MAP_PROJ_FORWARD(vert1, $
                MAP=sMap, $
                CONNECTIVITY=polygons1, $
                POLYGONS=polygons1)
            ; See if polygon is off the map.
            if (N_ELEMENTS(vert1) le 1) then $
                continue
            nsubvert = N_ELEMENTS(vert1)/2
            nsubcolor = N_ELEMENTS(color1)/(3+hasAlpha)
            if (nsubvert ne nsubcolor) then begin
              if (nsubvert eq (nsubcolor-1)) then begin
                color1 = color1[*,0:nsubvert-1]
              endif else begin
                ;start of edit by Paulo Penteado, to preserve vertex colors
                ;hasColors = 0b
                ;color1 = !NULL
                ;colorbuffer = !NULL
                color1=color1[*,0]#replicate(1B,nsubvert)
                ;end of edit
              endelse
            endif
        endif

        ; Tessellate
        voffset = vert1[*,0]
        ; Remove any huge offsets (CR40875)
        for idx=0,ndimension-1 do vert1[idx,*] -= voffset[idx]

        ; Check for non-finite values before attempting to tessellate.
        ; This prevents the tessellator from throwing an error.
        !null = Where(Finite(vert1), NCOMPLEMENT=nBad, COMPLEMENT=wBad)
        if (nBad ne 0) then begin
          continue
        endif
        self._oTessellate->AddPolygon, TEMPORARY(vert1), $
            POLYGON=TEMPORARY(polygons1), AUXDATA=color1
        success = self._oTessellate->Tessellate(vert1, polygons1, AUXDATA=color1)
        self._oTessellate->Reset
        if (~success) then $
            continue

        ; Put back the "huge" offset
        for idx=0,ndimension-1 do vert1[idx,*] += voffset[idx]

        ; Offset this connectivity by the total # of verts.
        ; This assumes the connectivity is sets of triangles.
        polygons1 += nvert
        polygons1[0:*:4] = 3
        nNewVert = N_ELEMENTS(vert1)/ndimension

        ; See if we need to grow our vert buffer array
        if (nNewVert+nvert gt nmaxvert) then begin
          nmaxvert = LONG(1.5*(nNewVert+nvert))
          vtmp = TEMPORARY(vertbuffer)
          vertbuffer = DBLARR(ndimension, nmaxvert, /NOZERO)
          if (nvert gt 0) then vertbuffer[0] = vtmp
          if (hasColors) then begin
            ctmp = TEMPORARY(colorbuffer)
            colorbuffer = BYTARR((3+hasAlpha)*nmaxvert, /NOZERO)
            if (nvert gt 0) then colorbuffer[0] = ctmp
          endif
        endif

        ; See if we need to grow our polygons buffer array
        nNewPoly = N_ELEMENTS(polygons1)
        if (npoly+nNewPoly gt nmaxpoly) then begin
          nmaxpoly = LONG(1.5*(npoly+nNewPoly))
          ptmp = TEMPORARY(polygonbuffer)
          polygonbuffer = LONARR(nmaxpoly, /NOZERO)
          if (npoly gt 0) then polygonbuffer[0] = ptmp
        endif

        ; Append the vertices and connectivity for this shape.
        vert1 = REFORM(vert1, ndimension*nNewVert, /OVERWRITE)
        vertbuffer[ndimension*nvert] = TEMPORARY(vert1)
        polygonbuffer[npoly] = TEMPORARY(polygons1)
        if (hasColors) then begin
          colorbuffer[(3+hasAlpha)*nvert] = $
            REFORM(color1,(3+hasAlpha)*nNewVert,/OVERWRITE)
        endif

        ; Now increment the counters.
        nvert += nNewVert
        npoly += nNewPoly

    endfor

    if (nvert gt 0) then begin
      if (nvert ne nmaxvert) then $
        vertbuffer = vertbuffer[0:ndimension*nvert-1]
      if (npoly ne nmaxpoly) then $
        polygonbuffer = polygonbuffer[0:npoly-1]
      vertbuffer = REFORM(vertbuffer, ndimension, nvert, /OVERWRITE)
      if (hasColors) then begin
        if (nvert ne nmaxvert) then $
          colorbuffer = colorbuffer[0:(3+hasAlpha)*nvert-1]
        colorbuffer = REFORM(colorbuffer, 3+hasAlpha, nvert, /OVERWRITE)
      endif
      return, vertbuffer
    endif

    return, 0
end

;+
; :Description:
;    A dummy procedure, that exists just to get this file compiled when needed, to
;    get the _tessellateshapes method above compiled.
;    This file is compiled by `pp_drawshericalpoly`, to make sure this method is compiled
;    after the native _tessellateshapes.
;
; :Author: Paulo Penteado (http://www.ppenteado.net), Aug/2015
;-
pro tessellateshapes_pp
compile_opt idl2,logical_predicate
!null='Dummy procedure, exists just to compile IDLitVisPolygon::_TessellateShapes above'  
end
