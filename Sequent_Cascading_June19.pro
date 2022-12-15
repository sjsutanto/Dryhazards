; try to find the sequent of cascading event
; Samuel Jonson Sutanto
; 15 October 2018
; Updating if there is 7 days without hazard in between 2 events, we count this as 1 event.

read = 1

IF read EQ 1 THEN BEGIN
  files = FILE_SEARCH('/Users/sutanto/Documents/Postdoc/OVIMA/Daily_SM_drought_WB_Converted.nc')
  peyl_readnc, lon, file=files, var_name='longitude'
  peyl_readnc, lat, file=files, var_name='latitude'
  peyl_readnc, New_SM, file=files, var_name='New_SM'

  ; open fires data
  files = FILE_SEARCH('/Users/sutanto/Documents/Postdoc/OVIMA/Correct_data/fwi_1990_2016_binary_95th_lowThreshold.nc')   ; still using old data
  peyl_readnc, fires, file=files, var_name='variable'
  peyl_readnc, longclad, file=files, var_name='longitude'
  peyl_readnc, laticlad, file=files, var_name='latitude'

  ; open heatwave data
  files = FILE_SEARCH('/Users/sutanto/Documents/Postdoc/OVIMA/Correct_data/datacube_2mtpp_19902016_HW.nc')
  peyl_readnc, heatx, file=files, var_name='variable'
  peyl_readnc, time, file=files, var_name='z'
  peyl_readnc, longheat, file=files, var_name='longitude'
  peyl_readnc, latiheatx, file=files, var_name='latitude'
  latiheat = latiheatx[0:52]
  heat = heatx[*,0:52,*]

  ; for lsm
  LSM = REFORM(fires[*,*,0])

  ; considering only summer
  ;SAVE,Mont,FILENAME = 'Month_Number_19792016.sav'
  RESTORE, 'Month_Number_19792016.sav'
  RESTORE, 'Years_Number_19792016.sav'
  Mont1 = Mont[4018:*]
  year1 = years[4018:*]

  pp = WHERE(Mont1 EQ 6, count)
  fires6 = fires[*,*,pp]
  sm6 = New_SM[*,*,pp]

  pp = WHERE(Mont1 EQ 7, count)
  fires7 = fires[*,*,pp]
  sm7 = New_SM[*,*,pp]

  pp = WHERE(Mont1 EQ 8, count)
  fires8 = fires[*,*,pp]
  sm8 = New_SM[*,*,pp]

  ; Change heat data into the same format
  heat6 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),810)
  heat7 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),837)
  heat8 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),837)
  FOR i=0, 26 DO BEGIN
    ; June
    j = i*30
    k = i*92
    heat6[*,*,j:j+29] = heat[*,*,k:k+29]
    ; July
    l = i*31
    m = k+30
    heat7[*,*,l:l+30] = heat[*,*,m:m+30]
    ; August
    n = k+61
    heat8[*,*,l:l+30] = heat[*,*,n:n+30]
  ENDFOR

  ; Make it for every year
  dro_yr = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27,92)
  hea_yr = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27,92)
  fir_yr = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27,92)
  FOR i=0, 26 DO BEGIN
    j = i*30
    l = i*31
    dro_yr[*,*,i,*] = [[[sm6[*,*,j:j+29]]],[[sm7[*,*,l:l+30]]],[[sm8[*,*,l:l+30]]]]
    hea_yr[*,*,i,*] = [[[heat6[*,*,j:j+29]]],[[heat7[*,*,l:l+30]]],[[heat8[*,*,l:l+30]]]]
    fir_yr[*,*,i,*] = [[[fires6[*,*,j:j+29]]],[[fires7[*,*,l:l+30]]],[[fires8[*,*,l:l+30]]]]
  ENDFOR

  ; ========================================================================
  ; hotspot of dry hazards

  ; transform binary SM is 2
  qq = WHERE(dro_yr EQ 1, count)
  IF count NE 0 THEN dro_yr[qq] = 2
  ; transform binary fires is 4
  qq = WHERE(fir_yr EQ 1, count)
  IF count NE 0 THEN fir_yr[qq] = 4
; ========================================================================
; Compound hazards
Compound_dry = dro_yr + hea_yr + fir_yr

Cascad_eve = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
dummy_patt = FLTARR(N_ELEMENTS(lon)*N_ELEMENTS(lat)*27*92) 
bb = 0L

FOR i=0, N_ELEMENTS(lon)-1 DO BEGIN
  FOR j=0, N_ELEMENTS(lat)-1 DO BEGIN
    IF LSM[i,j] NE -3.40000e+38 THEN BEGIN
      FOR zz=0, 26 DO BEGIN
        dummy = REFORM(Compound_dry[i,j,zz,*])
        ; to define event
        ppp = WHERE(dummy NE 0, count)
        IF (count GT 0) THEN dummy[ppp] = 1

        xx = TS_DIFF(dummy,1)
        IF ((dummy[0] EQ 1) AND (dummy[1] EQ 0)) THEN xx[0] = 0   ; to avoid negative values in lengt and there is value 1 in the beginning of the data
        IF ((dummy[0] EQ 1) AND (dummy[1] EQ 1)) THEN xx[0] = -1   ; if the cascading events already start since the beginning
        dummy2 = WHERE(xx EQ -1)   ; starting the event
        dummy3 = WHERE(xx EQ 1)    ; ending the event
        IF (N_ELEMENTS(dummy3) NE N_ELEMENTS(dummy2)) THEN dummy3=[dummy3,91]  ; if no end for the event

        ; If only 1 event then just move out
        IF N_ELEMENTS(dummy2) EQ 1 THEN CONTINUE  ;***** SONY: CONTINUE jumps directly to the next iteration of the enclosing FOR
        ;==================================================
        ;==================================================
        ; Here I will put a filter for combining events into 1 if there are 7 days without event.
        dummy2bb = dummy2[1:*]
        dummy2bb = [dummy2bb,dummy3[N_ELEMENTS(dummy3)-1]]
        dummyxx = dummy2bb-dummy3

        ;***** SONY: change this value in order to have different zero gap in cascading event
        pp = WHERE(dummyxx GE 1)
        dummy2cc = [dummy2[0],dummy2bb[pp]]
        dummy3cc = [dummy3[pp],dummy3[N_ELEMENTS(dummy3)-1]]
        ; since dummy2cc always starts from 0, then we move back 1
        dummy2cc = dummy2cc+1
        
        FOR kk=0, N_ELEMENTS(dummy2cc)-1 DO BEGIN
          dummy4 = REFORM(Compound_dry[i,j,zz,dummy2cc[kk]:dummy3cc[kk]])
          dummy5 = dummy4[UNIQ(dummy4)]
          IF N_ELEMENTS(dummy5) GT 1 THEN BEGIN
            Cascad_eve[i,j,zz] = Cascad_eve[i,j,zz] + 1
            dummy_patt[bb:bb+N_ELEMENTS(dummy5)-1]= dummy5
            bb = bb+N_ELEMENTS(dummy5)+1
          ENDIF
        ENDFOR
;STOP
      ENDFOR
    ENDIF
  ENDFOR
ENDFOR

Num_event = TOTAL(Cascad_eve,/NaN)
; Filter out unused array
pp = WHERE(dummy_patt NE 0)
dummy_patt2 = dummy_patt[0:pp[N_ELEMENTS(pp)-1]]

dummy_patt3 = dummy_patt2
ppp = WHERE(dummy_patt3 NE 0, count)
IF (count GT 0) THEN dummy_patt3[ppp] = 1

xx = TS_DIFF(dummy_patt3,1)
IF ((dummy_patt3[0] EQ 1) AND (dummy_patt3[1] EQ 0)) THEN xx[0] = 0   ; to avoid negative values in lengt and there is value 1 in the beginning of the data
IF ((dummy_patt3[0] EQ 1) AND (dummy_patt3[1] EQ 1)) THEN xx[0] = -1   ; if the cascading events already start since the beginning
dummy2 = WHERE(xx EQ -1)   ; starting the event
dummy3 = WHERE(xx EQ 1)    ; ending the event
IF (N_ELEMENTS(dummy3) NE N_ELEMENTS(dummy2)) THEN dummy3=[dummy3,N_ELEMENTS(dummy_patt3)-1]  ; if no end for the event

dummy2[1:*] = dummy2[1:*]+1   ; since there is zero in between
numcascad = 0
dummy_patt2_ori = dummy_patt2

numcascad_final = FLTARR(N_ELEMENTS(dummy2))
location_final = FLTARR(N_ELEMENTS(dummy2))
location_end = FLTARR(N_ELEMENTS(dummy2))
;STOP
FOR i=0, N_ELEMENTS(dummy2)-1 DO BEGIN
  dummy = dummy_patt2[dummy2[i]:dummy3[i]]
  IF TOTAL(dummy) EQ 0 THEN CONTINUE
  FOR j=0, N_ELEMENTS(dummy2)-1 DO BEGIN
    dummy4 = dummy_patt2[dummy2[j]:dummy3[j]]
    IF ARRAY_EQUAL(dummy4, dummy) THEN BEGIN
      numcascad = numcascad + 1
      dummy_patt2[dummy2[j]:dummy3[j]] = 0
    ENDIF  
  ENDFOR
  numcascad_final[i] = numcascad
  location_final[i] = dummy2[i]
  location_end[i] = dummy3[i]
numcascad = 0   ; Reset the number  
;STOP
ENDFOR

; sort from big to small
mostcas = REVERSE(SORT(numcascad_final))
numlocation = location_final[mostcas]
numlocation2 = location_end[mostcas]

ENDIF



END
