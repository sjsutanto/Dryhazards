; Plotting and analyse the fires, drought, and heatwave data
; New datasets from claudia di napoli and vitolo 
; Samuel Jonson Sutanto
; 4 October 2018
; Updated using the correct data for fires on 23 October 2018

read = 1

IF read EQ 1 THEN BEGIN
  files = FILE_SEARCH('/Users/sutanto/Documents/Postdoc/OVIMA/Daily_SM_drought_WB_Converted.nc')
  peyl_readnc, lon, file=files, var_name='longitude'
  peyl_readnc, lat, file=files, var_name='latitude'
  peyl_readnc, New_SM, file=files, var_name='New_SM'
  ; cropping to fires dimension
  ;lat = latx[1:52]
  ;New_SM = New_SMx[*,1:52,*]
  
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
;STOP
  ; for lsm
  LSM = REFORM(fires[*,*,0])

  ; make 0
  p = WHERE(fires EQ -3.40000e+38, count)
  IF (count GT 0) THEN fires[p] = 0
  p = WHERE(heat EQ -3.40000e+38, count)
  IF (count GT 0) THEN heat[p] = 0

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


ENDIF

;Only plot year 2003
fires603 = fires6[*,*,390:419]
sm603 = sm6[*,*,390:419]
heat603 = heat6[*,*,390:419]

fires703 = fires7[*,*,403:433]
sm703 = sm7[*,*,403:433]
heat703 = heat7[*,*,403:433]

fires803 = fires8[*,*,403:433]
sm803 = sm8[*,*,403:433]
heat803 = heat8[*,*,403:433]

fires_03 = [[[fires603]],[[fires703]],[[fires803]]]
sm_03 = [[[sm603]],[[sm703]],[[sm803]]]
heat_03 = [[[heat603]],[[heat703]],[[heat803]]]

fires_03tot = TOTAL(fires_03,3,/NaN)
sm_03tot = TOTAL(sm_03,3,/NaN)
heat_03tot = TOTAL(heat_03,3,/NaN)

p = WHERE(LSM EQ -3.40000e+38, count)
IF (count GT 0) THEN heat_03tot[p] = !VALUES.F_NAN
IF (count GT 0) THEN sm_03tot[p] = !VALUES.F_NAN
IF (count GT 0) THEN fires_03tot[p] = !VALUES.F_NAN
; ========================================================================
; hotspot of dry hazards
SMJJA = [[[sm6]],[[sm7]],[[sm8]]]
firesJJA = [[[fires6]],[[fires7]],[[fires8]]]
heatJJA = [[[heat6]],[[heat7]],[[heat8]]]
 
; transform binary SM is 2
qq = WHERE(SMJJA EQ 1, count)
IF count NE 0 THEN SMJJA[qq] = 2
; transform binary fires is 4
qq = WHERE(firesJJA EQ 1, count)
IF count NE 0 THEN firesJJA[qq] = 4

; now plot the hotspot for single hazard
Num_fires = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))       ; for summer4
Num_drought = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))  ; for summer2
Num_heatwave = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))    ; for summer1

FOR i=0, N_ELEMENTS(lon)-1 DO BEGIN
  FOR j=0, N_ELEMENTS(lat)-1 DO BEGIN
    IF LSM[i,j] NE -3.40000e+38 THEN BEGIN
      pp = WHERE(firesJJA[i,j,*] EQ 4, count)
      Num_fires[i,j] = count
      pp = WHERE(SMJJA[i,j,*] EQ 2, count)
      Num_drought[i,j] = count
      pp = WHERE(heatJJA[i,j,*] EQ 1, count)
      Num_heatwave[i,j] = count
    ENDIF ELSE BEGIN
      Num_fires[i,j] = !VALUES.F_NAN
      Num_drought[i,j] = !VALUES.F_NAN
      Num_heatwave[i,j] = !VALUES.F_NAN
    ENDELSE
  ENDFOR
ENDFOR

LSM2 = Num_drought
p = WHERE(LSM2 GT 0, count)
IF (count GT 0) THEN LSM2[p] = 1

; Filtering heatwaves
p = WHERE(LSM2 EQ 0, count)
IF (count GT 0) THEN Num_heatwave[p] = !VALUES.F_NAN
IF (count GT 0) THEN Num_fires[p] = !VALUES.F_NAN
IF (count GT 0) THEN heat_03tot[p] = !VALUES.F_NAN
IF (count GT 0) THEN sm_03tot[p] = !VALUES.F_NAN
IF (count GT 0) THEN fires_03tot[p] = !VALUES.F_NAN

; ========================================================================
; Compound hazards
Compound_dry = SMJJA + firesJJA + heatJJA

; now plot the hotspot for compound hazard
Com_DRHW = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))       ; for DR + HW
Com_DRFR = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))  ; for DR + FR
Com_ALL = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))    ; for all 3 Hazards

FOR i=0, N_ELEMENTS(lon)-1 DO BEGIN
  FOR j=0, N_ELEMENTS(lat)-1 DO BEGIN
    IF LSM[i,j] NE -3.40000e+38 THEN BEGIN
      pp = WHERE(Compound_dry[i,j,*] EQ 3, count)
      Com_DRHW[i,j] = count
      pp = WHERE(Compound_dry[i,j,*] EQ 6, count)
      Com_DRFR[i,j] = count
      pp = WHERE(Compound_dry[i,j,*] EQ 7, count)
      Com_ALL[i,j] = count
    ENDIF ELSE BEGIN
      Com_DRHW[i,j] = !VALUES.F_NAN
      Com_DRFR[i,j] = !VALUES.F_NAN
      Com_ALL[i,j] = !VALUES.F_NAN
    ENDELSE
  ENDFOR
ENDFOR

; Filtering NAN
p = WHERE(LSM2 EQ 0, count)
IF (count GT 0) THEN Com_DRHW[p] = !VALUES.F_NAN
IF (count GT 0) THEN Com_DRFR[p] = !VALUES.F_NAN
IF (count GT 0) THEN Com_ALL[p] = !VALUES.F_NAN

; ========================================================================
; Cascading events
Cascad_dry = Compound_dry
ppp = WHERE(Cascad_dry NE 0, count)
IF (count GT 0) THEN Cascad_dry[ppp] = 1

Cascad_eve = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))    ; for cascading events
Cascad_dur = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))    ; for cascading duration
Freq_1 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_2 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_3 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_4 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_5 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_6 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_7 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))

Freq_1_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_2_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_3_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_4_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_5_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_6_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_7_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))

Freq_1_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_2_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_3_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_4_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_5_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_6_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
Freq_7_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
;STOP
FOR i=0, N_ELEMENTS(lon)-1 DO BEGIN
  FOR j=0, N_ELEMENTS(lat)-1 DO BEGIN
    IF LSM[i,j] NE -3.40000e+38 THEN BEGIN
      dummy = REFORM(Cascad_dry[i,j,*])
      xx = TS_DIFF(dummy,1)
      IF ((dummy[0] EQ 1) AND (dummy[1] EQ 0)) THEN xx[0] = 0   ; to avoid negative values in lengt and there is value 1 in the beginning of the data
      IF ((dummy[0] EQ 1) AND (dummy[1] EQ 1)) THEN xx[0] = -1   ; if the cascading events already start since the beginning
      dummy2 = WHERE(xx EQ -1)   ; starting the event
      dummy3 = WHERE(xx EQ 1)    ; ending the event
      IF (N_ELEMENTS(dummy3) NE N_ELEMENTS(dummy2)) THEN dummy3=[dummy3,2483]  ; if no end for the event
      ; if only single hazards then filter out
        FOR k=0, N_ELEMENTS(dummy2)-1 DO BEGIN
          dummy5 = REFORM(Compound_dry[i,j,dummy2[k]:dummy3[k]])
          zzz = N_ELEMENTS(dummy5)
          yy = TS_DIFF(dummy5,1)
          yyy = FLTARR(zzz)
          yyy[0:zzz-2] = ABS(yy[1:*])  ; always starting with zero, so I need to filter out
          yyy[zzz-1] = 0
          ;pp = WHERE(yy LT 0, count)
          ;IF (count GT 0) THEN yy[pp] = 0
          zz = TOTAL(yyy)
              IF zz GT 0 THEN BEGIN
                lengt = dummy3[k]-dummy2[k]
                Cascad_eve[i,j] = Cascad_eve[i,j] + 1
                Cascad_dur[i,j] = Cascad_dur[i,j] + lengt
                qqq = WHERE(dummy5 EQ 1, count)
                Freq_1[i,j] = Freq_1[i,j] + count                                
                qqq = WHERE(dummy5 EQ 2, count)
                Freq_2[i,j] = Freq_2[i,j] + count
                qqq = WHERE(dummy5 EQ 3, count)
                Freq_3[i,j] = Freq_3[i,j] + count
                qqq = WHERE(dummy5 EQ 4, count)
                Freq_4[i,j] = Freq_4[i,j] + count
                qqq = WHERE(dummy5 EQ 5, count)
                Freq_5[i,j] = Freq_5[i,j] + count
                qqq = WHERE(dummy5 EQ 6, count)
                Freq_6[i,j] = Freq_6[i,j] + count
                qqq = WHERE(dummy5 EQ 7, count)
                Freq_7[i,j] = Freq_7[i,j] + count
                ; for starting. Starting from 1 because zero = 0
                qq = WHERE(dummy5[1] EQ 1, count)
                IF (count GT 0) THEN Freq_1_start[i,j] = Freq_1_start[i,j] + 1
                qq = WHERE(dummy5[1] EQ 2, count)
                IF (count GT 0) THEN Freq_2_start[i,j] = Freq_2_start[i,j] + 1
                qq = WHERE(dummy5[1] EQ 3, count)
                IF (count GT 0) THEN Freq_3_start[i,j] = Freq_3_start[i,j] + 1
                qq = WHERE(dummy5[1] EQ 4, count)
                IF (count GT 0) THEN Freq_4_start[i,j] = Freq_4_start[i,j] + 1
                qq = WHERE(dummy5[1] EQ 5, count)
                IF (count GT 0) THEN Freq_5_start[i,j] = Freq_5_start[i,j] + 1
                qq = WHERE(dummy5[1] EQ 6, count)
                IF (count GT 0) THEN Freq_6_start[i,j] = Freq_6_start[i,j] + 1
                qq = WHERE(dummy5[1] EQ 7, count)
                IF (count GT 0) THEN Freq_7_start[i,j] = Freq_7_start[i,j] + 1
                ; for Ending.
                qq = WHERE(dummy5[N_ELEMENTS(dummy5)-1] EQ 1, count)
                IF (count GT 0) THEN Freq_1_end[i,j] = Freq_1_end[i,j] + 1
                qq = WHERE(dummy5[N_ELEMENTS(dummy5)-1] EQ 2, count)
                IF (count GT 0) THEN Freq_2_end[i,j] = Freq_2_end[i,j] + 1
                qq = WHERE(dummy5[N_ELEMENTS(dummy5)-1] EQ 3, count)
                IF (count GT 0) THEN Freq_3_end[i,j] = Freq_3_end[i,j] + 1
                qq = WHERE(dummy5[N_ELEMENTS(dummy5)-1] EQ 4, count)
                IF (count GT 0) THEN Freq_4_end[i,j] = Freq_4_end[i,j] + 1
                qq = WHERE(dummy5[N_ELEMENTS(dummy5)-1] EQ 5, count)
                IF (count GT 0) THEN Freq_5_end[i,j] = Freq_5_end[i,j] + 1
                qq = WHERE(dummy5[N_ELEMENTS(dummy5)-1] EQ 6, count)
                IF (count GT 0) THEN Freq_6_end[i,j] = Freq_6_end[i,j] + 1
                qq = WHERE(dummy5[N_ELEMENTS(dummy5)-1] EQ 7, count)
                IF (count GT 0) THEN Freq_7_end[i,j] = Freq_7_end[i,j] + 1                
                                                
              ENDIF
        ENDFOR
        Cascad_dur[i,j] = Cascad_dur[i,j]/Cascad_eve[i,j]
        ; Activate if you want to calculate average per event (maximum)
        ;Freq_1[i,j] = Freq_1[i,j]/Cascad_eve[i,j]     
        ;Freq_2[i,j] = Freq_2[i,j]/Cascad_eve[i,j]     
        ;Freq_3[i,j] = Freq_3[i,j]/Cascad_eve[i,j]     
        ;Freq_4[i,j] = Freq_4[i,j]/Cascad_eve[i,j]     
        ;Freq_5[i,j] = Freq_5[i,j]/Cascad_eve[i,j]     
        ;Freq_6[i,j] = Freq_6[i,j]/Cascad_eve[i,j]     
        ;Freq_7[i,j] = Freq_7[i,j]/Cascad_eve[i,j]                 
    ENDIF ELSE BEGIN
      Cascad_eve[i,j] = !VALUES.F_NAN
      Cascad_dur[i,j] = !VALUES.F_NAN
      Freq_1[i,j] = !VALUES.F_NAN
      Freq_2[i,j] = !VALUES.F_NAN
      Freq_3[i,j] = !VALUES.F_NAN
      Freq_4[i,j] = !VALUES.F_NAN
      Freq_5[i,j] = !VALUES.F_NAN
      Freq_6[i,j] = !VALUES.F_NAN
      Freq_7[i,j] = !VALUES.F_NAN
      Freq_1_start[i,j] = !VALUES.F_NAN
      Freq_2_start[i,j] = !VALUES.F_NAN
      Freq_3_start[i,j] = !VALUES.F_NAN
      Freq_4_start[i,j] = !VALUES.F_NAN
      Freq_5_start[i,j] = !VALUES.F_NAN
      Freq_6_start[i,j] = !VALUES.F_NAN
      Freq_7_start[i,j] = !VALUES.F_NAN 
      Freq_1_end[i,j] = !VALUES.F_NAN
      Freq_2_end[i,j] = !VALUES.F_NAN
      Freq_3_end[i,j] = !VALUES.F_NAN
      Freq_4_end[i,j] = !VALUES.F_NAN
      Freq_5_end[i,j] = !VALUES.F_NAN
      Freq_6_end[i,j] = !VALUES.F_NAN
      Freq_7_end[i,j] = !VALUES.F_NAN                                        
    ENDELSE
  ENDFOR
ENDFOR

; Filtering NAN
p = WHERE(LSM2 EQ 0, count)
IF (count GT 0) THEN Cascad_eve[p] = !VALUES.F_NAN
IF (count GT 0) THEN Cascad_dur[p] = !VALUES.F_NAN
IF (count GT 0) THEN Freq_1[p] = !VALUES.F_NAN
IF (count GT 0) THEN Freq_2[p] = !VALUES.F_NAN
IF (count GT 0) THEN Freq_3[p] = !VALUES.F_NAN
IF (count GT 0) THEN Freq_4[p] = !VALUES.F_NAN
IF (count GT 0) THEN Freq_5[p] = !VALUES.F_NAN
IF (count GT 0) THEN Freq_6[p] = !VALUES.F_NAN
IF (count GT 0) THEN Freq_7[p] = !VALUES.F_NAN
; testing location
; Stuttgart 48.7758° N, 9.1829° E    (31,33)
;testtt = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
;testtt[31,33] = 100

; Amiens 49.8941° N, 2.2958° E   (21,32)
;testtt = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat))
;testtt[21,32] = 100

; Madrid 40.4168° N, 3.7038° W   (13,45)

; ========================================================================
; checking results by plotting world map
tops = 0
thickness = 6

plsin = 0     ; single Figure 2
hotsp = 1     ; hotspot single hazard Figure 3a
complot = 0   ; compound hotspots Figure 3b
caseve = 0    ; number of cascading events Figure 4a
casdur = 0    ; duration of cascading events Figure 4b
casfreq = 0   ; cascading frequency Figure 5 and 6
testi = 0

IF casfreq EQ 1 THEN BEGIN
  DEVICE, true=24, decomposed=0, retain=2
  ;set the tops value to 1 for print in file, and not 1 for not reading
  IF tops NE 1 THEN BEGIN
    WINDOW,1,xsize=600,ysize=600
  ENDIF ELSE BEGIN
    SET_PLOT, 'ps'
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/Cascading_Frequency7.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /LAMBERT,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Frequency of all hazards concurrent appears in the dry hazards cascading events', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA)', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 30
  step = (500 - 0) / nlevels
  levels = IndGen(nlevels) * step + 0
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, Freq_2, lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[0,500], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
    Position=[0.05, 0.06,0.93, 0.09], Charsize=1.2    ;,/INVERTCOLORS
  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  XYOUTS,   0.5 , 0.11, 'day', /NORMAL,  charsize=1,alignment=0.5,FONT=0

  IF tops EQ 1 THEN BEGIN
    DEVICE,/close
    SET_PLOT, 'x'
  ENDIF

ENDIF

IF casdur EQ 1 THEN BEGIN
  DEVICE, true=24, decomposed=0, retain=2
  ;set the tops value to 1 for print in file, and not 1 for not reading
  IF tops NE 1 THEN BEGIN
    WINDOW,1,xsize=600,ysize=600
  ENDIF ELSE BEGIN
    SET_PLOT, 'ps'
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/Cascading_duration.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /LAMBERT,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Duration of dry hazards cascading events', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA)', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 30
  step = (80 - 0) / nlevels
  levels = IndGen(nlevels) * step + 0
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, Cascad_dur, lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[0,80], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
    Position=[0.05, 0.06,0.93, 0.09], Charsize=1.2    ;,/INVERTCOLORS
  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  XYOUTS,   0.5 , 0.11, 'day', /NORMAL,  charsize=1,alignment=0.5,FONT=0

  IF tops EQ 1 THEN BEGIN
    DEVICE,/close
    SET_PLOT, 'x'
  ENDIF

ENDIF

IF caseve EQ 1 THEN BEGIN
  DEVICE, true=24, decomposed=0, retain=2
  ;set the tops value to 1 for print in file, and not 1 for not reading
  IF tops NE 1 THEN BEGIN
    WINDOW,1,xsize=600,ysize=600
  ENDIF ELSE BEGIN
    SET_PLOT, 'ps'
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/Cascading_events.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /LAMBERT,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Number of dry hazards cascading events', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA)', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 30
  step = (60 - 0) / nlevels
  levels = IndGen(nlevels) * step + 0
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, Cascad_eve, lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[0,60], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
    Position=[0.05, 0.06,0.93, 0.09], Charsize=1.2    ;,/INVERTCOLORS
  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  XYOUTS,   0.5 , 0.11, 'number of event', /NORMAL,  charsize=1,alignment=0.5,FONT=0

  IF tops EQ 1 THEN BEGIN
    DEVICE,/close
    SET_PLOT, 'x'
  ENDIF

ENDIF

IF complot EQ 1 THEN BEGIN
  DEVICE, true=24, decomposed=0, retain=2
  ;set the tops value to 1 for print in file, and not 1 for not reading
  IF tops NE 1 THEN BEGIN
    WINDOW,1,xsize=600,ysize=600
  ENDIF ELSE BEGIN
    SET_PLOT, 'ps'
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/Hotspot_DRFR.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /LAMBERT,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Concurrent of drought and wildfires hotspots', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA)', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 20
  step = (140 - 0) / nlevels
  levels = IndGen(nlevels) * step + 0
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, Com_DRFR, lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[0,140], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
    Position=[0.05, 0.06,0.93, 0.09], Charsize=1.2    ;,/INVERTCOLORS
  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  XYOUTS,   0.5 , 0.11, 'number of day', /NORMAL,  charsize=1,alignment=0.5,FONT=0

  IF tops EQ 1 THEN BEGIN
    DEVICE,/close
    SET_PLOT, 'x'
  ENDIF

ENDIF

IF hotsp EQ 1 THEN BEGIN
  DEVICE, true=24, decomposed=0, retain=2
  ;set the tops value to 1 for print in file, and not 1 for not reading
  IF tops NE 1 THEN BEGIN
    WINDOW,1,xsize=600,ysize=600
  ENDIF ELSE BEGIN
    SET_PLOT, 'ps'
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/Hotspot_Fires.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /LAMBERT,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Wildfires hotspots', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA)', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 20
  step = (150 - 120) / nlevels
  levels = IndGen(nlevels) * step + 120
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, Num_fires, lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[120,150], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
    Position=[0.05, 0.06,0.93, 0.09], Charsize=1.2    ;,/INVERTCOLORS
  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  XYOUTS,   0.5 , 0.11, 'number of day', /NORMAL,  charsize=1,alignment=0.5,FONT=0

  IF tops EQ 1 THEN BEGIN
    DEVICE,/close
    SET_PLOT, 'x'
  ENDIF

ENDIF

IF plsin EQ 1 THEN BEGIN
  DEVICE, true=24, decomposed=0, retain=2
  ;set the tops value to 1 for print in file, and not 1 for not reading
  IF tops NE 1 THEN BEGIN
    WINDOW,1,xsize=600,ysize=600
  ENDIF ELSE BEGIN
    SET_PLOT, 'ps'
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/Fires_2003.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /LAMBERT,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Wildfires occurrence year 2003', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA)', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 30
  step = (92 - 0) / nlevels
  levels = IndGen(nlevels) * step + 0
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, fires_03tot, lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[0,92], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
    Position=[0.05, 0.06,0.93, 0.09], Charsize=1.2    ;,/INVERTCOLORS
  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  XYOUTS,   0.5 , 0.11, 'number of day', /NORMAL,  charsize=1,alignment=0.5,FONT=0

  IF tops EQ 1 THEN BEGIN
    DEVICE,/close
    SET_PLOT, 'x'
  ENDIF

ENDIF

IF testi EQ 1 THEN BEGIN
  DEVICE, true=24, decomposed=0, retain=2
  ;set the tops value to 1 for print in file, and not 1 for not reading
  IF tops NE 1 THEN BEGIN
    WINDOW,1,xsize=600,ysize=600
  ENDIF ELSE BEGIN
    SET_PLOT, 'ps'
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/Testing_LSM.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  everyOther = Reform(Rebin([1,0], 2, 6), 12)
  MAP_SET, /robinson, /isotropic,/GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1.4

  XYOUTS,   0.5 , 0.95, 'Testing LSM', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 4
  step = (-0.75 - (-2)) / nlevels
  levels = IndGen(nlevels) * step + (-2)

  SetDecomposedState, 0, CurrentState=currentState
  Contour, LSM2, lon, lat, /Cell_Fill, C_Colors=FSC_COLOR('red'), /overplot, color=cgcolor('black'), background=cgcolor('white')
  Contour, testtt, lon, lat, /Cell_Fill, C_Colors=FSC_COLOR('green'), /overplot, color=cgcolor('black'), background=cgcolor('white')
  SetDecomposedState, currentState

  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  IF tops EQ 1 THEN BEGIN
    DEVICE,/close
    SET_PLOT, 'x'
  ENDIF

ENDIF

END