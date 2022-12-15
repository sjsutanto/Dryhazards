; New code for dry hazard calculation after reviewer comments
; June 2019

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

  ; for lsm drought
  LSM = TOTAL(New_SM,3,/NaN)
  
  ; For LSM same with fires
  LSM2 = REFORM(fires[*,*,0])
  
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
hea_yr2 = TOTAL(hea_yr,4,/NaN)
dro_yr2 = TOTAL(dro_yr,4,/NaN)
fir_yr2 = TOTAL(fir_yr,4,/NaN)

  ; transform binary SM is 2
  qq = WHERE(dro_yr EQ 1, count)
  IF count NE 0 THEN dro_yr[qq] = 2
  ; transform binary fires is 4
  qq = WHERE(fir_yr EQ 1, count)
  IF count NE 0 THEN fir_yr[qq] = 4

; ========================================================================
; Compound hazards
Compound_dry = dro_yr + hea_yr + fir_yr

; now plot the hotspot for compound hazard
Com_DRHW = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)       ; for DR + HW
Com_DRFR = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)  ; for DR + FR
Com_HWFR = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)  ; for HW + FR
Com_ALL = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)    ; for all 3 Hazards

FOR i=0, N_ELEMENTS(lon)-1 DO BEGIN
  FOR j=0, N_ELEMENTS(lat)-1 DO BEGIN
    IF LSM2[i,j] NE -3.40000e+38 THEN BEGIN  
      FOR k=0, 26 DO BEGIN
      pp = WHERE(Compound_dry[i,j,k,*] EQ 3, count)
      Com_DRHW[i,j,k] = count
      pp = WHERE(Compound_dry[i,j,k,*] EQ 5, count)
      Com_HWFR[i,j,k] = count
      pp = WHERE(Compound_dry[i,j,k,*] EQ 6, count)
      Com_DRFR[i,j,k] = count
      pp = WHERE(Compound_dry[i,j,k,*] EQ 7, count)
      Com_ALL[i,j,k] = count
      ENDFOR
    ENDIF ELSE BEGIN
      Com_DRHW[i,j,*] = !VALUES.F_NAN
      Com_HWFR[i,j,*] = !VALUES.F_NAN
      Com_DRFR[i,j,*] = !VALUES.F_NAN
      Com_ALL[i,j,*] = !VALUES.F_NAN
    ENDELSE
  ENDFOR
ENDFOR

; make it percentage
Com_DRHW_percent = Com_DRHW/92*100
Com_HWFR_percent = Com_HWFR/92*100
Com_DRFR_percent = Com_DRFR/92*100
Com_ALL_percent = Com_ALL/92*100

Com_DRHW_AVG = AVG(Com_DRHW_percent,2,/NaN)*10 
Com_HWFR_AVG = AVG(Com_HWFR_percent,2,/NaN)*10 
Com_DRFR_AVG = AVG(Com_DRFR_percent,2,/NaN)*10 
Com_ALL_AVG = AVG(Com_ALL_percent,2,/NaN)*10  

;pp = WHERE(Com_HWFR_percent EQ 0, count)
;IF (count GT 0) THEN Com_HWFR_percent[pp] = !VALUES.F_NAN

ENDIF

;FOR i=0, 26 DO BEGIN
;  print, max(Com_ALL_percent[*,*,i],/NaN)  
;ENDFOR

; ========================================================================
; Cascading events
Cascad_dry = Compound_dry

Cascad_eve = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)    ; for cascading events
Cascad_dur = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)    ; for cascading duration
Freq_1 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_2 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_3 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_4 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_5 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_6 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_7 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)

Freq_1_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_2_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_3_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_4_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_5_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_6_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_7_start = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)

Freq_1_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_2_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_3_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_4_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_5_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_6_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
Freq_7_end = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)

;STOP
FOR i=0, N_ELEMENTS(lon)-1 DO BEGIN
  FOR j=0, N_ELEMENTS(lat)-1 DO BEGIN
    IF LSM2[i,j] NE -3.40000e+38 THEN BEGIN
      FOR zz=0, 26 DO BEGIN
      dummy = REFORM(Cascad_dry[i,j,zz,*])
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
      ; Here I will put a filter for combining events into 1 if there are 7 days without event.
      dummy2bb = dummy2[1:*]
      dummy2bb = [dummy2bb,dummy3[N_ELEMENTS(dummy3)-1]]
      dummyxx = dummy2bb-dummy3

;***** SONY: change this value in order to have different zero gap in cascading event (GE 1, LT 7)  
      pp = WHERE(dummyxx GE 1, count)   ; because dummy2 start with zero so it is OK to use GE 1
      ;pp = WHERE((dummyxx GE 1) AND (dummyxx GT 21), count)
      IF count GE 1 THEN BEGIN
      dummy2cc = [dummy2[0],dummy2bb[pp]]
      dummy3cc = [dummy3[pp],dummy3[N_ELEMENTS(dummy3)-1]]
      ; since dummy2cc always starts from 0, then we move back 1
      dummy2cc = dummy2cc+1
      ENDIF ELSE BEGIN
      dummy2cc = dummy2[0]+1
      dummy3cc = dummy3[N_ELEMENTS(dummy3)-1]  
      ENDELSE
      ;STOP
      FOR kk=0, N_ELEMENTS(dummy2cc)-1 DO BEGIN
        dummy4 = REFORM(Cascad_dry[i,j,zz,dummy2cc[kk]:dummy3cc[kk]])
        dummy5 = dummy4[UNIQ(dummy4)]
        ; removing zero value
        qq = WHERE(dummy5 NE 0, count)
        dummy5 = dummy5[qq]
        dummy6 = dummy5[UNIQ(dummy5)]
        ;STOP
        IF N_ELEMENTS(dummy6) GT 1 THEN BEGIN
          lengt = dummy3cc[kk]-dummy2cc[kk]+1 ; because it is lenght and not array. e.g. 20-18=2, in fact it is array 18-20, which is 3 lenght
          Cascad_eve[i,j,zz] = Cascad_eve[i,j,zz] + 1
          Cascad_dur[i,j,zz] = Cascad_dur[i,j,zz] + lengt
          qqq = WHERE(dummy4 EQ 1, count)
          Freq_1[i,j,zz] = Freq_1[i,j,zz] + count
          qqq = WHERE(dummy4 EQ 2, count)
          Freq_2[i,j,zz] = Freq_2[i,j,zz] + count
          qqq = WHERE(dummy4 EQ 3, count)
          Freq_3[i,j,zz] = Freq_3[i,j,zz] + count
          qqq = WHERE(dummy4 EQ 4, count)
          Freq_4[i,j,zz] = Freq_4[i,j,zz] + count
          qqq = WHERE(dummy4 EQ 5, count)
          Freq_5[i,j,zz] = Freq_5[i,j,zz] + count
          qqq = WHERE(dummy4 EQ 6, count)
          Freq_6[i,j,zz] = Freq_6[i,j,zz] + count
          qqq = WHERE(dummy4 EQ 7, count)
          Freq_7[i,j,zz] = Freq_7[i,j,zz] + count
          ; for starting.
          qq = WHERE(dummy4[0] EQ 1, count)
          IF (count GT 0) THEN Freq_1_start[i,j,zz] = Freq_1_start[i,j,zz] + 1
          qq = WHERE(dummy4[0] EQ 2, count)
          IF (count GT 0) THEN Freq_2_start[i,j,zz] = Freq_2_start[i,j,zz] + 1
          qq = WHERE(dummy4[0] EQ 3, count)
          IF (count GT 0) THEN Freq_3_start[i,j,zz] = Freq_3_start[i,j,zz] + 1
          qq = WHERE(dummy4[0] EQ 4, count)
          IF (count GT 0) THEN Freq_4_start[i,j,zz] = Freq_4_start[i,j,zz] + 1
          qq = WHERE(dummy4[0] EQ 5, count)
          IF (count GT 0) THEN Freq_5_start[i,j,zz] = Freq_5_start[i,j,zz] + 1
          qq = WHERE(dummy4[0] EQ 6, count)
          IF (count GT 0) THEN Freq_6_start[i,j,zz] = Freq_6_start[i,j,zz] + 1
          qq = WHERE(dummy4[0] EQ 7, count)
          IF (count GT 0) THEN Freq_7_start[i,j,zz] = Freq_7_start[i,j,zz] + 1
          ; for Ending.
          qq = WHERE(dummy4[N_ELEMENTS(dummy4)-1] EQ 1, count)
          IF (count GT 0) THEN Freq_1_end[i,j,zz] = Freq_1_end[i,j,zz] + 1
          qq = WHERE(dummy4[N_ELEMENTS(dummy4)-1] EQ 2, count)
          IF (count GT 0) THEN Freq_2_end[i,j,zz] = Freq_2_end[i,j,zz] + 1
          qq = WHERE(dummy4[N_ELEMENTS(dummy4)-1] EQ 3, count)
          IF (count GT 0) THEN Freq_3_end[i,j,zz] = Freq_3_end[i,j,zz] + 1
          qq = WHERE(dummy4[N_ELEMENTS(dummy4)-1] EQ 4, count)
          IF (count GT 0) THEN Freq_4_end[i,j,zz] = Freq_4_end[i,j,zz] + 1
          qq = WHERE(dummy4[N_ELEMENTS(dummy4)-1] EQ 5, count)
          IF (count GT 0) THEN Freq_5_end[i,j,zz] = Freq_5_end[i,j,zz] + 1
          qq = WHERE(dummy4[N_ELEMENTS(dummy4)-1] EQ 6, count)
          IF (count GT 0) THEN Freq_6_end[i,j,zz] = Freq_6_end[i,j,zz] + 1
          qq = WHERE(dummy4[N_ELEMENTS(dummy4)-1] EQ 7, count)
          IF (count GT 0) THEN Freq_7_end[i,j,zz] = Freq_7_end[i,j,zz] + 1        
        ENDIF
      ENDFOR
      ;STOP                  
      ENDFOR
      ;STOP
      ; Average duration
      Cascad_dur[i,j,*] = Cascad_dur[i,j,*]/Cascad_eve[i,j,*]
    ENDIF ELSE BEGIN
      Cascad_eve[i,j,*] = !VALUES.F_NAN
      Cascad_dur[i,j,*] = !VALUES.F_NAN
      Freq_1[i,j,*] = !VALUES.F_NAN
      Freq_2[i,j,*] = !VALUES.F_NAN
      Freq_3[i,j,*] = !VALUES.F_NAN
      Freq_4[i,j,*] = !VALUES.F_NAN
      Freq_5[i,j,*] = !VALUES.F_NAN
      Freq_6[i,j,*] = !VALUES.F_NAN
      Freq_7[i,j,*] = !VALUES.F_NAN
      Freq_1_start[i,j,*] = !VALUES.F_NAN
      Freq_2_start[i,j,*] = !VALUES.F_NAN
      Freq_3_start[i,j,*] = !VALUES.F_NAN
      Freq_4_start[i,j,*] = !VALUES.F_NAN
      Freq_5_start[i,j,*] = !VALUES.F_NAN
      Freq_6_start[i,j,*] = !VALUES.F_NAN
      Freq_7_start[i,j,*] = !VALUES.F_NAN
      Freq_1_end[i,j,*] = !VALUES.F_NAN
      Freq_2_end[i,j,*] = !VALUES.F_NAN
      Freq_3_end[i,j,*] = !VALUES.F_NAN
      Freq_4_end[i,j,*] = !VALUES.F_NAN
      Freq_5_end[i,j,*] = !VALUES.F_NAN
      Freq_6_end[i,j,*] = !VALUES.F_NAN
      Freq_7_end[i,j,*] = !VALUES.F_NAN
    ENDELSE
  ENDFOR
ENDFOR

; Average for every year
; trying to normalize
Cascad_eve2 = AVG(Cascad_eve,2,/NaN)*10
Cascad_dur2 = AVG(Cascad_dur,2,/NaN)

; Filtering NAN
p = WHERE(LSM EQ 0, count)
IF (count GT 0) THEN Cascad_eve2[p] = 0
IF (count GT 0) THEN Cascad_dur2[p] = 0

Freq_1_avg = AVG(Freq_1,2,/NaN)*10
IF (count GT 0) THEN Freq_1_avg[p] = 0

Freq_2_avg = AVG(Freq_2,2,/NaN)   ; no need for filtering ocean

Freq_4_avg = AVG(Freq_4,2,/NaN)*10
IF (count GT 0) THEN Freq_4_avg[p] = 0

Freq_3_avg = AVG(Freq_3,2,/NaN)*10
IF (count GT 0) THEN Freq_3_avg[p] = 0

Freq_5_avg = AVG(Freq_5,2,/NaN)*10
IF (count GT 0) THEN Freq_5_avg[p] = 0

Freq_6_avg = AVG(Freq_6,2,/NaN)*10
IF (count GT 0) THEN Freq_6_avg[p] = 0

Freq_7_avg = AVG(Freq_7,2,/NaN)*10
IF (count GT 0) THEN Freq_7_avg[p] = 0

; filter for single hazard
LSM3 = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),27)
FOR i=0, 26 DO BEGIN
  LSM3[*,*,i] = LSM
ENDFOR

pp = WHERE(LSM3 EQ 0, count)
IF (count GT 0) THEN hea_yr2[pp] = 0
IF (count GT 0) THEN dro_yr2[pp] = 0
IF (count GT 0) THEN fir_yr2[pp] = 0
; in percent
hea_yr2 = hea_yr2/92*100
dro_yr2 = dro_yr2/92*100
fir_yr2 = fir_yr2/92*100

; Calculate the frequency dry hazard during a cascading event
Freq_1_eve = TOTAL(Freq_1,3,/NaN)/2484*100
Freq_2_eve = TOTAL(Freq_2,3,/NaN)/2484*100
Freq_3_eve = TOTAL(Freq_3,3,/NaN)/2484*100
Freq_4_eve = TOTAL(Freq_4,3,/NaN)/2484*100
Freq_5_eve = TOTAL(Freq_5,3,/NaN)/2484*100
Freq_6_eve = TOTAL(Freq_6,3,/NaN)/2484*100
Freq_7_eve = TOTAL(Freq_7,3,/NaN)/2484*100

; Calculate the frequency dry hazard starting with during a cascading event
Freq_1_sta = TOTAL(Freq_1_start,3,/NaN)/2484*100
Freq_2_sta = TOTAL(Freq_2_start,3,/NaN)/2484*100
Freq_3_sta = TOTAL(Freq_3_start,3,/NaN)/2484*100
Freq_4_sta = TOTAL(Freq_4_start,3,/NaN)/2484*100
Freq_5_sta = TOTAL(Freq_5_start,3,/NaN)/2484*100
Freq_6_sta = TOTAL(Freq_6_start,3,/NaN)/2484*100
Freq_7_sta = TOTAL(Freq_7_start,3,/NaN)/2484*100

; Calculate the frequency dry hazard ending with during a cascading event
Freq_1_end = TOTAL(Freq_1_end,3,/NaN)/2484*100
Freq_2_end = TOTAL(Freq_2_end,3,/NaN)/2484*100
Freq_3_end = TOTAL(Freq_3_end,3,/NaN)/2484*100
Freq_4_end = TOTAL(Freq_4_end,3,/NaN)/2484*100
Freq_5_end = TOTAL(Freq_5_end,3,/NaN)/2484*100
Freq_6_end = TOTAL(Freq_6_end,3,/NaN)/2484*100
Freq_7_end = TOTAL(Freq_7_end,3,/NaN)/2484*100

; Change the name to print
;print, max(Freq_1_eve,/NaN)
;print, max(Freq_2_eve,/NaN)
;print, max(Freq_3_eve,/NaN)
;print, max(Freq_4_eve,/NaN)
;print, max(Freq_5_eve,/NaN)
;print, max(Freq_6_eve,/NaN)
;print, max(Freq_7_eve,/NaN)

; calculate percentile
pp = WHERE(LSM GT 0, count)

p25 = ROUND(0.25*count)-1   ; Median
P50 = ROUND(0.50*count)-1   ; Median
P75 = ROUND(0.75*count)-1   ; Median

Freq_1_eve1 = Freq_1_eve[pp]
Freq_1_eve2 = Freq_1_eve1[SORT(Freq_1_eve1)] ; from small to big
Freq_2_eve1 = Freq_2_eve[pp]
Freq_2_eve2 = Freq_2_eve1[SORT(Freq_2_eve1)] ; from small to big
Freq_4_eve1 = Freq_4_eve[pp]
Freq_4_eve2 = Freq_4_eve1[SORT(Freq_4_eve1)] ; from small to big
Freq_3_eve1 = Freq_3_eve[pp]
Freq_3_eve2 = Freq_3_eve1[SORT(Freq_3_eve1)] ; from small to big
Freq_5_eve1 = Freq_5_eve[pp]
Freq_5_eve2 = Freq_5_eve1[SORT(Freq_5_eve1)] ; from small to big
Freq_6_eve1 = Freq_6_eve[pp]
Freq_6_eve2 = Freq_6_eve1[SORT(Freq_6_eve1)] ; from small to big
Freq_7_eve1 = Freq_7_eve[pp]
Freq_7_eve2 = Freq_7_eve1[SORT(Freq_7_eve1)] ; from small to big

Freq_1_sta1 = Freq_1_sta[pp]
Freq_1_sta2 = Freq_1_sta1[SORT(Freq_1_sta1)] ; from small to big
Freq_2_sta1 = Freq_2_sta[pp]
Freq_2_sta2 = Freq_2_sta1[SORT(Freq_2_sta1)] ; from small to big
Freq_4_sta1 = Freq_4_sta[pp]
Freq_4_sta2 = Freq_4_sta1[SORT(Freq_4_sta1)] ; from small to big
Freq_3_sta1 = Freq_3_sta[pp]
Freq_3_sta2 = Freq_3_sta1[SORT(Freq_3_sta1)] ; from small to big
Freq_5_sta1 = Freq_5_sta[pp]
Freq_5_sta2 = Freq_5_sta1[SORT(Freq_5_sta1)] ; from small to big
Freq_6_sta1 = Freq_6_sta[pp]
Freq_6_sta2 = Freq_6_sta1[SORT(Freq_6_sta1)] ; from small to big
Freq_7_sta1 = Freq_7_sta[pp]
Freq_7_sta2 = Freq_7_sta1[SORT(Freq_7_sta1)] ; from small to big


Freq_1_end1 = Freq_1_end[pp]
Freq_1_end2 = Freq_1_end1[SORT(Freq_1_end1)] ; from small to big
Freq_2_end1 = Freq_2_end[pp]
Freq_2_end2 = Freq_2_end1[SORT(Freq_2_end1)] ; from small to big
Freq_4_end1 = Freq_4_end[pp]
Freq_4_end2 = Freq_4_end1[SORT(Freq_4_end1)] ; from small to big
Freq_3_end1 = Freq_3_end[pp]
Freq_3_end2 = Freq_3_end1[SORT(Freq_3_end1)] ; from small to big
Freq_5_end1 = Freq_5_end[pp]
Freq_5_end2 = Freq_5_end1[SORT(Freq_5_end1)] ; from small to big
Freq_6_end1 = Freq_6_end[pp]
Freq_6_end2 = Freq_6_end1[SORT(Freq_6_end1)] ; from small to big
Freq_7_end1 = Freq_7_end[pp]
Freq_7_end2 = Freq_7_end1[SORT(Freq_7_end1)] ; from small to big

; Try to make time series of Table 3 for every year
Freq_1_yr_max = FLTARR(27)
Freq_2_yr_max = FLTARR(27)
Freq_3_yr_max = FLTARR(27)
Freq_4_yr_max = FLTARR(27)
Freq_5_yr_max = FLTARR(27)
Freq_6_yr_max = FLTARR(27)
Freq_7_yr_max = FLTARR(27)
FOR i=0, 26 DO BEGIN
  Freq_1_yr_max[i] = MAX(Freq_1[*,*,i],/NaN)
  Freq_2_yr_max[i] = MAX(Freq_2[*,*,i],/NaN)
  Freq_4_yr_max[i] = MAX(Freq_4[*,*,i],/NaN)
ENDFOR

axist = FINDGEN(27)+1
; ========================================================================
; checking results by plotting world map
tops = 0
thickness = 6

hotsp = 0     ; FIgure A1
complot = 0    ; FIgure 3
caseve = 0    ; FIgure 4a    also for FIg A2
casdur = 0    ; Figure 4b
casfreq = 0   ; cascading frequency Figure 5
casfreq2 = 0   ; cascading frequency Figure 6 
ts_freq = 1

IF ts_freq EQ 1 THEN BEGIN

  !p.font=0
  ;set_plot,'ps'
  ;device,file='/Users/sutanto/Documents/Postdoc/IDL_Plot/ACA_Coordinate/Reservoir/Compare_WB_Obs.ps',XSIZE=35,YSIZE=15, SCALE_FACTOR=0.7, /LANDSCAPE

  PLOT, axist,Freq_1_yr_max,YRANGE=[0,max(Freq_2_yr_max,/NaN)],TITLE='Maximum frequency of each hazard in cascading events', XRANGE=[0,27], $
    XTITLE='Year', YTITLE='Frequency of Hazard', LINESTYLE=0, charthick=thickness,xthick=thickness,ythick=thickness,thick=thickness2
  OPLOT, axist,Freq_2_yr_max, LINESTYLE=0,thick=thickness2, color=[FSC_COLOR('red')]
  OPLOT, axist,Freq_4_yr_max, LINESTYLE=0,thick=thickness2, color=[FSC_COLOR('orange')]

  LEGEND,['Heatwave','Drought','Wildfire'],LINESTYLE=[0,0,0],colors=[FSC_COLOR('black'),FSC_COLOR('red'),FSC_COLOR('orange')],thick=thickness,charthick=thickness

  ;device,/close
  ;set_plot,'x'

ENDIF

IF casfreq2 EQ 1 THEN BEGIN
  DEVICE, true=24, decomposed=0, retain=2
  ;set the tops value to 1 for print in file, and not 1 for not reading
  IF tops NE 1 THEN BEGIN
    WINDOW,1,xsize=600,ysize=600
  ENDIF ELSE BEGIN
    SET_PLOT, 'ps'
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/New_Plots_R1/Fig6_Cascading_Frequency_All.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /robinson,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Frequency of heatwave-wildfire-drought concurrent appears in the dry hazards cascading events', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA)', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 12
  step = (40 - 0) / nlevels
  levels = IndGen(nlevels) * step + 1
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, Freq_7_avg, lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[1,40], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
    Position=[0.05, 0.06,0.93, 0.09], Charsize=1.2    ;,/INVERTCOLORS
  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  XYOUTS,   0.5 , 0.11, 'Number of time x 10', /NORMAL,  charsize=1,alignment=0.5,FONT=0

  IF tops EQ 1 THEN BEGIN
    DEVICE,/close
    SET_PLOT, 'x'
  ENDIF

ENDIF

IF casfreq EQ 1 THEN BEGIN
  DEVICE, true=24, decomposed=0, retain=2
  ;set the tops value to 1 for print in file, and not 1 for not reading
  IF tops NE 1 THEN BEGIN
    WINDOW,1,xsize=600,ysize=600
  ENDIF ELSE BEGIN
    SET_PLOT, 'ps'
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/New_Plots_R1/Fig5_Cascading_Frequency_FR.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /robinson,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Frequency of wildfire appears in a cascading event', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA)', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 12
  step = (25 - 0) / nlevels
  levels = IndGen(nlevels) * step + 1
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, Freq_4_avg, lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[1,25], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
    Position=[0.05, 0.06,0.93, 0.09], Charsize=1.2    ;,/INVERTCOLORS
  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  XYOUTS,   0.5 , 0.11, 'Number of time', /NORMAL,  charsize=1,alignment=0.5,FONT=0

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
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/New_Plots_R1/Fig4_Cascading_duration.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /robinson,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Duration of dry hazards cascading events', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA)', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 20
  step = (90 - 1) / nlevels
  levels = IndGen(nlevels) * step + 1
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, Cascad_dur2, lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[1,90], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
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
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/New_Plots_R1/Fig4_Cascading_events.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /robinson,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Number of dry hazards cascading events', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA)', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 15
  step = (30 - 1) / nlevels
  levels = IndGen(nlevels) * step + 1
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, Cascad_eve2, lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[1,30], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
    Position=[0.05, 0.06,0.93, 0.09], Charsize=1.2    ;,/INVERTCOLORS
  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  XYOUTS,   0.5 , 0.11, 'number of event (x 10)', /NORMAL,  charsize=1,alignment=0.5,FONT=0

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
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/New_Plots_R1/Fig3_Hotspot_DRFR.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /robinson,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Concurent of drought-wildfire hotspots', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA) year 2003', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 12
  step = (40 - 1) / nlevels
  levels = IndGen(nlevels) * step + 1
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, Com_DRFR_percent[*,*,13], lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[1,40], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
    Position=[0.05, 0.06,0.93, 0.09], Charsize=1.2    ;,/INVERTCOLORS
  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  XYOUTS,   0.5 , 0.11, 'Number of days in percent (% of total)', /NORMAL,  charsize=1,alignment=0.5,FONT=0

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
    DEVICE, filename='/Users/sutanto/Documents/Postdoc/IDL_Plot/OVIMA/Paper/New_Plots_R1/FigA1_Hotspot_FR_2015.ps', /color, /encapsulated, bits=8, $
      SET_FONT='Helvetica', /TT_FONT, /portrait,xsize=20, ysize=20
  ENDELSE

  MAP_SET, /robinson,/ISOTROPIC, /GRID, /CONTINENTS, limit = [33, -25, 75, 35],color=0 , position=[0.04, 0.15, 0.95, 0.95], $
    xmargin = [4,4], ymargin = [8,2], e_horizon = {fill:1,color:223}, mlinethick = 1., /noerase, LATALIGN=1, LONALIGN=1, charsize = 1

  XYOUTS,   0.5 , 0.95, 'Wildfire hotspots', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0
  XYOUTS,   0.5 , 0.92, 'Summer period (JJA)', /NORMAL,  charsize=1.2,alignment=0.5,FONT=0

  nlevels = 10
  step = (60 - 0) / nlevels
  levels = IndGen(nlevels) * step + 1
  cgLoadCT, 25, NColors=nlevels, Bottom=1, /Reverse, /Brewer
  SetDecomposedState, 0, CurrentState=currentState
  Contour, fir_yr2[*,*,25], lon, lat, /Cell_Fill, Levels=levels, C_Colors=IndGen(nlevels)+1, /overplot    ; Data with Rhine dot
  SetDecomposedState, currentState

  cgColorbar, Range=[1,60], NColors=nlevels, Bottom=1,XTicklen=1, FONT=0, $
    Position=[0.05, 0.06,0.93, 0.09], Charsize=1.2    ;,/INVERTCOLORS
  Map_Continents,/COUNTRIES, COLOR=0, coasts=1, MLINETHICK=1

  XYOUTS,   0.5 , 0.11, 'Number of days in percent (% of total)', /NORMAL,  charsize=1,alignment=0.5,FONT=0

  IF tops EQ 1 THEN BEGIN
    DEVICE,/close
    SET_PLOT, 'x'
  ENDIF

ENDIF

END
