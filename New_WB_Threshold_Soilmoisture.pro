;; This script calculates the threshold values for soil moisture (upper part)
;; Data is obtained from the new Lisflood forcing data 
;; Output is raster map in NetCDF/sav file
;;
;;            Author: samuel.sutanto@wur.nl
;;        Created on: 14-August-2018
;; Last modification: -
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

read = 1

IF read EQ 1 THEN BEGIN

  ; ========================================================================
  ; Loading the data
  files = FILE_SEARCH('/Volumes/Sony_EXTHD2/New_WB/tha.nc')
  peyl_readnc, lon, file=files, var_name='x'
  peyl_readnc, lat, file=files, var_name='y'
  peyl_readnc, sm14, file=files, var_name='tha'
  
  ; Loading the data 2015-2016
  files = FILE_SEARCH('/Volumes/Sony_EXTHD2/New_WB/tha20152016.nc')
  peyl_readnc, sm15, file=files, var_name='tha'
  
  sm = [[[sm14]],[[sm15]]]
  UNDEFINE, sm14
  UNDEFINE, sm15
  
  ;SAVE,Mont,FILENAME = 'Month_Number_19792016.sav'
  RESTORE, 'Month_Number_19792016.sav'

  ;Select only 1990 till 2012, and then 2014 and 2016
  Mont = Mont[4018:*]   ; 1990-2012

  ; Open landseamask
  files = FILE_SEARCH('/Users/sutanto/Landseamask_New_Lisflood.nc')
  peyl_readnc, mask, file=files, var_name='mask'

  print, 'finish loading data'
  dates = FINDGEN(N_ELEMENTS(Mont))

  ;STOP
  ; ========================================================================
  ;; calculate monthly threshold
  threshold = 80  ; thresholds in % of FDC

  mmon = FINDGEN(12)+1
  Pjanthres_month = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),N_ELEMENTS(mmon))

  FOR k = 0, N_ELEMENTS(mmon)-1 DO BEGIN
    ppp = WHERE(Mont EQ mmon[k], count)
    IF (count GT 0) THEN Pmonth = sm[*,*,ppp]
    FOR i=0, N_ELEMENTS(lon)-1 DO BEGIN
      FOR j=0, N_ELEMENTS(lat)-1 DO BEGIN
        IF (mask[i,j] EQ 1) THEN BEGIN
        Pmonth2 = Pmonth[i,j,*]

        Pjansort = Pmonth2[REVERSE(SORT(Pmonth2))]
        Xseq = FINDGEN(N_ELEMENTS(ppp))/N_ELEMENTS(ppp)*100
        mm = WHERE(Xseq GE threshold, count)
        IF (count GT 0) THEN Pjanthres = Pjansort[mm]

        Pjanthres_month[i,j,k] = Pjanthres[0]
        ENDIF ELSE BEGIN
        Pjanthres_month[i,j,k] = !VALUES.F_NAN
        ENDELSE
      ENDFOR
    ENDFOR
    print, STRING('finish',mmon[k])
  ENDFOR

  UNDEFINE, sm
  ;STOP

  ; fill in the threshold values (Q80) to all month
  jump_thres_Psim = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),N_ELEMENTS(dates))

  FOR k = 0, N_ELEMENTS(mmon)-1 DO BEGIN
    ppp = WHERE(Mont EQ mmon[k])
    dummy = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),N_ELEMENTS(ppp))
    FOR i=0, N_ELEMENTS(ppp)-1 DO BEGIN
      dummy[*,*,i] = Pjanthres_month[*,*,k]
    ENDFOR
    jump_thres_Psim[*,*,ppp] = dummy
  ENDFOR

  UNDEFINE, Pjanthres_month
  UNDEFINE, dummy

  ; We only need threshold values for leap and no leap years
  ; 1979-1980 = 365+366
  ; moving average period for monthly threshold (days)
  MA = 30
  MA2 = (MA/2)   ; uneven MA-values need to be rounded upward

  twoyears = FINDGEN(365+366)

  j = (MA-1)
  temp_threshold_Psim = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),N_ELEMENTS(twoyears)+j)

  FOR i=0, N_ELEMENTS(twoyears)-MA DO BEGIN
    temp_threshold_Psim[*,*,i+j] = TOTAL(jump_thres_Psim[*,*,i:i+j],3,/NaN)/MA
  ENDFOR
  print, 'finish temp thres psim'

  UNDEFINE, jump_thres_Psim

  raw_threshold_Psim = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),N_ELEMENTS(twoyears))
  raw_threshold_Psim[*,*,0:(N_ELEMENTS(twoyears)-MA2)] = temp_threshold_Psim[*,*,MA2:N_ELEMENTS(twoyears)]    ; 1 because IDL starts from zero

  UNDEFINE, temp_threshold_Psim

  ;; first 15 days is the same with first 15 days in the year after!
  threshold_Psim = raw_threshold_Psim
  year1 = 365 ; days in 1st year (1990)
  yearlast = 366  ; 1980

  threshold_Psim[*,*,0:MA2-1] = raw_threshold_Psim[*,*,year1:(year1+MA2-1)]

  ;; last 15 days is the same with last 15 days in the year before
  threshold_Psim[*,*,(N_ELEMENTS(twoyears)-MA2):(N_ELEMENTS(twoyears)-1)] = raw_threshold_Psim[*,*,(N_ELEMENTS(twoyears)-yearlast-MA2):(N_ELEMENTS(twoyears)-yearlast-1)]

  UNDEFINE, raw_threshold_Psim

  print, 'saving threshold_Psim'
  months = FINDGEN(N_ELEMENTS(threshold_Psim[0,0,*]))
  ; write 1 for saving threshold results to NetCDF
  savsgi = 1

  IF savsgi EQ 1 THEN BEGIN
    ; saving netcdf
    description = 'Threshold WB values for soil moisture derived from New LISFLOOD WB 1990-2016'
    fid=NCDF_CREATE( '/Volumes/Sony_EXTHD2/New_WB/Thres_Psim_soilmoisture_NEW_LISFLOOD_Daily.nc', /CLOBBER,/NETCDF4_FORMAT)
    NCDF_ATTPUT, fid, 'Description', description, /GLOBAL

    longid=NCDF_DIMDEF(fid,'longitude',N_ELEMENTS(lon))
    latid=NCDF_DIMDEF(fid,'latitude',N_ELEMENTS(lat))
    timeid=NCDF_DIMDEF(fid,'Time',N_ELEMENTS(months))

    Thresvid=NCDF_VARDEF(fid,'threshold_Psim', [longid,latid,timeid],/FLOAT)
    longvid=NCDF_VARDEF(fid,'longitude', [longid],/FLOAT)
    latvid=NCDF_VARDEF(fid,'latitude', [latid],/FLOAT)
    timevid=NCDF_VARDEF(fid,'Time', [timeid],/FLOAT)

    NCDF_ATTPUT, fid, 'threshold_Psim', 'units', 'mm'
    NCDF_ATTPUT, fid, 'threshold_Psim', 'title', 'Threshold value'
    NCDF_ATTPUT, fid, 'longitude', 'standard_name', 'projection_lon_coordinate'
    NCDF_ATTPUT, fid, 'longitude', 'long_name', 'longitude coordinate of projection'
    NCDF_ATTPUT, fid, 'longitude', 'units', 'm'
    NCDF_ATTPUT, fid, 'longitude', 'axis', 'longitude'

    NCDF_ATTPUT, fid, 'latitude', 'standard_name', 'projection_lat_coordinate'
    NCDF_ATTPUT, fid, 'latitude', 'long_name', 'latitude coordinate of projection'
    NCDF_ATTPUT, fid, 'latitude', 'units', 'm'
    NCDF_ATTPUT, fid, 'latitude', 'axis', 'latitude'

    NCDF_ATTPUT, fid, 'Time', 'standard_name', 'Time in day'
    NCDF_ATTPUT, fid, 'Time', 'long_name', 'Time in day for no leap and leap year'
    NCDF_ATTPUT, fid, 'Time', 'units', 'day'
    NCDF_ATTPUT, fid, 'Time', 'axis', 'Time'

    NCDF_CONTROL, fid, /ENDEF

    NCDF_VARPUT, fid, longvid, lon
    NCDF_VARPUT, fid, latvid, lat
    NCDF_VARPUT, fid, Thresvid, threshold_Psim
    NCDF_VARPUT, fid, timevid, months
    NCDF_CLOSE, fid

  ENDIF 
  ;SAVE,threshold_Psim,FILENAME = '/Volumes/Transcend/ECMWF_ERA/IDL_Save/Thres_Psim_GW_LISFLOOD_Daily_17_MA.sav'   

ENDIF


END