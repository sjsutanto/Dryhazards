;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New_Lisflood_Soilmoisture_Ovima.pro
;;
;; This script calculates drought in Soil Moisture using the new WB data and makes binary time series
;; Data is obtained from the new Lisflood forcing data
;; Output is raster map in NetCDF/sav file
;;
;;            Author: samuel.sutanto@wur.nl
;;        Created on: 19-June-2019
;; Last modification: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

read = 0

IF read EQ 1 THEN BEGIN   ; Activate this if you want to re-load the data.

  ; ========================================================================
  ; Loading the data
  ; SM data from 1990-2014
  files = FILE_SEARCH('/Volumes/Sony_EXTHD2/New_Lisflood_2019/WB_1990_2018/tha.nc')
  peyl_readnc, lon, file=files, var_name='x'
  peyl_readnc, lat, file=files, var_name='y'
  peyl_readnc, sm14, file=files, var_name='tha'
  p = WHERE(sm14 EQ -9999.00, count)
  IF (count GT 0) THEN sm14[p] = !VALUES.F_NAN

  print, 'Finish loading 1990-2014 data'

  ; Loading the data 2015-2016
  ; SM data from 2015-2016
  files = FILE_SEARCH('/Volumes/Sony_EXTHD2/New_Lisflood_2019/WB_1990_2018/tha20152016.nc')
  peyl_readnc, sm15, file=files, var_name='tha'
  p = WHERE(sm15 EQ -9999.00, count)
  IF (count GT 0) THEN sm15[p] = !VALUES.F_NAN

  ; Open data year 2017-2018
  files = FILE_SEARCH('/Volumes/Sony_EXTHD2/New_Lisflood_2019/WB_1990_2018/tha2017_2019.nc')   ; though it is 2019, but it is 2018.
  peyl_readnc, tha, file=files, var_name='tha'
  sm17 = tha[*,*,0:729]
  UNDEFINE, tha
  p = WHERE(sm17 EQ -9999.00, count)
  IF (count GT 0) THEN sm17[p] = !VALUES.F_NAN

  sm = [[[sm14]],[[sm15]],[[sm17]]]  ; Combine all data
  UNDEFINE, sm14
  UNDEFINE, sm15
  UNDEFINE, sm17

  ; Reload monthly numbers
  ;SAVE,Mont,FILENAME = 'Month_Number_19792016.sav'
  RESTORE, 'Month_Number_19792016.sav'

  ;Select only 1990 till 2016
  Mont1 = Mont[4018:*]   ; 1990-2016
  ; duplicating month data for 2017
  mont17 = Mont1[0:364]   ; taken from 1990 no leap year
  mont18 = Mont1[0:364]   ; taken from 1990 no leap year
  Mont = [Mont1,mont17,mont18]

pp = WHERE(Mont EQ 6, count)
smjune = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),N_ELEMENTS(pp))
IF (count GT 0) THEN smjune = sm[*,*,pp]

pp = WHERE(Mont EQ 7, count)
smjuly = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),N_ELEMENTS(pp))
IF (count GT 0) THEN smjuly = sm[*,*,pp]

pp = WHERE(Mont EQ 8, count)
smaugust = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),N_ELEMENTS(pp))
IF (count GT 0) THEN smaugust = sm[*,*,pp]

; Make it for every year
sm_yr = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),29,92)
FOR i=0, 28 DO BEGIN
  j = i*30
  l = i*31
  sm_yr[*,*,i,*] = [[[smjune[*,*,j:j+29]]],[[smjuly[*,*,l:l+30]]],[[smaugust[*,*,l:l+30]]]]
ENDFOR

  UNDEFINE, sm

print, 'saving data'
; write 1 for saving results to NetCDF
savsgi = 0

months = FINDGEN(92)
years = FINDGEN(29)

IF savsgi EQ 1 THEN BEGIN
  ; saving netcdf
  description = 'Soil moisture (Tha) during summer derived from New LISFLOOD WB 1990-2018'
  fid=NCDF_CREATE( '/Volumes/Sony_EXTHD2/New_Lisflood_2019/OVIMA/Soilmoisture_NEW_LISFLOOD_Daily_JJA.nc', /CLOBBER,/NETCDF4_FORMAT)
  NCDF_ATTPUT, fid, 'Description', description, /GLOBAL

  longid=NCDF_DIMDEF(fid,'longitude',N_ELEMENTS(lon))
  latid=NCDF_DIMDEF(fid,'latitude',N_ELEMENTS(lat))
  monthid=NCDF_DIMDEF(fid,'Month',N_ELEMENTS(months))
  yearid=NCDF_DIMDEF(fid,'Year',N_ELEMENTS(years))

  Thresvid=NCDF_VARDEF(fid,'sm_yr', [longid,latid,yearid,monthid],/FLOAT)
  longvid=NCDF_VARDEF(fid,'longitude', [longid],/FLOAT)
  latvid=NCDF_VARDEF(fid,'latitude', [latid],/FLOAT)
  monthvid=NCDF_VARDEF(fid,'Month', [monthid],/FLOAT)
  yearvid=NCDF_VARDEF(fid,'Year', [yearid],/FLOAT)

  NCDF_ATTPUT, fid, 'sm_yr', 'units', 'mm'
  NCDF_ATTPUT, fid, 'sm_yr', 'title', 'Soil moisture values'
  NCDF_ATTPUT, fid, 'longitude', 'standard_name', 'projection_lon_coordinate'
  NCDF_ATTPUT, fid, 'longitude', 'long_name', 'longitude coordinate of projection'
  NCDF_ATTPUT, fid, 'longitude', 'units', 'm'
  NCDF_ATTPUT, fid, 'longitude', 'axis', 'longitude'

  NCDF_ATTPUT, fid, 'latitude', 'standard_name', 'projection_lat_coordinate'
  NCDF_ATTPUT, fid, 'latitude', 'long_name', 'latitude coordinate of projection'
  NCDF_ATTPUT, fid, 'latitude', 'units', 'm'
  NCDF_ATTPUT, fid, 'latitude', 'axis', 'latitude'

  NCDF_ATTPUT, fid, 'Month', 'standard_name', 'Month in summer'
  NCDF_ATTPUT, fid, 'Month', 'long_name', 'Month in summer for JJA'
  NCDF_ATTPUT, fid, 'Month', 'units', 'Month'
  NCDF_ATTPUT, fid, 'Month', 'axis', 'Month'
  
  NCDF_ATTPUT, fid, 'Year', 'standard_name', 'Year in summer'
  NCDF_ATTPUT, fid, 'Year', 'long_name', 'Year in summer for JJA'
  NCDF_ATTPUT, fid, 'Year', 'units', 'Year'
  NCDF_ATTPUT, fid, 'Year', 'axis', 'Year'

  NCDF_CONTROL, fid, /ENDEF

  NCDF_VARPUT, fid, longvid, lon
  NCDF_VARPUT, fid, latvid, lat
  NCDF_VARPUT, fid, Thresvid, sm_yr
  NCDF_VARPUT, fid, Monthvid, months  
  NCDF_VARPUT, fid, Yearvid, years
  NCDF_CLOSE, fid

ENDIF

;========================================================================================================
ENDIF

; reload the saved soil moisture data above (you do not need this if you calculate the data since the beginning.
files = FILE_SEARCH('/Volumes/Sony_EXTHD2/New_Lisflood_2019/OVIMA/Soilmoisture_NEW_LISFLOOD_Daily_JJA.nc')
peyl_readnc, sm_yr, file=files, var_name='sm_yr'
peyl_readnc, lon, file=files, var_name='x'
peyl_readnc, lat, file=files, var_name='y'

; Reload Threshold data
files = FILE_SEARCH('/Volumes/Sony_EXTHD2/New_Lisflood_2019/Threshold/Thres_Psim_soilmoisture_NEW_LISFLOOD_Daily_18.nc')
peyl_readnc, Threshold_Psim, file=files, var_name='threshold_Psim'

tres_yr = FLTARR(N_ELEMENTS(lon),N_ELEMENTS(lat),29,92)

tres_yr[*,*,0,*] = Threshold_Psim[*,*,151:242]    ; 1990
tres_yr[*,*,1,*] = Threshold_Psim[*,*,151:242]    ; 1991
tres_yr[*,*,2,*] = Threshold_Psim[*,*,517:608]    ; 1992 leap
tres_yr[*,*,3,*] = Threshold_Psim[*,*,151:242]    ; 1993
tres_yr[*,*,4,*] = Threshold_Psim[*,*,151:242]    ; 1994
tres_yr[*,*,5,*] = Threshold_Psim[*,*,151:242]    ; 1995
tres_yr[*,*,6,*] = Threshold_Psim[*,*,517:608]    ; 1996  leap
tres_yr[*,*,7,*] = Threshold_Psim[*,*,151:242]    ; 1997
tres_yr[*,*,8,*] = Threshold_Psim[*,*,151:242]    ; 1998
tres_yr[*,*,9,*] = Threshold_Psim[*,*,151:242]    ; 1999
tres_yr[*,*,10,*] = Threshold_Psim[*,*,517:608]    ; 2000 leap
tres_yr[*,*,11,*] = Threshold_Psim[*,*,151:242]    ; 2001
tres_yr[*,*,12,*] = Threshold_Psim[*,*,151:242]    ; 2002
tres_yr[*,*,13,*] = Threshold_Psim[*,*,151:242]    ; 2003
tres_yr[*,*,14,*] = Threshold_Psim[*,*,517:608]    ; 2004  leap
tres_yr[*,*,15,*] = Threshold_Psim[*,*,151:242]    ; 2005
tres_yr[*,*,16,*] = Threshold_Psim[*,*,151:242]    ; 2006
tres_yr[*,*,17,*] = Threshold_Psim[*,*,151:242]    ; 2007
tres_yr[*,*,18,*] = Threshold_Psim[*,*,517:608]    ; 2008   leap
tres_yr[*,*,19,*] = Threshold_Psim[*,*,151:242]    ; 2009
tres_yr[*,*,20,*] = Threshold_Psim[*,*,151:242]    ; 2010
tres_yr[*,*,21,*] = Threshold_Psim[*,*,151:242]    ; 2011
tres_yr[*,*,22,*] = Threshold_Psim[*,*,517:608]    ; 2012   leap
tres_yr[*,*,23,*] = Threshold_Psim[*,*,151:242]    ; 2013
tres_yr[*,*,24,*] = Threshold_Psim[*,*,151:242]    ; 2014
tres_yr[*,*,25,*] = Threshold_Psim[*,*,151:242]    ; 2015
tres_yr[*,*,26,*] = Threshold_Psim[*,*,517:608]    ; 2016   leap
tres_yr[*,*,27,*] = Threshold_Psim[*,*,151:242]    ; 2017
tres_yr[*,*,28,*] = Threshold_Psim[*,*,151:242]    ; 2018

UNDEFINE, Threshold_Psim

dro_yr = sm_yr-tres_yr

;STOP
pp = WHERE(dro_yr LE 0, count)    ; Now here I used 0 instead of 1.00e-04
IF (count GT 0) THEN dro_yr[pp] = 1
pp = WHERE(dro_yr NE 1, count)
IF (count GT 0) THEN dro_yr[pp] = 0

savdr = 1

IF savdr EQ 1 THEN BEGIN
print, 'Saving as NC'
; Making NetCDF file
months = FINDGEN(92)
years = FINDGEN(29)

description = 'Soil moisture drought based on the new LISFLOOD 1990-2018'

Lati = N_ELEMENTS(lat)
Longi = N_ELEMENTS(lon)
month = N_ELEMENTS(months)
year = N_ELEMENTS(years)

SM_drought = dro_yr
SM_drought_name = 'Dailly soil moisture drought'
SM_drought_units = '-'

lat = lat
lat_name = 'latitude'
lat_units = 'none'

lon = lon
lon_name = 'longitude'
lon_units = 'none'

; create NetCDF files
data = NCDF_CREATE('/Volumes/Sony_EXTHD2/New_Lisflood_2019/OVIMA/Daily_New_SM_drought_WB.nc', /CLOBBER,/NETCDF4_FORMAT)
NCDF_ATTPUT, data, 'Description', description, /GLOBAL

tid = NCDF_DIMDEF(data, 'Longi', Longi)
tid1 = NCDF_DIMDEF(data, 'lati', lati)
tid2 = NCDF_DIMDEF(data, 'year', year)
tid3 = NCDF_DIMDEF(data, 'month', month)

vid = NCDF_VARDEF(data, 'SM_drought', [tid,tid1,tid2,tid3], /FLOAT)
vid1 = NCDF_VARDEF(data, 'lat', [tid1], /FLOAT)
vid2 = NCDF_VARDEF(data, 'lon', [tid], /FLOAT)


NCDF_ATTPUT, data, 'SM_drought', 'SM_drought', SM_drought_name
NCDF_ATTPUT, data, 'SM_drought', 'SM_drought', SM_drought_units
NCDF_ATTPUT, data, 'lat', 'lat', lat_name
NCDF_ATTPUT, data, 'lat', 'lat', lat_units
NCDF_ATTPUT, data, 'lon', 'lon', lon_name
NCDF_ATTPUT, data, 'lon', 'lon', lon_units

NCDF_CONTROL, data, /ENDEF

NCDF_VARPUT, data, 'SM_drought', SM_drought
NCDF_VARPUT, data, 'lat', lat
NCDF_VARPUT, data, 'lon', lon
NCDF_CLOSE, data

ENDIF

END