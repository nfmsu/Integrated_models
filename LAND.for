C=======================================================================
C COPYRIGHT 1998-2020 
C                     DSSAT Foundation
C                     University of Florida, Gainesville, Florida
C                     International Fertilizer Development Center
C                     
C ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  LAND UNIT Module. G.Hoogenboom, J.W.Jones, C.Porter
C-----------------------------------------------------------------------
C  Land Unit Module.  Provides the interface between soil, weather
C  and crops.  Based on the original CROPGRO routine
C=======================================================================
C  REVISION       HISTORY
C  12/01/2001 CHP Written.
C  12/12/2001 GH  Rename to Land
!  10/24/2005 CHP Put weather variables in constructed variable. 
!  02/28/2006 CHP Rename Alt_Plant to Plant, move call to CROPGRO there
!  03/03/2006 CHP Added tillage (A.Andales & WDBatchelor).
!  03/21/2006 CHP Added mulch effects
!  10/31/2007 CHP Added simple K model.

C=======================================================================
      SUBROUTINE LAND(CONTROL, ISWITCH, 
     &                YRPLT, MDATE, YREND)
      
C-----------------------------------------------------------------------
      USE ModuleDefs      
      USE FloodModule      
      USE CsvOutput   ! VSH 
      ! MODIFICATION JJ
      USE interface2	
      IMPLICIT NONE
      SAVE
C-----------------------------------------------------------------------
C     Crop, Experiment, Command line Variables
C-----------------------------------------------------------------------
      CHARACTER*2  CROP
      CHARACTER*6  ERRKEY
      PARAMETER   (ERRKEY = 'LAND  ')
      CHARACTER*8  MODEL
      CHARACTER*30 FILEIO
      
C-----------------------------------------------------------------------
C     Date / Timing / Sequencing Variables
C-----------------------------------------------------------------------
      INTEGER      DYNAMIC, YRSIM, YRDOY

C-----------------------------------------------------------------------
C     Input and Output Handling
C-----------------------------------------------------------------------
      CHARACTER*1  IDETS, IPLTI
      CHARACTER*78 MSG(2)

C-----------------------------------------------------------------------
C     Weather module Variables
C-----------------------------------------------------------------------
      TYPE (WeatherType)  WEATHER

C-----------------------------------------------------------------------
C     Soil Processes Module Variables 
C-----------------------------------------------------------------------
      REAL SNOW, WINF
      REAL, DIMENSION(NL) :: NH4_plant, NO3_plant, SPi_Avail, SKi_Avail
      REAL, DIMENSION(NL) :: ST, UPPM, SW, SWDELTS, UPFLOW
      TYPE (SoilType) SOILPROP    !type defined in ModuleDefs
      TYPE (FloodWatType) FLOODWAT
      TYPE (FloodNType)   FloodN
      TYPE (MulchType)    MULCH
!     Needed for ORYZA-Rice
      REAL, DIMENSION(0:NL) :: SomLitC
      REAL, DIMENSION(0:NL,NELEM) :: SomLitE

C-----------------------------------------------------------------------
C     Soil - Plant - Atmosphere Module Variables
C-----------------------------------------------------------------------
      REAL EO, EOP, ES, SRFTEMP, TRWUP
      REAL SWDELTU(NL), SWDELTX(NL) !, RWU(NL)
!     Needed for CaneGro_SA
      REAL EOS, EP, TRWU
!     Calculated by ORYZA-Rice
      REAL UH2O(NL)
!     Needed for SALUS
      REAL RWU(NL)

C-----------------------------------------------------------------------
C     PLANT Module Variables
C-----------------------------------------------------------------------
      INTEGER MDATE
      INTEGER STGDOY(20)
      REAL CANHT, EORATIO, NSTRES, PORMIN, PSTRES1, RWUMX
      REAL XHLAI, XLAI
      REAL KSEVAP, KTRANS
      REAL, Dimension(NL) :: PUptake, RLV, FracRts, UNH4, UNO3, KUptake
      Type (ResidueType) HARVRES  !type defined in ModuleDefs
      Type (ResidueType) SENESCE  
      
C-----------------------------------------------------------------------
C     Operations Management Module Variables 
C-----------------------------------------------------------------------
      TYPE (TillType) TILLVALS
      INTEGER YREND, YRPLT
      REAL IRRAMT_DSSAT
      REAL, DIMENSION(2) :: HARVFRAC   !Harvest & byproduct fractions
      TYPE (FertType) FERTDATA         !Fertilizer application
      TYPE (OrgMatAppType)OMAData      !Organic matter application

C-----------------------------------------------------------------------
!!     Temporary timer function
!!     Date / time variables
!      INTEGER DATE_TIME(8)
!!      date_time(1)  The 4-digit year  
!!      date_time(2)  The month of the year  
!!      date_time(3)  The day of the month  
!!      date_time(4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes  
!!      date_time(5)  The hour of the day (range 0 to 23) - local time  
!!      date_time(6)  The minutes of the hour (range 0 to 59) - local time  
!!      date_time(7)  The seconds of the minute (range 0 to 59) - local time  
!!      date_time(8)  The milliseconds of the second (range 0 to 999) - local time  



!      REAL TIME0, TIME1, TIME_START DELTA_TIME

	!REAL PLANTING_DSSAT_LAND


C-----------------------------------------------------------------------

C     Define constructed variable types based on definitions in
C     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
      TYPE(LAND_S) LAND_C
      TYPE(PLANT_GROWTH) PlantL
      TYPE(SPAMType) SPAM_DATA
      TYPE(MgmtType) MGM_DATA		
      TYPE(WatType) SOIL_W
      TYPE(PlantType) Type_Plant

      TYPE(ToSwatType) SWAT_DATA
!	MODIFICATION JJ 5.2.2025
	INTEGER,allocatable,SAVE::MGM_DSSAT(:)
C     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      MODEL   = CONTROL % MODEL
      YRDOY   = CONTROL % YRDOY
      YRSIM   = CONTROL % YRSIM

      IPLTI   = ISWITCH % IPLTI
!      IF(interface_ihru==1)THEN	
!	write(*,*)'LAND,DYNAMIC',DYNAMIC,
 !    &'ihru=',interface_ihru,PLANTING_DSSAT(interface_ihru)
!	END IF
!	write(*,*)'LAND, CONTROL%DYNAMIC=',CONTROL%DYNAMIC
C***********************************************************************
C***********************************************************************
C     Run Initialization - Called once per simulation
C***********************************************************************

      IF (DYNAMIC .EQ. RUNINIT) THEN
C-----------------------------------------------------------------------
!!     Temporary timer function
!      !Get initial time
!      CALL DATE_AND_TIME (VALUES=DATE_TIME)
!!     Convert time to seconds
!      TIME0 = DATE_TIME(7) 
!     &      + DATE_TIME(8) / 1000.  
!     &      + DATE_TIME(6) * 60.  
!     &      + DATE_TIME(5) * 3600.
!      TIME_START = TIME0
!MODIFICATION
	IF(.NOT.ALLOCATED(MGM_DSSAT))THEN
	ALLOCATE(MGM_DSSAT(1:SIZE(PLANTING_DSSAT)))
	MGM_DSSAT=0
	END IF
C-----------------------------------------------------------------------
C     Read switches from FILEIO
C-----------------------------------------------------------------------
	IF(useSWAT)THEN
!	write(*,*)'LAND,to interface DYNAMIC',CONTROL%DYNAMIC
		CALL interface_IPIBS(CONTROL,ISWITCH,CROP,IDETS,MODEL)
	!write(*,*)'RUN INIT ISWITCH%ISWWAT=',ISWITCH%ISWWAT
	!write(*,*)'KYTKIN',KYTKIN%ISWWAT
	!STOP
	ELSE
	      CALL IPIBS (CONTROL, ISWITCH, CROP, IDETS, MODEL)
	END IF
!	write(*,*)'LAND,DYNAMIC',DYNAMIC,'CONTROL%DYNAMIC',CONTROL%DYNAMIC
C-----------------------------------------------------------------------
C     Read input parameters for weather routines
C-----------------------------------------------------------------------
!MODIFICATION BY JJ
!      CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)
      CALL interface_weather(WEATHER)
C-----------------------------------------------------------------------
C     Read initial soil data 
C-----------------------------------------------------------------------
!	write(*,*)'LAND:To SOIL, DYNAMIC=',DYNAMIC,'CONTROL%DYNAMIC=',
 !    &   CONTROL%DYNAMIC
	CALL GET(PlantL)
	CALL GET(LAND_C)
	CALL GET(SWAT_DATA)

	UNH4=PlantL%UNH4
	UNO3=PlantL%UNO3
	UPFLOW=LAND_C%UPFLOW
	SWDELTX=LAND_C%SWDELTX
	KTRANS=LAND_C%KTRANS
	ES=SPAM_DATA%ES
	IRRAMT_DSSAT=MGM_DATA%IRRAMT
	KUptake=LAND_C%KUptake
	PUptake=LAND_C%PUptake
	ST=LAND_C%ST
	FracRts=PlantL%FracRts
	XHLAI=LAND_C%XHLAI
	
	FERTDATA=SWAT_DATA%FERT_DATA
	HARVRES=SWAT_DATA%HARVRES_DATA
	OMAData=SWAT_DATA%ORG_DATA
	SENESCE=SWAT_DATA%SENESCE
	TILLVALS=SWAT_DATA%TILL_DATA
	FLOODN=SWAT_DATA%FLOOD_N
	FLOODWAT=SWAT_DATA%FLOOD_WATER
	MULCH=SWAT_DATA%MULCH_DATA

	!ES SPAMType
	!FERTDATA
      CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT_DSSAT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, ST,         !Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4_plant, NO3_plant, SKi_AVAIL, SNOW,          !Output
     &    SPi_AVAIL, SOILPROP, SomLitC, SomLitE,          !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

	LAND_C%NH4_plant=NH4_plant
	LAND_C%NO3_plant=NO3_plant
	LAND_C%SKi_AVAIL=SKi_AVAIL
	LAND_C%SPi_AVAIL=SPi_AVAIL
	LAND_C%SomLitC=SomLitC
	LAND_C%SomLitE=SomLItE
	LAND_C%SW=SW
	LAND_C%SWDELTS=SWDELTS
	LAND_C%SWDELTU=SWDELTU
	LAND_C%UPPM=UPPM
	LAND_C%WINF=WINF
	SOIL_W%SNOW=SNOW



C-----------------------------------------------------------------------
C     Read initial soil-plant-atmosphere data
C-----------------------------------------------------------------------

	
	CANHT=Type_Plant%CANHT
	EORATIO=PlantL%EORATIO
	KSEVAP=LAND_C%KSEVAP
	PSTRES1=PlantL%PSTRES1
	PORMIN=PlantL%PORMIN
	RLV=PlantL%RLV
	RWUMX=PlantL%RWUMX
	UH2O=SPAM_DATA%UH2O
	XLAI=PlantL%XLAI
	write(*,*)'TO SPAM, RUNINIT'
      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output
	LAND_C%SWDELTX=SWDELTX
	LAND_C%SWDELTU=SWDELTU
	SPAM_DATA%EO=EO
	SPAM_DATA%EOP=EOP
	SPAM_DATA%EP=EP
	SPAM_DATA%ES=ES
	LAND_C%EOS=EOS
	LAND_C%RWU=RWU
	LAND_C%SRFTEMP=SRFTEMP
	LAND_C%ST=ST
	LAND_C%TRWU=TRWU
	LAND_C%TRWUP=TRWUP
	LAND_C%UPFLOW=UPFLOW
C-----------------------------------------------------------------------
C     Read initial plant module data
C-----------------------------------------------------------------------
	!write(*,*)'TO PLANT'
	if(CROP.NE.'WA')THEN
	write(*,*)'LAND:To DSSAT PLANT ',interface_ihru
      CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4_plant, NO3_plant, SKi_Avail, SomLitC, SomLitE, !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    IRRAMT_DSSAT,                                         !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UH2O, UNH4, UNO3, XHLAI, XLAI) !Output
	Call GET(PlantL)
!	write(*,*)'INITRUN, PROSTI=',PlantL%PROSTI
	Type_Plant%CANHT=CANHT
	PlantL%EORATIO=EORATIO
	LAND_C%KSEVAP=KSEVAP
	LAND_C%KTRANS=KTRANS
	LAND_C%KUptake=KUptake
	PlantL%NSTRES=NSTRES
	PlantL%PSTRES1=PSTRES1
	LAND_C%PUptake=PUptake
	PlantL%PORMIN=PORMIN
	PlantL%RLV=RLV
	PlantL%RWUMX=RWUMX
	PlantL%FracRts=FracRts
	SPAM_DATA%UH2O=UH2O
	PlantL%UNH4=UNH4
	PlantL%UNO3=UNO3
	PlantL%XHLAI=XHLAI
	PlantL%XLAI=XLAI

	!FERTDATA=SWAT_DATA%FERT_DATA
        !HARVRES=SWAT_DATA%HARVRES_DATA
        !OMAData=SWAT_DATA%ORG_DATA
        SWAT_DATA%SENESCE=SENESCE
        !TILLVALS=SWAT_DATA%TILL_DATA
        SWAT_DATA%FLOOD_N=FLOODN
        SWAT_DATA%FLOOD_WATER=FLOODWAT
        SWAT_DATA%MULCH_DATA=MULCH


	ELSE
		!write(*,*)'LAND:To interface_plant'
		call interface_plant
	END IF
	CALL PUT(PlantL)
	CALL PUT(LAND_C)
	CALL PUT(SWAT_DATA)
C-----------------------------------------------------------------------
C     Initialize summary.out information
C-----------------------------------------------------------------------
!	write(*,*)'To OPSUM'
      CALL OPSUM (CONTROL, ISWITCH, YRPLT)
	write(*,*)'END RUNINIT'
C*********************************************************************** 
C*********************************************************************** 
C     SEASONAL INITIALIZATION
C*********************************************************************** 
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
	CALL GET(PlantL)
	CALL GET(LAND_C)
	CALL GET(SWAT_DATA)
C-----------------------------------------------------------------------
C     Call WEATHR for initialization - reads first day of weather
C     data for use in soil N and soil temp initialization.
C-----------------------------------------------------------------------
!MODIFICATION BY JJ
!      CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)
!	write(*,*)'LAND:weather'
	CALL interface_weather(WEATHER)
C-----------------------------------------------------------------------
C     Set planting date, adjust operations dates for seasonal or 
C     sequenced runs.
C-----------------------------------------------------------------------
	IF(useSWAT_M)THEN
	
	CALL interface_MGMTOPS(SW,ISWITCH,CONTROL,YREND,FERTDATA,HARVFRAC,
     & IRRAMT_DSSAT,MDATE,OMADATA,TILLVALS,YRPLT,
     & PLANTING_DSSAT(interface_ihru))
	!STOP
	
	ELSE
!MODIFICATION BY JJ
       CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, HARVRES, SOILPROP, ST,                !Input 
     &    STGDOY, SW, WEATHER,                            !Input
     &    YREND, FERTDATA, HARVFRAC, IRRAMT_DSSAT,              !Output
     &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output

!	write(*,*)'LAND',YRPLT,'<',YRSIM,'IPLTI=',IPLTI	
	END IF
        SWAT_DATA%FERT_DATA=FERTDATA
        !HARVRES=SWAT_DATA%HARVRES_DATA
        SWAT_DATA%ORG_DATA=OMADATA
        !SWAT_DATA%SENESCE=SENESCE
        SWAT_DATA%TILL_DATA=TILLVALS
        !SWAT_DATA%FLOOD_N=FLOODN
        !SWAT_DATA%FLOOD_WATER=FLOODWAT
        !SWAT_DATA%MULCH_DATA=MULCH

C-----------------------------------------------------------------------
      IF (YRPLT < YRSIM .AND. CROP /= 'FA' .AND.
     &    INDEX('AF', IPLTI) == 0) THEN
!	write(*,*)'YRPLT=',YRPLT,'YRSIM=',YRSIM,'IPLTI=',IPLTI
	write(*,*)'LAND: ERROR'
          CALL ERROR(ERRKEY,2,' ',0)
      ENDIF

C-----------------------------------------------------------------------
C     Seasonal initialization for soil processes
C-----------------------------------------------------------------------
	HARVRES=SWAT_DATA%HARVRES_DATA
	SENESCE=SWAT_DATA%SENESCE
	FLOODN=SWAT_DATA%FLOOD_N
	FLOODWAT=SWAT_DATA%FLOOD_WATER
	MULCH=SWAT_DATA%MULCH_DATA

	 UNH4=PlantL%UNH4
        UNO3=PlantL%UNO3
        UPFLOW=LAND_C%UPFLOW
        SWDELTX=LAND_C%SWDELTX
        KTRANS=LAND_C%KTRANS
        ES=SPAM_DATA%ES
        IRRAMT_DSSAT=MGM_DATA%IRRAMT
        KUptake=LAND_C%KUptake
        PUptake=LAND_C%PUptake
        ST=LAND_C%ST
        FracRts=PlantL%FracRts
        XHLAI=LAND_C%XHLAI


	write(*,*)'LAND:TO SOIL'
      CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT_DSSAT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, ST,         !Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4_plant, NO3_plant, SKi_AVAIL, SNOW,          !Output
     &    SPi_AVAIL, SOILPROP, SomLitC, SomLitE,          !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output
  	
	LAND_C%NH4_plant=NH4_plant
        LAND_C%NO3_plant=NO3_plant
        LAND_C%SKi_AVAIL=SKi_AVAIL
        LAND_C%SPi_AVAIL=SPi_AVAIL
        LAND_C%SomLitC=SomLitC
        LAND_C%SomLitE=SomLItE
        LAND_C%SW=SW
        LAND_C%SWDELTS=SWDELTS
        LAND_C%SWDELTU=SWDELTU
        LAND_C%UPPM=UPPM
        LAND_C%WINF=WINF
        SOIL_W%SNOW=SNOW




C-----------------------------------------------------------------------
C     Seasonal initialization for soil-plant-atmosphere processes
!     chp moved this before SOIL, so soil temp is available 
!     update 2020-12-04 - order makes no difference
C-----------------------------------------------------------------------
	CANHT=Type_Plant%CANHT
        EORATIO=PlantL%EORATIO
        KSEVAP=LAND_C%KSEVAP
        PSTRES1=PlantL%PSTRES1
        PORMIN=PlantL%PORMIN
        RLV=PlantL%RLV
        RWUMX=PlantL%RWUMX
        UH2O=SPAM_DATA%UH2O
        XLAI=PlantL%XLAI
	write(*,*)'To SPAM'
      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output
        LAND_C%SWDELTX=SWDELTX
        LAND_C%SWDELTU=SWDELTU
        SPAM_DATA%EO=EO
        SPAM_DATA%EOP=EOP
        SPAM_DATA%EP=EP
        SPAM_DATA%ES=ES
        LAND_C%EOS=EOS
        LAND_C%RWU=RWU
        LAND_C%SRFTEMP=SRFTEMP
        LAND_C%ST=ST
        LAND_C%TRWU=TRWU
        LAND_C%TRWUP=TRWUP
        LAND_C%UPFLOW=UPFLOW

	SWAT_DATA%FLOOD_WATER=FLOODWAT


!C-----------------------------------------------------------------------
!C     Seasonal initialization for soil processes
!C-----------------------------------------------------------------------
!      CALL SOIL(CONTROL, ISWITCH, 
!     &    ES, FERTDATA, HARVRES, IRRAMT_DSSAT, KTRANS,          !Input
!     &    KUptake, OMAData, PUptake, SENESCE, ST,         !Input
!     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
!     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
!     &    NH4_plant, NO3_plant, SKi_AVAIL, SNOW,          !Output
!     &    SPi_AVAIL, SOILPROP, SomLitC, SomLitE,          !Output
!     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

C-----------------------------------------------------------------------
C     Initialize PLANT routines (including phenology and pest)
C----------------------------------------------------------------------
!	if(interface_ihru==1)write(*,*)'LAND:To PLANT, DYNAMIC=',
!     &CONTROL%DYNAMIC,'CROP=',CROP
	IF(CROP.NE.'WA')THEN
	!write(*,*)'LAND: To DSSAT PLANT',interface_ihru






      write(*,*)'LAND,SEASINIT,TO PLANT'	


      CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4_plant, NO3_plant, SKi_Avail, SomLitC, SomLitE, !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    IRRAMT_DSSAT,                                         !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UH2O, UNH4, UNO3, XHLAI, XLAI) !Output


	write(*,*)'LAND,From PLANT'
	CALL GET(PlantL)

	SWAT_DATA%FLOOD_N=FLOODN
	SWAT_DATA%HARVRES_DATA=HARVRES
	SWAT_DATA%SENESCE=SENESCE

 	Type_Plant%CANHT=CANHT
        PlantL%EORATIO=EORATIO
        LAND_C%KSEVAP=KSEVAP
        LAND_C%KTRANS=KTRANS
        LAND_C%KUptake=KUptake
        PlantL%NSTRES=NSTRES
        PlantL%PSTRES1=PSTRES1
        LAND_C%PUptake=PUptake
        PlantL%PORMIN=PORMIN
        PlantL%RLV=RLV
        PlantL%RWUMX=RWUMX
        PlantL%FracRts=FracRts
        SPAM_DATA%UH2O=UH2O
        PlantL%UNH4=UNH4
        PlantL%UNO3=UNO3
        PlantL%XHLAI=XHLAI
        PlantL%XLAI=XLAI



	ELSE
		write(*,*)'LAND:TO interface_plant'
		call interface_plant
	END IF	
C-----------------------------------------------------------------------
C     Initialize summary output file - possible output from 
C     various modules.
C-----------------------------------------------------------------------
	write(*,*)'LAND to OPSUM'
      IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
        CALL OPSUM (CONTROL, ISWITCH, YRPLT)
      ENDIF
	CALL PUT(PlantL)
	CALL PUT(LAND_C)
	CALL PUT(SWAT_DATA)
	write(*,*)'LAND,SEASINIT OVER'
C***********************************************************************
C***********************************************************************
C     DAILY RATE CALCULATIONS
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
	CALL GET(SOILPROP)
	!write(*,*)'LAND,RATE,SOILPROP=',SOILPROP%DLAYR
	!write(*,*)'LAND,RATE,SOURCE=',PLANT_SOURCE(interface_ihru)
	CALL GET(PlantL)
	CAll GET(LAND_C)
	CALL GET(SWAT_DATA)

C!IF(interface_ihru==2)write(*,*)'LAND,RATE,Plant%WTNTOT=',
C!& PlantL%WTNTOT,PlantL%WTNLF
C-----------------------------------------------------------------------
C     Call WEATHER Subroutine to input weather data and to
C     calculate hourly radiation and air temperature values
C     Note: First day of weather has already been read by 
C       initialization call to WEATHR.
C-----------------------------------------------------------------------
!MODIFICATION BY JJ
!      CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)
	call interface_weather(WEATHER)
C-----------------------------------------------------------------------
C     Call Operations Management module to determine today's 
C     applications of irrigation, tillage, etc.
C-----------------------------------------------------------------------
!MODIFICATION BY JJ
	IF(useSWAT_M)THEN

        !IF(PLANTING_DSSAT(interface_ihru).EQ.11)THEN
         !       MGM_DSSAT(interface_ihru)=11
       

	!if(MGM_DSSAT(interface_ihru)>0)THEN
	!write(*,*)'To management',MGM_DSSAT(interface_ihru),interface_ihru
	!write(*,*)'Planting_DSSAT(i)=',PLANTING_DSSAT(interface_ihru)
	!write(*,*)'SOURCE',PLANT_SOURCE(interface_ihru)
	!END IF

!	write(*,*)'LAND,SOURCE=',PLANT_SOURCE(interface_ihru),
!     & MGM_DSSAT(interface_ihru),PLANTING_DSSAT(interface_ihru)

!       write(*,*)'LAND,MDATE=',MDATE,'YREND=',YREND

	!IF(MGM_DSSAT(interface_ihru)==0 .AND. 
!& PLANTING_DSSAT(interface_ihru)>0)THEN
!	MGM_DSSAT(interface_ihru)=PLANTING_DSSAT(interface_ihru)
	!ELSE
	!	STOP
!	END IF
	
	CALL interface_MGMTOPS(SW,ISWITCH,CONTROL,YREND,FERTDATA,HARVFRAC,
     &	IRRAMT_DSSAT,
     & MDATE,OMADATA,TILLVALS,YRPLT,MGM_DSSAT(interface_ihru))
	
!	IF(MGM_DSSAT(interface_ihru)==1.OR.MGM_DSSAT(interface_ihru)==3)
!    & MGM_DSSAT(interface_ihru)=0
	MGM_DSSAT(interface_ihru)=0
	!END IF
	!IF(interface_ihru==1)THEN
	!write(*,*)'---------------------FROM MANAGEMENT-------------'
	!write(*,*)'RATE,MANAGEMENT',MGM_DSSAT(interface_ihru)
	!write(*,*)'----------------FROM MANAGEMENT--------------------'
	!END IF
	!STOP
	ELSE
       CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, HARVRES, SOILPROP, ST,                !Input 
     &    STGDOY, SW, WEATHER,                            !Input
     &    YREND, FERTDATA, HARVFRAC, IRRAMT_DSSAT,              !Output
     &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output
	END IF

	!write(*,*)'LAND,MDATE=',MDATE,'YREND=',YREND
	CALL GET(PlantL)
	PlantL%YREND=YREND
        SWAT_DATA%FERT_DATA=FERTDATA
        !HARVRES=SWAT_DATA%HARVRES_DATA
        SWAT_DATA%ORG_DATA=OMADATA
        !SWAT_DATA%SENESCE=SENESCE
        SWAT_DATA%TILL_DATA=TILLVALS
        !SWAT_DATA%FLOOD_N=FLOODN
        !SWAT_DATA%FLOOD_WATER=FLOODWAT
        !SWAT_DATA%MULCH_DATA=MULCH

        HARVRES=SWAT_DATA%HARVRES_DATA
        SENESCE=SWAT_DATA%SENESCE
        FLOODN=SWAT_DATA%FLOOD_N
        FLOODWAT=SWAT_DATA%FLOOD_WATER
        MULCH=SWAT_DATA%MULCH_DATA


         UNH4=PlantL%UNH4
        UNO3=PlantL%UNO3
        UPFLOW=LAND_C%UPFLOW
        SWDELTX=LAND_C%SWDELTX
        KTRANS=LAND_C%KTRANS
        ES=SPAM_DATA%ES
        IRRAMT_DSSAT=MGM_DATA%IRRAMT
        KUptake=LAND_C%KUptake
        PUptake=LAND_C%PUptake
        ST=LAND_C%ST
        FracRts=PlantL%FracRts
        XHLAI=LAND_C%XHLAI

C-----------------------------------------------------------------------
C     Call Soil processes module to determine today's rates of 
C     change of soil properties.
C-----------------------------------------------------------------------
      CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT_DSSAT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, ST,         !Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4_plant, NO3_plant, SKi_AVAIL, SNOW,          !Output
     &    SPi_AVAIL, SOILPROP, SomLitC, SomLitE,          !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

	  LAND_C%NH4_plant=NH4_plant
        LAND_C%NO3_plant=NO3_plant
        LAND_C%SKi_AVAIL=SKi_AVAIL
        LAND_C%SPi_AVAIL=SPi_AVAIL
        LAND_C%SomLitC=SomLitC
        LAND_C%SomLitE=SomLItE
        LAND_C%SW=SW
        LAND_C%SWDELTS=SWDELTS
        LAND_C%SWDELTU=SWDELTU
        LAND_C%UPPM=UPPM
        LAND_C%WINF=WINF
        SOIL_W%SNOW=SNOW

!	write(*,*)'LAND,SOil water',SW

C-----------------------------------------------------------------------
C     Call Soil-plant-atmosphere module to determine today's
C     rates of evapotranspiration.
C-----------------------------------------------------------------------
        CANHT=Type_Plant%CANHT
        EORATIO=PlantL%EORATIO
        KSEVAP=LAND_C%KSEVAP
        PSTRES1=PlantL%PSTRES1
        PORMIN=PlantL%PORMIN
        RLV=PlantL%RLV
        RWUMX=PlantL%RWUMX
        UH2O=SPAM_DATA%UH2O
        XLAI=PlantL%XLAI
!	write(*,*)'EORATIO=',EORATIO




      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output
	
        LAND_C%SWDELTX=SWDELTX
        LAND_C%SWDELTU=SWDELTU
        SPAM_DATA%EO=EO
        SPAM_DATA%EOP=EOP
        SPAM_DATA%EP=EP
        SPAM_DATA%ES=ES
        LAND_C%EOS=EOS
        LAND_C%RWU=RWU
        LAND_C%SRFTEMP=SRFTEMP
        LAND_C%ST=ST
        LAND_C%TRWU=TRWU
        LAND_C%TRWUP=TRWUP
        LAND_C%UPFLOW=UPFLOW

        SWAT_DATA%FLOOD_WATER=FLOODWAT

C-----------------------------------------------------------------------
C     Call PLANT Subroutine to calculate crop growth and
C     development rates.
C     Skip plant growth and development routines for fallow runs
C-----------------------------------------------------------------------





C!      IF (CROP .NE. 'FA' .AND. 
C!     &    YRDOY .GE. YRPLT .AND. YRPLT .NE. -99) THEN
C!IF(interface_ihru==2)write(*,*)'LAND,TO PLANT, DYNAMIC=',
C!& CONTROL%DYNAMIC,'YREMRG=',PlantL%YREMRG
	IF(CROP.NE.'WA')THEN
C	! write(*,*)'LAND: To DSSAT PLANT',interface_ihru
C	!	write(*,*)'IRRAMT_DSSAT=',IRRAMT_DSSAT	
        CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4_plant, NO3_plant, SKi_Avail, SomLitC, SomLitE, !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    IRRAMT_DSSAT,                                         !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UH2O, UNH4, UNO3, XHLAI, XLAI) !Output
	IF(NSTRES==0)THEN
		MGM_DSSAT(interface_ihru)=11
	END IF
	CALL GET(PlantL)
	Type_Plant%CANHT=CANHT
        PlantL%EORATIO=EORATIO
        LAND_C%KSEVAP=KSEVAP
        LAND_C%KTRANS=KTRANS
        LAND_C%KUptake=KUptake
        PlantL%NSTRES=NSTRES
        PlantL%PSTRES1=PSTRES1
        LAND_C%PUptake=PUptake
        PlantL%PORMIN=PORMIN
        PlantL%RLV=RLV
        PlantL%RWUMX=RWUMX
        PlantL%FracRts=FracRts
        SPAM_DATA%UH2O=UH2O
        PlantL%UNH4=UNH4
        PlantL%UNO3=UNO3
        PlantL%XHLAI=XHLAI
        PlantL%XLAI=XLAI
	SWAT_DATA%FLOOD_N=FLOODN
        SWAT_DATA%HARVRES_DATA=HARVRES
        SWAT_DATA%SENESCE=SENESCE

	ELSE



	!write(*,*)'LAND:To interface_plant'
	call interface_plant
	if(PLANTING_DSSAT(interface_ihru)==110)THEN
	write(*,*)'AUTO FERT,SWAT SIDE'
	END IF

	
	END IF
!      ENDIF
	CALL PUT(PlantL)
	CALL PUT(LAND_C)
	CALL PUT(SWAT_DATA)
	!write(*,*)'AFTER, LAND,RATE, SOILPROP=',SOILPROP%DLAYR
	CALL PUT(SOILPROP)
C***********************************************************************
C     DAILY INTEGRATION 
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. INTEGR) THEN
	CALL GET(PlantL)
	CAll GET(LAND_C)
	CALL GET(SWAT_DATA)
	CALL GET(SOILPROP)
	!write(*,*)'LAND,INTEGR,SOILPROP=',SOILPROP%DLAYR
	UNH4=PlantL%UNH4
        UNO3=PlantL%UNO3
        UPFLOW=LAND_C%UPFLOW
        SWDELTX=LAND_C%SWDELTX
        KTRANS=LAND_C%KTRANS
        ES=SPAM_DATA%ES
        IRRAMT_DSSAT=MGM_DATA%IRRAMT
        KUptake=LAND_C%KUptake
        PUptake=LAND_C%PUptake
        ST=LAND_C%ST
        FracRts=PlantL%FracRts
        XHLAI=LAND_C%XHLAI




	!write(*,*)'TO SOil'
C***********************************************************************
C     Integrate soil state variables
C-----------------------------------------------------------------------
      CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT_DSSAT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, ST,         !Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4_plant, NO3_plant, SKi_AVAIL, SNOW,          !Output
     &    SPi_AVAIL, SOILPROP, SomLitC, SomLitE,          !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

	!write(*,*)'LAND, FROM SOIL ',SOILPROP%DLAYR
	LAND_C%NH4_plant=NH4_plant
        LAND_C%NO3_plant=NO3_plant
        LAND_C%SKi_AVAIL=SKi_AVAIL
        LAND_C%SPi_AVAIL=SPi_AVAIL
        LAND_C%SomLitC=SomLitC
        LAND_C%SomLitE=SomLItE
        LAND_C%SW=SW
        LAND_C%SWDELTS=SWDELTS
        LAND_C%SWDELTU=SWDELTU
        LAND_C%UPPM=UPPM
        LAND_C%WINF=WINF
        SOIL_W%SNOW=SNOW


C-----------------------------------------------------------------------
C     Compute cumulative totals for soil-plant-atmosphere processes
C-----------------------------------------------------------------------
        CANHT=Type_Plant%CANHT
        EORATIO=PlantL%EORATIO
        KSEVAP=LAND_C%KSEVAP
        PSTRES1=PlantL%PSTRES1
        PORMIN=PlantL%PORMIN
        RLV=PlantL%RLV
        RWUMX=PlantL%RWUMX
        UH2O=SPAM_DATA%UH2O
        XLAI=PlantL%XLAI



	!write(*,*)'To Spam'
      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output


	        LAND_C%SWDELTX=SWDELTX
        LAND_C%SWDELTU=SWDELTU
        SPAM_DATA%EO=EO
        SPAM_DATA%EOP=EOP
        SPAM_DATA%EP=EP
        SPAM_DATA%ES=ES
        LAND_C%EOS=EOS
        LAND_C%RWU=RWU
        LAND_C%SRFTEMP=SRFTEMP
        LAND_C%ST=ST
        LAND_C%TRWU=TRWU
        LAND_C%TRWUP=TRWUP
        LAND_C%UPFLOW=UPFLOW



	!write(*,*)'To Plant'
C-----------------------------------------------------------------------
C     Call Plant module to integrate daily plant processes and update
C     plant state variables.
C-----------------------------------------------------------------------
	IF(CROP.NE.'WA')THEN					
	!write(*,*)'CROP=',CROP,'YRDOY=',YRDOY,'YRPLT',YRPLT
      IF (CROP .NE. 'FA' .AND. 
     &        YRDOY .GE. YRPLT .AND. YRPLT .NE. -99) THEN

!	write(*,*)'LAND TO DSSAT PLANT',PlantL%WTNLF,'ihru=',
!     &  interface_ihru
        CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4_plant, NO3_plant, SKi_Avail, SomLitC, SomLitE, !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    IRRAMT_DSSAT,                                         !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UH2O, UNH4, UNO3, XHLAI, XLAI) !Output
	END IF
	CALL GET(PlantL)
        Type_Plant%CANHT=CANHT
        PlantL%EORATIO=EORATIO
        LAND_C%KSEVAP=KSEVAP
        LAND_C%KTRANS=KTRANS
        LAND_C%KUptake=KUptake
        PlantL%NSTRES=NSTRES
        PlantL%PSTRES1=PSTRES1
        LAND_C%PUptake=PUptake
        PlantL%PORMIN=PORMIN
        PlantL%RLV=RLV
        PlantL%RWUMX=RWUMX
        PlantL%FracRts=FracRts
        SPAM_DATA%UH2O=UH2O
        PlantL%UNH4=UNH4
        PlantL%UNO3=UNO3
        PlantL%XHLAI=XHLAI
        PlantL%XLAI=XLAI


	ELSE
	!Write(*,*)'LAND: To interface'
	CALL interface_plant
	END IF
     
	!write(*,*)'TO Management, useSWAT_M=',useSWAT_M
C-----------------------------------------------------------------------
C     Call Operations Management module to check for harvest end, 
C     accumulate variables.
C-----------------------------------------------------------------------
! MODIFICATION BY JJ
	IF(useSWAT_M)THEN
	!write(*,*)'LAND,PLANTING_DSSAT=',PLANTING_DSSAT(interface_ihru)
	!write(*,*)'ihru=',interface_ihru
	CALL interface_MGMTOPS(SW,ISWITCH,CONTROL,YREND,FERTDATA,HARVFRAC,
     & IRRAMT_DSSAT,
     & MDATE,OMADATA,TILLVALS,YRPLT,MGM_DSSAT(interface_ihru))
	MGM_DSSAT(interface_ihru)=0
	ELSE
       CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, HARVRES, SOILPROP, ST,                !Input 
     &    STGDOY, SW, WEATHER,                            !Input
     &    YREND, FERTDATA, HARVFRAC, IRRAMT_DSSAT,              !Output
     &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output
	END IF
	CALL GET(PlantL)
	PlantL%YREND=YREND
	!KOHTA
	CALL PUT(LAND_C)
	CALL PUT(PlantL)
	CALL PUT(SWAT_DATA)
	CALL PUT(SOILPROP)
	!write(*,*)'LAND,INTEGR,END,SOILPROP=',SOILPROP%DLAYR
	!Write(*,*)'LAND OVER'
C***********************************************************************
C***********************************************************************
C     Daily Output
C***********************************************************************
      ELSE IF (DYNAMIC .EQ. OUTPUT) THEN
!NODIFICATION BY JJ
      !CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)
	CALL interface_weather(WEATHER)
        CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT_DSSAT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, ST,         !Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4_plant, NO3_plant, SKi_AVAIL, SNOW,          !Output
     &    SPi_AVAIL, SOILPROP, SomLitC, SomLitE,          !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

        CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output

C-----------------------------------------------------------------------
C     Call plant module for daily printout.
C-----------------------------------------------------------------------
	IF(useSWAT)THEN
	ELSE
        IF (CROP .NE. 'FA') THEN
          CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4_plant, NO3_plant, SKi_Avail, SomLitC, SomLitE, !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    IRRAMT_DSSAT,                                         !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UH2O, UNH4, UNO3, XHLAI, XLAI) !Output
        ENDIF
	END IF
!MODIFICATION BY JJ
	IF(useSWAT_M)THEN
       CALL interface_MGMTOPS(SW,ISWITCH,CONTROL,YREND,FERTDATA,
     &	HARVFRAC,
     & IRRAMT_DSSAT,MDATE,OMADATA,TILLVALS,YRPLT,
     &	PLANTING_DSSAT(interface_ihru))
	ELSE
         CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, HARVRES, SOILPROP, ST,                !Input 
     &    STGDOY, SW, WEATHER,                            !Input
     &    YREND, FERTDATA, HARVFRAC, IRRAMT_DSSAT,              !Output
     &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output
	END IF
C*********************************************************************** 
C***********************************************************************
C     Seasonal Output
C*********************************************************************** 
      ELSE IF (DYNAMIC .EQ. SEASEND) THEN

C     Call WEATHER module to close current weather file 
!MODIFICATION BY JJ
!      CALL WEATHR(CONTROL, ISWITCH, WEATHER, YREND)

C     Print seasonal summaries and close files.
      CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT_DSSAT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, ST,         !Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4_plant, NO3_plant, SKi_AVAIL, SNOW,          !Output
     &    SPi_AVAIL, SOILPROP, SomLitC, SomLitE,          !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

      CALL SPAM(CONTROL, ISWITCH,
     &    CANHT, EORATIO, KSEVAP, KTRANS, MULCH,          !Input
     &    PSTRES1, PORMIN, RLV, RWUMX, SOILPROP, SW,      !Input
     &    SWDELTS, UH2O, WEATHER, WINF, XHLAI, XLAI,      !Input
     &    FLOODWAT, SWDELTU,                              !I/O
     &    EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST,         !Output
     &    SWDELTX, TRWU, TRWUP, UPFLOW)                   !Output

      CALL PLANT(CONTROL, ISWITCH, 
     &    EO, EOP, EOS, EP, ES, FLOODWAT, HARVFRAC,       !Input
     &    NH4_plant, NO3_plant, SKi_Avail, SomLitC, SomLitE, !Input
     &    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW,     !Input
     &    TRWU, TRWUP, UPPM, WEATHER, YREND, YRPLT,       !Input
     &    IRRAMT_DSSAT,                                         !Input
     &    FLOODN,                                         !I/O
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS,        !Output
     &    KUptake, MDATE, NSTRES, PSTRES1,                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UH2O, UNH4, UNO3, XHLAI, XLAI) !Output

!     Call management operations module for seasonal printout.
!MODIFICATION BY JJ
	IF(useSWAT_M)THEN
	CALL interface_MGMTOPS(SW,ISWITCH,CONTROL,YREND,FERTDATA,HARVFRAC,
     & IRRAMT_DSSAT,MDATE,OMADATA,TILLVALS,YRPLT,
     & PLANTING_DSSAT(interface_ihru))
	ELSE
       CALL MGMTOPS(CONTROL, ISWITCH, 
     &    FLOODWAT, HARVRES, SOILPROP, ST,                !Input 
     &    STGDOY, SW, WEATHER,                            !Input
     &    YREND, FERTDATA, HARVFRAC, IRRAMT_DSSAT,              !Output
     &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output
	END IF
C-----------------------------------------------------------------------
C     Seasonal Output
C     Call end of season and summary output subroutines
C-----------------------------------------------------------------------
      CALL OPSUM (CONTROL, ISWITCH, YRPLT)

!!     Temporary timer function
!      CALL DATE_AND_TIME (VALUES=DATE_TIME)
!      
!!     Convert time to seconds
!      TIME1 = DATE_TIME(7) 
!     &      + DATE_TIME(8) / 1000.  
!     &      + DATE_TIME(6) * 60.  
!     &      + DATE_TIME(5) * 3600.
!      DELTA_TIME = TIME1 - TIME0
!      WRITE(200,'(1X,"RUN ",I3,3X,F10.3)') RUN, DELTA_TIME
!      TIME0 = TIME1

      IF (CONTROL % ERRCODE > 0) THEN
        WRITE(MSG(1),'(A,I8)') "End of run ", CONTROL % RUN
        WRITE(MSG(2),'("Simulation ended with error code ",I3)') 
     &      CONTROL % ERRCODE
        CALL WARNING(2,'ENDRUN',MSG)
        CALL INFO(2,'ENDRUN',MSG)
      ELSE
        WRITE(MSG(1),'(A,I8)') "Normal end of run ", CONTROL % RUN
        CALL WARNING(0,'ENDRUN',MSG)
        CALL INFO(1,'ENDRUN',MSG)
      ENDIF
      
!     VSH
      if (SOILPROP % NLAYR > maxnlayers ) then
         maxnlayers = SOILPROP % NLAYR
      end if 
C*********************************************************************** 
C***********************************************************************
C     End of Run
C*********************************************************************** 
      ELSE IF (DYNAMIC .EQ. ENDRUN) THEN
        CALL SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT_DSSAT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, ST,         !Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4_plant, NO3_plant, SKi_AVAIL, SNOW,          !Output
     &    SPi_AVAIL, SOILPROP, SomLitC, SomLitE,          !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

!!     Temporary timer function
!      CALL DATE_AND_TIME (VALUES=DATE_TIME)
!      
!!     Convert time to seconds
!      TIME1 = DATE_TIME(7) 
!     &      + DATE_TIME(8) / 1000.  
!     &      + DATE_TIME(6) * 60.  
!     &      + DATE_TIME(5) * 3600.
!      DELTA_TIME = TIME1 - TIME_START
!      WRITE(200,'(/," Total Time",F10.3)') RUN, DELTA_TIME

!      VSH CSV outputs
       IF (ISWITCH % FMOPT == 'C') THEN
          CALL CsvOutputs(CONTROL % MODEL(1:5), CONTROL % N_ELEMS,
     & maxnlayers)
        END IF 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
	SOILPROP%SW=SW
	CALL PUT(SOILPROP)
	!SOILPROP_NEW(interface_ihru)=SOILPROP
      RETURN
      END SUBROUTINE LAND 
