!=======================================================================
C  MODULE ModuleDefs
C  11/01/2001 CHP Written
C  06/15/2002 CHP Added flood-related data constructs 
C  03/12/2003 CHP Added residue data construct
C  05/08/2003 CHP Added version information
C  09/03/2004 CHP Modified GetPut_Control routine to store entire
C                   CONTROL variable. 
C             CHP Added GETPUT_ISWITCH routine to store ISWITCH.
C             CHP Added TRTNUM to CONTROL variable.
!  06/14/2005 CHP Added FILEX to CONTROL variable.
!  10/24/2005 CHP Put weather variables in constructed variable. 
!             CHP Added PlantStres environmental stress variable
!  11/07/2005 CHP Added KG2PPM conversion variable to SoilType
!  03/03/2006 CHP Tillage variables added to SOILPROP
!                 Added N_ELEMS to CONTROL variable.
!  03/06/2006 CHP Added mulch variable
!  07/13/2006 CHP Add P variables to SwitchType and SoilType TYPES
!  07/14/2006 CHP Added Fertilizer type, Organic Matter Application type
!  07/24/2006 CHP Added mulch/soil albedo (MSALB) and canopy/mulch/soil
!                   albedo (CMSALB) to SOILPROP variable
!  01/12/2007 CHP Changed TRTNO to TRTNUM to avoid conflict with same
!                 variable name (but different meaning) in input module.
!  01/24/2007 CHP Changed GET & PUT routines to more extensible format.
!  01/22/2008 CHP Moved data exchange (GET & PUT routines) to ModuleData
!  04/28/2008 CHP Added option to read CO2 from file 
!  08/08/2008 CHP Use OPSYS to define variables dependant on operating system
!  08/08/2008 CHP Compiler directives for system call library
!  08/21/2008 CHP Add ROTNUM to CONTROL variable
!  11/25/2008 CHP Mauna Loa CO2 is default
!  12/09/2008 CHP Remove METMP
!  11/19/2010 CHP Added "branch" to version to keep track of non-release branches
!  08/08/2017 WP  Version identification moved to CSMVersion.for
!  08/08/2017 WP  Definitions related with OS platform moved to OSDefinitions.for
!=======================================================================

      MODULE ModuleDefs
!     Contains defintion of derived data types and constants which are 
!     used throughout the model.

!=======================================================================
      USE CSMVersion
      USE OSDefinitions
      SAVE
!=======================================================================
!     Global constants
      INTEGER, PARAMETER :: 
     &    NL       = 20,  !Maximum number of soil layers 
     &    TS       = 24,  !Number of hourly time steps per day
     &    NAPPL    = 9000,!Maximum number of applications or operations
     &    NCOHORTS = 300, !Maximum number of cohorts
     &    NELEM    = 3,   !Number of elements modeled (currently N & P)
!            Note: set NELEM to 3 for now so Century arrays will match
     &    NumOfDays = 1000, !Maximum days in sugarcane run (FSR)
     &    NumOfStalks = 42, !Maximum stalks per sugarcane stubble (FSR)
     &    EvaluateNum = 40, !Number of evaluation variables
     &    MaxFiles = 500,   !Maximum number of output files
     &    MaxPest = 500    !Maximum number of pest operations

      REAL, PARAMETER :: 
     &    PI = 3.14159265,
     &    RAD=PI/180.0

      INTEGER, PARAMETER :: 
         !Dynamic variable values
     &    RUNINIT  = 1, 
     &    INIT     = 2,  !Will take the place of RUNINIT & SEASINIT
                         !     (not fully implemented)
     &    SEASINIT = 2, 
     &    RATE     = 3,
     &    EMERG    = 3,  !Used for some plant processes.  
     &    INTEGR   = 4,  
     &    OUTPUT   = 5,  
     &    SEASEND  = 6,
     &    ENDRUN   = 7 

      INTEGER, PARAMETER :: 
         !Nutrient array positions:
     &    N = 1,          !Nitrogen
     &    P = 2,          !Phosphorus
     &    Kel = 3         !Potassium

      CHARACTER(LEN=3)  ModelVerTxt
      CHARACTER(LEN=6)  LIBRARY    !library required for system calls

      CHARACTER*3 MonthTxt(12)
      DATA MonthTxt /'JAN','FEB','MAR','APR','MAY','JUN',
     &               'JUL','AUG','SEP','OCT','NOV','DEC'/

!     MAKEFILEW VARIABLES 
      INTEGER:: FirstWeatherDate = -99
      INTEGER:: NEWSDATE = -99
!=======================================================================
!     Data construct for control variables
      TYPE ControlType
        CHARACTER (len=1)  MESIC, RNMODE
        CHARACTER (len=2)  CROP
        CHARACTER (len=8)  MODEL, ENAME
        CHARACTER (len=12) FILEX
        CHARACTER (len=30) FILEIO
        CHARACTER (len=102)DSSATP
        CHARACTER (len=120) :: SimControl = 
     &  "                                                            "//
     &  "                                                            "
        INTEGER   DAS, DYNAMIC, FROP, ErrCode, LUNIO, MULTI, N_ELEMS
        INTEGER   NYRS, REPNO, ROTNUM, RUN, TRTNUM
        INTEGER   YRDIF, YRDOY, YRSIM
        INTEGER   FODAT, ENDYRS  !Forecast start date and ensemble #
      END TYPE ControlType

!=======================================================================
!     Data construct for control switches
      TYPE SwitchType
        CHARACTER (len=1) FNAME
        CHARACTER (len=1) IDETC, IDETD, IDETG, IDETH, IDETL, IDETN
        CHARACTER (len=1) IDETO, IDETP, IDETR, IDETS, IDETW
        CHARACTER (len=1) IHARI, IPLTI, IIRRI, ISIMI
        CHARACTER (len=1) ISWCHE, ISWDIS, ISWNIT
        CHARACTER (len=1) ISWPHO, ISWPOT, ISWSYM, ISWTIL, ISWWAT
        CHARACTER (len=1) MEEVP, MEGHG, MEHYD, MEINF, MELI, MEPHO
        CHARACTER (len=1) MESOM, MESOL, MESEV, MEWTH
        CHARACTER (len=1) METMP !Temperature, EPIC
        CHARACTER (len=1) IFERI, IRESI, ICO2, FMOPT
        INTEGER NSWI
      END TYPE SwitchType

!Other switches and methods used by model:
! MELI, IOX - not used
! IDETH - only used in MgmtOps
! MEWTH - only used in WEATHR

!=======================================================================
!     Data construct for weather variables
      TYPE WeatherType
        SEQUENCE

!       Weather station information
        REAL REFHT, WINDHT, XLAT, XLONG, XELEV

!       Daily weather data.
        REAL CLOUDS, CO2, DAYL, DCO2, PAR, RAIN, RHUM, SNDN, SNUP, 
     &    SRAD, TAMP, TA, TAV, TAVG, TDAY, TDEW, TGROAV, TGRODY,      
     &    TMAX, TMIN, TWILEN, VAPR, WINDRUN, WINDSP, VPDF, VPD_TRANSP,
     &    OZON7

!       Hourly weather data
        REAL, DIMENSION(TS) :: AMTRH, AZZON, BETA, FRDIFP, FRDIFR, PARHR
        REAL, DIMENSION(TS) :: RADHR, RHUMHR, TAIRHR, TGRO, WINDHR

      END TYPE WeatherType

!=======================================================================
!     Data construct for soil variables
      TYPE SoilType
        INTEGER NLAYR
        CHARACTER (len=5) SMPX
        CHARACTER (len=10) SLNO
        CHARACTER (len=12) TEXTURE(NL)
        CHARACTER (len=17) SOILLAYERTYPE(NL)
        CHARACTER*50 SLDESC, TAXON
        
        LOGICAL COARSE(NL)
        
        REAL ALES, DMOD, SLPF         !DMOD was SLNF
        REAL CMSALB, MSALB, SWALB, SALB      !Albedo 
        REAL, DIMENSION(NL) :: BD, CEC, CLAY, DLAYR, DS, DUL
        REAL, DIMENSION(NL) :: KG2PPM, LL, OC, PH, PHKCL, POROS
        REAL, DIMENSION(NL) :: SAND, SAT, SILT, STONES, SWCN
        REAL,DIMENSION(NL)::SW  ! Added by JJ 19.3.2025
      !Residual water content
        REAL, DIMENSION(NL) :: WCR

      !vanGenuchten parameters
        REAL, DIMENSION(NL) :: alphaVG, mVG, nVG

      !Second tier soils data:
        REAL, DIMENSION(NL) :: CACO3, EXTP, ORGP, PTERMA, PTERMB
        REAL, DIMENSION(NL) :: TOTP, TOTBAS, EXCA, EXK, EXNA

      !Soil analysis data 
        REAL, DIMENSION(NL) :: SASC   !stable organic C

      !Variables added with new soil format:
        REAL ETDR, PONDMAX, SLDN, SLOPE
!       REAL, DIMENSION(NL) :: RCLPF, RGIMPF

      !Variables deleted with new soil format:
      !Still needed for Ritchie hydrology
        REAL CN, SWCON, U
        REAL, DIMENSION(NL) :: ADCOEF, TOTN, TotOrgN, WR

      !Text describing soil layer depth data
      !1-9 describe depths for layers 1-9
      !10 depths for layers 10 thru NLAYR (if NLAYR > 9)
      !11 depths for layers 5 thru NLAYR (if NLAYR > 4)
        CHARACTER*8 LayerText(11)

      !These variables could be made available if needed elsewhere.
      !  They are currently read by SOILDYN module.
      !  CHARACTER*5 SLTXS
      !  CHARACTER*11 SLSOUR
      !  CHARACTER*50 SLDESC, TAXON

      !Second tier soils data that could be used:
!        REAL, DIMENSION(NL) :: EXTAL, EXTFE, EXTMN, 
!        REAL, DIMENSION(NL) :: EXMG, EXTS, SLEC

      END TYPE SoilType

!=======================================================================
!     Data construct for mulch layer
      TYPE MulchType
        REAL MULCHMASS    !Mass of surface mulch layer (kg[dry mat.]/ha)
        REAL MULCHALB     !Albedo of mulch layer
        REAL MULCHCOVER   !Coverage of mulch layer (frac. of surface)
        REAL MULCHTHICK   !Thickness of mulch layer (mm)
        REAL MULCHWAT     !Water content of mulch (mm3/mm3)
        REAL MULCHEVAP    !Evaporation from mulch layer (mm/d)
        REAL MULCHSAT     !Saturation water content of mulch (mm3/mm3)
        REAL MULCHN       !N content of mulch layer (kg[N]/ha)
        REAL MULCHP       !P content of mulch layer (kg[P]/ha)
        REAL NEWMULCH     !Mass of new surface mulch (kg[dry mat.]/ha)
        REAL NEWMULCHWAT  !Water content of new mulch ((mm3/mm3)
        REAL MULCH_AM     !Area covered / dry weight of residue (ha/kg)
        REAL MUL_EXTFAC   !Light extinction coef. for mulch layer
        REAL MUL_WATFAC   !Saturation water content (mm[water] ha kg-1)
      END TYPE MulchType

!=======================================================================
!     Data construct for tillage operations
      TYPE TillType
        INTEGER NTIL      !Total number of tillage events in FILEX
        INTEGER TILDATE   !Date of current tillage event

!       Maximum values for multiple events in a single day
        REAL TILDEP, TILMIX, TILRESINC

!       Irrigation amount which affects tilled soil properties 
!          expressed in units of equivalent rainfall depth
        REAL TIL_IRR   

!       Allows multiple tillage passes in a day
        INTEGER NTil_today !number of tillage events today (max 3)
        INTEGER, DIMENSION(NAPPL) :: NLYRS
        REAL, DIMENSION(NAPPL) :: CNP, TDEP
        REAL, DIMENSION(NAPPL,NL) :: BDP, DEP, SWCNP
      END TYPE TillType

!=======================================================================
!     Data construct for oxidation layer
      TYPE OxLayerType
        INTEGER IBP
        REAL    OXU, OXH4, OXN3   
        REAL    OXLT, OXMIN4, OXMIN3
        REAL    DLTOXU, DLTOXH4, DLTOXN3
        REAL    ALGACT
        LOGICAL DailyCalc
      END TYPE OxLayerType

!======================================================================
!     Fertilizer application data
      TYPE FertType
        CHARACTER*7 AppType != 'UNIFORM', 'BANDED ' or 'HILL   '
        INTEGER FERTDAY, FERTYPE
        INTEGER, DIMENSION(NELEM) :: NAPFER
        REAL FERDEPTH, FERMIXPERC
        REAL ADDFNH4, ADDFNO3, ADDFUREA
        REAL ADDOXU, ADDOXH4, ADDOXN3
        REAL, DIMENSION(NELEM) :: AMTFER
        REAL, DIMENSION(NL) :: ADDSNH4, ADDSNO3, ADDUREA
        REAL, DIMENSION(NL) :: ADDSPi
        REAL, DIMENSION(NL) :: ADDSKi
        LOGICAL UNINCO
      END TYPE FertType

!=======================================================================
!   Data construct for residue (harvest residue, senesced matter, etc.)
      TYPE ResidueType
        REAL, DIMENSION(0:NL) :: ResWt        !kg[dry matter]/ha/d
        REAL, DIMENSION(0:NL) :: ResLig       !kg[lignin]/ha/d
        REAL, DIMENSION(0:NL,NELEM) :: ResE   !kg[E]/ha/d (E=N,P,K,..)
        REAL  CumResWt                        !cumul. kg[dry matter]/ha
        REAL, DIMENSION(NELEM) :: CumResE     !cumulative kg[E]/ha
      END TYPE ResidueType

!======================================================================
!     Organic Matter Application data
      TYPE OrgMatAppType
        INTEGER NAPRes, ResDat, ResDepth
        CHARACTER (len=5) RESTYPE
        REAL ResMixPerc   !Percent mixing rate for SOM module

        REAL, DIMENSION(0:NL) :: ResWt        !kg[dry matter]/ha/d
        REAL, DIMENSION(0:NL) :: ResLig       !kg[lignin]/ha/d
        REAL, DIMENSION(0:NL,NELEM) :: ResE   !kg[E]/ha/d (E=N, P, ..)
        REAL  CumResWt                        !cumul. kg[dry matter]/ha
        REAL, DIMENSION(NELEM) :: CumResE     !cumulative kg[E]/ha
      END TYPE OrgMatAppType

!======================================================================
!     Plant stresses for environmental stress summary
      Type PlStresType
        INTEGER NSTAGES   !# of stages (max 5)
        CHARACTER(len=23) StageName(0:5)
        REAL W_grow, W_phot, N_grow, N_phot
        REAL P_grow, P_phot
        LOGICAL ACTIVE(0:5)
      End Type PlStresType

!======================================================================
!     Array of output files, aliases, unit numbers, etc.
      Type OutputType
        INTEGER NumFiles
        CHARACTER*16, DIMENSION(MaxFiles) :: FileName
        CHARACTER*2,  DIMENSION(MaxFiles) :: OPCODE
        CHARACTER*50, DIMENSION(MaxFiles) :: Description
        CHARACTER*10, DIMENSION(MaxFiles) :: ALIAS
        INTEGER, DIMENSION(MaxFiles) :: LUN
      End Type


!======================================================================
!      CONTAINS
!
!!----------------------------------------------------------------------
!      SUBROUTINE SETOP ()
!      IMPLICIT NONE
!
!      WRITE(ModelVerTxt,'(I2.2,I1)') Version%Major, Version%Minor
!
!      END SUBROUTINE SETOP

!======================================================================
      END MODULE ModuleDefs
!======================================================================



!======================================================================
!     Paddy Managment routines.
!======================================================================
      MODULE FloodModule
!=======================================================================
!     Data construct for flood data. 
      Type FloodWatType
        !From IRRIG
        LOGICAL BUNDED        
        INTEGER NBUND         
        REAL ABUND            
        REAL PUDPERC, PERC
        REAL PLOWPAN    !Depth of puddling (m) (ORYZA)

        !From Paddy_Mgmt
        INTEGER YRDRY, YRWET  
        REAL FLOOD, FRUNOFF   
        REAL TOTBUNDRO        
        LOGICAL PUDDLED       

        REAL CEF, EF          !From SPAM
        REAL INFILT, RUNOFF   !From WATBAL
      End Type FloodWatType

      Type FloodNType
        INTEGER NDAT
        REAL    ALGFON                        !Algae kill or dry-up
        REAL    FLDH4C, FLDN3C                !Flood N concentrations
        REAL    FLDU, FLDN3, FLDH4            !Flood N mass (kg/ha)
        REAL    FRNH4U, FRNO3U                !Flood N uptake (kg/ha)
        REAL    DLTFUREA, DLTFNH4, DLTFNO3    !Flood N flux (kg/ha)
      End Type FloodNType

      END MODULE FloodModule
!======================================================================

!=======================================================================
!  MODULE ModuleData
!  01/22/2008 CHP Written
!=======================================================================

      MODULE ModuleData
!     Data storage and retrieval module.
!     Defines data structures that hold information that can be 
!       stored or accessed by query.  

!     A call to the GET routine will return the value of variable 
!       requested.  The procedure is "overloaded", i.e., the procedure 
!       executed will depend on the type of variable(s) in the argument 
!       list.  A request for a "real" data type will invoke the GET_Real
!       procedure, for example.  

!     Similarly, a call to the PUT routine will store the data sent.
!       It is also an overloaded procedure including several different
!       types of data which can be stored.

!     The SAVE_data variable is used to store all information.

!     To add a variable for storage and retrieval: 
!     1.  Add the variable to one of the Type constructs based on the 
!         module that "owns" the variable, for example SPAMType, Planttype 
!         or MgmtType.
!     2.  For a real data type, add a line of code in both the GET_Real and
!         PUT_Real subroutines.  
!     3.  For an integer data type, add a line of code in both the 
!         GET_Integer and PUT_Integer subroutines.  
!     4.  All routines accessing GET or PUT procedures must include a 
!         "USE ModuleData" statement.
!     5.  A call to the PUT routine must be used to store data prior to
!         a call to the GET routine to retrive the data.

      USE ModuleDefs
	USE FloodModule
      SAVE

!======================================================================
!     Data transferred from hourly energy balance 
      Type SPAMType
        REAL AGEFAC, PG                   !photosynthese
        REAL CEF, CEM, CEO, CEP, CES      !Cumulative ET - mm
        REAL CET, CEVAP                   !Cumulative ET - mm
        REAL  EF,  EM,  EO,  EP,  ES,  ET !Daily ET - mm/d
        REAL  EOP, EVAP                   !Daily mm/d
        REAL, DIMENSION(NL) :: UH2O       !Root water uptake
        !ASCE reference ET with FAO-56 dual crop coefficient (KRT)
        REAL REFET, SKC, KCBMAX, KCB, KE, KC
        !VPD parameters for CSYCA model (LPM)
        REAL PHSV, PHTV
      End Type SPAMType

!     Data transferred from CROPGRO routine 
      TYPE PlantType
        REAL CANHT, CANWH, DXR57, EXCESS,
     &    PLTPOP, RNITP, SLAAD, XPOD
        REAL BIOMAS
        INTEGER NR5, iSTAGE, iSTGDOY
        CHARACTER*10 iSTNAME
      END TYPE PlantType

!     Data transferred from management routine 
      Type MgmtType
        REAL DEPIR, EFFIRR, FERNIT, IRRAMT, TOTIR, TOTEFFIRR
        REAL V_AVWAT(20) !Create vectors for growth stage based irrig
        REAL V_IMDEP(20)
        REAL V_ITHRL(20)
        REAL V_ITHRU(20)
        INTEGER V_IRON(20)
        REAL V_IRAMT(20)
        REAL V_IREFF(20)
        INTEGER V_IFREQ(20)
        INTEGER GSIRRIG
        CHARACTER*5 V_IRONC(20)
      End Type MgmtType

!     Data transferred from Soil water routine
      Type WatType
        REAL DRAIN, RUNOFF, SNOW
      End Type WatType

!     Data transferred from Soil Inorganic Nitrogen routine
      Type NiType
        REAL TNOXD, TLeachD    !, TN2OD     ! added N2O PG
      End Type NiType

!     Data transferred from Organic C routines
      Type OrgCType
        REAL TOMINFOM, TOMINSOM, TOMINSOM1, TOMINSOM2
        REAL TOMINSOM3, TNIMBSOM
        REAL MULCHMASS
      End Type OrgCType

!     Data from weather
      Type WeathType
        INTEGER WYEAR
        Character*8 WSTAT
      End Type WeathType

      TYPE PDLABETATYPE
        REAL PDLA
        REAL BETALS
      END TYPE
	!MODIFICATION 9.9 JanneJ added
      TYPE ToSwatType
	REAL::SPi_Soluble(1:NL),NO3(1:NL),NH4(1:NL)		! Soluble Phosphorus,nitrate,ammonium
	REAL::SOM1E1(1:NL),SOM1E2(1:NL),SOM1E3(1:NL)
	REAL::SOM2E1(1:NL),SOM2E2(1:NL),SOM2E3(1:NL)
        REAL::SOM3E1(1:NL),SOM3E2(1:NL),SOM3E3(1:NL)
	REAL::SOM23E1(1:NL),SOM23E2(1:NL),SOM23E3(1:NL)
	REAL::PPlant2_kg,WTNCAN,BIOMAS2
	TYPE(SPAMType) SPAM_DATA
	TYPE(MgmtType) MGM_DATA
	TYPE(FloodWatType) FLOOD_WATER
	TYPE(FloodNType) FLOOD_N
	TYPE(MulchType) MULCH_DATA
	TYPE(ResidueType) HARVRES_DATA
	TYPE(ResidueType) SENESCE
	TYPE(TillType) TILL_DATA
	TYPE(FertType) FERT_DATA
	TYPE(OrgMatAppType) ORG_DATA
	TYPE(WatType) WATER_DATA

      END Type ToSwatType
      TYPE PLANT_GROWTH
	CHARACTER(len=6) ECONO
	REAL::PHZACC(20)
	REAL::CASEV,RNO3
	REAL::UNH4(NL),PROVEG,CTONODR,AGGRT,SLPF,TRLV
	REAL::CDMREP,TURADD,PNTIM(300),CSAVEV,POTWTD,ARGNOD,UNO3(NL)
	REAL::NDMTOT,FLAGSD,SATFAC,NDMOLD,GRRAT1,GDMSD
	REAL::PConc_Shel,PConc_Shut,RTDEP,PCNVEG,CSAVEC,XFRT,NRUSHH
	REAL::PCLSD,PODWTD,FracRts(NL),Puptake(NL),PConc_Seed,PConc_Root
	TYPE(ResidueType) SENESCE
	REAL::TRNH4U,SSNDOT,SRDOT,SLNDOT,SENRT(NL),SENNOD(NL),SDIDOT
	REAL::WNRLF,WTNNOD,ShelMob,RootMob,ShutMob,XLAI,YRPLT,WRIDOTN
	REAL::TRNO3U,TRNU,GROWTH
	REAL::PCNST,PCNSH,SDRATE,WTNNAG,WTNEW,WTMAIN,WNRST,WNRSH,WNRRT
	REAL::CANWAA,CANNAA,PCNSD,PCNRT,PCNL,PCLCD,PCCSD,LAIMX,GRWRES
	REAL::RO,RP,NAVL,MAINR,CTONOD,CNODMN,CNOD,CMINEP
	REAL::PPLTD,SLDOT,RSPNH4,RSPNO3,SWFAC,TURFAC,RPROAV
	REAL::ASMDOT,RLV(NL),WSHIDT,SWIDOT,SSDOT,SLA,PHTIM(300),PGAVL
	REAL::XHLAI,SLAAD,RNITP,PStres1,WSIDOT,WRIDOT,WLIDOT,SDDES(300),PPLTD
	REAL::NDSET,NDLEAF,FRACDN,DXR57,DTX,DRPP,PStres2,PG
	REAL::VSTAGE,CRUSSH,VegFrac,TDUMX,TDUMX2,RSTAGE,PHTHRS(20)
	REAL::DISLA,AGEFAC,YRNR7,YRNR5,YRNR2,YRNR1,SeedFrac
	REAL::NRUSST,NRUSSH,NRUSRT,NRUSLF,NPLTD,NODGR,NMINEA,NFIXN,NDTH
	REAL::RHOS,RHOSH,RHOR,RHOL
	REAL::SDPROR,AGRVG2,AGRVG,AGRSTM,AGRSH2,AGRSH1,AGRSD1,AGRSD2
	REAL::AGRRT,AGRNOD,AGRLF
	REAL::DAYS(300),WPODY(300),MSHELN(300),DTC(300),DWC
	REAL::XP2DET,XP1DET,PR1DET,PR2DET,TO1(5),TO2(5),TM(5),TB(5)
	INTEGER::NR2TIM,YREMRG,STGDOY(20),RVSTGE,NVEG0,NR1,NR2,NR5,NR7
	INTEGER::MDATE,YREND
	CHARACTER(len=3)::TYPPGT,TYPPGN
	REAL::PARMAX,PHTMAX,LAI
	REAL::PGREF,LNREF,CCEFF,CCMAX,CCMP
	REAL::XRTFAC(4),YRTFAC(4),RTWTMIN,YPGSLW(15),XPGSLW(15)
	REAL::RTEXF,RTSDF,RLDSM,RTSEN,RFAC1,RTDEPI
	REAL::WRCSDT,WRCRDT,WRCSHD,XPOD,WTFL,RTNO3,RTNH4
	REAL::CADPR1,RFIXN,NOUTDO,PCH2O
	REAL::FRCNOD,TTFIX,RES30C,KC_SLOPE,R30C2,RNH4C,KCAN
	REAL::EORATIO,KEP,PORMIN,RWUMX,RWUEP1,FREEZ1,FREEZ2
	REAL::WTSHE(300),PGAVLR,SDNO(300),SHELN(300),FLWN(300)
	REAL::PCTMAT,NGRSH,NGRSD,NAVPOD,FNINSH,WTABRT,WTSHM
	REAL::WTSD(300),WTSHMT
	REAL::WSHDOT,CPSTRES,CNSTRES,RSD,CUMSIG,ANINSD,AGRSD3,POTLIP
	REAL::POTCAR,WSHDTN,WSDDTN,PODNO,PGNPOD
	REAL::SHVAR,SDVAR,LAGSD,LNGPEG,MNESPM,YSWBAR(10),XSWBAR(10)
	REAL::XSWFAC(10),YSWFAC(10),PROSHI,SETMAX,RFLWAB,XMPAGE,DSWBAR
	CHARACTER(len=6)::ECOTYP
	CHARACTER(len=3)::TYPPDT
	REAL::FRRT,TMPFAC,NMINEP,SDGR,NMOBR,NDMVEG,NDMSDR
	REAL::NMOBMX,CDMSDR,GDMSDR,FNINSD,NDMNEW,NDMREP,NDMSD,NDMSH
	REAL::XFRUIT,THRESH,RPRO,FRSTM,FRLF,SIZRAT,GROMAX,SLAMX
	REAL::SLAMN,FVEG,F,DUMFAC,RPRPUN,ALFDOT
	REAL::YVGROW(6)
	REAL::YTRFAC(10),YVREF(6),YXFTEM(10)
	REAL::XTRFAC(10),XVGROW(6),XXFTEM(10),YLEAF(25),YSLATM(10)
	REAL::YSTEM(25)
	REAL::VSSINK,XFRMAX,XFUIT,XLEAF(25),XSLATM(10)
	REAL::SIZELF,SIZREF,SRMAX,THRES,TURSLA
	REAL::SLAMIN,SLAPAR,SLAREF,SLAVAR,SLOSUM
	REAL::RLIG,RLIP,RNO3C,ROA,SHLAG,SLAMAX
	REAL::CARMIN,FINREF,FNSDT(4),FRLFF,FRLFMX,FRSTMF
	REAL::LIPOPT,LIPTB,LNGSH,NRCVR,NVSMOB,RCH2O
	REAL::WTNOO,WTNTOT,SDNPL,SEEDNI
	REAL::WLDOT,WSDOT,WRDOT,WNDOT,WPDOT,WLFDOT
	REAL::WTPSD,PLIPRT,BETN,CPFLF,CPFSTM,CPFRT,CPFNOD	
	REAL::CPFSH1,CPFSD1,PCNMIN,PROLLF
	CHARACTER(len=1)::PLME,DETACH,RWMTXT
	CHARACTER(len=2)::XPODF
	CHARACTER(len=3)::TYPSDT
	REAL::NGRSTG,NGRRTG,NGRVGG
	REAL::FNINL,FNINS,FNINR,NGRVEG,NGRLFG
	REAL::PROLFG,PROSTG,PRORTG,CMOBMX,CADSTF,ATOP
	REAL::CANHT,CANWH,CADLF,CADST,CMINEA,CRUSLF,CRUSRT
	REAL::CRUSST,CUMTUR,EXCESS,FNINLG,FNINRG,FNINSG,NADLF
	REAL::NADRT,NADST,NGRLF,NGRRT,NGRST,NTRES,PGLEFT,SUPPN
	REAL::TNLEAK,VGRDEM,WLDOTN,WRDOTN,WSDOTN,NSTRES
	REAL::ALPHL,ALPHR,ALPHS,ALPHSH,PCARLF,PCARST
	REAL::PCARRT,PCARSH,PCARSD,PCARNO,PLIGLF
	REAL::PLIGST,PLIGRT,PLIGSH,PLIGSD,PLIGNO
	REAL::PLIPLF,PLIPST,PLIRT,PLIPSH,PLIPNO
	REAL::PMINLF,PMINST,PMINRT,PMINSH,PMINSD,PMINNO
	REAL::POALF,POAST,POART,POASH,POASD,POANO
	REAL::PROLFF,PROSTF,PRORTF,PROSHF,PRONOD
	REAL::PROLFI,PROSTI,PRORTI,ROWSPC,RMIN,SDWTPL
	REAL::SDLIP,SDPRO,WTFDS,WTFSD
	REAL::AREALF,CLW,CSW,WTNLA,TOTWT,TOPWT,WTLF,STMWT
	REAL::SDWT,SHELWT,RTWT,PODWT,DWNOD,TGROW
	REAL::WCRLF,WCRST,WCRRT,WCRSH ! Concetrations in Leaves,Stems,ROOTS and Shells
	REAL::WTLO,WTSO,WTRO,WTSHO,WTSDO,WTNDOO,WTCO ! Cumulative C loss during season
	REAL::NLPEST	
	REAL:: WTNLF ! Total Nitrogen in leaves
	REAL::WTNST ! Total nitrogen in the stems
	REAL::WTNRT ! Total nitrogen in the roots
	REAL::WTNSH ! Total nitrogen in the shells
	REAL::WTNSD ! total nitrogen in hte seed
	REAL::WTNCAN ! Total nitrogen in the canopy
	REAL::WTNSA,WTNRA,WTNSHA,WTNSDA
	!NODULES	
	REAL:: DWNODA,WTNFX
	! CUMULATIVE N LOSS During SEASON
	REAL:: WTNLO,WTNSO,WTNRO,WTNSHO,WTNSDO,WTNNO
	! N Balance components
	REAL::WTNMOB,WTNUP,WTNNA
	! PEST DAMAGE TO SEEDS	
	REAL::PUNCSD,SEEDNO,PUNCTR
	! Lost in Plants due to Pest
	REAL:: PLTPOP
	!SENES
	REAL::ICMP,SENDAY,SENRT2,SENRTE,TCMP,PORPT
	REAL::SENMAX(4),SENPOR(4),XSENMX(4),XSTAGE(4)
	REAL::RATTP,SWFCAB(5)
      END TYPE PLANT_GROWTH

	TYPE LAND_S
	REAL::SWDELTX(NL),UPFLOW(NL),KTRANS
	REAL::KUptake(NL),NH4_plant(NL),NO3_plant(NL),SKi_AVAIL(NL)
	REAL::SPi_AVAIL(NL),SomLitC(0:NL),SomLitE(0:NL,NELEM)
	REAL::SW(NL),SWDELTS(NL),SWDELTU(NL),UPPM(NL),WINF,PUptake(NL)
	REAL::ST(NL),XHLAI,KSEVAP,EOS,RWU(NL),SRFTEMP,TRWU,TRWUP
	END TYPE LAND_S	
		
!     Data which can be transferred between modules
      Type TransferType
        Type (ControlType) CONTROL
        Type (SwitchType)  ISWITCH
        Type (OutputType)  OUTPUT
        Type (PlantType)   PLANT
        Type (MgmtType)    MGMT
        Type (NiType)      NITR
        Type (OrgCType)    ORGC
        Type (SoilType)    SOILPROP
        Type (SPAMType)    SPAM
        Type (WatType)     WATER
        Type (WeathType)   WEATHER
        TYPE (PDLABETATYPE) PDLABETA
	TYPE (ToSwatType) ToSwat ! JJ added
	TYPE (PLANT_GROWTH) PlantG
	TYPE (LAND_S) 	S_Land 
      End Type TransferType

!     The variable SAVE_data contains all of the components to be 
!     stored and retrieved.
      Type (TransferType) SAVE_data

!======================================================================
!     GET and PUT routines are differentiated by argument type.  All of 
!       these procedures can be accessed with a CALL GET(...)
      INTERFACE GET
         MODULE PROCEDURE GET_Control
     &                  , GET_ISWITCH 
     &                  , GET_Output 
     &                  , GET_SOILPROP
!     &                  , GET_Weather
     &                  , GET_Real 
     &                  , GET_Real_Array_NL
     &                  , GET_Integer
     &                  , GET_Char
     &			, GET_PlantG
     &                  , GET_S_Land
     &	                , GET_ToSwat  
      END INTERFACE

      INTERFACE PUT
         MODULE PROCEDURE PUT_Control
     &                  , PUT_ISWITCH 
     &                  , PUT_Output 
     &                  , PUT_SOILPROP
!     &                  , PUT_Weather
     &                  , PUT_Real 
     &                  , PUT_Real_Array_NL
     &                  , PUT_Integer
     &                  , PUT_Char
     &			, PUT_PlantG
     &                  , PUT_S_Land 
     &                  , PUT_ToSwat		
      END INTERFACE

      CONTAINS

!----------------------------------------------------------------------
      Subroutine Get_CONTROL (CONTROL_arg)
!     Retrieves CONTROL variable
      IMPLICIT NONE
      Type (ControlType) Control_arg
      Control_arg = SAVE_data % Control
      Return
      End Subroutine Get_CONTROL

!----------------------------------------------------------------------
      Subroutine Put_CONTROL (CONTROL_arg)
!     Stores CONTROL variable
      IMPLICIT NONE
      Type (ControlType) Control_arg
      SAVE_data % Control = Control_arg
      Return
      End Subroutine Put_CONTROL

!----------------------------------------------------------------------
      Subroutine Get_ISWITCH (ISWITCH_arg)
!     Retrieves ISWITCH variable
      IMPLICIT NONE
      Type (SwitchType) ISWITCH_arg
      ISWITCH_arg = SAVE_data % ISWITCH
      Return
      End Subroutine Get_ISWITCH

!----------------------------------------------------------------------
      Subroutine Put_ISWITCH (ISWITCH_arg)
!     Stores ISWITCH variable
      IMPLICIT NONE
      Type (SwitchType) ISWITCH_arg
      SAVE_data % ISWITCH = ISWITCH_arg
      Return
      End Subroutine Put_ISWITCH

!----------------------------------------------------------------------
      SUBROUTINE GET_OUTPUT(OUTPUT_ARG)
!     Retrieves OUTPUT variable as needed
      IMPLICIT NONE
      TYPE (OutputType) OUTPUT_ARG
      OUTPUT_ARG = SAVE_data % OUTPUT
      RETURN
      END SUBROUTINE GET_OUTPUT

!----------------------------------------------------------------------
      SUBROUTINE PUT_OUTPUT(OUTPUT_ARG)
!     Stores OUTPUT variable 
      IMPLICIT NONE
      TYPE (OutputType) OUTPUT_ARG
      SAVE_data % OUTPUT = OUTPUT_ARG
      RETURN
      END SUBROUTINE PUT_OUTPUT

!----------------------------------------------------------------------
      SUBROUTINE GET_SOILPROP(SOIL_ARG)
!     Retrieves SOILPROP variable as needed
      IMPLICIT NONE
      TYPE (SoilType) SOIL_ARG
      SOIL_ARG = SAVE_data % SOILPROP
      RETURN
      END SUBROUTINE GET_SOILPROP

!----------------------------------------------------------------------
      SUBROUTINE PUT_SOILPROP(SOIL_ARG)
!     Stores SOILPROP variable 
      IMPLICIT NONE
      TYPE (SoilType) SOIL_ARG
      SAVE_data % SOILPROP = SOIL_ARG
      RETURN
      END SUBROUTINE PUT_SOILPROP
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE PUT_PlantG(PlantG_ARG)
	IMPLICIT NONE
	TYPE(PLANT_GROWTH) PlantG_ARG
	        !write(*,*)'ModuleDefs,Put PlantG%XLEAF=',PlantG_ARG%XLEAF
	SAVE_data%PlantG=PlantG_ARG
		!write(*,*)'ModuleDefs, Saved data',SAVE_data%PlantG%XLEAF
	RETURN
      END SUBROUTINE PUT_PlantG	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE GET_PlantG(PlantG_ARG)
	IMPLICIT NONE
	TYPE(PLANT_GROWTH) PlantG_ARG
	
		PlantG_ARG=SAVE_data%PlantG
	

	!write(*,*)'ModuleDefs,GET DATA PlantG%XLEAF=',
     	!& SAVE_data%PlantG%XLEAF
	!write(*,*)'Returned data:',PlantG_ARG%XLEAF
	RETURN
      END SUBROUTINE GET_PlantG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	SUBROUTINE PUT_S_Land(S_Land_ARG)
	IMPLICIT NONE
	TYPE(LAND_S) S_Land_ARG
	SAVE_data%S_Land=S_land_ARG
	END SUBROUTINE PUT_S_Land
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	SUBROUTINE GET_S_Land(S_Land_ARG)
	IMPLICIT NONE
	TYPE(LAND_S) S_Land_ARG
		S_Land_ARG=SAVE_data%S_Land
	RETURN
	END SUBROUTINE GET_S_Land

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE PUT_ToSwat(ToSwat_ARG)
        IMPLICIT NONE
        TYPE(ToSwatType) ToSwat_ARG
        SAVE_data%ToSwat=ToSwat_ARG
        END SUBROUTINE PUT_ToSwat
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        SUBROUTINE GET_ToSwat(ToSwat_ARG)
        IMPLICIT NONE
        TYPE(ToSwatType) ToSwat_ARG
                ToSwat_ARG=SAVE_data%ToSwat
        RETURN
        END SUBROUTINE GET_ToSwat




!!----------------------------------------------------------------------
!      SUBROUTINE GET_WEATHER(WEATHER_ARG)
!!     Retrieves WEATHER variable as needed
!      IMPLICIT NONE
!      TYPE (WeathType) WEATHER_ARG
!      WEATHER_ARG = SAVE_data % WEATHER
!      RETURN
!      END SUBROUTINE GET_WEATHER
!
!!----------------------------------------------------------------------
!      SUBROUTINE PUT_WEATHER(WEATHER_ARG)
!!     Stores WEATHER variable 
!      IMPLICIT NONE
!      TYPE (WeathType) WEATHER_ARG
!      SAVE_data % WEATHER = WEATHER_ARG
!      RETURN
!      END SUBROUTINE PUT_WEATHER

!----------------------------------------------------------------------
      Subroutine GET_Real(ModuleName, VarName, Value)
!     Retrieves real variable from SAVE_data.  Variable must be
!         included as a component of SAVE_data. 
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      Real Value
      Logical ERR

      Value = 0.0
      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('SPAM')
        SELECT CASE (VarName)
        Case ('AGEFAC'); Value = SAVE_data % SPAM % AGEFAC
        Case ('PG');     Value = SAVE_data % SPAM % PG
        Case ('CEF');    Value = SAVE_data % SPAM % CEF
        Case ('CEM');    Value = SAVE_data % SPAM % CEM
        Case ('CEO');    Value = SAVE_data % SPAM % CEO
        Case ('CEP');    Value = SAVE_data % SPAM % CEP
        Case ('CES');    Value = SAVE_data % SPAM % CES
        Case ('CET');    Value = SAVE_data % SPAM % CET
        Case ('CEVAP');  Value = SAVE_data % SPAM % CEVAP
        Case ('EF');     Value = SAVE_data % SPAM % EF
        Case ('EM');     Value = SAVE_data % SPAM % EM
        Case ('EO');     Value = SAVE_data % SPAM % EO
        Case ('EP');     Value = SAVE_data % SPAM % EP
        Case ('ES');     Value = SAVE_data % SPAM % ES
        Case ('ET');     Value = SAVE_data % SPAM % ET
        Case ('EOP');    Value = SAVE_data % SPAM % EOP
        Case ('EVAP');   Value = SAVE_data % SPAM % EVAP
        Case ('REFET');  Value = SAVE_data % SPAM % REFET
        Case ('SKC');    Value = SAVE_data % SPAM % SKC
        Case ('KCBMAX'); Value = SAVE_data % SPAM % KCBMAX
        Case ('KCB');    Value = SAVE_data % SPAM % KCB
        Case ('KE');     Value = SAVE_data % SPAM % KE
        Case ('KC');     Value = SAVE_data % SPAM % KC
        Case ('PHSV');   Value = SAVE_data % SPAM % PHSV
        Case ('PHTV');   Value = SAVE_data % SPAM % PHTV
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('BIOMAS'); Value = SAVE_data % PLANT % BIOMAS
        Case ('CANHT') ; Value = SAVE_data % PLANT % CANHT
        Case ('CANWH') ; Value = SAVE_data % PLANT % CANWH
        Case ('DXR57') ; Value = SAVE_data % PLANT % DXR57
        Case ('EXCESS'); Value = SAVE_data % PLANT % EXCESS
        Case ('PLTPOP'); Value = SAVE_data % PLANT % PLTPOP
        Case ('RNITP') ; Value = SAVE_data % PLANT % RNITP
        Case ('SLAAD') ; Value = SAVE_data % PLANT % SLAAD
        Case ('XPOD')  ; Value = SAVE_data % PLANT % XPOD
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('MGMT')
        SELECT CASE (VarName)
        Case ('EFFIRR'); Value = SAVE_data % MGMT % EFFIRR
        Case ('TOTIR');  Value = SAVE_data % MGMT % TOTIR
        Case ('TOTEFFIRR');Value=SAVE_data % MGMT % TOTEFFIRR
        Case ('DEPIR');  Value = SAVE_data % MGMT % DEPIR
        Case ('IRRAMT'); Value = SAVE_data % MGMT % IRRAMT
        Case ('FERNIT'); Value = SAVE_data % MGMT % FERNIT
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('WATER')
        SELECT CASE (VarName)
        Case ('DRAIN'); Value = SAVE_data % WATER % DRAIN
        Case ('RUNOFF');Value = SAVE_data % WATER % RUNOFF
        Case ('SNOW');  Value = SAVE_data % WATER % SNOW
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('NITR')
        SELECT CASE (VarName)
        Case ('TNOXD'); Value = SAVE_data % NITR % TNOXD
       Case ('TLCHD'); Value = SAVE_data % NITR % TLeachD
!       Case ('TN2OD'); Value = SAVE_data % NITR % TN2OD
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('ORGC')
        SELECT CASE (VarName)
        Case ('MULCHMASS');Value = SAVE_data % ORGC % MULCHMASS
        Case ('TOMINFOM'); Value = SAVE_data % ORGC % TOMINFOM
        Case ('TOMINSOM'); Value = SAVE_data % ORGC % TOMINSOM
        Case ('TOMINSOM1');Value = SAVE_data % ORGC % TOMINSOM1
        Case ('TOMINSOM2');Value = SAVE_data % ORGC % TOMINSOM2
        Case ('TOMINSOM3');Value = SAVE_data % ORGC % TOMINSOM3
        Case ('TNIMBSOM'); Value = SAVE_data % ORGC % TNIMBSOM
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('SOIL')
        SELECT CASE (VarName)
        Case ('TOMINFOM'); Value = SAVE_data % ORGC % TOMINFOM
        Case ('TOMINSOM'); Value = SAVE_data % ORGC % TOMINSOM
        Case ('TOMINSOM1');Value = SAVE_data % ORGC % TOMINSOM1
        Case ('TOMINSOM2');Value = SAVE_data % ORGC % TOMINSOM2
        Case ('TOMINSOM3');Value = SAVE_data % ORGC % TOMINSOM3
        Case ('TNIMBSOM'); Value = SAVE_data % ORGC % TNIMBSOM
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      CASE ('PDLABETA')
        SELECT CASE(VarName)
        CASE('PDLA'); Value = SAVE_data % PDLABETA % PDLA
        CASE('BETA'); Value = SAVE_data % PDLABETA % BETALS
        CASE DEFAULT; ERR = .TRUE.
        END SELECT
            
      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, " in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_REAL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Real

!----------------------------------------------------------------------
      SUBROUTINE PUT_Real(ModuleName, VarName, Value)
!     Stores real variable SAVE_data.  
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      Real Value
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('SPAM')
        SELECT CASE (VarName)
        Case ('AGEFAC'); SAVE_data % SPAM % AGEFAC = Value
        Case ('PG');     SAVE_data % SPAM % PG     = Value
        Case ('CEF');    SAVE_data % SPAM % CEF    = Value
        Case ('CEM');    SAVE_data % SPAM % CEM    = Value
        Case ('CEO');    SAVE_data % SPAM % CEO    = Value
        Case ('CEP');    SAVE_data % SPAM % CEP    = Value
        Case ('CES');    SAVE_data % SPAM % CES    = Value
        Case ('CET');    SAVE_data % SPAM % CET    = Value
        Case ('CEVAP');  SAVE_data % SPAM % CEVAP  = Value
        Case ('EF');     SAVE_data % SPAM % EF     = Value
        Case ('EM');     SAVE_data % SPAM % EM     = Value
        Case ('EO');     SAVE_data % SPAM % EO     = Value
        Case ('EP');     SAVE_data % SPAM % EP     = Value
        Case ('ES');     SAVE_data % SPAM % ES     = Value
        Case ('ET');     SAVE_data % SPAM % ET     = Value
        Case ('EOP');    SAVE_data % SPAM % EOP    = Value
        Case ('EVAP');   SAVE_data % SPAM % EVAP   = Value
        Case ('REFET');  SAVE_data % SPAM % REFET  = Value
        Case ('SKC');    SAVE_data % SPAM % SKC    = Value
        Case ('KCBMAX'); SAVE_data % SPAM % KCBMAX = Value
        Case ('KCB');    SAVE_data % SPAM % KCB    = Value
        Case ('KE');     SAVE_data % SPAM % KE     = Value
        Case ('KC');     SAVE_data % SPAM % KC     = Value
        Case ('PHSV');   SAVE_data % SPAM % PHSV   = Value
        Case ('PHTV');   SAVE_data % SPAM % PHTV   = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('BIOMAS'); SAVE_data % PLANT % BIOMAS = Value
        Case ('CANHT');  SAVE_data % PLANT % CANHT  = Value
        Case ('CANWH');  SAVE_data % PLANT % CANWH  = Value
        Case ('DXR57');  SAVE_data % PLANT % DXR57  = Value
        Case ('EXCESS'); SAVE_data % PLANT % EXCESS = Value
        Case ('PLTPOP'); SAVE_data % PLANT % PLTPOP = Value
        Case ('RNITP');  SAVE_data % PLANT % RNITP  = Value
        Case ('SLAAD');  SAVE_data % PLANT % SLAAD  = Value
        Case ('XPOD');   SAVE_data % PLANT % XPOD   = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('MGMT')
        SELECT CASE (VarName)
        Case ('EFFIRR'); SAVE_data % MGMT % EFFIRR = Value
        Case ('TOTIR');  SAVE_data % MGMT % TOTIR  = Value
        Case ('TOTEFFIRR');SAVE_data%MGMT % TOTEFFIRR=Value
        Case ('DEPIR');  SAVE_data % MGMT % DEPIR  = Value
        Case ('IRRAMT'); SAVE_data % MGMT % IRRAMT = Value
        Case ('FERNIT'); SAVE_data % MGMT % FERNIT = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('WATER')
        SELECT CASE (VarName)
        Case ('DRAIN'); SAVE_data % WATER % DRAIN  = Value
        Case ('RUNOFF');SAVE_data % WATER % RUNOFF = Value
        Case ('SNOW');  SAVE_data % WATER % SNOW   = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('NITR')
        SELECT CASE (VarName)
        Case ('TNOXD'); SAVE_data % NITR % TNOXD = Value
        Case ('TLCHD'); SAVE_data % NITR % TLeachD = Value
!       Case ('TN2OD'); SAVE_data % NITR % TN2OD = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('ORGC')
        SELECT CASE (VarName)
        Case ('MULCHMASS');SAVE_data % ORGC % MULCHMASS = Value
        Case ('TOMINFOM'); SAVE_data % ORGC % TOMINFOM  = Value
        Case ('TOMINSOM'); SAVE_data % ORGC % TOMINSOM  = Value
        Case ('TOMINSOM1');SAVE_data % ORGC % TOMINSOM1 = Value
        Case ('TOMINSOM2');SAVE_data % ORGC % TOMINSOM2 = Value
        Case ('TOMINSOM3');SAVE_data % ORGC % TOMINSOM3 = Value
        Case ('TNIMBSOM'); SAVE_data % ORGC % TNIMBSOM  = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      CASE ('PDLABETA')
        SELECT CASE(VarName)
        CASE('PDLA'); SAVE_data % PDLABETA % PDLA = Value
        CASE('BETA'); SAVE_data % PDLABETA % BETALS = Value
        CASE DEFAULT; ERR = .TRUE.
        END SELECT
            
      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_REAL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Real

!----------------------------------------------------------------------
      SUBROUTINE GET_Real_Array_NL(ModuleName, VarName, Value)
!     Retrieves array of dimension(NL) 
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      REAL, DIMENSION(NL) :: Value
      Logical ERR

      Value = 0.0
      ERR = .FALSE.

      SELECT CASE (ModuleName)

      CASE ('SPAM')
        SELECT CASE (VarName)
          CASE ('UH2O'); ; Value = SAVE_data % SPAM % UH2O
          CASE DEFAULT; ERR = .TRUE.
        END SELECT
	CASE('ToSwat')
	SELECT CASE(VarName)
	CASE('SPi_Soluble'); Value=SAVE_data%ToSwat%SPi_Soluble
	CASE('NO3');Value=SAVE_data%ToSWat%NO3
	CASE('NH4');VAlue=SAVE_data%ToSwat%NH4
	CASE('SOM1E1');Value=SAVE_data%ToSwat%SOM1E1
	CASE('SOM1E2');Value=SAVE_data%ToSwat%SOM1E2
	CASE('SOM1E3');Value=SAVE_data%ToSwat%SOM1E3
	CASE('SOM2E1');Value=SAVE_data%ToSwat%SOM2E1
        CASE('SOM2E2');Value=SAVE_data%ToSwat%SOM2E2
        CASE('SOM2E3');Value=SAVE_data%ToSwat%SOM2E3
	CASE('SOM3E1');Value=SAVE_data%ToSwat%SOM3E1
        CASE('SOM3E2');Value=SAVE_data%ToSwat%SOM3E2
        CASE('SOM3E3');Value=SAVE_data%ToSwat%SOM3E3
        CASE('SOM23E1');Value=SAVE_data%ToSwat%SOM23E1
        CASE('SOM23E2');Value=SAVE_data%ToSwat%SOM23E2
        CASE('SOM23E3');Value=SAVE_data%ToSwat%SOM23E3

	CASE DEFAULT
	END SELECT
        CASE DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_Real_Array_NL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Real_Array_NL

!----------------------------------------------------------------------
      SUBROUTINE PUT_Real_Array_NL(ModuleName, VarName, Value)
!     Stores array of dimension NL
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      REAL, DIMENSION(NL) :: Value
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('SPAM')
        SELECT CASE (VarName)
        Case ('UH2O'); SAVE_data % SPAM % UH2O = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT
	CASE('ToSwat')
	SELECT CASE(VarName)
	case('SPi_Soluble'); SAVE_data%ToSwat%SPi_Soluble=Value
	case('NO3');SAVE_data%ToSwat%NO3=Value
	case('NH4');SAVE_data%ToSwat%NH4=Value
	case('SOM1E1');SAVE_data%ToSwat%SOM1E1=VAlue
	case('SOM1E2');SAVE_data%ToSwat%SOM1E2=VAlue
        case('SOM1E3');SAVE_data%ToSwat%SOM1E3=VAlue
        case('SOM2E1');SAVE_data%ToSwat%SOM2E1=VAlue
        case('SOM2E2');SAVE_data%ToSwat%SOM2E2=VAlue
        case('SOM2E3');SAVE_data%ToSwat%SOM2E3=VAlue
        case('SOM3E1');SAVE_data%ToSwat%SOM3E1=VAlue
        case('SOM3E2');SAVE_data%ToSwat%SOM3E2=VAlue
        case('SOM3E3');SAVE_data%ToSwat%SOM3E3=VAlue
        case('SOM23E1');SAVE_data%ToSwat%SOM23E1=VAlue
        case('SOM23E2');SAVE_data%ToSwat%SOM23E2=VAlue
        case('SOM23E3');SAVE_data%ToSwat%SOM23E3=VAlue

	Case DEFAULT;ERR=.TRUE.
	END SELECT
      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_Real_Array_NL',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Real_Array_NL

!----------------------------------------------------------------------
      Subroutine GET_Integer(ModuleName, VarName, Value)
!     Retrieves Integer variable as needed
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78  MSG(2)
      Integer Value
      Logical ERR

      Value = 0
      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('NR5');  Value = SAVE_data % PLANT % NR5
        Case ('iSTAGE');  Value = SAVE_data % PLANT % iSTAGE
        Case ('iSTGDOY'); Value = SAVE_data % PLANT % iSTGDOY
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('WEATHER')
        SELECT CASE (VarName)
        Case ('WYEAR'); Value = SAVE_data % WEATHER % WYEAR
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case Default; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_INTEGER',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Integer

!----------------------------------------------------------------------
      SUBROUTINE PUT_Integer(ModuleName, VarName, Value)
!     Stores Integer variable
      IMPLICIT NONE
      Character*(*) ModuleName, VarName
      Character*78 MSG(2)
      Integer Value
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('NR5');  SAVE_data % PLANT % NR5  = Value
        Case ('iSTAGE');  SAVE_data % PLANT % iSTAGE  = Value
        Case ('iSTGDOY'); SAVE_data % PLANT % iSTGDOY = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('WEATHER')
        SELECT CASE (VarName)
        Case ('WYEAR'); SAVE_data % WEATHER % WYEAR = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_Integer',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Integer

!----------------------------------------------------------------------
      Subroutine GET_Char(ModuleName, VarName, Value)
!     Retrieves Integer variable as needed
      IMPLICIT NONE
      Character*(*) ModuleName, VarName, Value
      Character*78  MSG(2)
      Logical ERR

      Value = ' '
      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('WEATHER')
        SELECT CASE (VarName)
        Case ('WSTA');  Value = SAVE_data % WEATHER % WSTAT
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('iSTNAME');  Value = SAVE_data % PLANT % iSTNAME
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case Default; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value set to zero.'
        CALL WARNING(2,'GET_INTEGER',MSG)
      ENDIF

      RETURN
      END SUBROUTINE GET_Char

!----------------------------------------------------------------------
      SUBROUTINE PUT_Char(ModuleName, VarName, Value)
!     Stores Character variable
      IMPLICIT NONE
      Character*(*) ModuleName, VarName, Value
      Character*78 MSG(2)
      Logical ERR

      ERR = .FALSE.

      SELECT CASE (ModuleName)
      Case ('WEATHER')
        SELECT CASE (VarName)
        Case ('WSTA');  SAVE_data % WEATHER % WSTAT  = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case ('PLANT')
        SELECT CASE (VarName)
        Case ('iSTNAME');  SAVE_data % PLANT % iSTNAME = Value
        Case DEFAULT; ERR = .TRUE.
        END SELECT

      Case DEFAULT; ERR = .TRUE.
      END SELECT

      IF (ERR) THEN
        WRITE(MSG(1),'("Error transferring variable: ",A, "in ",A)') 
     &      Trim(VarName), Trim(ModuleName)
        MSG(2) = 'Value not saved! Errors may result.'
        CALL WARNING(2,'PUT_Integer',MSG)
      ENDIF

      RETURN
      END SUBROUTINE PUT_Char

!======================================================================
      END MODULE ModuleData
!======================================================================
