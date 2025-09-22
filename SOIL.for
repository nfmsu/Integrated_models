!=======================================================================
!  COPYRIGHT 1998-2010 The University of Georgia, Griffin, Georgia
!                      University of Florida, Gainesville, Florida
!                      Iowa State University, Ames, Iowa
!                      International Center for Soil Fertility and 
!                       Agricultural Development, Muscle Shoals, Alabama
!                      University of Guelph, Guelph, Ontario
!  ALL RIGHTS RESERVED
!=======================================================================
!  SOIL, Subroutine
!-----------------------------------------------------------------------
!  Soil Processes subroutine.  Calls the following modules:
!     SOILDYN     - integrates soil properties variables
!     WATBAL      - soil water balance
!     SoilN_inorg - inorganic soil N (from NTRANS)
!     SoilPi      - inorganic soil P
!     SoilKi      - inorganic soil K
!     SoilOrg     - Ceres soil organic matter (from NTRANS)
!     CENTURY     - Century soil organic matter
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  11/02/2001 CHP Written
!  04/20/2002 GH  Modified for crop rotations
!  10/24/2005 CHP Put weather variables in constructed variable. 
!  10/25/2005 CHP Removed NTRANS_OLD module
!  02/22/2006 CHP Added tiledrain.
!  03/03/2006 CHP Added tillage (A.Andales & WDBatchelor).
!  03/21/2006 CHP Added mulch effects
!  07/14/2006 CHP Added P model, split inorganic and organic N routines,
!                 move fertilizer and organic matter placement routines
!                 to management module.
!  10/31/2007 CHP Added simple K model.
C=====================================================================

      SUBROUTINE SOIL(CONTROL, ISWITCH, 
     &    ES, FERTDATA, HARVRES, IRRAMT, KTRANS,          !Input
     &    KUptake, OMAData, PUptake, SENESCE, ST,         !Input
     &    FracRts, SWDELTX,TILLVALS, UNH4, UNO3, UPFLOW,  !Input
     &    WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH,        !I/O
     &    NH4_plant, NO3_plant, SKi_AVAIL, SNOW,          !Output
     &    SPi_AVAIL, SOILPROP, SomLitC, SomLitE,          !Output
     &    SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)        !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      USE FloodModule
	USE interface2
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------
!     Interface variables:
!-----------------------------------------------------------------------
!     Input:
      TYPE (ControlType) , INTENT(IN) :: CONTROL
      TYPE (SwitchType)  , INTENT(IN) :: ISWITCH
      REAL               , INTENT(IN) :: ES
      TYPE (FertType)    , INTENT(IN) :: FERTDATA
      Type (ResidueType) , INTENT(IN) :: HARVRES
      REAL               , INTENT(IN) :: IRRAMT
      REAL               , INTENT(IN) :: KTRANS
      TYPE (OrgMatAppType),INTENT(IN) :: OMAData
      REAL, DIMENSION(NL), INTENT(IN) :: PUptake, KUptake
      Type (ResidueType) , INTENT(IN) :: SENESCE
!     REAL               , INTENT(IN) :: SRFTEMP 
      REAL, DIMENSION(NL), INTENT(IN) :: ST
      REAL, DIMENSION(NL), INTENT(IN) :: FracRts
      REAL, DIMENSION(NL), INTENT(IN) :: SWDELTX
      TYPE (TillType)    , INTENT(IN) :: TILLVALS
      REAL, DIMENSION(NL), INTENT(IN) :: UNH4, UNO3
      TYPE (WeatherType) , INTENT(IN) :: WEATHER
      REAL               , INTENT(IN) :: XHLAI

      REAL, DIMENSION(NL) :: SomLit 

!     Input/Output:
      REAL, DIMENSION(NL), INTENT(INOUT) :: UPFLOW
      TYPE (FloodNType)   FLOODN
      TYPE (FloodWatType) FLOODWAT
      TYPE (MulchType)    MULCH

!     Output:
      REAL, DIMENSION(NL), INTENT(OUT) :: NH4_plant
      REAL, DIMENSION(NL), INTENT(OUT) :: NO3_plant
      REAL, DIMENSION(NL), INTENT(OUT) :: UPPM
      REAL, DIMENSION(NL), INTENT(OUT) :: SPi_AVAIL, SKi_AVAIL
      REAL               , INTENT(OUT) :: SNOW
      TYPE (SoilType)    , INTENT(OUT) :: SOILPROP
      REAL, DIMENSION(NL), INTENT(OUT) :: SW
      REAL, DIMENSION(NL), INTENT(OUT) :: SWDELTS
      REAL, DIMENSION(NL), INTENT(OUT) :: SWDELTU
      REAL               , INTENT(OUT) :: WINF
      INTEGER            , INTENT(OUT) :: YREND
      REAL, DIMENSION(0:NL) :: SomLitC
      REAL, DIMENSION(0:NL,NELEM) :: SomLitE

!-----------------------------------------------------------------------
!     Local variables:
      CHARACTER*1  MESOM

      INTEGER DYNAMIC

      REAL, DIMENSION(0:NL) :: newCO2 !DayCent
      REAL, DIMENSION(NL) :: DRN
      REAL, DIMENSION(NL) :: SPi_Labile, NO3, NH4
      REAL, DIMENSION(0:NL) :: LITC, SSOMC
      REAL, DIMENSION(0:NL,NELEM) :: IMM, MNR
      
!     Added for tile drainage:
      REAL TDFC
      INTEGER TDLNO
	DYNAMIC=0
	!write(*,*)'SOIL:DYNAMIC=',CONTROL%DYNAMIC
!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      MESOM   = ISWITCH % MESOM

!***********************************************************************
	!write(*,*)'SOIL,DYNAMIC=',DYNAMIC
	!write(*,*)'SOIL:TO SOILDYN'
!     Call Soil Dynamics module 
!      IF (DYNAMIC < OUTPUT) THEN
        CALL SOILDYN(CONTROL, ISWITCH, 
     &    KTRANS, MULCH, SomLit, SomLitC, SW, TILLVALS,   !Input
     &    WEATHER, XHLAI,                                 !Input
     &    SOILPROP)                                       !Output
!      ENDIF
	!write(*,*)'SOILDYN over,SOILPROP',SOILPROP%DLAYR
!     Call WATBAL first for all except seasonal initialization
	IF(useSWAT)THEN
	IF(DYNAMIC/=SEASINIT)THEN
!	if(interface_ihru==1)write(*,*)'SOIL To WATBAL SWDELTX=',SWDELTX
		CALL interface_WATBAL(DRN,SNOW,SW,SWDELTS,
     &TDFC,TDLNO,UPFLOW,WINF,30.0,SOILPROP%DLAYR,SOILPROP%DUL,
     &SOILPROP%LL,
     &SOILPROP%NLAYR,100.00,SWDELTX,IRRAMT)
		

!	if(interface_ihru==1)write(*,*)'SOIL,Root uptake=', SWDELTX
	END IF
	ELSE	
      IF (DYNAMIC /= SEASINIT) THEN
        CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX,                  !Input
     &    TILLVALS, WEATHER,                              !Input
     &    FLOODWAT, MULCH, SWDELTU,                       !I/O
     &    DRN, SNOW, SW, SWDELTS,                         !Output
     &    TDFC, TDLNO, UPFLOW, WINF)                      !Output
      ENDIF
	END IF
!	write(*,*)'SOIL,MESOM',MESOM,'To Century'
!	write(*,*)'Soil organic matter,MESOM =P is Century. MESOM=',MESOM
!     Soil organic matter modules
      IF (MESOM .EQ. 'P') THEN
!       Parton (Century-based) soil organic matter module
!	write(*,*)'Going to CENTURY'
        CALL CENTURY(CONTROL, ISWITCH, 
     &    FERTDATA, FLOODWAT, FLOODN, HARVRES, NH4,       !Input
     &    NO3, OMADATA, SENESCE, SOILPROP, SPi_Labile,    !Input
     &    ST, SW, TILLVALS,                               !Input
     &    IMM, LITC, MNR, MULCH, SomLit, SomLitC,         !Output
     &    SomLitE, SSOMC,                                 !Output
     &    newCO2)             !for DayCent in SOILNI added by PG
!	write(*,*)'Coming frim CENTURY'
      ELSE
!      ELSEIF (MESOM .EQ. 'G') THEN
!       Godwin (Ceres-based) soil organic matter module (formerly NTRANS)
        CALL SoilOrg (CONTROL, ISWITCH, 
     &    FLOODWAT, FLOODN, HARVRES, NH4, NO3, OMAData,   !Input
     &    SENESCE, SOILPROP, SPi_Labile, ST, SW, TILLVALS,!Input
     &    IMM, LITC, MNR, MULCH, newCO2, SomLit, SomLitC, !Output
     &    SomLitE, SSOMC)                                 !Output
      ENDIF
!	write(*,*)'SOIL:GOING TO SOilNi'
!     Inorganic N (formerly NTRANS)
      CALL SoilNi (CONTROL, ISWITCH, 
     &    DRN, ES, FERTDATA, FLOODWAT, IMM, LITC, MNR,    !Input
     &    newCO2, SNOW, SOILPROP, SSOMC, ST, SW, TDFC,    !Input
     &    TDLNO, TILLVALS, UNH4, UNO3, UPFLOW, WEATHER,   !Input
     &    XHLAI,                                          !Input
     &    FLOODN,                                         !I/O
     &    NH4, NO3, NH4_plant, NO3_plant, UPPM)           !Output
!	write(*,*)'SOIL:Going to SoilPi'
!     Inorganic P
      CALL SoilPi(CONTROL, ISWITCH, FLOODWAT, 
     &    FERTDATA, IMM, MNR, PUptake, SOILPROP,          !Input
     &    FracRts, SW, TillVals,                          !Input
     &    SPi_AVAIL, SPi_Labile, YREND)                   !Output
!	write(*,*)'SOIL:Going to SoilKi'
!     Inorganic K
      CALL SoilKi(CONTROL, ISWITCH, 
     &    FERTDATA, KUptake, SOILPROP, TILLVALS,          !Input
     &    SKi_Avail)                                      !Output

      IF (DYNAMIC == SEASINIT) THEN
	IF(useSWAT)THEN
!	write(*,*)'To interface_WATBAL'
		CALL interface_WATBAL(DRN,SNOW,SW,SWDELTS,
     &TDFC,TDLNO,UPFLOW,WINF,
     &30.0,SOILPROP%DLAYR,SOILPROP%DUL,SOILPROP%LL,
     &SOILPROP%NLAYR,100.00,SWDELTX,IRRAMT	)
	!WINF=WINF+IRRAMT
	!WRITE(*,*)'SOIL,ROOT UPTAKE=',SWDELTX
	ELSE
!       Soil water balance -- call last for initialization
        CALL WATBAL(CONTROL, ISWITCH, 
     &    ES, IRRAMT, SOILPROP, SWDELTX,                  !Input
     &    TILLVALS, WEATHER,                              !Input
     &    FLOODWAT, MULCH, SWDELTU,                       !I/O
     &    DRN, SNOW, SW, SWDELTS,                         !Output
     &    TDFC, TDLNO, UPFLOW, WINF)                      !Output
      ENDIF
	END IF

!***********************************************************************

!-----------------------------------------------------------------------
!	WRITE(*,*)'SOil over'
      RETURN
      END SUBROUTINE SOIL

!=======================================================================
