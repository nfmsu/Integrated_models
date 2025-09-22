C=======================================================================
C COPYRIGHT 1998-2021
C                     DSSAT Foundation
C                     University of Florida, Gainesville, Florida
C                     International Fertilizer Development Center
C 
C ALL RIGHTS RESERVED
C=======================================================================
C=======================================================================
C  CROPGRO, Subroutine, G. Hoogenboom, J.W. Jones, K.J. Boote, C. Porter
C-----------------------------------------------------------------------
C  CROPGRO template plant growth subroutine.
C  Computes plant development and growth.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1990 GH  Written.
C  04/24/1994 NBP Changed TAIRHR to TGRO.
C  07/14/1995 JWH Modified to reserve some C for nodule growth
C  08/21/1995 GH  Added seed composition.
C  01/13/1999 CHP Adapted for modular system
C  05/11/1999 GH  Further adaptation for modular system
C  03/16/2000 GH  Further adaptation for modular system
C  09/17/2001 CHP Removed CANNAA, RO and RP from output list.
C  09/21/2001 CHP Added KCAN, PORMIN, RWUEP1, RWUMX to output list.
C  12/03/2001 GH  Changed name to CROPGRO to reflect template approach.
C  02/03/2003 CHP EORATIO and KEP now exported for ET routines.
C  06/19/2003 CHP Added KTRANS for export to transpiration routines instead of KEP.
C  07/08/2003 CHP Added KSEVAP for export to soil evaporation routines.
!  12/16/2004 KJB, LAH, CHP Revise harvest variables, call to HRes_CRGRO
!  02/21/2005 SJR Moved ISWWAT condition to ROOTS routine to allow computation
!                 of root senescence even when water not simulated.
!  10/24/2005 CHP Put weather variables in constructed variable. 
!  11/07/2005 CHP Replaced FAC with SOILPROP variable KG2PPM
!  06/06/2006 CHP/CDM Added KC_SLOPE to SPE file and KC_ECO to ECO file.
!  07/13/2006 CHP Added P model
!  06/11/2007 CHP PStres1 affects photosynthesis, PStres2 affects growth
C=======================================================================

      SUBROUTINE CROPGRO(CONTROL, ISWITCH, 
     &    EOP, HARVFRAC, NH4, NO3, SOILPROP, SPi_AVAIL,   !Input
     &    ST, SW, TRWUP, WEATHER, YREND, YRPLT,           !Input
     &    CANHT, EORATIO, HARVRES, KSEVAP, KTRANS, MDATE, !Output
     &    NSTRES, PSTRES1,                                !Output
     &    PUptake, PORMIN, RLV, RWUMX, SENESCE,           !Output
     &    STGDOY, FracRts, UNH4, UNO3, XHLAI, XLAI)       !Output

!-----------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
      USE ModuleData
	USE interface2
      IMPLICIT NONE
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 DETACH, IDETO, ISWNIT, ISWSYM,
     &    ISWWAT, ISWDIS, ISWPHO, MEPHO, RNMODE
      CHARACTER*2 CROP
      CHARACTER*6 ECONO
      CHARACTER*30 FILEIO
      CHARACTER*92 FILECC, FILEGC

      INTEGER DAS, DYNAMIC, RUN
      INTEGER NDLEAF, NDSET, NLAYR, NOUTDO,
     &    NR1, NR2, NR5, NR7, NVEG0
      INTEGER RSTAGE, YREND
      INTEGER YREMRG, YRDOY, YRNR1, YRNR2,
     &    YRNR3, YRNR5, YRNR7, MDATE, YRPLT
      INTEGER STGDOY(20)

      REAL AGEFAC, AGRSD3, AREALF, ASMDOT
      REAL AGRLF, AGRRT, AGRSH2, AGRSTM
      REAL AGRSD1, AGRSD2, AGRVG, AGRVG2
      REAL AGRNOD, AGRSH1
      REAL BETN
      REAL CANNAA, CANWAA, CDMREP, CGRSH, CGRSD, CNODMN, CO2, CSAVEV
      REAL CNDFX, CTONODR, CTONOD, CAVVEG
      REAL CADPR1, CMOBMX, CMINEP
      REAL CLW, CSW
      REAL CNOD, CRUSLF, CRUSRT, CRUSST,
     &    CRUSSH, CADLF, CADST, CANHT, CANWH, CMINEA
      REAL DAYL, DWNOD
      REAL DWNODA, DISLA, DRPP, DTX, DXR57
      REAL EOP, EP1, EXCESS
      REAL FNINSH, FRACDN, FRCNOD,
     &    F, FNINL, FNINR, FNINS, FNINSD,
     &    FRLF, FRRT, FRSTM
      REAL FREEZ1, FREEZ2
      REAL GDMSD, GRRAT1, GROWTH, GRWRES
      REAL HARVFRAC(2)
      REAL LAIMX, LAGSD, LNGPEG
      REAL MAINR
      REAL NSTRES, NPLTD, NDMNEW,
     &    NADRT,  NADST,  NGRLF,  NGRRT,  NGRST,
     &    NADLF, NMINEA,
     &    NDMOLD, NDMREP, NDMSDR, NDMTOT, NDMVEG,
     &    NMINEP, NMOBR, NDTH, NFIXN, NODGR
      REAL NAVL, NRUSSH, NGRSD, NGRSH
      REAL NRUSLF, NRUSRT, NRUSST
      REAL POTLIP, POTCAR, PPLTD,
     &    PAR, PCNL, PCNRT, PCNST, PCTMAT
      REAL PLIGLF, PLIGNO, PLIGRT, PLIGST
      REAL PODWTD, PG, PCLSD, PCCSD, PCNSD, PCNSH, PODWT
      REAL PODNO
      REAL PGAVL, PLTPOP, PCARSH, PCH2O, PLIPSH,
     &    PLIGSD, PLIGSH, PMINSD, PMINSH, POASD, POASH,
     &    PROLFI, PRORTI, PROSHI, PROSTI
      REAL R30C2, RCH2O, RES30C, RPROAV,
     &    RFIXN, RLIG, RLIP, RMIN, RNITP,
     &    RNH4C, RNO3C, ROA, RPRO
      REAL RHOL, RO, RP, RTDEP, RHOS, ROWSPC
      REAL RTWT, RVSTGE, RSPNO3, RSPNH4
      REAL SEEDNI, NAVLV, PROVEG, SDNPL

!     Senescence variables computed for soil N routines.
      REAL SENNOD(NL), SENRT(NL)

      REAL SATFAC, SWFAC, SDWTAH,
     &    SDRATE, SDWT, SDIDOT,
     &    SDVAR, SHVAR, SDGR, SDPROR, SHELWT,
     &    SLA, SLDOT, STMWT, SWIDOT, SEEDNO
      REAL SLPF
      REAL SRDOT, SLAAD, SLNDOT, SSDOT, SSNDOT
      REAL TDAY, TDUMX, TDUMX2, TGROAV, TMIN, TURFAC, TAVG, TURADD,
     &    TRNH4U, TRNO3U, TRNU, TNLEAK, TRWUP, TTFIX, TOPWT, TOTWT
      REAL VSTAGE
      REAL WLFDOT, WSIDOT, WRIDOT
      REAL WTNCAN, WTNFX, WTNLA, WTNLO, WTNNA, WTNNAG
      REAL WTNNO, WTNNOD, WTNRA, WTNRO, WTNSA, WTNSDA
      REAL WTNSDO, WTNSHA, WTNSHO, WTNSO, WTNUP, WTNOO
      REAL WTLO, WTSO, WTRO, WSDDTN, WSHDTN, WTABRT, WTSHMT
      REAL WTCO, WTSHO, WTSDO, WCRLF, WCRRT,
     &    WCRSH, WLDOTN, WRDOTN,  WSDOTN,
     &    WCRST, WNRLF, WNRRT, WNRSH, WNRST,
     &    WTNRT, WTNSD, WTNSH, WTNST, WTNEW, WTMAIN, WTNLF,
     &    WTLF, WSHIDT, WLIDOT
      REAL XPOD, XFRT, XHLAI, XLAI

      REAL DLAYR(NL), DS(NL), DUL(NL), KG2PPM(NL), LL(NL), 
     &    SAT(NL), SW(NL), ST(NL), RLV(NL), WR(NL)
      REAL NH4(NL), NO3(NL), UNH4(NL), UNO3(NL)
      REAL PHTHRS(20)
      REAL TGRO(TS)
      REAL SDDES(NCOHORTS)
      REAL WTSD(NCOHORTS), WTSHE(NCOHORTS), SDNO(NCOHORTS) 
      REAL SHELN(NCOHORTS)
      REAL PHTIM(NCOHORTS)
      REAL PNTIM(NCOHORTS)

      REAL FLWN(NCOHORTS)

!CHP - puncture variables, not functional
      REAL PUNCSD, PUNCTR

!     Species-dependant variables exported to SPAM module:
      REAL EORATIO, KEP, KSEVAP, KTRANS
      REAL PORMIN, RWUMX, RWUEP1
      REAL KCAN, KC_SLOPE

!     P model
      REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed
      REAL PStres1, PStres2, SeedFrac, VegFrac
      REAL, DIMENSION(NL) :: PUptake, SPi_AVAIL, FracRts
      REAL PCNVEG
      REAL ShutMob, RootMob, ShelMob

!     K model (not yet implemented)
      REAL KSTRES

!-----------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SoilType) SOILPROP
      TYPE (SwitchType) ISWITCH
      Type (ResidueType) HARVRES
      Type (ResidueType) SENESCE
      Type (WeatherType) WEATHER
      Type(Plant_Growth) PlantF
!     Transfer values from constructed data types into local variables.
      CROP    = CONTROL % CROP
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      YRDOY   = CONTROL % YRDOY
      DAS     = CONTROL % DAS
      RUN     = CONTROL % RUN
      RNMODE  = CONTROL % RNMODE

      DLAYR  = SOILPROP % DLAYR  
      DS     = SOILPROP % DS     
      DUL    = SOILPROP % DUL    
      KG2PPM = SOILPROP % KG2PPM  
      LL     = SOILPROP % LL     
      NLAYR  = SOILPROP % NLAYR  
      SAT    = SOILPROP % SAT    
      SLPF   = SOILPROP % SLPF  
!	write(*,*)'SLPF',SLPF
!
      WR     = SOILPROP % WR     

      IDETO  = ISWITCH % IDETO
      ISWDIS = ISWITCH % ISWDIS
      ISWNIT = ISWITCH % ISWNIT
      ISWSYM = ISWITCH % ISWSYM
      ISWPHO = ISWITCH % ISWPHO
      ISWWAT = ISWITCH % ISWWAT
      MEPHO  = ISWITCH % MEPHO

      CO2    = WEATHER % CO2   
      DAYL   = WEATHER % DAYL  
      PAR    = WEATHER % PAR   
      TAVG   = WEATHER % TAVG  
      TDAY   = WEATHER % TDAY  
      TGRO   = WEATHER % TGRO  
      TGROAV = WEATHER % TGROAV
      TMIN   = WEATHER % TMIN  
  !    write(*,*)'CROPGROW,DYNAMIC=',DYNAMIC,'ihru',interface_ihru!
!	write(*,*)'NH4=',NH4,'NO3=',NO3 
!	write(*,*)'..............NUTRIENTS at begin...................'   	 
!***********************************************************************
!***********************************************************************
!     Run Initialization - Called once per simulation
!***********************************************************************
      IF (DYNAMIC .EQ. RUNINIT) THEN
!-----------------------------------------------------------------------
!     Call input routine for CROPGRO module parameters
!-----------------------------------------------------------------------
!	IF(useSWAT)THEN
!		call interface_ipplnt
!	ELSE

      CALL IPPLNT(CONTROL, ISWITCH, 
     &  CADPR1, CMOBMX, CROP, DETACH, ECONO,              !Output
     &  EORATIO, FILECC, FILEGC, FRCNOD, FREEZ1, FREEZ2,  !Output
     &  KCAN, KC_SLOPE, KEP, NOUTDO, PCARSH, PCH2O,       !Output
     &  PLIPSH, PLIGSD, PLIGSH, PMINSD, PMINSH, POASD,    !Output
     &  POASH, PORMIN, PROLFI, PRORTI, PROSHI, PROSTI,    !Output
     &  R30C2, RCH2O, RES30C, RFIXN, RLIG, RLIP, RMIN,    !Output
     &  RNH4C, RNO3C, ROA, RPRO, RWUEP1, RWUMX, TTFIX)    !Output
!	END IF
	IF(PCH2O==0)THEN
	write(*,*)'PCH2O=0, STOP,CROPGROW'
	STOP
	END IF
!	write(*,*)'FROM IPPLNT:',FILECC
      KTRANS = KEP
      KSEVAP = -99.   !Defaults to old method of light
                      !  extinction calculation for soil evap.

      IF (CROP .NE. 'FA' .AND. MEPHO .EQ. 'C') THEN
        CALL PHOTO(CONTROL, 
     &    BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE,       !Input
     &    NR5, PAR, PStres1, SLPF, RNITP, SLAAD,          !Input
     &    SWFAC, TDAY, XHLAI, XPOD,                       !Input
     &    AGEFAC, PG)                                     !Output
      ENDIF
	CALL GET(PlantF)
	PlantF%AGEFAC=AGEFAC
	PlantF%PG=PG
!-----------------------------------------------------------------------
		NSTRES=PlantF%NSTRES
		PStres2=PlantF%PStres2
		XPOD=PlantF%XPOD

	CALL PUT(PlantF)
        
      CALL PHENOL(CONTROL, ISWITCH, 
     &    DAYL, NSTRES, PStres2, SOILPROP, ST,            !Input
     &    SW, SWFAC, TGRO, TMIN, TURFAC, XPOD, YRPLT,     !Input
     &    DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        !Output
     &    NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       !Output
     &    RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,        !Output
     &    TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1,         !Output
     &    YRNR2, YRNR3, YRNR5, YRNR7)                     !Output
	CALL GET(PlantF)
		PlantF%DRPP=DRPP
		PlantF%DTX=DTX
		PlantF%DXR57=DXR57
		PlantF%FRACDN=FRACDN
		PlantF%MDATE=MDATE
		PlantF%NDLEAF=NDLEAF
		PlantF%NDSET=NDSET
		PlantF%NR1=NR1
		PlantF%NR2=NR2
		PlantF%NR5=NR5
		PlantF%NR7=NR7
		!write(*,*)'RUNINIT,interface_ihru=',interface_ihru,'NVEG0=',NVEG0
		PlantF%NVEG0=NVEG0
		PlantF%PHTHRS=PHTHRS
		PlantF%RSTAGE=RSTAGE
		PlantF%RVSTGE=RVSTGE
		PlantF%STGDOY=STGDOY
		PlantF%SeedFrac=SeedFrac
		PlantF%TDUMX=TDUMX
		PlantF%TDUMX2=TDUMX2
		PlantF%VegFrac=VegFrac
		PlantF%VSTAGE=VSTAGE
		PlantF%YREMRG=YREMRG
		PlantF%YRNR1=YRNR1
		PlantF%YRNR2=YRNR2
		PlantF%YRNR5=YRNR5
		PlantF%YRNR7=YRNR7
		CALL PUT(PlantF)
!	write(*,*)'YREMRG',YREMRG
!	

!-----------------------------------------------------------------------
      IF (ISWDIS .EQ. 'Y') THEN
	CALL GET(PlantF)
		AREALF=PlantF%AREALF
		CLW=PlantF%CLW
		CSW=PlantF%CSW
		LAGSD=PlantF%LAGSD
		LNGPEG=PlantF%LNGPEG
		NR2=PlantF%NR2
		PGAVL=PlantF%PGAVL
		PHTIM=PlantF%PHTIM
		PLTPOP=PlantF%PLTPOP
		RTWT=PlantF%RTWT
		SLA=PlantF%SLA
		SLDOT=PlantF%SLDOT
		SSDOT=PlantF%SSDOT
		STMWT=PlantF%STMWT
		TOPWT=PlantF%TOPWT
		WLFDOT=PlantF%WLFDOT
		WTLF=PlantF%WTLF
		RLV=PlantF%RLV
		SDNO=PlantF%SDNO
		SHELN=PlantF%SHELN
		SWIDOT=PlantF%SWIDOT
		VSTAGE=PlantF%VSTAGE
		WSHIDT=PlantF%WSHIDT
		WTSD=PlantF%WTSD
		WTSHE=PlantF%WTSHE	
        CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)                  !Output
	CALL GET(PlantF)
	        PlantF%PGAVL=PGAVL
                PlantF%RLV=RLV
                PlantF%SDNO=SDNO
                PlantF%SHELN=SHELN
                PlantF%SWIDOT=SWIDOT
                PlantF%VSTAGE=VSTAGE
                PlantF%WSHIDT=WSHIDT
                PlantF%WTSD=WTSD
                PlantF%WTSHE=WTSHE
                PlantF%ASMDOT=ASMDOT
                PlantF%DISLA=DISLA
                PlantF%NPLTD=NPLTD
                PlantF%PPLTD=PPLTD
                PlantF%SDDES=SDDES
                PlantF%WLIDOT=WLIDOT
                PlantF%WRIDOT=WRIDOT
                PlantF%WSIDOT=WSIDOT
                PlantF%SDWT=SDWT
	CALL PUT(PlantF)
      ENDIF
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
		CALL GET(PlantF)
	  	AGRLF=PlantF%AGRLF
		AGRRT=PlantF%AGRRT
		AGRSH2=PlantF%AGRSH2
		AGRSTM=PlantF%AGRSTM
		DRPP=PlantF%DRPP
		DXR57=PlantF%DXR57
		FNINSH=PlantF%FNINSH
		FRACDN=PlantF%FRACDN
		LAGSD=PlantF%FLAGSD
		LNGPEG=PlantF%LNGPEG
		NDLEAF=PlantF%NDLEAF
		NSTRES=PlantF%NSTRES
		PCNL=PlantF%PCNL
		PCNRT=PlantF%PCNRT
		PCNST=PlantF%PCNST
		PGAVL=PlantF%PGAVL
		PUNCSD=PlantF%PUNCSD
		PUNCTR=PlantF%PUNCTR
		PLTPOP=PlantF%PLTPOP
		RPROAV=PlantF%RPROAV
		RTWT=PlantF%RTWT
		SDDES=PlantF%SDDES
		SDNO=PlantF%SDNO
		SDVAR=PlantF%SDVAR
		SHELN=PlantF%SHELN
		SHVAR=PlantF%SHVAR
		STMWT=PlantF%STMWT
		SWFAC=PlantF%SWFAC
		TDUMX=PlantF%TDUMX
		TDUMX2=PlantF%TDUMX2
		TURFAC=PlantF%TURFAC
		VSTAGE=PlantF%VSTAGE
		WCRLF=PlantF%WCRLF
		WCRRT=PlantF%WCRRT
		WCRST=PlantF%WCRST
		WNRLF=PlantF%WNRLF
		WNRRT=PlantF%WNRRT
		WNRSH=PlantF%WNRSH
		WNRST=PlantF%WNRST
		WTLF=PlantF%WTLF
		WTSD=PlantF%WTSD
		WTSHE=PlantF%WTSHE
		XPOD=PlantF%XPOD
		NVEG0=PlantF%NVEG0
		NR1=PlantF%NR1
		NR2=PlantF%NR2
		NR5=PlantF%NR5
		NR7=PlantF%NR7
        CALL DEMAND(RUNINIT, CONTROL,
     &  AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  !Input
     &  FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    !Input
     &  LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  !Input
     &  PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,      !Input
     &  SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,   !Input
     &  TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, !Input
     &  WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,   !Input
     &  WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7,     !Input
     &  AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, F, FNINL,  !Output
     &  FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   !Output
     &  GRRAT1, NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT,  !Output
     &  NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM, POTCAR,      !Output
     &  POTLIP, SDGR, TURADD, XFRT, YREND)                !Output
		CALL GET(PlantF)
		PlantF%NMOBR=NMOBR
		PlantF%PHTIM=PHTIM
		PlantF%PNTIM=PNTIM
		PlantF%POTCAR=POTCAR
		PlantF%POTLIP=POTLIP
		PlantF%SDGR=SDGR
		PlantF%TURADD=TURADD
		PlantF%XFRT=XFRT
		PlantF%YREND=YREND	
		PlantF%AGRSD1=AGRSD1
		PlantF%AGRSD2=AGRSD2
		PlantF%AGRVG=AGRVG
		PlantF%AGRVG2=AGRVG2
		PlantF%CDMREP=CDMREP
		PlantF%F=F
		PlantF%FNINL=FNINL
		PlantF%FNINR=FNINR
		PlantF%FNINS=FNINS
		PlantF%FNINSD=FNINSD
		PlantF%FRLF=FRLF
		PlantF%FRRT=FRRT
		PlantF%FRSTM=FRSTM
		PlantF%GDMSD=GDMSD
		PlantF%GRRAT1=GRRAT1
		PlantF%NDMNEW=NDMNEW
		PlantF%NDMOLD=NDMOLD
		PlantF%NDMREP=NDMREP
		PlantF%NDMSDR=NDMSDR
		PlantF%NDMTOT=NDMTOT
		PlantF%NDMVEG=NDMVEG
		PlantF%NMINEP=NMINEP
		CALL PUT(PlantF)
!-----------------------------------------------------------------------
C    Call plant COMPosition INitialization (for data input)
C-----------------------------------------------------------------------
	FRLF=PlantF%FRLF
	FRRT=PlantF%FRRT
	FRSTM=PlantF%FRSTM
        CALL INCOMP(RUNINIT, 
     &    FILECC, FILEIO, FRLF, FRRT, FRSTM,              !Input
     &    AGRLF, AGRNOD, AGRRT, AGRSD1, AGRSD2, AGRSH1,   !Output
     &    AGRSH2, AGRSTM, AGRVG, AGRVG2, SDPROR)          !Output
	CALL GET(PlantF)
	PlantF%AGRLF=AGRLF
	PlantF%AGRNOD=AGRNOD
	PlantF%AGRRT=AGRRT
	PlantF%AGRSD1=AGRSD1
	PlantF%AGRSD2=AGRSD2
	PlantF%AGRSH1=AGRSH1
	PlantF%AGRSH2=AGRSH2
	PlantF%AGRSTM=AGRSTM
	PlantF%AGRVG=AGRVG
	PlantF%AGRVG2=AGRVG2
	PlantF%SDPROR=SDPROR
	CALL PUT(PlantF)
	
!-----------------------------------------------------------------------
		NDMSDR=PlantF%NDMSDR	
		NDMTOT=PlantF%NDMTOT
		RLV=PlantF%RLV
        CALL NUPTAK(RUNINIT,
     &     DLAYR, DUL, FILECC, KG2PPM, LL, NDMSDR, NDMTOT,!Input
     &     NH4, NO3, NLAYR, RLV, SAT, SW,                 !Input
     &     TRNH4U, TRNO3U, TRNU, UNH4, UNO3)              !Output
		CALL GET(PlantF)
		PlantF%TRNH4U=TRNH4U
		PlantF%TRNO3U=TRNO3U
		PlantF%TRNU=TRNU
		PlantF%UNH4=UNH4
		PlantF%UNO3=UNO3
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
        IF (ISWSYM .EQ. 'Y') THEN
		AGRNOD=PlantF%AGRNOD
		CNODMN=PlantF%CNODMN
		CTONOD=PlantF%CTONOD
		DXR57=PlantF%DXR57
		NR7=PlantF%NR7
		PLTPOP=PlantF%PLTPOP
		TURFAC=PlantF%TURFAC
          CALL NFIX(RUNINIT,
     &      AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,         !Input
     &      FILECC, FILEIO, NLAYR, NR7, PLTPOP,           !Input
     &      SAT, ST, SW, TURFAC,                          !Input
     &      CNOD, DWNOD, DWNODA, NDTH, NFIXN,             !Output
     &      NODGR, WTNFX, SENNOD)                         !Output
	 	CALL GET(PlantF)
		PlantF%CNOD=CNOD
		PlantF%DWNOD=DWNOD	
		PlantF%DWNODA=DWNODA
		PlantF%NDTH=NDTH	
		PlantF%NFIXN=NFIXN
		PlantF%NODGR=NODGR
		PlantF%WTNFX=WTNFX
		PlantF%SENNOD=SENNOD
	 	CALL PUT(PlantF)
        ENDIF

!-----------------------------------------------------------------------
		AGRSH1=PlantF%AGRSH1
		FNINSH=PlantF%FNINSH
		NAVL=PlantF%NAVL
		NDSET=PlantF%NDSET
		NRUSSH=PlantF%NRUSSH
		PHTHRS=PlantF%PHTHRS
		RNITP=PlantF%RNITP
		SDDES=PlantF%SDDES
		SDGR=PlantF%SDGR
		XFRT=PlantF%XFRT
		YRNR1=PlantF%YRNR1
		YRNR2=PlantF%YRNR2
		PStres2=PlantF%PStres2
        CALL PODS(RUNINIT, 
     &    AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       !Input
     &    FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    !Input
     &    GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, !Input
     &    NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    !Input
     &    PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  !Input
     &    TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, !Input
     &    PStres2, YRPLT,                                 !Input
     &    AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    !Output
     &    PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     !Output
     &    SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     !Output
     &    WTSHE, WTSHMT, FLWN)                            !Output
	CALL GET(PlantF)
		PlantF%AGRSD3=AGRSD3
		PlantF%LAGSD=LAGSD
		PlantF%LNGPEG=LNGPEG
		PlantF%NGRSD=NGRSD
		PlantF%NGRSH=NGRSH
		PlantF%PCTMAT=PCTMAT
		PlantF%PODNO=PODNO
		PlantF%POTCAR=POTCAR
		PlantF%POTLIP=POTLIP
		PlantF%SDNO=SDNO
		PlantF%SDVAR=SDVAR
		PlantF%SEEDNO=SEEDNO
		PlantF%SHELN=SHELN
		PlantF%SHVAR=SHVAR
		PlantF%WSDDTN=WSDDTN
		PlantF%WSHDTN=WSHDTN
		PlantF%WTABRT=WTABRT
		PlantF%WTSD=WTSD
		PlantF%WTSHE=WTSHE
		PlantF%WTSHMT=WTSHMT
		PLantF%FLWN=FLWN
	CAll PUT(PlantF)
!-----------------------------------------------------------------------
        IF (DETACH .EQ. 'Y') THEN
		WTLF=PlantF%WTLF
		YRNR2=PlantF%YRNR2
	

          CALL PODDET(
     &      FILECC, TGRO, WTLF, YRDOY, YRNR2,             !Input
     &      PODWTD, SDNO, SHELN, SWIDOT,                  !Output
     &      WSHIDT, WTSD, WTSHE,                          !Output
     &      RUNINIT)                                      !Control
		CALL GET(PlantF)
		PlantF%SDNO=SDNO
		PlantF%SHELN=SHELN
		PlantF%SWIDOT=SWIDOT
		PlantF%PODWTD=PODWTD
		PlantF%WSHIDT=WSHIDT
			PlantF%WTSD=WTSD
			PlantF%WTSHE=WTSHE
	CALL PUT(PlantF)
			
        ENDIF

!-----------------------------------------------------------------------
		CMINEP=PlantF%CMINEP
		CSAVEV=PlantF%CSAVEV
		DTX=PlantF%DTX
		FNINR=PlantF%FNINR
		FNINS=PlantF%FNINS
		KCAN=PlantF%KCAN
		NAVL=PlantF%NAVL
		NDMNEW=PlantF%NDMNEW
		NDMOLD=PlantF%NDMOLD
		NFIXN=PlantF%NFIXN
		NMINEA=PlantF%NMINEA
		PCH2O=PlantF%PCH2O
		PG=PlantF%PG
		PStres2=PlantF%PStres2
		ROWSPC=PlantF%ROWSPC
		RVSTGE=PlantF%RVSTGE
		STMWT=PlantF%STMWT
		TRNU=PlantF%TRNU
		TURFAC=PlantF%TURFAC
		VSTAGE=PlantF%VSTAGE
		WCRLF=PlantF%WCRLF
		WCRRT=PlantF%WCRRT
		WCRSH=PlantF%WCRSH
		WCRST=PlantF%WCRST
		WTLF=PlantF%WTLF
		XLAI=PlantF%XLAI	
		YREMRG=PlantF%YREMRG
		AGRVG=PlantF%AGRVG
		FRLF=PlantF%FRLF
		FRRT=PlantF%FRRT
		FRSTM=PlantF%FRSTM
		AGRLF=PlantF%AGRLF
!	write(*,*)'BEFORE PROSTI=',PROSTI	
        CALL VEGGR (RUNINIT, 
     &    AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,      !Input
     &    DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,     !Input
     &    FNINS, KCAN, NAVL, NDMNEW, NDMOLD,              !Input
     &    NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,      !Input
     &    PStres2, ROWSPC, RVSTGE, STMWT, TGRO,           !Input
     &    TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,      !Input
     &    WCRST, WTLF, XLAI, YRDOY, YREMRG,               !Input
     &    AGRVG, FRLF, FRRT, FRSTM,                       !I/O
     &    CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,     !Output
     &    CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,   !Output
     &    NADST, NGRLF, NGRRT, NGRST, NSTRES,             !Output
     &    TNLEAK, WLDOTN, WRDOTN, WSDOTN)                 !Output
!	write(*,*)'AFTER PROSTI=',PROSTI
	CALL GET(PlantF)
		PlantF%AGRVG=AGRVG
		PlantF%FRLF=FRLF
		PlantF%FRRT=FRRT
		PlantF%FRSTM=FRSTM
		PlantF%CADLF=CADLF
		PlantF%CADST=CADST
		PlantF%CANHT=CANHT
		PlantF%CANWH=CANWH
		PlantF%CMINEA=CMINEA
		PlantF%CRUSLF=CRUSLF
		PlantF%CRUSRT=CRUSRT
		PlantF%CRUSSH=CRUSSH
		PlantF%CRUSST=CRUSST
		PlantF%EXCESS=EXCESS
		PlantF%NADLF=NADLF
		PlantF%NADRT=NADRT
		PlantF%NADST=NADST
		PlantF%NGRLF=NGRLF
		PlantF%NGRRT=NGRRT
		PlantF%NGRST=NGRST
		PlantF%NSTRES=NSTRES
		PlantF%TNLEAK=TNLEAK
		PlantF%WLDOTN=WLDOTN
		PlantF%WRDOTN=WRDOTN
		PlantF%WSDOTN=WSDOTN
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
C     Call leaf senescence routine for initialization
C-----------------------------------------------------------------------
		CLW=PlantF%CLW
		DTX=PlantF%DTX
		KCAN=PlantF%KCAN
		NR7=PlantF%NR7
		NRUSLF=PlantF%NRUSLF
		RHOL=PlantF%RHOL
		SLAAD=PlantF%SLAAD
		STMWT=PlantF%STMWT
		SWFAC=PlantF%SWFAC
		VSTAGE=PlantF%VSTAGE
			WTLF=PlantF%WTLF
			XLAI=PlantF%XLAI

        CALL SENES(RUNINIT, 
     &    FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,       !Input
     &    RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI,  !Input
     &    SLDOT, SLNDOT, SSDOT, SSNDOT)                   !Output
	CALL GET(PlantF)
	PlantF%SLDOT=SLDOT
	PlantF%SLNDOT=SLNDOT
	PlantF%SSDOT=SSDOT
	PlantF%SSNDOT=SSNDOT
!	write(*,*)'PROSTI=',PlantF%PROSTI
	CALL PUT(PlantF)
C-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
C-----------------------------------------------------------------------
	AGRRT=PlantF%AGRRT
	DTX=PlantF%DTX
	FRRT=PlantF%FRRT
	PG=PlantF%PG
	PLTPOP=PlantF%PLTPOP
	RO=PlantF%RO
	RP=PlantF%RP
	RTWT=PlantF%RTWT
	SWFAC=PlantF%SWFAC
	VSTAGE=PlantF%VSTAGE
	WRDOTN=PlantF%WRDOTN
	WTNEW=PlantF%WTNEW
        CALL ROOTS(RUNINIT,
     &    AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, !Input
     &    ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    !Input
     &    SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      !Input
     &    RLV, RTDEP, SATFAC, SENRT, SRDOT)               !Output
	CALL GET(PlantF)
	PlantF%RLV=RLV
	PlantF%RTDEP=RTDEP
	PlantF%SATFAC=SATFAC
	PlantF%SENRT=SENRT
	PlantF%SRDOT=SRDOT
	CALL PUT(PlantF)	
        ENDIF

!-----------------------------------------------------------------------
!	write(*,*)'CROPGRO, RUNINIT: To GROW, FRLF=',FRLF
		AGEFAC=PlantF%AGEFAC
		CADLF=PlantF%CADLF
		CADST=PlantF%CADST
		CRUSLF=PlantF%CRUSLF
		CRUSRT=PlantF%CRUSRT
		CRUSSH=PlantF%CRUSSH
		CRUSST=PlantF%CRUSST
		DISLA=PlantF%DISLA
		F=PlantF%F
		FRLF=PlantF%FRLF
		FRSTM=PlantF%FRSTM
		NADLF=PlantF%NADLF
		NADRT=PlantF%NADRT
		NADST=PlantF%NADST
		NDTH=PlantF%NDTH
		NFIXN=PlantF%NFIXN
		NGRLF=PlantF%NGRLF
		NGRRT=PlantF%NGRRT
		NGRSD=PlantF%NGRSD
		NGRSH=PlantF%NGRSH
		NGRST=PlantF%NGRST
		NMINEA=PlantF%NMINEA
		NODGR=PlantF%NODGR
		NOUTDO=PlantF%NOUTDO
		NPLTD=PlantF%NPLTD
		NRUSLF=PlantF%NRUSLF
		NRUSRT=PlantF%NRUSRT
		NRUSSH=PlantF%NRUSSH
		NRUSST=PlantF%NRUSST
		POTCAR=PlantF%POTCAR
		POTLIP=PlantF%POTLIP
		PPLTD=PlantF%PPLTD
		SDIDOT=PlantF%SDIDOT
		SDPROR=PlantF%SDPROR
		SENNOD=PlantF%SENNOD
		SENRT=PlantF%SENRT
		SLDOT=PlantF%SLDOT
		SLNDOT=PlantF%SLNDOT
		SRDOT=PlantF%SRDOT
		SSDOT=PlantF%SSDOT
		SSNDOT=PlantF%SSNDOT
		TRNH4U=PlantF%TRNH4U
		TRNO3U=PlantF%TRNO3U
		TRNU=PlantF%TRNU
		TURFAC=PlantF%TURFAC
		WLDOTN=PlantF%WLDOTN
		WLIDOT=PlantF%WLIDOT
		WRDOTN=PlantF%WRDOTN
		WRIDOT=PlantF%WRIDOT
		WSDDTN=PlantF%WSDDTN
		WSDOTN=PlantF%WSDOTN
		WSHDTN=PlantF%WSHDTN
		WSIDOT=PlantF%WSIDOT
		WTABRT=PlantF%WTABRT
		WTSHMT=PlantF%WTSHMT
		YRNR1=PlantF%YRNR1
		MDATE=PlantF%MDATE
		!YRPLT=PlantF%YRPLT
		SWIDOT=PlantF%SWIDOT
		WLFDOT=PlantF%WLFDOT
		WSHIDT=PlantF%WSHIDT
		WTNFX=PlantF%WTNFX
		XHLAI=PlantF%XHLAI	

!	write(*,*)'CROPGRO:To GROW_DSSAT ,FRLF=',FRLF
      CALL GROW_DSSAT(CONTROL, ISWITCH, RUNINIT, SOILPROP, 
     &  AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     !Input
     &  CRUSST, DISLA, F, FILECC, FRLF, FRSTM,            !Input
     &  NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   !Input
     &  NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       !Input
     &  NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,            !Input
     &  POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            !Input
     &  SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       !Input
     &  SSNDOT, TRNH4U, TRNO3U, TRNU,                     !Input
     &  TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   !Input
     &  WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    !Input
     &  MDATE, YRPLT,                                     !Input
     &  SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             !Input/Output
     &  AREALF, BETN, CANNAA, CANWAA, CLW, CSW, DWNOD,    !Output
     &  DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, PCLSD,      !Output
     &  PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,         !Output
     &  PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   !Output
     &  PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         !Output
     &  ROWSPC, RTWT, SDNPL, SDRATE, SDWT,                !Output
     &  SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             !Output
     &  SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  !Output
     &  WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          !Output
     &  WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  !Output
     &  WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       !Output
     &  WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        !Output
     &  WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      !Output
     &  WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      !Output
     &  ShutMob, RootMob, ShelMob)                        !Output
!	write(*,*)'CROPGROW,SDNPL=',SDNPL
	!STOP
	CALL GET(PlantF)
	 	PlantF%WTSHO=WTSHO
		PlantF%WTNSO=WTNSO
		PlantF%WTNST=WTNST
		PlantF%WTNUP=WTNUP
		PlantF%WTRO=WTRO
		PlantF%WTSDO=WTSDO
		PlantF%WTSHO=WTSHO
		PlantF%WTSO=WTSO
		PlantF%XLAI=XLAI
		PlantF%XPOD=XPOD
		PlantF%ShutMob=ShutMob
		PlantF%RootMob=RootMob
		PlantF%ShelMob=ShelMob

		PlantF%WTNNO=WTNNO
        PlantF%WTNNOD=WTNNOD
        PlantF%WTNOO=WTNOO
        PlantF%WTNRA=WTNRA
        PlantF%WTNRO=WTNRO
        PlantF%WTNRT=WTNRT
        PlantF%WTNSA=WTNSA
        PlantF%WTNSD=WTNSD
        PlantF%WTNSDA=WTNSDA
        PlantF%WTNSDO=WTNSDO
        PlantF%WTNSH=WTNSH
        PlantF%WTNSHA=WTNSHA
        PlantF%WTNSHO=WTNSHO
        PlantF%WTNSO=WTNSO
        PlantF%WTNST=WTNST
        PlantF%WTNUP=WTNUP
        PlantF%WTRO=WTRO
        PlantF%WTSDO=WTSDO

	
		PlantF%WCRRT=WCRRT
		PlantF%WCRSH=WCRSH
		PlantF%WCRST=WCRST
		PlantF%WNRLF=WNRLF
		PlantF%WNRRT=WNRRT
		PlantF%WNRSH=WNRSH
		PlantF%WNRST=WNRST
		PlantF%WTCO=WTCO
		PlantF%WTLF=WTLF
		PlantF%WTLO=WTLO
		PlantF%WTMAIN=WTMAIN
		PlantF%WTNCAN=WTNCAN
		PlantF%WTNEW=WTNEW
		PlantF%WTNLA=WTNLA
		PlantF%WTNLF=WTNLF
		PlantF%WTNLO=WTNLO
		PlantF%WTNNA=WTNNA
		PlantF%WTNNAG=WTNNAG
		PlantF%ROWSPC=ROWSPC
		PlantF%RTWT=RTWT
		PlantF%SDNPL=SDNPL
		PlantF%SDRATE=SDRATE
		PlantF%SDWT=SDWT
		PlantF%SEEDNI=SEEDNI
		PlantF%SEEDNO=SEEDNO
		PlantF%SENESCE=SENESCE
		PlantF%SHELWT=SHELWT
		PlantF%SLA=SLA
		PlantF%SLAAD=SLAAD
		PlantF%STMWT=STMWT
		PlantF%TOPWT=TOPWT
		PlantF%TOTWT=TOTWT
		PlantF%WCRLF=WCRLF



		PlantF%PODWT=PODWT
		PlantF%PUNCSD=PUNCSD
		PlantF%PUNCTR=PUNCTR
		PlantF%RHOL=RHOL
		PlantF%RHOS=RHOS
		PlantF%RNITP=RNITP
		PlantF%PCNSH=PCNSH
		PlantF%PCNST=PCNST
		PlantF%PLTPOP=PLTPOP
		PlantF%PLIGLF=PLIGLF
		PlantF%PLIGNO=PLIGNO
		PlantF%PLIGRT=PLIGRT
		PlantF%PLIGSD=PLIGSD
		PlantF%PLIGSH=PLIGSH
		PlantF%PLIGST=PLIGST

		PlantF%DWNODA=DWNODA
		PlantF%GROWTH=GROWTH
		PlantF%GRWRES=GRWRES
		PlantF%LAIMX=LAIMX	
		PlantF%PCCSD=PCCSD
		PlantF%PCLSD=PCLSD	
		PlantF%PCNL=PCNL
		PlantF%PCNRT=PCNRT
		PlantF%PCNSD=PCNSD
	
		PlantF%SWIDOT=SWIDOT
		PlantF%WLFDOT=WLFDOT
		PlantF%WSHIDT=WSHIDT
		PlantF%WTNFX=WTNFX
		PlantF%XHLAI=XHLAI
		PlantF%AREALF=AREALF
		PlantF%BETN=BETN
		PlantF%CANNAA=CANNAA
		PlantF%CANWAA=CANWAA
		PlantF%CLW=CLW
		PlantF%CSW=CSW
		PlantF%DWNOD=DWNOD
	CALL PUT(PlantF)
!	WRITE(*,*)'CROPGROW:From GROW'
      CALL OPGROW(CONTROL, ISWITCH, SoilProp, 
     &    CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD, GROWTH,  
     &    GRWRES, KSTRES, MAINR, MDATE, NFIXN, NLAYR, NSTRES, 
     &    PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG, 
     &    PODNO, PODWT, PODWTD, PSTRES1, PSTRES2, RHOL, RHOS, 
     &    RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDWT, SEEDNO, 
     &    SENESCE, SLA, STMWT, SWFAC, TGRO, TGROAV, TOPWT, 
     &    TOTWT, TURFAC, VSTAGE, WTLF, WTNCAN, WTNLF, WTNST, 
     &    WTNSD, WTNUP, WTNFX, XLAI, YRPLT) 

!     Initialize Overview.out file.
      CALL OPHARV(CONTROL, ISWITCH, 
     &    AGEFAC, CANHT, CANNAA, CANWAA, CROP,            !Input
     &    HARVFRAC, LAIMX, MDATE, NSTRES, PCLSD, PCNSD,   !Input
     &    PODNO, PODWT, PStres1, PStres2, SDRATE, SDWT,   !Input
     &    SEEDNO, STGDOY, SWFAC, TOPWT, TURFAC,           !Input
     &    VSTAGE, WTNCAN, WTNFX, WTNSD, WTNST, WTNUP,     !Input
     &    XLAI, RSTAGE, YREMRG, YRNR1, YRNR3, YRNR5,      !Input
     &    YRNR7, YRPLT,                                   !Input
     &    SDWTAH)                                         !Output

!     If this is not a sequenced run, don't use any previously calculated
!       harvest residue.
      IF (RUN .EQ. 1 .OR. INDEX('QF',RNMODE) .LE. 0) THEN
        HARVRES % RESWT  = 0.0
        HARVRES % RESLIG = 0.0
        HARVRES % RESE   = 0.0
      ENDIF

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. SEASINIT) THEN
!***********************************************************************
!     The following statements came from INPLNT
!-----------------------------------------------------------------------
      CMINEP = 0.0
      CNOD   = 0.0
      CNODMN = 0.0
      CTONOD = 0.0
      MAINR  = 0.0
      NAVL   = 0.0
      PGAVL  = 0.0
      RO     = 0.0
      RP     = 0.0
      RPROAV = RFIXN

      TURFAC = 1.0
      SWFAC  = 1.0

      RSPNO3 = 0.0
      RSPNH4 = 0.0

      KSTRES = 1.0
	CALL GET(PlantF)
	write(*,*)'BEGINNIN OF SEASINIT'
!	write(*,*)'interface_ihru=',interface_ihru
	PlantF%CMINEP=CMINEP
	PlantF%CNOD=CNOD
	PlantF%CNODMN=CNODMN
	PlantF%CTONOD=CTONOD
	PlantF%MAINR=MAINR
	PlantF%NAVL=NAVL
	PlantF%PGAVL=PGAVL
	PlantF%RO=RO
	PlantF%RP=RP
	PlantF%RPROAV=RPROAV
	PlantF%TURFAC=TURFAC
	PlantF%SWFAC=SWFAC
	PlantF%RSPNO3=RSPNO3
	PlantF%RSPNH4=RSPNH4
	CALL PUT(PlantF)	
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        IF (MEPHO .EQ. 'L') THEN
          !Retrieve AGEFAC and PG from ETPHOT routine.
          CALL GET('SPAM', 'AGEFAC', AGEFAC)
          CALL GET('SPAM', 'PG'    , PG)
        ELSEIF (MEPHO .EQ. 'C') THEN
          CALL PHOTO(CONTROL, 
     &    BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE,       !Input
     &    NR5, PAR, PStres1, SLPF, RNITP, SLAAD,          !Input
     &    SWFAC, TDAY, XHLAI, XPOD,                       !Input
     &    AGEFAC, PG)                                     !Output
        ENDIF
      ENDIF
	write(*,*)'CROP:TO PHENOL'
!-----------------------------------------------------------------------
	CALL GET(PlantF)
		NSTRES=PlantF%NSTRES
		PStres2=PlantF%PStres2
		XPOD=PlantF%XPOD
    
	CALL PHENOL(CONTROL, ISWITCH, 
     &    DAYL, NSTRES, PStres2, SOILPROP, ST,            !Input
     &    SW, SWFAC, TGRO, TMIN, TURFAC, XPOD, YRPLT,     !Input
     &    DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        !Output
     &    NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       !Output
     &    RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,        !Output
     &    TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1,         !Output
     &    YRNR2, YRNR3, YRNR5, YRNR7)                     !Output
	CALL GET(PlantF)
		PlantF%DRPP=DRPP
		PlantF%DTX=DTX
		PlantF%DXR57=DXR57
		PlantF%FRACDN=FRACDN
		PlantF%MDATE=MDATE
		PlantF%NDLEAF=NDLEAF
		PlantF%NDSET=NDSET
		PlantF%NR1=NR1
		PlantF%NR2=NR2
		PlantF%NR5=NR5
		PlantF%NR7=NR7
		

		!write(*,*)'SEASINIT, interface_ihru=',interface_ihru,
     		!& 'NVEG0=',NVEG0
		PlantF%NVEG0=NVEG0
		PlantF%PHTHRS=PHTHRS
		PlantF%RSTAGE=RSTAGE
		PlantF%RVSTGE=RVSTGE
		PlantF%STGDOY=STGDOY
		PlantF%SeedFrac=SeedFrac
		PlantF%TDUMX=TDUMX
		PlantF%TDUMX2=TDUMX2
		PlantF%VegFrac=VegFrac
		PlantF%VSTAGE=VSTAGE
		PlantF%YREMRG=YREMRG
		PlantF%YRNR1=YRNR1
		PlantF%YRNR2=YRNR2
		PlantF%YRNR5=YRNR5
		PlantF%YRNR7=YRNR7
	CALL PUT(PlantF)
	!	write(*,*)'EMERGENSE=',YREMRG,'NVEG0=',NVEG0
	
!-----------------------------------------------------------------------
C     Initialize pest coupling point and damage variables
!     Need to initialize even if pests are not being modelled this
!     season - zero out values from last simulation.
!-----------------------------------------------------------------------
                write(*,*)'CROP:To PEST'
		AREALF=PlantF%AREALF
		CLW=PlantF%CLW
		CSW=PlantF%CSW
		LAGSD=PlantF%LAGSD
		LNGPEG=PlantF%LNGPEG
		NR2=PlantF%NR2
		PGAVL=PlantF%PGAVL
		PHTIM=PlantF%PHTIM
		PLTPOP=PlantF%PLTPOP
		RTWT=PlantF%RTWT
		SLA=PlantF%SLA
		SLDOT=PlantF%SLDOT
		SSDOT=PlantF%SSDOT
		STMWT=PlantF%STMWT
		TOPWT=PlantF%TOPWT
		WLFDOT=PlantF%WLFDOT
		WTLF=PlantF%WTLF
		RLV=PlantF%RLV
		SDNO=PlantF%SDNO
		SHELN=PlantF%SHELN
		SWIDOT=PlantF%SWIDOT
		VSTAGE=PlantF%VSTAGE
		WSHIDT=PlantF%WSHIDT
		WTSD=PlantF%WTSD
		WTSHE=PlantF%WTSHE	
	  CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)                  !Output
	CALL GET(PlantF)
		PlantF%PGAVL=PGAVL
		PlantF%RLV=RLV
		PlantF%SDNO=SDNO
		PlantF%SHELN=SHELN
		PlantF%SWIDOT=SWIDOT
		PlantF%VSTAGE=VSTAGE
		PlantF%WSHIDT=WSHIDT
		PlantF%WTSD=WTSD
		PlantF%WTSHE=WTSHE
		PlantF%ASMDOT=ASMDOT
		PlantF%DISLA=DISLA
		PlantF%NPLTD=NPLTD
		PlantF%PPLTD=PPLTD
		PlantF%SDDES=SDDES
		PlantF%WLIDOT=WLIDOT
		PlantF%WRIDOT=WRIDOT
		PlantF%WSIDOT=WSIDOT
		PlantF%SDWT=SDWT
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
!     Initialization call to DEMAND must preceed initialization calls
!         to INCOMP and GROW (need to initialize values of F, FRLF,
!         FRRT, and FRSTM for use in those routines)  chp 9/22/98
!-----------------------------------------------------------------------
		AGRLF=PlantF%AGRLF
		AGRRT=PlantF%AGRRT
		AGRSH2=PlantF%AGRSH2
		AGRSTM=PlantF%AGRSTM
		DRPP=PlantF%DRPP
		DXR57=PlantF%DXR57
		FNINSH=PlantF%FNINSH
		FRACDN=PlantF%FRACDN
		LAGSD=PlantF%FLAGSD
		LNGPEG=PlantF%LNGPEG
		NDLEAF=PlantF%NDLEAF
		NSTRES=PlantF%NSTRES
		PCNL=PlantF%PCNL
		PCNRT=PlantF%PCNRT
		PCNST=PlantF%PCNST
		PGAVL=PlantF%PGAVL
		PUNCSD=PlantF%PUNCSD
		PUNCTR=PlantF%PUNCTR
		PLTPOP=PlantF%PLTPOP
		RPROAV=PlantF%RPROAV
		RTWT=PlantF%RTWT
		SDDES=PlantF%SDDES
		SDNO=PlantF%SDNO
		SDVAR=PlantF%SDVAR
		SHELN=PlantF%SHELN
		SHVAR=PlantF%SHVAR
		STMWT=PlantF%STMWT
		SWFAC=PlantF%SWFAC
		TDUMX=PlantF%TDUMX
		TDUMX2=PlantF%TDUMX2
		TURFAC=PlantF%TURFAC
		VSTAGE=PlantF%VSTAGE
		WCRLF=PlantF%WCRLF
		WCRRT=PlantF%WCRRT
		WCRST=PlantF%WCRST
		WNRLF=PlantF%WNRLF
		WNRRT=PlantF%WNRRT
		WNRSH=PlantF%WNRSH
		WNRST=PlantF%WNRST
		WTLF=PlantF%WTLF
		WTSD=PlantF%WTSD
		WTSHE=PlantF%WTSHE
		XPOD=PlantF%XPOD
		NVEG0=PlantF%NVEG0
		NR1=PlantF%NR1
		NR2=PlantF%NR2
		NR5=PlantF%NR5
		NR7=PlantF%NR7
 	write(*,*)'CROP:TO DEMAND' 
        CALL DEMAND(SEASINIT, CONTROL,
     &  AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  !Input
     &  FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    !Input
     &  LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  !Input
     &  PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,      !Input
     &  SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,   !Input
     &  TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, !Input
     &  WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,   !Input
     &  WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7,     !Input
     &  AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, F, FNINL,  !Output
     &  FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   !Output
     &  GRRAT1, NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT,  !Output
     &  NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM, POTCAR,      !Output
     &  POTLIP, SDGR, TURADD, XFRT, YREND)                !Output
	CALL GET(PlantF)
		PlantF%NMOBR=NMOBR
		PlantF%PHTIM=PHTIM
		PlantF%PNTIM=PNTIM
		PlantF%POTCAR=POTCAR
		PlantF%POTLIP=POTLIP
		PlantF%SDGR=SDGR
		PlantF%TURADD=TURADD
		PlantF%XFRT=XFRT
		PlantF%YREND=YREND	
		PlantF%AGRSD1=AGRSD1
		PlantF%AGRSD2=AGRSD2
		PlantF%AGRVG=AGRVG
		PlantF%AGRVG2=AGRVG2
		PlantF%CDMREP=CDMREP
		PlantF%F=F
		PlantF%FNINL=FNINL
		PlantF%FNINR=FNINR
		PlantF%FNINS=FNINS
		PlantF%FNINSD=FNINSD
		PlantF%FRLF=FRLF
		PlantF%FRRT=FRRT
		PlantF%FRSTM=FRSTM
		PlantF%GDMSD=GDMSD
		PlantF%GRRAT1=GRRAT1
		PlantF%NDMNEW=NDMNEW
		PlantF%NDMOLD=NDMOLD
		PlantF%NDMREP=NDMREP
		PlantF%NDMSDR=NDMSDR
		PlantF%NDMTOT=NDMTOT
		PlantF%NDMVEG=NDMVEG
		PlantF%NMINEP=NMINEP
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
!     Call plant COMPosition INitialization
!     This call must preceed initialization call to GROW (need to
!         initialize value of SDPROR for use in that routine) chp 9/22/98
!-----------------------------------------------------------------------
    	write(*,*)'CROP:INCOMP'
	  IF (CROP .NE. 'FA') THEN
	FRLF=PlantF%FRLF
	FRRT=PlantF%FRRT
	FRSTM=PlantF%FRSTM
        CALL INCOMP(SEASINIT, 
     &    FILECC, FILEIO, FRLF, FRRT, FRSTM,              !Input
     &    AGRLF, AGRNOD, AGRRT, AGRSD1, AGRSD2, AGRSH1,   !Output
     &    AGRSH2, AGRSTM, AGRVG, AGRVG2, SDPROR)          !Output
      ENDIF
	CALL GET(PlantF)
	PlantF%AGRLF=AGRLF
	PlantF%AGRNOD=AGRNOD
	PlantF%AGRRT=AGRRT
	PlantF%AGRSD1=AGRSD1
	PlantF%AGRSD2=AGRSD2
	PlantF%AGRSH1=AGRSH1
	PlantF%AGRSH2=AGRSH2
	PlantF%AGRSTM=AGRSTM
	PlantF%AGRVG=AGRVG
	PlantF%AGRVG2=AGRVG2
	PlantF%SDPROR=SDPROR
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
	write(*,*)'CROPGRO, SEASINIT:TO GROW, FRLF=',FRLF
		AGEFAC=PlantF%AGEFAC
		CADLF=PlantF%CADLF
		CADST=PlantF%CADST
		CRUSLF=PlantF%CRUSLF
		CRUSRT=PlantF%CRUSRT
		CRUSSH=PlantF%CRUSSH
		CRUSST=PlantF%CRUSST
		DISLA=PlantF%DISLA
		F=PlantF%F
		FRLF=PlantF%FRLF
		FRSTM=PlantF%FRSTM
		NADLF=PlantF%NADLF
		NADRT=PlantF%NADRT
		NADST=PlantF%NADST
		NDTH=PlantF%NDTH
		NFIXN=PlantF%NFIXN
		NGRLF=PlantF%NGRLF
		NGRRT=PlantF%NGRRT
		NGRSD=PlantF%NGRSD
		NGRSH=PlantF%NGRSH
		NGRST=PlantF%NGRST
		NMINEA=PlantF%NMINEA
		NODGR=PlantF%NODGR
		NOUTDO=PlantF%NOUTDO
		NPLTD=PlantF%NPLTD
		NRUSLF=PlantF%NRUSLF
		NRUSRT=PlantF%NRUSRT
		NRUSSH=PlantF%NRUSSH
		NRUSST=PlantF%NRUSST
		POTCAR=PlantF%POTCAR
		POTLIP=PlantF%POTLIP
		PPLTD=PlantF%PPLTD
		SDIDOT=PlantF%SDIDOT
		SDPROR=PlantF%SDPROR
		SENNOD=PlantF%SENNOD
		SENRT=PlantF%SENRT
		SLDOT=PlantF%SLDOT
		SLNDOT=PlantF%SLNDOT
		SRDOT=PlantF%SRDOT
		SSDOT=PlantF%SSDOT
		SSNDOT=PlantF%SSNDOT
		TRNH4U=PlantF%TRNH4U
		TRNO3U=PlantF%TRNO3U
		TRNU=PlantF%TRNU
		TURFAC=PlantF%TURFAC
		WLDOTN=PlantF%WLDOTN
		WLIDOT=PlantF%WLIDOT
		WRDOTN=PlantF%WRDOTN
		WRIDOT=PlantF%WRIDOT
		WSDDTN=PlantF%WSDDTN
		WSDOTN=PlantF%WSDOTN
		WSHDTN=PlantF%WSHDTN
		WSIDOT=PlantF%WSIDOT
		WTABRT=PlantF%WTABRT
		WTSHMT=PlantF%WTSHMT
		YRNR1=PlantF%YRNR1
		MDATE=PlantF%MDATE
		!YRPLT=PlantF%YRPLT
		SWIDOT=PlantF%SWIDOT
		WLFDOT=PlantF%WLFDOT
		WSHIDT=PlantF%WSHIDT
		WTNFX=PlantF%WTNFX
		XHLAI=PlantF%XHLAI	
      CALL GROW_DSSAT(CONTROL, ISWITCH, SEASINIT, SOILPROP, 
     &  AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     !Input
     &  CRUSST, DISLA, F, FILECC, FRLF, FRSTM,            !Input
     &  NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   !Input
     &  NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       !Input
     &  NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,            !Input
     &  POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            !Input
     &  SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       !Input
     &  SSNDOT, TRNH4U, TRNO3U, TRNU,                     !Input
     &  TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   !Input
     &  WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    !Input
     &  MDATE, YRPLT,                                     !Input
     &  SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             !Input/Output
     &  AREALF, BETN, CANNAA, CANWAA, CLW, CSW, DWNOD,    !Output
     &  DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, PCLSD,      !Output
     &  PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,         !Output
     &  PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   !Output
     &  PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         !Output
     &  ROWSPC, RTWT, SDNPL, SDRATE, SDWT,                !Output
     &  SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             !Output
     &  SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  !Output
     &  WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          !Output
     &  WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  !Output
     &  WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       !Output
     &  WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        !Output
     &  WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      !Output
     &  WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      !Output
     &  ShutMob, RootMob, ShelMob)                        !Output

!	write(*,*)'CROPGROW SEASINIT, SDNPL',SDNPL
	!STOP
	CALL GET(PlantF)
			PlantF%WTSHO=WTSHO
		PlantF%WTNSO=WTNSO
		PlantF%WTNST=WTNST
		PlantF%WTNUP=WTNUP
		PlantF%WTRO=WTRO
		PlantF%WTSDO=WTSDO
		PlantF%WTSHO=WTSHO
		PlantF%WTSO=WTSO
		PlantF%XLAI=XLAI
		PlantF%XPOD=XPOD
		PlantF%ShutMob=ShutMob
		PlantF%RootMob=RootMob
		PlantF%ShelMob=ShelMob

		PlantF%WTNNO=WTNNO
        PlantF%WTNNOD=WTNNOD
        PlantF%WTNOO=WTNOO
        PlantF%WTNRA=WTNRA
        PlantF%WTNRO=WTNRO
        PlantF%WTNRT=WTNRT
        PlantF%WTNSA=WTNSA
        PlantF%WTNSD=WTNSD
        PlantF%WTNSDA=WTNSDA
        PlantF%WTNSDO=WTNSDO
        PlantF%WTNSH=WTNSH
        PlantF%WTNSHA=WTNSHA
        PlantF%WTNSHO=WTNSHO
        PlantF%WTNSO=WTNSO
        PlantF%WTNST=WTNST
        PlantF%WTNUP=WTNUP
        PlantF%WTRO=WTRO
        PlantF%WTSDO=WTSDO


		PlantF%WCRRT=WCRRT
		PlantF%WCRSH=WCRSH
		PlantF%WCRST=WCRST
		PlantF%WNRLF=WNRLF
		PlantF%WNRRT=WNRRT
		PlantF%WNRSH=WNRSH
		PlantF%WNRST=WNRST
		PlantF%WTCO=WTCO
		PlantF%WTLF=WTLF
		PlantF%WTLO=WTLO
		PlantF%WTMAIN=WTMAIN
		PlantF%WTNCAN=WTNCAN
		PlantF%WTNEW=WTNEW
		PlantF%WTNLA=WTNLA
		PlantF%WTNLF=WTNLF
		PlantF%WTNLO=WTNLO
		PlantF%WTNNA=WTNNA
		PlantF%WTNNAG=WTNNAG
		PlantF%ROWSPC=ROWSPC
		PlantF%RTWT=RTWT
		PlantF%SDNPL=SDNPL
		PlantF%SDRATE=SDRATE
		PlantF%SDWT=SDWT
		PlantF%SEEDNI=SEEDNI
		PlantF%SEEDNO=SEEDNO
		PlantF%SENESCE=SENESCE
		PlantF%SHELWT=SHELWT
		PlantF%SLA=SLA
		PlantF%SLAAD=SLAAD
		PlantF%STMWT=STMWT
		PlantF%TOPWT=TOPWT
		PlantF%TOTWT=TOTWT
		PlantF%WCRLF=WCRLF



		PlantF%PODWT=PODWT
		PlantF%PUNCSD=PUNCSD
		PlantF%PUNCTR=PUNCTR
		PlantF%RHOL=RHOL
		PlantF%RHOS=RHOS
		PlantF%RNITP=RNITP
		PlantF%PCNSH=PCNSH
		PlantF%PCNST=PCNST
		PlantF%PLTPOP=PLTPOP
		PlantF%PLIGLF=PLIGLF
		PlantF%PLIGNO=PLIGNO
		PlantF%PLIGRT=PLIGRT
		PlantF%PLIGSD=PLIGSD
		PlantF%PLIGSH=PLIGSH
		PlantF%PLIGST=PLIGST

		PlantF%DWNODA=DWNODA
		PlantF%GROWTH=GROWTH
		PlantF%GRWRES=GRWRES
		PlantF%LAIMX=LAIMX	
		PlantF%PCCSD=PCCSD
		PlantF%PCLSD=PCLSD	
		PlantF%PCNL=PCNL
		PlantF%PCNRT=PCNRT
		PlantF%PCNSD=PCNSD
	
		PlantF%SWIDOT=SWIDOT
		PlantF%WLFDOT=WLFDOT
		PlantF%WSHIDT=WSHIDT
		PlantF%WTNFX=WTNFX
		PlantF%XHLAI=XHLAI
		PlantF%AREALF=AREALF
		PlantF%BETN=BETN
		PlantF%CANNAA=CANNAA
		PlantF%CANWAA=CANWAA
		PlantF%CLW=CLW
		PlantF%CSW=CSW
		PlantF%DWNOD=DWNOD
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
	write(*,*)'CROP:TO NUPTAK'
	NDMSDR=PlantF%NDMSDR
	NDMTOT=PlantF%NDMTOT
	RLV=PlantF%RLV
!	write(*,*)'To NUPTAK'
      CALL NUPTAK(SEASINIT, 
     &    DLAYR, DUL, FILECC, KG2PPM, LL, NDMSDR, NDMTOT, !Input
     &    NH4, NO3, NLAYR, RLV, SAT, SW,                  !Input
     &    TRNH4U, TRNO3U, TRNU, UNH4, UNO3)               !Output
	CALL GET(PlantF)
	PlantF%TRNH4U=TRNH4U
	PlantF%TRNO3U=TRNO3U
	PlantF%TRNU=TRNU
	PlantF%UNH4=UNH4
	PlantF%UNO3=UNO3
	CALL PUT(PlantF)
	write(*,*)'CROP: TO P_CGRO'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
!     Plant phosphorus module initialization
	MDATE=PlantF%MDATE
	PCNVEG=PlantF%PCNVEG
	PLTPOP=PLantF%PLTPOP
	RLV=PlantF%RLV
	RootMob=PlantF%RootMob
	RTDEP=PlantF%RTDEP
	RTWT=PlantF%RTWT
	SDWT=PlantF%SDWT
	SeedFrac=PlantF%SeedFrac
	ShelMob=PlantF%ShelMob
	SHELWT=PlantF%SHELWT
	ShutMob=PlantF%ShutMob
	STMWT=PlantF%STMWT
	SWIDOT=PlantF%SWIDOT
	VegFrac=PlantF%VegFrac
	WLIDOT=PlantF%WLIDOT
	WRIDOT=PlantF%WRIDOT
	WSHIDT=PlantF%WSHIDT
	WSIDOT=PlantF%WSIDOT
	WTLF=PlantF%WTLF
	SENESCE=PlantF%SENESCE	
      CALL P_CGRO (DYNAMIC, ISWITCH, 
     &    CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,       !Input
     &    RootMob, RTDEP, RTWT, SDWT, SeedFrac,           !Input
     &    ShelMob, SHELWT, ShutMob, SOILPROP,             !Input
     &    SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,      !Input
     &    WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,            !Input
     &    SENESCE,                                        !I/O
     &    PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &    PStres1, PStres2, PUptake, FracRts)             !Output
	CALL GET(PlantF)
	PlantF%SENESCE=SENESCE
	PlantF%PConc_Shut=PConc_Shut
	PlantF%PConc_Root=PConc_Root
	PlantF%PConc_Shel=PConc_Shel
	PlantF%PConc_Seed=PConc_Seed
	PlantF%PStres1=PStres1
	PlantF%PStres2=PStres2
	PlantF%PUptake=PUptake
	PlantF%FracRts=FracRts
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
	write(*,*)'CROP:TO MOBIL'
	NDMNEW=PlantF%NDMNEW
	NMINEP=PLantF%NMINEP
	NMOBR=PlantF%NMOBR
	RPRO=PlantF%RPRO
	TRNU=PlantF%TRNU
	WNRLF=PlantF%WNRLF
	WNRRT=PlantF%WNRRT
	WNRSH=PlantF%WNRSH
	WNRST=PlantF%WNRST
      CALL MOBIL(SEASINIT,
     &    NDMNEW, NMINEP, NMOBR, RPRO, TRNU,              !Input
     &    WNRLF, WNRRT, WNRSH, WNRST,                     !Input
     &    NMINEA, NRUSLF, NRUSRT, NRUSSH, NRUSST)         !Output
	CALL GET(PlantF)
	PlantF%NMINEA=NMINEA
	PlantF%NRUSLF=NRUSLF
	PlantF%NRUSRT=NRUSRT
	PlantF%NRUSSH=NRUSSH
	PlantF%NRUSST=NRUSST
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
	write(*,*)'CROP;TO NFIX'
	CALL GET(PlantF)
		AGRNOD=PlantF%AGRNOD
		CNODMN=PlantF%CNODMN
		CTONOD=PlantF%CTONOD
		DXR57=PlantF%DXR57
		NR7=PlantF%NR7
		PLTPOP=PlantF%PLTPOP
		TURFAC=PlantF%TURFAC
      CALL NFIX(SEASINIT,
     &    AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,           !Input
     &    FILECC, FILEIO, NLAYR, NR7, PLTPOP,             !Input
     &    SAT, ST, SW, TURFAC,                            !Input
     &    CNOD, DWNOD, DWNODA, NDTH, NFIXN,               !Output
     &    NODGR, WTNFX, SENNOD)                           !Output
				PlantF%CNOD=CNOD
		PlantF%DWNOD=DWNOD	
		PlantF%DWNODA=DWNODA
		PlantF%NDTH=NDTH	
		PlantF%NFIXN=NFIXN
		PlantF%NODGR=NODGR
		PlantF%WTNFX=WTNFX
		PlantF%SENNOD=SENNOD
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
	write(*,*)'CROP:TO PODS'
		AGRSH1=PlantF%AGRSH1
		FNINSH=PlantF%FNINSH
		NAVL=PlantF%NAVL
		NDSET=PlantF%NDSET
		NRUSSH=PlantF%NRUSSH
		PHTHRS=PlantF%PHTHRS
		RNITP=PlantF%RNITP
		SDDES=PlantF%SDDES
		SDGR=PlantF%SDGR
		XFRT=PlantF%XFRT
		YRNR1=PlantF%YRNR1
		YRNR2=PlantF%YRNR2
		PStres2=PlantF%PStres2
      CALL PODS(SEASINIT, 
     &    AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       !Input
     &    FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    !Input
     &    GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, !Input
     &    NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    !Input
     &    PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  !Input
     &    TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, !Input
     &    PStres2, YRPLT,                                 !Input
     &    AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    !Output
     &    PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     !Output
     &    SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     !Output
     &    WTSHE, WTSHMT, FLWN)                            !Output
	CALL GET(PlantF)
		PlantF%AGRSD3=AGRSD3
		PlantF%LAGSD=LAGSD
		PlantF%LNGPEG=LNGPEG
		PlantF%NGRSD=NGRSD
		PlantF%NGRSH=NGRSH
		PlantF%PCTMAT=PCTMAT
		PlantF%PODNO=PODNO
		PlantF%POTCAR=POTCAR
		PlantF%POTLIP=POTLIP
		PlantF%SDNO=SDNO
		PlantF%SDVAR=SDVAR
		PlantF%SEEDNO=SEEDNO
		PlantF%SHELN=SHELN
		PlantF%SHVAR=SHVAR
		PlantF%WSDDTN=WSDDTN
		PlantF%WSHDTN=WSHDTN
		PlantF%WTABRT=WTABRT
		PlantF%WTSD=WTSD
		PlantF%WTSHE=WTSHE
		PlantF%WTSHMT=WTSHMT
		PLantF%FLWN=FLWN
	CALL PUT(plantF)
!-----------------------------------------------------------------------
	write(*,*)'CROP:TO VEGGR'
		CMINEP=PlantF%CMINEP
		CSAVEV=PlantF%CSAVEV
		DTX=PlantF%DTX
		FNINR=PlantF%FNINR
		FNINS=PlantF%FNINS
		KCAN=PlantF%KCAN
		NAVL=PlantF%NAVL
		NDMNEW=PlantF%NDMNEW
		NDMOLD=PlantF%NDMOLD
		NFIXN=PlantF%NFIXN
		NMINEA=PlantF%NMINEA
		PCH2O=PlantF%PCH2O
		PG=PlantF%PG
		PStres2=PlantF%PStres2
		ROWSPC=PlantF%ROWSPC
		RVSTGE=PlantF%RVSTGE
		STMWT=PlantF%STMWT
		TRNU=PlantF%TRNU
		TURFAC=PlantF%TURFAC
		VSTAGE=PlantF%VSTAGE
		WCRLF=PlantF%WCRLF
		WCRRT=PlantF%WCRRT
		WCRSH=PlantF%WCRSH
		WCRST=PlantF%WCRST
		WTLF=PlantF%WTLF
		XLAI=PlantF%XLAI	
		YREMRG=PlantF%YREMRG
		AGRVG=PlantF%AGRVG
		FRLF=PlantF%FRLF
		FRRT=PlantF%FRRT
		FRSTM=PlantF%FRSTM	
      CALL VEGGR (SEASINIT, 
     &    AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,      !Input
     &    DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,     !Input
     &    FNINS, KCAN, NAVL, NDMNEW, NDMOLD,              !Input
     &    NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,      !Input
     &    PStres2, ROWSPC, RVSTGE, STMWT, TGRO,           !Input
     &    TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,      !Input
     &    WCRST, WTLF, XLAI, YRDOY, YREMRG,               !Input
     &    AGRVG, FRLF, FRRT, FRSTM,                       !I/O
     &    CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,     !Output
     &    CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,   !Output
     &    NADST, NGRLF, NGRRT, NGRST, NSTRES,             !Output
     &    TNLEAK, WLDOTN, WRDOTN, WSDOTN)                 !Output
	CALL GET(PlantF)
		PlantF%AGRVG=AGRVG
		PlantF%FRLF=FRLF
		PlantF%FRRT=FRRT
		PlantF%FRSTM=FRSTM
		PlantF%CADLF=CADLF
		PlantF%CADST=CADST
		PlantF%CANHT=CANHT
		PlantF%CANWH=CANWH
		PlantF%CMINEA=CMINEA
		PlantF%CRUSLF=CRUSLF
		PlantF%CRUSRT=CRUSRT
		PlantF%CRUSSH=CRUSSH
		PlantF%CRUSST=CRUSST
		PlantF%EXCESS=EXCESS
		PlantF%NADLF=NADLF
		PlantF%NADRT=NADRT
		PlantF%NADST=NADST
		PlantF%NGRLF=NGRLF
		PlantF%NGRRT=NGRRT
		PlantF%NGRST=NGRST
		PlantF%NSTRES=NSTRES
		PlantF%TNLEAK=TNLEAK
		PlantF%WLDOTN=WLDOTN
		PlantF%WRDOTN=WRDOTN
		PlantF%WSDOTN=WSDOTN
!-----------------------------------------------------------------------
C     Call leaf senescence routine for initialization
C-----------------------------------------------------------------------
	write(*,*)'CROP:TO SENES'

	CLW=PlantF%CLW
	DTX=PlantF%DTX
	KCAN=PlantF%KCAN
	NR7=PlantF%NR7
	NRUSLF=PlantF%NRUSLF
	RHOL=PlantF%RHOL
	SLAAD=plantF%SLAAD
	STMWT=PlantF%STMWT
	SWFAC=PlantF%SWFAC
	VSTAGE=PlantF%VSTAGE
	WTLF=PlantF%WTLF
	XLAI=PlantF%XLAI		
      CALL SENES(SEASINIT, 
     &    FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,       !Input
     &    RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI,  !Input
     &    SLDOT, SLNDOT, SSDOT, SSNDOT)                   !Output
	CALL GET(PlantF)
	PlantF%SLDOT=SLDOT
	PlantF%SLNDOT=SLNDOT
	PlantF%SSDOT=SSDOT
	PlantF%SSNDOT=SSNDOT
	CALL PUT(PlantF)

C-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
C-----------------------------------------------------------------------
	AGRRT=PlantF%AGGRT
	DTX=PlantF%DTX
	FRRT=PlantF%FRRT
	PG=PlantF%PG
	PLTPOP=PlantF%PLTPOP
	RO=PlantF%RO
	RP=PlantF%RP
	RTWT=PlantF%RTWT
	SWFAC=PlantF%SWFAC
	VSTAGE=PlantF%VSTAGE
	WRDOTN=PlantF%WRDOTN
	WTNEW=PlantF%WTNEW
!	write(*,*)'ROOTS'
      CALL ROOTS(SEASINIT,
     &    AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, !Input
     &    ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    !Input
     &    SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      !Input
     &    RLV, RTDEP, SATFAC, SENRT, SRDOT)               !Output
	CALL GET(PlantF)
	PlantF%RLV=RLV
	PlantF%RTDEP=RTDEP
	PlantF%SATFAC=SATFAC
	PlantF%SENRT=SENRT
	PlantF%SRDOT=SRDOT
	CALL PUT(PlantF)	
!-----------------------------------------------------------------------
!     Write headings to output file GROWTH.OUT
!-----------------------------------------------------------------------
	write(*,*)'CROP: to OPGROW'
      CALL OPGROW(CONTROL, ISWITCH, SoilProp,  
     &    CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD, GROWTH,  
     &    GRWRES, KSTRES, MAINR, MDATE, NFIXN, NLAYR, NSTRES, 
     &    PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG, 
     &    PODNO, PODWT, PODWTD, PSTRES1, PSTRES2, RHOL, RHOS, 
     &    RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDWT, SEEDNO, 
     &    SENESCE, SLA, STMWT, SWFAC, TGRO, TGROAV, TOPWT, 
     &    TOTWT, TURFAC, VSTAGE, WTLF, WTNCAN, WTNLF, WTNST, 
     &    WTNSD, WTNUP, WTNFX, XLAI, YRPLT) 

      CALL OPHARV (CONTROL, ISWITCH, 
     &    AGEFAC, CANHT, CANNAA, CANWAA, CROP,            !Input
     &    HARVFRAC, LAIMX, MDATE, NSTRES, PCLSD, PCNSD,   !Input
     &    PODNO, PODWT, PStres1, PStres2, SDRATE, SDWT,   !Input
     &    SEEDNO, STGDOY, SWFAC, TOPWT, TURFAC,           !Input
     &    VSTAGE, WTNCAN, WTNFX, WTNSD, WTNST, WTNUP,     !Input
     &    XLAI, RSTAGE, YREMRG, YRNR1, YRNR3, YRNR5,      !Input
     &    YRNR7, YRPLT,                                   !Input
     &    SDWTAH)                                         !Output

! Zero the value of HARVRES composite variable here 
!     NOTE: At this time, the variable has already been used to 
!     initialize soil properties for this season.
        HARVRES % RESWT  = 0.0
        HARVRES % RESLIG = 0.0
        HARVRES % RESE   = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. RATE) THEN
!***********************************************************************
	!IF(interface_ihru==2)write(*,*)'CROPGROW,RATE'
	CALL GET(PlantF)
	KC_SLOPE=PlantF%KC_SLOPE
	NOUTDO=PlantF%NOUTDO
	KCAN=PlantF%KCAN
	CADPR1=PlantF%CADPR1
	CMOBMX=PlantF%CMOBMX
	DETACH=PlantF%DETACH
	!ECONO=PlantF%ECONO
	EORATIO=PlantF%EORATIO
	FRCNOD=PlantF%FRCNOD
	FREEZ1=PlantF%FREEZ1
	FREEZ2=PlantF%FREEZ2
	KCAN=PlantF%KCAN
	KC_SLOPE=PlantF%KC_SLOPE
	KEP=PlantF%KEP
	NOUTDO=PlantF%NOUTDO
	PCARSH=PlantF%PCARSH
	PCH2O=PlantF%PCH2O
	PLIPSH=PlantF%PLIPSH
	PLIGSD=PlantF%PLIGSD
	PLIGSH=PlantF%PLIGSH
	PMINSD=PlantF%PMINSD
	PMINSH=PlantF%PMINSH
	POASD=PlantF%POASD
	POASH=PlantF%POASH
	PORMIN=PlantF%PORMIN
	PROLFI=PlantF%PROLFI
	PRORTI=PlantF%PRORTI
	PROSHI=PlantF%PROSHI
	PROSTI=PlantF%PROSTI
	R30C2=PlantF%R30C2
	RCH2O=PlantF%RCH2O
	RES30C=PlantF%RES30C
	RFIXN=PlantF%RES30C
	ROA=PlantF%ROA
	RPRO=PlantF%RPRO
	RWUEP1=PlantF%RWUEP1
	RWUMX=PlantF%RWUMX
	TTFIX=PlantF%TTFIX
	YREMRG=PlantF%YREMRG
	IF(interface_ihru==2)THEN
   	write(*,*)'YRDOY=',YRDOY,'YREMRG=',YREMRG
	END IF
	  IF (YRDOY .GT. YREMRG .AND. YREMRG .GT. 0 
     &                    .AND. ISWWAT .EQ. 'Y') THEN
!       Calculate daily water stess factors (from SWFACS)
!       EOP in mm/d
!       TRWUP and EP1 in cm/d
        SWFAC  = 1.0
        TURFAC = 1.0
        IF (EOP .GT. 0.001) THEN
          EP1 = EOP * 0.1
          IF (TRWUP / EP1 .LT. RWUEP1) THEN
            TURFAC = (1./RWUEP1) * TRWUP / EP1
          ENDIF
          IF (EP1 .GE. TRWUP) THEN
            SWFAC = TRWUP / EP1
          ENDIF
        ENDIF
	PlantF%SWFAC=SWFAC
	PlantF%TURFAC=TURFAC
	CALL PUT(PlantF)
      ENDIF
!	write(*,*)'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
!	write(*,*)'CROPGROW,TURFAC=',TURFAC,'EOP=',EOP,'TRWUP=',TRWUP
!	write(*,*)'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
!-----------------------------------------------------------------------
!     CALL vegetative and reproductive development subroutine
!-----------------------------------------------------------------------
	NSTRES=PlantF%NSTRES
	PStres2=PlantF%PStres2
	XPOD=PlantF%XPOD
!	write(*,*)'YRPLT=',YRPLT
      IF (CROP .NE. 'FA') THEN
        CALL PHENOL(CONTROL, ISWITCH, 
     &    DAYL, NSTRES, PStres2, SOILPROP, ST,            !Input
     &    SW, SWFAC, TGRO, TMIN, TURFAC, XPOD, YRPLT,     !Input
     &    DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        !Output
     &    NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       !Output
     &    RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,        !Output
     &    TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1,         !Output
     &    YRNR2, YRNR3, YRNR5, YRNR7)                     !Output
      ENDIF
	CALL GET(PlantF)
	IF(interface_ihru==2)write(*,*)'YREMRG=',YREMRG,'NVEG0=',NVEG0
	PlantF%DRPP=DRPP
	PlantF%DTX=DTX
	PlantF%DXR57=DXR57
	PlantF%FRACDN=FRACDN
	PlantF%MDATE=MDATE
	PlantF%NDLEAF=NDLEAF
	PlantF%NDSET=NDSET
	PlantF%NR1=NR1
	PlantF%NR2=NR2
	PlantF%NR5=NR5
	PlantF%NR7=NR7
	!write(*,*)'RATE,interface_ihru=',interface_ihru,'NVEG0=',NVEG0
	PlantF%NVEG0=NVEG0
	PlantF%PHTHRS=PHTHRS
	PlantF%RSTAGE=RSTAGE
	PlantF%RVSTGE=RVSTGE
	PlantF%STGDOY=STGDOY
	PlantF%SeedFrac=SeedFrac
	PlantF%TDUMX=TDUMX
	PlantF%TDUMX2=TDUMX2
	PlantF%VegFrac=VegFrac
	PlantF%VSTAGE=VSTAGE
	PlantF%YREMRG=YREMRG
	PlantF%YRNR1=YRNR1
	PlantF%YRNR2=YRNR2
	PlantF%YRNR5=YRNR5
	PlantF%YRNR7=YRNR7
	CALL PUT(PlantF)	
!----------------------------------------------------------------------
	AREALF=PlantF%AREALF
	CLW=PlantF%CLW
	CSW=PlantF%CSW
	LAGSD=PlantF%LAGSD
	LNGPEG=PlantF%LNGPEG
	NR2=PlantF%NR2
	PGAVL=PlantF%PGAVL
	PHTIM=PlantF%PHTIM
	PLTPOP=PlantF%PLTPOP
	RTWT=PlantF%RTWT
	SLA=PlantF%SLA
	SLDOT=PlantF%SLDOT
	SSDOT=PlantF%SSDOT
	STMWT=PlantF%STMWT
	TOPWT=PlantF%TOPWT
	WLFDOT=PlantF%WLFDOT
	WTLF=PlantF%WTLF
	RLV=PlantF%RLV
	SDNO=PlantF%SDNO
	SHELN=PlantF%SHELN
	SWIDOT=PlantF%SWIDOT
	VSTAGE=PlantF%VSTAGE
	WSHIDT=PlantF%WSHIDT
	WTSD=PlantF%WTSD
	WTSHE=PlantF%WTSHE	
      IF (ISWDIS .EQ. 'Y') THEN
        CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
      ENDIF
	CALL GET(PlantF)
	PlantF%RLV=RLV
	PlantF%SDNO=SDNO
	PlantF%SHELN=SHELN
	PlantF%SWIDOT=SWIDOT
	PlantF%VSTAGE=VSTAGE
	PlantF%WSHIDT=WSHIDT
	PlantF%WTSD=WTSD
	PlantF%WTSHE=WTSHE
	PlantF%ASMDOT=ASMDOT
	PlantF%DISLA=DISLA
	PlantF%NPLTD=NPLTD
	PlantF%PPLTD=PPLTD
	PlantF%SDDES=SDDES
	PlantF%WLIDOT=WLIDOT
	PlantF%WRIDOT=WRIDOT
	PlantF%WSIDOT=WSIDOT
	PlantF%SDWT=SDWT
	CALL PUT(PlantF)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	BETN=PlantF%BETN
	EXCESS=PlantF%EXCESS
	PStres1=PlantF%PStres1
	RNITP=PlantF%RNITP
	SLAAD=PlantF%SLAAD
	XHLAI=PlantF%XHLAI
	XPOD=PlantF%XPOD
      IF (CROP .NE. 'FA' .AND. DAS .GT. NVEG0) THEN
        IF (MEPHO .EQ. 'L') THEN
          !Retrieve AGEFAC and PG from ETPHOT routine.
          CALL GET('SPAM', 'AGEFAC', AGEFAC)
          CALL GET('SPAM', 'PG'    , PG)
        ELSEIF (MEPHO .EQ. 'C') THEN
	BETN=PlantF%BETN
	DXR57=PlantF%DXR57
	EXCESS=PlantF%EXCESS
	KCAN=PlantF%KCAN
	KC_SLOPE=PlantF%KC_SLOPE
	NR5=PlantF%NR5
	PStres1=PlantF%PStres1
	!SLPF=PlantF%SLPF
	RNITP=PlantF%RNITP
	SLAAD=PlantF%SLAAD
	SWFAC=PlantF%SWFAC
	XHLAI=PlantF%XHLAI
	XPOD=PlantF%XPOD
          CALL PHOTO(CONTROL, 
     &    BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE,       !Input
     &    NR5, PAR, PStres1, SLPF, RNITP, SLAAD,          !Input
     &    SWFAC, TDAY, XHLAI, XPOD,                       !Input
     &    AGEFAC, PG)                                     !Output
        ENDIF
      ENDIF
       PlantF%AGEFAC=AGEFAC
       PlantF%PG=PG
       
	CALL PUT(PlantF)
!write(*,*)'CROPGROW,RATE, YREMRG=',YREMRG,
!& PlantF%WTNTOT,PlantF%WTNLF,'ihru=',interface_ihru
!***********************************************************************
!***********************************************************************
!     DAILY INTEGRATION 
!***********************************************************************
      ELSE IF (DYNAMIC .EQ. INTEGR .AND. CROP .NE. 'FA') THEN
!***********************************************************************
!	write(*,*)'CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC'
!	write(*,*)'CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC'
!	write(*,*)'TO INTEGROINTI'	
	CALL GET(PlantF)
	!write(*,*)'INTEGR,interface_ihru=',interface_ihru,'NVEG0=',NVEG0
	NVEG0=PlantF%NVEG0
	DETACH=PlantF%DETACH
	NSTRES=PlantF%NSTRES
	XPOD=PlantF%XPOD
	KCAN=PlantF%KCAN
	KC_SLOPE=PlantF%KC_SLOPE
	NOUTDO=PlantF%NOUTDO
	PCARSH=PlantF%PCARSH
	PCH2O=PlantF%PCH2O
	PLIPSH=PlantF%PLIPSH
	PMINSD=PlantF%PMINSD
	PMINSH=PlantF%PMINSH
	POASD=PlantF%POASD
	POASH=PlantF%POASH
	PROLFI=PlantF%PROLFI
	PRORTI=PlantF%PRORTI
	PROSHI=PlantF%PROSHI
	PROSTI=PLantF%PROSTI
	R30C2=PlantF%R30C2
	RCH2O=PlantF%RCH2O
	RES30C=PlantF%RES30C
	RFIXN=PlantF%RFIXN
	RLIG=PlantF%RLIG
	RLIP=PlantF%RLIP
	RMIN=PlantF%RMIN
	RNH4C=PlantF%RNH4C
	RNO3C=PlantF%RNO3C
	ROA=PlantF%ROA
	RPRO=PlantF%RPRO
	TTFIX=PlantF%TTFIX
!-----------------------------------------------------------------------
!     Move PHENOL integration up here.
!     Need to set NVEG0 before test for DAS = NVEG0, otherwise,
!     initialization on day of emergence will never occur.
!-----------------------------------------------------------------------
        NSTRES=PlantF%NSTRES
        PStres2=PlantF%PStres2
        XPOD=PlantF%XPOD



!	write(*,*)'To Phenol'
      CALL PHENOL(CONTROL, ISWITCH, 
     &    DAYL, NSTRES, PStres2, SOILPROP, ST,            !Input
     &    SW, SWFAC, TGRO, TMIN, TURFAC, XPOD, YRPLT,     !Input
     &    DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        !Output
     &    NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       !Output
     &    RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,        !Output
     &    TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1,         !Output
     &    YRNR2, YRNR3, YRNR5, YRNR7)                     !Output
!	write(*,*)'YREMRG=',YREMRG,'NVEG0=',NVEG0,'DAS=',DAS
	CALL GET(PlantF)
        PlantF%DRPP=DRPP
        PlantF%DTX=DTX
        PlantF%DXR57=DXR57
        PlantF%FRACDN=FRACDN
        PlantF%MDATE=MDATE
        PlantF%NDLEAF=NDLEAF
        PlantF%NDSET=NDSET
        PlantF%NR1=NR1
        PlantF%NR2=NR2
        PlantF%NR5=NR5
        PlantF%NR7=NR7
	!write(*,*)'INTEGRATIOM,interface_ihru=',interface_ihru,
    ! & 'NVEG0=',NVEG0
        PlantF%NVEG0=NVEG0
        PlantF%PHTHRS=PHTHRS
        PlantF%RSTAGE=RSTAGE
        PlantF%RVSTGE=RVSTGE
        PlantF%STGDOY=STGDOY
        PlantF%SeedFrac=SeedFrac
        PlantF%TDUMX=TDUMX
        PlantF%TDUMX2=TDUMX2
        PlantF%VegFrac=VegFrac
        PlantF%VSTAGE=VSTAGE
        PlantF%YREMRG=YREMRG
        PlantF%YRNR1=YRNR1
        PlantF%YRNR2=YRNR2
        PlantF%YRNR5=YRNR5
        PlantF%YRNR7=YRNR7
	CALL PUT(PlantF)
!	write(*,*)'Before EMERGENSE DAS=',DAS,'NVEG0=',
!     & NVEG0,interface_ihru
	IF(interface_ihru==2)write(*,*)'DAS=',DAS,'NVEG0=',NVEG0
!-----------------------------------------------------------------------
      IF (DAS .EQ. NVEG0) THEN
	!CALL GET(PlantF)
	!write(*,*)'CROPGROW,EMERG,BEF,PlantF%AREALF=',PlantF%AREALF,NVEG0
	!IF(interface_ihru==39)STOP
!----------------------------------------------------------------------
!     On day of emergence, initialize:
!-----------------------------------------------------------------------
	AGEFAC=PlantF%AGEFAC
	CADLF=PlantF%CADLF
	CADST=PlantF%CADST
	CRUSLF=PlantF%CRUSLF
	CRUSRT=PlantF%CRUSRT
	CRUSSH=PlantF%CRUSSH
	CRUSST=PlantF%CRUSST
	DISLA=PlantF%DISLA
	F=PlantF%F
	FRLF=PlantF%FRLF
	FRSTM=PlantF%FRSTM
	NADLF=PlantF%NADLF
	NADRT=PlantF%NADRT
	NADST=PlantF%NADST
	NDTH=PlantF%NDTH
	NFIXN=PlantF%NFIXN
	NGRLF=PlantF%NGRLF
	NGRRT=PlantF%NGRRT
	NGRSD=PlantF%NGRSD
	NGRSH=PlantF%NGRSH
	NGRST=PlantF%NGRST
	NMINEA=PlantF%NMINEA
	NODGR=PlantF%NODGR
	NOUTDO=PlantF%NOUTDO
	NPLTD=PlantF%NPLTD
	NRUSLF=PlantF%NRUSLF
	NRUSRT=PlantF%NRUSRT
	NRUSSH=PlantF%NRUSSH
	NRUSST=PlantF%NRUSST
	POTCAR=PlantF%POTCAR
	POTLIP=PlantF%POTLIP
	PPLTD=PlantF%PPLTD
	SDIDOT=PlantF%SDIDOT
	SDPROR=PlantF%SDPROR
	SENNOD=PlantF%SENNOD
	SENRT=PlantF%SENRT
	SLDOT=PlantF%SLDOT
	SLNDOT=PlantF%SLNDOT
	SRDOT=PlantF%SRDOT
	SSDOT=PlantF%SSDOT
	SSNDOT=PlantF%SSNDOT
	TRNH4U=PlantF%TRNH4U
	TRNO3U=PlantF%TRNO3U
	TRNU=PlantF%TRNU
	TURFAC=PlantF%TURFAC
	WLDOTN=PlantF%WLDOTN
	WLIDOT=PlantF%WLIDOT
	WRDOTN=PlantF%WRDOTN
	WRIDOT=PlantF%WRIDOT
	WSDDTN=PlantF%WSDDTN
	WSDOTN=PlantF%WSDOTN
	WSHDTN=PlantF%WSHDTN
	WSIDOT=PlantF%WSIDOT
	WTABRT=PlantF%WTABRT
	WTSHMT=PlantF%WTSHMT
	YRNR1=PlantF%YRNR1
	MDATE=PlantF%MDATE
	!YRPLT=PlantF%YRPLT
	SWIDOT=PlantF%SWIDOT
	WLFDOT=PlantF%WLFDOT
	WSHIDT=PlantF%WSHIDT
	WTNFX=PlantF%WTNFX
	XHLAI=PlantF%XHLAI	
	write(*,*)'CROPGROW,TO GROW_DSSAT,EMERGENCE'
        CALL GROW_DSSAT(CONTROL, ISWITCH, EMERG, SOILPROP, 
     &  AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     !Input
     &  CRUSST, DISLA, F, FILECC, FRLF, FRSTM,            !Input
     &  NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   !Input
     &  NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       !Input
     &  NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,            !Input
     &  POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            !Input
     &  SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       !Input
     &  SSNDOT, TRNH4U, TRNO3U, TRNU,                     !Input
     &  TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   !Input
     &  WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    !Input
     &  MDATE, YRPLT,                                     !Input
     &  SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             !Input/Output
     &  AREALF, BETN, CANNAA, CANWAA, CLW, CSW, DWNOD,    !Output
     &  DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, PCLSD,      !Output
     &  PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,         !Output
     &  PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   !Output
     &  PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         !Output
     &  ROWSPC, RTWT, SDNPL, SDRATE, SDWT,                !Output
     &  SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             !Output
     &  SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  !Output
     &  WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          !Output
     &  WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  !Output
     &  WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       !Output
     &  WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        !Output
     &  WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      !Output
     &  WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      !Output
     &  ShutMob, RootMob, ShelMob)                        !Output
!	write(*,*)'FROM GROW,WTNTOT=',WTNTOT
	!STOP
	CALL GET(PlantF)		
        write(*,*)'FROM GROW,WTNTOT=',PlantF%WTNTOT,PlantF%WTNLF

	PlantF%WTSHO=WTSHO
	PlantF%WTNSO=WTNSO
	PlantF%WTNST=WTNST
	PlantF%WTNUP=WTNUP
	PlantF%WTRO=WTRO
	PlantF%WTSDO=WTSDO
	PlantF%WTSHO=WTSHO
	PlantF%WTSO=WTSO
	PlantF%XLAI=XLAI
	PlantF%XPOD=XPOD
	PlantF%ShutMob=ShutMob
	PlantF%RootMob=RootMob
	PlantF%ShelMob=ShelMob

	PlantF%WTNNO=WTNNO
        PlantF%WTNNOD=WTNNOD
        PlantF%WTNOO=WTNOO
        PlantF%WTNRA=WTNRA
        PlantF%WTNRO=WTNRO
        PlantF%WTNRT=WTNRT
        PlantF%WTNSA=WTNSA
        PlantF%WTNSD=WTNSD
        PlantF%WTNSDA=WTNSDA
        PlantF%WTNSDO=WTNSDO
        PlantF%WTNSH=WTNSH
        PlantF%WTNSHA=WTNSHA
        PlantF%WTNSHO=WTNSHO
        PlantF%WTNSO=WTNSO
        PlantF%WTNST=WTNST
        PlantF%WTNUP=WTNUP
        PlantF%WTRO=WTRO
        PlantF%WTSDO=WTSDO


	PlantF%WCRRT=WCRRT
	PlantF%WCRSH=WCRSH
	PlantF%WCRST=WCRST
	PlantF%WNRLF=WNRLF
	PlantF%WNRRT=WNRRT
	PlantF%WNRSH=WNRSH
	PlantF%WNRST=WNRST
	PlantF%WTCO=WTCO
	PlantF%WTLF=WTLF
	PlantF%WTLO=WTLO
	PlantF%WTMAIN=WTMAIN
	PlantF%WTNCAN=WTNCAN
	PlantF%WTNEW=WTNEW
	PlantF%WTNLA=WTNLA
	PlantF%WTNLF=WTNLF
	PlantF%WTNLO=WTNLO
	PlantF%WTNNA=WTNNA
	PlantF%WTNNAG=WTNNAG
	PlantF%ROWSPC=ROWSPC
 	PlantF%RTWT=RTWT
 	PlantF%SDNPL=SDNPL
 	PlantF%SDRATE=SDRATE
 	PlantF%SDWT=SDWT
	PlantF%SEEDNI=SEEDNI
 	PlantF%SEEDNO=SEEDNO
 	PlantF%SENESCE=SENESCE
 	PlantF%SHELWT=SHELWT
 	PlantF%SLA=SLA
 	PlantF%SLAAD=SLAAD
 	PlantF%STMWT=STMWT
	PlantF%TOPWT=TOPWT
 	PlantF%TOTWT=TOTWT
	PlantF%WCRLF=WCRLF



	PlantF%PODWT=PODWT
	PlantF%PUNCSD=PUNCSD
	PlantF%PUNCTR=PUNCTR
	PlantF%RHOL=RHOL
	PlantF%RHOS=RHOS
	PlantF%RNITP=RNITP
	PlantF%PCNSH=PCNSH
	PlantF%PCNST=PCNST
	PlantF%PLTPOP=PLTPOP
	PlantF%PLIGLF=PLIGLF
	PlantF%PLIGNO=PLIGNO
	PlantF%PLIGRT=PLIGRT
	PlantF%PLIGSD=PLIGSD
	PlantF%PLIGSH=PLIGSH
	PlantF%PLIGST=PLIGST

	PlantF%DWNODA=DWNODA
	PlantF%GROWTH=GROWTH
	PlantF%GRWRES=GRWRES
	PlantF%LAIMX=LAIMX	
	PlantF%PCCSD=PCCSD
	PlantF%PCLSD=PCLSD	
	PlantF%PCNL=PCNL
	PlantF%PCNRT=PCNRT
	PlantF%PCNSD=PCNSD
	
	PlantF%SWIDOT=SWIDOT
	PlantF%WLFDOT=WLFDOT
	PlantF%WSHIDT=WSHIDT
	PlantF%WTNFX=WTNFX
	PlantF%XHLAI=XHLAI
	PlantF%AREALF=AREALF
	PlantF%BETN=BETN
	PlantF%CANNAA=CANNAA
	PlantF%CANWAA=CANWAA
	PlantF%CLW=CLW
	PlantF%CSW=CSW
	PlantF%DWNOD=DWNOD	
	CALL PUT(PlantF)
!	write(*,*)'GROPGROW,EMERGENSE,AFT,GROWTH',GROWTH

C-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
C-----------------------------------------------------------------------
	CALL GET(PlantF)
	AGRRT=PlantF%AGRRT
	FRRT=PlantF%FRRT
	PG=PlantF%PG
	RO=PlantF%RO
	RP=PlantF%RP
	RTWT=PlantF%RTWT
	SWFAC=PlantF%SWFAC
	VSTAGE=PlantF%VSTAGE
	WRDOTN=PlantF%WRDOTN
	WTNEW=PlantF%WTNEW
      CALL ROOTS(EMERG,
     &    AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, !Input
     &    ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    !Input
     &    SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      !Input
     &    RLV, RTDEP, SATFAC, SENRT, SRDOT)               !Output
!	write(*,*)'From ROOTS'
!	write(*,*)'RLV=',RLV,'RTDEP=',RTDEP,'SATFAC =',SATFAC
!	write(*,*)'SENRT=',SENRT,'SRDOT=',SRDOT
	CALL GET(PlantF)
	PlantF%RLV=RLV
	PlantF%RTDEP=RTDEP
	PlantF%SATFAC=SATFAC
	PlantF%SENRT=SENRT
	PlantF%SRDOT=SRDOT
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
!       DYNAMIC = EMERG (not INTEGR) here
	AGRLF=PlantF%AGRLF
	AGRRT=PlantF%AGRRT
	AGRSH2=PlantF%AGRSH2
	AGRSTM=PlantF%AGRSTM
	DRPP=PlantF%DRPP
	DXR57=PlantF%DXR57
	FNINSH=PlantF%FNINSH
	FRACDN=PlantF%FRACDN
	LAGSD=PlantF%FLAGSD
	LNGPEG=PlantF%LNGPEG
	NDLEAF=PlantF%NDLEAF
	NSTRES=PlantF%NSTRES
	PCNL=PlantF%PCNL
	PCNRT=PlantF%PCNRT
	PCNST=PlantF%PCNST
	PGAVL=PlantF%PGAVL
	PUNCSD=PlantF%PUNCSD
	PUNCTR=PlantF%PUNCTR
	PLTPOP=PlantF%PLTPOP
	RPROAV=PlantF%RPROAV
	RTWT=PlantF%RTWT
	SDDES=PlantF%SDDES
	SDNO=PlantF%SDNO
	SDVAR=PlantF%SDVAR
	SHELN=PlantF%SHELN
	SHVAR=PlantF%SHVAR
	STMWT=PlantF%STMWT
	SWFAC=PlantF%SWFAC
	TDUMX=PlantF%TDUMX
	TDUMX2=PlantF%TDUMX2
	TURFAC=PlantF%TURFAC
	VSTAGE=PlantF%VSTAGE
	WCRLF=PlantF%WCRLF
	WCRRT=PlantF%WCRRT
	WCRST=PlantF%WCRST
	WNRLF=PlantF%WNRLF
	WNRRT=PlantF%WNRRT
	WNRSH=PlantF%WNRSH
	WNRST=PlantF%WNRST
	WTLF=PlantF%WTLF
	WTSD=PlantF%WTSD
	WTSHE=PlantF%WTSHE
	XPOD=PlantF%XPOD
	NVEG0=PlantF%NVEG0
	NR1=PlantF%NR1
	NR2=PlantF%NR2
	NR5=PlantF%NR5
	NR7=PlantF%NR7
        CALL DEMAND(EMERG, CONTROL, 
     &  AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  !Input
     &  FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    !Input
     &  LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  !Input
     &  PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,      !Input
     &  SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,   !Input
     &  TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, !Input
     &  WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,   !Input
     &  WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7,     !Input
     &  AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, F, FNINL,  !Output
     &  FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   !Output
     &  GRRAT1, NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT,  !Output
     &  NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM, POTCAR,      !Output
     &  POTLIP, SDGR, TURADD, XFRT, YREND)                !Output
	CALL GET(PlantF)
	PlantF%NMOBR=NMOBR
	PlantF%PHTIM=PHTIM
	PlantF%PNTIM=PNTIM
	PlantF%POTCAR=POTCAR
	PlantF%POTLIP=POTLIP
	PlantF%SDGR=SDGR
	PlantF%TURADD=TURADD
	PlantF%XFRT=XFRT
	PlantF%YREND=YREND	
	PlantF%AGRSD1=AGRSD1
	PlantF%AGRSD2=AGRSD2
	PlantF%AGRVG=AGRVG
	PlantF%AGRVG2=AGRVG2
	PlantF%CDMREP=CDMREP
	PlantF%F=F
	PlantF%FNINL=FNINL
	PlantF%FNINR=FNINR
	PlantF%FNINS=FNINS
	PlantF%FNINSD=FNINSD
	PlantF%FRLF=FRLF
	PlantF%FRRT=FRRT
	PlantF%FRSTM=FRSTM
	PlantF%GDMSD=GDMSD
	PlantF%GRRAT1=GRRAT1
	PlantF%NDMNEW=NDMNEW
	PlantF%NDMOLD=NDMOLD
	PlantF%NDMREP=NDMREP
	PlantF%NDMSDR=NDMSDR
	PlantF%NDMTOT=NDMTOT
	PlantF%NDMVEG=NDMVEG
	PlantF%NMINEP=NMINEP
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
	AGRSH1=PlantF%AGRSH1
	FNINSH=PlantF%FNINSH
	NAVL=PlantF%NAVL
	NDSET=PlantF%NDSET
	NRUSSH=PlantF%NRUSSH
	PHTHRS=PlantF%PHTHRS
	RNITP=PlantF%RNITP
	SDDES=PlantF%SDDES
	SDGR=PlantF%SDGR
	XFRT=PlantF%XFRT
	YRNR1=PlantF%YRNR1
	YRNR2=PlantF%YRNR2
	PStres2=PlantF%PStres2

        CALL PODS(EMERG, 
     &    AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       !Input
     &    FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    !Input
     &    GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, !Input
     &    NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    !Input
     &    PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  !Input
     &    TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, !Input
     &    PStres2, YRPLT,                                 !Input
     &    AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    !Output
     &    PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     !Output
     &    SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     !Output
     &    WTSHE, WTSHMT, FLWN)                            !Output
	CALL GET(PlantF)
	PlantF%AGRSD3=AGRSD3
	PlantF%LAGSD=LAGSD
	PlantF%LNGPEG=LNGPEG
	PlantF%NGRSD=NGRSD
	PlantF%NGRSH=NGRSH
	PlantF%PCTMAT=PCTMAT
	PlantF%PODNO=PODNO
	PlantF%POTCAR=POTCAR
	PlantF%POTLIP=POTLIP
	PlantF%SDNO=SDNO
	PlantF%SDVAR=SDVAR
	PlantF%SEEDNO=SEEDNO
	PlantF%SHELN=SHELN
	PlantF%SHVAR=SHVAR
	PlantF%WSDDTN=WSDDTN
	PlantF%WSHDTN=WSHDTN
	PlantF%WTABRT=WTABRT
	PlantF%WTSD=WTSD
	PlantF%WTSHE=WTSHE
	PlantF%WTSHMT=WTSHMT
	PLantF%FLWN=FLWN
	CALL PUT(PlantF)
!-----------------------------------------------------------------------
	CMINEP=PlantF%CMINEP
	CSAVEV=PlantF%CSAVEV
	DTX=PlantF%DTX
	FNINR=PlantF%FNINR
	FNINS=PlantF%FNINS
	KCAN=PlantF%KCAN
	NAVL=PlantF%NAVL
	NDMNEW=PlantF%NDMNEW
	NDMOLD=PlantF%NDMOLD
	NFIXN=PlantF%NFIXN
	NMINEA=PlantF%NMINEA
	PCH2O=PlantF%PCH2O
	PG=PlantF%PG
	PStres2=PlantF%PStres2
	ROWSPC=PlantF%ROWSPC
	RVSTGE=PlantF%RVSTGE
	STMWT=PlantF%STMWT
	TRNU=PlantF%TRNU
	TURFAC=PlantF%TURFAC
	VSTAGE=PlantF%VSTAGE
	WCRLF=PlantF%WCRLF
	WCRRT=PlantF%WCRRT
	WCRSH=PlantF%WCRSH
	WCRST=PlantF%WCRST
	WTLF=PlantF%WTLF
	XLAI=PlantF%XLAI	
	YREMRG=PlantF%YREMRG
	AGRVG=PlantF%AGRVG
	FRLF=PlantF%FRLF
	FRRT=PlantF%FRRT
	FRSTM=PlantF%FRSTM
!	write(*,*)'TO VEGGR,FRLF=',FRLF,NAVL,'NSTRES=',NSTRES	
        CALL VEGGR(EMERG, 
     &    AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,      !Input
     &    DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,     !Input
     &    FNINS, KCAN, NAVL, NDMNEW, NDMOLD,              !Input
     &    NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,      !Input
     &    PStres2, ROWSPC, RVSTGE, STMWT, TGRO,           !Input
     &    TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,      !Input
     &    WCRST, WTLF, XLAI, YRDOY, YREMRG,               !Input
     &    AGRVG, FRLF, FRRT, FRSTM,                       !I/O
     &    CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,     !Output
     &    CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,   !Output
     &    NADST, NGRLF, NGRRT, NGRST, NSTRES,             !Output
     &    TNLEAK, WLDOTN, WRDOTN, WSDOTN)                 !Output
	IF(NSTRES==0)THEN
		PLANTING_DSSAT(interface_ihru)=11
!	write(*,*)'CROPGROW EMERGENSE :interface_ihru=',interface_ihru
	!STOP
	END IF
	IF(TURFAC==0)THEN
		PLANTING_DSSAT(interface_ihru)=1
	END IF
!	write(*,*)'FROM VEGGR,FRLF=',FRLF
	CALL GET(PlantF)
	PlantF%AGRVG=AGRVG
	PlantF%FRLF=FRLF
	PlantF%FRRT=FRRT
	PlantF%FRSTM=FRSTM
	PlantF%CADLF=CADLF
	PlantF%CADST=CADST
	PlantF%CANHT=CANHT
	PlantF%CANWH=CANWH
	PlantF%CMINEA=CMINEA
	PlantF%CRUSLF=CRUSLF
	PlantF%CRUSRT=CRUSRT
	PlantF%CRUSSH=CRUSSH
	PlantF%CRUSST=CRUSST
	PlantF%EXCESS=EXCESS
	PlantF%NADLF=NADLF
	PlantF%NADRT=NADRT
	PlantF%NADST=NADST
	PlantF%NGRLF=NGRLF
	PlantF%NGRRT=NGRRT
	PlantF%NGRST=NGRST
	PlantF%NSTRES=NSTRES
	PlantF%TNLEAK=TNLEAK
	PlantF%WLDOTN=WLDOTN
	PlantF%WRDOTN=WRDOTN
	PlantF%WSDOTN=WSDOTN
	CALL PUT(PlantF)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	PCNVEG=PlantF%PCNVEG
	PLTPOP=PlantF%PLTPOP
	RLV=PlantF%RLV
	RTDEP=PlantF%RTDEP
	RTWT=PlantF%RTWT
	SDWT=PlantF%SDWT
	SeedFrac=PlantF%SeedFrac
	SHELWT=PlantF%SHELWT
	STMWT=PlantF%STMWT
	SWIDOT=PlantF%SWIDOT
	VegFrac=PlantF%VegFrac
	WLIDOT=PlantF%WLIDOT
	WRIDOT=PlantF%WRIDOT
	WSHIDT=PlantF%WSHIDT
	WSIDOT=PlantF%WSIDOT
	WTLF=PlantF%WTLF
	SENESCE=PlantF%SENESCE
        IF (ISWPHO .EQ. 'Y' .OR. ISWPHO .EQ. 'H') THEN
!       Plant phosphorus module initialization at plant emergence
          CALL P_CGRO (EMERG, ISWITCH, 
     &      CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,       !Input
     &      RootMob, RTDEP, RTWT, SDWT, SeedFrac,           !Input
     &      ShelMob, SHELWT, ShutMob, SOILPROP,             !Input
     &      SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,      !Input
     &      WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,            !Input
     &      SENESCE,                                        !I/O
     &      PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &      PStres1, PStres2, PUptake, FracRts)             !Output
        ENDIF
	CALL GET(PlantF)
	PlantF%SENESCE=SENESCE
	PlantF%PConc_Shut=PConc_Shut
	PlantF%PConc_Shel=PConc_Shel
	PlantF%PConc_Root=PConc_Root
	PlantF%PConc_Seed=PConc_Seed
	PlantF%PStres1=PStres1
	PlantF%PUptake=PUptake
	plantF%FracRts=FracRts
	CALL PUT(PlantF)
	
!-----------------------------------------------------------------------
      ENDIF
!	write(*,*)'CROPGROW,INT, PlantF%AREALF=',PlantF%AREALF
!----------------------------------------------------------------------
	YRNR2=PlantF%YRNR2
      IF (DETACH .EQ. 'Y' .AND. DAS .LE. NVEG0+1) THEN
	  WTLF=PlantF%WTLF
		YRNR2=PlantF%YRNR2
		PODWTD=plantF%POTWTD

        CALL PODDET(
     &    FILECC, TGRO, WTLF, YRDOY, YRNR2,               !Input
     &    PODWTD, SDNO, SHELN, SWIDOT,                    !Output
     &    WSHIDT, WTSD, WTSHE,                            !Output
     &    EMERG)                                          !Control
	CALL GET(PlantF)
        PlantF%PODWTD=PODWTD
        PlantF%SDNO=SDNO
        PlantF%SHELN=SHELN
        PlantF%SWIDOT=SWIDOT
        PlantF%WSHIDT=WSHIDT
        PlantF%WTSD=WTSD
        PlantF%WTSHE=WTSHE
	CALL PUT(PlantF)
      ENDIF
!	write(*,*)'DAS=',DAS,'NVEG0=',NVEG0
!***********************************************************************
C     Skip growth processes and N and C balances before plants emerge
C-----------------------------------------------------------------------
      IF (DAS .GE. NVEG0) THEN
	CALL GET(PlantF)
!	write(*,*)'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'
!	write(*,*)'CROPGROW,INTEG, PlantF%FRLF=',PlantF%FRLF
	
!-----------------------------------------------------------------------
!     Initialize available N and C for beginning of daily calcs.
!-----------------------------------------------------------------------
      NAVL = 0.0
      PGAVL = 0.0
	PlantF%NAVL=NAVL
	PlantF%PGAVL=PGAVL
C-----------------------------------------------------------------------
C    Initialize variables that represent N and C availability during a day
C    Assume that Fraction CMOBMX of CH2O can be Mobilized per Day
C    PGAVL is the total available CH2O available for growth & respiration
C
C    8/26/97 KJB  DTX IN PLACE OF 1 TO SLOW IT DOWN A BIT AT ALL TIMES
C    AND TO BE SENSITIVE TO TEMPERATURE PRIOR TO R5 STAGE, BUT
C    STILL WANT THE SPEED-UP CAUSED BY THE "+ DXR57" FEATURE AFTER R5.
C
C-----------------------------------------------------------------------
	CMOBMX=PlantF%CMOBMX
      CMINEP = CMOBMX * (DTX + DXR57) * (WCRST + WCRRT + WCRSH +WCRLF)
      PGAVL = PG + CMINEP
!     ! write(*,*)'CROPGROW:INTEG,PGAVL',PGAVL,'PG=',PG,'CMINEP=',CMINEP
!     ! write(*,*)'CMOBMX=',CMOBMX,'DTX=',DTX,'DXR57=',DXR57
!     ! write(*,*)'WCRST=',WCRST,'WCRRT=',WCRRT,
!     !& 'WCRSH=',WCRSH,'WCRLF=',WCRLF
	PlantF%CMINEP=CMINEP
	PlantF%PGAVL=PGAVL
	CALL PUT(PlantF)			
C-----------------------------------------------------------------------
C       Compute maintenance respiration and subtract from available CH2O
C-----------------------------------------------------------------------
	PG=PlantF%PG
	R30C2=PlantF%R30C2
	RES30C=PlantF%RES30C
	WTMAIN=PlantF%WTMAIN
	RO=PlantF%RO
	RP=PlantF%RP
      CALL RESPIR(
     &    PG, R30C2, RES30C, TGRO, WTMAIN,                !Input
     &    RO, RP,                                         !Input/Output
     &    MAINR)                                          !Output
	CALL GET(PlantF)
      IF (MAINR .GT. PGAVL) THEN
        PGAVL  = 0.0
      ELSE
        PGAVL = PGAVL - MAINR
      ENDIF
	PlantF%RO=RO
	PlantF%RP=RP
	PlantF%MAINR=MAINR
	PlantF%PGAVL=PGAVL
	CALL PUT(PlantF)
!	write(*,*)'CROPGROWTH,PGAVL=',PGAVL,'MAINR=',MAINR
!-----------------------------------------------------------------------
!     Reduce PGAVL if pest damage occurs to C assimilation
!     Moved to PEST module - chp
!-----------------------------------------------------------------------
!     Call PEST Module for INTEGRATION calculations
!-----------------------------------------------------------------------
	AREALF=PlantF%AREALF
	CLW=PlantF%CLW
	CSW=PlantF%CSW
	LAGSD=PlantF%LAGSD
	LNGPEG=PlantF%LNGPEG
	NR2=PlantF%NR2
	PHTIM=PlantF%PHTIM
	PLTPOP=PlantF%PLTPOP
	RTWT=PlantF%RTWT
	SLA=PlantF%SLA
	SLDOT=PlantF%SLDOT
	SSDOT=PlantF%SSDOT
	STMWT=PlantF%STMWT
	TOPWT=PlantF%TOPWT
	WLFDOT=PlantF%WLFDOT		
	WTLF=PlantF%WTLF
	RLV=PlantF%RLV
	SDNO=PlantF%SDNO
	SHELN=PlantF%SHELN
	SWIDOT=PlantF%SWIDOT
	VSTAGE=PlantF%VSTAGE
	WSHIDT=PlantF%WSHIDT
	WTSD=PlantF%WTSD
	WTSHE=PlantF%WTSHE
      IF (ISWDIS.EQ.'Y') THEN
        CALL PEST(CONTROL, ISWITCH, 
     &    AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &    PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &    SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &    RLV, SDNO, SHELN, SWIDOT,                       !Input/Output
     &    VSTAGE, WSHIDT, WTSD, WTSHE,                    !Input/Output
     &    ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &    SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)                  !Output
	CALL GET(PlantF)
        IF (ASMDOT .GT. 1.E-4) THEN
          PGAVL = PGAVL - ASMDOT
          PGAVL = MAX(0.0,PGAVL)
        ENDIF
	PlantF%PGAVL=PGAVL
	PlantF%RLV=RLV
	PlantF%SDNO=SDNO
	PlantF%SHELN=SHELN
	PlantF%SWIDOT=SWIDOT
	PlantF%VSTAGE=VSTAGE
	PlantF%WSHIDT=WSHIDT
	PlantF%WTSD=WTSD
	PlantF%WTSHE=WTSHE
	PlantF%ASMDOT=ASMDOT
	PlantF%DISLA=DISLA
	PlantF%NPLTD=NPLTD
	PlantF%PPLTD=PPLTD
	PlantF%SDDES=SDDES
	PlantF%WLIDOT=WLIDOT
	PlantF%WRIDOT=WRIDOT
	PlantF%WSIDOT=WSIDOT
	PlantF%SDWT=SDWT
	CALL PUT(PlantF)
      ENDIF
!	WRITE(*,*)'CROPGROW,INTEG,From PEST AREALF=',AREALF
!	write(*,*)'PlantF%AREALF=',PlantF%AREALF
C-----------------------------------------------------------------------
C    Call Subroutine to calculate Nitrogen and Carbon Demand for new growth
C-----------------------------------------------------------------------
	AGRLF=PlantF%AGRLF
	AGRRT=PlantF%AGRRT
	AGRSH2=PlantF%AGRSH2
	AGRSTM=PlantF%AGRSTM
	DRPP=PlantF%DRPP
	DXR57=PlantF%DXR57
	FNINSH=PlantF%FNINSH
	FRACDN=PlantF%FRACDN
	LAGSD=PlantF%FLAGSD
	LNGPEG=PlantF%LNGPEG
	NDLEAF=PlantF%NDLEAF
	NSTRES=PlantF%NSTRES
	PCNL=PlantF%PCNL
	PCNRT=PlantF%PCNRT
	PCNST=PlantF%PCNST
	PGAVL=PlantF%PGAVL
	PUNCSD=PlantF%PUNCSD
	PUNCTR=PlantF%PUNCTR
	PLTPOP=PlantF%PLTPOP
	RPROAV=PlantF%RPROAV
	RTWT=PlantF%RTWT
	SDDES=PlantF%SDDES
	SDNO=PlantF%SDNO
	SDVAR=PlantF%SDVAR
	SHELN=PlantF%SHELN
	SHVAR=PlantF%SHVAR
	STMWT=PlantF%STMWT
	SWFAC=PlantF%SWFAC
	TDUMX=PlantF%TDUMX
	TDUMX2=PlantF%TDUMX2
	TURFAC=PlantF%TURFAC
	VSTAGE=PlantF%VSTAGE
	WCRLF=PlantF%WCRLF
	WCRRT=PlantF%WCRRT
	WCRST=PlantF%WCRST
	WNRLF=PlantF%WNRLF
	WNRRT=PlantF%WNRRT
	WNRSH=PlantF%WNRSH
	WNRST=PlantF%WNRST
	WTLF=PlantF%WTLF
	WTSD=PlantF%WTSD
	WTSHE=PlantF%WTSHE
	XPOD=PlantF%XPOD
	NVEG0=PlantF%NVEG0
	NR1=PlantF%NR1
	NR2=PlantF%NR2
	NR5=PlantF%NR5
	NR7=PlantF%NR7
!	write(*,*)'To DEMAND,FRLF=',FRLF
      CALL DEMAND(INTEGR, CONTROL, 
     &  AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  !Input
     &  FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,    !Input
     &  LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  !Input
     &  PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,      !Input
     &  SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,   !Input
     &  TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, !Input
     &  WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,   !Input
     &  WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7,     !Input
     &  AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, F, FNINL,  !Output
     &  FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD,   !Output
     &  GRRAT1, NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT,  !Output
     &  NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM, POTCAR,      !Output
     &  POTLIP, SDGR, TURADD, XFRT, YREND)                !Output
!	write(*,*)'From DEMAND,FRLF=',FRLF
	CALL GET(PlantF)
		PlantF%NMOBR=NMOBR
		PlantF%PHTIM=PHTIM
		PlantF%PNTIM=PNTIM
		PlantF%POTCAR=POTCAR
		PlantF%POTLIP=POTLIP
		PlantF%SDGR=SDGR
		PlantF%TURADD=TURADD
		PlantF%XFRT=XFRT
		PlantF%YREND=YREND	
		PlantF%AGRSD1=AGRSD1
		PlantF%AGRSD2=AGRSD2
		PlantF%AGRVG=AGRVG
		PlantF%AGRVG2=AGRVG2
		PlantF%CDMREP=CDMREP
		PlantF%F=F
		PlantF%FNINL=FNINL
		PlantF%FNINR=FNINR
		PlantF%FNINS=FNINS
		PlantF%FNINSD=FNINSD
		PlantF%FRLF=FRLF
		PlantF%FRRT=FRRT
		PlantF%FRSTM=FRSTM
		PlantF%GDMSD=GDMSD
		PlantF%GRRAT1=GRRAT1
		PlantF%NDMNEW=NDMNEW
		PlantF%NDMOLD=NDMOLD
		PlantF%NDMREP=NDMREP
		PlantF%NDMSDR=NDMSDR
		PlantF%NDMTOT=NDMTOT
		PlantF%NDMVEG=NDMVEG
		PlantF%NMINEP=NMINEP
		CALL PUT(PlantF)
      IF (YRDOY == YREND) RETURN

C-----------------------------------------------------------------------
C    Compute N Available From Seed, During Early Growth
!     chp - this takes much longer than 7 ptd to deplete seed N.
C-----------------------------------------------------------------------
	
	SDNPL=PlantF%SDNPL
!	write(*,*)'SDNPL=',SDNPL
	!STOP
	NAVL=PlantF%NAVL
!	write(*,*)'CROPGROW,SDNPL=',SDNPL,'NAVL=',NAVL
      IF (SDNPL .GT. 0.0001) THEN
        NAVL = MAX(SDNPL * DTX / 7. , 0.0)
        SDNPL = SDNPL - NAVL
      ELSE
        SDNPL = 0.0
        NAVL = 0.0
      ENDIF
!	write(*,*)'CROPGROW:SDNPL=',SDNPL,'DTX=',DTX,'NAVL=',NAVL
	
       PlantF%NAVL=NAVL	
	   PlantF%SDNPL=SDNPL
		CALL PUT(PlantF)
C-----------------------------------------------------------------------
C    If ISWNIT = Y - Call soil N routines. Balance Available C and N
C    If ISWNIT = N - Do not call soil N routines, N assumed to be limited by C
C-----------------------------------------------------------------------
      IF (ISWNIT .EQ. 'Y') THEN
		NDMSDR=PlantF%NDMSDR
		NDMTOT=PlantF%NDMTOT
		RLV=PlantF%RLV
		TRNH4U=PlantF%TRNH4U
		TRNO3U=PlantF%TRNO3U
		TRNU=PlantF%TRNU
        CALL NUPTAK(INTEGR, 
     &    DLAYR, DUL, FILECC, KG2PPM, LL, NDMSDR, NDMTOT, !Input
     &    NH4, NO3, NLAYR, RLV, SAT, SW,                  !Input
     &    TRNH4U, TRNO3U, TRNU, UNH4, UNO3)               !Output
!	write(*,*)'CROPGROW FROM NUPTAK'!
!	write(*,*)'NH4=',NH4,'NO3=',NO3
!	write(*,*)'Plant Uptake,UNH4=',UNH4,'UNO3=',UNO3
!	write(*,*)'TRNH4U=',TRNH4U,'TRNO3U=',TRNO3U
	CALL GET(PlantF)
	PlantF%TRNH4U=TRNH4U
	PlantF%TRNO3U=TRNO3U
	PlantF%TRNU=TRNU
	PlantF%UNH4=UNH4
	PlantF%UNO3=UNO3	
C-----------------------------------------------------------------------
C    Account for C Used to reduce N Uptake to protein
C-----------------------------------------------------------------------
	RNO3C=PlantF%RNO3C
	RNH4C=PlantF%RNH4C
        RSPNO3 = TRNO3U/0.16 * RNO3C
        RSPNH4 = TRNH4U/0.16 * RNH4C
        IF (PGAVL .LT. (RSPNO3+RSPNH4)) THEN
          PGAVL = 0.0
        ELSE
          PGAVL = PGAVL - (RSPNO3 + RSPNH4)
        ENDIF
C-----------------------------------------------------------------------
C       Accumulate nitrogen for today's growth, NAVL
C-----------------------------------------------------------------------
        NAVL = NAVL + TRNU
	PlantF%PGAVL=PGAVL
	PlantF%NAVL=NAVL
	CALL PUT(PlantF)
	IF(NAVL==0)THEN
			
	!	write(*,*)'CROPGROW,NAVL=',NAVL
		!STOP
	END IF
      ENDIF
C-----------------------------------------------------------------------
C    CALL Nitrogen mobilization subroutine
C    to compute availability of N from other tissue (NMINEA)
C-----------------------------------------------------------------------
	NDMNEW=PlantF%NDMNEW
	NMINEP=PlantF%NMINEP
	NMOBR=PlantF%NMOBR
	RPRO=PlantF%RPRO
	TRNU=PlantF%TRNU
	WNRLF=PlantF%WNRLF
	WNRRT=PlantF%WNRRT
	WNRSH=PlantF%WNRSH
	WNRST=PlantF%WNRST
      CALL MOBIL(INTEGR,
     &    NDMNEW, NMINEP, NMOBR, RPRO, TRNU,              !Input
     &    WNRLF, WNRRT, WNRSH, WNRST,                     !Input
     &    NMINEA, NRUSLF, NRUSRT, NRUSSH, NRUSST)         !Output
	CALL GET(PlantF)
	PlantF%NMINEA=NMINEA
	PlantF%NRUSLF=NRUSLF
	PlantF%NRUSRT=NRUSRT
	PlantF%NRUSSH=NRUSSH
	PlantF%NRUSST=NRUSST
	CALL PUT(PlantF)
C-----------------------------------------------------------------------
!       Plant phosphorus module
C-----------------------------------------------------------------------
      IF (ISWPHO .EQ. 'Y' .OR. ISWPHO .EQ. 'H') THEN
		MDATE=PlantF%MDATE
		PCNVEG=PlantF%PCNVEG
		PLTPOP=PLantF%PLTPOP
		RLV=PlantF%RLV
		RootMob=PlantF%RootMob
		RTDEP=PlantF%RTDEP
		RTWT=PlantF%RTWT
		SDWT=PlantF%SDWT
		SeedFrac=PlantF%SeedFrac
		ShelMob=PlantF%ShelMob
		SHELWT=PlantF%SHELWT
		ShutMob=PlantF%ShutMob
		STMWT=PlantF%STMWT
		SWIDOT=PlantF%SWIDOT
		VegFrac=PlantF%VegFrac
		WLIDOT=PlantF%WLIDOT
		WRIDOT=PlantF%WRIDOT
		WSHIDT=PlantF%WSHIDT
		WSIDOT=PlantF%WSIDOT
		WTLF=PlantF%WTLF
		SENESCE=PlantF%SENESCE	




        CALL P_CGRO (DYNAMIC, ISWITCH, 
     &    CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,       !Input
     &    RootMob, RTDEP, RTWT, SDWT, SeedFrac,           !Input
     &    ShelMob, SHELWT, ShutMob, SOILPROP,             !Input
     &    SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,      !Input
     &    WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,            !Input
     &    SENESCE,                                        !I/O
     &    PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &    PStres1, PStres2, PUptake, FracRts)             !Output
	CALL GET(PlantF)
		PlantF%SENESCE=SENESCE
		PlantF%PConc_Shut=PConc_Shut
		PlantF%PConc_Root=PConc_Root
		PlantF%PConc_Shel=PConc_Shel
		PlantF%PConc_Seed=PConc_Seed
		PlantF%PStres1=PStres1
		PlantF%PStres2=PStres2
		PlantF%PUptake=PUptake
		PlantF%FracRts=FracRts
	CALL PUT(PlantF)	
      ENDIF

C-----------------------------------------------------------------------
C    Accumulate NAVL for growth, reduce PGAVL by protein re-synthesis cost
C-----------------------------------------------------------------------
      IF (PGAVL .GT. NMINEA/0.16*RPRO) THEN
         PGAVL = PGAVL - NMINEA/0.16*RPRO
      ELSE
         PGAVL = 0.0
      ENDIF
      NAVL   = NAVL + NMINEA
	PlantF%PGAVL=PGAVL
	PlantF%NAVL=NAVL
	CALL PUT(PlantF)
C-----------------------------------------------------------------------
C     Allow some of today's PG to be used for N fixation, depending
C     on N uptake and mining, and on demand for N.
C     NAVLV = N available for veg growth from uptake and mining
C     CAVVEG = C available for veg growth
C     NDMVEG = N required for veg growth if all PGAVL is used as computed
C     CNDFX = carbon needed to fix N needed but not supplied by uptake or mining
C     PROVEG = average protein composition of growing tissue today
C     CTONOD = C to allocate to nodules to fix N needed for Rep and Veg growth
C-----------------------------------------------------------------------
	RFIXN=PlantF%RFIXN
	NDMREP=PlantF%NDMREP
	CTONODR=PlantF%CTONODR
	CDMREP=PlantF%CDMREP
	PROLFI=PlantF%PROLFI
	FRLF=PlantF%FRLF
	PRORTI=PlantF%PRORTI
	FRRT=PlantF%FRRT
	PROSTI=PlantF%PROSTI
	FRSTM=PlantF%FRSTM
	AGRVG=PlantF%AGRVG
	PROVEG=PlantF%PROVEG
      CTONODR = MAX(0.0, (NDMREP-NAVL)*RFIXN/0.16)
      CTONODR = MIN(CTONODR,PGAVL)
      CTONOD = 0.0
      CAVVEG = MAX(0.,(PGAVL - CDMREP))
      NAVLV = MAX(0.,(NAVL-NDMREP))
      CNDFX = MAX(0.,(RFIXN/0.16)*(NDMVEG-NAVLV))
      IF(CAVVEG .GT. 1.E-4 .AND. CNDFX .GT. 1.E-4) THEN
         PROVEG = PROLFI * FRLF + PRORTI * FRRT + PROSTI * FRSTM
         CTONOD = CAVVEG - (CAVVEG +
     &            (NAVLV*RFIXN/0.16))*AGRVG/(AGRVG+PROVEG*RFIXN)
      ENDIF

C-----------------------------------------------------------------------
C     Reserve for nodule growth an amount of C equivalent to a fixed
C     fraction (FRCNOD) of C allocated to root growth.  JWH 7/11/95
C-----------------------------------------------------------------------
	FRCNOD=PlantF%FRCNOD
      IF (DAS .LT. NR2) THEN
        CNODMN = CAVVEG * FRRT * FRCNOD
      ELSE
        CNODMN = 0.0
      END IF
      CTONOD = MIN(CNODMN + MAX(0.0, CTONOD), CAVVEG) + CTONODR
	PlantF%CTONOD=CTONOD
	PlantF%CNODMN=CNODMN
	CALL PUT(PlantF)
C-----------------------------------------------------------------------
C     Call nitrogen fixation routine if ISWSYM is set to Y
C     and if thermal time exceeds the lag phase for n-fixation
C-----------------------------------------------------------------------
      IF (ISWNIT .EQ. 'Y' .AND. ISWSYM .EQ. 'Y') THEN
        IF (VSTAGE .GT. TTFIX) THEN
		AGRNOD=PlantF%AGRNOD
		CNODMN=PlantF%CNODMN
		CTONOD=PlantF%CTONOD
		DXR57=PlantF%DXR57
		NR7=PlantF%NR7
		PLTPOP=PlantF%PLTPOP
		TURFAC=PlantF%TURFAC
          CALL NFIX(INTEGR, 
     &    AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,           !Input
     &    FILECC, FILEIO, NLAYR, NR7, PLTPOP,             !Input
     &    SAT, ST, SW, TURFAC,                            !Input
     &    CNOD, DWNOD, DWNODA, NDTH, NFIXN,               !Output
     &    NODGR, WTNFX, SENNOD)                           !Output
	CALL GET(PlantF)
					PlantF%CNOD=CNOD
		PlantF%DWNOD=DWNOD	
		PlantF%DWNODA=DWNODA
		PlantF%NDTH=NDTH	
		PlantF%NFIXN=NFIXN
		PlantF%NODGR=NODGR
		PlantF%WTNFX=WTNFX
		PlantF%SENNOD=SENNOD
	CALL PUT(PlantF)
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
C       If ISWSYM = U, then N-FIXATION is assumed to occur at a rate
C       that carbon will allow, and nodules are not grown explicitely
C-----------------------------------------------------------------------
      IF ((ISWNIT .EQ. 'Y') .AND. (ISWSYM .EQ. 'U') .OR.
     &   (ISWNIT .NE. 'Y')) THEN
        NFIXN = MAX(0.0,NDMREP + NDMVEG - NAVL)
        CNOD = RFIXN * NFIXN/0.16
	PlantF%NFIXN=NFIXN
	PlantF%CNOD=CNOD
	CALL PUT(PlantF)
      ENDIF
C-----------------------------------------------------------------------
C    Accumulate NAVL for growth, reduce PGAVL by cost to fix N
C-----------------------------------------------------------------------
      IF (PGAVL .GT. CNOD) THEN
         PGAVL = PGAVL - CNOD
      ELSE
         PGAVL = 0.0
      ENDIF
      NAVL = NAVL + NFIXN
	PlantF%PGAVL=PGAVL
	PlantF%NAVL=NAVL
	CALL PUT(plantF)
C-----------------------------------------------------------------------
C     Call routine to compute actual seed and shell growth
C-----------------------------------------------------------------------

		AGRSH1=PlantF%AGRSH1
		FNINSH=PlantF%FNINSH
		NAVL=PlantF%NAVL
		NDSET=PlantF%NDSET
		NRUSSH=PlantF%NRUSSH
		PHTHRS=PlantF%PHTHRS
		RNITP=PlantF%RNITP
		SDDES=PlantF%SDDES
		SDGR=PlantF%SDGR
		XFRT=PlantF%XFRT
		YRNR1=PlantF%YRNR1
		YRNR2=PlantF%YRNR2
		PStres2=PlantF%PStres2

      CALL PODS(INTEGR, 
     &    AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       !Input
     &    FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    !Input
     &    GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, !Input
     &    NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    !Input
     &    PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  !Input
     &    TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, !Input
     &    PStres2, YRPLT,                                 !Input
     &    AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    !Output
     &    PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     !Output
     &    SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     !Output
     &    WTSHE, WTSHMT, FLWN)                            !Output
	CALL GET(PlantF)
		PlantF%AGRSD3=AGRSD3
		PlantF%LAGSD=LAGSD
		PlantF%LNGPEG=LNGPEG
		PlantF%NGRSD=NGRSD
		PlantF%NGRSH=NGRSH
		PlantF%PCTMAT=PCTMAT
		PlantF%PODNO=PODNO
		PlantF%POTCAR=POTCAR
		PlantF%POTLIP=POTLIP
		PlantF%SDNO=SDNO
		PlantF%SDVAR=SDVAR
		PlantF%SEEDNO=SEEDNO
		PlantF%SHELN=SHELN
		PlantF%SHVAR=SHVAR
		PlantF%WSDDTN=WSDDTN
		PlantF%WSHDTN=WSHDTN
		PlantF%WTABRT=WTABRT
		PlantF%WTSD=WTSD
		PlantF%WTSHE=WTSHE
		PlantF%WTSHMT=WTSHMT
		PLantF%FLWN=FLWN
	CALL PUT(PlantF)
C-----------------------------------------------------------------------
C     Call specific routines for peanut to determine
C         Seed size
C         Pod color
C         Pod Detachment
C-----------------------------------------------------------------------
      IF (DETACH .EQ. 'Y' .AND. DAS .GE. NR1) THEN
	  WTLF=PlantF%WTLF
		YRNR2=PlantF%YRNR2
		PODWTD=plantF%POTWTD

        CALL PODDET(
     &    FILECC, TGRO, WTLF, YRDOY, YRNR2,               !Input
     &    PODWTD, SDNO, SHELN, SWIDOT,                    !Output
     &    WSHIDT, WTSD, WTSHE,                            !Output
     &    INTEGR)                                         !Control
	 	CALL GET(PlantF)
			PlantF%SDNO=SDNO
		PlantF%SHELN=SHELN
		PlantF%SWIDOT=SWIDOT
		PlantF%PODWTD=PODWTD
		PlantF%WSHIDT=WSHIDT
			PlantF%WTSD=WTSD
			PlantF%WTSHE=WTSHE
	CALL PUT(PlantF)
			
      ENDIF

C-----------------------------------------------------------------------
C     Compute carbon required for seed (CGRSD) and shell (CGRSH) growth
C-----------------------------------------------------------------------
	WSDDTN=PlantF%WSDDTN
	WSHDTN=PlantF%WSHDTN
	AGRSD3=PlantF%AGRSD3
	AGRSH1=PlantF%AGRSH1
	NAVL=PlantF%NAVL
      CGRSD = WSDDTN * AGRSD3
      CGRSH = WSHDTN * AGRSH1
C-----------------------------------------------------------------------
C     Reduce PGAVL by C used for seed growth
C     Also reduce NAVL by N used for seed growth
C-----------------------------------------------------------------------
      IF (PGAVL .GT. (CGRSD + CGRSH)) THEN
         PGAVL = PGAVL - CGRSD - CGRSH
      ELSE
         PGAVL = 0.0
      ENDIF
      NAVL   = NAVL - (NGRSD + NGRSH)
      PGAVL  = MAX(0.0,PGAVL)
      NAVL   = MAX(0.0,NAVL)
	PlantF%NAVL=NAVL
	PlantF%PGAVL=PGAVL
!	CALL PUT(PlantF)
!-----------------------------------------------------------------------
C     CSAVEV is a faction of PG for vegetative growth that is stored
C     as CH2O.  Increase this as plant moves from R1 into seed fill.
!  These two statements came from the VEGGR subroutine - chp
C-----------------------------------------------------------------------
      CADPR1=PlantF%CADPR1
      FRACDN=PlantF%FRACDN
      CSAVEV = CADPR1 * PGAVL * FRACDN
      PGAVL = PGAVL - CSAVEV
!	write(*,*)'CSAVEV=',CSAVEV
!	write(*,*)'CADPR1=',CADPR1,'PGAVL=',PGAVL,'FRACDN=',FRACDN

       PlantF%NAVL=NAVL
        PlantF%PGAVL=PGAVL
	PlantF%CSAVEV=CSAVEV
        CALL PUT(PlantF)

C-----------------------------------------------------------------------
C     Call routine to compute actual vegetative growth, C to mine or add
C-----------------------------------------------------------------------
		CMINEP=PlantF%CMINEP
		CSAVEV=PlantF%CSAVEV
		DTX=PlantF%DTX
		FNINR=PlantF%FNINR
		FNINS=PlantF%FNINS
		KCAN=PlantF%KCAN
		NAVL=PlantF%NAVL
		NDMNEW=PlantF%NDMNEW
		NDMOLD=PlantF%NDMOLD
		NFIXN=PlantF%NFIXN
		NMINEA=PlantF%NMINEA
		PCH2O=PlantF%PCH2O
		PG=PlantF%PG
		PStres2=PlantF%PStres2
		ROWSPC=PlantF%ROWSPC
		RVSTGE=PlantF%RVSTGE
		STMWT=PlantF%STMWT
		TRNU=PlantF%TRNU
		TURFAC=PlantF%TURFAC
		VSTAGE=PlantF%VSTAGE
		WCRLF=PlantF%WCRLF
		WCRRT=PlantF%WCRRT
		WCRSH=PlantF%WCRSH
		WCRST=PlantF%WCRST
		WTLF=PlantF%WTLF
		XLAI=PlantF%XLAI	
		YREMRG=PlantF%YREMRG
		AGRVG=PlantF%AGRVG
		FRLF=PlantF%FRLF
		FRRT=PlantF%FRRT
		FRSTM=PlantF%FRSTM	
!	Write(*,*)'To VEGGR2,FRLF=',FRLF
      CALL VEGGR(INTEGR, 
     &    AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,      !Input
     &    DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,     !Input
     &    FNINS, KCAN, NAVL, NDMNEW, NDMOLD,              !Input
     &    NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,      !Input
     &    PStres2, ROWSPC, RVSTGE, STMWT, TGRO,           !Input
     &    TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,      !Input
     &    WCRST, WTLF, XLAI, YRDOY, YREMRG,               !Input
     &    AGRVG, FRLF, FRRT, FRSTM,                       !I/O
     &    CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,     !Output
     &    CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,   !Output
     &    NADST, NGRLF, NGRRT, NGRST, NSTRES,             !Output
     &    TNLEAK, WLDOTN, WRDOTN, WSDOTN)                 !Output
	IF(NSTRES==0)THEN
		PLANTING_DSSAT(interface_ihru)=11
		write(*,*)'CROPGROW,NSTRES=',NSTRES,'ihru=',interface_ihru
		write(*,*)'WTNTOT=',PlantF%WTNTOT
		!write(*,*)'----------------------------------------------'
		!write(*,*)'*                                             '
		!write(*,*)'*                                             '
		write(*,*)'*  PUUTE RAVINTEISTA                          '
		write(*,*)'DAS=',DAS,'ihru=',interface_ihru,'CROP=',CROP	
		!STOP
	END IF
	!write(*,*)'From VEGGR2,FRLF=',FRLF
	CALL GET(PlantF)
		PlantF%AGRVG=AGRVG
		PlantF%FRLF=FRLF
		PlantF%FRRT=FRRT
		PlantF%FRSTM=FRSTM
		PlantF%CADLF=CADLF
		PlantF%CADST=CADST
		PlantF%CANHT=CANHT
		PlantF%CANWH=CANWH
		PlantF%CMINEA=CMINEA
		PlantF%CRUSLF=CRUSLF
		PlantF%CRUSRT=CRUSRT
		PlantF%CRUSSH=CRUSSH
		PlantF%CRUSST=CRUSST
		PlantF%EXCESS=EXCESS
		PlantF%NADLF=NADLF
		PlantF%NADRT=NADRT
		PlantF%NADST=NADST
		PlantF%NGRLF=NGRLF
		PlantF%NGRRT=NGRRT
		PlantF%NGRST=NGRST
		PlantF%NSTRES=NSTRES
		PlantF%TNLEAK=TNLEAK
		PlantF%WLDOTN=WLDOTN
		PlantF%WRDOTN=WRDOTN
		PlantF%WSDOTN=WSDOTN
		CALL PUT(PlantF)
	
C-----------------------------------------------------------------------
C     Compute C required for LF, ST, and RT growth, and remaining C and N
C-----------------------------------------------------------------------
	PGAVL=PlantF%PGAVL
	AGRVG=PlantF%AGRVG
	WLDOTN=PlantF%WLDOTN
	WSDOTN=PlantF%WSDOTN
	WRDOTN=PlantF%WRDOTN
	NAVL=PlantF%NAVL
	NGRLF=PlantF%NGRLF
	NGRST=PlantF%NGRST
	NGRRT=PlantF%NGRRT
	NADLF=PlantF%NADLF
	NADST=PlantF%NADST
	NADRT=PlantF%NADRT
	CADST=PlantF%CADST
	CADLF=PlantF%CADLF
	PCH2O=PlantF%PCH2O
      PGAVL = PGAVL - AGRVG * (WLDOTN + WSDOTN + WRDOTN)
      NAVL = NAVL - (NGRLF + NGRST + NGRRT)
      NAVL = NAVL - (NADLF + NADST + NADRT)
      PGAVL = PGAVL - (CADST + CADLF) * PCH2O
	PlantF%NAVL=NAVL
	PlantF%PGAVL=PGAVL
	CALL PUT(PlantF)
C-----------------------------------------------------------------------
C     Call leaf senescence routine to compute leaf loss variables
C-----------------------------------------------------------------------
     	CLW=PlantF%CLW
	DTX=PlantF%DTX
	KCAN=PlantF%KCAN
	NR7=PlantF%NR7
	NRUSLF=PlantF%NRUSLF
	RHOL=PlantF%RHOL
	SLAAD=PlantF%SLAAD
	STMWT=PlantF%STMWT
	SWFAC=PlantF%SWFAC
	VSTAGE=PlantF%VSTAGE
	WTLF=PlantF%WTLF
	XLAI=PlantF%XLAI
	 CALL SENES(INTEGR, 
     &    FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,       !Input
     &    RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI,  !Input
     &    SLDOT, SLNDOT, SSDOT, SSNDOT)                   !Output
	CALL GET(PlantF)
	PlantF%SLDOT=SLDOT
	PlantF%SLNDOT=SLNDOT
	PlantF%SSDOT=SSDOT
	PlantF%SSNDOT=SSNDOT
	CALL PUT(PlantF)
C-----------------------------------------------------------------------
C     Call freeze damage routine if TMIN is less than FREEZ1 deg C
C-----------------------------------------------------------------------
	FREEZ1=PlantF%FREEZ1
	FREEZ2=PlantF%FREEZ2
      IF (TMIN .LT. FREEZ1) THEN
	NOUTDO=PlantF%NOUTDO
	NRUSLF=PlantF%NRUSLF
	SLDOT=PlantF%SLDOT
	WTLF=PlantF%WTLF
	MDATE=PlantF%MDATE
        CALL FREEZE(
     &    FREEZ2, IDETO, NOUTDO, NRUSLF, SLDOT,           !Input
     &    TMIN, WTLF, YRDOY,  YRPLT,                      !Input
     &    MDATE,                                          !Input/Output
     &    WLFDOT)                                         !Output
      ELSE
          WLFDOT = 0.0
      ENDIF
	CALL GET(PlantF)
	PlantF%MDATE=MDATE
	PlantF%WLFDOT=WLFDOT
	CALL PUT(PlantF)
C-----------------------------------------------------------------------
C     Call to root growth and rooting depth routine
!-----------------------------------------------------------------------
	AGRRT=PlantF%AGRRT
	DTX=PlantF%DTX
	FRRT=PlantF%FRRT
	PG=PlantF%PG
	PLTPOP=PlantF%PLTPOP
	RO=PlantF%RO
	RP=PlantF%RP
	RTWT=PlantF%RTWT
	SWFAC=PlantF%SWFAC
	VSTAGE=PlantF%VSTAGE
	WRDOTN=PlantF%WRDOTN
	WTNEW=PlantF%WTNEW
	
      CALL ROOTS(INTEGR,
     &    AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, !Input
     &    ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    !Input
     &    SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      !Input
     &    RLV, RTDEP, SATFAC, SENRT, SRDOT)               !Output

!	write(*,*)'CROPGRO,From ROOTS,SATFAC=',SATFAC
!	write(*,*)'ROOTSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS'
	CALL GET(PlantF)
	PlantF%RLV=RLV
	PlantF%RTDEP=RTDEP
	PlantF%SATFAC=SATFAC
	PlantF%SENRT=SENRT
	PlantF%SRDOT=SRDOT
	CALL PUT(PlantF)
C-----------------------------------------------------------------------
C     Compute total C cost for growing seed, shell, and vegetative tissue
C     for tomorrow's potential growth calculations
C-----------------------------------------------------------------------
C       Calculate the respiration required for seed, shell, and veg tissue
C       depending on the source of N uptake
C-----------------------------------------------------------------------
	TRNU=PlantF%TRNU
	NFIXN=PlantF%NFIXN
	NMINEA=plantF%NMINEA
	RNO3C=PlantF%RNO3
	RNH4C=PlantF%RNH4C
	RPRO=PlantF%RPRO
      IF ((TRNU + NFIXN + NMINEA) .GT. 1.E-4) THEN
         RPROAV = ((RSPNO3 + RSPNH4) * 0.16 + NFIXN * RFIXN + NMINEA
     &        * RPRO) / (TRNU + NFIXN + NMINEA)
      ELSE
         RPROAV = (RNO3C + RNH4C) / 2.
      ENDIF

	RPRO=PlantF%RPRO
	PMINSD=PlantF%PMINSD
	RMIN=PlantF%RMIN
	PLIGSD=PlantF%PLIGSD
	RLIG=PlantF%RLIG
	POASD=PlantF%POASD
	ROA=PlantF%ROA
	!SDLIP=PlantF%SDLIP
	RLIP=PlantF%RLIP
	!PCARSD=PlantF%PCARSD
	RCH2O=PlantF%RCH2O
	SDPROR=PlantF%SDPROR
	FNINSD=PlantF%FNINSD
	POTLIP=PlantF%POTLIP
	RLIP=PlantF%RLIP
	POTCAR=PlantF%POTCAR
	PROSHI=PlantF%PROSHI
	PLIPSH=PlantF%PLIPSH
	POASH=PlantF%POASH
	PMINSH=PlantF%PMINSH
	PCARSH=PlantF%PCARSH
	AGRVG=PlantF%AGRVG
	FRLF=PlantF%FRLF
	PROLFI=PlantF%PROLFI
	FRRT=PlantF%FRRT
	PRORTI=PlantF%PRORTI
	FRSTM=PlantF%FRSTM
	PROSTI=PlantF%PROSTI
C-----------------------------------------------------------------------
C     AGRSD2 = SDPRO*RPROAV + PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA
C    &         + (SDLIP*RLIP + PCARSD*RCH2O)*(1. - SDPROR)
C-----------------------------------------------------------------------
      AGRSD2 = FNINSD*6.25*RPROAV + PMINSD*RMIN + PLIGSD*RLIG
     &        + POASD*ROA   +  POTLIP*RLIP + POTCAR*RCH2O
      AGRSH2=  PROSHI*RPROAV + PLIPSH*RLIP + PLIGSH*RLIG + POASH*ROA
     &         + PMINSH*RMIN + PCARSH*RCH2O
      AGRVG2 = AGRVG + (FRLF*PROLFI+FRRT*PRORTI+FRSTM*PROSTI)*RPROAV

	PlantF%AGRSD2=AGRSD2
	PlantF%AGRSH2=AGRSH2
	PlantF%AGRVG2=AGRVG2
	CALL PUT(PlantF)
C-----------------------------------------------------------------------
C     Call routine to integrate growth and damage
C-----------------------------------------------------------------------
!	write(*,*)'CROPGROW,INTEG,BEF,AREALF',AREALF
	!CALL GET(PlantF)	
!	write(*,*)'CROPGROW,PlantG%AREALF=',PlantF%AREALF
		AGEFAC=PlantF%AGEFAC
		CADLF=PlantF%CADLF
		CADST=PlantF%CADST
		CRUSLF=PlantF%CRUSLF
		CRUSRT=PlantF%CRUSRT
		CRUSSH=PlantF%CRUSSH
		CRUSST=PlantF%CRUSST
		DISLA=PlantF%DISLA
		F=PlantF%F
		FRLF=PlantF%FRLF
		FRSTM=PlantF%FRSTM
		NADLF=PlantF%NADLF
		NADRT=PlantF%NADRT
		NADST=PlantF%NADST
		NDTH=PlantF%NDTH
		NFIXN=PlantF%NFIXN
		NGRLF=PlantF%NGRLF
		NGRRT=PlantF%NGRRT
		NGRSD=PlantF%NGRSD
		NGRSH=PlantF%NGRSH
		NGRST=PlantF%NGRST
		NMINEA=PlantF%NMINEA
		NODGR=PlantF%NODGR
		NOUTDO=PlantF%NOUTDO
		NPLTD=PlantF%NPLTD
		NRUSLF=PlantF%NRUSLF
		NRUSRT=PlantF%NRUSRT
		NRUSSH=PlantF%NRUSSH
		NRUSST=PlantF%NRUSST
		POTCAR=PlantF%POTCAR
		POTLIP=PlantF%POTLIP
		PPLTD=PlantF%PPLTD
		SDIDOT=PlantF%SDIDOT
		SDPROR=PlantF%SDPROR
		SENNOD=PlantF%SENNOD
		SENRT=PlantF%SENRT
		SLDOT=PlantF%SLDOT
		SLNDOT=PlantF%SLNDOT
		SRDOT=PlantF%SRDOT
		SSDOT=PlantF%SSDOT
		SSNDOT=PlantF%SSNDOT
		TRNH4U=PlantF%TRNH4U
		TRNO3U=PlantF%TRNO3U
		TRNU=PlantF%TRNU
		TURFAC=PlantF%TURFAC
		WLDOTN=PlantF%WLDOTN
		WLIDOT=PlantF%WLIDOT
		WRDOTN=PlantF%WRDOTN
		WRIDOT=PlantF%WRIDOT
		WSDDTN=PlantF%WSDDTN
		WSDOTN=PlantF%WSDOTN
		WSHDTN=PlantF%WSHDTN
		WSIDOT=PlantF%WSIDOT
		WTABRT=PlantF%WTABRT
		WTSHMT=PlantF%WTSHMT
		YRNR1=PlantF%YRNR1
		MDATE=PlantF%MDATE
		!YRPLT=PlantF%YRPLT
		SWIDOT=PlantF%SWIDOT
		WLFDOT=PlantF%WLFDOT
		WSHIDT=PlantF%WSHIDT
		WTNFX=PlantF%WTNFX
		XHLAI=PlantF%XHLAI	
      CALL GROW_DSSAT(CONTROL, ISWITCH, INTEGR, SOILPROP, 
     &  AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     !Input
     &  CRUSST, DISLA, F, FILECC, FRLF, FRSTM,            !Input
     &  NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   !Input
     &  NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       !Input
     &  NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,            !Input
     &  POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            !Input
     &  SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       !Input
     &  SSNDOT, TRNH4U, TRNO3U, TRNU,                     !Input
     &  TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   !Input
     &  WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    !Input
     &  MDATE, YRPLT,                                     !Input
     &  SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI,             !Input/Output
     &  AREALF, BETN, CANNAA, CANWAA, CLW, CSW, DWNOD,    !Output
     &  DWNODA, GROWTH, GRWRES, LAIMX, PCCSD, PCLSD,      !Output
     &  PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,         !Output
     &  PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,   !Output
     &  PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,         !Output
     &  ROWSPC, RTWT, SDNPL, SDRATE, SDWT,                !Output
     &  SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,             !Output
     &  SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,  !Output
     &  WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,          !Output
     &  WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,  !Output
     &  WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,       !Output
     &  WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,        !Output
     &  WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,      !Output
     &  WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,      !Output
     &  ShutMob, RootMob, ShelMob)                        !Output
	 
	 CALL GET(PlantF)
	!write(*,*)'CROPGROW.INTEG.WTNTOT=',PlantF%WTNTOT,interface_ihru
	 PlantF%WTSHO=WTSHO
	PlantF%WTNSO=WTNSO
	PlantF%WTNST=WTNST
	PlantF%WTNUP=WTNUP
	PlantF%WTRO=WTRO
	PlantF%WTSDO=WTSDO
	PlantF%WTSHO=WTSHO
	PlantF%WTSO=WTSO
	PlantF%XLAI=XLAI
	PlantF%XPOD=XPOD
	PlantF%ShutMob=ShutMob
	PlantF%RootMob=RootMob
	PlantF%ShelMob=ShelMob

	PlantF%WTNNO=WTNNO
        PlantF%WTNNOD=WTNNOD
        PlantF%WTNOO=WTNOO
        PlantF%WTNRA=WTNRA
        PlantF%WTNRO=WTNRO
        PlantF%WTNRT=WTNRT
        PlantF%WTNSA=WTNSA
        PlantF%WTNSD=WTNSD
        PlantF%WTNSDA=WTNSDA
        PlantF%WTNSDO=WTNSDO
        PlantF%WTNSH=WTNSH
        PlantF%WTNSHA=WTNSHA
        PlantF%WTNSHO=WTNSHO
        PlantF%WTNSO=WTNSO
        PlantF%WTNST=WTNST
        PlantF%WTNUP=WTNUP
        PlantF%WTRO=WTRO
        PlantF%WTSDO=WTSDO


	PlantF%WCRRT=WCRRT
	PlantF%WCRSH=WCRSH
	PlantF%WCRST=WCRST
	PlantF%WNRLF=WNRLF
	PlantF%WNRRT=WNRRT
	PlantF%WNRSH=WNRSH
	PlantF%WNRST=WNRST
	PlantF%WTCO=WTCO
	PlantF%WTLF=WTLF
	PlantF%WTLO=WTLO
	PlantF%WTMAIN=WTMAIN
	PlantF%WTNCAN=WTNCAN
	PlantF%WTNEW=WTNEW
	PlantF%WTNLA=WTNLA
	PlantF%WTNLF=WTNLF
	PlantF%WTNLO=WTNLO
	PlantF%WTNNA=WTNNA
	PlantF%WTNNAG=WTNNAG
	PlantF%ROWSPC=ROWSPC
 	PlantF%RTWT=RTWT
 	PlantF%SDNPL=SDNPL
 	PlantF%SDRATE=SDRATE
 	PlantF%SDWT=SDWT
	PlantF%SEEDNI=SEEDNI
 	PlantF%SEEDNO=SEEDNO
 	PlantF%SENESCE=SENESCE
 	PlantF%SHELWT=SHELWT
 	PlantF%SLA=SLA
 	PlantF%SLAAD=SLAAD
 	PlantF%STMWT=STMWT
	PlantF%TOPWT=TOPWT
 	PlantF%TOTWT=TOTWT
	PlantF%WCRLF=WCRLF



	PlantF%PODWT=PODWT
	PlantF%PUNCSD=PUNCSD
	PlantF%PUNCTR=PUNCTR
	PlantF%RHOL=RHOL
	PlantF%RHOS=RHOS
	PlantF%RNITP=RNITP
	PlantF%PCNSH=PCNSH
	PlantF%PCNST=PCNST
	PlantF%PLTPOP=PLTPOP
	PlantF%PLIGLF=PLIGLF
	PlantF%PLIGNO=PLIGNO
	PlantF%PLIGRT=PLIGRT
	PlantF%PLIGSD=PLIGSD
	PlantF%PLIGSH=PLIGSH
	PlantF%PLIGST=PLIGST

	PlantF%DWNODA=DWNODA
	PlantF%GROWTH=GROWTH
	PlantF%GRWRES=GRWRES
	PlantF%LAIMX=LAIMX	
	PlantF%PCCSD=PCCSD
	PlantF%PCLSD=PCLSD	
	PlantF%PCNL=PCNL
	PlantF%PCNRT=PCNRT
	PlantF%PCNSD=PCNSD
	
	PlantF%SWIDOT=SWIDOT
	PlantF%WLFDOT=WLFDOT
	PlantF%WSHIDT=WSHIDT
	PlantF%WTNFX=WTNFX
	PlantF%XHLAI=XHLAI
	PlantF%AREALF=AREALF
	PlantF%BETN=BETN
	PlantF%CANNAA=CANNAA
	PlantF%CANWAA=CANWAA
	PlantF%CLW=CLW
	PlantF%CSW=CSW
	PlantF%DWNOD=DWNOD
	CALL PUT(PlantF)
	!CALL GET(PlantF)
!	write(*,*)'CROPGROW,AFTER,AREALF=',AREALF
!	write(*,*)'AFTER,PlantG%AREALF=',PlantF%AREALF
	
      IF ((WTLF+STMWT).GT. 0.0001) THEN
        PCNVEG = (WTNLF+WTNST)/(WTLF+STMWT)*100.
      ELSE
        !PCNVEG = -99.    !Wait for GBuild fix for -99's
        PCNVEG = 0.
      ENDIF
	PlantF%PCNVEG=PCNVEG
	CAll PUT(PlantF)
!-----------------------------------------------------------------------
!     End of DAS > NVEG0 if construct
!-----------------------------------------------------------------------
      ENDIF
!-----------------------------------------------------------------------
	CALL PUT(PlantF)
!***********************************************************************
!***********************************************************************
!-----------------------------------------------------------------------
!     OUTPUT / SEASEND section
!-----------------------------------------------------------------------
      ELSE IF (DYNAMIC .EQ. OUTPUT .OR. DYNAMIC .EQ. SEASEND) THEN
!-----------------------------------------------------------------------
      IF (CROP .NE. 'FA') THEN
        IF (YRDOY .EQ. YREND .AND. DYNAMIC .EQ. OUTPUT) THEN
          STGDOY(16) = YREND
        ENDIF
		AREALF=PlantF%AREALF
		CLW=PlantF%CLW
		CSW=PlantF%CSW
		LAGSD=PlantF%LAGSD
		LNGPEG=PlantF%LNGPEG
		NR2=PlantF%NR2
		PGAVL=PlantF%PGAVL
		PHTIM=PlantF%PHTIM
		PLTPOP=PlantF%PLTPOP
		RTWT=PlantF%RTWT
		SLA=PlantF%SLA
		SLDOT=PlantF%SLDOT
		SSDOT=PlantF%SSDOT
		STMWT=PlantF%STMWT
		TOPWT=PlantF%TOPWT
		WLFDOT=PlantF%WLFDOT
		WTLF=PlantF%WTLF
		RLV=PlantF%RLV
		SDNO=PlantF%SDNO
		SHELN=PlantF%SHELN
		SWIDOT=PlantF%SWIDOT
		VSTAGE=PlantF%VSTAGE
		WSHIDT=PlantF%WSHIDT
		WTSD=PlantF%WTSD
		WTSHE=PlantF%WTSHE	
        IF (ISWDIS.EQ.'Y') THEN
          CALL PEST(CONTROL, ISWITCH, 
     &      AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,    !Input
     &      PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,      !Input
     &      SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,       !Input
     &      RLV, SDNO, SHELN, SWIDOT,                       !I/O
     &      VSTAGE, WSHIDT, WTSD, WTSHE,                    !I/O
     &      ASMDOT, DISLA, NPLTD, PPLTD,                    !Output
     &      SDDES, WLIDOT, WRIDOT, WSIDOT,SDWT)             !Output
        ENDIF
		PlantF%PGAVL=PGAVL
		PlantF%RLV=RLV
		PlantF%SDNO=SDNO
		PlantF%SHELN=SHELN
		PlantF%SWIDOT=SWIDOT
		PlantF%VSTAGE=VSTAGE
		PlantF%WSHIDT=WSHIDT
		PlantF%WTSD=WTSD
		PlantF%WTSHE=WTSHE
		PlantF%ASMDOT=ASMDOT
		PlantF%DISLA=DISLA
		PlantF%NPLTD=NPLTD
		PlantF%PPLTD=PPLTD
		PlantF%SDDES=SDDES
		PlantF%WLIDOT=WLIDOT
		PlantF%WRIDOT=WRIDOT
		PlantF%WSIDOT=WSIDOT
		PlantF%SDWT=SDWT
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
        AGRSH1=PlantF%AGRSH1
		FNINSH=PlantF%FNINSH
		NAVL=PlantF%NAVL
		NDSET=PlantF%NDSET
		NRUSSH=PlantF%NRUSSH
		PHTHRS=PlantF%PHTHRS
		RNITP=PlantF%RNITP
		SDDES=PlantF%SDDES
		SDGR=PlantF%SDGR
		XFRT=PlantF%XFRT
		YRNR1=PlantF%YRNR1
		YRNR2=PlantF%YRNR2
		PStres2=PlantF%PStres2
		CALL PODS(DYNAMIC, 
     &    AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       !Input
     &    FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    !Input
     &    GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, !Input
     &    NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    !Input
     &    PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  !Input
     &    TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, !Input
     &    PStres2, YRPLT,                                 !Input
     &    AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    !Output
     &    PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     !Output
     &    SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     !Output
     &    WTSHE, WTSHMT, FLWN)                            !Output
		PlantF%AGRSD3=AGRSD3
		PlantF%LAGSD=LAGSD
		PlantF%LNGPEG=LNGPEG
		PlantF%NGRSD=NGRSD
		PlantF%NGRSH=NGRSH
		PlantF%PCTMAT=PCTMAT
		PlantF%PODNO=PODNO
		PlantF%POTCAR=POTCAR
		PlantF%POTLIP=POTLIP
		PlantF%SDNO=SDNO
		PlantF%SDVAR=SDVAR
		PlantF%SEEDNO=SEEDNO
		PlantF%SHELN=SHELN
		PlantF%SHVAR=SHVAR
		PlantF%WSDDTN=WSDDTN
		PlantF%WSHDTN=WSHDTN
		PlantF%WTABRT=WTABRT
		PlantF%WTSD=WTSD
		PlantF%WTSHE=WTSHE
		PlantF%WTSHMT=WTSHMT
		PLantF%FLWN=FLWN
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        CALL OPGROW(CONTROL, ISWITCH, SoilProp, 
     &    CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD, GROWTH,  
     &    GRWRES, KSTRES, MAINR, MDATE, NFIXN, NLAYR, NSTRES, 
     &    PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG, 
     &    PODNO, PODWT, PODWTD, PSTRES1, PSTRES2, RHOL, RHOS, 
     &    RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDWT, SEEDNO, 
     &    SENESCE, SLA, STMWT, SWFAC, TGRO, TGROAV, TOPWT, 
     &    TOTWT, TURFAC, VSTAGE, WTLF, WTNCAN, WTNLF, WTNST, 
     &    WTNSD, WTNUP, WTNFX, XLAI, YRPLT) 

        IF (ISWPHO .EQ. 'Y' .OR. ISWPHO .EQ. 'H') THEN
			MDATE=PlantF%MDATE
		PCNVEG=PlantF%PCNVEG
		PLTPOP=PLantF%PLTPOP
		RLV=PlantF%RLV
		RootMob=PlantF%RootMob
		RTDEP=PlantF%RTDEP
		RTWT=PlantF%RTWT
		SDWT=PlantF%SDWT
		SeedFrac=PlantF%SeedFrac
		ShelMob=PlantF%ShelMob
		SHELWT=PlantF%SHELWT
		ShutMob=PlantF%ShutMob
		STMWT=PlantF%STMWT
		SWIDOT=PlantF%SWIDOT
		VegFrac=PlantF%VegFrac
		WLIDOT=PlantF%WLIDOT
		WRIDOT=PlantF%WRIDOT
		WSHIDT=PlantF%WSHIDT
		WSIDOT=PlantF%WSIDOT
		WTLF=PlantF%WTLF
		SENESCE=PlantF%SENESCE	
          CALL P_CGRO (DYNAMIC, ISWITCH, 
     &      CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,       !Input
     &      RootMob, RTDEP, RTWT, SDWT, SeedFrac,           !Input
     &      ShelMob, SHELWT, ShutMob, SOILPROP,             !Input
     &      SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,      !Input
     &      WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,            !Input
     &      SENESCE,                                        !I/O
     &      PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, !Output
     &      PStres1, PStres2, PUptake, FracRts)             !Output
		CALL GET(PlantF)
		PlantF%SENESCE=SENESCE
		PlantF%PConc_Shut=PConc_Shut
		PlantF%PConc_Root=PConc_Root
		PlantF%PConc_Shel=PConc_Shel
		PlantF%PConc_Seed=PConc_Seed
		PlantF%PStres1=PStres1
		PlantF%PStres2=PStres2
		PlantF%PUptake=PUptake
		PlantF%FracRts=FracRts
		CALL PUT(PlantF)	
        ENDIF
      ENDIF

!     Write to Overview.out and summary.out files.
      CALL OPHARV (CONTROL, ISWITCH, 
     &    AGEFAC, CANHT, CANNAA, CANWAA, CROP,            !Input
     &    HARVFRAC, LAIMX, MDATE, NSTRES, PCLSD, PCNSD,   !Input
     &    PODNO, PODWT, PStres1, PStres2, SDRATE, SDWT,   !Input
     &    SEEDNO, STGDOY, SWFAC, TOPWT, TURFAC,           !Input
     &    VSTAGE, WTNCAN, WTNFX, WTNSD, WTNST, WTNUP,     !Input
     &    XLAI, RSTAGE, YREMRG, YRNR1, YRNR3, YRNR5,      !Input
     &    YRNR7, YRPLT,                                   !Input
     &    SDWTAH)                                         !Output

!     Call PlantNBal only for seasonal output.
      IF (DYNAMIC .EQ. SEASEND) THEN
        IF (CROP .NE. 'FA') THEN
          CALL PlantNBal (CONTROL, ISWITCH, 
     &      SEEDNI, TNLEAK, WTNFX, WTNLA, WTNLF, WTNLO,     !Input
     &      WTNNA, WTNNO, WTNNOD, WTNRA, WTNRO, WTNRT,      !Input
     &      WTNSA, WTNSD, WTNSDA, WTNSDO, WTNSH, WTNSHA,    !Input
     &      WTNSHO, WTNSO, WTNST, WTNUP)                    !Input
        ENDIF
!-----------------------------------------------------------------------
!     Calculate harvest residue left in field
        CALL HRes_CGRO(CONTROL,
     &    CROP, DLAYR, DWNOD, HARVFRAC, NLAYR, PConc_Shut,!Input
     &    PConc_Root, PConc_Shel, PConc_Seed, PLIGLF,     !Input
     &    PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST, RLV,    !Input
     &    RTWT, SDWT, SENESCE, SHELWT, STMWT, WTLF,       !Input
     &    WTNLF,WTNNOD, WTNRT, WTNSD, WTNSH, WTNST,       !Input
     &    HARVRES)                                        !Output

        SENESCE % ResWt  = 0.0
        SENESCE % ResLig = 0.0
        SENESCE % ResE   = 0.0
      ENDIF

!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
!     Store plant module data for use in ETPHOT.
      Call PUT('PLANT', 'CANHT',  CANHT)
      Call PUT('PLANT', 'CANWH',  CANWH)
      Call PUT('PLANT', 'DXR57',  DXR57)
      Call PUT('PLANT', 'EXCESS', EXCESS)
      Call PUT('PLANT', 'NR5',    NR5)   
      Call PUT('PLANT', 'PLTPOP', PLTPOP)
      Call PUT('PLANT', 'RNITP',  RNITP) 
      Call PUT('PLANT', 'SLAAD',  SLAAD) 
      Call PUT('PLANT', 'XPOD',   XPOD)
	CALL GET(PlantF)
!	write(*,*)'END OF CROPGROW, PROSTI=',PlantF%PROSTI
      PlantF%LAI=XHLAI
	CALL PUT(PlantF)
		
      RETURN
      END SUBROUTINE CROPGRO
!=======================================================================

!***********************************************************************
!     Variable listing (updated 25 Feb 2004)
!***********************************************************************
! AGEFAC    Relative effect of current leaf N on canopy photosynthesis 
!             (0-1) (fraction)
! AGRLF     Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
! AGRNOD    CH2O requirement for nodule growth (g[CH2O] / g[nodule])
! AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
! AGRSD1    CH2O requirement for seed growth, excluding cost for protein 
!             content (g[CH2O] / g[seed])
! AGRSD2    CH2O requirement for seed growth, including cost for protein 
!             content (g[CH2O] / g[seed])
! AGRSD3    CH2O requirement for seed growth, with reduced N content
!            (g[CH2O] / g[seed])
! AGRSH1    CH2O required for shell growth, excluding cost for protein 
!             content (g[CH2O] / g[shell])
! AGRSH2    CH2O requirement for shell growth, including cost for protein 
!             content (g[CH2O] / g[shell])
! AGRSTM    Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
! AGRVG     Mass of CH2O required for vegetative tissue growth including 
!             stoichiometry and respiration (g[CH2O] / g[tissue])
! AGRVG2    Total mass of CH2O required for vegetative tissue growth
!            (g[CH2O] / g[tissue])
! AREALF    Area of leaves (one side) per unit ground area
!            (cm2[leaf] / m2[ground])
! ASMDOT    Daily assimilative damage (g[CH2O] /m2 / d)
! BETN      Spacing between plants along a row (m / plant)
! BWAH      Weight of by-product not harvested (top weight minus seed 
!             weight) (g/m2)
! CADLF     Mass of CH2O added to leaf reserves after growth
!            (g[CH2O] / m2 / d)
! CADPR1    Maximum fraction of stem growth after flowering that can be 
!             allocated to carbohydrate storage just before a full seed 
!             load is set. (fraction)
! CADST     Mass of CH2O added to stems (g[CH2O] / m2 / d)
! CANHT     Canopy height (m)
! CANNAA    Weight of N in total plant at flowering (g[N] / m2)
! CANWAA    Canopy weight at flowering stage (g[plant] / m2)
! CANWH     Canopy width normal to row (m)
! CAVVEG    C available for vegetative tissue growth (g[CH2O] / m2 / d)
! CDMREP    Total CH2O needed for potential reproductive growth
!            (g[CH2O] / m2 / d)
! CGRSD     Carbon required for seed growth (g[CH2O] / m2 / d)
! CGRSH     Carbon required for shell growth (g[CH2O] / m2 / d)
! CLW       Cumulative leaf growth (g[leaf]/m2)
! CMINEA    Actual carbon mined from vegetative tissue (g[CH2O] / m2 / d)
! CMINEP    Potential CH2O mobilization from storage (g[CH2O] / m2 / d)
! CMOBMX    Maximum C pool mobilization rate (g[CH2O] / m2 / d)
! CNDFX     Carbon needed to fix N needed but not supplied by uptake or 
!             mining (g[CH2O] / m2 / d)
! CNOD      C used in N-Fixation and nodule growth (including respiration 
!             costs) today (g[CH2O] / m2 / d)
! CNODMN    Minimum C reserved for nodule growth (g[CH2O] / m2 / d)
! CO2       Atmospheric carbon dioxide concentration (µmol[CO2] / mol[air])
! CONTROL   Composite variable containing variables related to control 
!             and/or timing of simulation.    See Appendix A. 
! CROP      Crop identification code 
! CRUSLF    C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
! CRUSRT    C mobilized from root tissue in a day (g[CH2O] / m2 / d)
! CRUSSH    C mobilized from shell tissue in a day (g[CH2O] / m2 / d)
! CRUSST    C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
! CSAVEV    Fraction of PG for VEG that is stored as CH2O 
! CSW       Cumulative stem growth (g[stem]/m2)
! CTONOD    C to allocate to nodules to fix N needed for reproductive and 
!             vegetative growth (g[CH2O] / m2 / d)
! CTONODR   CH2O allocated to nodules for fixing N needed for reproductive 
!             growth (g[CH2O] / m2 / d)
! DAS       Days after start of simulation (d)
! DAYL      Day length on day of simulation (from sunrise to sunset) (hr)
! DETACH    Switch to determine if pod detachment will be simulated (Y or 
!             N) 
! DISLA     Diseased leaf area (cm2[leaf]/m2[ground]/d)
! DLAYR(L)  Thickness of soil layer L (cm)
! DRPP      Photoperiod days which occur in a real day
!            (photoperiod days / day)
! DS(L)     Cumulative depth in soil layer L (cm)
! DTX       Thermal time that occurs in a real day based on vegetative 
!             development temperature function (thermal days / day)
! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil 
!             layer L (cm3[water]/cm3[soil])
! DWNOD     Current nodule mass (g[nodule] / m2)
! DWNODA    Cumulative nodule growth (g[nodule] / m2)
! DXR57     Relative time between first seed (NR5) and physiological 
!             maturity (NR7) (fraction)
! ECONO     Ecotype code - used to match ECOTYP in .ECO file 
! EOP       Potential plant transpiration rate (mm/d)
! EORATIO   Ratio of increase in potential evapotranspiration with increase 
!             in LAI (up to LAI=6.0) for use with FAO-56 Penman reference 
!             potential evapotranspiration. 
! EP1       Actual plant transpiration rate (cm/d)
! EXCESS    Factor based on excess PG used to affect tomorrow's PG 
!             calculation 
! F         Specific leaf area of new leaf tissue growth, including N
!            (cm2[leaf] / g[leaf])
! FILECC    Path plus filename for species file (*.spe) 
! FILEGC    Pathname plus filename for ECO file 
! FILEIO    Filename for input file (e.g., IBSNAT35.INP) 
! FLWN(J)   Number of flowers added for cohort J (# / m2)
! FNINL     Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
! FNINR     Maximum fraction of N for growing root tissue (g[N] / g[root])
! FNINS     Maximum fraction of N for growing stem tissue (g[N] / g[stem])
! FNINSD    Maximum fraction of N for growing seed tissue based on 
!             temperature (g[N] / g[seed])
! FNINSH    Maximum fraction of N for growing shell tissue
!            (g[N] / g[shell])
! FRACDN    Relative time between flowering (NR1) and last leaf appearance 
!             (NDLEAF) 
! FRCNOD    Fraction of new root dry matter allocation that is diverted to 
!             nodule growth 
! FREEZ1    Temperature below which plant loses all leaves, but development 
!             continues (°C)
! FREEZ2    Temperature below which plant growth stops completely. (°C)
! FRLF      Fraction of vegetative tissue growth that goes to leaves on a 
!             day (g[leaf] / g[veg])
! FRRT      Fraction of vegetative tissue growth that goes to roots on a 
!             day (g[root] / g[veg])
! FRSTM     Fraction of vegetative tissue growth that goes to stems on a 
!             day (g[stem] / g[veg])
! GDMSD     Seed growth demand based on temperature and photoperiod
!            (g[seed] / m2 / d)
! GROWTH    Total new growth of plant tissue on a day (g[tissue] / m2 / d)
! GRRAT1    Maximum growth per individual shell (g / shell / d)
! GRWRES    Growth respiration (g[CH2O]/m2-d)
! HARVFRAC  Two-element array containing fractions of (1) yield harvested 
!             and (2) by-product harvested (fraction)
! HARVRES   Composite variable containing harvest residue amounts for total 
!             dry matter, lignin, and N amounts.  Structure of variable is 
!             defined in ModuleDefs.for. 
! IDETO     Switch for printing OVERVIEW.OUT file 
! ISWDIS    Pest damage simulation switch (Y or N) 
! ISWITCH   Composite variable containing switches which control flow of 
!             execution for model.  The structure of the variable 
!             (SwitchType) is defined in ModuleDefs.for. 
! ISWNIT    Nitrogen simulation switch (Y or N) 
! ISWSYM    Nitrogen fixation simulation switch (Y = simulate nodule 
!             growth, N = no nodule growth, U = N-fixation occurs at a rate 
!             that carbon will allow, and nodules are not grown explicitly) 
! ISWWAT    Water simulation control switch (Y or N) 
! KCAN      Canopy light extinction coefficient for daily PAR, for 
!             equidistant plant spacing, modified when in-row and between 
!             row spacing are not equal 
! KEP       Energy extinction coefficient for partitioning EO to EP 
! KSEVAP    Light extinction coefficient used for computation of soil 
!             evaporation 
! KTRANS    Light extinction coefficient used for computation of plant 
!             transpiration 
! LAGSD     Time required between shell growth and seed growth, per cohort
!            (Photo-thermal days)
! LAIMX     Maximum leaf area index this season (m2[leaf] / m2[ground])
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!            (cm3 [water] / cm3 [soil])
! LNGPEG    Time between start of peg (full flower) and shell formation 
!             (for peanuts only).  Defines slow growth period.
!             (Photo-thermal days)
! MAINR     Maintenance respiration (g[CH2O] / m2 / d)
! MDATE     Harvest maturity date (YYYYDDD)
! MEPHO     Method for photosynthesis computation ('C'=Canopy or daily, 
!             'L'=hedgerow or hourly) 
! NADLF     N added to leaf N reserves (g[N] / m2 / d)
! NADRT     N added to root N reserves (g[N] / m2 / d)
! NADST     N added to stem N reserves (g[N] / m2 / d)
! NAVL      Total mass of nitrogen available for growth (g[N] / m2 / d)
! NAVLV     N available for vegetative growth (g[N] / m2 / d)
! NDLEAF    Day when leaf expansion ceased (d)
! NDMNEW    Total N demand for new growth (g[N] / m2 / d)
! NDMOLD    N demand for old tissue (g[N] / m2 / d)
! NDMREP    Total N needed for potential reproductive growth
!            (g[N] / m2 / d)
! NDMSDR    Amount of Mobilized N which can be used for seed growth
!            (g[N] / m2 / d)
! NDMTOT    Total N demand (g[N] / m2 / d)
! NDMVEG    N required for vegetative growth if all PGAVL is used as 
!             computed (g[N] / m2 / d)
! NDSET     Normal time by which a pod load (full number) should be 
!             achieved with no water stress (d)
! NDTH      Nodule death rate (g[nodule] / m2 / d)
! NFIXN     Amount of N fixed during the day (g[N] / m2 / d)
! NGRLF     Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
! NGRRT     Maximum N demand for root growth (g[root N] / m2[ground] / d)
! NGRSD     Rate of N accumulation in new seeds (g[N] / m2 / d)
! NGRSH     Rate of N accumulation in new shells (g[N] / m2 / d)
! NGRST     Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
! NH4(L)    Ammonium N in soil layer L (µg[N] / g[soil])
! NLAYR     Actual number of soil layers 
! NMINEA    Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
! NMINEP    Potential N mobilization from storage (g[N] / m2 / d)
! NMOBR     Stage-dependent potential N mining rate expressed as a fraction 
!             of the maximum rate (NMOBMX) 
! NO3(L)    Nitrate in soil layer L (µg[N] / g[soil])
! NODGR     New nodule growth (g[nod] / m2 / d)
! NOUTDO    Logical unit for OVERVIEW.OUT file 
! NPLTD     Number of plants destroyed (#/m2/d)
! NR1       Day when 50% of plants have at least one flower (d)
! NR2       Day when 50% of plants have one fruit (pod or peg) (d)
! NR5       Day when 50% of plants have pods with beginning seeds (d)
! NR7       Day when 50% of plants first have yellowing or maturing pods
!            (d)
! NRUSLF    N actually mobilized from leaves in a day (g[N]/m2-d)
! NRUSRT    N actually mobilized from roots in a day (g[N]/m2-d)
! NRUSSH    N actually mobilized from shells in a day (g[N]/m2-d)
! NRUSST    N actually mobilized from stems in a day (g[N]/m2-d)
! NSTRES    Nitrogen stress factor (1=no stress, 0=max stress) 
! NVEG0     Day of emergence (d)
! PAR       Daily photosynthetically active radiation or photon flux 
!             density (moles[quanta]/m2-d)
! PCARSH    Proportion of shell tissue that is carbohydrate (fraction)
! PCCSD     Percentage of carbohydrate in seed tissue (100 g[C] / g[seed])
! PCH2O     Respiration loss due to storage/mobilization of CH2O
!            (g[CH2O] / g[CH2O])
! PCLSD     Percentage of lipid in seed tissue (100 g[lipid] / g[seed])
! PCNL      Percentage of N in leaf tissue (100 g[N] / g[leaf])
! PCNRT     Percent N in root tissue (100 g[N] / g[root])
! PCNSD     Percentage of N in seed tissue (100 g[N] / g[seed])
! PCNSH     Percentage of N in shell tissue (100 g[N] / g[shell])
! PCNST     Percent N in stem tissue (100 g[N] / g[stem])
! PCTMAT    Fraction of pods that are mature (seed are 90% of final size) 
! PG        Daily gross photosynthesis (g[CH2O] / m2 - d)
! PGAVL     Total available CH2O available for growth & respiration
!            (g[CH2O] / m2)
! PHTHRS(I) Threshold time that must accumulate in phase I for the next 
!             stage to occur (thermal or photothermal days)
! PHTIM(I)  Cumulative photothermal time ages of seeds and shells 
! PLIGLF    Proportion of leaf tissue that is lignin (fraction)
! PLIGNO    Proportion of nodule tissue that is lignin (fraction)
! PLIGRT    Proportion of root tissue that is lignin (fraction)
! PLIGSD    Proportion of seed tissue that is lignin (fraction)
! PLIGSH    Proportion of shell tissue that is lignin (fraction)
! PLIGST    Proportion of stem tissue that is lignin (fraction)
! PLIPSH    Proportion of shell tissue that is lipid (fraction)
! PLTPOP    Plant population (# plants / m2)
! PMINSD    Proportion of seed tissue that is mineral (fraction)
! PMINSH    Proportion of shell tissue that is mineral (fraction)
! PNTIM(I)  Photothermal days from first flower when flowers in age group I 
!             formed (p-t-d)
! POASD     Proportion of seed tissue that is organic acid (fraction)
! POASH     Proportion of shell tissue that is organic acid (fraction)
! PODNO     Total number of pods (#/m2)
! PODWT     Dry mass of seeds plus shells, including C and N
!            (g[pods] / m2[ground])
! PODWTD    Mass of detached pods (g[pods] / m2[ground])
! PORMIN    Minimum pore space required for supplying oxygen to roots for 
!             optimal growth and function (cm3/cm3)
! POTCAR    Potential carbohydrate composition of seed based on temperature
!            (fraction)
! POTLIP    Potential lipid composition of seed based on temperature
!            (fraction)
! PPLTD     Percent plants destroyed (%/m2/d)
! PROLFI    Maximum protein composition in leaves during growth with 
!             luxurious supply of N (g[protein] / g[leaf tissue])
! PRORTI    Maximum protein composition in roots during growth with 
!             luxurious supply of N (g[protein] / g[root])
! PROSHI    Maximum protein composition in shells during growth with 
!             luxurious supply of N (g[protein] / g[shell tissue])
! PROSTI    Maximum protein composition in stems during growth with 
!             luxurious supply of N (g[protein] / g[stem])
! PROVEG    Average protein composition of growing tissue today
!            (g[protein] / g[veget. tissue])
! PUNCSD    Cumulative puncture damage to seed (not yet implemented) 
! PUNCTR    Cumulative puncture damage (not yet implemented) 
! R30C2     Respiration coefficient that depends on total plant mass, value 
!             at 30C (g[CH2O] used / g[CH2O] fixed / hr)
! RCH2O     Respiration required for synthesizing CH2O structure
!            (g[CH2O] / g[tissue])
! RES30C    Respiration coefficient that depends on gross photosynthesis, 
!             value at 30C (g CH2O/g DW/hr)
! RFIXN     CH2O required for biological N fixation (g[CH2O] / g[protein])
! RHOL      Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
! RHOS      Fraction of stem which is carbohydrate (g [CH2O] / g[stem])
! RLIG      Respiration required for synthesizing lignin structure
!            (g[CH2O] / g[lignin])
! RLIP      Respiration required for synthesizing lipid structure
!            (g[CH2O] / g[lipid])
! RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
! RMIN      Respiration required for synthesizing mineral structure
!            (g[CH2O] / g[mineral])
! RNH4C     CH2O required for protein synthesis when source of N is 
!             ammonium uptake (g[CH2O] / g[protein])
! RNITP     True nitrogen concentration in leaf tissue for photosynthesis 
!             reduction. (%)
! RNMODE    Simulation run mode (I=Interactive, A=All treatments, B=Batch 
!             mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence) 
! RNO3C     Respiration required for reducing NO3 to protein
!            (g[CH2O] / g[protein])
! RO        Respiration coefficient that depends on total plant mass
!            (g[CH2O] / g[tissue])
! ROA       Respiration required for synthesizing organic acids
!            (g[CH2O] / g[product])
! ROWSPC    Row spacing (m)
! RP        Proportion of the day's photosynthesis which is respired in the 
!             maintenance process 
! RPRO      Respiration required for re-synthesizing protein from mobilized 
!             N (g[CH2O] / g[protein])
! RPROAV    Respiration required for protein synthesis, average based on 
!             sources of N (g[CH2O] / g[protein])
! RSPNH4    Respiration required for reducing NH4 to protein
!            (g[CH2O] / m2 / d)
! RSPNO3    Respiration required for reducing NO3 to protein
!            (g[CH2O] / m2 / d)
! RSTAGE    Number of RSTAGES which have occurred. 
! RTDEP     Root depth (cm)
! RTWT      Dry mass of root tissue, including C and N
!            (g[root] / m2[ground])
! RUN       Change in date between two observations for linear 
!             interpolation 
! RVSTGE    Rate of VSTAGE change (nodes/day)
! RWUEP1    Threshold for reducing leaf expansion compared w/ ratio of 
!             TRWU/EP1 (total potential daily root water uptake/ actual 
!             transpiration) 
! RWUMX     Maximum water uptake per unit root length, constrained by soil 
!             water (cm3[water] / cm [root])
! SAT(L)    Volumetric soil water content in layer L at saturation
!            (cm3 [water] / cm3 [soil])
! SATFAC    Root length weighted soil water excess stress factor ( 0 = no 
!             stress; 1 = saturated stress ) 
! SDDES(J)  Number of seeds destroyed today in cohort J when shells are not 
!             destroyed (#/m2/d)
! SDGR      Potential growth rate per seed (g / seed / d)
! SDIDOT    Number of seeds destroyed on the current day (#/m2/d)
! SDNO(J)   Number of seeds for cohort J (#/m2)
! SDNPL     Seed N (g[N] / m2)
! SDPROR    Ratio to adjust lipid and carbohydrate proportions when seed 
!             protein differs from protein composition of standard cultivar 
!             (SDPROS) 
! SDRATE    Seeding rate, mass of seed sown (g[seed] / m2[ground])
! SDVAR     Maximum cultivar-dependent seed growth rate, per seed
!            (g / seed / d)
! SDWT      Dry mass of seed tissue, including C and N
!            (g[seed] / m2[ground])
! SDWTAH    Actual seed weight harvested (g[seed] / m2[ground])
! SEEDNI    Seed or transplant N at planting (g[N] / m2)
! SEEDNO    Total number of seeds (#/m2)
! SENESCE   Composite variable containing data about daily senesced plant 
!             matter. Structure of variable is defined in ModuleDefs.for 
! SENNOD(L) Daily senesced matter from nodules in soil layer L
!            (kg[dry matter]/ha)
! SENRT(L)  Daily senesced matter from roots in soil layer L
!            (kg[dry matter]/ha)
! SHELN(J)  Number of shells for cohort J (#/m2)
! SHELWT    Total mass of all shells (g / m2)
! SHVAR     Shell growth rate during its rapid growth phase, per shell
!            (g / shell / d)
! SLA       Specific leaf area (cm2[leaf] / m2[ground])
! SLAAD     Specific leaf area, excluding weight of C stored in leaves
!            (cm2[leaf] / g[leaf])
! SLDOT     Defoliation due to daily leaf senescence (g/m2/day)
! SLNDOT    Leaf senescence due to water stress (g/m2/day)
! SLPF      Soil photosynthesis factor, 0 to 1 scale 
! SOILPROP  Composite variable containing soil properties including bulk 
!             density, drained upper limit, lower limit, pH, saturation 
!             water content.  Structure defined in ModuleDefs. 
! SRDOT     Daily root senescence (g / m2 / d)
! SSDOT     Daily senescence of petioles (g / m2 / d)
! SSNDOT    Petiole senescence due to water stress (g/m2/day)
! ST(L)     Soil temperature in soil layer L (°C)
! STGDOY(I) Day when plant stage I occurred (YYYYDDD)
! STMWT     Dry mass of stem tissue, including C and N
!            (g[stem] / m2[ground)
! SW(L)     Volumetric soil water content in layer L
!            (cm3 [water] / cm3 [soil])
! SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress, 
!             0.0=max stress 
! SWIDOT    Daily seed mass damage (g/m2/day)
! TAVG      Average daily temperature (°C)
! TDAY      Average temperature during daylight hours (°C)
! TDUMX     Photo-thermal time that occurs in a real day based on early 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TDUMX2    Photo-thermal time that occurs in a real day based on late 
!             reproductive development temperature function
!             (photo-thermal days / day)
! TGRO(I)   Hourly canopy temperature (°C)
! TGROAV    Average daily canopy temperature (°C)
! TMIN      Minimum daily temperature (°C)
! TNLEAK    Total nitrogen leak (g[N] / m2 / d)
! TOPWT     Total weight of above-ground portion of crop, including pods
!            (g[tissue] / m2)
! TOTWT     Total weight of crop (g[tissue] / m2)
! TRNH4U    Total N uptake in ammonium form in a day (kg[N] / ha / d)
! TRNO3U    Total N uptake in nitrate form in a day (kg[N] / ha / d)
! TRNU      Total N uptake in a day (kg[N] / ha / d)
! TRWUP     Potential daily root water uptake over soil profile (cm/d)
! TTFIX     Physiological days delay in nodule initiation
!            (photo-thermal days / day)
! TURADD    Water stress factor (TURFAC) effect on reproductive growth and 
!             pod addition.  Stress is defined to INCREASE growth and 
!             addition. 
! TURFAC    Water stress factor for expansion (0 - 1) 
! UNH4(L)   Rate of root uptake of NH4, computed in NUPTAK
!            (kg [N] / ha - d)
! UNO3(L)   Rate of root uptake of NO3, computed in NUPTAK (kg [N] / ha -d)
! VSTAGE    Number of nodes on main stem of plant (nodes)
! WCRLF     Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
! WCRRT     Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
! WCRSH     Mass of CH2O reserves in shells (g[shell CH2O] / m2[ground])
! WCRST     Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
! WLDOTN    Dry weight growth rate of new leaf tissue including N but not C 
!             reserves (g[leaf] / m2[ground]-d)
! WLFDOT    Leaf weight losses due to freezing (g[leaf]/m2-d)
! WLIDOT    Daily pest or freeze damage to leaf mass (g/m2/day)
! WNRLF     N available for mobilization from leaves above lower limit of 
!             mining (g[N] / m2)
! WNRRT     N available for mobilization from roots above lower limit of 
!             mining (g[N] / m2)
! WNRSH     N available for mobilization from shells above lower limit of 
!             mining (g[N] / m2)
! WNRST     N available for mobilization from stems above lower limit of 
!             mining (g[N] / m2)
! WR(L)     Root hospitality factor, used to compute root distribution 
! WRDOTN    Dry weight growth rate of new root tissue including N but not C 
!             reserves (g[root] / m2[ground]-d)
! WRIDOT    Daily pest damage to root mass (g/m2/day)
! WSDDTN    New seed growth today (g[seed] / m2 / d)
! WSDOTN    Dry weight growth rate of new stem tissue including N but not C 
!             reserves (g[stem] / m2[ground]-d)
! WSHDTN    New shell growth today (g[shell] / m2 / d)
! WSHIDT    Weight of shell tissue consumed by pests today (g[shell]/m2-d)
! WSIDOT    Daily pest damage to stem mass (g/m2/day)
! WTABRT    Weight of shells aborted on a day (g[shell] / m2 / d)
! WTCO      Cumulative losses of plant tissue (g[tissue] / m2)
! WTLF      Dry mass of leaf tissue including C and N
!            (g[leaf] / m2[ground])
! WTLO      Cumulative leaf losses (g[leaf] / m2)
! WTMAIN    Mass of tissue assumed to require maintenance (g[tissue] / m2)
! WTNCAN    Mass of N in canopy (g[N] / m2[ground])
! WTNEW     Initial mass of seedling or seed (g / plant)
! WTNFX     Cumulative weight of N fixed (g[N] / m2)
! WTNLA     Cumulative N added to leaves (g[N]/m2-d)
! WTNLF     Mass of N in leaves (g[leaf N] / m2[ground])
! WTNLO     Cumulative N loss from leaves (g[N] / m2)
! WTNNA     Cumulative N added to nodules (g[N]/m2-d)
! WTNNAG    Total accumulated N used in nodule growth (g[N] / m2)
! WTNNO     Cumulative N loss from nodules (g[N] / m2)
! WTNNOD    Mass of N in nodules (g[N] / m2[ground])
! WTNOO     Cumulative nodule losses (g[nodule] / m2)
! WTNRA     Cumulative N added to roots (g[N]/m2-d)
! WTNRO     Cumulative N loss from roots (g[N] / m2)
! WTNRT     Mass of N in roots (g[root N] / m2[ground])
! WTNSA     Cumulative N added to stems (g[N]/m2-d)
! WTNSD     Mass of N in seeds (g[N] / m2[ground])
! WTNSDA    Cumulative N added to seeds (g[N]/m2-d)
! WTNSDO    Cumulative N loss from seeds (g[N] / m2)
! WTNSH     Mass of N in shells (g[N] / m2[ground])
! WTNSHA    Cumulative N added to shells (g[N]/m2-d)
! WTNSHO    Cumulative N loss from shells (g[N] / m2)
! WTNSO     Cumulative N loss from stems (g[N] / m2)
! WTNST     Mass of N in stems (g[stem N] / m2[ground])
! WTNUP     Cumulative N uptake (g[N] / m2)
! WTRO      Cumulative root losses (g[root] / m2)
! WTSD(J)   Seed mass  for cohort J (g/m2)
! WTSDO     Cumulative seed losses (g[seed] / m2)
! WTSHE(J)  Shell mass  for cohort J (g/m2)
! WTSHMT    Cohorts that reach THRESH today (g/m2)
! WTSHO     Cumulative shell losses (g[shell] / m2)
! WTSO      Cumulative stem losses (g[stem] / m2)
! XFRT      Current day's partitioning to reproductive growth (0-1)
!            (g[fruit] / g[plant])
! XHLAI     Healthy leaf area index (m2[leaf] / m2[ground])
! XLAI      Leaf area (one side) per unit of ground area
!            (m2[leaf] / m2[ground])
! XPOD      Growth partitioning to pods which slows node appearance
!            (fraction)
! YRDOY     Current day of simulation (YYYYDDD)
! YREMRG    Day of emergence (YYYYDDD)
! YREND     Date for end of season (usually harvest date) (YYYYDDD)
! YRNR1     Day when 50% of plants have at least one flower (YYYYDDD)
! YRNR2     Day when 50% of plants have one fruit (pod or peg) (YYYYDDD)
! YRNR3     Day when 50% of plants have at least one beginning pod
!            (YYYYDDD)
! YRNR5     Day when 50% of plants have pods with beginning seeds (YYYYDDD)
! YRNR7     Day when 50% of plants first have yellowing or maturing pods
!            (YYYYDDD)
! YRPLT     Planting date (YYYYDDD)
!***********************************************************************
!      END SUBROUTINE CROPGRO
!=======================================================================



