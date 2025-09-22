# Purpose:
#	Compiles SWAT with gfortran
#
# Author: Andre Dozier and Olaf David
#   Date: May 10, 2013
#

.SUFFIXES:
.SUFFIXES: .f .f90 .f03 .o .mod .for .for.in

SYS := $(shell gcc -dumpmachine)
ifneq (, $(findstring mingw, $(SYS)))
OS := win
endif

# if not release, set the debug flag
ifneq ($(rel),true)
deb := -g
endif

ifeq ($(OS),win)

# windows
RM = del /F
SWAT.OUT = swat.exe
SWAT.DLL = libswat.dll
EVENT.DIR = ..\event\\
EVENT.DLL = libevent.dll
LIB.DIR = ..\..\lib\\
BIN.DIR = ..\..\bin\\
FFLAGS = -fno-underscoring $(deb)
CP = copy /y

else

# linux 
RM = rm -f
SWAT.OUT = swat
SWAT.DLL = libswat.so
EVENT.DIR := ../event/
LIB.DIR = lib/
BIN.DIR = bin/
RUN.DIR=/modeller3/WATDEV/TOOLBOX/EGYPT/NEW2/TxtInOut/
RUN2.DIR=/modeller3/WATDEV/TOOLBOX/KOGA/NEW2/TxtInOut/
RUN3.DIR=INPUT_SWAT/
EVENT.DLL = libevent.so
FFLAGS = -fno-underscoring -fPIC $(deb) #ORIGINAL
#FFLAGS = -O0 -ggdb -ffpe-trap=invalid,zero,overflow -fbounds-check -fimplicit-none
CP = cp -f

endif
smrt=SMRT
MODFLOW=MODFLOW
rt=RT3D
swat=SWAT
plant=PLANT
utils1=UTILS1
inputDSSAT=INPUT_DSSAT
CSV=UTILS1/CsvOuts
PEST=PEST
PLANTP=PLANTP
PLANT_ALL=PLANT_ALL
SPAM=SPAM
MULCH=MULCH
SOIL=SOIL
CSCERES=CSCER
FORAGE=FORAGE
CANEGRO=CANEGRO
CASUPRO=CASUPRO
ALOHA=ALOHA
SAMUCA=SAMUCA
CSYCA=CSYCA
NWHEAT=NWHEAT
NTEF=NTEF
CMILLET=CMILLET
CMAIZE=CMAIZE
CSUGAR=CSUGAR
POTATO=POTATO
CRICE=CRICE
CSORGHUM=CSORGHUM
AROIDS=AROIDS
CERES_SHARED=CERES_SHARED
DWEATHER=DWEATHER
DSOILWATER=DSOILWATER
IX=IX
SWEET=SWEET
LAND=LAND
MANAGEMENT=MANAGEMENT
CENTURY=CENTURY
SOIL_UTILS=SOIL_UTILS
SOIL_NI=SOIL_NI
SOIL_PI=SOIL_PI
SOIL_KI=SOIL_KI
OM=CERES_ORGANIC_MATTER
GHG=GHG
FLOODN=FLOODN
VPATH= .:$(swat):$(smrt):$(MODFLOW):$(rt):$(plant):$(utils1):$(inputDSSAT):$(CSV):$(PEST):$(PLANTP):$(PLANT_ALL):$(SPAM):$(FORAGE) :$(CANEGRO) :$(CASUPRO) :$(ALOHA) :$(SAMUCA)\
 :$(CSCERES) :$(CSYCA) :$(NWHEAT) :$(NTEF) :$(CMILLET) :$(CMAIZE) :$(CSUGAR) :$(POTATO) :$(CRICE) :$(CSORGHUM) :$(AROIDS) :$(CERES_SHARED) :$(DWEATHER) :$(DSOILWATER) :$(IX) :$(SWEET)\
:$(MULCH) :$(LAND) :$(MANAGEMENT) :$(SOIL) :$(CENTURY) :$(SOIL_UTILS) :$(SOIL_NI) :$(SOIL_PI) :$(SOIL_KI) :$(OM) :$(GHG) :$(FLOODN)
EVENT = $(LIB.DIR)$(EVENT.DLL)
FC = gfortran
FCI=ifort
MODFS = modparm.f smrt_parm.f rt_modparm.f mf_rt_link.f mf_mach_mod.f90 mf_gwf2bas7_NWT.f mf_de47_NWT.f mf_sip7_NWT.f mf_gwf2riv7_NWT.f mf_modules.f90\
 mf_gwfsfrmodule_NWT.f mf_gwfuzfmodule_NWT.f mf_pcg7_NWT.f mf_nogmg.f mf_NWT1_module.f mf_gwf2bcf7.f mf_gwf2lpf7.f mf_gwf2huf7.f mf_gwf2upw1.f mf_gwf2sub7.f\
mf_gwf2drn7_NWT.f mf_obs2bas7.f mf_obs2drn7.f mf_obs2riv7.f mf_gwf2str7.f mf_obs2str7.f mf_gwf2ghb7_NWT.f mf_obs2ghb7.f mf_obs2chd7.f mf_gwf2swr7_NWT.f\
mf_gwf2wel7_NWT.f mf_gwf2evt7.f mf_gwf2rch7.f mf_NWT1_gmres.f90 mf_NWT1_xmdlib.f mf_NWT1_xmd.f mf_gwf2fhb7.f mf_gwf2res7.f mf_gwf2ibs7.f mf_gwf2chd7.f \
mf_gwf2hydmod7.f mf_gwf2hfb7_NWT.f mf_gwf2lak7_NWT.f mf_gwf2ets7.f mf_gwf2drt7.f mf_gwf2sfr7_NWT.f mf_gwf2gag7.f mf_gwf2mnw27_NWT.f mf_gwf2mnw2i7.f\
mf_gwf2uzf1_NWT.f mf_gwf2swt7.f mf_gwf2mnw17_NWT.f mf_lmt7_NWT.f mf_NWT1_ilupc_mod.f90\
OSDefsLINUX.for CSMVersion.for ModuleDefs.for OPHEAD.for csvlinklist.f90 csvoutput.f90 YCA_Node.f90 YCA_First_Trans_m.f90 YCA_Albedo_Check_m.f90 WH_module.for\
TF_module.f90 SC_CNG_mods.for SC_SAM_ModuleDefs.f90 Aloha_mod.f90 SLigCeres.for YCA_Formats_m.f90 YCA_Control_Environment.f90 YCA_Growth_VPD.f90 YCA_Model_VPD_Interface.f90 YCA_Control_Photosynthesis.f90\
YCA_Control_Leaf.f90 YCA_Control_Plant.f90 FertType_mod.for N2O_mod.for IPSOIL.for SoilMixing.for SoilPBalSum.for SoilNBalSum.for OPMULCH.for  MULCHWAT.for interface.f90


MODOBJ =modparm.o smrt_parm.o rt_modparm.o mf_rt_link.o mf_mach_mod.o mf_gwf2bas7_NWT.o mf_de47_NWT.o mf_sip7_NWT.o  mf_gwf2riv7_NWT.o mf_modules.o\
 mf_gwfsfrmodule_NWT.o mf_gwfuzfmodule_NWT.o mf_pcg7_NWT.o mf_nogmg.o mf_NWT1_module.o mf_gwf2bcf7.o  mf_gwf2lpf7.o mf_gwf2huf7.o mf_gwf2upw1.o mf_gwf2sub7.o\
mf_gwf2drn7_NWT.o mf_obs2bas7.o mf_obs2drn7.o mf_obs2riv7.o  mf_gwf2str7.o mf_obs2str7.o mf_gwf2ghb7_NWT.o mf_obs2ghb7.o mf_obs2chd7.o mf_gwf2swr7_NWT.o\
mf_gwf2wel7_NWT.o mf_gwf2evt7.o mf_gwf2rch7.o mf_NWT1_gmres.o mf_NWT1_xmdlib.o mf_NWT1_xmd.o mf_gwf2fhb7.o mf_gwf2res7.o mf_gwf2ibs7.o mf_gwf2chd7.o \
mf_gwf2hydmod7.o mf_gwf2hfb7_NWT.o mf_gwf2lak7_NWT.o mf_gwf2ets7.o mf_gwf2drt7.o mf_gwf2sfr7_NWT.o mf_gwf2gag7.o mf_gwf2mnw27_NWT.o mf_gwf2mnw2i7.o\
mf_gwf2uzf1_NWT.o mf_gwf2swt7.o mf_gwf2mnw17_NWT.o  mf_lmt7_NWT.o mf_NWT1_ilupc_mod.o\
OSDefsLINUX.o CSMVersion.o ModuleDefs.o OPHEAD.o csvlinklist.o csvoutput.o YCA_Node.o YCA_First_Trans_m.o YCA_Albedo_Check_m.o WH_module.o\
TF_module.o SC_CNG_mods.o SC_SAM_ModuleDefs.o Aloha_mod.o SLigCeres.o YCA_Formats_m.o YCA_Control_Environment.o YCA_Growth_VPD.o YCA_Model_VPD_Interface.o YCA_Control_Photosynthesis.o \
YCA_Control_Leaf.o YCA_Control_Plant.o FertType_mod.o N2O_mod.o IPSOIL.o SoilMixing.o SoilPBalSum.o SoilNBalSum.o OPMULCH.o MULCHWAT.o interface.o

MODFS2 = smrt_parm.f mf_link.f
MODOBJ2 = $(pathsubst %.f,%.o, $(MODFS2))
#OBJ = $(patsubst %.f,%.o, $(wildcard *.f)) $(patsubst %.f90,%.o, $(wildcard *.f90)) $(patsubst %.f03,%.o, $(wildcard *.f03) $(pathsubst %.f,%.o,$(smrt)$(wildcard *.f)) )
OBJ =rt_openio.o smrt_modifySWATgw.o smrt_allocate.o rt_init.o rt_read.o smrt_init_rt3d.o smrt_read_drain2sub.o smrt_read_irrig.o smrt_read_river2grid.o smrt_read_dhru2hru.o smrt_read_dhru2grid.o smrt_read_grid2dhru.o mf_read.o rtsed_yangsand.o rtsed_Molinas_Wu.o rtsed_kodatie.o rtsed_bagnold.o smrt_init_mf.o smrt_grid2dhru2D.o smrt_dhru2hru.o smrt_grid2dhru3D.o smrt_conversion2swat.o smrt_dhru2grid2D.o rt_stress.o smrt_dhru2rtgrid3D.o smrt_hru2dhru.o rt_vst.o rt_gcg.o rt_ssm.o rt_adv.o rt_implicit.o rt_dsp.o rt_run.o rt_fmi.o smrt_conversion2rt3d.o smrt_rt3d_run.o mf_NWT1_solver.o smrt_units.o smrt_rt3d_stress.o smrt_well.o smrt_evt.o smrt_recharge.o smrt_mfRiver.o mf_run.o smrt_conversion2mf.o smrt_mf_run.o mf_gsol7.o\
 rt_rxns.o rt_rct.o mf_hufutl7.o rt_btn.o rt_close.o mf_MF_NWT.o mf_close.o mf_parutl7.o mf_utl7.o smrt_close.o smrt_read_link.o\
 smrt_main.o addh.o albedo.o allocate_parms.o alph.o anfert.o apex_day.o apply.o ascrv.o atri.o aunif.o autoirr.o bacteria.o biozone.o\
bmp_det_pond.o bmpinit.o bmp_ri_pond.o bmp_sand_filter.o bmp_sed_pond.o bmp_wet_pond.o buffer.o burnop.o canopyint.o caps.o carbon_new.o\
carbon_zhang2.o cfactor.o clgen.o clicon.o command.o conapply.o confert.o crackflow.o crackvol.o curno.o dailycn.o decay.o depstor.o\
 dormant.o drains.o dstn1.o ee.o eiusle.o enrsb.o estimate_ksat.o etact.o etpot.o expo.o fert.o filter.o filtw.o distrib_bmps.o\
finalbal.o gcycl.o getallo.o grass_wway.o graze.o grow.o GROW_DSSAT.o gwmod_deep.o gwmod.o gw_no3.o gwnutr.o h2omgt_init.o harvestop.o harvkillop.o\
header.o headout.o hhnoqual.o hhwatqual.o hmeas.o hruaa.o hruallo.o hruday.o hrumon.o hrupond.o hrupondhr.o hruyr.o hydroinit.o icl.o bmpfixed.o \
impndaa.o impndday.o impnd_init.o impndmon.o impndyr.o irrigate.o irr_rch.o irr_res.o irrsub.o jdt.o killop.o lakeq.o latsed.o layersplit.o \
 lwqdef.o main.o ndenit.o newtillmix.o nfix.o rthr.o NCsed_leach.o \
nitvol.o nlch.o nminrl.o noqual.o npup.o nrain.o nup.o nuts.o openwth.o operatn.o orgncswat.o orgn.o origtile.o ovr_sed.o\
percmacro.o percmain.o percmicro.o pestlch.o pestw.o pesty.o pgen.o pgenhr.o pkq.o plantmod.o plantop.o\
pmeas.o pminrl2.o pminrl.o pond.o pondhr.o pothole.o print_hyd.o psed.o qman.o rchaa.o rchday.o rchinit.o rchmon.o rchuse.o rchyr.o\
readatmodep.o readbsn.o readchm.o readcnst.o readfcst.o readfert.o readfig.o readfile.o readgw.o readhru.o readinpt.o readlup.o readlwq.o\
readmgt.o readmon.o readops.o readpest.o readplant.o readpnd.o readres.o readrte.o readru.o readsdr.o readsepticbz.o readseptwq.o readsno.o\
readsol.o readsub.o readswq.o readtill.o readurban.o readwgn.o readwus.o readwwq.o readyr.o reccnst.o recday.o rechour.o recmon.o recyear.o\
regres.o resetlu.o res.o reshr.o resinit.o resnut.o rewind_init.o rhgen.o rootfr.o route.o routels.o routeunit.o routres.o rsedaa.o\
rseday.o rsedmon.o rsedyr.o rtbact.o rtday.o rteinit.o rthmusk.o rthpest.o rthsed.o  rtmusk.o rtout.o rtpest.o rtsed.o\
sat_excess.o saveconc.o save.o sched_mgt.o schedule_ops.o sim_initday.o sim_inityr.o simulate.o slrgen.o smeas.o snom.o soil_chem.o\
soil_phys.o soil_write.o solp.o solt.o std1.o std2.o std3.o stdaa.o storeinitial.o structure.o subaa.o subbasin.o subday.o submon.o\
substor.o sub_subbasin.o subwq.o subyr.o sumhyd.o sumv.o surface.o surfst_h2o.o surfstor.o surq_daycn.o surq_greenampt.o swbl.o sweep.o\
swu.o tair.o tgen.o theta.o tillfactor.o tmeas.o tran.o transfer.o tstr.o ttcoef.o ttcoef_wway.o urban.o urbanhr.o urb_bmp.o varinit.o\
vbl.o virtual.o volq.o washp.o watbal.o water_hru.o watqual2.o watqual.o wattable.o watuse.o weatgn.o wetlan.o wmeas.o wndgen.o writeaa.o\
writea.o writed.o writem.o xmon.o ysed.o zero0.o zero1.o zero2.o zeroini.o zero_urbn.o\
Info.o ERROR.o OPSUM.o OPVIEW.o OPHARV.o PlantNBal.o HRes_CGRO.o PODDET.o PODS.o NUPTAK.o INCOMP.o DEMAND.o PHENOL.o PHOTO.o IPPLNT.o READS.o UTILS.o DATES.o Warning.o\
OPPEST.o OpPlantP.o PPlantSubs.o P_Uptake.o P_IPPLNT.o LINDM.o\
ROOTDM.o VEGDM.o SEEDDM.o ASMDM.o PESTCP.o IPPROG.o IPPARM.o IPPEST.o P_Plant.o RootSoilVol.o P_CGRO.o PEST.o OPSTRESS.o CANOPY.o RStages.o Ipphenol.o SDCOMP.o\
FreshWt.o RESPIR.o MOBIL.o VEGGR.o SENES.o FREEZE.o ROOTS.o Opgrow.o \
WH_ROOTS.o WH_NUPTAK.o for_ipphenol.o for_sdcomp.o for_rstages.o \
TF_ROOTS.o TF_SW_SUBS.o WH_SW_SUBS.o TF_COLD.o TF_temp.o TF_NFACTO.o TF_ROOTS.o  \
CSCAS.o SC_radiat.o MZ_IX_PLANTG.o MZ_IX_RESPIR.o MZ_IX_RADABS.o TF_SW_SUBS.o TF_ROOTS.o  \
for_poddet.o for_pods.o for_nfix.o for_mobil.o for_nuptak.o for_demand.o for_ch2oref.o for_respir.o for_senmob.o \
for_ipplnt.o for_photo.o for_phenol.o for_incomp.o for_ippest.o for_ipparm.o for_ipprog.o for_pestcp.o for_asmdm.o for_seeddm.o \
for_vegdm.o for_rootdm.o for_oppest.o for_lindm.o for_canopy.o \
for_hres_cgro.o for_plantnbal.o for_dormancy.o for_opgrow.o for_opmob.o for_pest.o for_harv.o for_grow.o for_roots.o for_freeze.o for_veggr.o \
WBSUBS.o WH_COLD.o WH_temp.o WH_NFACTO.o P_CERES.o SC_GTP_SHOOTS.o RI_Calcshk.o RI_Ipcrop.o RI_KUPTAK.o RI_Transpl_p.o RI_Transpl_g.o RI_Phenol.o \
CSUTS.o HMET.o CSREADS.o CSDISEASE.o YCA_RunInit.o YCA_SeasInit.o YCA_Growth.o YCA_Integrate.o YCA_Output.o YCA_PrePlant.o \
YCA_Growth_Evapo.o YCA_Growth_Init.o YCA_Growth_Rates.o YCA_Growth_Senesce.o YCA_Growth_Photo.o YCA_Growth_Part.o YCA_Growth_NUptake.o YCA_Growth_Distribute.o \
SC_COEFFS.o CSCER.o CSCRP.o CSYCA.o WH_PHENOL.o WH_GROSUB.o WH_OPGROW.o WH_OPNIT.o WH_OPHARV.o HResCeres.o \
 TF_PHENOL.o TF_GROSUB.o TF_OPGROW.o TF_OPNIT.o TF_OPHARV.o YCA_SeasInit_VarInit.o YCA_SeasInit_ReadXfile.o \
YCA_SeasInit_PlHarvDat.o YCA_SeasInit_ReadGeno.o YCA_SeasInit_SetStage.o YCA_SeasInit_Final.o \
YCA_Out_ModFail.o YCA_Out_PlGrow.o YCA_Out_FreshWt.o YCA_Out_Eval.o YCA_Out_PlantSum.o YCA_Out_LfTier.o YCA_Out_WrkPhenRes.o YCA_Out_Error.o YCA_Out_CrpSim.o YCA_Out_Sens.o\
YCA_Out_StoreVars.o YCA_Out_ReInit.o \
CSP_INPHENOL.o BS_NUPTAK.o BS_NFACTO.o PT_NUPTAK.o PT_NFACTO.o PT_PHASEI.o PT_THTIME.o SG_NUPTAK.o SG_NFACT.o SG_PHASEI.o \
CSP_IPPHENOL.o ML_PHASEI.o ML_NUPTAK.o ML_NFACT.o ML_TILLSUB.o SW_FreshWt.o MZ_NFACTO.o MZ_NUPTAK.o MZ_IX_NUPTAK.o \
MZ_IX_KNUMBER.o MZ_IX_LEAFAREA.o MZ_IX_PHOTSYNT.o MZ_KUPTAK.o \
SC_substrates_balance.o SC_PGS.o SC_TOTASS_SAM.o SC_astro.o SC_water_stress_sugarcane.o SC_find_inp_sam.o CSP_CANOPY.o \
CSP_INCOMP_OUT.o SC_sucrose_content.o CSCAS_Interface.o forage.o \
SC_OUTPUT.o SC_OPHARV.o SC_Poplt3.o SC_Canop3.o SC_ROOTG.o SC_PHENOL.o SC_PHOTOS.o SC_CCOUT.o SC_ETOUT.o \
RI_Opharv.o RI_Nuptak.o RI_Tillsub.o RI_Nfacto.o RI_Opgrow.o RI_Grosub.o RI_Rootgr.o \
SG_PHENOL.o SG_GROSUB.o SG_OPHARV.o SG_ROOTGR.o  MZ_OPGROW.o MZ_OPNIT.o \
PT_OPGROW.o PT_OPHARV.o PT_ROOTGR.o PT_PHENOL.o PT_GROSUB.o \
BS_PHENOL.o BS_GROSUB.o BS_ROOTS.o BS_OPGROW.o BS_OPNIT.o BS_OPHARV.o \
MZ_PHENOL.o MZ_IX_PHENOL.o MZ_GROSUB.o MZ_IX_GROSUB.o SW_GROSUB.o MZ_ROOTS.o MZ_OPHARV.o \
ML_opharv.o ML_OPGROW.o ML_rootgr.o ML_GROSUB.o ML_PHENOL.o \
TR_Ipcrop.o TR_Nfacto.o TR_Calcshk.o TR_Tillsub.o TR_Nuptak.o TR_Transpl_g.o TR_Transpl_p.o \
CSP_IPPLNT.o CSP_PHENOL.o CSP_PHOTO.o CSP_INCOMP.o CSP_GROW_CANE.o CSP_NUPTAK.o CSP_SENES.o CSP_ROOTS.o CSP_OPGROW.o CSP_OPHARV.o  CSP_HRes.o P_CASUPRO.o CSP_RESPIR.o \
SC_functions.o SC_root_profile.o SC_SAMUCA_MODEL.o SC_OUTPUT_SAM.o SC_Det_outputs.o SC_OPHARV_SAM.o \
Aloha_NFACTO.o Aloha_NUPTAK.o SC_rm_file.o  \
Aloha_PHENOL.o Aloha_GROSUB.o Aloha_ROOTGR.o Aloha_OPGROW.o Aloha_OPHARV.o TR_OPGROW.o TR_Opharv.o TR_Phenol.o TR_Rootgr.o TR_Grosub.o RI_GNURSE.o \
YCA_Integ_AgesWts.o YCA_Integ_LA.o YCA_Integ_N.o YCA_Integ_Stages.o YCA_Integ_Nconc.o YCA_Integ_HstFail.o YCA_Integ_SeasEnd.o YCA_Integ_WthrSum.o YCA_Integ_EndCrop.o \
CROPGRO.o CSCERES_Interface.o CSCRP_Interface.o CSYCA_Interface.o \
WH_APSIM.o TF_APSIM.o ML_CERES.o MZ_CERES.o BS_CERES.o PT_SUBSTOR.o RICE.o SC_PARTIT.o SC_CNGRO.o CSP_CASUPRO.o SG_CERES.o TR_SUBSTOR.o Aloha_PINE.o  plant.o PET.o \
OPFLOODN.o IPHedley_inorg.o \
FCHEM.o FLOODI.o EQUIL2.o OPSOILNI.o OpSoilPi.o SoilPi_init.o SoilPiBal.o \
RPLACE_C.o INCORPOR_C.o DECRAT_C.o CE_RATIO_C.o LITDEC_C.o SOMDEC_C.o IMMOBLIMIT_C.o NCHECK_C.o SoilOrg_init.o NCHECK_organic.o SoilNoPoBal.o SoilNi_init.o NCHECK_inorg.o \
OpSoilKi.o SoilK_init.o EFLOW_C.o Flood_Chem.o OXLAYER.o Denit_DayCent.o Denit_Ceres.o nox_pulse.o diffusiv.o NFLUX.o SoilNiBal.o\
IPHedley_C.o SoilPoBal_C.o SoilNoBal_C.o SoilCBal.o OpSoilOrg.o SENESADD_C.o MULCHLAYER.o SOMINIT_c.o PARTIT_C.o SOMFIX_C.o \
LMATCH.o RETC_VG.o TextureClass.o TillEvent.o PATH.o OPSOMLIT_C.o SoilCNPinit_C.o TSOMLIT_C.o SOMLITPRINT_C.o \
ETPHR.o OpStemp.o OPETPHOT.o ETPHOT.o  ROOTWU.o SOILEV.o STEMP.o  STEMP_EPIC.o TRANS.o MULCHEVAP.o ESR_SoilEvap.o SPSUBS.o SOILDYN.o CENTURY.o SoilOrg.o \
SoilKi.o SOILNI.o SoilPi.o SOLAR.o CO2VAL.o Flood_Irrig.o AUTHAR.o AUTPLT.o CHEMICAL.o Fert_Place.o OM_Place.o Tillage.o IRRIG.o \
OpFlood.o Paddy_Mgmt.o MgmtOps.o \
SPAM.o SOIL.o IPIBS.o LAND.o

all: swat swatdll

swat: $(MODOBJ) $(OBJ)
	$(FC) $(FFLAGS) $(MODOBJ) $(OBJ) -o $(SWAT.OUT)
#	$(CP) $(SWAT.OUT) $(BIN.DIR)
	$(CP) $(SWAT.OUT) $(RUN.DIR)
	$(CP) $(SWAT.OUT) $(RUN2.DIR)
	$(CP) $(SWAT.OUT) $(RUN3.DIR)
#	$(CP) *.mod $(LIB.DIR)

swatdll: $(MODOBJ2) $(OBJ)
	$(FC) $(FFLAGS) $(MODOBJ2) $(OBJ) -shared -o $(SWAT.DLL)
	$(CP) $(SWAT.DLL) $(LIB.DIR)
	$(CP) *.mod $(LIB.DIR)

clean:
#	$(MAKE) clean -C
#	$(RM) *.o *.mod *.exe *.dll *.so $(SWAT.OUT)
	@rm -f *.o *.mod *.exe *.dll *.s0 $(SWAT.OUT)
.for.o:
	$(FC) $(FFLAGS) -c $<
.f90.o:
	$(FC) $(FFLAGS) -c $<
.f03.o:
	$(FC) $(FFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<
.for.in.o:
	$(FC) $(FFLAGS) -c $<

$(LIB.DIR)$(EVENT.DIR):
	$(MAKE) -C $(EVENT.DIR)


