!=======================================================================
!  MODULE CSMVersion
!  07/05/2018 CV  Written
!=======================================================================

      MODULE CSMVersion
!     Contains defintion of version and branch which will be
!     used throughout the model.

!=======================================================================
!MODIFICATION COMMENT @xxxx@
!     Global CSM Version Number
      TYPE VersionType
        INTEGER :: Major = 1!@MAJOR@
        INTEGER :: Minor = 2!@MINOR@
        INTEGER :: Model = 3!@MODEL@
        INTEGER :: Build = 4!@BUILD@
      END TYPE VersionType
      TYPE (VersionType) Version
!     CMake no longer able to change the branch name, do it manually.      
!     CHARACTER(len=*), PARAMETER :: VBranch = '-@BRANCH@'
      CHARACTER(len=*), PARAMETER :: VBranch = '-release'

!     Version history:  
!       4.8.0.7  chp 2021-05-14 v4.8 (=v4.7.5.37)  May 2021 Release
!       4.8.0.6  chp 2021-05-04 v4.8 (=v4.7.5.36+) Pre-release test version
!       4.8.0.5  chp 2021-04-23 v4.8 (=v4.7.5.36)
!       4.8.0.4  chp 2021-04-04 v4.8 (=v4.7.5.33)
!       4.8.0.3  chp 2021-02-08 v4.8 (=v4.7.5.29)
!       4.8.0.2  chp 2020-07-21 v4.8 (=v4.7.5.28)
!       4.8.0.1  chp 2020-07-21 v4.8 (=v4.7.5.14)
!       4.7.5.0  chp 2018-12-18 Pre-release version
!       4.7.2.3  chp 2018-10-25 Rice - G5 parameter (rolled back for v4.7.5)  
!       4.7.2.2  chp 2018-10-25 Sugarbeet, Forage, Cassava,    
!       4.7.2.1  chp 2018-07-08 Use CSMVersion, minor bug fixes, Linux hacks, 
!       4.7.2.0  chp 2018-05-07 v4.7.2 Release 2018 Workshop
!       4.7.1.0  chp 2017-10-27 v4.7.1 Release
!       4.7.0.0  chp 2017-08-09 v4.7.0 Release
!       4.6.5.1  chp 2017-05-10 v4.6.5 Release 2017 Workshop  
!       4.6.0.1  chp 2011-06-28 v4.6.0 Release
!       4.5.1.0  chp 2010-10-10 v4.5.1 Release
!       4.0.2.0  chp 2005-08-11 v4.0.2 Release
!       4.0.1.0  chp 2004-01-28 v4.0.1 Release 
!=======================================================================
      END MODULE CSMVersion

