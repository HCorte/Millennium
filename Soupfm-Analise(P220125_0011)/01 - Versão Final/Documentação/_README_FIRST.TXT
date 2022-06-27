********************************************************************************
* This item is the property of SCML.
*
* All rights reserved.
*
* VER  DATE         AUTHOR  DESCRIPTION
* ---  -----------  ------  ----------------------------------------------------
* V02  08-APR-2022  SCML    Added changes to SOUPFM_REL_AM_EXPIRED for reports
* V01  23-MAR-2022  SCML    Bug fix for SPORTS (GAME NUM=10) not generating reports with the 
*                           data of prizes purged (uncashed) - ticket(P220125_0011)
********************************************************************************

This release contains the following source file:

SOUPFM_AM_EXPIRED.FOR
SOUPFM_REL_AM_EXPIRED.FOR

================================================================================
INSTRUCTIONS FOR EXECUTABLE GENERATION:
--------------------------------------------------------------------------------

1. COPY FROM THE DIRECTORY "EXECUTAVEIS" THE FOLLOWING FILE INTO WORKING DIRECTORY

   SOUPFM_AM_EXPIRED.FOR
   SOUPFM_REL_AM_EXPIRED.FOR

2. COMPILE SOURCE CODE

   DFOR SOUPFM_AM_EXPIRED.FOR
   DFOR SOUPFM_REL_AM_EXPIRED.FOR
   
4. LINK PROGRAMS

   DLNK SOUPFM

5. ADD THE FOLLOWING SOURCE CODE FILE INTO CMS

   PUT SOUPFM_AM_EXPIRED.FOR
   PUT SOUPFM_REL_AM_EXPIRED.FOR
