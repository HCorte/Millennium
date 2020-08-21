C
C V02 11-AUG-11 RXK Call of COPYRITE added
C V01 11-JAN-99 GPW INITIAL RELEASE FOR FINLAND
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1999 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  -------- STOPSYS OPTIMISATION ---------
C
C  -- SET STOPCOM SECTION FOR MULTIWIN
C  -- CLEAR ASFLOCK
C  -- SET STOPMOD TO WINMANUAL	
C
C	
      PROGRAM STARTER
C
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSDEFINE.DEF'
C
      CALL COPYRITE
      CALL SETSTOPCOM
C
      CALL GSTOP(GEXIT_SUCCESS)
      END
