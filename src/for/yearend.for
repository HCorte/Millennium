C
C PROGRAM YEAREND
C $Log:   GXAFXT:[GOLS]YEAREND.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:48:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   27 Jun 1993 15:02:06   HXK
C  MOVED AGTINF.DEF INCLUDE
C  
C     Rev 1.0   21 Jan 1993 18:39:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - yearend.for **
C
C YEAREND.FOR
C
C V02 06-JAN-92 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 02-DEC-91 JPJ RELEASED FOR VAX
C
C
C
C COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C COPYRIGHT 1990 GTECH CORPORATION.  ALL RIGHTS RESERVED.
C
C CONFIDENTIAL PROPRIETARY INFORMATION
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information.  It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH.  Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published or disclosed, in whold or in part, directly
C or indirectly, except as expressly authorized by an officer of
C GTECH pursuant to written agreement.
C COPYRITF.DEF-------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM YEAREND
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
C
	INTEGER*4 AGT, ST
C
C OPEN THE AGENT SALES FILE
C
	CALL OPENASF(ASF)
C
	TYPE *,IAM(),' *** Beginning Year End Processing '
	DO 100 AGT=1,NUMAGT
	CALL READASF(AGT,ASFREC,ST)
	IF(ST.NE.0) THEN
	  TYPE*,'ASF.FIL READ ERROR ',ST,' REC ',AGT
	  STOP
	ENDIF
C
C MOVE CURRENT TO LAST YEAR
C
	CALL FASTMOV(ASFYTD(1,1,1),ASFYTD(1,1,2),14*MAXGAM)
	CALL FASTMOV(ASFYTDINV(1,1),ASFYTDINV(1,2),30)
C
C CLEAR OUT CURRENT YEAR
C
	CALL FASTSET(0,ASFYTD,14*MAXGAM)
	CALL FASTSET(0,ASFYTDINV,30)
C
	IF(MOD(AGT,1000).EQ.0) TYPE*,AGT,' Agents Processed '
	CALL WRITASF(AGT,ASFREC,ST)
	IF(ST.NE.0) THEN
	  TYPE*,'ASF.FIL WRITE ERROR ',ST,' REC ',AGT
	  STOP
	ENDIF
100	CONTINUE
	TYPE *,IAM(),' *** Year end processing complete *** '
C
	CALL CLOSASF
	END
