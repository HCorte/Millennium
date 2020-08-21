C  GXSRC:GETLUN.FOR
C  
C  $Log:   GXAFXT:[GOLS]GETLUN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:20:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   24 Mar 1994 14:08:48   MCM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    03 Jan 1994 15:39:02   JPJ
C  Initial revision.
C  
C GETLUN(LUN) RETURNS AVAILABLE LOGICAL UNIT (>= THAN INPUT 'LUN')
C OR -1 IF NONE
C INPUT:
C	LUN
C OUTPUT:
C	LUN (>= THAN INPUT LUN)
C	OR -1 IF NONE
C V01 28-OCT-92 MP  RELEASED FOR MARYLAND
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETLUN(LUN)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   LUN, ST
C
	INTEGER*4   LOLUN	    !SMALLEST LUN TO LOOK AT
	INTEGER*4   HILUN/200/	    !HIGHEST  LUN TO LOOK AT
	LOGICAL*1   ISTHERE
C
	LOLUN=MAX(1,LUN)
C
C Find a free lun to use for the open
C
	DO 1100 LUN = LOLUN, HILUN
C
C ISTHERE is set to true if LU is already used.
C
	  INQUIRE(UNIT=LUN, OPENED=ISTHERE, IOSTAT=ST)
	  IF(ST.EQ.0.AND..NOT.ISTHERE) GOTO 1200
C
1100	CONTINUE
	TYPE *,IAM(),'GETLUN - NO LUN AVAILABLE'
	LUN = -1		! NO AVAILABLE LUN
	GOTO 9000
C
1200	CONTINUE
C
9000	CONTINUE
	RETURN
	END
