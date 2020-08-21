C
C $Log:   GXAFXT:[GOLS]PUTIME.FOV  
C  
C     Rev 1.0   17 Apr 1996 14:34:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   10 Aug 1993 17:54:00   GXA
C  Initial revision.
C
C SUBROUTINE TO PUT TIME IN TERMINAL MESSAGE BUFFER
C
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PUTIME(TIME,OUTTAB,IND)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C 
        BYTE      OUTTAB(*)			!Output Message table.
C
	INTEGER*4 IND				!Index into Output Table
	INTEGER*4 TIME				!Time in ms since midnight
	INTEGER*4 HOURS
	INTEGER*4 MINS
	INTEGER*4 SECS
C
	BYTE	  I1TEMP(4)
	INTEGER*4 TEMP
	EQUIVALENCE(TEMP,I1TEMP)
C
	LOGICAL   HH_MM_SS_FLAG/.TRUE./		!Indicate what format.
C
	IF(HH_MM_SS_FLAG) THEN
	   HOURS = TIME/3600
	   MINS = (TIME-HOURS*3600) / 60
	   SECS = TIME - HOURS*3600 - MINS*60
C
	   OUTTAB(IND+0) = HOURS
	   OUTTAB(IND+1) = MINS
	   OUTTAB(IND+2) = SECS
	ELSE
	   TEMP = TIME
	   OUTTAB(IND+0) = I1TEMP(3)
	   OUTTAB(IND+1) = I1TEMP(2)
	   OUTTAB(IND+2) = I1TEMP(1)
	ENDIF
	IND = IND + 3
C	   
	RETURN
	END
