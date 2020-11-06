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
C   lembrar que horas, minutos e segundos são inteiros logo o resto é ignorado e só têm em conta na diferença com o TIME
	IF(HH_MM_SS_FLAG) THEN !enters alls in this condition.... (so time is in seconds or milisenconds after all?????)
	   HOURS = TIME/3600 !a hour is 60 minutes and a minute is 60 seconds and a second is 1000 ms so 60*60*1000=3600*1000
	   MINS = (TIME-HOURS*3600) / 60 !converte horas em segundos e obtêm o resto em segundos e converte para minutos dai dividir por 60
	   SECS = TIME - HOURS*3600 - MINS*60! obtêm o resto em segundos
C
	   OUTTAB(IND+0) = HOURS !IND = 19 + 0 -> 19
	   OUTTAB(IND+1) = MINS !IND = 19 + 1 -> 20
	   OUTTAB(IND+2) = SECS !IND = 19 + 2 -> 21
	ELSE
	   TEMP = TIME
	   OUTTAB(IND+0) = I1TEMP(3)
	   OUTTAB(IND+1) = I1TEMP(2)
	   OUTTAB(IND+2) = I1TEMP(1)
	ENDIF
	IND = IND + 3 !IND=19+3=22
C	   
	RETURN
	END
