C
C SUBROUTINE TSKABORT
C $Log:   GXAFXT:[GOLS]TSKABORT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:37:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:53:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_tskabort.for **
C
C TSKABORT.FOR
C
C V03 22-OCT-91 TKO Name changed from NET_ABORT (see below)
C V02 13-MAY-91 JWE Make it work.
C V01 26-DEC-90 MRM INITIAL RELEASE.
C
C This will abort (i.e., stop) a running task
C
C Before V03, this source was named NET_ABORT and included the subroutine
C ABORT.  Since this name conflicts with other names (specifically in SYBASE),
C I have changed the subroutine name to TSKABORT and have included this
C routine in NORMLIB.  
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TSKABORT(TASK_PARAMETER,STATUS)
	IMPLICIT NONE
C
	INCLUDE	'($SSDEF)'
	INCLUDE '($SYSSRVNAM)'
C
	BYTE	    TASK_PARAMETER(8)
	INTEGER*4   STATUS
C
	BYTE	    SYSTEM_PREFIX(4)
	INTEGER*4   SYSTEM_PREFIX_LENGTH
	INTEGER*2   I2
	INTEGER*2   LENGTH_OF_PROCESS_NAME
	INTEGER*2   BASE
	CHARACTER   PROCESS_NAME*15
C
C Get prefix for system
C
	CALL GETPRFX(SYSTEM_PREFIX,SYSTEM_PREFIX_LENGTH)
	DO 1000 I2=1,SYSTEM_PREFIX_LENGTH
	    PROCESS_NAME(I2:I2)=CHAR(SYSTEM_PREFIX(I2))
1000	CONTINUE
C
C Add task name - from end of prefix to length of parameter or max.
C
	BASE=SYSTEM_PREFIX_LENGTH+1
	DO 1010 I2=BASE,MIN(BASE+7,LEN(PROCESS_NAME))
	    PROCESS_NAME(I2:I2)=
	1	CHAR(TASK_PARAMETER(I2-BASE+1))
1010	CONTINUE
C
	DO 1020 I2=LEN(PROCESS_NAME),1,-1
	    IF(PROCESS_NAME(I2:I2).GT.'A'.AND.
	1   	PROCESS_NAME(I2:I2).LT.'Z')GOTO 1030
1020	CONTINUE
1030	CONTINUE
	LENGTH_OF_PROCESS_NAME=I2	    
C
	STATUS=SYS$FORCEX(,PROCESS_NAME(:LENGTH_OF_PROCESS_NAME),)
C
	RETURN
	END
