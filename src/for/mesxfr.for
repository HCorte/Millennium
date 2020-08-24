C
C SUBROUTINE MESXFR
C $Log:   GXAFXT:[GOLS]MESXFR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:02:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:00:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - mesxfr.for **
C
C MESXFR.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01  29-SEP-89  GCAN  INITIAL RELEASE FOR FINLAND
C
C SUBROUTINE TO FORMAT TRANSFERE (XFRPRO,HASP) MESSAGES FOR ERRLOG
C
C
C
C MESSAGE #               MESSAGE
C   1               HASP COMMAND NUMBER H<XX> IS NOT SUPPORTED
C   2               NAK TO FILE OR MESSAGE SENT, <XX> RETRY
C   3               MAX NUMBER OF RETRYES EXEDED, SWITCHING TO TAPE
C
C
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
	SUBROUTINE MESXFR(MNUM,DBUF,MBUF,ALARM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4 DBUF(*), MNUM
	CHARACTER*140 MBUF
	LOGICAL ALARM
C
	GOTO (1,2,3) MNUM
	GOTO 99
C
C
1	CONTINUE
	WRITE (MBUF,901) DBUF(1)
901	FORMAT(' HASP COMMAND NUMBER H',I2.2,' IS NOT SUPPORTED')
	RETURN
C
2	CONTINUE
	IF(DBUF(2).EQ.0) THEN
	   WRITE (MBUF,902) 'FILE',DBUF(1)
	ELSE
	   WRITE (MBUF,902) 'MESS',DBUF(1)
	ENDIF
902	FORMAT(' NAK TO ',A4,' SENT, ',I2,' RETRY')
	RETURN
C
3	CONTINUE
	WRITE (MBUF,903)
903	FORMAT(' MAX NUMBER OF RETRYES EXEDED, SWITCHING TO TAPE ')
	RETURN
C
C INVALID MESSAGE NUMBER
C
99	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,999) MNUM
999	FORMAT('INVALID COMMAND MESSAGE NUMBER> ',I4)
	RETURN
	END
