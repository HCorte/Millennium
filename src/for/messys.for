C
C SUBROUTINE MESSYS
C $Log:   GXAFXT:[GOLS]MESSYS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:02:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:00:22   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - messys.for **
C
C MESSYS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO FORMAT SYSTEM MESSAGES FOR ERRLOG
C
C
C V01 01-FEB-89 MTK INITIAL RELEASE FOR SWEDEN
C
C
C MESSAGE #               MESSAGE
C   1               HOLD DIRECTIVE ERROR <ERROR>
C   2               RELSE DIRECTIVE ERROR <ERROR>
C   3               SNDMSG DIRECTIVE ERROR <ERROR>
C   4-98            AVAILABLE FOR USE
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
	SUBROUTINE MESSYS(MNUM,DBUF,MBUF,ALARM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4 DBUF(*), MNUM
	CHARACTER*140 MBUF
	LOGICAL ALARM
C
	GOTO (1,2,3,4,5,6,7,8,9,10) MNUM
	GOTO 99
C
C
1	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,901) DBUF(1)
901	FORMAT('HOLD DIRECTIVE ERROR> ',I4)
	RETURN
C
2	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,902) DBUF(1)
902	FORMAT('RELSE DIRECTIVE ERROR> ',I4)
	RETURN
C
3	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,903) DBUF(1)
903	FORMAT('SNDMSG DIRECTIVE ERROR> ',I4)
	RETURN
C
C PUT NEXT SYSTEM MESSAGE HERE
C
4	CONTINUE
5	CONTINUE
6	CONTINUE
7	CONTINUE
8	CONTINUE
9	CONTINUE
10	CONTINUE
C
C INVALID MESSAGE NUMBER
C
99	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,999) MNUM
999	FORMAT('INVALID SYSTEM MESSAGE NUMBER> ',I4)
	RETURN
	END
