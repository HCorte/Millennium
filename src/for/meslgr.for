C
C SUBROUTINE MESLGR
C $Log:   GXAFXT:[GOLS]MESLGR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:02:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:00:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - meslgr.for **
C
C MESLGR.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO FORMAT LOGGER MESSAGES FOR ERRLOG
C
C
C V01 01-FEB-89 MTK INITIAL RELEASE FOR SWEDEN
C
C
C MESSAGE #               MESSAGE
C   1               BACKUP TAPE WRITE ERROR <ERROR>
C   2               PRIMARY DISK WRITE ERROR <ERROR>
C   3               PRIMARY DISK READ ERROR <ERROR>
C   4               BACKUP DISK WRITE ERROR <ERROR>
C   5               TAPE SWITCH VALUE ERROR <ERROR>
C   6               TAPE SWITCH <OLD>/<NEW>
C   7               FILE ASSIGN ERROR
C   8               TAPE DRIVE ASSIGN ERROR
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
	SUBROUTINE MESLGR(MNUM,DBUF,MBUF,ALARM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4 DBUF(*), K, MNUM
	CHARACTER*140	MBUF
	LOGICAL ALARM
C
	GOTO (1,2,3,4,5,6,7,8,9,10) MNUM
	GOTO 99
C
C
1	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,901) DBUF(1)
901	FORMAT('TAPE WRITE ERROR> ',I4)
	RETURN
C
2	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,902) (DBUF(K),K=1,2)
902	FORMAT(1X,A8,'PRIMARY VOL ',A4,' WRITE ERROR> ',I4)
	RETURN
C
3	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,903) (DBUF(K),K=1,2)
903	FORMAT('PRIMARY VOL ',A4,' READ ERROR> ',I4)
	RETURN
C
4	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,904) (DBUF(K),K=1,2)
904	FORMAT('BACKUP VOL ',A4,' WRITE ERROR> ',I4)
	RETURN
C
5	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,905) DBUF(1)
905	FORMAT('TAPE SWITCH VALUE ERROR> ',I4)
	RETURN
C
6	CONTINUE
	WRITE (MBUF,906) (DBUF(K),K=1,2)
906	FORMAT('TAPE SWITCH ',I3,' / ',I3)
	RETURN
C
7	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,907) (DBUF(K),K=1,2)
907	FORMAT('UNIT ',I2,' FILE ASSIGN ERROR> ',I4)
	RETURN
C
8	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,908) (DBUF(K),K=1,2)
908	FORMAT(1X,'MAG',I1,': DEVICE ASSIGN ERROR> ',I4)
	RETURN
C
C PUT NEXT LOGGER MESSAGE HERE
C
9	CONTINUE
10	CONTINUE
C
C INVALID MESSAGE NUMBER
C
99	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,999) MNUM
999	FORMAT('INVALID LOGGER MESSAGE NUMBER> ',I4)
	RETURN
	END
