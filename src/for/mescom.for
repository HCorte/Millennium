C
C SUBROUTINE MESCOM
C $Log:   GXAFXT:[GOLS]MESCOM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:01:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:59:50   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - mescom.for **
C
C MESCOM.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO FORMAT COMMUNICATONS MESSAGES FOR ERRLOG
C
C
C V01 01-FEB-89 MTK INITIAL RELEASE FOR SWEDEN
C
C
C MESSAGE #                MESSAGE
C   1               LINE <XXX> ER1 ER2 ER3 ER4 .... ERX
C   2               LINE <XX> TERMINAL <XXXX> ERRCNT <XXXXX>
C   3               LINE <XX> TERMINAL <XXXX> SLOFLG <XXXXX>
C   4               DLL TER <XXXX> MESS <XXXX>
C   5               AVAILABLE FOR USE
C   6               BUFFER ERROR - BAD BUFFER NUMBER
C   7               QUEUE ERROR - QUEUE-<XXX> BUF-<XXXX>
C   8               PTL-[XXX] SER-[XXXXXXXX] -LAY-[XX] STTN-[XXXX] SAP
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
	SUBROUTINE MESCOM(MNUM,DBUF,MBUF,ALARM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4 DBUF(*),COMERR(15), CEBUF(10)
	INTEGER*4 K, IND, X, MNUM, BLANK
	CHARACTER*140	MBUF
	LOGICAL ALARM
	DATA COMERR/'RNG ','CAR ','EXA ','BUS ','RCR ','FRA ','PAR ',
     *	            'OVR ','CHE ','OVF ','TIM ','SYN ','XNS ','RNS ',
     *	            'LOST'/
C
	GOTO (1,2,3,4,5,6,7,8,9,10) MNUM
	GOTO 99
C
C
1	CONTINUE
	X=1
	IND=3
	CALL FASTSET(BLANK,CEBUF,10)
	DO 10001 K=1,14
	IF(IAND(X,DBUF(3)).NE.0) THEN
	  CEBUF(IND)=COMERR(K)
	  IND=IND+1
	  IF(IND.EQ.10) THEN
	    CEBUF(10)=COMERR(15)
	    GOTO 10002
	  ENDIF
	ENDIF
	X=ISHFT(X,1)
10001	CONTINUE
	RETURN
C
10002	CONTINUE
	WRITE(MBUF,901) DBUF(1), CEBUF
901	FORMAT('LIN ',I3,10A4)
C
C
2	WRITE (MBUF,902) (DBUF(K),K=1,3)
902	FORMAT('LINE> ',I3,' TERM> ',I5,' ERRCNT> ',I5)
	RETURN
C
3	WRITE (MBUF,903)(DBUF(K),K=1,3)
903	FORMAT('LINE> ',I2,' TERM> ',I5,' SLOFLG> ',I2)
	RETURN
C
4	WRITE (MBUF,904) (DBUF(K),K=1,2)
904	FORMAT('DLL TER # ',I2,' W/MESS #',I2)
	RETURN
C
5	CONTINUE
	GOTO 99
C
6	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,906)
906	FORMAT('BUFFER ERROR - BAD BUFFER NUMBER')
	RETURN
C
7	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,907) (DBUF(K),K=1,2)
907	FORMAT('QUE ERROR QUE> ',I3,' BUFFER> ',I4)
	RETURN
C
8	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,908) (DBUF(K),K=1,5)
908	FORMAT('SER-[',I9,']PTL-[',I3.3,']LAYER-[',I2.2,']STTN-[',I4.4,
     *	       ']SAP-[',I3,']')
	RETURN
C
C PUT NEXT COMMUNICATIONS MESSAGE HERE
C
9	CONTINUE
10	CONTINUE
C
C INVALID MESSAGE NUMBER
C
99	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,999) MNUM
999	FORMAT('INVALID COMMUNICATIONS MESSAGE NUMBER> ',I4)
	RETURN
	END
