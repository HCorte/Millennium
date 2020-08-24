C
C SUBROUTINE LDUMP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LDUMP.FOV                                    $
C  $Date::   17 Apr 1996 13:48:56                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - ldump.for **
C
C LDUMP.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LDUMP(BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
C
	INTEGER*4 DAT(3),TIM(3), LAST, LINK, OFF, II, BUF
	INTEGER*4 RECORD/0/
C
C
	IF (RECORD.EQ.0) THEN
	  OPEN(UNIT=6,FILE='NETERR.FIL',ACCESS='SEQUENTIAL',RECL=72,
     *	       STATUS='UNKNOWN')
	ENDIF
	CALL XDAT(DAT)
	CALL XTIM(TIM)
C
	RECORD=RECORD+1
	WRITE (6,910) RECORD,(DAT(II),II=3,1,-1),TIM
910	FORMAT(' buffer error, record - '
     *	       ,I4,' time - ',2(I2.2,'/'),I2.2,1H ,2(I2.2,':'),I2.2)
C
	WRITE (6,900)(NETBUF(OFF,BUF),OFF=1,NETLEN)
900	FORMAT(8(1X,Z8))
C
	LINK=HDRSIZ+1
	LAST=NETBUF(NEXT,BUF)
	TYPE *,' received buffer error '
	TYPE *,'buffer dumped to record nr ',RECORD,' in NETERR.FIL'
	TYPE 100,BUF,NETBUF(MODE,BUF),NETBUF(NEXT,BUF),
     +	         NETBUF(PSER,BUF),NETBUF(PSTATE,BUF),
     +	         NETBUF(NUMREC,BUF)
100	FORMAT(1X,'DUMP OF  BUFFER ',I2,/,
     +	       1X,'MODE = ',I5,' NEXT SLOT = ',I5,/,
     +	       1X,'HSER = ',I9.9,
     +	       1X,'STATE= ',I5,' NUMRECS   = ',I5)
	RETURN
	END
