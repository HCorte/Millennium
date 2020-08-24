C
C SUBROUTINE FILFBF
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]FILFBF.FOV                                   $
C  $Date::   17 Apr 1996 13:09:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - filsrv.for **
C
C
C
C     FILFBF(BUF) ;GET RECORDS FROM BUFFER
C     AND LOG THEM
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
	SUBROUTINE FILFBF(BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INTEGER*4 BUF  !BUFFER #
	INTEGER*4 MESS(EDLEN)
C
	INTEGER*4 DUMMY3, DUMMY2, DUMMY1, STATUS, DONTCARE, TKNXT
	INTEGER*4 NEWSER, SER, LOGSER , SERNUM, LENGTH, ACTOFF
C
	ACTOFF=HDRSIZ+1
C
10	CONTINUE
	CALL TMFRLN(NETBUF(ACTOFF+1,BUF),LENGTH)! GET REC LENGTH
	IF (LENGTH.NE.0) THEN
	   SERNUM=NETBUF(ACTOFF+LSER,BUF) !LAST RECORD WRITTEN
	   LOGSER=NETBUF(ACTOFF,BUF) !LOG POSITION
	   IF (SERNUM.EQ.0) GOTO20
	   NETSER(NODEID,WAYINP)=IAND(LOGSER,'3FFFFFFF'X)
C
C     CHECK IF THIS IS EXPECTED TRANSACTION
C
	   SER=MOD(NETSER(NODEID,WAYINP),SYSOFF)
D	   TYPE*,'SERIAL CAME ',SER
D	   TYPE*,'OF LENGTH ',LENGTH
D	   TYPE*,'NXTSER ',NXTSER
	   IF (SER.NE.NXTSER) THEN
	       CALL GSERIAL(NXTSER,NEWSER,LENGTH/LREC)
D	       TYPE*,'NXTSER NEW LEN ',NXTSER,NEWSER,LENGTH
	       IF (NEWSER.NE.SER) THEN
	          MESS(1)=NTM
	          MESS(2)=TENET
	          MESS(3)=6
	          MESS(4)=SER
	          MESS(5)=NEWSER
	          MESS(6)=FILMD
D	          IF(NETTST.EQ.-2) THEN
D	            TYPE*,'SERIALS OUT OF ORDER ',SER,NEWSER
D	          ENDIF
	          CALL QUEMES(MESS)
	       ENDIF
	   ENDIF
C
	   TKNXT=SER
	   CALL GSERIAL(TKNXT,DONTCARE,LENGTH/LREC)
	   NXTSER=TKNXT
D	   TYPE*,'NEXT NXTSER IS ',NXTSER
	   CALL WLOG(NETSER(NODEID,WAYINP),NETBUF(ACTOFF+1,BUF),NTM
     *	        ,STATUS)  !LOG IT
	   CALL REPROTRA(NETBUF(ACTOFF+1,BUF),DUMMY1,DUMMY2,DUMMY3)
C
C
20	   CONTINUE
	   ACTOFF=ACTOFF+LENGTH+1
	   IF (ACTOFF.GE.NETLEN-(LMUREC+1)-1)RETURN !IF NO MORE LEFT
	   GOTO 10
	ELSE
	   IF(NETTST.EQ.-2) THEN
	      TYPE*,'TRANSACTION LENGTH ',LENGTH
	   ENDIF
	ENDIF
	RETURN
	END
