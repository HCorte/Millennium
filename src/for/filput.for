C
C SUBROUTINE FILPUT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]FILPUT.FOV                                   $
C  $Date::   17 Apr 1996 13:10:02                                         $
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
C FILPUT(BUF,LOGSER,LBUF,STATUS) ; PUT RECORD IN BUFFER
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
	SUBROUTINE FILPUT(BUF,LOGSER,LBUF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
C
	INTEGER*4 LBUF(LMUREC) !RECORD
	INTEGER*4 BUF           !BUFFER NR
	INTEGER*4 STATUS        !RETURN STATUS, NON 0 IF NO MORE ROOM IN BUFFER
C
	INTEGER*4 LENGTH, LOGSER
 
C
	INTEGER*4 ACTOFF !ACTUAL OFFSET
	DATA ACTOFF/0/
C
	IF (ACTOFF.EQ.0) ACTOFF=HDRSIZ+1  !START FROM BEG OF DATA
	IF (ACTOFF.GE.NETLEN-(LMUREC+1)-1)  GOTO 1000 !IF BUF FULL
	CALL TMFRLN(LBUF,LENGTH) !GET RECORD SIZE
	NETBUF(ACTOFF,BUF)=LOGSER
	ACTOFF=ACTOFF+1
C***  DO 20, OFF=ACTOFF,ACTOFF+LENGTH-1
C***  NETBUF(OFF,BUF)=LBUF(OFF-ACTOFF+1)  ;COPY RECORD TO NETBUF
C***20    CONTINUE
	CALL MOVTAB(LBUF(1),NETBUF(ACTOFF,BUF),LENGTH)
	ACTOFF=ACTOFF+LENGTH
	STATUS=0
	RETURN
C
C--------------
C
	ENTRY ENDRST(BUF,STATUS)  !RESET END OF BUFFER ENTRY
C
	IF(ACTOFF.EQ.0)ACTOFF=HDRSIZ+1   !START FROM BEG OF DATA
1000	CONTINUE
C
C***  DO 10, OFF=ACTOFF,NETLEN ;CLEAR END OF BUFFER
C***  NETBUF(OFF,BUF)=0
C***10    CONTINUE
D	TYPE *,'ENDRST: ACTOFF,BUF,NETLEN ',ACTOFF,BUF,NETLEN
	CALL FASTSET(0,NETBUF(ACTOFF,BUF),NETLEN-ACTOFF+1)
C***	CALL SETTAB(0,NETBUF(ACTOFF,BUF),NETLEN-ACTOFF+1)
	LENGTH=0
	ACTOFF=0 !NEXT TIME START WITH BEGGINING OF BUFFER
	STATUS=-1 !NO MORE ROOM
	RETURN
	END
