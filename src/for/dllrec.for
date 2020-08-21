C  GXSRC:DLLREC.FOR
C  
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DLLREC.FOV                                   $
C  $Date::   17 Apr 1996 12:55:42                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C
C V02 15-Sep-95 das Changed for background loads
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
	SUBROUTINE DLLREC(NEW,FILNAM,LU,LEN,BUF,ST)
	IMPLICIT NONE
C
	INTEGER*4 IND, BLK, LU, ST, OUTIND, LEN
	INTEGER*4  VALUE, OFF, NEXT
	INTEGER*4 FILNAM(2),I4LBUF(128)
	INTEGER*4 LOCNAM(4) /4*0/, DOT_BIN/'.BIN'/
C
	CHARACTER BUF(*),LOCALBUF(512)
	BYTE	  CLOCNAM(4)
C
	LOGICAL NEW
C
	EQUIVALENCE (I4LBUF,LOCALBUF)
	EQUIVALENCE (LOCNAM(2),CLOCNAM)
C
C
C
        ST = 0 
	IF(NEW) THEN
	  NEW=.FALSE.
	  IND=999
	  BLK=0
	  CLOSE(UNIT=LU)
	  LOCNAM(1)=FILNAM(1)
	  IF (FILNAM(2).EQ.0) THEN
	    LOCNAM(2)=DOT_BIN
	    LOCNAM(3)=0
	  ELSE
	    NEXT=1
	    DO OFF=3,4
	      CALL GETNIBLE(VALUE,FILNAM(2),OFF)
	      IF (VALUE.GE.10) VALUE=VALUE+7
	      CLOCNAM(NEXT)=VALUE+48
	      NEXT=NEXT+1
	    ENDDO
	    DO OFF=1,2
	      CALL GETNIBLE(VALUE,FILNAM(2),OFF)
	      IF (VALUE.GE.10) VALUE=VALUE+7
	      CLOCNAM(NEXT)=VALUE+48
	      NEXT=NEXT+1
	    ENDDO
	    LOCNAM(3)=DOT_BIN
	  ENDIF
	  OPEN(UNIT=LU,NAME=LOCNAM,STATUS='OLD',IOSTAT=ST,FORM='UNFORMATTED',
     *         ACCESS='DIRECT')
	  IF(ST.NE.0) THEN
       	    WRITE(5,900) LOCNAM,ST
900	    FORMAT(1X,4A4,' open error ',I4)
	    CALL GPAUSE
	  ENDIF
	  RETURN
	ENDIF
C
C
	OUTIND=1
10	CONTINUE
	IF(IND.GT.512) THEN
	  IND=1
	  BLK=BLK+1
	  READ(LU,REC=BLK,IOSTAT=ST) I4LBUF
	  IF(ST.NE.0) THEN
	    WRITE(5,901) FILNAM,ST,BLK
901         FORMAT(1X,2A4,' read error ',I4,' block # ',I4)
	    CALL GPAUSE
	  ENDIF
	ENDIF
C
C
	BUF(OUTIND)=LOCALBUF(IND)
	OUTIND=OUTIND+1
	IND=IND+1
	IF(OUTIND.GT.LEN) RETURN
	GOTO 10
	END
 
