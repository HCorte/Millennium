C
C MLTWIN.FOR
C
C
C SENDS A COMMAND TO MAKE MULTIWIN TO PROCEED
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MLTWIN(GNUM,DRAW,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

C
        ! variables
	INTEGER*4  DRAW                    !
	INTEGER*4  STATUS,ST
	INTEGER*4  GNUM                    !
	INTEGER*4  GIND                    !
	INTEGER*4  I                       !
        INTEGER*4  CBUF(CDLEN)             ! COMMAND BUFFER
C
        GIND=GNTTAB(GAMIDX,GNUM)
	
        CALL FASTSET(0, CBUF, CDLEN)
        STATUS = 0
 		
        DO I=1,MAX_WINSEL
         IF ((DRWGAM(I,GNUM).EQ.DRAW).OR.(DRWGAM(I,GNUM).EQ.0)) THEN 
	  CBUF(1) = 12
          CBUF(2) = WINYES 
      	  CBUF(3) = TCGEN
          CBUF(8) = I
          CBUF(9) = GNUM
          CBUF(10)= DRAW
          CBUF(11)= GIND
          CALL QUECMD(CBUF,ST)
  	  IF(ST.NE.0) STATUS=-1
  	  RETURN
         ENDIF
        ENDDO
C	
	END
