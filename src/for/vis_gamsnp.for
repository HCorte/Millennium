C
C SUBROUTINE GAMSNP
C  
C V10 27-JAN-2011 RXK IF condition split
C V09 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V08 05-JAN-1996 PXB Headings are correct
C V07 12-DEC-1995 PXB changed for new games
C V06 23-NOV-1994 HXK Rearranged for Screen print
C V05 25-OCT-1994 PXB Move screen up 1 line to enable display of bingo game.
C V04 11-JUN-1993 HXK ADDED AGTINF.FCC, PRMAGT.FCC
C V03 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update DEC Baseline
C V02 01-AUG-1990 XXX RELEASED FOR VAX
C V01 01-MAR-1990 TDM INITIAL RELEASE FOR DENMARK
C
C
C VIS_GAMSNP.FOR
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GAMSNP(PAGE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
        INTEGER*4 PAGESIZ                ! # of lines displayd on page
        PARAMETER (PAGESIZ=20) 
C
	INTEGER*4 K, GTYP, GIND, I, IND, PAGE, FROM, UNTIL
	INTEGER*4 GTYP2, GIND2

	CHARACTER*4 TEMP(4)
	CHARACTER*8 TEMP1
C
	TEMP(1) = ' '
	TEMP(2) = ' '
	TEMP(3) = ' '
	TEMP(4) = ' '
	TEMP1 = ' '

C	SMODE=.TRUE.
	WRITE(CLIN1,901)
	WRITE(CLIN2,902)
	IND=3

        IF(PAGE.LE.0) PAGE=1
        IF(PAGE.GT.(MAXGAM/(2*PAGESIZ))+1) PAGE=2

        FROM =(PAGE-1)*2*PAGESIZ+1
        UNTIL=(PAGE-1)*2*PAGESIZ+PAGESIZ 

	DO 100 I=FROM,UNTIL
          GIND = 0
          GTYP = 0 
          IF(I.LE.MAXGAM) THEN
	    IF(GNTTAB(GAMIDX,I).GT.0) THEN
	      GIND = GNTTAB(GAMIDX,I)
	      GTYP = GNTTAB(GAMTYP,I)
	    ENDIF
	  ENDIF

	  GIND2 = 0
	  GTYP2 = 0
	  IF(I+PAGESIZ.LE.MAXGAM) THEN
            IF(GNTTAB(GAMIDX,(I+PAGESIZ)).GT.0) THEN
	      GIND2 = GNTTAB(GAMIDX,(I+PAGESIZ))
	      GTYP2 = GNTTAB(GAMTYP,(I+PAGESIZ))
	    ENDIF
	  ENDIF

	  IF (GTYP.GT.0. AND. GTYP2.GT.0) THEN
 	    WRITE(XNEW(  IND),904) (GLNAMES(K,I),K=1,4),
     *			           GTNAMES(GTYP),
     *	                           GIND,
     *                             I,
     *                             (GLNAMES(K,(I+PAGESIZ)),K=1,4),
     *			           GTNAMES(GTYP2),
     *	                           GIND2,
     *                             (I+PAGESIZ)
	  ELSEIF (GTYP.GT.0. AND. GTYP2.EQ.0) THEN
 	    WRITE(XNEW(  IND),904) (GLNAMES(K,I),K=1,4),
     *		  	           GTNAMES(GTYP),
     *	                           GIND,
     *                             I,
     *                             (TEMP(K),K=1,4),
     *			           TEMP1,
     *	                           GIND2,
     *                             0
	  ELSEIF (GTYP.EQ.0. AND. GTYP2.GT.0) THEN
            WRITE(XNEW(  IND),904) (TEMP(K),K=1,4),
     *			           TEMP1,
     *	                           GIND,
     *                             0,
     *                             (GLNAMES(K,(I+PAGESIZ)),K=1,4),
     *			           GTNAMES(GTYP2),
     *	                           GIND2,
     *                             (I+PAGESIZ)
	  ELSE
	     GOTO 100
	  END IF

	  IND=IND+1

100	CONTINUE

	RETURN
C
C

901	FORMAT('System Games Snapshot')

902	FORMAT('Name',14X,'Type',6X,'Index  Num',2X,
     *         'Name',14X,'Type',6X,'Index  Num')

904	FORMAT(4A4,3X,A8,2X,I1,6X,I2,2X,
     *	       4A4,3X,A8,2X,I1,6X,I2)


CCC903   FORMAT('Name',18X,'Type',6X,'Index  Number  Joker')
CCC905   FORMAT(4A4,7X,A8,2X,I1,6X,I2,6X,I2)
	END
