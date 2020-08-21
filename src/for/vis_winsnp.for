C SUBROUTINE WINSNP
C
C V03 19-FEB-2001 ANG ADDED PASSIVE
C V02 29-DEC-1999 OXK Added SHAROK to STSNAM
C V01 24-DEC-1999 OXK Initial revision
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
	SUBROUTINE WINSNP(PAGE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
	
C
        INTEGER*4 PAGESIZ                ! # of lines displayd on page
        PARAMETER (PAGESIZ=20) 
C
	INTEGER*4 PAGE			 ! Page# to be displayed

	INTEGER*4 GTYP, GIND, GSTS, GDRW
	INTEGER*4 I, K, CURLIN, CURPAG, IND, WIN

	CHARACTER*30 STSNAM(0:6)
        DATA STSNAM/'No Winsel Today               ',
     *		    'Regular Winsel                ',
     *		    'Previously Postponed Winsel   ',
     *		    'Results Not In                ',
     *		    'Winsel Cancelled Or Removed   ',
     *		    'Winsel Done                   ',
     *		    'Share calculation done        '/
C
	SMODE=.FALSE.

        IF(PAGE.LE.0) PAGE=1
        IF(PAGE.GT.(MAXGAM/PAGESIZ)+1) PAGE=(MAXGAM/PAGESIZ)+1

	WRITE(CLIN1,901)
	WRITE(CLIN2,902)

	IND=0
	CURPAG=1

	DO 100 I=1,MAXGAM
	  IF (GNTTAB(GAMIDX,I).GT.0) THEN
	    GIND = GNTTAB(GAMIDX,I)
	    GTYP = GNTTAB(GAMTYP,I)
      	    DO WIN=1,MAX_WINSEL
	       GSTS = DRWSTS(WIN,I)
	       GDRW = DRWGAM(WIN,I)
	       IF ((GSTS.NE.WINNOT).AND.(GDRW.GT.0)) THEN
		  IF (PAGE.EQ.CURPAG) THEN
		     CURLIN=IND+3-(CURPAG-1)*PAGESIZ
		     WRITE(XNEW(CURLIN),904) (GLNAMES(K,I),K=1,4),
     *			           GTNAMES(GTYP),
     *	                           GIND,
     *                             I,
     *                             GDRW,
     *                             STSNAM(GSTS),
     *                             WIN
		  ENDIF
		  IND=IND+1
		  IF (MOD(IND,PAGESIZ).EQ.0) CURPAG=CURPAG+1
	       ENDIF
	    ENDDO

            IF (GTYP.EQ.TPAS) THEN
              IF (PASESD(CURDRW,GIND).EQ.DAYCDC .AND. PASSTS(CURDRW,GIND).GE.GAMOPN) THEN
		  CURLIN=IND+3-(CURPAG-1)*PAGESIZ
                  GSTS = 1                                              ! REGULAR WINSEL
                  IF(PASSTS(CURDRW,GIND) .GE. GFINAL) GSTS =  5         ! WINSEL DONE
      		  WRITE(XNEW(CURLIN),904) (GLNAMES(K,I),K=1,4),
     *                                     GTNAMES(GTYP),
     *                                     GIND,
     *                                     I,
     *                                     PASEMIS(CURDRW,GIND),
     *                                     STSNAM(GSTS),
     *                                     WIN
                  IND=IND+1
	          IF (MOD(IND,PAGESIZ).EQ.0) CURPAG=CURPAG+1
	      ENDIF
            ENDIF

	  END IF

100	CONTINUE

	RETURN
C
901	FORMAT('Multiwin Snapshot')
902	FORMAT('Name',T17,'Type',T25,'Ind',T29,'Num',T34,'Draw',T40,
     *		'---- Status ------------------',T71,'WS-ind')
904	FORMAT(   4A4,T17,    A8,T25,   I2,T29,   I2,T34,    I4,T40,A30,T71,I3)

	END
