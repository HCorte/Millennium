C PROGRAM FCANWIN
C
C V10 29-NOV-2000 UXN Totogola added
C V09 28-DEC-1999 OXK Postponed Winsels also included
C V08 13-OCT-1999 RXK World Tour added.
C V07 14-MAY-1999 UXN Super Triple added.
C V06 15-JAN-1999 GLS Auto reporting if MULTIWIN
C V05 18-JAN-1996 RXK Today's Couple and Super Double added 
C V04 14-DEC-1994 PXB Added bingo cancelled winners report.
C V03 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V02 27-APR-1994 JXP COPY=0
C V01 18-NOV-1993 SXH Initial revision.
C
C GENERATE CANCELLED REPORTS FOR FINLAND
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM FCANWIN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:SCRCOM.DEF'
        INCLUDE 'INCLIB:WITCOM.DEF'
        INCLUDE 'INCLIB:CPLCOM.DEF'
        INCLUDE 'INCLIB:DBLCOM.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
	INCLUDE 'INCLIB:SSCCOM.DEF'
	INCLUDE 'INCLIB:TRPCOM.DEF'
	INCLUDE 'INCLIB:STRCOM.DEF'
	INCLUDE 'INCLIB:BNGCOM.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'                
	INCLUDE 'INCLIB:VDETAIL.DEF'
C
        ! parameter
        INTEGER*4    TUBSIZ
        PARAMETER   (TUBSIZ=I4BUCSIZ*7)

        ! variables
	INTEGER*4  DRAWS(MAXGAM)         
	INTEGER*4  VLFBUF(TUBSIZ)        
	INTEGER*4  GIND                  
	INTEGER*4  GTYP                  
	INTEGER*4  GAMCNT                
	INTEGER*4  GAM                   
	INTEGER*4  I                     
	INTEGER*4  K                     
	INTEGER*4  ST                    
	INTEGER*4  COPY                  
	INTEGER*4  FLAG                  
	INTEGER*4  DRAW
	INTEGER*4  REP_INIT(MAXGAM)
C
C
	CALL COPYRITE
	TYPE*,IAM(),' Generating cancelled reports'
	COPY=0
C
	GAMCNT=0
	DO 100 GAM=1,MAXGAM
	    REP_INIT(GAM)=0
	    DRAWS(GAM)=0
	    GTYP = GNTTAB(GAMTYP,GAM)
	    GIND = GNTTAB(GAMIDX,GAM)
	    IF(GTYP.LE.0) GOTO 100
C
	    IF(GTYP.EQ.TLTO) THEN
	        IF(LTODAT(CURDRW,GIND).NE.DAYCDC) GOTO 100
	        GAMCNT=GAMCNT+1
	        DRAWS(GAM)=LTODRW(GIND)
	        WRITE(6,901) IAM(),GTNAMES(TLTO),GIND,LTODRW(GIND)
	    ENDIF
C
            IF(GTYP.EQ.TSPT) THEN
                IF(SPTDAT(CURDRW,GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=SPTDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TSPT),GIND,SPTDRW(GIND)
            ENDIF

            IF(GTYP.EQ.TTGL) THEN
                IF(TGLDAT(CURDRW,GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=TGLDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TTGL),GIND,TGLDRW(GIND)
            ENDIF
C
            IF(GTYP.EQ.TNBR) THEN
                IF(NBRDAT(CURDRW,GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=NBRDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TNBR),GIND,NBRDRW(GIND)
            ENDIF
C
            IF(GTYP.EQ.TWIT) THEN
                IF(WITDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=WITDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TWIT),GIND,WITDRW(GIND)
            ENDIF
C
            IF(GTYP.EQ.TCPL) THEN
                IF(CPLDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=CPLDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TCPL),GIND,CPLDRW(GIND)
            ENDIF
C
            IF(GTYP.EQ.TDBL) THEN
                IF(DBLDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=DBLDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TDBL),GIND,DBLDRW(GIND)
            ENDIF
C
            IF(GTYP.EQ.TKIK) THEN
                IF(KIKDAT(CURDRW,GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=KIKDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TKIK),GIND,KIKDRW(GIND)
            ENDIF
C
            IF(GTYP.EQ.TSCR) THEN
                IF(SCRDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=SCRDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TSCR),GIND,SCRDRW(GIND)
            ENDIF
C
            IF(GTYP.EQ.TTSL) THEN
	        DO 10 K=1,MAXSRW
                    IF(TSLDAT(K,GIND).NE.DAYCDC) GOTO 10
                    GAMCNT=GAMCNT+1
                    DRAWS(GAM)=TSLDRW(GIND)
                    WRITE(6,901) IAM(),GTNAMES(TTSL),GIND,TSLDRW(GIND)
	            GOTO 100
10	        CONTINUE
            ENDIF
C
            IF (GTYP .EQ. TBNG) THEN
                IF (BNGDAT(CURDRW,GIND) .NE. DAYCDC) GOTO 100
                GAMCNT = GAMCNT+1
                DRAWS(GAM) = BNGDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TBNG),GIND,BNGDRW(GIND)
            ENDIF
C
            IF(GTYP.EQ.TSSC) THEN
                IF(SSCDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=SSCDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TSSC),GIND,SSCDRW(GIND)
            ENDIF
C
            IF(GTYP.EQ.TTRP) THEN
                IF(TRPDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=TRPDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TTRP),GIND,TRPDRW(GIND)
            ENDIF
C
            IF(GTYP.EQ.TSTR) THEN
                IF(STRDAT(GIND).NE.DAYCDC) GOTO 100
                GAMCNT=GAMCNT+1
                DRAWS(GAM)=STRDRW(GIND)
                WRITE(6,901) IAM(),GTNAMES(TSTR),GIND,STRDRW(GIND)
            ENDIF
C
100	CONTINUE
C
        IF(STOPMOD.EQ.WINMULTI) THEN                                      
          FLAG=0
        ELSE
       	  CALL PRMYESNO('Do you want to override games on reports? ', FLAG)
        ENDIF
	IF(FLAG.EQ.1) CALL FCANWIN_GETGAM(DRAWS,GAMCNT)
C
C INITIALIZE REPORTS
C
	DO 110 GAM = 1, MAXGAM
	    IF(DRAWS(GAM).EQ.0) GOTO 110
	    GTYP=GNTTAB(GAMTYP,GAM)
	    GIND=GNTTAB(GAMIDX,GAM)
            CALL CANWIN(VALREC,GAM,GTYP,GIND,DRAWS(GAM),COPY,1)
	    REP_INIT(GAM)=1
110	CONTINUE
C
C MAIN LOOP:
C SCAN VLF FOR CANCELLED WINNERS
C
        CALL IOPEN(SFNAMES(1,VLF),1,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),1,ST,0)
        CALL ITUBSIZE(1,TUBSIZ)
C
1000	CONTINUE
        CALL ISREAD(V4BUF,1,VLFBUF,ST)
        IF(ST.EQ.ERREND) THEN
            CALL ICLOSE(1,VLFBUF,ST)
            DO 200 I=1,MAXGAM
                IF(REP_INIT(I).EQ.1) THEN
 	            GTYP=GNTTAB(GAMTYP,I)
	            GIND=GNTTAB(GAMIDX,I)
                    CALL CANWIN(VALREC,I,GTYP,GIND,DRAWS(I),COPY,3)
                ENDIF
200	    CONTINUE
	    CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),2,ST,0)
	CALL LOGVAL(VALREC,V4BUF)
	GAM   = VALREC(VGAM)
	GTYP  = VALREC(VGTYP)
	GIND  = VALREC(VGIND)
C
        IF ((DRAWS(GAM).EQ.0).AND.(VALREC(VWCDC).EQ.DAYCDC)) THEN
          CALL DLOGVAL(VALREC,VDETAIL)
	  DRAW=0
          DO I=1,VALREC(VPZOFF)
	      DRAW=MAX(DRAW,VDETAIL(VDRW,I))
	  ENDDO
	  IF (REP_INIT(GAM).NE.1) THEN
             CALL CANWIN(VALREC,GAM,GTYP,GIND,DRAW,COPY,1)
	     REP_INIT(GAM)=1
	     WRITE(6,*) IAM(),' inserted a previously posponed draw'
             WRITE(6,901) IAM(),GTNAMES(GTYP),GIND,DRAW
	  ENDIF
          CALL CANWIN(VALREC,GAM,GTYP,GIND,DRAW,COPY,2)
	ENDIF

 	IF(DRAWS(GAM).EQ.0) GOTO 1000
C
        CALL CANWIN(VALREC,GAM,GTYP,GIND,DRAWS(GAM),COPY,2)
C
	GOTO 1000
C
901	FORMAT(1X,A,' Generating reports for ',A8,I1,' draw ',I5)
C
	END
