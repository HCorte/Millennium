C CANREP1.FOR
C
C GENERATES A CANCELLED WAGER REPORT                                            
C
C V05 20-MAR-2011 GPW NUMAGT=12288
C V04 20-APR-2010 RXK Changes for ePassive
C V03 29-MAY-2008 CPH IF CHANGE, PROBLEM WITH CNTMAX
C V02 13-APR-2000 UXN Fix for JOKER amount
C V01 13-NOV-1997 UXN Initial release (produced from CANREP.FOR)
C                                                                               
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
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
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
        SUBROUTINE CANREP1
        IMPLICIT NONE
	INCLUDE '(LIB$ROUTINES)'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:TMFREP.DEF'
C
C VARIABLES
C                                      
        INTEGER*4  LUN	      ! logical unit for report file
C
	INTEGER*4  LINMAX                        !
        PARAMETER(LINMAX=48)                      

        INTEGER*4  CNTMAX                        !
        PARAMETER(CNTMAX=20000)                   
C                                                                               

        INTEGER*4  SORT(9,CNTMAX)                !
	STATIC	   SORT
        INTEGER*4  TOTPAD(DOLAMT+1)              !
        INTEGER*4  PAGE                          !
        INTEGER*4  SER                           !
        INTEGER*4  SRTCNT                        !
	STATIC	   SRTCNT
        INTEGER*4  LINCNT                        !
        INTEGER*4  CDCTRA                        !
	STATIC	   CDCTRA
        INTEGER*4  JULTRA                        !
        INTEGER*4  COPY                          !
        INTEGER*4  WSER                          !
        INTEGER*4  WCHK                          !
        INTEGER*4  I                             !
        INTEGER*4  CSER                          !
        INTEGER*4  CCHK                          !
        INTEGER*4  KGNUM                         !
        INTEGER*4  MGNUM                         !
        INTEGER*4  MGTYP                         !
        INTEGER*4  MGIND                         !
        INTEGER*4  KGIND                         !
        INTEGER*4  GAMPAD                        !
        INTEGER*4  KGTYP                         !
        INTEGER*4  S,ST

        INTEGER*4  TOTSUMS(NO_BALSUMS)            !
        INTEGER*4  GAMESUMS(MAXGAM,NUMFIN,NUMTOT) !
        INTEGER*4  RAPCODE                        !
        INTEGER*2  DATBUF(LDATE_LEN)              !
        REAL*8     TOTREAL                       !
        CHARACTER*4  MGAME                       !
        CHARACTER*4  KGAME                       !                            
        CHARACTER*6 REPNAM/'CANREP'/
        CHARACTER*41 HEAD                        !
        CHARACTER*8 IAGT_NO                      ! FUNCTION: FORMAT AGT NUMBER
	LOGICAL FIRST/.TRUE./
C
	IF(FIRST) THEN
	  FIRST = .FALSE.
          PAGE=0
          SER=1
          SRTCNT=0
          CDCTRA=0
          JULTRA=0
          CALL FASTSET(0,TOTPAD,DOLAMT+1)
	  CALL FASTSET(0,SORT,SIZEOF(SORT)/4)
          COPY=0
	  ST = LIB$GET_LUN(LUN)
	  IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
	  CALL ROPEN('CANREP.REP',LUN,ST)
	  IF(ST.NE.0) THEN
	     TYPE*,IAM(),'CANREP - Error opening CANREP.REP >',ST
	     CALL GSTOP(GEXIT_FATAL)
	  ENDIF
	ENDIF
C
	IF(EOF) GOTO 100
C                                                                               
C PROCESS ALL GOOD CANCELLATIONS                                                
C                                                                               
        IF(CDCTRA.LE.0) THEN
            CDCTRA=TRABUF(TCDC)
            DATBUF(VCDC)=CDCTRA
            CALL LCDATE(DATBUF)
            JULTRA=DATBUF(VJUL)
        ENDIF
        CALL OUTGEN(CDCTRA,TRABUF(TSER),WSER,WCHK)
C                                                                               
C STUFF SORT ARRAY                                                              
C                                                                               
        SRTCNT = SRTCNT+1
C V03	IF(SRTCNT.GT.CNTMAX) THEN
C V03	  TYPE*,IAM(),'CANREP - Sort Table is Full...'
C V03	  RETURN
C V03	ENDIF 
        SORT(1,SRTCNT) = TRABUF(TAGT)
        SORT(2,SRTCNT) = TRABUF(TTER)
        SORT(3,SRTCNT) = TRABUF(TGAM)
        SORT(4,SRTCNT) = TRABUF(TWKGME)
        SORT(5,SRTCNT) = WSER
        SORT(6,SRTCNT) = WCHK
        SORT(7,SRTCNT) = TRABUF(TWCSER)
        SORT(8,SRTCNT) = TRABUF(TWTOT)
        SORT(9,SRTCNT) = TRABUF(TWKAMT)*TRABUF(TWKDUR)
C
	RETURN
C                                                                               
C GENERATE A REPORT
C
100	CONTINUE
C                                                                               
C ENCODE REPORT HEADER                                                          
C                                                                               
        DATBUF(VCDC)=CDCTRA
        CALL LCDATE(DATBUF)
        WRITE(HEAD,9000) DATBUF(VDAY),DATBUF(VMON),DATBUF(VYEAR2)
        IF (SRTCNT .EQ. 0) THEN
            TYPE *, IAM(),' NO CANCELLED WAGERS FOUND'
            WRITE(LUN,9060)
            RETURN
        ENDIF
C                                                                               
C SORT ON AGENT NUMBER AND MAIN GAME                                            
C                                                                               
        TYPE*,IAM(),'CANREP - Generating report sorted by Agent',
     *              ' and Game number '
        CALL I4MSORT(SRTCNT,SORT,9)
        TYPE*,IAM(),'CANREP - Sort completed '
C                                                                               
C ENCODE REPORT                                                                 
C                                                                               
        LINCNT=70
        DO I=1,SRTCNT                                                        
            LINCNT=LINCNT+1
            IF(LINCNT.GT.LINMAX) THEN
                CALL TITLE(HEAD,REPNAM,0,LUN,PAGE,DAYCDC)
                WRITE(LUN,*)
                WRITE(LUN,9010)
                WRITE(LUN,9020)
                WRITE(LUN,*)
                LINCNT=7
            ENDIF
C                                                                               
C GET CANCELLING SERIAL NUMBER                                                  
C                                                                               
            CALL OUTGEN(CDCTRA,SORT(7,I),CSER,CCHK)
C                                                                               
            KGNUM=SORT(4,I)
            MGNUM=SORT(3,I)
            MGTYP=GNTTAB(GAMTYP,MGNUM)
            MGIND=GNTTAB(GAMIDX,MGNUM)
            WRITE(MGAME,9030) GSNAMES(MGNUM)
                                      
            IF(SORT(9,I).LE.0.OR.MGTYP.EQ.TKIK) THEN
                KGAME='----'
                KGIND=0
                GAMPAD=SORT(8,I)
                SORT(9,I)=0
            ELSE
                KGTYP=GNTTAB(GAMTYP,KGNUM)
                KGIND=GNTTAB(GAMIDX,KGNUM)
                WRITE(KGAME,9030) GSNAMES(KGNUM)
                GAMPAD=SORT(8,I)-SORT(9,I)
            ENDIF

            TOTPAD(TRACNT)   = TOTPAD(TRACNT)+1
            TOTPAD(DOLAMT)   = TOTPAD(DOLAMT)+GAMPAD
            TOTPAD(DOLAMT+1) = TOTPAD(DOLAMT+1)+SORT(9,I)
C                                                                               
            WRITE(LUN,9040)   IAGT_NO(SORT(1,I)),
     *                        SORT(2,I),            
     *                        MGAME,
     *                        MGIND,
     *                        KGAME,
     *                        KGIND,                                 
     *                        JULTRA,
     *                        SORT(5,I),
     *                        SORT(6,I),
     *                        JULTRA,
     *                        CSER,
     *                        CCHK,             
     *                        CMONY(GAMPAD,10,BETUNIT),
     *                        CMONY(SORT(9,I),10,BETUNIT)
        END DO
C                                                                               
C WRITE TOTAL LINE                                                              
C                                                                               
        WRITE(LUN,9050) TOTPAD(TRACNT),
     *                    (CMONY(TOTPAD(S),10,BETUNIT),S=DOLAMT,DOLAMT+1)
C
C CLOSE THE REPORT FILE.
C
	CLOSE(LUN)  
	ST = LIB$FREE_LUN(LUN)
C
C SPOOL THE REPORT.
C
	CALL SPOOL('CANREP.REP',COPY,ST)
C                                                                               
C       TOTAL TO BALANSFILE (BALWRI)     V02
C                                                                               
        TOTSUMS(4) = TOTPAD(DOLAMT) + TOTPAD(DOLAMT+1)
        RAPCODE = 7
        CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)
C                                                                               
C FORMAT STATEMENTS FOR CANREP REPORT                                           
C                                                                               
9000    FORMAT('CANCELLED TICKETS REPORT FOR: ',I2.2,'.',I2.2,'.',I4.4)
9010    FORMAT(2X,'AGENT#',2X,'TERMINAL#',2X,'GAME/IND',
     *         2X,'OTHER/IND',6X,'SELLING SERIAL#',
     *         6X,'CANCELLING SERIAL#',6X,'GAME PAID',
     *         6X,'OTHER PAID')
9020    FORMAT(1X,132('='))
9030    FORMAT(A4)
9040    FORMAT(2X,A8,4X,I5,2X,A4,'/',I2.2,3X,A4,'/',I2.2,
     *         8X,I3.3,'-',I8.8,'-',I3.3,5X,I3.3,'-',I8.8,'-',I3.3,
     *         7X,A10,6X,A10)
9050    FORMAT(//,50X,' Total wagers cancelled: ',I8,' for: ',1X,
     *         A10,6X,A10)
9060    FORMAT(1X,'NO CANCELLED WAGERS FOUND IN TMF FILE')
        END
