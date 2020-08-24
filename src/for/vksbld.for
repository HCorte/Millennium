C VKSBLD.FTN                                                                    
C 
C V16 30-MAR-2015 MTK Modified Super 14 game
C V15 28-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C V14 11-DEC-2000 JHR TRANSLATION TO ENGLISH
C V13 29-JUN-2000 UXN P(SPTTCF) not needed any more.
C V12 25-MAY-2000 UXN Scan draw files from DSPBSD up to MIN(DSPESD,DAYCDC)
C V11 10-MAR-2000 OXK SPGNBR used with SPSTAB, NOT NUMROWS
C V10 01-MAR-2000 UXN P(SPTTCF) added.
C V09 22-FEB-2000 OXK Optimized handling multiple game indexes (Vakio changes)
C V08 01-FEB-2000 OXK Removed hardcoded GIND=1 etc. (Vakio changes)
C V07 05-JAN-2000 UXN Draw file scanning added.
C V06 29-APR-1999 RXK Stopsys optimization (some IAM()s added).
C V05 30-JUL-1998 RXK Fastset removed, Sports part only cleared
C V04 26-JAN-1996 RXK Rfss 94166. Fix for calculation of statistics.
C V03 12-DEC-1994 PXB Make table setup work in the same way as UPDSTA:FOR
C V02 08-JAN-1994 HXK muliply stfsec by 256 for VAX.
C V01 12-FEB-1993 HJK INITIAL RELEASE FOR FINLAND
C                                                                               
C PROGRAM TO BUILD VAKIO STATS FILE FROM TCF and from draw files
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
C Copyright 2000 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      PROGRAM VKSBLD                                                            
      IMPLICIT NONE                                              
                                                             
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'

      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:SPTCOM.DEF'
      INCLUDE 'INCLIB:PRMLOG.DEF'
      INCLUDE 'INCLIB:DESLOG.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:RECDAF.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
                                               

      INTEGER*4 LOGREC(LREC*3)                                                  
      INTEGER*4 I,GIND,ST,DUMMY                                       
      LOGICAL   EOF /.FALSE./, NEW
      INTEGER*4 DRAW(NUMSPT), GNUM
      INTEGER*4 FILES(5,60), FILNUM, FILCNT, FDB(7), DAFFDB(7)
      INTEGER*4 CDC
      CHARACTER*20 CFILES(60)
      EQUIVALENCE (FILES,CFILES)
      LOGICAL*4 NEED_TCF
C                                                                               
C CALL  COPYRITE  SUBROUTINE                                                    
C                                                                               
      CALL COPYRITE
C                                                                               
C READ SCF RECORD                                                               
C                                                                               
      CALL GETSCONF(SCFREC,ST)
      IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)

C Read draw files. 
C
      FILCNT = 0
      CALL OPENW(2,SFNAMES(1,DAF),4,0,0,ST)
      CALL IOINIT(DAFFDB,2,DAFSEC*256)
      IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)

C
      NEED_TCF = .FALSE.
      DO 1000 GIND = 1, NUMSPT

      GNUM = SCFGTN(TSPT,GIND)
      IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) GOTO 1000
C                                                                               
      DRAW(GIND) = MAX(SPTDRW(GIND), DAYHDR(GNUM))
      IF (DRAW(GIND).EQ.0) GOTO 1000
      IF(SPTSTS(GIND).GT.GAMOPN) DRAW(GIND) = DRAW(GIND) + 1                   
      WRITE(6,9009)IAM(),(GLNAMES(I,GNUM),I=1,4),DRAW(GIND) 
      IF(SPTMLT(GIND).GT.1) NEED_TCF = .TRUE.
      IF(DRAW(GIND).NE.SPTDRW(GIND)) GOTO 1000
C
      CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
      CALL IOINIT(FDB,1,DSPSEC*256)
      IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
      CALL READW(FDB,DRAW(GIND),DSPREC,ST)
      IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,1)
      CALL CLOSEFIL(FDB)

      DO 50 CDC=DSPBSD,MIN(DSPESD,DAYCDC)
         CALL READW(DAFFDB,CDC,DAFREC,ST)
         IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),2,ST,CDC)
         IF(DAFSTS.EQ.DNOSAL) GOTO 50
         FILCNT=FILCNT+1
         WRITE (CFILES(FILCNT),9006) GSNAMES(GNUM),CDC
50    CONTINUE

1000  CONTINUE

      CALL CLOSEFIL(DAFFDB)
C
      CALL UPD_VAKSTA(TRABUF, DRAW, 1)
      IF(.NOT.NEED_TCF) GOTO 101
C                                                                               
C OPEN CARRYOVER FILE                                                           
C                                                                               
      CALL IOPEN(SCFSFN(1,TCF),1,LREC*2,LCDC,LSER*2-1,ST)                       
      IF(ST.NE.0) CALL FILERR(SCFSFN(1,TCF),1,ST,0)                             
C
      WRITE(6,9007) IAM(),(SCFSFN(I,TCF),I=1,5)
C                                                                               
C READ ALL TRANSACTIONS IN TCF                                                  
C                                                                               
20    CONTINUE                                                                  

      CALL READTCF(LOGREC,1,EOF)                                 
      IF(EOF) GOTO 100                                                         
      CALL LOGTRA(TRABUF,LOGREC)                                                

      CALL UPD_VAKSTA(TRABUF, DRAW, 2)
                                                                               
      GOTO 20                                                                   
                                                                               
100   CONTINUE                                                                  
      CALL ICLOSE(1,DUMMY,ST)                                                   
C
101   CONTINUE
C
      DO 70 FILNUM=1, FILCNT

         CALL OPENW(4,FILES(1,FILNUM),4,0,0,ST)
         IF(ST.NE.0) CALL FILERR(FILES(1,FILNUM),1,ST,0)
         CALL IOINIT(FDB,4,128*256)

         WRITE(6,9005) IAM(),CFILES(FILNUM)
	 NEW = .TRUE.
	 EOF = .FALSE.

75	 CONTINUE

	 CALL READDRWN(LOGREC,FDB,EOF,NEW)
	 IF(EOF) THEN
	    CALL CLOSEFIL(FDB)
	    GOTO 70
	 ENDIF
         CALL LOGTRA(TRABUF,LOGREC)
      	 
         CALL UPD_VAKSTA(TRABUF, DRAW, 2)

	 GOTO 75                                                               

70    CONTINUE

C
      CALL UPD_VAKSTA(TRABUF, DRAW, 3)
C
      CALL GSTOP(GEXIT_SUCCESS)  
C
9005  FORMAT(1X,A,'Scanning file ',A)
9006  FORMAT('DRAW',':',A4,I4.4,'.FIL')
9007  FORMAT(1X,A,'Scanning file ',5A4)
9009  FORMAT(1X,A,'Building statistics for ',4A4,' draw ',I4)
      END                                                                       
C
C Update SPORTS statistics.
C
      SUBROUTINE UPD_VAKSTA(TRABUF, DRAW, FUN)
      IMPLICIT NONE
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'

      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:SPTCOM.DEF'
      INCLUDE 'INCLIB:PRMLOG.DEF'
      INCLUDE 'INCLIB:DESLOG.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:RECSTF.DEF'
C 
      INTEGER*4 ROWS(SPGNBR,MAXBRD)
      INTEGER*4 RROWS(2,TGGNBR,MAXBRD)
      INTEGER*4 J, FUN, DRAW(NUMSPT)
      INTEGER*4 STFFDB(7)                                 
      INTEGER*4 COUNTER(SPGNBR,3),K,L,PTR
      INTEGER*4 INDEX_TAB(16),LOCAL_TAB(16),IND(3)
      INTEGER*4 SYSNR,NUMROWS,INDEX,CNT,NUM_BOARDS
      INTEGER*4 BITCNT(0:15) /0,1,1,2,1,2,2,3, 0,1,1,1,1,1,1,1/
      INTEGER*4 CNV_TAB(0:7,7) !TABLE FOR NEW WINNER TRANSFORMATION
      INTEGER*4 VAKAMT(NUMSPT)
      INTEGER*4 ST, GIND, I, GNUM, BCNT
      DATA CNV_TAB/0,0,0,1, 0,1,2,0
     *		    ,0,0,0,2, 0,4,4,0
     *		    ,0,0,0,3, 0,5,6,0
     *		    ,0,0,0,4, 0,2,1,0
     *		    ,0,0,0,5, 0,3,3,0
     *		    ,0,0,0,6, 0,6,5,0
     *		    ,0,0,0,7, 0,7,7,0/
C

 
      GOTO(100, 200, 300) FUN
      TYPE*,IAM(),'Invalid function >', FUN
      RETURN
C
C OPEN SPORTS statistics file and initialize some variables.
C
100   CONTINUE


      CALL OPENW(3,SFNAMES(1,STF),4,0,0,ST)
      CALL IOINIT (STFFDB,3,STFSEC*256)
      IF(ST.NE.0) CALL FILERR(SFNAMES(1,STF),1,ST,0)
      CALL READW(STFFDB,1,STFREC,ST)
      IF(ST.NE.0) CALL FILERR(SFNAMES(1,STF),3,ST,1)

      CALL FASTSET(0,VAKAMT,NUMSPT)
      CALL FASTSET(0,STFSPT_CUP,NUMSPT)
      CALL FASTSET(0,STFSPT_TAB1,SPGNBR*7*NUMSPT)
      CALL FASTSET(0,STFSPT_TAB2,SPGNBR*3*NUMSPT)
      CALL FASTSET(0,STFSPT,NUMTOT*2*MAXBRD*(MAXBRD+1)*NUMSPT)
C
      RETURN
C
C Update SPORTS statistics.
C
200   CONTINUE
C
      GIND = TRABUF(TGAMIND)
      IF(TRABUF(TGAMTYP).NE.TSPT) RETURN                                      
      IF(TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TSTAT).NE.FRAC.AND.
     *   TRABUF(TSTAT).NE.EXCH)   RETURN               
      IF(TRABUF(TFRAC).NE.MAXFRC(TRABUF(TGAM))) RETURN
C                                                                               
C CHECK DRAW                                                                    
C                                                                               
      IF (TRABUF(TWBEG).GT.DRAW(GIND)) RETURN
      IF (TRABUF(TWEND).LT.DRAW(GIND)) RETURN

      CALL GETROW(TRABUF,ROWS,RROWS)                                                  

      BCNT = 0
      IF(TRABUF(TWSPFRG).NE.0) BCNT = 1
C
      DO 50 J=1,TRABUF(TWNBET)                                                  
      	 CALL FASTSET(1,COUNTER(1,1),SPGNBR*3)
C
      	 IF (TRABUF(TWSYST) .NE. REDSYS) THEN 

      	    DO 30 I=1,TRABUF(TWSRW)-BCNT
      	       IF(ROWS(I,J).EQ.3 .OR. ROWS(I,J).EQ.5 .OR. 
     *		  ROWS(I,J).EQ.6) THEN
      		  DO K=1,TRABUF(TWSRW)-BCNT
      		     IF(K.NE.I) THEN
      			DO L=1,3 
      			   COUNTER(K,L)=2*COUNTER(K,L)
      			ENDDO
      		     ENDIF 
      		  ENDDO
      	       ELSEIF(ROWS(I,J).EQ.7) THEN    
      		  DO K=1,TRABUF(TWSRW)-BCNT
      		     IF(K.NE.I) THEN
      			DO L=1,3
      			   COUNTER(K,L)=3*COUNTER(K,L)         
      			ENDDO
      		     ENDIF
      		  ENDDO
      	       ENDIF
30    	    CONTINUE
C
      	    DO 40 I=1,TRABUF(TWSRW)-BCNT
      	       IF(ROWS(I,J).EQ.1) THEN
      		  STFSPT_TAB1(I,1,GIND)=STFSPT_TAB1(I,1,GIND)+1
      		  STFSPT_TAB2(I,1,GIND)=STFSPT_TAB2(I,1,GIND)+COUNTER(I,1)  
      	       ELSEIF(ROWS(I,J).EQ.2) THEN 
      		  STFSPT_TAB1(I,2,GIND)=STFSPT_TAB1(I,2,GIND)+1             
      		  STFSPT_TAB2(I,2,GIND)=STFSPT_TAB2(I,2,GIND)+COUNTER(I,2)  
      	       ELSEIF(ROWS(I,J).EQ.3) THEN 
      		  STFSPT_TAB1(I,4,GIND)=STFSPT_TAB1(I,4,GIND)+1          
      		  STFSPT_TAB2(I,1,GIND)=STFSPT_TAB2(I,1,GIND)+COUNTER(I,1)
      		  STFSPT_TAB2(I,2,GIND)=STFSPT_TAB2(I,2,GIND)+COUNTER(I,2)
      	       ELSEIF(ROWS(I,J).EQ.4) THEN
      		  STFSPT_TAB1(I,3,GIND)=STFSPT_TAB1(I,3,GIND)+1          
      		  STFSPT_TAB2(I,3,GIND)=STFSPT_TAB2(I,3,GIND)+COUNTER(I,3)
      	       ELSEIF(ROWS(I,J).EQ.5) THEN
      		  STFSPT_TAB1(I,5,GIND)=STFSPT_TAB1(I,5,GIND)+1
      		  STFSPT_TAB2(I,1,GIND)=STFSPT_TAB2(I,1,GIND)+COUNTER(I,1)
      		  STFSPT_TAB2(I,3,GIND)=STFSPT_TAB2(I,3,GIND)+COUNTER(I,3)
      	       ELSEIF(ROWS(I,J).EQ.6) THEN
      		  STFSPT_TAB1(I,6,GIND)=STFSPT_TAB1(I,6,GIND)+1      
      		  STFSPT_TAB2(I,2,GIND)=STFSPT_TAB2(I,2,GIND)+COUNTER(I,2)
      		  STFSPT_TAB2(I,3,GIND)=STFSPT_TAB2(I,3,GIND)+COUNTER(I,3)
      	       ELSEIF(ROWS(I,J).EQ.7) THEN
      		  STFSPT_TAB1(I,7,GIND)=STFSPT_TAB1(I,7,GIND)+1   
      		  STFSPT_TAB2(I,1,GIND)=STFSPT_TAB2(I,1,GIND)+COUNTER(I,1)
      		  STFSPT_TAB2(I,2,GIND)=STFSPT_TAB2(I,2,GIND)+COUNTER(I,2)
      		  STFSPT_TAB2(I,3,GIND)=STFSPT_TAB2(I,3,GIND)+COUNTER(I,3)
      	       ENDIF                      
40    	    CONTINUE
      	 ENDIF     !for non-reduced systems
C
C REDUCED SYSTEMS
C
      	 IF (TRABUF(TWSYST) .EQ. REDSYS) THEN 
C
      	    SYSNR=TRABUF(TWSYSN)
      	    IND(1)=SPSNUM(3,SYSNR)
      	    IND(2)=SPSNUM(2,SYSNR)
      	    IND(3)=1
CV08   	    NUMROWS=SPSNUM(4,SYSNR)
      	    NUMROWS=TRABUF(TWSRW)-BCNT
      	    DO K=1,NUMROWS 
      	       INDEX_TAB(IND(BITCNT(ROWS(K,J))))=K
      	       IND(BITCNT(ROWS(K,J)))=IND(BITCNT(ROWS(K,J)))+1
      	    ENDDO
      	    PTR=SPSPTR(SYSNR)
      	    NUM_BOARDS=SPSTAB(PTR)
      	    DO L=1,NUM_BOARDS
      	       DO 45, K=1,SPGNBR
      		  PTR=PTR+1
	          IF (K.GT.NUMROWS) GOTO 45
      		  INDEX=INDEX_TAB(K)
      		  CNT=BITCNT(ROWS(INDEX,J))
      		  IF (CNT.EQ.3) THEN
      		     LOCAL_TAB(INDEX)=SPSTAB(PTR)
      		  ELSEIF (CNT.EQ.2) THEN
      		     LOCAL_TAB(INDEX)=CNV_TAB(ROWS(INDEX,J),SPSTAB(PTR))
      		  ELSEIF (CNT.EQ.1) THEN
      		     LOCAL_TAB(INDEX)=IAND(ROWS(INDEX,J),7)
      		  ENDIF
45	       CONTINUE
      	       DO K=1,NUMROWS
      		  IF(LOCAL_TAB(K).EQ.1) THEN                      
      		     STFSPT_TAB2(K,1,GIND)=STFSPT_TAB2(K,1,GIND)+1
      		  ELSEIF(LOCAL_TAB(K).EQ.2) THEN                  
      		     STFSPT_TAB2(K,2,GIND)=STFSPT_TAB2(K,2,GIND)+1
      		  ELSEIF(LOCAL_TAB(K).EQ.4) THEN                  
      		     STFSPT_TAB2(K,3,GIND)=STFSPT_TAB2(K,3,GIND)+1
      		  ENDIF
      	       ENDDO
      	    ENDDO
      	 ENDIF         !reduced systems

50    CONTINUE                                            

      STFSPT_CUP(GIND)=STFSPT_CUP(GIND)+1
      VAKAMT(GIND)=VAKAMT(GIND)+TRABUF(TWAMT)

      RETURN
C
C Update and close SPORTS statistics file 
C
300   CONTINUE

      WRITE(6,9004) IAM(),(SFNAMES(I,STF),I=1,5)

      CALL WRITEW(STFFDB,1,STFREC,ST)
      IF(ST.NE.0) CALL FILERR(SFNAMES(1,STF),3,ST,1)
      CALL CLOSEFIL(STFFDB)

      WRITE(6,9005)IAM()
      DO 400 GIND=1,NUMSPT
          GNUM = GTNTAB(TSPT,GIND)
	  IF (GNUM.LT.1.OR.GNUM.GT.MAXGAM) GOTO 400
      	  WRITE(6,9001) IAM(),(GLNAMES(I,GNUM),I=1,4),
     *		DRAW(GIND),STFSPT_CUP(GIND),CSMONY(VAKAMT(GIND),11,BETUNIT)
400   CONTINUE
      WRITE(6,9003) IAM()

      RETURN

9001  FORMAT(1X,A,4A4,2X,I4,2X,I11,2X,A11)
9003  FORMAT(1X,A,'Sports stats   file building complete')
9004  FORMAT(1X,A,'Updating file ', 5A4)
9005  FORMAT(1X,A,'Game name         Draw      Coupons       Amount')
      END
