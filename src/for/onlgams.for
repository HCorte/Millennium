C ONLGAMS.FOR
C
C V16 11-DEC-2000 UXN TRANSLATED TO ENGLISH
C V15 01-FEB-2000 UXN TWFFLG added.
C V14 10-FEB-1998 UXN Report generated for all the games.
C V13 13-NOV-1997 UXN TMFREP.DEF added.
C V12 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V11 27-APR-1994 JXP COPY=0
C V10 24-JAN-1994 JXP Excluded oddset & Pitka games from report
C V09 17-OCT-1993 HXK FIX FOR CALCULATIONS OF TOTALS.
C V08 17-OCT-1993 HXK FIX FOR FORMAT BUG.
C V07 16-OCT-1993 HXK Print financial amounts using CMONY.
C V06 16-SEP-1993 SXH Display all of game name
C V05 07-SEP-1993 SXH De-commented SPOOL
C V04 03-SEP-1993 SXH Copy=1, added IAM()
C V03 13-OCT-1992 HJK CHANGED FOR SPEDEN GAME #!#
C V02 06-MAR-1991 PP  FIXED BUG: JOKER DURATION CHECK                         
C                     AMOUNT FIELDS OVERFLOW                          
C V01 12-DEC-1990 PP  INITIAL RELEASE FOR FINLAND
C                                                                               
C REPORT SHOWS TOTAL SALES BY GAME                                              
C  THE SALES OF CURRENT DRAW AND THE MULTIWEEK PORTION                          
C SEPARATED                                                                     
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
C Copyright 1992 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
        SUBROUTINE ONLGAMS
        IMPLICIT NONE

	INCLUDE '(LIB$ROUTINES)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB: GLOBAL.DEF'
	INCLUDE 'INCLIB: CONCOM.DEF'
	INCLUDE 'INCLIB: DESTRA.DEF'
	INCLUDE 'INCLIB: DATBUF.DEF'
	INCLUDE 'INCLIB: TNAMES.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:TMFREP.DEF'
C
	INTEGER*4   LUN
C
      	INTEGER*2 DATE(12)
      	INTEGER*4 GTOTALS(MAXGAM,6)
	STATIC	  GTOTALS
	INTEGER*4 PAGE
C
        LOGICAL	  FIRST  /.TRUE./
      	INTEGER*4 I,J,K,ST
      	INTEGER*4 COPY,GTYP,GIND,GAM
C
C BEGIN CODE ---------------------------------
C
      IF (EOF)        GOTO 500                                                  
      IF (FIRST) THEN
     	  FIRST = .FALSE.
      	  TYPE *, IAM(),'<<< TMREPS (ONLGAMS) V01 (ONLGAMS.REP)         >>>'
          TYPE *, IAM(),'<<< Online sales by game - Totals Report      >>>'
          TYPE *
C         TYPE *,'Enter number of report copies '

C         CALL INPNUM('Enter number of copies',COPY,0,10,EXT)
C         IF (EXT.NE.0) THEN
C           NOREP = .TRUE.
C           GOTO 1000
C         ENDIF
          COPY=0
C
C CLEAR/SET VARIABLES                                                           
C -------------------
          CALL FASTSET(0,GTOTALS,MAXGAM*6)
C
C GET TODAYS DATE                                                               
C ---------------
          DATE(VCDC)=DAYCDC
          CALL CDATE(DATE)
      ENDIF
C
      IF (TRABUF(TTYP).NE.TWAG)    GOTO 1000                                    
      IF (TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TSTAT).NE.FRAC) GOTO 1000
C
C     GET GAME TYPE AND INDEX
C     -----------------------
      IF(TRABUF(TGAM).LT.1.OR.TRABUF(TGAM).GT.MAXGAM) THEN                      
         TYPE*,IAM(),'Invalid game number found, serial: ',TRABUF(TSER)
         GOTO 1000                                                              
      ENDIF                                                                     

      GTYP = GNTTAB(GAMTYP,TRABUF(TGAM))                                        

      GAM = TRABUF(TGAM)                                                        
      GIND = GNTTAB(GAMIDX,TRABUF(TGAM))                                        

      IF(GAM.LT.1.OR.GAM.GT.MAXGAM) GOTO 1000                                   
      IF(GIND.LT.1.OR.GIND.GT.MAXIND) GOTO 1000                                 

      IF (TRABUF(TWFFLG).NE.0) GOTO 1000                 

      IF(TRABUF(TWDUR).NE.1) THEN                                               
        GTOTALS(GAM,2)=GTOTALS(GAM,2)+                                          
     *                 (TRABUF(TWAMT)*(TRABUF(TWDUR)-1))                 
        GTOTALS(GAM,1)=GTOTALS(GAM,1)+TRABUF(TWAMT)                     
      ELSE                                                                      
         GTOTALS(GAM,1)=GTOTALS(GAM,1)+TRABUF(TWAMT)                    
      ENDIF                                                                     

      GTOTALS(GAM,3)=GTOTALS(GAM,3) +                                           
     *               (TRABUF(TWAMT)*TRABUF(TWDUR))                     

      IF(TRABUF(TWKAMT).EQ.0) GOTO 1000                                         
      IF(TRABUF(TWKDUR).NE.1) THEN        !V02: TWDUR -> TWKDUR                 
         GTOTALS(GAM,5)=GTOTALS(GAM,5) +                                        
     *                  (TRABUF(TWKAMT)*(TRABUF(TWKDUR)-1))              
         GTOTALS(GAM,4)=GTOTALS(GAM,4)+TRABUF(TWKAMT)                   
      ELSE                                                                      
         GTOTALS(GAM,4)=GTOTALS(GAM,4)+TRABUF(TWKAMT)                   
      ENDIF                                                                     

      GTOTALS(GAM,6)=GTOTALS(GAM,6) +                                           
     *               (TRABUF(TWKAMT)*TRABUF(TWKDUR))                   

      GOTO 1000                                                                 

C PRODUCE THE REPORT
C ------------------

500   CONTINUE                                                                  

      ST = LIB$GET_LUN(LUN)
      IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
      CALL ROPEN('ONLGAMS.REP',LUN,ST)                                        
      IF (ST.NE.0) THEN                                                         
         TYPE*,IAM(),'Error opening ONLGAMS.REP > ',ST
         CLOSE(LUN)                                                           
         CALL GSTOP(GEXIT_FATAL)
      ENDIF                                                                     
      PAGE = 0
      CALL TITLE('ONLINE SALES BY GAME',                                    
     *           ' ONLGAMS ',1,LUN,PAGE,DAYCDC)                               

      WRITE(LUN,9001)                                                         
      WRITE(LUN,9002)                                                         

      DO 600 I=1,MAXGAM                                                         
         IF (GTOTALS(I,3).EQ.0.AND.GTOTALS(I,6).EQ.0) THEN              
            GOTO 600                                                            
         ELSE                                                                   
            WRITE(LUN,9004) (GLNAMES(K,I),K=1,4),                             
     *                        (CMONY(GTOTALS(I,J),12,BETUNIT),
     *                         J=1,6)
                                          
         ENDIF                                                                  
600   CONTINUE                                                                  
C
C CLOSE THE REPORT AND SPOOL IT TO THE PRINTER.
C
      CLOSE(UNIT=LUN)
      ST = LIB$FREE_LUN(LUN)	                                                         
      CALL SPOOL('ONLGAMS.REP',COPY,ST)                                         
C
1000  CONTINUE                                                                  
      RETURN                                                                    
C     ======================= FORMAT STATEMENTS ====================            
C                                                                
9001  FORMAT (//,' GAME',22X,'CURRENT',9X,'DURATION',23X,'JOKER',
     *        10X,'JOKER',10X,'JOKER')                           
9002  FORMAT (26X,'DRAW'   ,13X,'SHARE',11X,'TOT.',8X,'CURRENT DRAW',
     *        7X,'DURATION',6X,'  TOT.',/)                           
9004  FORMAT (1X,4A4,2X,A12,3X,A12,3X,A12,                                
     *        5X,A12,3X,A12,3X,A12)                                       

      END                                                                       
