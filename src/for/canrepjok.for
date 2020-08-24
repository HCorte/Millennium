C CANREPJOK
C
C V06 30-NOV-2010 FJG TWEMSER/TWEMCHK replaced by TWLNKSER/TWLNKCHK
C V05 12-OCT-2009 FJG TAKE INTO ACCOUNT THE EM CANCEL STATUS (OUT)
C V04 26-AUG-2009 FJG GENERATE A REPORT, EVEN IF IT IS EMPTY
C V01 01-APR-2009 MMO INITIAL RELEASE FOR PORTUGAL                             
C                                                                               
C GENERATES A CANCELED WAGER REPORT FOR EM PLAYED WITH JOKER
C WHICH ARE NOT CANCELED                                           
C                                                                               
C                                                                               
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
C Copyright 1991 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C
C=======OPTIONS/CHECK=NOOVERFLOW/EXT                       
        PROGRAM CANREPJOK                                    
        IMPLICIT NONE

        INCLUDE '(LIB$ROUTINES)'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
C                                                                               
C                                                                               
        INTEGER*4  REPLU                         
        INTEGER*4  LINMAX                        
        PARAMETER(LINMAX=48)                     

        INTEGER*4  CNTMAX                        
        PARAMETER(CNTMAX=20000)                  
C                                                             

        INTEGER*4  LOGREC(LMUREC)                
        INTEGER*4  TOTPAD(DOLAMT+1)              
        INTEGER*4  NOCHECK0                      
        INTEGER*4  PAGE                          
        INTEGER*4  SER                           
        INTEGER*4  SRTCNT                        
        INTEGER*4  LINCNT                        
        INTEGER*4  CDCTRA                        
        INTEGER*4  JULTRA                        
        INTEGER*4  ST, ST_JOK, ST_EM             
        INTEGER*4  REC                           
        INTEGER*4  LUN_EM, LUN_JOK
        INTEGER*4  EM_LEN, JOK_LEN               
        INTEGER*4  POINTER_DATA(2)
        INTEGER*4  POINTER_DATA_JOK(2)
        INTEGER*4  POINTER_DATA_EM(2)
C
        INTEGER*4  TEMPSER
        INTEGER*4  TEMPCHK
C        
        INTEGER*4 I4TEMP
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP,I1TEMP)
C
        INTEGER*2  DATBUF(LDATE_LEN)             
C
        CHARACTER*1  BELL                        
        CHARACTER*15 REPNAM                      
        CHARACTER*44 HEAD                        
        CHARACTER*8 IAGT_NO
        CHARACTER*2 REV                      
C                                                
        LOGICAL EOF/.FALSE./     
        LOGICAL TOSORT                
        COMMON /NOCHECK0/ NOCHECK0 

        DATA REPNAM/'  CANREPJOK.REP'/
        DATA BELL/Z07/             
        DATA REV/'07'/
        DATA LUN_EM/1/
        DATA LUN_JOK/2/               


C                                                                               
C CALL  COPYRITE  SUBROUTINE                                                    
C                                  
        CALL COPYRITE              
C                                                                               
C SET / CLEAR VARIABLES                                                         
C                                  
        NOCHECK0=-1                
        PAGE=0                     
        SER=1                      
        SRTCNT=0                   
        LINCNT=7                  
        CDCTRA=DAYCDC
        JULTRA=0                   
        CALL FASTSET(0,TOTPAD,DOLAMT+1)       
C                                                                               
        TYPE*,IAM(),' '                       
        TYPE*,IAM(),'<<<<< CANREPJOK V ',REV,' >>>>>'
        TYPE*,IAM(),'<<<<< REPORT MISMATCH OF CANCELED JOKER TICKETS >>>>>'
        TYPE*,IAM(),' '                                                    
C                                                                               
C GET SYSTEM CONTROL INFORMATION                                               
C                                                                               
        CALL GETSCONF(SCFREC,ST)                                           
        IF(ST .NE. 0) THEN                                                   
          TYPE*,IAM(),' Unable to get system control info. ',BELL        
          CALL GPAUSE                                                    
        ENDIF                                                              
C                                                                               
C OPEN THE TRANSACTION FILE                                                     
C                                                                               
        CALL OPENW(PTMF,SCFSFN(1,PTMF),4,0,0,ST)                         
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,PTMF),1,ST,0)                   
        CALL TOPEN(PTMF)
C
C CREATE UNIT OF CANCELED EM WAGER
C
        CALL SORT_CREATE(LUN_EM, 8)
C
C CREATE UNIT OF CANCELED JOKER WAGER PLAYED WITH EM
C
        CALL SORT_CREATE(LUN_JOK, 8)                                                 
C                                                                               
C LOOP THROUGH THE TM                                                           
C                                                                               
100     CONTINUE  
        CALL READTMF(LOGREC,SER,EOF)                                     
        IF(EOF) GOTO 1000                                                
C                                                                               
C CONVERT TO INTERNAL TRANSACTION FORMAT                                        
C                                                                               
        CALL LOGTRA(TRABUF,LOGREC)
C        
        CALL FASTSET(0,POINTER_DATA,SIZEOF(POINTER_DATA)/4)                                                  
C                                                                               
C PROCESS ALL GOOD EM CANCELATIONS                                                
C                                                                               
        TOSORT = .FALSE.                                                       
        IF(TRABUF(TTYP).EQ.TEUR.AND.TRABUF(TSTAT).EQ.GOOD.AND.TRABUF(TEUTYP).EQ.TCAN.AND.TRABUF(TEUCST).EQ.0) TOSORT=.TRUE.
        IF(TRABUF(TGAMTYP).EQ.TLTO.AND.TRABUF(TSTAT).EQ.VOID.AND.TRABUF(TTYP).EQ.TWAG) TOSORT=.TRUE.
C
        IF(TOSORT) THEN
          IF(CDCTRA .LE. 0) THEN
            CDCTRA=TRABUF(TCDC)
            DATBUF(VCDC)=CDCTRA
            CALL LCDATE(DATBUF)
            JULTRA=DATBUF(VJUL)
          ENDIF
          IF(TRABUF(TTYP).EQ.TEUR) THEN
            I4TEMP     = TRABUF(TEUCWSER)
            I1TEMP(4)  = TRABUF(TEUCWCKD)
          ELSE
            CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),TEMPSER,TEMPCHK)
            I4TEMP     = TEMPSER
            I1TEMP(4)  = TEMPCHK
          ENDIF
          POINTER_DATA(1) = I4TEMP
          POINTER_DATA(2) = TRABUF(TSER)
          CALL SORT_ADD(LUN_EM, POINTER_DATA, ST)          
        ENDIF
C                                                                               
C PROCESS ALL GOOD JOKER WAGER PLAYED WITH EM                                                
C       
        IF(TRABUF(TSTAT) .EQ. GOOD .AND. TRABUF(TTYP) .EQ. TWAG .AND.
     *    TRABUF(TWLNKSER) .GT. 0) THEN
          I4TEMP     = TRABUF(TWLNKSER)
          I1TEMP(4)  = TRABUF(TWLNKCHK)
          POINTER_DATA(1) = I4TEMP                                                         
          POINTER_DATA(2) = TRABUF(TSER)
          CALL SORT_ADD(LUN_JOK, POINTER_DATA, ST)
        ENDIF                       
C        
        GOTO 100                                                         
C                                                                               
C END OF FILE                                                                   
C                                                                               
1000    CONTINUE                                                         
C                                                                               
C OPEN REPORT FILE                                                              
C                                                                               
	ST = LIB$GET_LUN(REPLU)
	IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
        CALL ROPEN(REPNAM,REPLU,ST)                                      
        IF(ST .NE. 0) THEN                                                 
          TYPE*,IAM(),REPNAM,' Open error status - ',ST                   
          CALL GSTOP(GEXIT_FATAL)                                      
        ENDIF                                                            
C                                                                               
C ENCODE REPORT HEADER                                                          
C                                                                               
        DATBUF(VCDC)=CDCTRA                                              
        CALL LCDATE(DATBUF)                                              
        WRITE(HEAD,9000) DATBUF(VDAY),DATBUF(VMON),DATBUF(VYEAR2)        
C=V04============================================================================
        CALL TITLE(HEAD,REPNAM(1:8),REC,REPLU,PAGE,DAYCDC)       
        WRITE(REPLU,*)                                           
        WRITE(REPLU,9010)                                        
        WRITE(REPLU,9020)                                        
        WRITE(REPLU,*)      
C=V04============================================================================  
C
C CHECK IF NO CANCELED EM WAGERS
C
        CALL SORT_GET_LEN(LUN_EM, EM_LEN)
        IF(EM_LEN .EQ. 0) THEN
          TYPE *, IAM(),' NO CANCELED EM WAGERS FOUND'
          WRITE(REPLU,9060)
          CALL USRCLOS1(REPLU)
          CALL USRCLOS1(PTMF)                                         
          CALL SORT_CLOSE_ALL()
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
C                                                                               
C SORT EM UNIT                                            
C
        CALL SORT_INDEX(LUN_EM, 0, 4, 1, 0, ST)                                                                              
C                                                                               
C ENCODE REPORT                                                                 
C
        CALL SORT_GET_LEN(LUN_JOK, JOK_LEN)
        ST_JOK = 0
        CALL SORT_GO_FIRST(LUN_JOK, ST_JOK)
        DO WHILE (ST_JOK .EQ. 0)                                                        
          CALL FASTSET(0,POINTER_DATA_JOK,SIZEOF(POINTER_DATA_JOK)/4)                                                  
          CALL SORT_GET_NEXT(LUN_JOK, POINTER_DATA_JOK, ST_JOK)
          ST_EM = 0
          CALL SORT_GO_FIRST(LUN_EM, ST_EM)
          DO WHILE (ST_EM .EQ. 0 .AND. ST_JOK .EQ. 0)            
            CALL FASTSET(0,POINTER_DATA_EM,SIZEOF(POINTER_DATA_EM)/4)                                                  
            CALL SORT_GET_NEXT(LUN_EM, POINTER_DATA_EM, ST_EM)
C
C CHECK IF CANCELED EM WAGER PLAYED WITH JOKER WHICH WAS NOT CANCELED
C
            IF(POINTER_DATA_JOK(1) .EQ. POINTER_DATA_EM(1)) THEN
              SRTCNT = SRTCNT + 1
              LINCNT = LINCNT + 1                                              
              IF(LINCNT .GT. LINMAX) THEN                                    
                CALL TITLE(HEAD,REPNAM(1:8),REC,REPLU,PAGE,DAYCDC)       
                WRITE(REPLU,*)                                           
                WRITE(REPLU,9010)                                        
                WRITE(REPLU,9020)                                        
                WRITE(REPLU,*)                                           
                LINCNT=7                                                 
              ENDIF                                                        
C
              CALL READTMF(LOGREC,POINTER_DATA_JOK(2),EOF)                                     
C                                                                               
              CALL LOGTRA(TRABUF,LOGREC)
C
              WRITE(REPLU,9040) IAGT_NO(TRABUF(TAGT)),
     *                          TRABUF(TTER),      
     *                          GTNAMES(TRABUF(TGAMTYP)),
     *                          TRABUF(TGAMIND),
     *                          TRABUF(TWLNKSER),
     *                          IAND(TRABUF(TWKICK),'00FFFFFF'X),
     *                          TRABUF(TCDC),TRABUF(TSER)

            ENDIF                
          ENDDO
        ENDDO
C
        CALL SORT_CLOSE_ALL()
        CALL USRCLOS1(PTMF)                                               
C                                                                               
C CLOSE REPORT FILE AND SPOOL REPORT                                            
C              
        IF(SRTCNT.EQ.0) THEN
          TYPE *, IAM(),' NO ORPHANED JOKER WAGERS FOUND'  
          WRITE(REPLU,9070)                  
        ENDIF                                                                 
        CALL USRCLOS1(REPLU)
        CALL GSTOP(GEXIT_SUCCESS)        
C                                                                               
C FORMAT STATEMENTS FOR CANREP REPORT                                           
C                                                                               
9000    FORMAT('MISMATCH CANCELLED WAGERS & JOKER ',I2.2,'.',I2.2,'.',I4.4)
9010    FORMAT(2X,'AGENT#',2X,'TERMINAL#',2X,'  GAME IND#',2X,'WAGSERIAL#',2X,' JOKER#',4X,'  INT SERIAL#')
9020    FORMAT(1X,132('='))                                              
9040    FORMAT(2X,A8,5X,I4,2X,A8,2X,I1,2X,I10,2X,I7.7,4X,I5,2X,I8)         
9060    FORMAT(/,1X,'NO CANCELED WAGERS FOUND IN TMF FILE',/)
9070    FORMAT(/,1X,'NO ORPHANED JOKER WAGERS FOUND IN TMF FILE',/)
        END                                                              
