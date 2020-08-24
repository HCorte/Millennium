C
C LTOST39.FTN                                                                   
C
C V07 06-JUL-2000 UXN COUNT_PLAYED_COMBINATIONS renamed to CNTCMB
C V06 28-FEB-2000 RXK Promotion ("add 1 free week") added.
C V05 08-SEP-1997 UXN  SYSTEM BETS COUNTING ADDED. MOST POPULAR
C                    COMBINATIONS REPORTING ADDED.
C V04 22-JAN-1993 HJK  DO NOT COUNT VIKING LOTTO WAGER #*#                        
C V03 06-JAN-1992 HJK  COUNT ORIGINAL WAGER ONLY (TFRAC=10)                       
C V02 03-SEP-1990 GCAN FIXED BUG WITH CHECK FOR CORRECT GAMES                     
C V01 24-NOV-1989 MGM  INITIAL RELEASE FOR FINLAND                                
C                                                                               
C Lotto 7/39 Statistics Report                                                  
C                                                                               
C  Statistic arrays to hold quick pick and manual selection flags.              
C  For each element there are 32 bits available.  The total select-             
C  ions available in a 7/39 game: 15,380,937 / 32 = 480655 elements             
C                                                                               
C     Formula:  39*38*37*36*35*34*33                                            
C               --------------------  = 15,380,937                              
C                  7! (factorial)                                               
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
C Copyright 2000 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                      
C=======OPTIONS/CHECK=NOOVERFLOW/EXT                                                 
        PROGRAM LTOST39           
        IMPLICIT NONE  
C           
        INCLUDE 'INCLIB:SYSPARAM.DEF'                                                            
        INCLUDE 'INCLIB:SYSEXTRN.DEF'                                                            
        INCLUDE 'INCLIB:GLOBAL.DEF'

        INCLUDE 'INCLIB:POOLLTO.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'  
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:LSYSCOM.DEF'
        INCLUDE 'INCLIB:LTOSTREP.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C                                                                               
        INTEGER*4  TOTSEL
        PARAMETER  (TOTSEL=15380937)
        INTEGER*4  TOTEL                                                    
        PARAMETER (TOTEL  =   480655)                                            
        INTEGER*4  MAXNR                                                    
        PARAMETER (MAXNR  =       70)        ! Maximum number of digits  
        INTEGER*4  N                                                         
        PARAMETER (N      =        7)        ! Maximum # of selections           
C                                                                               
        INTEGER*4  TMFBUF(8192)                                                  
        INTEGER*4  LOGBUF(LREC*3)                                                

        INTEGER*4  NPICKED(MAXNR,2)          ! 1=MANUAL 2=QP                           
        INTEGER*4  TOTPIC(2)                 ! 1=MANUAL 2=QP                           
        INTEGER*4  FDB(7)                    !
        INTEGER*4  EOF/0/                    !
        INTEGER*4  IND/0/                    !                   
        INTEGER*4  TFDB(7)                   !                                   
        INTEGER*4  SCFFDB(7)                 !
        INTEGER*4  SCFNAM(5)                 !                        
        INTEGER*4  BOARDS(84)                ! Total # of selections
        INTEGER*4  DRAWD(3)                  !                                   
        INTEGER*4  PERTAB(0:MAXNR,N)         !                                   
        INTEGER*4  LMCNTS(10)                !                                   
        INTEGER*4  REPLU /7/                 !                                   
        INTEGER*4  QP_TBL(32)                ! Quick pick flag table                  
        INTEGER*4  COPY                      !
        INTEGER*4  STCDC                     ! starting CDC
        INTEGER*4  ENDCDC                    ! ending CDC
        INTEGER*4  DRAW                      ! draw number
        INTEGER*4  VOLN                      ! volume for DRAW files
        INTEGER*4  OFFST                     !
        INTEGER*4  TCFCNT                    !
        INTEGER*4  LMCNT                     !
        INTEGER*4  BLOCK                     !
        INTEGER*4  COUNTER,TIMES             !
        INTEGER*4  ST                        ! return status
        INTEGER*4  EXT                       ! return status
        INTEGER*4  STATUS                    !
        INTEGER*4  GNUM                      ! game number
        INTEGER*4  CDC                       !
        INTEGER*4  I                         ! counter
        INTEGER*4  J                         ! counter
        INTEGER*4  K                         ! counter
        INTEGER*4  LUNC/1/                   ! logical unit for carryover file
        INTEGER*4  DRWCDC                    !
        INTEGER*4  XTYPE                     !
        INTEGER*4  LENGTH                    !
        INTEGER*4  TOT_SALES                 !
        INTEGER*4  CDCB                      !
        INTEGER*4  PAGE                      !
        INTEGER*4  DRWFILE(5)                ! draw file name
        INTEGER*4  I4TEMP                    ! I*4 work variable
        INTEGER*4  INLEN                     !
        INTEGER*4  DLT_SALES,TOT_BOARDS
        INTEGER*4  MRKS,MAX_NR
        PARAMETER (MRKS=7)
        PARAMETER (MAX_NR=39)

        INTEGER*2  BETS(LMXMARK)             !                              
        INTEGER*2  XDATE(LDATE_LEN)          !                              

        BYTE       I1TEMP(4)                 ! I*1 work array
C                                                                               
        REAL*8     PERPICK(2)                !                              
C
        RECORD /POPULAR_COMBINATION/ POP(MAX_POP)
C           
        CHARACTER*4  CVOLN
                                                             
        LOGICAL    EOFFLG   /.FALSE./        !                              
        LOGICAL    ERR_STAT /.TRUE./         !                              
        LOGICAL    READLCF  /.FALSE./        !               

        EQUIVALENCE(I4TEMP,I1TEMP)
        EQUIVALENCE(VOLN,CVOLN)


C                                                                               
        DATA       LMCNTS     /10*0/                                        
        DATA       SCFNAM/'SCF.','FIL ',3*'    '/                           
        DATA       DRWFILE/'XXXX',':XXX','XXXX','X.FI','L   '/              
C                                                                               
C CALL  COPYRITE  SUBROUTINE                                                    
C                                                                               
        CALL COPYRITE                                                             
C                                                                               
C                                                                               
        TYPE *                                                                    
        TYPE *,IAM(),'<<<<< LTOST39 Lotto 1 Statistics Report      V01 >>>>>'           
        TYPE *                                                                    

C        CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)               
C        IF(EXT.LT.0) STOP                                                         
        COPY=0

        CALL PRMNUM('Enter the starting CDC:       ',STCDC,0,99999,EXT)           
        IF (EXT.NE.0) STOP                                                        

        CALL PRMNUM('Enter the ending   CDC:       ',ENDCDC,                      
     *               STCDC,99999,EXT)             
        IF (EXT.NE.0) STOP                                                        

        CALL PRMNUM('Enter the draw number:        ',DRAW,1,5000,EXT)                  
        IF (EXT.NE.0) STOP                                                        

        CALL PRMTEXT('Enter volume for draw files:  ',CVOLN,INLEN)                            
C                                                                               
C       Initialize counters                                                       
C                                                                               
        OFFST      = 0                                                             
        DRWCDC     = 0                                                             
        TCFCNT     = 0                                                             
        LMCNT      = 0                                                             
        TOTPIC(1)  = 0                                                             
        TOTPIC(2)  = 0                                                             
        PERPICK(1) = 0.0D0                                                         
        PERPICK(2) = 0.0D0                                                         
        BLOCK      = 0                                                             
        COUNTER    = 0                                                             
C                                                                               
C CALL PERM TO FIND ALL POSSIBLE COMBINATIONS                                   
C                                                                               
        CALL PERM(PERTAB,MAXNR,N,-1)                                              
C                                                                               
C READ SCF RECORD                                                               
C                                                                               
        CALL OPENW(1,SCFNAM,4,0,0,ST)                                             
        CALL IOINIT(SCFFDB,1,SCFSEC*256)                                              
        IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)                                    

        CALL READW(SCFFDB,1,SCFREC,ST)                                            
        IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)                                    

        CALL CLOSEFIL(SCFFDB)                                                     

        DO I = 1, MAXFIL                                                          
            IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))                     
        END DO

        ! open report file
        CALL ROPEN('LTOST39.REP',REPLU,ST)                                        
        IF(ST.NE.0)THEN                                                           
            TYPE *,IAM(),'Error openning LTOST39.REP > ',ST                               
            CALL GPAUSE
        END IF                                                                    
C                                                                               
C OPEN LOTTO 1 GAME FILE                                                        
C                                                                               
        GNUM = GTNTAB(TLTO,1)                                                                    
        CALL OPENW(8,SCFGFN(1,GNUM),4,0,0,ST)                                     
        CALL IOINIT(FDB,8,DLTSEC*256)                                                 
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)                            
C        TYPE *, IAM(),'GETTING HERE'
C                                                                               
C READ LOTTO 1 GAME FILE                                                        
C                                                                               
        CALL READW(FDB,DRAW,DLTREC,ST)                                            
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)                         
        CALL CLOSEFIL(FDB)                                                        
C
C CALCULATE NUMBER OF TOTAL BOARDS
C
        IF(ENDCDC.EQ.DLTDAT(1)) THEN
           DO K=1,LTGENT
              DLT_SALES = DLT_SALES + DLTSAL(K)
           ENDDO
        ELSE
           DO K=DLTDAT(1)-ENDCDC+3,DLTDAT(1)-STCDC+3
              DLT_SALES = DLT_SALES + DLTSAL(K)
           ENDDO
        ENDIF
C        TOT_BOARDS = DLT_SALES / LTOPRC(1) 
C                                                                               
C INITIALIZE DRAW DATE VARIABLES                                                
C                                                                               
        DRWCDC   = DLTDAT(1)                                                      
        XDATE(5) = DRWCDC                                                         
        CALL LCDATE(XDATE)                                                         
        DRAWD(1) = XDATE(1)   !DAY                                                
        DRAWD(2) = XDATE(2)   !MONTH                                              
        DRAWD(3) = XDATE(14)   !YEAR                                               

        ! set up draw file name
        DRWFILE(1) = VOLN
        CALL MOVBYT(SCFSGN(GNUM),1,DRWFILE(1),6,4)
C                                                                               
C       The draw number and date have been found now go through the LM            
C       files and check the wagers.                                               
C                                                                               
        WRITE(5,9009)IAM(), DRAW,DRWCDC,DRAWD(1),DRAWD(2),DRAWD(3)                      
C                                                                               
C       ================================================================          
C                            Main processing Loop                               
C       ================================================================          
C                                                                               
        DO 200 CDC  = STCDC, ENDCDC + 1                                            
            ERR_STAT = .TRUE.                                                      
            READLCF  = .FALSE.                                                     
            IF(CDC.EQ.ENDCDC+1) READLCF =.TRUE.                                    
C                                                                               
C           OPEN LOTTO TRANSACTION FILE OR CARRYOVER FILE                          
C                                                                               
            IF(READLCF) THEN                                                       
                ! OPEN CARRYOVER FILE                                                           
                CLOSE(UNIT=PTMF)                                                    
                CALL IOPEN(SCFSFN(1,TCF),LUNC,LREC*2,LCDC,LSER*2-1,ST)                 
                IF(ST.NE.0)  CALL FILERR(SCFSFN(1,TCF),1,ST,0)                      
            ELSE                                                                   
                ! OPEN DRAW FILE                                                                
                CALL OPNDRW(CDC,PTMF,DRWFILE)                                       
                CALL IOINIT(TFDB,PTMF,128*256)                                          
                BLOCK=0                                                             
                EOF=0                                                               
                IND=8192                                                            
            ENDIF                                                                  
C                                                                               
            IF(READLCF) TYPE *,IAM(),'Processing TCF'                                    
C                                                                               
2030        CONTINUE                                                               
            IF(READLCF)THEN                                                        
                CALL READTCF(LOGBUF,LUNC,EOFFLG)                                         
                IF(EOFFLG) GOTO 200                                                 
            ELSE                                                                   
                IF(IND.GT.8157) THEN                                                
                    ST = 0                                                           
                    BLOCK = BLOCK + 1                                                    
                    IND = 1                                                            
                    CALL READW(TFDB,BLOCK,TMFBUF,ST)                                 
                    IF(ST.NE.0) THEN                                                 
                        WRITE(5,8002) IAM(),CDC,ST,BLOCK                                    
                        CALL GPAUSE                                                         
                    ENDIF                                                            
                ENDIF                                                               
            ENDIF                                                                  
C                                                                               
            IF (READLCF) THEN                                                       
                CALL LOGTRA(TRABUF,LOGBUF)                                          
            ELSE                                                                   
                IF(EOF.GT.1000) GOTO 200                                            
                IF(TMFBUF(IND).EQ.0) THEN                                           
                    EOF = EOF + 1                                                        
                    IND = IND + LREC                                                     
                    GOTO 2030                                                        
                ENDIF                                                               

                EOF=0                                                               

                I4TEMP = TMFBUF(IND+LREC-1)                                            
                XTYPE  = ZEXT(I1TEMP(4))                                                 
                IF(XTYPE.NE.LONE .AND. XTYPE.NE.LREG) THEN                          
                    TYPE*,IAM(),'Bad record type > ',XTYPE,' index > ',IND                 
                    IND=IND+LREC                                                     
                    GOTO 2030                                                        
                ENDIF                                                               
C                                                                               
                LENGTH=LREC                                                         
                IF(XTYPE.EQ.LONE) THEN                                              
                    I4TEMP = TMFBUF(IND+LREC*2-1)                                    
                    XTYPE  = ZEXT(I1TEMP(4))
                    IF(XTYPE.EQ.LEND) LENGTH=LREC*2                                  
                    IF(XTYPE.EQ.LTWO) LENGTH=LREC*3                                  
                ENDIF                                                               

                CALL FASTMOV(TMFBUF(IND),LOGBUF,LENGTH)                             
                CALL LOGTRA(TRABUF,LOGBUF)                                          
                IND = IND + LENGTH                                                      
            ENDIF                                                                 
C                                                                               
C VALIDATE TRANSACTION CDC                                                      
C                                                                               
            IF(TRABUF(TCDC).NE.CDC.AND.ERR_STAT.AND..NOT.READLCF)THEN             
                TYPE*,IAM(),' CDC of transaction: ',TRABUF(TCDC),                        
     *                ' Does not equal expected CDC of: ',CDC                      
                CALL GPAUSE                                                              
                ERR_STAT = .FALSE.                                                 
            ENDIF                                                                 
C                                                                               
C VALIDATION TRANSACTION STATUS                                                 
C                                                                               
            IF (TRABUF(TGAMTYP).NE.TLTO) GOTO 2030                                
            IF (TRABUF(TGAMIND).NE.1)    GOTO 2030  !V04
            IF(TRABUF(TWFFLG).NE.0)      GOTO 2030                          
C            IF (TRABUF(TFRAC).NE.10)     GOTO 2030                                
            IF (TRABUF(TTYP) .NE.TWAG)   GOTO 2030                                
            IF (TRABUF(TSTAT).NE.GOOD.AND.
     *          TRABUF(TSTAT).NE.FRAC.AND.
     *          TRABUF(TSTAT).NE.EXCH)   GOTO 2030
C                                                                               
C VALIDATE DRAW NUMBER                                                          
C                                                                               
            IF ((TRABUF(TWBEG) .LT. DRAW) .AND.                                   
     *          (TRABUF(TWEND)+TRABUF(TWADDFW) .LT. DRAW)) GOTO 2030                              
            IF ((TRABUF(TWBEG) .GT. DRAW) .AND.                                   
     *          (TRABUF(TWEND)+TRABUF(TWADDFW) .GT. DRAW)) GOTO 2030                              
C                                                                               
C CONVERT BOARDS                                                                
C                                                                               
            CALL CNVBRD(TRABUF(TWNBET),TRABUF(TWNMRK),39,TRABUF(TWBORD),          
     *                  STATUS,BOARDS)                                            
            IF (STATUS .NE. 0) THEN                                               
                TYPE *,IAM(),'ERROR CONVERTING BET DATA ',STATUS                        
                TYPE *,IAM(),'SERIAL ',TRABUF(TSER)                                     
                CALL GPAUSE                                                             
            END IF                                                                
C                                                                               
C GET QUICK PICK BIT FLAGS                                                      
C                                                                               
            CALL GETFLG(TRABUF(TWQPF),QP_TBL,TRABUF(TWNBET))
C
C CHECK FOR SYSTEM BETS.
C
        IF(TRABUF(TWSYST).EQ.1) THEN ! FULL SYSTEM
            CALL COMBINATION(TRABUF(TWNMRK)-1,MRKS-1,TIMES)
        ELSEIF(TRABUF(TWSYST).EQ.2) THEN ! REDUCED SYSTEM BET
            CALL REDUCED_SYSTEM_INIT(TRABUF(TWSYSN))
        ELSE
            TIMES = 1
        ENDIF
C                                                                               
C COUNT NUMBER OF TIMES AN IDIVIDUAL NUMBER HAS BEEN SELECTED                   
C                                                                               
            DO 240 J = 1,TRABUF(TWNBET)
                DO 250 I = 1,TRABUF(TWNMRK)
                    BETS(I) = BOARDS(I+((J-1)*TRABUF(TWNMRK)))
                    IF(TRABUF(TWSYST).EQ.2) THEN ! REDUCED SYSTEM BET
                      CALL REDUCED_SYSTEM(I,TIMES)
                    ENDIF
                    IF (QP_TBL(J).EQ.1) THEN                                 
                        NPICKED(BETS(I),2) = NPICKED(BETS(I),2) + TIMES 
                    ELSE                                                  
                        NPICKED(BETS(I),1) = NPICKED(BETS(I),1) + TIMES
                    END IF 
250             CONTINUE
C                                                                               
                COUNTER = COUNTER + 1 
240         CONTINUE     
                IF(READLCF) TCFCNT = TCFCNT + TRABUF(TWSIMP) 
                IF(.NOT.READLCF) THEN 
                    LMCNTS(CDC+1-STCDC) = LMCNTS(CDC+1-STCDC) + TRABUF(TWSIMP)
                END IF                                                         
            GOTO 2030                                                             
C                                                                               
200     CONTINUE                                                                  
C                                                                               
C       =====================================================================     
C                     End of Main processing loop                               
C       =====================================================================     
C                   
        IF (READLCF) THEN                                                    
            CLOSE(UNIT=PTMF) 
        ELSE
            CLOSE(UNIT=LUNC)
        END IF                                

        WRITE(5,8004)IAM(),'Advance boards sold',TCFCNT                                 
        TOT_SALES = TCFCNT                                                        
        J = 1                                                                     
        DO  CDCB = STCDC,ENDCDC                                               
            WRITE(5,8005)IAM(),'Boards sold for CDC:',CDCB,LMCNTS(J)                    
            TOT_SALES = TOT_SALES + LMCNTS(J)                                     
            J = J + 1                                                             
        END DO 

        WRITE(5,*)IAM(),'===================================='                          
        WRITE(5,8004)IAM(),'Total boards sold',TOT_SALES                                
C
C CALCULATE TOTAL NUMBER OF COMBINATIONS PLAYED AND MOST
C POPULAR COMBINATIONS.
C
        TYPE*,IAM(),'Generating report for most played combinations...'
        CALL CNTCMB(GNUM,TOT_BOARDS,COUNTER,TOTSEL,POP)
C                                                                               
C TOP OF PAGE:   Pool Coverage                                                  
C                                                                               
        CALL TITLE('LOTTO 1 STATISTICS','LTOST39 ',                               
     *              1,REPLU,PAGE,DAYCDC)                                          
        WRITE(REPLU,9000) DRAW,(XDATE(K),K=7,13),STCDC,ENDCDC,TOTSEL,             
     *                    TOT_BOARDS,COUNTER,TOTSEL-COUNTER,                           
     *                    DFLOAT(TOT_BOARDS)/DFLOAT(TOTSEL)*100.0D0,                   
     *                    DFLOAT(COUNTER)/DFLOAT(TOTSEL)*100.0D0                  
C                                                                               
C SECOND PART:   Selection data by number                                       
C                                                                               
        WRITE(REPLU,9001)                                                         
        DO  J = 1,MAXNR                                                       
            TOTPIC(1) = TOTPIC(1) + NPICKED(J,1)                                  
            TOTPIC(2) = TOTPIC(2) + NPICKED(J,2)                                  
        END DO

        DO  K = 1,39                                                          
            IF(TOTPIC(1).GT.0) PERPICK(1) = (DFLOAT(NPICKED(K,1))/                
     *                                 DFLOAT(TOTPIC(1)))*100.0D0                 
            IF(TOTPIC(2).GT.0) PERPICK(2) = (DFLOAT(NPICKED(K,2))/                
     *                                 DFLOAT(TOTPIC(2)))*100.0D0                 
            WRITE(REPLU,9002) K,NPICKED(K,1),PERPICK(1),                          
     *                        NPICKED(K,2),PERPICK(2),                            
     *                        NPICKED(K,1)+NPICKED(K,2),                          
     *                        PERPICK(1)+PERPICK(2)                               
        END DO

        WRITE(REPLU,9003)TOTPIC(1),TOTPIC(2),TOTPIC(1)+TOTPIC(2)                  
C                                                                               
C SECOND PAGE - MOST POPULAR COMBINATIONS.
C
        CALL TITLE('LOTTO 1 STATISTICS','LTOST39 ',
     *              1,REPLU,PAGE,DAYCDC)
        WRITE(REPLU,9004) DRAW,(XDATE(K),K=7,13),STCDC,ENDCDC
        DO I = 1,MAX_POP
          IF(POP(I).COUNT.GT.0) THEN
            WRITE(REPLU,9005) I,POP(I).COUNT,(POP(I).BOARD(K),K=1,MRKS)
          ENDIF
        ENDDO
C                                                                               
C       All Done                                             
C                                                                               
        TYPE *,IAM(),'Processing completed'
        CLOSE(REPLU)

        CALL SPOOL('LTOST39.REP',COPY,ST)

        CALL GSTOP(GEXIT_SUCCESS)
C                                                                               
C       ===================== Format Statements =====================             
C                                                                               
8001    FORMAT(A4)                                                                
8002    FORMAT(1X,A,' Error reading LOTO',I4.4,' > ',I4,' Block: ',I4)            
8004    FORMAT(1X,A,1X,A25,' = ',I8)                                              
8005    FORMAT(1X,A,1X,A21,I4,' = ',I8)                                           
C                                                                               
9000    FORMAT(1X,131('='),/,1X,T41,'PRODUCED FOR DRAW: ',I4,                
     *         T71,'DATE:',T79,7(A2),/,1X,T41,'STARTING CDC:',T59,I5,T71,    
     *        'ENDING CDC:',T88,I5,//,1X,T41,'AVAILABLE COMBINATIONS:',      
     *         T81,I12,/,1X,T41,'TOTAL BOARDS:',T81,I12,2X,'(pools)',/,1X,T41,
     *        'COMBINATIONS PLAYED:',T81,I12,2X,'(pools)',/,1X,T41,
     *        'COMBINATIONS NOT PLAYED:',T81,I12,2X,'(pools)',/,1X,T41,
     *        'TOTAL BOARDS / AVAILABLE COMBINATIONS:',T87,F6.2,' %',2X,
     *        '(pools)',/,      
     *        1X,T41,'COMBINATIONS PLAYED / AVAILABLE COMBINATIONS:',        
     *         T87,F6.2,' %',2X,'(pools)',/,1X,131('='),/)
9001    FORMAT(10X,'NUMBER',9X,'PLAYER',25X,'QUICK PICK',26X,'TOTAL',        
     *         12X,'TOTAL',/,9X,'SELECTED',7X,'SELECTED',9X,'PERCENT',       
     *         9X,'SELECTED',9X,'PERCENT',10X,'SELECTED',9X,'PERCENT',       
     *         /)                                                            
9002    FORMAT(12X,I2.2,3(10X,I8,10X,F6.2))                                  
9003    FORMAT(9X,109('='),/,9X,'TOTAL',6X,I12,22X,I12,22X,I12)                       
C                                                                               
9004    FORMAT(1X,131('='),/,1X,T41,'PRODUCED FOR DRAW: ',I4,                
     *         T71,'DATE:',T79,7(A2),/,1X,T41,'STARTING CDC:',T59,I5,T71,    
     *        'ENDING CDC:',T88,I5,///,10X,'POPULARITY',10X,'PLAYED',
     *         10X,'MARKS',/)
9005    FORMAT(14X,I2,10X,I10,10X,<MRKS>(I2.2,2X))
9009    FORMAT(/,1X,A,' Processing  Draw # : ',I4,'   CDC : ',I4,               
     *          '   Date : ',I2.2,'/',I2.2,'/',I4.4,/)                       
C                                                                               
      END                                                                       
C                                                                               
