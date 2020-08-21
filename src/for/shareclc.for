C PROGRAM SHARECLC.FOR
C
C V23 31-MAR-2017 MTK ALLOW DRAWS ENTRY > 999
C V22 21-DEC-2010 MAC LOTTO3 & LOTTO4
C     20-JAN-2011 FJG Previous LOTTO need to be done
C V21 01-DEC-2000 UXN TOTOGOLA ADDED.
C V20 24-FEB-2000 OXK Removed hardcoding GIND=1 for SPORT (Vakio changes)
C V19 29-DEC-1999 OXK DRWSTS=SHAROK added
C V18 13-DEC-1999 OXK MULTIWIN changes.
C V17 10-NOV-1999 UXN LTCALC parameters changed.
C V16 13-OCT-1999 RXK World Tour added.
C V15 05-AUG-1999 PXO Don't skip share calculation for Lotto 1 and Vakio
C V14 25-MAY-1999 RXK Skip automated share calculation for Lotto 1 and Vakio.
C V13 10-JAN-1999 GPW STOPSYS OPTIMISATION
C V12 31-JUL-1998 RXK Added kicker
C V11 03-MAR-1995 HXK Added V5 game
C V10 07-JAN-1995 HXK Added Bingo
C V09 12-DEC-1992 HJK MODIFIED FOR VIKING LOTTO #*#
C V08 14-SEP-1992 HJK CHANGED FOR SPEDEN GAME #!#  
C V07 16-MAR-1992 MTK CHANGED FOR NEW KENO GAME  #$#
C
C PROGRAM TO CALCULATE PRIZE/TAX VALUES FOR ALL GAMES                           
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM SHARECLC                                                    
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
                                                                         
        INTEGER*4  ACTIVE(MAXTYP,MAXIND)  ! active games for share calc
        INTEGER*4  GTYP                   ! game type
        INTEGER*4  GIND                   ! game index
        INTEGER*4  K                      ! counter
        INTEGER*4  GNUM                   ! game number
        INTEGER*4  DRAW                   ! draw number
        INTEGER*4  EXT                    ! status
        INTEGER*4  FLAG                   ! answer variable
        INTEGER*4  ST                     ! status
        INTEGER*4  I                      ! counter
        CHARACTER*34 STRING               !       

        DATA DRAW/0/                                                        

       COMMON SCFREC
                                                                               
C                                                                               
C CALL  COPYRITE  SUBROUTINE                                                    
C                                                                               
        CALL COPYRITE                                                       
C                                                                               
C ASK TO USER IN HE/SHE WANTS RUN SHARE CALCULATION PROCEDURES
C
        CALL PRMYESNO('Are You Sure You Want To Run Shareclc task [Y/N]?', FLAG)
        IF(FLAG .NE. 1) CALL GSTOP(GEXIT_SUCCESS)
C                                                                               
        CALL FASTMOV(0,ACTIVE,MAXTYP*MAXIND)             

C       ACTIVE(TLTO,3) = 1      !V22 
C       ACTIVE(TLTO,4) = 1      !V22
        DO I=1,NUMLTO
           ACTIVE(TLTO,I) = 1
        ENDDO
	DO I=1,NUMSPT
           ACTIVE(TSPT,I) = 1
	ENDDO
	DO I=1,NUMTGL
  	   ACTIVE(TTGL,I) = 1
	ENDDO
        ACTIVE(TKIK,1) = 1

	CALL GETSCONF(SCFREC,ST)

        GNUM=0
100     CONTINUE                                                            
C                                                             !V15...
C        IF ((STOPMOD.EQ.WINMULTI).AND.ISSUBPROC()) THEN
C             GNUM=GNUM+1
C             IF(GNUM.GT.MAXGAM) CALL GSTOP(GEXIT_SUCCESS)
C	     DRAW = 0
C             IF(DRWSTS(MLWININD,GNUM).EQ.WINSOK) DRAW=DRWGAM(MLWININD,GNUM)
C             IF(DRAW.LE.0) GO TO 100
C             GTYP=SCFGNT(GAMTYP,GNUM)
C             GIND=SCFGNT(GAMIDX,GNUM)
C             IF(GTYP.LE.0.OR.GTYP.GT.MAXTYP) GO TO 100
C             IF(ACTIVE(GTYP,GIND).NE.1) GO TO 100
C             GO TO 99
C        ENDIF
C                                                             !...V15
        WRITE(6,900) (K,GTNAMES(K),K=1,MAXTYP)                              
        CALL PRMNUM('Enter Game Type [E: Exit]', GTYP, 1, MAXTYP, EXT)      
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)                  
        CALL PRMNUM('Enter Game Index [E: Exit]', GIND, 1, MAXIND, EXT)   
        IF(EXT.LT.0) GOTO 100                                               
                                                                               
        GNUM=SCFGTN(GTYP,GIND)                                              
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN                                
          TYPE*,IAM(),'Sorry, game selected is not active'    
          GOTO 100                                                          
        ENDIF                                                               

        IF(ACTIVE(GTYP,GIND).NE.1) THEN                                     
          WRITE(6,930) IAM(),GTNAMES(GTYP),GIND 
          GOTO 100                                                          
        ENDIF                                                               
                                                                               
        WRITE(STRING,800) GTNAMES(GTYP),GIND                               
        CALL PRMNUM(STRING,DRAW,1,9999,EXT)                                  
        IF(EXT.LT.0) GOTO 100                                               
                                                                               
        WRITE(6,910) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),DRAW         
        CALL PRMYESNO('Is this correct (Y/N) ? ',FLAG)                               
        IF(FLAG.NE.1) GOTO 100                                              
   99   CONTINUE
        SHARERPT_AUTO(GNUM) = DRAW
        IF(GTYP.EQ.TLTO) CALL LTCALC(GNUM,GIND,DRAW)
        IF(GTYP.EQ.TSPT) CALL SPCALC(GNUM,GIND,DRAW)
        IF(GTYP.EQ.TTGL) CALL TGCALC(GNUM,GIND,DRAW)
        IF(GTYP.EQ.TBNG) CALL BNCALC(GNUM,GIND,DRAW)                         
        IF(GTYP.EQ.TKIK) CALL KIKCALC(GNUM,GIND,DRAW)                         

        IF (STOPMOD.EQ.WINMULTI) DRWSTS(MLWININD,GNUM)=SHAROK
        WRITE(6,920) IAM(),GTNAMES(GTYP),GIND   
                                                                               
        GOTO 100                                                            
                                                                               
800     FORMAT('Enter ',A8,I1,' event/draw number ')                        
900     FORMAT(//,' Game share calculation',//,                         
     *        <MAXTYP>(1X,I2,' - ',A8,/))          
910     FORMAT(1X,A,A8,I1,2X,4A4,'Draw ',I5,/)                                
920     FORMAT(1X,A,A8,I1,' share calculation complete')                      
930     FORMAT(1X,A,A8,I1,' share calculation not available')                 
940     FORMAT(1X,A,A8,I1,' multiwinsel automated share calculation skipped')

        END                                                                 
