C SHARERPT.FOR
C
C V16 21-DEC-2016 SCML Added share interface file generation
C V15 01-DEC-2000 UXN TOTOGOLO ADDED.
C V14 14-DEC-1999 OXK MULTIWIN changes.
C V13 01-DEC-1999 OXK Set COPY to 0. 
C V12 13-OCT-1999 RXK World Tour added.
C V11 25-MAY-1999 RXK Further stopsys optimisation changes.
C V10 10-JAN-1999 GPW STOPSYS OPTIMISATION
C V09 13-NOV-1997 UXN Automated SHARERPT added.
C V08 03-MAR-1995 HXK Added V5 game
C V07 01-FEB-1995 HXK Added Lotto 1, Lotto 2 sharecalcs
C V06 07-DEC-1994 PXB added bingo share report.
C V05 29-NOV-1994 JXP INCLUDE LOTTO 7/39, VAKIO AND KICKER REPORTS
C V04 27-OCT-1993 HXK CHANGED STOP TO GSTOP.
C V03 07-OCT-1993 HXK Set COPY to 1. 
C V02 19-JAN-1993 HJK CHANGED FOR VIKING LOTTO #*#
C V01 16-MAR-1992 MTK CHANGED FOR NEW KENO GAME  #$#
C                                                                               
C SHARE CALCULATION REPORT                                                      
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
C Copyright 1999 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                    
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
        PROGRAM SHARERPT            
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:LTSHFIL.DEF'                                            !V16
        INCLUDE 'INCLIB:KISHFIL.DEF'                                            !V16
        INCLUDE 'INCLIB:SPSHFIL.DEF'                                            !V16
C                                                                               

        INTEGER*4  ACTIVE(MAXTYP)      ! game valid for report
        INTEGER*4  I                   ! counter
        INTEGER*4  K                   ! counter
        INTEGER*4  ST                  ! status
        INTEGER*4  FLAG                ! answer variable
        INTEGER*4  COPY                ! numberof report copies
        INTEGER*4  DRAW                ! draw number
        INTEGER*4  GNUM                ! game number
        INTEGER*4  GIND                ! game index
        INTEGER*4  GTYP                ! game type
        INTEGER*4  EXT                 ! status
C
        RECORD /STCLTSHFIL/ LTSHDATA                                            !V16
        RECORD /STCKISHFIL/ KISHDATA                                            !V16
        RECORD /STCSPSHFIL/ SPSHDATA                                            !V16
C
        CHARACTER*34 STRING            !
        COMMON SCFREC

        DATA ACTIVE/MAXTYP*0/                                          
        DATA DRAW/0/                                                   
        LOGICAL EMPTY_LIST /.TRUE./
        LOGICAL    AUTOPROMPT   
C                                                                               
C                                                                               
C CALL  COPYRITE  SUBROUTINE                                                    
C                                                                               
        CALL COPYRITE
C
        AUTOPROMPT = .FALSE.
        DO I=1,MAXGAM
           IF(SHARERPT_AUTO(I).NE.0) EMPTY_LIST = .FALSE.
        ENDDO
C                                                             !V12...
        IF ((STOPMOD.EQ.WINMULTI).AND.ISSUBPROC()) THEN
            IF(.NOT.EMPTY_LIST) THEN
               AUTOPROMPT=.TRUE.
               GO TO 99
            ELSE
               WRITE(5,921) IAM() 
               CALL GSTOP(GEXIT_SUCCESS)
            ENDIF
        ENDIF                                                 !...V12

        CALL STTSK(8HSTSYSTEM,K,ST)
        IF(ISSUBPROC().AND.ST.NE.4.AND..NOT.EMPTY_LIST) AUTOPROMPT = .TRUE.
C                                                                               
   99   CONTINUE
C
        ACTIVE(TLTO)=1                                                 
        ACTIVE(TSPT)=1
	ACTIVE(TTGL)=1
        ACTIVE(TKIK)=1

        COPY = 0

	CALL GETSCONF(SCFREC,ST)
C                                                                               
C                                                                               
100     CONTINUE                                                             
        IF(AUTOPROMPT) THEN
          DO I = 1, MAXGAM
            IF(SHARERPT_AUTO(I).NE.0) THEN
              DRAW = SHARERPT_AUTO(I)
              GNUM = I
              GIND = SCFGNT(GAMIDX,GNUM)
              GTYP = SCFGNT(GAMTYP,GNUM)
              SHARERPT_AUTO(I) = 0
              GOTO 110
            ENDIF
          ENDDO
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
        WRITE(5,900) (K,GTNAMES(K),K=1,MAXTYP)                               
        CALL INPNUM('Enter game type ',GTYP,1,MAXTYP,EXT)                    
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)                                                   
        CALL INPNUM('Enter game index ',GIND,1,MAXIND,EXT)                   
        IF(EXT.LT.0) GOTO 100                                                
C                                                                               
C                                                                               
        GNUM = SCFGTN(GTYP,GIND)                                             
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN                                 
            TYPE*,IAM(),'Sorry, game selected is not active' 
            GOTO 100
        ENDIF   
                                                                  
        IF(ACTIVE(GTYP).NE.1) THEN                                           
            WRITE(5,930) IAM(),GTNAMES(GTYP),GIND                                  
            GOTO 100                                                         
        ENDIF                                                                
C                                                                               
C                                                                               
        WRITE(STRING,800) GTNAMES(GTYP),GIND                                 
        CALL INPNUM(STRING,DRAW,1,999999,EXT)                                   
        IF(EXT.LT.0) GOTO 100                                                
C                                                                               
C                                                                               
        WRITE(5,910) IAM(),GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),DRAW    
        CALL INPYESNO('Is this correct (Y/N) ',FLAG)        
        IF(FLAG.NE.1) GOTO 100
110     CONTINUE                                               
C                                                                               
CV16        IF(GTYP.EQ.TLTO) CALL LTSREP(GNUM,GIND,DRAW,COPY)
        IF(GTYP.EQ.TLTO) THEN 
          CALL FASTSET(0,LTSHDATA,SIZEOF(LTSHDATA)/4)                           !V16 CLEAR DATA STRUCTURE
          CALL LTSREP(GNUM,GIND,DRAW,LTSHDATA)                                  !V16 NOW ALSO SAVE SHARE REPORT DATA FOR FURTHER LOTTO SHARE INTERFACE FILE CREATION
          WRITE(5,920) IAM(),GTNAMES(GTYP),GIND                                 !V16
          CALL LTSHFIL(LTSHDATA,ST)                                             !V16 GENERATE LOTTO SHARE INTERFACE FILE
          GOTO 100                                                              !V16
        ENDIF
C
CV16        IF(GTYP.EQ.TSPT) CALL SPSREP(GNUM,GIND,DRAW,COPY)                      
        IF(GTYP.EQ.TSPT) THEN
          CALL FASTSET(0,SPSHDATA,SIZEOF(SPSHDATA)/4)                           !V16 CLEAR DATA STRUCTURE
          CALL SPSREP(GNUM,GIND,DRAW,COPY,SPSHDATA)                             !V16 NOW ALSO SAVE SHARE REPORT DATA FOR FURTHER SPORTS SHARE INTERFACE FILE CREATION
          WRITE(5,920) IAM(),GTNAMES(GTYP),GIND                                 !V16
          CALL SPSHFIL(SPSHDATA,ST)                                             !V16 GENERATE SPORTS SHARE INTERFACE FILE
          GOTO 100                                                              !V16
        ENDIF
C
        IF(GTYP.EQ.TTGL) CALL TGSREP(GNUM,GIND,DRAW,COPY)                      
CV16        IF(GTYP.EQ.TKIK) CALL KISREP(GNUM,GIND,DRAW,COPY)                      
        IF(GTYP.EQ.TKIK) THEN
          CALL FASTSET(0,KISHDATA,SIZEOF(KISHDATA)/4)                           !V16 CLEAR DATA STRUCTURE
          CALL KISREP(GNUM,GIND,DRAW,COPY,KISHDATA)                             !V16 NOW ALSO SAVE SHARE REPORT DATA FOR FURTHER KICKER SHARE INTERFACE FILE CREATION
          WRITE(5,920) IAM(),GTNAMES(GTYP),GIND                                 !V16
          CALL KISHFIL(KISHDATA,ST)                                             !V16 GENERATE KICKER SHARE INTERFACE FILE
          GOTO 100                                                              !V16
        ENDIF
C
        IF(GTYP.EQ.TBNG) CALL BNSREP(GNUM,GIND,DRAW,COPY)                      

        WRITE(5,920) IAM(),GTNAMES(GTYP),GIND                                  
C                                                                               
C                                                                               
        GOTO 100                                                             
C                                                                               
C                                                                               
800     FORMAT('Enter ',A8,I1,' event/draw number ')                          
900     FORMAT(//,' Game share/tax report',//,                               
     *         <MAXTYP>(1X,I2,' - ',A8,/))                                  
910     FORMAT(1X,A,A8,I1,2X,4A4,'Draw ',I5,/)                            
920     FORMAT(1X,A,A8,I1,' share report complete')                     
921     FORMAT(1X,A,'No multiwinsel automated share reports for these games')
930     FORMAT(1X,A,A8,I1,' share report not available')                  
931     FORMAT(1X,A,A,1X,'Generating share report for ',A8,'index ',I1)
        END                                                            
