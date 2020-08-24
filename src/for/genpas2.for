C GENPAS2.FTN                                                                   
C                                                                               
C V02 03-MAY-90  PP  MODIFIED FROM GENPASS                                      
C - THIS PROGRAM LISTS EITHER 55 LINES PER PAGE OR                              
C   ONLY ONE AGENT PER PAGE (NEW PARAMETER ADDED)                               
C                                                                               
C V01 13-DEC-89  LOU R.    INITIAL RELEASE FOR FINLAND.                         
C                                                                               
C READ ASF AND PRODUCE REPORT WITH AGENT PASSNUMBERS.
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM GENPAS2
        IMPLICIT NONE


        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'


        INTEGER*4 PASSNBR(8), Y, K, X, J, W, V, I, CERR, AGTNBR
        INTEGER*4 REC, ENDA, EXT, BEG, PAGE, LINCNT, ST
        CHARACTER CZERO/Z0/

	INTEGER*4 ANSWER,AGTPAG,COPY


C BEGIN CODE -------------------------------------------

      CALL COPYRITE                                                             


C CLEAR COUNTERS                                                                
C                                                                               
      LINCNT=70                                                                 
      PAGE=0                                                                    



C  QUESTION ADDED: DO YOU WANT ONLY ONE AGENT PER PAGE LISTED                   
C                                                                               
      CALL WIMG (5,'HALUATKO LISTAAN YKSI ASIAMIES/SIVU?  (Y/N) ')              
      CALL YESNO(ANSWER)                                                        
      IF (ANSWER.EQ.1) THEN                                                     
          AGTPAG=1                                                              
      ELSE                                                                      
          AGTPAG=0                                                              
      ENDIF                                                                     



      CALL INPNUM('ENTER STARTING TERMINAL NUMBER ',BEG,                        
     *     1,NUMAGT,EXT)                                                        
      IF(EXT.LT.0) STOP                                                         
      CALL INPNUM('ENTER ENDING TERMINAL NUMBER ',ENDA,                         
     *     BEG,NUMAGT,EXT)                                                      
      IF(EXT.LT.0) STOP                                                         


C OPEN THE AGENT SALES FILE                                                     
C                                                                               
      CALL OPENASF(ASF)                                                         
      CALL ROPEN('GENPAS2.REP',7,ST)                                            
      IF(ST.NE.0) THEN                                                          
        TYPE*,' GENPAS2.REP OPEN ERROR ',ST                                     
        STOP                                                                    
      ENDIF                                                                     



      DO 1000 REC=BEG,ENDA                                                      
      CALL FASTSET(0,PASSNBR,8)                                                 
      CALL READASF(REC,ASFREC,ST)                                               
      CALL ASCBIN(ASFINF,SAGNO,LAGNO,AGTNBR,CERR)                               
      IF(AGTNBR.EQ.0) GOTO 1000                                                 
      CALL ASCBIN(ASFINF,SPAS1,LPAS1,PASSNBR(1),CERR)                           
      CALL ASCBIN(ASFINF,SPAS2,LPAS2,PASSNBR(2),CERR)                           
      CALL ASCBIN(ASFINF,SPAS3,LPAS3,PASSNBR(3),CERR)                           
      CALL ASCBIN(ASFINF,SPAS4,LPAS4,PASSNBR(4),CERR)                           
      CALL ASCBIN(ASFINF,SPAS5,LPAS5,PASSNBR(5),CERR)                           
      CALL ASCBIN(ASFINF,SPAS6,LPAS6,PASSNBR(6),CERR)                           
      CALL ASCBIN(ASFINF,SPAS7,LPAS7,PASSNBR(7),CERR)                           
      CALL ASCBIN(ASFINF,SPAS8,LPAS8,PASSNBR(8),CERR)                           

      DO I=1,ALENGTH        !ALENGTH=512
         IF(ASFBYT(I).EQ.CZERO) ASFBYT(I)=' '
      END DO




C     GENERATE REPORT OF ALL PASSNUMBERS                                            
C                                                                               
C                                                                               
C     CHANGED 3-MAY-90: IF AGTPAG=1 - PRINT ONLY ONE AGENT PER PAGE /PP            
C                                                                               
      IF(AGTPAG.EQ.1.OR.AGTPAG.EQ.0.AND.LINCNT.GT.55) THEN                      
        CALL TITLE('AGENT PASSNUMBER LISTING ',                                 
     *           ' GENPAS2',1,7,PAGE,DAYCDC)                                    
        LINCNT=10                                                               
        WRITE(7,10000)                                                          
      ENDIF                                                                     



      WRITE(7,10001)(ASFBYT(V),V=SAGNO,EAGNO),                                  
     *              (ASFBYT(W),W=SNAME,ENAME),(PASSNBR(J),J=1,4),               
     *              (ASFBYT(X),X=SSTRT,ESTRT),(PASSNBR(K),K=5,8),               
     *              (ASFBYT(Y),Y=SCITY,ECITY)                                   
      LINCNT=LINCNT+4                                                           

1000  CONTINUE                                                                  



      COPY=1                                                                    
      CALL SPOOL('GENPAS2.REP',COPY,ST)                                         
      IF (ST.NE.0) THEN                                                         
         TYPE*,'Error while spooling report st -' ,ST                           
      ENDIF                                                                     






10000 FORMAT(/,1X,'AGENT #',4X,'AGENT NAME',30X,'PASSNUMBERS ',/)               
10001   FORMAT(1X,<LAGNO>(A1),3X,<LNAME>(A1),9X,4(2X,I4.4),/,
     *         12X,<LSTRT>(A1),5X,4(2X,I4.4),/,
     *         12X,<LCITY>(A1),/)


      END                                                                       
