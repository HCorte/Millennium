C PROGRAM TO CONVERT TCDC_SOLD FROM 0 TO TCDC
C
C $Log:   GXAFXT:[GOLS]FIXTMF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:11:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   09 Oct 1993 17:36:02   HXK
C  Added check to only include played kickers.
C  
C     Rev 1.1   09 Oct 1993 16:44:42   HXK
C  Added some shit
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
C Copyright 1993 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
	PROGRAM FIXTMF
	IMPLICIT NONE
                                 

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'               
	INCLUDE 'INCLIB:PRMLOG.DEF' 
            
	INTEGER*4 FDB(7),FILE(5),CON(2)  
	INTEGER*4 LOGREC(LREC*3)
	INTEGER*4 SER,ST,DUMMY,BLOCK,IND,EOF
	INTEGER*4 DIV_WIN(6)
	INTEGER*4 DIV_WIN_CNT(6)
	INTEGER*4 I,EXT
	DATA CON/'CON:','    '/,BLOCK/0/,IND/8192/ 
	DATA EOF/0/ 
	LOGICAL EOT 
C
	DATA DIV_WIN /1234567,0234567,0034567,0004567,0000567,0000067/                                                      
C                                                                               
C                                                         

      type*,'change all jokers to winners'

                      
      SER=1                   
      CALL FASTSET(0,DIV_WIN_CNT,6)                                                  
C                                                                               
C GET INITIAL DATA                                                              
C                                                                               
      CALL OPENW(5,CON,4,0,0,ST)                                                
      CALL WIMG(5,' Enter file name: ')                                         
      READ(5,900) FILE                                                          
900   FORMAT(5A4)                                                               
      CALL OPENW(1,FILE,4,0,0,ST)                                               
      CALL TOPEN(1)                                                             
      IF(ST.NE.0) CALL FILERR(FILE,1,ST,0)
C
	DO I = 1,6
	   TYPE*,IAM(),'DIV: ',I
	   TYPE*,IAM(),' '
	   CALL INPNUM('Enter # of winners for this division: ',
     *                 DIV_WIN_CNT(I),0,999,EXT)
           IF(EXT.NE.0) STOP
	END DO                                      
C                                                                               
10    CONTINUE                                                                  
      CALL READTMF(LOGREC,SER,EOT)                                              
      IF(EOT) GOTO 11                                                           
      CALL LOGTRA(TRABUF,LOGREC)                                                
      IF(TRABUF(TTYP).NE.TWAG) GOTO 10
      IF(TRABUF(TSTAT).NE.GOOD) GOTO 10
      IF(TRABUF(TWKFLG).LE.0) GOTO 10

	DO I = 1,6
	   IF(DIV_WIN_CNT(I).GT.0) THEN
	      TRABUF(TWKICK)  = DIV_WIN(I)
	      DIV_WIN_CNT(I) = DIV_WIN_CNT(I) - 1
	      TYPE*,IAM(),'Added division ',I,' winner, ser: ',TRABUF(TSER)
	      GOTO 15
	   ENDIF
	END DO
15	CONTINUE


      type*,' '
      type*,'cdc:',trabuf(tcdc),' ser:',trabuf(tser)
      type*,'stat:',trabuf(tstat)
      type*,'kik1:',trabuf(twkick),'kik2:',trabuf(twkick2)

      CALL TRALOG(TRABUF,LOGREC)                                                
      CALL WLOG(SER-1,LOGREC,DUMMY)                                             
      GOTO 10                                                                   
11    CONTINUE                                                                  
      CALL CLOSEFIL(FDB)
      type*,'conversion complete'
      STOP
      END                                                                       
