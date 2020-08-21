C TCFDMP.FOR
C $Log:   GXAFXT:[GOLS]TCFDMP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:28:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   31 Aug 1993 20:08:56   HXN
C  Initial revision.
C  
C     Rev 1.0   26 Jul 1993 12:20:12   HXN
C  Initial revision.
C                                                                               
C V02  22-FEB-93  HJK   CHANGED FOR VIKING / JOKERI                             
C V01  18-DEC-89  GCAN  INITIAL RELEASE FOR FINLAND                             
C                                                                               
C DUMPS ALL THE CARRYOVERS TO TAPE EXCEPT THE EXCHANGED                          
C (LOG FORMAT USED)                                                             
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
C Copyright 1993 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      PROGRAM TCFDMP                                                            
      IMPLICIT NONE                                              


	INCLUDE 'INCLIB:GLOBAL.DEF'                                                           
	INCLUDE 'INCLIB:CONCOM.DEF'                                                           
	INCLUDE 'INCLIB:DESTRA.DEF'                                                           
	INCLUDE 'INCLIB:PRMLOG.DEF'                                                           
	INCLUDE 'INCLIB:DESLOG.DEF'                                                           
	INCLUDE 'INCLIB:RECSCF.DEF'                                                           
	INCLUDE 'INCLIB:HSHCOM.DEF'                                                           
	INCLUDE 'INCLIB:DATBUF.DEF'                                                           



      INTEGER*4 TCFLU,TAPLU,TAPLU2,TPIDFL,MAXMAG,
     *          BLKLEN,RECMAX
	
      PARAMETER(TCFLU=01)                                                       
      PARAMETER(TAPLU=02)                                                       
      PARAMETER(TAPLU2=03)                                                      
      PARAMETER(TPIDFL=2)          !TAPE IDENTIFIER FOR CARRYOVERS              
      PARAMETER(MAXMAG=2)                                                       
      PARAMETER(BLKLEN=2048)                                                    
      PARAMETER(RECMAX=127)        !128 - 1 FOR THE HEADER REC                  


      INTEGER*4 TFDB(7),TFDB2(7),
     *		TAPBLK(BLKLEN),
     *          LOGBUF(LREC*3),                                                  
     *          REV              /01/

      CHARACTER*20 MAGNAM,MAGNAM2

      LOGICAL EOF/.FALSE./,TAPE2/.FALSE./                                       


      INTEGER*4 BLKCNT,RECIND,RECLEFT,ST,ST1,
     *          MAGNUM,MAGNUM2,RET,
     *          RECCNTS,NUMREC,MOVLEN


C BEGIN CODE ---------------------------------------                                                                               

      CALL COPYRITE                                                             

      TYPE*,' '                                                                 
      TYPE*,'<<<<< TCFDMP  V',REV,' Transaction Carryover Dump program >>>>> '                              
      TYPE*,' '                                                                 


C SET / CLEAR VARIABLES                                                         
C ---------------------                                                                               
      BLKCNT=1                                                                  
      RECIND=LREC+1                                                             
      RECLEFT=RECMAX                                                            
      CALL FASTSET(0,TAPBLK,BLKLEN)                                             


C GET SYSTEM CONTROLL CONFIGURATION                                             
C ---------------------------------
      CALL GETSCONF(SCFREC,ST)                                                  




C GET TAPE DRIVE NUMBER AND BUILD DEVICE NAME                                   
C -------------------------------------------
      CALL WIMG(5,'Enter tape drive name: ')
      ACCEPT '(A)',MAGNAM
      IF(MAGNAM.EQ.'E'  .OR.  MAGNAM.EQ.'e') STOP

      CALL WIMG(5,'Enter tape 2 drive name: ')
      ACCEPT '(A)',MAGNAM2
      IF(MAGNAM2.NE.'E' .AND. MAGNAM2.NE.'e') THEN
	 TAPE2=.TRUE.                   
	 TYPE*,' MAGNAM2 : ',MAGNAM2,'...'
      ENDIF 


      CALL WIMG(5,'Mount tape(s) and hit <RETURN> ')                            
      READ(5,9000) RET                                                          




C OPEN TAPE AND REWIND IT           
C -----------------------
      CALL TAPOPEN (TFDB,MAGNAM,ST)                                         
      CALL TAPINT  (TFDB,TAPLU,BLKLEN*4)
      CALL XREWIND (TFDB,ST1)           
      IF(ST.NE.0.OR.ST1.NE.0) THEN                                              
         TYPE*,'Tape open error on MAG:',MAGNUM,                                
     *         ' st - ',ST,'  rewind st - ',ST1                                 
         CLOSE(TAPLU)                                                           
         STOP                                                                   
      ENDIF                                                                     


      IF(TAPE2) THEN                                                            
        CALL TAPOPEN (TFDB2,MAGNAM2,ST)                                     
        CALL TAPINT  (TFDB2,TAPLU2,BLKLEN*4)                                      
        CALL XREWIND (TFDB2,ST1)                                                  
        IF(ST.NE.0.OR.ST1.NE.0) THEN                                            
           TYPE*,'Tape open error on MAG:',MAGNUM2,                             
     *         ' st - ',ST,'  rewind st - ',ST1                                 
           CLOSE(TAPLU2)                                                        
           STOP                                                                 
        ENDIF                                                                   
      ENDIF                                                                     


C BUILD TAPE HEADER AND WRITE TO TAPE                                           
C -----------------------------------
      CALL TAPHDR(TAPBLK,TPIDFL)                                                
      CALL WTAPEW(TFDB,TAPBLK,ST)                                               
      IF(ST.NE.0) THEN                                                          
         TYPE*,'Tape Write error on MAG:',MAGNUM,'  st - ',ST                   
         CLOSE(TAPLU)                                                           
         STOP                                                                   
      ENDIF                                                                     


      IF(TAPE2) THEN                                                            
        CALL WTAPEW(TFDB2,TAPBLK,ST)                                            
        IF(ST.NE.0) THEN                                                        
           TYPE*,'Tape Write error on MAG:',MAGNUM2,'  st - ',ST                
           CLOSE(TAPLU2)                                                        
           STOP                                                                 
        ENDIF                                                                   
      ENDIF                                                                     





C OPEN THE CARRYOVER FILE                                                       
C -----------------------
      CALL IOPEN(SCFSFN(1,TCF),TCFLU,LREC*2,LCDC,LSER*2-1,ST)                   
      IF(ST.NE.0) CALL FILERR(SCFSFN(1,TCF),1,ST,0)                             




C CARRYOVER READ LOOP                                                           
C -------------------
      RECCNTS=0                                                                 

100   CONTINUE                                                                  

      CALL READTCF(LOGBUF,TCFLU,EOF)                                                  
      IF(EOF) GOTO 1000                                                         


C     CHECK IF TICKET EXCHANGED, IF SO, SKIPP                                       
C     ---------------------------------------
      CALL LOGTRA(TRABUF,LOGBUF)                                                
C***  IF(TRABUF(TSTAT).EQ.XCHD.AND.TRABUF(TGAM).NE.7) GOTO 100 ! 7=KENO
      IF(TRABUF(TSTAT).EQ.XCHD) GOTO 100                  
      IF(TRABUF(TSTAT).EQ.FRAC) GOTO 100                                        


C     DETERMINE NUMBER OF LOG RECORDS                                               
C     -------------------------------
      NUMREC=TRABUF(TSIZE)                                                      


C     CHECK IF IT FITTS IN CURRENT BLOCK                                            
C     IF NOT WRITE BLOCK TO TAPE , WRITE HEADER AND DATA IN NEW BLOCK               
C     ELSE MOVE DATA OVER TO BLOCK                                                  
C     ----------------------------
      IF(NUMREC.GT.RECLEFT) THEN                                                

         IF (MOD(BLKCNT-1,1000).EQ.0) TYPE*,' TCFDMP in progress ...',BLKCNT
         BLKCNT=BLKCNT+1                                                        
         TAPBLK(2)=BLKCNT                                                       
         CALL WTAPEW(TFDB,TAPBLK,ST)                                            
         IF(ST.NE.0) THEN                                                       
            TYPE*,'Tape Write error on MAG:',MAGNUM,' st - ',ST                 
            CLOSE(TAPLU)                                                        
            CALL ICLOSE(TCFLU,LOGBUF,ST)                                        
            IF(ST.NE.0) CALL FILERR(SCFSFN(1,TCF),4,ST,0)                       
            STOP                                                                
         ENDIF                                                                  


         IF(TAPE2) THEN                                                         
           CALL WTAPEW(TFDB2,TAPBLK,ST)                                         
           IF(ST.NE.0) THEN                                                     
             TYPE*,'Tape Write error on MAG:',MAGNUM2,' st - ',ST               
             CLOSE(TAPLU2)                                                      
             CALL ICLOSE(TCFLU,LOGBUF,ST)                                       
             IF(ST.NE.0) CALL FILERR(SCFSFN(1,TCF),4,ST,0)                      
             STOP                                                               
           ENDIF                                                                
         ENDIF                                                                  
         CALL FASTSET(0,TAPBLK,BLKLEN)                                          
         RECIND=LREC+1                                                          
         RECLEFT=RECMAX                                                         
      ENDIF                                                                     


      MOVLEN=LREC*NUMREC                                                        
      CALL FASTMOV(LOGBUF,TAPBLK(RECIND),MOVLEN)                                
      RECIND=RECIND+MOVLEN                                                      
      RECLEFT=RECLEFT-NUMREC                                                    
      RECCNTS=RECCNTS+1                                                         
      GOTO 100                                                                  





C END OF FILE, WRITE LAST BLOCK IF NOT EMPTY                                    
C ------------------------------------------
1000  CONTINUE                                                                  

      IF(RECLEFT.NE.RECMAX) THEN                                                
         BLKCNT=BLKCNT+1                                                        
         TAPBLK(2)=BLKCNT                                                       
         CALL WTAPEW(TFDB,TAPBLK,ST)                                            
         IF(ST.NE.0) THEN                                                       
            TYPE*,'Tape write error on MAG:',MAGNUM,                            
     *            ' Block - ',BLKCNT,'  st - ',ST                               
            CLOSE(TAPLU)                                                        
            STOP                                                                
         ENDIF                                                                  


         IF(TAPE2) THEN                                                         
           CALL WTAPEW(TFDB2,TAPBLK,ST)                                         
           IF(ST.NE.0) THEN                                                     
              TYPE*,'Tape write error on MAG:',MAGNUM2,                         
     *            ' Block - ',BLKCNT,'  st - ',ST                               
              CLOSE(TAPLU2)                                                     
              STOP                                                              
           ENDIF                                                                
         ENDIF                                                                  
      ENDIF                                                                     




C CLOSE CARRYOVER FILE.
C WRITE END OF TAPE MARK AND CLOSE TAPE.
C -------------------------------------
      CALL ICLOSE (TCFLU,LOGBUF,ST)                                              
      IF(ST.NE.0) CALL FILERR(SCFSFN(1,TCF),4,ST,0)                             


      CALL WEOT(TFDB,ST)                                                        
      CALL WEOT(TFDB,ST)                                                        
      CALL XREWIND (TFDB,ST)                                                      
      CALL TAPCLOS (TFDB,ST)                                                              

      IF(TAPE2) THEN                                                            
        CALL WEOT(TFDB2,ST)                                                     
        CALL WEOT(TFDB2,ST)                                                     
        CALL XREWIND (TFDB2,ST)                                                   
        CALL TAPCLOS (TFDB2,ST)                                                           
      ENDIF                                                                     

      TYPE*,'Total number of blocks  written to tape: ',BLKCNT                   
      TYPE*,'Total number of records written to tape: ',RECCNTS                 
C                                                                               
C                                                                               
9000  FORMAT(A4)                                                                
      END        ! TCFDMP.FCC
