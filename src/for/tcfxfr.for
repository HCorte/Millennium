C TCFXFR.FOR
C $Log:   GXAFXT:[GOLS]TCFXFR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:29:16   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   12 Sep 1994 12:17:40   HXK
C  Initial revision.
C  
C                                                                               
C DUMPS ALL THE CARRYOVERS TO FILE EXCEPT THE EXCHANGED                          
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
C Copyright 1994 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      PROGRAM TCFXFR
      IMPLICIT NONE                                              


	INCLUDE 'INCLIB:GLOBAL.DEF'                                                           
	INCLUDE 'INCLIB:CONCOM.DEF'                                                           
	INCLUDE 'INCLIB:DESTRA.DEF'                                                           
	INCLUDE 'INCLIB:PRMLOG.DEF'                                                           
	INCLUDE 'INCLIB:DESLOG.DEF'                                                           
	INCLUDE 'INCLIB:RECSCF.DEF'                                                           
	INCLUDE 'INCLIB:HSHCOM.DEF'                                                           
	INCLUDE 'INCLIB:DATBUF.DEF'                                                           

      INTEGER*4 TCFLU,TPIDFL,
     *          BLKLEN,RECMAX
      PARAMETER(TCFLU=01)                                                       
      PARAMETER(TPIDFL=2)          !TAPE IDENTIFIER FOR CARRYOVERS                                                   
      PARAMETER(BLKLEN=2048)                                                    
      PARAMETER(RECMAX=127)        !128 - 1 FOR THE HEADER REC                  
      INTEGER*4 TFDB(7),
     *		TAPBLK(BLKLEN),
     *          LOGBUF(LREC*3),                                                  
     *          REV              /01/

      INTEGER*4 LUN,WRTCNT
      INTEGER*4 NOCONSIG
      INCLUDE   'INCLIB:DISKIO.DEF'
      INTEGER*4 DISKCREATE
      EXTERNAL  DISKCREATE
      EXTERNAL  NOCONSIG

      LOGICAL EOF/.FALSE./

      INTEGER*4 BLKCNT,RECIND,RECLEFT,ST,
     *          RECCNTS,NUMREC,MOVLEN

C BEGIN CODE ---------------------------------------                                                                               
      CALL COPYRITE                                                             

      CALL LIB$ESTABLISH ( NOCONSIG )

      TYPE*,' '                                                                 
      TYPE*,'<<<<< TCFXFR V',REV,' Transaction Carryover XFR program >>>>> '                              
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

C     OPEN XFR FILE    
C     -------------
	LUN=2
        OPEN(UNIT=LUN,FILE='FILE:TCFDMP.XFR',IOSTAT=ST,
     *       STATUS='NEW',RECL=128, BLOCKSIZE=512,
     *	     ORGANIZATION='SEQUENTIAL',
     *	     ACCESS='SEQUENTIAL',
     *       USEROPEN=DISKCREATE,
     *       RECORDTYPE='FIXED')
        IF(ST.NE.0) THEN
	     CALL FILERR('FILE:TCFDMP.XFR' ,1,ST,0)
             STOP
        ENDIF
	CALL IOINIT(TFDB,LUN,DBLOCK*4)

        WRTCNT = 1
                             
        TYPE*,' '
        TYPE*,'Generating TCFDMP.XFR'
        TYPE*,' '

C BUILD TAPE HEADER AND WRITE TO TAPE                                           
C -----------------------------------
      CALL FTPHDR(TAPBLK,TPIDFL)                                                
      CALL FTP_WRITEW(TFDB,WRTCNT,TAPBLK,ST)                                               
      IF(ST.NE.0) THEN                                                          
         TYPE*,'Write error HEADER, st - ',ST                                      
         STOP                                                                   
      ENDIF                                                                     
      WRTCNT = WRTCNT + 1

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
      IF(TRABUF(TSTAT).EQ.XCHD) GOTO 100                  
      IF(TRABUF(TSTAT).EQ.FRAC) GOTO 100                                        

C     DETERMINE NUMBER OF LOG RECORDS                                               
C     -------------------------------
      NUMREC=TRABUF(TSIZE)                                                      

C     CHECK IF IT FITS IN CURRENT BLOCK                                            
C     IF NOT WRITE BLOCK TO TAPE , WRITE HEADER AND DATA IN NEW BLOCK               
C     ELSE MOVE DATA OVER TO BLOCK                                                  
C     ----------------------------
      IF(NUMREC.GT.RECLEFT) THEN                                                
         IF (MOD(BLKCNT-1,1000).EQ.0) THEN
            TYPE*,' TCFXFR in progress ...',BLKCNT
         ENDIF
         BLKCNT=BLKCNT+1                                                        
         TAPBLK(2)=BLKCNT                                                       
         CALL FTP_WRITEW(TFDB,WRTCNT,TAPBLK,ST)                                            
         IF(ST.NE.0) THEN                                                       
            TYPE*,'Write error block:',WRTCNT,' st - ',ST                                                    
            CALL ICLOSE(TCFLU,LOGBUF,ST)                                        
            IF(ST.NE.0) CALL FILERR(SCFSFN(1,TCF),4,ST,0)                       
            STOP                                                                
         ENDIF                                                                  
         CALL FASTSET(0,TAPBLK,BLKLEN)                                          
         RECIND=LREC+1                                                          
         RECLEFT=RECMAX
         WRTCNT = WRTCNT + 1                                                         
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
         CALL FTP_WRITEW(TFDB,WRTCNT,TAPBLK,ST)                                            
         IF(ST.NE.0) THEN                                                       
            TYPE*,'write error Block - ',BLKCNT,'  st - ',ST           
            STOP                                                                
         ENDIF
      ENDIF                                                                     

C CLOSE CARRYOVER FILE.
C WRITE END OF TAPE MARK AND CLOSE TAPE.
C -------------------------------------
      CALL ICLOSE (TCFLU,LOGBUF,ST)                                              
      IF(ST.NE.0) CALL FILERR(SCFSFN(1,TCF),4,ST,0)
      CALL CLOSEFIL(TFDB)                            
      TYPE*,'Total number of blocks  written to file: ',BLKCNT
      TYPE*,'Total number of writes to file         : ',WRTCNT-1                  
      TYPE*,'Total number of records written to file: ',RECCNTS                 
C                                                                               
9000  FORMAT(A4)                                                                
      END        ! TCFXFR.FOR
