C ANLTCF.FOR                                                                    
C  
C V08 08-JUN-2000 OXK CLEANUP
C V07 17 May 1996 HXK Update from Wojtek, Siew Mun
C V06 07 Sep 1993 HXN Initial revision.
C V05 27 Aug 1993 HXN Initial revision.
C V04 19-FEB-1992 HJK DO NOT REPORT FRACS FOR FINLAND
C V03 25-APR-1991 MGM DO NOT REPORT INCA TRANSACTIONS
C     			  AS ABNOMALITIES            
C V01 22-MAR-1990 TDM INITIAL RELEASE FOR DENMARK    
C V02 2X-AUG-1990 TFH REWRITTEN FOR SWEDEN           
C                                                                               
C THIS PROGRAM WILL READ THROUGH THE VALIDATION FILE TO CHECK FOR ERRORS
C                                                                               
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, Providence, Rhode             
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

C=======OPTIONS /CHECK/EXT
      PROGRAM ANLTCF                                                            
      IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'

      INTEGER*4 MAXREC,TEMP,REPCDC,COPY,EXT,ST
      INTEGER*4 PAGE,TOTREC,BLK,XCNT,ZFLAG,MLTCNT,NDX,STA                       
      INTEGER*4 LO,HI,CNT,X,AVAIL,GIND,GTYP,J                                   
      INTEGER*4 K,EXPCNT,DUMCNT,NUMPRT,JNDX,GAM,BLKCNT                      

      PARAMETER   (MAXREC=2048/LREC)                                            
      INTEGER*4    BLKUSE(MAXREC+1)                                             
      REAL*8       TOTS(NUMTOT,6,MAXGAM), AMT
      INTEGER*4    MLTREC(0:3)                                                  
      INTEGER*4    FDB(7)                                                       

      INTEGER*4    BIGBUF(32*64)                                                
      INTEGER*4    BUF(LREC,MAXREC)                                             
      EQUIVALENCE (BIGBUF,BUF)                                                  

      INTEGER*2 I2TEMP(2)                                                       
      EQUIVALENCE (TEMP,I2TEMP)                                                 


      REAL*8       PCNT                                                         

C BEGIN CODE ----------------------------------------------

      CALL COPYRITE                                                             
      TYPE *                                                                    
      TYPE *,'<<<<< ANLTCF Validation File Analysis   V01 >>>>>'                
      TYPE *                                                                    

      CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)               
      IF(EXT.LT.0) STOP                                                         

      REPCDC=DAYCDC                                                             


C READ SCF RECORD                                                               
C ---------------
      CALL GETSCONF(SCFREC,ST)
      IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)

C CLEAR TOTALS                                                                  
C ------------
      CALL FASTSET(0,BLKUSE,MAXREC+1)                                           
      CALL FASTSET(0,TOTS,NUMTOT*6*MAXGAM*2)

      DO 130 K=0,3                                                              
        MLTREC(K) = 0                                                           
130   CONTINUE                                                                  

      EXPCNT = 0                                                                
      DUMCNT = 0                                                                

      NUMPRT=0                                                                  



C OPEN TCF FILE
C -------------
      CALL OPENW(1,SCFSFN(1,TCF),0,0,0,ST)                                      
      IF (ST .NE. 0)THEN                                                        
        CALL FILERR(SCFSFN(1,TCF),1,ST,0)                                       
        TYPE *,'***ERROR ON OPEN OF FILE OR DEVICE -- # ',ST                    
        STOP
      ENDIF                                                                     

      CALL IOINIT(FDB,1,32*256)   !IN BYTES, NOT IN SECTORS

      PAGE = 0                                                                  

      CALL ROPEN('ANLTCF.REP',7,ST)                                             
      IF(ST.NE.0)THEN                                                           
        TYPE *,'CANNOT OPEN ANLTCF.REP > ',ST                                   
        STOP                                                                    
      ENDIF                                                                     

      CALL TITLE('CARRY OVER FILE ANALYSIS',                                    
     *           '  ANLTCF',1,7,PAGE,REPCDC)                                    
      WRITE(7,9000)                                                             

      WRITE(7,501)                                                              
501   FORMAT(///,X,30('*'),' ABNORMALITIES ',30('*'),/)                         


      TOTREC = 0                                                                
      BLK = 0                                                                   



C READ NEXT BLOCK                                                               
C ---------------
1000  CONTINUE                                                                  
      BLK = BLK + 1                                                             
      CALL READW(FDB,BLK,BIGBUF,ST)                                             
      IF (ST .NE. 0) THEN                                                       
        CLOSE(UNIT=1)                                                           
        GO TO 5000                                                              
      ENDIF                                                                     
C                                                                               
      XCNT = 0                                                                  
      ZFLAG = 0                                                                 
      MLTCNT = 0                                                                

      DO 4000 NDX=1,MAXREC                                                      
        IF (MLTCNT .GT. 0) THEN                                                 
          MLTCNT = MLTCNT - 1                                                   
          GO TO 4000                                                            
        ENDIF                                                                   
        IF(BUF(1,NDX) .EQ. 0) THEN                                              
          IF(ZFLAG .EQ. 0) ZFLAG = NDX                                          
          GO TO 4000                                                            
        ENDIF                                                                   
        IF(ZFLAG .NE. 0)THEN                                                    
          WRITE(7,1001)BLK,ZFLAG,NDX-1                                          
          ZFLAG = 0                                                             
        ENDIF                                                                   
C                                                                               
        MLTCNT = ISHFT(BUF(1,NDX),-30)                                          
        MLTREC(MLTCNT) = MLTREC(MLTCNT) + 1                                     
        XCNT = XCNT + MLTCNT + 1                                                
        CALL LOGTRA(TRABUF,BUF(1,NDX))                                          
        IF(NUMPRT.GT.0)THEN                                                     
          NUMPRT=NUMPRT-1                                                       
          WRITE(7,*)TRABUF                                                      
        ENDIF                                                                   
C                                                                               
        IF(TRABUF(TCDC) .LT. 0) THEN                                            
          WRITE(7,10021)BLK,NDX                                                 
10021     FORMAT(X,'BLOCK',I8,'/',I3,' IS A DUMMY RECORD')                      
          DUMCNT = DUMCNT + 1                                                   
          GOTO 4000                                                             
        ENDIF                                                                   
C                                                                               
C**     IF(TRABUF(TWODUR).LT.5) GOTO 4000                                       
C                                                                               
        STA = TRABUF(TSTAT)                                                     
        IF(STA .EQ. GOOD) THEN                                                  
          JNDX = 1                                                              
        ELSE IF(STA .EQ. EXCH) THEN                                             
          JNDX = 2                                                              
        ELSE IF(STA.EQ.VOID.OR.STA.EQ.INCA.OR.STA.EQ.FRAC) THEN                 
          JNDX = 3                                                              
        ELSE IF(STA .EQ. XCHD) THEN                                             
          JNDX = 4                                                              
        ELSE                                                                    
          WRITE(7,1002)BLK,NDX,TRABUF(TCDC),TRABUF(TSER),'STATUS',STA
          GO TO 4000                                                            
        ENDIF                                                                   

        GAM = TRABUF(TGAM)                                                      
        IF(GAM.LT.1 .OR. GAM.GT.MAXGAM)THEN                                     
          WRITE(7,1002)BLK,NDX,TRABUF(TCDC),TRABUF(TSER),                       
     *                 'GAME  ',GAM                                             
          GO TO 4000                                                            
        ENDIF                                                                   

        TOTS(1,JNDX,GAM) = TOTS(1,JNDX,GAM) + 1 
	AMT = TRABUF(TWTOT)*DYN_BETUNIT 
	IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / DFLOAT(TRABUF(TNFRAC))
        TOTS(2,JNDX,GAM) = TOTS(2,JNDX,GAM) + AMT

4000  CONTINUE                                                                  

      BLKUSE(XCNT+1) = BLKUSE(XCNT+1) + 1                                       
      TOTREC = TOTREC + XCNT                                                    
      GO TO 1000                                                                

C --------------------------------------------------------



C COME HERE WHEN ALL DONE
C -----------------------
5000  CONTINUE                                                                  
      CLOSE(UNIT=1)                                                             
      BLKCNT = BLK - 1                                                          



C TOTAL UP ALL SHARES INTO DRAW 101                                             
C ---------------------------------
      LO=MAXREC+1                                                               
      HI=1                                                                      

      DO 6010 K=1,MAXREC+1                                                      
        IF(BLKUSE(K).NE.0)THEN                                                  
          LO=MIN(K,LO)                                                          
          HI=K                                                                  
        ENDIF                                                                   
6010  CONTINUE                                                                  


      CNT=0                                                                     
      DO 6100 K=LO,HI                                                           
        CNT=CNT+1                                                               
        IF(MOD(CNT,50).EQ.1)THEN                                                
          CALL TITLE('CARRY OVER FILE ANALYSIS',                                
     *               '  ANLTCF',1,7,PAGE,REPCDC)                                
          WRITE(7,9000)                                                         
C                                                                               
          WRITE(7,6001)                                                         
6001      FORMAT(///,X,30('*'),' BLOCK USAGE ',30('*'),/)                       
        ENDIF                                                                   

        X = BLKUSE(K)                                                           
        PCNT = DFLOAT(X) / DFLOAT(BLKCNT) * 100.0D0                             
        WRITE(7,6002) K-1,X,PCNT                                                
6002    FORMAT(X,'TOTAL BUCKETS WITH',I4,' RECORDS = ',I6,X,F6.2,'%')           
6100  CONTINUE                                                                  



      AVAIL = BLKCNT * MAXREC                                                   
      PCNT = DFLOAT(TOTREC) / DFLOAT(AVAIL) * 100.0D0                           

      WRITE(7,6003)TOTREC,BLKCNT,PCNT                                           
      WRITE(7,6004)(K+1,MLTREC(K),K=0,3)                                        
      WRITE(7,6005) DUMCNT, SCFSFN(1,TCF)                                       
      CALL TITLE('CARRY OVER FILE ANALYSIS',                                    
     *           '  ANLTCF',1,7,PAGE,REPCDC)                                    
      WRITE(7,9000)                                                             

      DO 6800 GAM=1,MAXGAM                                                      
         GIND = SCFGNT(GAMIDX,GAM)                                              
         GTYP = SCFGNT(GAMTYP,GAM)                                              
         IF (GTYP.LE.0) GOTO 6800                                               
         IF(TOTS(1,1,GAM).LE.0) GOTO 6800                                       
         WRITE(7,6301) GTNAMES(GTYP),GIND                                       
         WRITE(7,6303)( TOTS(1,J,GAM),
     *                  TOTS(2,J,GAM)/100.0D0, J=1,4)
6800  CONTINUE                                                                  

      CLOSE(UNIT=7)                                                             
      CALL SPOOL('ANLTCF.REP',COPY,ST)                                          



C     ===================== Format Statements ======================            
C                                                                               
9000  FORMAT(1X,131('='))                                                       
1001  FORMAT(X,'HOLE IN BLOCK ',I8,' FROM',I4,' TO',I4)                         
1002  FORMAT(X,'BLOCK',I8,'/',I3,' SRL',I5,'/',I9,' *** BAD ',A,I12,            
     *         ' *** IGNORED')                                                  
6003  FORMAT(/,X,I12,' TOTAL RECORDS IN ',I8,' BLOCKS',/,                       
     *         X,F6.2,'% OF FILE IS USED')                                      
6004  FORMAT(//,4(X,'# OF RECORDS USING',I2,' SLOTS=',I12,/))                   
6005  FORMAT(/,' THERE ARE ',I4,' DUMMY RECORDS IN ',A16)                       
6301  FORMAT(////,30('*'),X,' ON FILE FOR ',A8,I1,//,                           
C     *         X,10X,7X,'GOOD',18X,'EXCH',15X,'CXL/DEL',
C     *               13X,'XCHD',/,                      
     *         X,10X,7X,'GOOD',18X,'EXCH',12X,'VOID/INCA/FRAC',
     *               9X,'XCHD',/,                                              
     *         X,10X,4(2X,'COUNT',7X,'AMOUNT'),/)                               
6303  FORMAT(/,X,'*  TOTAL *',4(F7.0,F13.2))


      END      ! ANLTCF.FCC
