C MISSNP.FOR
C  
C V07 16-MAR-2011 GPW NUMAGT=12288
C V06 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V05 04-NOV-1993 HXK TAKE INSTANT NAMES FROM CONCOM.
C V04 28-SEP-1993 GXA Corrected negative amounts for instant sales.
C V03 24-SEP-1993 GXA Corrected negative amount display.                       
C V02 09-MAY-1990 MGM REMOVED USE MRKS SINCE COULD NOT HANDLE NEGATIVES           
C V01 01-FEB-1990 LOU R. INITIAL RELEASE FOR FINLAND                              
C
C AGENT MISCELLANEOUS SNAPSHOT                                                  
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
        SUBROUTINE MISSNP(AGT,DAT,CLINE,LKEY,LAGT)                                
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'


        ! arguments
        INTEGER*4  AGT                                  !
        INTEGER*4  DAT                                  !
        INTEGER*4  CLINE(20)                            !
        INTEGER*4  LKEY                                 !
        INTEGER*4  LAGT                                 !

        ! variable                                       
        INTEGER*4  INDVTAB(AGAMLEN,MAXGAM)              !                         
        INTEGER*4  INDVSPE(ASPELEN,MAXGAM)              !                         
        INTEGER*4  INDVMIS(AMISLEN,NUMTOT)              !                         
        INTEGER*4  BUF(CDLEN)                           !
        INTEGER*4  FDB(7)                               !    
        INTEGER*4  CNT(AMISLEN)                         !
        INTEGER*4  AMT(AMISLEN)                         !            
        INTEGER*4  ST                                   !
        INTEGER*4  XX                                   !
        INTEGER*4  POS                                  !
        INTEGER*4  KEYNUM                               !
        INTEGER*4  VALUE                                !
        INTEGER*4  I                                    !
        INTEGER*4  J                                    !
        INTEGER*4  DAY                                  !
        INTEGER*4  IDX                                  !
        INTEGER*4  CLRKNUM                              !


        INTEGER*2  DBUF(LDATE_LEN)                      !

        REAL*8     K(5)                                 !


        CHARACTER    CZERO                              !                             
        CHARACTER*8  REPMOD(2)                          !                           
        CHARACTER*12 MISNAM(AMISLEN)                    !
                                                        
                                          
        DATA MISNAM/'INSTANT CASH','            ','            ', 
     *              '            ','            ','            ', 
     *              '            ',
     *              '            ','            ','            ', 
     *              '            ','            ','            ', 
     *              '            ',
     *              'MISC ITEM  0','MISC ITEM  1','MISC ITEM  2', 
     *              'MISC ITEM  3','MISC ITEM  4','MISC ITEM  5', 
     *              'MISC ITEM  6','MISC ITEM  7','MISC ITEM  8', 
     *              'MISC ITEM  9','PETTY CASH 0','PETTY CASH 1', 
     *              'PETTY CASH 2','PETTY CASH 3','PETTY CASH 4', 
     *              'PETTY CASH 5','PETTY CASH 6','PETTY CASH 7', 
     *              'PETTY CASH 8','PETTY CASH 9'/                                
C                                                                               
        DATA K/'OPStat  ','PASnum  ','BROnum  ',                                  
     *         'SUMmary ','CLErk   '/                                             
        DATA REPMOD/'CLERK   ','SUMMARY '/                                        
        DATA CZERO/Z0/,VALUE/0/                                                   
C                                                                               
C                                                                               
        IF(AGT.LT.1.OR.AGT.GT.NUMAGT) AGT=1                                       
        LSTAGT = AGTTAB(AGTNUM,AGT)                                                 
C                                                                               
C GET AGTSNP INPUT                                                              
C                                                                               
        POS=1                                                                     
        IF(DAT.LT.0) DAT=DAYCDC                                                   

        CALL KEY(CLINE,K,5,POS,KEYNUM)                                            
        IF(KEYNUM.GE.4) GOTO 5                                                    
        IF(POS.GT.40) GOTO 100                     !NO INPUT                      
        IF(KEYNUM.EQ.0)GOTO 10                     !INPUT ERROR                   

        CALL NUMB(CLINE,POS,VALUE)                 !GET VALUE                     
        IF(VALUE.LT.0)  GOTO 20                                                   
C                                                                               
5       CONTINUE                                                                  
        GOTO (30,40,50,60,70) KEYNUM                                              

10      CONTINUE                                                                  
        WRITE(CLIN23,923)                                                         

        GOTO 100                                                                  

20      CONTINUE                                                                  
        WRITE(CLIN23,1023)                                                        

        GOTO 100                                                                  
C                                                                               
C CHANGE OPSTAT                                                                 
C                                                                               
30      CONTINUE                                                                  
        IF(VALUE.LT.SIGNOF.OR.VALUE.GT.SERVFD) GOTO 20                            
        BUF(1) = 1                                                                  
        BUF(2) = VALUE                                                              
        BUF(3) = TCAGT                                                              
        BUF(5) = AGT                                                                
        BUF(6) = IDNUM                                                              
        GOTO 90                                                                   
C                                                                               
C CHANGE PASS NUMBER                                                            
C                                                                               
40      CONTINUE                                                                  
        IF(VALUE.LT.0.OR.VALUE.GT.9999) GOTO 20                                   
        BUF(1) = 2                                                                  
        BUF(2) = VALUE                                                              
        BUF(3) = TCAGT                                                              
        BUF(4) = 1                                                                  
        BUF(5) = AGT                                                                
        BUF(6) = IDNUM                                                              
        GOTO 90                                                                   
C                                                                               
C PROCESS BROADCAST                                                             
C                                                                               
50      CONTINUE                                                                  
        IF(VALUE.LT.1.OR.VALUE.GT.256) GOTO 20                                    
        BUF(1) = 5                                                                  
        BUF(2) = VALUE                                                              
        BUF(3) = TCSPE                                                              
        BUF(4) = 0                                                                  
        BUF(5) = AGT                                                                
        BUF(6) = IDNUM                                                              
        WRITE(CLIN23,1323) VALUE,AGT                                              
        GOTO 90                                                                   
C                                                                               
C SUM UP FOR ALL CLERKS FOR THIS AGENT                                          
C                                                                               
60      CONTINUE                                                                  
        ALLON=.TRUE.                                                              
        CLRKNUM = AGTHTB(AGTPASOFF,AGT)                                             
        CALL FNGETCLERK(AGT,CLRKNUM,1,INDVTAB,INDVSPE,INDVMIS)                      
        CLERKON=0                                                                 
        GOTO 120                                                                  
C                                                                               
C SET FOR INDIVIDUAL CLERK ONLY                                                 
C                                                                               
70      CONTINUE                                                                  
        ALLON=.FALSE.                                                             
        CALL FASTSET(0,INDVTAB,AGAMLEN*MAXGAM)                                    
        CALL FASTSET(0,INDVSPE,ASPELEN*MAXGAM)                                    
        CALL FASTSET(0,INDVMIS,AMISLEN*NUMTOT)                                    
        CLERKON=AGTHTB(AGTPASOFF,AGT)                                             
        GOTO 120                                                                  
C                                                                               
C                                                                               
C QUEUE COMMAND BUFFER TO SYSTEM INPUT QUEUE                                    
C                                                                               
90      CONTINUE                                                                  
        CALL VISCMD(BUF,ST)                                                       
        CALL XWAIT(2,1,ST)                                                         
C                                                                               
C                                                                               
100     CONTINUE                                                                  
C                                                                               
C READ AGENTS RECORD                                                            
C                                                                               
        IF(LKEY.EQ.8.AND.LAGT.EQ.AGT) GOTO 120                                    

        CALL OPENW(1,SFNAMES(1,ASF),0,0,0,ST)                                     
        CALL IOINIT(FDB,1,ASFSEC*256)                                                 
        IF(ST.NE.0) THEN                                                          
            CALL USRCLOS1(1)                                                           
            WRITE(CLIN23,1123) (SFNAMES(J,ASF),J=1,5),ST                            
            RETURN                                                                  
        ENDIF                                                                     

        ALLON   = .FALSE.                                                             
        CLERKON =  AGTHTB(AGTPASOFF,AGT)                                             
        CALL READW(FDB,AGT,ASFREC,ST)                                             
        IF(ST.NE.0) THEN                                                          
            CALL USRCLOS1(1)                                                           
            WRITE(CLIN23,1223) (SFNAMES(J,ASF),J=1,5),ST,AGT                        
            RETURN                                                                  
        ENDIF                                                                     
        CALL USRCLOS1(1)                                                             
        DO I = 1, 512                                                            
            IF(ASFBYT(I).EQ.CZERO) ASFBYT(I)=' '                                      
        END DO
C                                                                               
C                                                                               
120     CONTINUE                                                                  
        DBUF(5)=DAT                                                               
        CALL LCDATE(DBUF)                                                          
        IF(DAT.EQ.DAYCDC) GOTO 160                                                
C                                                                               
C IF NOT TODAYS MISCELLANEOUS DATA THEN GET DATA FROM FILE.                     
C                                                                               
        DO DAY = 1, 9                                                            
            IF(ASFDAT(ASFCDC,DAY).EQ.DAT) GOTO 140                                    
        END DO
        WRITE(CLIN23,1423) (DBUF(J),J=7,13)                                       

        RETURN                                                                    
C                                                                               
C                                                                               
140     CONTINUE                                                                  
        CALL FASTSET(0,AMT,AMISLEN)                                               
        CALL FASTSET(0,CNT,AMISLEN)                                               
        DO IDX = 1, AMISLEN                                                      
            CNT(IDX) = CNT(IDX)+ASFMIS(IDX,1,DAY)                                       
            AMT(IDX) = AMT(IDX)+ASFMIS(IDX,2,DAY)                                       
            ALLON    = .TRUE.                                                              
        END DO
        GOTO 180                                                                  
C                                                                               
C TODAYS SALES                                                                  
C                                                                               
C                                                                               
160     CONTINUE                                                                  
C                                                                               
C IF LAST CLERK SIGNED ON NOT SAME AS CURRENT ONE SIGNED ON AND                 
C IN CLERK MODE CLEAR SUMMARY TABLE.                                            
C                                                                               
        IF(CLERKON.NE.AGTHTB(AGTPASOFF,AGT).AND.(.NOT.ALLON)) THEN                
            CALL FASTSET(0,INDVTAB,AGAMLEN*MAXGAM)                                  
            CALL FASTSET(0,INDVSPE,ASPELEN*MAXGAM)                                  
            CALL FASTSET(0,INDVMIS,AMISLEN*NUMTOT)                                  
            CLERKON=AGTHTB(AGTPASOFF,AGT)                                           
        ENDIF                                                                     
C                                                                               
C IF CLERK SIGNED ON NOT SAME AS CURRENT ONE SIGNED ON AND                      
C IN SUMMARY MODE RE-READ CLERK FILE.                                           
C                                                                               
        IF(CLERKON.NE.AGTHTB(AGTPASOFF,AGT).AND.ALLON) THEN                       
            CLERKON=0                                                               
            CLRKNUM=AGTHTB(AGTPASOFF,AGT)                                           
            CALL FNGETCLERK(AGT,CLRKNUM,1,INDVTAB,INDVSPE,INDVMIS)                    
        ENDIF                                                                     
C                                                                               
C IF CLERK MODE FROM LAST SNAPSHOT CLEAR TABLE                                  
C                                                                               
        IF(.NOT.(ALLON)) THEN                                                     
            CALL FASTSET(0,INDVTAB,AGAMLEN*MAXGAM)                                  
            CALL FASTSET(0,INDVSPE,ASPELEN*MAXGAM)                                  
            CALL FASTSET(0,INDVMIS,AMISLEN*NUMTOT)                                  
        ENDIF                                                                     
C                                                                               
        CALL FASTSET(0,AMT,AMISLEN)                                               
        CALL FASTSET(0,CNT,AMISLEN)                                               
C                                                                               
        DO IDX = 1, AMISLEN                                                      
            CNT(IDX) = CNT(IDX)+AGTMIS(IDX,1,AGT)                                       
            AMT(IDX) = AMT(IDX)+AGTMIS(IDX,2,AGT)                                       
        END DO 
C                                                                               
C ADD UP FROM CLERK FILE                                                        
C                                                                               
        DO IDX=1,AMISLEN                                                      
            CNT(IDX)=CNT(IDX)+INDVMIS(IDX,1)                                          
            AMT(IDX)=AMT(IDX)+INDVMIS(IDX,2)                                          
        END DO
C                                                                               
C                                                                               
180     CONTINUE                                                                  
C                                                                               
C ENCODE AGENT FINANCIAL SNAPSHOT                                               
C                                                                               
C                                                                               
        WRITE(CLIN1,901) (DBUF(J),J=7,13)                                         
        WRITE(CLIN2,902) (ASFBYT(J),J=SNAME,ENAME),                               
     *                    AGTTAB(AGTNUM,AGT)                                   
        WRITE(CLIN3,903) (ASFBYT(J),J=SSTRT,ESTRT),AGT                            
        WRITE(CLIN4,904) (ASFBYT(J),J=SCITY,ECITY)                               
        IF(.NOT.ALLON) THEN                                                       
            WRITE(CLIN5,906) REPMOD(1),AGTHTB(AGTPASOFF,AGT)                        
        ELSE                                                                      
            WRITE(CLIN5,907) REPMOD(2)                                              
        ENDIF      
                                                               
C        DO XX = 1, PRM_NUMINS                                                             
C             WRITE(XNEW(XX+5),908) MISNAM(XX),
C     *                             CNT(XX),
C     *                             CSMONY(AMT(XX),10,BETUNIT),
C     *                             MISNAM(XX+PRM_NUMINS),                    
C     *                             CNT(XX+PRM_NUMINS),
C     *                             CSMONY(AMT(XX+PRM_NUMINS),10,BETUNIT)
C        END DO



C We display only the first 12 characters (3A4) of the Instant games names, 
C because of the max width of the screen=80.

C        DO XX = 1, PRM_NUMINS
C             WRITE(XNEW(XX+5),909) (SCC_INSNAM(J,XX),J=1,3),' Sale',
C     *                             CNT(XX),
C     *                             CSMONY(AMT(XX),10,BETUNIT),
C     *                             (SCC_INSNAM(J,XX),J=1,3),' Cash',
C     *                             CNT(XX+PRM_NUMINS),
C     *                             CSMONY(AMT(XX+PRM_NUMINS),10,BETUNIT)
C        END DO
        WRITE(XNEW(6),910) MISNAM(1),
     *                     CNT(1),
     *                     CSMONY(AMT(1),10,BETUNIT)

        DO XX = (PRM_NUMINS*2)+1, (PRM_NUMINS*2)+PRM_NUMISAL                                                           
            WRITE(XNEW(XX-8),908) MISNAM(XX),
     *                            CNT(XX),
     *                            CSMONY(AMT(XX),10,BETUNIT),
     *                            MISNAM(XX+PRM_NUMISAL),                   
     *                            CNT(XX+PRM_NUMISAL),
     *                            CSMONY(AMT(XX+PRM_NUMISAL),10,BETUNIT)
        END DO







        RETURN                                                                    
C                                                                               
C FORMAT STATEMENTS                                                             
C                                                                               
901     FORMAT('Miscellaneous data for ',7A2)                                     
902     FORMAT('Name',6X,<LNAME>A1,T50,' Agtnum  ',T60,I8.8)                      
903     FORMAT('Address',3X,<LSTRT>A1,T50,' Terminal',T60,I5.5)                          
904     FORMAT(10X,<LCITY>A1,15X)                                     
906     FORMAT(16X,'<< MODE >> ',A8,' ID ',I4)                                    
907     FORMAT(16X,'<< MODE >> ',A8,10(' '))                                      
C908     FORMAT(2(1X,A12,1X,I8,1X,I8,'.',I2.2))                                    
C908     FORMAT(2(1X,A12,1X,I8,1X,A10))                                    
908     FORMAT(2(1X,A12,5X,1X,I8,1X,A10))                                    
909     FORMAT(2(1X,3A4,A5,1X,I8,1X,A10))                                    
910     FORMAT(1X,A12,5X,1X,I8,1X,A10)                                    
923     FORMAT('Input error')                                                     
1023    FORMAT('Value error')                                                     
1123    FORMAT(5A4,' open error ',I4)                                             
1223    FORMAT(5A4,' read error ',I4,' record ',I4)                               
1323    FORMAT('Message ',I4,' queued to agent ',I4)                              
1423    FORMAT('Miscellaneous data for ',7A2,' not posted')                       

        END                                                                       
