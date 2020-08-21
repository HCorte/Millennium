C PROGRAM TO GENERATE REPORTS ON EXTRA WINNERS
C
C V01 27-NOV-97 UXN Initial release. (produced from FINDBIG.FOR)
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
C Copyright 1997 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                              
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
	PROGRAM EXWINRPT
	IMPLICIT NONE                                                     
C                                 
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'                             
	INCLUDE 'INCLIB:CONCOM.DEF'                             
	INCLUDE 'INCLIB:AGTCOM.DEF'                             
	INCLUDE 'INCLIB:DESVAL.DEF'                             
	INCLUDE 'INCLIB:VALFIL.DEF'                             
	INCLUDE 'INCLIB:HSHCOM.DEF'                             
	INCLUDE 'INCLIB:TNAMES.DEF'                             
	INCLUDE 'INCLIB:DATBUF.DEF'                             
	INCLUDE 'INCLIB:GTNAMES.DEF'                            
	INCLUDE 'INCLIB:DLTREC.DEF'                             
	INCLUDE 'INCLIB:VDETAIL.DEF'                              
	INTEGER*4    TUBSIZ
	PARAMETER   (TUBSIZ=I4BUCSIZ*7)

	INTEGER*4    GNUM,GTYP,GIND,ST,COPY
	INTEGER*4    SSER
	INTEGER*4    SCHK,K,DIV,STATUS
	INTEGER*4    LDIV
                                                            
	INTEGER*4    VLFBUF(TUBSIZ),FDB(7)                        
	INTEGER*4    LINCNT /99/
	INTEGER*4    REPLU,  DRAW, PAGE
	INTEGER*2    DATE(LDATE_LEN) /LDATE_LEN*0/

	CHARACTER    REPHDR*51                                            
	CHARACTER    REPNAM*12 /'L1EXTWIN.REP'/
	CHARACTER*3  JOKNAM
	INTEGER*4    TOTAMOUNT,AMOUNT,FRCS,SHARES(LTGDIV)
	LOGICAL	    FULSER,WINNER
	INTEGER*4   PRZIND,DIND,SHR
	CHARACTER*20 PASENT
	CHARACTER*8  PASPAS
	EQUIVALENCE  (PASPAS,PASENT)
C
	CALL COPYRITE
C
C READ DRAW NUMBER                                        
C 
      GIND = 1
      GTYP = TLTO ! EXTRA DRAW IS ONLY FOR LOTTO IDX 1 AT THE MOMENT...
      GNUM = GNTTAB(GTYP,GIND)
C
      CALL INPNUM('Enter draw number for LOTTO 1 ',DRAW,1,9999,ST)
      IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
      FULSER=.FALSE.                                                            
      CALL INPYESNO('Do you want full serial numbers (Y/N) ',ST)
      IF(ST.EQ.1) THEN                                                        
        FULSER=.TRUE.                                                           
        CALL PASSWORD(5,PASENT)                                                 
        IF(PASPAS.NE.'ZQYAWE1C') THEN                                           
          TYPE*,' ********** ACCESS DENIED SORRY **********'                   
          TYPE*,' YOU MUST HAVE CORRECT PASSWORD FOR ACCESS'                    
          CALL GSTOP(GEXIT_SUCCESS)        
        ENDIF                                                                   
      ENDIF                                                                     
      CALL OPENW(3,GFNAMES(1,GNUM),4,0,0,ST)
      IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),1,ST,0)                
      CALL IOINIT(FDB,3,DLTSEC*256)     
      CALL READW(FDB,DRAW,DLTREC,ST)                                   
      IF(ST.NE.0) CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)                
      CALL CLOSEFIL(FDB)
C                                                                               
C OPEN VALIDATION FILE FOR SEQUENTIAL READ                                      
C                                                                               
20    CONTINUE                                                                  
      CALL IOPEN(SFNAMES(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
      IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),1,ST,0)
      CALL ITUBSIZE(VLF,TUBSIZ)                                                 
      COPY=0
C
C INITIAL IDENTIFICATION VARIABLES FOR EACH GAME                                
C
      
      REPLU = 7
      PAGE = 0
      REPNAM = 'L1EXTWIN.REP'              
      CALL ROPEN(REPNAM,REPLU,ST)                                
      IF(ST.NE.0) THEN                                                        
         TYPE *,IAM(),' Error opening ',REPNAM,' > ',ST
         CALL GSTOP(GEXIT_FATAL)
      ENDIF
      WRITE(REPHDR,8001) DRAW
C
C
      TYPE*,IAM(),'Reading VLF....'
C                                                                               
C     =========================  Main Loop  ==========================          
C
      TOTAMOUNT = 0
350   CONTINUE                                                                  
C                                                                               
C READ VALIDATION FILE                                                          
C                                                                               
      CALL ISREAD(V4BUF,VLF,VLFBUF,ST)                                          
      IF(ST.EQ.ERREND) THEN                                                     
         CALL ICLOSE(VLF,VLFBUF,ST)                                             
         GOTO 2000 
      ENDIF                                                                     
      IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),2,ST,0)                            
C                                                                               
C CHECK IF TICKET SHOULD BE PRINTED                                             
C                                                                               
      CALL LOGVAL(VALREC,V4BUF)                                                 
      IF(VALREC(VSTAT).EQ.VCXL.OR.VALREC(VSTAT).EQ.VDEL) GOTO 350
      IF(VALREC(VGTYP).NE.GTYP) GOTO 350
      IF(VALREC(VGIND).NE.GIND) GOTO 350               
C
C GET BET DETAILS.
C
      FRCS = VALREC(VFRAC)
      CALL FASTSET(0,SHARES,LTGDIV)
      AMOUNT = 0
      CALL DLOGVAL(VALREC,VDETAIL)
      WINNER = .FALSE.
      DO 400 PRZIND=1,VALREC(VPZOFF)
	IF(VDETAIL(VDRW,PRZIND).NE.DRAW) GOTO 400
	IF(IAND(VDETAIL(VSUB,PRZIND), 1).EQ.0) GOTO 400
	WINNER = .TRUE.
	DIV = VDETAIL(VDIV,PRZIND)
	IF(DLTBDR.NE.0) THEN
	    DIND = VDETAIL(VBDR,PRZIND)+1
	ELSE
	    DIND = 1
	ENDIF 
	SHARES(DIV) =  SHARES(DIV) + 1
	SHR = VDETAIL(VSHR,PRZIND)
	IF(FRCS.EQ.0.OR.FRCS.EQ.10) THEN  
	    AMOUNT = AMOUNT + DLTSHV(DIV,DIND)*SHR
	ELSE
	    AMOUNT = AMOUNT + (DLTSHV(DIV,DIND)/10)*SHR*FRCS
	ENDIF
	TOTAMOUNT = TOTAMOUNT + AMOUNT
400   CONTINUE	
C             
      IF(.NOT.WINNER) GOTO 350
C                                                                               
      CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)                        
C                                                
      IF(LINCNT.GT.55) THEN                                               
        LINCNT=7 
        CALL TITLE(REPHDR,REPNAM,1,REPLU,PAGE,DAYCDC)
        IF(DLTBDR.NE.0) THEN
          WRITE(REPLU,90014) (' DIV',K,K=1,DLTDIV)                  
        ELSE
          WRITE(REPLU,90011) (' DIV',K,K=1,DLTDIV)                  
        ENDIF                                                                   
        WRITE(REPLU,9000)                                                 
      ENDIF                                                                     
C                                                                               
      DATE(VCDC) = VALREC(VSCDC)                                                
      CALL LCDATE(DATE)                                                          
      LDIV=DLTDIV                                                         
      IF(GTYP.EQ.TLTO .AND. DLTBDR.NE.0) LDIV=LDIV+1
      IF(LDIV.GT.7) LDIV=7            
      JOKNAM='   '
                                   
      IF(FULSER) THEN
         WRITE(REPLU,9002) AGTTAB(AGTNUM,VALREC(VSTER))/10,                  
     *                       MOD(AGTTAB(AGTNUM,VALREC(VSTER)),10),              
     *                       VALREC(VSTER),                                     
     *                       DATE(VJUL),SSER,SCHK,                              
     *                       VALREC(VSCDC),VALST(VALREC(VSTAT)),                
     *                       VALREC(VBNKID),VALREC(VBNKNUM)/10,
     *                       MOD(VALREC(VBNKNUM),10),
     *                       DRAW,VALREC(VEXP),                          
     *                       CMONY(AMOUNT,11,VALUNIT),
     *                       JOKNAM,VALREC(VFRAC),                              
     *                       (SHARES(DIV),DIV=1,LDIV)                        
      ELSE
         WRITE(REPLU,9006) AGTTAB(AGTNUM,VALREC(VSTER))/10,                  
     *                       MOD(AGTTAB(AGTNUM,VALREC(VSTER)),10),              
     *                       VALREC(VSTER),                                     
     *                       DATE(VJUL),SSER,                              
     *                       VALREC(VSCDC),VALST(VALREC(VSTAT)),                
     *                       VALREC(VBNKID),VALREC(VBNKNUM)/10,
     *                       MOD(VALREC(VBNKNUM),10),
     *                       DRAW,VALREC(VEXP),                          
     *                       CMONY(AMOUNT,11,VALUNIT),
     *                       JOKNAM,VALREC(VFRAC),                              
     *                       (SHARES(DIV),DIV=1,LDIV)                        
      ENDIF
C                                                                               
       LINCNT=LINCNT+1
       GOTO 350
C                                               
2000	CONTINUE          
C                                                                               
C SPOOL THE REPORT
C                                                                               
	WRITE(REPLU,9003) GTNAMES(GTYP),GIND, CMONY(TOTAMOUNT,13,VALUNIT)
	CLOSE(REPLU)
	IF(COPY.NE.0) CALL SPOOL(REPNAM,COPY,STATUS)
	CALL GSTOP(GEXIT_SUCCESS)
C                                                                               
C     ==================== Format Statements ======================             
C                                                                               

C---- Page Header format statements.

8001  FORMAT('L1EXTWIN  EXTRA WINNERS REPORT FOR DRAW ',I4)

9000  FORMAT(1X,131('='),/)                                                     

9003  FORMAT(//,' Total ',A8,I1,' Won ',A13)                                  

C---- Game Column headings format statements.


90011 FORMAT(1X,'SELLING  SELLING',20X,' CDC TICKET  BANK     BANK',        
     *       8X,' DRAW',14X,'JOKER',/,2X, 
     *       'AGENT    TERM',5X,'TICKET SERIAL',                                
     *       3X,' SOLD STATUS   ID     ACCOUNT',5X,' WON/EXP',
     *       5X,'TOT WON 1/2',' FR ',7(A4,I1))     

90014 FORMAT(1X,'SELLING  SELLING',20X,' CDC TICKET  BANK     BANK',    
     *       8X,' DRAW',14X,'JOKER',/,2X,
     *       'AGENT    TERM',5X,'TICKET SERIAL',                                
     *       3X,' SOLD STATUS   ID     ACCOUNT',5X,' WON/EXP',
     *       5X,'TOT WON 1/2',' FR ',<DLTDIV>(A4,I1),' BONUS')     

C---- Data format Statements.

9002  FORMAT(1X,I6.6,'-',I1.1,2X,I5,3X,I3.3,'-',I8.8,'-',I3.3,                  
     *       1X,I5,2X,A4,2X,I8.8,1X,I7.7,'-',I1,1X,I5,                          
     *       '/',I5,A11,A3,1X,I2,1X,7I5)  

9006  FORMAT(1X,I6.6,'-',I1.1,2X,I5,3X,I3.3,'-',I8.8,'-','***',
     *       1X,I5,2X,A4,2X,I8.8,1X,I7.7,'-',I1,1X,I5,                          
     *       '/',I5,A11,A3,1X,I2,1X,7I5)  

C                                                                               
      END                                                                       
