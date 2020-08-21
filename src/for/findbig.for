C FINDBIG.FOR
C
C V32 29-NOV-2000 UXN Totogola added.
C V31 03-FEB-2000 RXK Display all Maailman Ympari refund divisions in 1 column.
C V30 13-JAN-2000 RXK Def-file for Bingo division names added
C V29 02-DEC-1999 OXK Oddset games selected either by draw# or winsel cdc
C V28 13-OCT-1999 RXK World Tour added.
C V27 14-MAY-1999 UXN Super Triple added.
C V26 09-SEP-1998 RXK Changed for new Kicker
C V25 23-JAN-1998 UXN Super Score and Todays Triple added.
C V24 02-OCT-1997 UXN CHANGES FOR BINGO LOTTO
C V23 18-DEC-1996 HXK Update from TEBE project (MXP,WXW,PXN,MJF)
C V22 28-NOV-1996 WXW Telebetting startup, changes MP/PXN/WXW.
C                     Format fixes.
C V21 18-JAN-1996 RXK Today's Couple and Super Double added 
C V20 04-SEP-1995 RXK Report names for Ravi chamged, bonus draw for Viking added
C V19 24-AUG-1995 PXB RFSS 206: Vakio report now begins with V.
C V18 10-AUG-1995 PXB RFSS 205: Names of vakio report.
C V17 04-AUG-1995 HXK Batch of fixes for Ravi V5 installation
C V16 05-DEC-1994 HXK Merging from 25-Nov -> 5 Dec
C V15 30-NOV-1994 PXB Added Bingo game report.
C V14 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V13 27-APR-1994 JXP COPY=0
C V12 24-JAN-1994 HXK ONLY PRINT REQUESTED DRAW FOR ODDSET GAMES.
C V11 24-NOV-1993 SXH REPLACE INPNUM WITH PRMNUM
C V10 18-NOV-1993 SXH Fixed bug where first PITKA amount over requested 
C                     amount is shown and then all amounts
C V09 17-NOV-1993 SXH Added division winners count
C V08 08-OCT-1993 HXK FIXED SOME STUFF.
C V07 05-OCT-1993 GXA Do not allow for winners with zero amount, 
C                     fixed check digit display of ***.
C V06 28-SEP-1993 HXK Initial revision.
C V05 27-AUG-1992 HJK  FIX FOR BOTH JOKERIS WINNING ON SAME TICKET
C V04 21-NOV-1991 STC  MODIFIED FOR DOUBLE JOKERI (Print which Jokeri)
C V03 22-FEB-1991 GCAN FIX FOR REPORT NAMES (1st Letter from GDF name)
C V02 04-FEB-1991 MTK CHANGED FOR KENO GAME
C V01 04-NOV-1989 MGM INITIAL RELEASE FOR FINLAND
C                                                                               
C SUBROUTINE TO GENERATE REPORTS ON BIG PRIZE WINNERS                           
C                                                                               
C INPUT VARIABLES:                                                              
C                                                                               
C GAMES(MAXGAM) = TABLE CONTAINING GAME NUMBERS TO REPORT ON                    
C DRAWS(MAXGAM) = DRAW NUMBER TO REPORT ON FOR EACH GAME (0=ALL)                
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
      SUBROUTINE FINDBIG(DRAWS,RAMOUNT,FULSER,DIVNUM,DIVOPT,USECDC)
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
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:TNAMES.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'
      INCLUDE 'INCLIB:GTNAMES.DEF'
      INCLUDE 'INCLIB:DLTREC.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
      INCLUDE 'INCLIB:DTGREC.DEF'
      INCLUDE 'INCLIB:DKKREC.DEF'
      INCLUDE 'INCLIB:LKKREC.DEF'
      INCLUDE 'INCLIB:LSPREC.DEF'
      INCLUDE 'INCLIB:LTGREC.DEF'
      INCLUDE 'INCLIB:LLTREC.DEF'

      INCLUDE 'INCLIB:VDETAIL.DEF'

      INCLUDE 'INCLIB:DBNREC.DEF'
      INCLUDE 'INCLIB:LBNREC.DEF'
      INCLUDE 'INCLIB:BNGDNAM.DEF'
      INCLUDE 'INCLIB:REPPRF.DEF'

      INTEGER*4    TUBSIZ
      PARAMETER   (TUBSIZ=I4BUCSIZ*7)

      INTEGER*4    KIND,I,GNUM,GTYP,GIND,ST,COPY,DRAW
      INTEGER*4    KNUM,GAMOUNT,KAMOUNT,DIVOFF,DIVNUM,RAMOUNT,SSER
      INTEGER*4    SCHK,K,PDIV,DIV,JOK,STATUS
      INTEGER*4    LDIV
      INTEGER*4    MAXDIV
      PARAMETER(MAXDIV=6)  ! MAX DIV. PER ONE LINE
      INTEGER*4    TIMES

      INTEGER*4    VLFBUF(TUBSIZ), FDB(7)                        
      INTEGER*4    GDIVS(20),KDIVS(20,2)                                        
      INTEGER*4    LINCNT(MAXGAM), NUMDIV(MAXGAM)                               
      INTEGER*4    GAMACT(0:MAXGAM)                                             
      INTEGER*4    REPLU(MAXGAM),  DRAWS(MAXGAM), PAGE(MAXGAM)                  
      INTEGER*2    DATE(LDATE_LEN) /LDATE_LEN*0/                                              
      INTEGER*4    GRDWON(MAXGAM)                                               

      INTEGER*4    DIV1,DIV2,DIV3,DIV4    ! PITKA DIVISION TOTAL BY RECORD
      INTEGER*4    TDIV1/0/               ! PITKA TOTAL DIVISION 1
      INTEGER*4    TDIV2/0/               ! PITKA TOTAL DIVISION 2
      INTEGER*4    TDIV3/0/               ! PITKA TOTAL DIVISION 3
      INTEGER*4    TDIV4/0/               ! PITKA TOTAL DIVISION 4
      INTEGER*4    JJ

      CHARACTER    REPHDR(MAXGAM)*51                                            
      CHARACTER    REPNAM(MAXGAM)*13                                            
      CHARACTER*3  JOKNAM
      CHARACTER*132 TEMPLINE

      CHARACTER * 8 IAGT_NO                ! FUNCTION TO FORMAT AGENT NUMBER

      LOGICAL FIRST_WRT(MAXGAM)
      LOGICAL USECDC                                                       
      LOGICAL FULSER,KPRINT,GPRINT,DIVOPT,KPRINT2,FIRST
      EQUIVALENCE (DKKREC,DSPREC,DLTREC,DBNREC,DTGREC)
      COMMON /SCF/ SCFREC
      COMMON /PRIZE/ LLTREC,LSPREC,LKKREC,LBNREC,LTGREC
C                                                                               
      DATA         GAMACT/MAXGAM*0,0/                                           
      DATA         PAGE/MAXGAM*0/                                               
C
      INTEGER*4 KGNUM,KDRAW
C       
      KGNUM = 0
      KDRAW = 0
C
      DO I=1,MAXGAM
        FIRST_WRT(I) = .TRUE.
        IF(DRAWS(I).NE.0.AND.SCFKGN(I).NE.0) THEN
            KDRAW = DRAWS(I)
            KGNUM = SCFKGN(I)
            DRAWS(KGNUM) = DRAWS(I)
        ENDIF
      ENDDO
      IF(KGNUM.NE.0) THEN
         GIND = 1
         CALL OPENW(3,SCFGFN(1,KGNUM),4,0,0,ST)
         IF(ST.NE.0) CALL FILERR(SCFGFN(1,KGNUM),1,ST,0)
         CALL IOINIT(FDB,3,DKKSEC*256)     
         CALL READW(FDB,KDRAW,DKKREC,ST)
         IF(ST.NE.0) CALL FILERR(SCFGFN(1,KGNUM),2,ST,KDRAW)
         CALL LOGGAM(TKIK,GIND,DKKREC,LKKREC)
         NUMDIV(KGNUM)=LKKDIV(GIND)                             
         CLOSE(UNIT=3)
      ENDIF                                                                     
C                                                                               
C READ GAME RECORDS FOR REQUESTED DRAWS                                         
C                                                                               
      KIND=0                                                                    
      DO 10 GNUM=1,MAXGAM                                                          
      IF(DRAWS(GNUM).EQ.0) GOTO  10
      GAMACT(GNUM)=1
      GTYP=SCFGNT(GAMTYP,GNUM)                                                  
      GIND=SCFGNT(GAMIDX,GNUM)                                                  
      CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)                                     
      IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)                            
C                                                                               
C                                                                               
      IF(GTYP.EQ.TLTO) THEN                                                     
        CALL IOINIT(FDB,3,DLTSEC*256)     
        CALL READW(FDB,DRAWS(GNUM),DLTREC,ST)                                   
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAWS(GNUM))                
        CALL LOGGAM(GTYP,GIND,DLTREC,LLTREC)                                    
      ENDIF                                                                     
C                                                                               
      IF(GTYP.EQ.TKIK) THEN                                                     
        CALL IOINIT(FDB,3,DKKSEC*256)     
        CALL READW(FDB,DRAWS(GNUM),DKKREC,ST)                                   
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAWS(GNUM))                
        CALL LOGGAM(GTYP,GIND,DKKREC,LKKREC)                                    
      ENDIF                                                                     
C                                                                               
      IF(GTYP.EQ.TSPT) THEN                                                     
        CALL IOINIT(FDB,3,DSPSEC*256)     
        CALL READW(FDB,DRAWS(GNUM),DSPREC,ST)                                   
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAWS(GNUM))                
        CALL LOGGAM(GTYP,GIND,DSPREC,LSPREC)                                    
      ENDIF                                                                     
C                                                                               
      IF(GTYP.EQ.TTGL) THEN                                                     
        CALL IOINIT(FDB,3,DTGSEC*256)     
        CALL READW(FDB,DRAWS(GNUM),DTGREC,ST)                                   
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAWS(GNUM))                
        CALL LOGGAM(GTYP,GIND,DTGREC,LSPREC)                                    
      ENDIF                                                                     
C                                                                               
      IF (GTYP .EQ. TBNG) THEN
        CALL IOINIT (FDB,3,DBNSEC*256)
        CALL READW (FDB,DRAWS(GNUM),DBNREC,ST)
        IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAWS(GNUM))
        CALL LOGGAM (GTYP,GIND,DBNREC,LBNREC)
      END IF

      CLOSE(UNIT=3)                                                           
10    CONTINUE
C                                                                               
C OPEN VALIDATION FILE FOR SEQUENTIAL READ                                      
C                                                                               
20    CONTINUE                                                                  
      CALL IOPEN(SCFSFN(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)                
      IF(ST.NE.0) CALL FILERR(SCFSFN(1,VLF),1,ST,0)                             
      CALL ITUBSIZE(VLF,TUBSIZ)                                                 
C                                                                               
C      CALL PRMNUM('Enter number of report copies            ',                  
C     *             COPY,0,20,ST)
C      IF(ST.NE.0) STOP
      COPY=0
C
C INITIAL IDENTIFICATION VARIABLES FOR EACH GAME                                
C
      CALL FASTSET(0,GRDWON,MAXGAM)                                             
      DO 250 GNUM=1,MAXGAM                         
         IF ((DRAWS(GNUM).LE.0).AND.(.NOT.USECDC))  GOTO 250
         REPLU(GNUM)  = 11 + GNUM                                              
         LINCNT(GNUM) = 70                                                      
         GTYP = SCFGNT(GAMTYP,GNUM)                                             
         IF ((GTYP.LE.0).OR.(GTYP.GT.MAXTYP)) GOTO 250
         GIND = SCFGNT(GAMIDX,GNUM)                                             
         IF ((GIND.LE.0).OR.(GIND.GT.MAXIND)) GOTO 250
         DRAW = DRAWS(GNUM)                                                     
         WRITE(REPHDR(GNUM),8001) GTNAMES(GTYP),GIND,ITOC(DRAW,K)              
         WRITE(REPNAM(GNUM),8002) PREFIX(GTYP),GIND
         IF(GTYP.EQ.TLTO) NUMDIV(GNUM)=LLTDIV(GIND)
         IF(GTYP.EQ.TKIK) NUMDIV(GNUM)=LKKDIV(GIND)                             
         IF(GTYP.EQ.TSPT) NUMDIV(GNUM)=LSPDIV(GIND)                             
         IF(GTYP.EQ.TTGL) NUMDIV(GNUM)=LTTDIV(GIND)
         IF(NUMDIV(GNUM).LE.0) NUMDIV(GNUM) = 7                                 
         IF(NUMDIV(GNUM).GT.14) NUMDIV(GNUM)=14
         CALL ROPEN(REPNAM(GNUM),REPLU(GNUM),ST)
         IF(ST.NE.0)THEN
              TYPE *,IAM(),' Error opening ',REPNAM(GNUM),' > ',ST
              CALL GPAUSE
         ENDIF
250   CONTINUE                                                                  
C                                                                               
C     =========================  Main Loop  ==========================          
C                                                                               
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
      GNUM=VALREC(VGAM)                                                         
      GIND=VALREC(VGIND)
      KNUM=VALREC(VKGME)

      IF(VALREC(VGTYP).NE.TLTO.AND.
     *   VALREC(VGTYP).NE.TSPT.AND.
     *   VALREC(VGTYP).NE.TTGL.AND.
     *   VALREC(VGTYP).NE.TKIK.AND.
     *   VALREC(VGTYP).NE.TBNG.AND.
     *   USECDC) GOTO 1000

      IF(GAMACT(GNUM).EQ.0.AND.GAMACT(KNUM).EQ.0) GOTO 350                      

      IF(VALREC(VGTYP).NE.TLTO.AND.
     *   VALREC(VGTYP).NE.TSPT.AND.
     *   VALREC(VGTYP).NE.TTGL.AND.
     *   VALREC(VGTYP).NE.TKIK.AND.
     *   VALREC(VGTYP).NE.TBNG) GOTO 1000

C                                                                               
C GET PRIZE AMOUNT FOR DRAW REQUESTED FOR NON ODDSET GAMES
C                                                                               
      KPRINT=.FALSE.                                                            
      KPRINT2=.FALSE.                                                           
      GPRINT=.FALSE.                                                            
      CALL BIGPRIZE(VALREC,GAMOUNT,KAMOUNT,GDIVS,KDIVS)
      IF(DIVOPT) THEN          
        DO 305 DIVOFF=1,DIVNUM                                                  
        IF(GDIVS(DIVOFF).NE.0) GPRINT=.TRUE.
        IF(GTYP.EQ.TLTO .AND. GDIVS(LLTDIV(GIND)+1).NE.0) GPRINT=.TRUE.
        IF(KDIVS(DIVOFF,1).NE.0) KPRINT=.TRUE.
        IF(KDIVS(DIVOFF,2).NE.0) KPRINT2=.TRUE.
305     CONTINUE                                                                
      ELSE                                                                      
        IF(GAMOUNT.GE.RAMOUNT.AND.GAMOUNT.GT.0) GPRINT=.TRUE.                   
        IF(KAMOUNT.GE.RAMOUNT.AND.KAMOUNT.NE.0) THEN
          DO 306 DIVOFF=1,NUMDIV(KNUM)                                          
          IF(KDIVS(DIVOFF,1).NE.0) KPRINT=.TRUE.                                
          IF(KDIVS(DIVOFF,2).NE.0) KPRINT2=.TRUE.                               
306       CONTINUE                                                              
        ENDIF                                                                   
      ENDIF                                                                     
C                        
      IF(.NOT.(KPRINT.OR.GPRINT.OR.KPRINT2)) GOTO 350                 
C                                                                               
      CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)                        
C                                                
C                         
      IF (VALREC(VGTYP) .EQ. TBNG) GOTO 360


      IF(.NOT.GPRINT.OR.GAMACT(GNUM).EQ.0.AND.
     *  (.NOT.(KPRINT.OR.KPRINT2).OR.GAMACT(KNUM).EQ.0)) GOTO 310

      IF(LINCNT(GNUM).GT.55) THEN                                               
        LINCNT(GNUM)=7                                                          
        CALL TITLE(REPHDR(GNUM),REPNAM(GNUM),1,                                 
     *                 REPLU(GNUM),PAGE(GNUM),DAYCDC)                           
        IF(NUMDIV(GNUM).GT.MAXDIV) THEN                
          IF (GTYP.EQ.TBNG) THEN
            WRITE(REPLU(GNUM),90013)
          ELSE
            WRITE(REPLU(GNUM),9001)
          END IF
        ELSE IF(GTYP.EQ.TLTO .AND. LLTBDR(GIND).NE.0) THEN                                                                   
          WRITE(REPLU(GNUM),90014) (' DIV',K,K=1,NUMDIV(GNUM))                  
        ELSE
          TIMES = NUMDIV(GNUM)
          WRITE(REPLU(GNUM),90011) (K,K=1,TIMES)                  
        ENDIF                                                                   
        WRITE(REPLU(GNUM),9000)                                                 
      ENDIF                                                                     
C                                                                               
      DATE(VCDC) = VALREC(VSCDC)                                                
      CALL LCDATE(DATE)     
      PDIV=NUMDIV(GNUM)                                                         
      LDIV=NUMDIV(GNUM)                                                         
      IF(GTYP.EQ.TLTO .AND. LLTBDR(GIND).NE.0) LDIV=LDIV+1
      IF(PDIV.GT.MAXDIV) PDIV=MAXDIV            
      IF(LDIV.GT.MAXDIV) LDIV=MAXDIV            
      JOKNAM='   '
      IF(KPRINT)  JOKNAM(1:1) = '1'
      IF(KPRINT2) JOKNAM(2:2) = '2'
      IF(KPRINT.OR.KPRINT2) JOKNAM(3:3) = '*'
                                   
      IF(FULSER) THEN
         WRITE(REPLU(GNUM),9002) 
     *                       IAGT_NO(AGTTAB(AGTNUM, VALREC(VSTER))),
     *                       VALREC(VSTER),VALREC(VSSER),
     *                       DATE(VJUL),SSER,SCHK,                              
     *                       VALREC(VSCDC),VALST(VALREC(VSTAT)),                
     *                       VALREC(VBNKID),VALREC(VBNKNUM)/10,
     *                       MOD(VALREC(VBNKNUM),10),
     *                       DRAWS(GNUM),VALREC(VEXP),                          
     *                       CMONY(GAMOUNT,11,VALUNIT),
     *                       JOKNAM,VALREC(VFRAC),                              
     *                       (GDIVS(DIV),DIV=1,LDIV)                        
      ELSE
         WRITE(REPLU(GNUM),9006) 
     *                       IAGT_NO(AGTTAB(AGTNUM, VALREC(VSTER))),
     *                       VALREC(VSTER),VALREC(VSSER),
     *                       DATE(VJUL),SSER,                              
     *                       VALREC(VSCDC),VALST(VALREC(VSTAT)),                
     *                       VALREC(VBNKID),VALREC(VBNKNUM)/10,
     *                       MOD(VALREC(VBNKNUM),10),
     *                       DRAWS(GNUM),VALREC(VEXP),                          
     *                       CMONY(GAMOUNT,11,VALUNIT),
     *                       JOKNAM,VALREC(VFRAC),                              
     *                       (GDIVS(DIV),DIV=1,LDIV)                        
      ENDIF
      IF(NUMDIV(GNUM).NE.PDIV) THEN                                             
        WRITE(REPLU(GNUM),9004) (GDIVS(DIV),DIV=PDIV+1,NUMDIV(GNUM))
        LINCNT(GNUM)=LINCNT(GNUM)+2                                             
      ENDIF                                                                     
C                                                                               
      LINCNT(GNUM)=LINCNT(GNUM)+1                                               
      GRDWON(GNUM)=GRDWON(GNUM)+GAMOUNT                                         
C                                                                               
C KICKER WINNERS
C                                                                               
310   CONTINUE
      IF(.NOT.KPRINT.AND..NOT.KPRINT2.OR.GAMACT(KNUM).EQ.0) GOTO 350
      IF(LINCNT(KNUM).GT.55) THEN                                               
        LINCNT(KNUM)=7                                                          
        CALL TITLE(REPHDR(KNUM),REPNAM(KNUM),1,                                 
     *                 REPLU(KNUM),PAGE(KNUM),DAYCDC)                           
        IF(NUMDIV(KNUM).GT.MAXDIV) THEN
          WRITE(REPLU(KNUM),9001)                                               
        ELSE
          TIMES = NUMDIV(KNUM)
          WRITE(REPLU(KNUM),90011) (K,K=1,TIMES)                  
        ENDIF                                                                   
        WRITE(REPLU(KNUM),9000)                                                 
      ENDIF                                                                     
C                                                                               
      DATE(VCDC) = VALREC(VSCDC)                                                
      CALL LCDATE(DATE)
      PDIV=NUMDIV(KNUM)                                                         
      IF(PDIV.GT.MAXDIV) PDIV=MAXDIV
      JOK=1                                                                     
      IF(.NOT.KPRINT) JOK=2

      JOKNAM='   '
      IF(KPRINT)  JOKNAM(1:1) = '1'
      IF(.NOT.KPRINT.AND.KPRINT2) JOKNAM(2:2) = '2'
      JOKNAM(3:3) = '*'

      IF(FULSER) THEN
         WRITE(REPLU(KNUM),9002) 
     *                       IAGT_NO(AGTTAB(AGTNUM, VALREC(VSTER))),
     *                       VALREC(VSTER),VALREC(VSSER),
     *                       DATE(VJUL),SSER,SCHK,                              
     *                       VALREC(VSCDC),VALST(VALREC(VSTAT)),                
     *                       VALREC(VBNKID),VALREC(VBNKNUM)/10,      
     *                       MOD(VALREC(VBNKNUM),10),
     *                       DRAWS(KNUM),VALREC(VKEXP),                         
     *                       CMONY(KAMOUNT,11,VALUNIT),
     *                       JOKNAM,VALREC(VFRAC),
     *                       (KDIVS(DIV,JOK),DIV=1,PDIV)                        
      ELSE
         WRITE(REPLU(KNUM),9006) 
     *                       IAGT_NO(AGTTAB(AGTNUM, VALREC(VSTER))),
     *                       VALREC(VSTER),VALREC(VSSER),
     *                       DATE(VJUL),SSER,                              
     *                       VALREC(VSCDC),VALST(VALREC(VSTAT)),                
     *                       VALREC(VBNKID),VALREC(VBNKNUM)/10,      
     *                       MOD(VALREC(VBNKNUM),10),
     *                       DRAWS(KNUM),VALREC(VKEXP),                         
     *                       CMONY(KAMOUNT,11,VALUNIT),
     *                       JOKNAM,VALREC(VFRAC),
     *                       (KDIVS(DIV,JOK),DIV=1,PDIV)
      ENDIF                        
      IF(NUMDIV(KNUM).NE.PDIV) THEN                                             
        WRITE(REPLU(KNUM),9004) (KDIVS(DIV,JOK),DIV=PDIV+1,NUMDIV(KNUM))
        LINCNT(KNUM)=LINCNT(KNUM)+1                                             
      ENDIF                                                                     
      IF(JOK.EQ.1.AND.KPRINT2) THEN                                             
        JOKNAM='   '
        IF(KPRINT2) JOKNAM(2:2) = '2'
        JOKNAM(3:3) = '*'
        WRITE(REPLU(KNUM),9005) JOKNAM,(KDIVS(DIV,2),DIV=1,PDIV)                
        LINCNT(KNUM)=LINCNT(KNUM)+1                                             
        IF(NUMDIV(KNUM).NE.PDIV) THEN                                           
          WRITE(REPLU(KNUM),9004) (KDIVS(DIV,2),DIV=PDIV+1,NUMDIV(KNUM))     
          LINCNT(KNUM)=LINCNT(KNUM)+1                                           
        ENDIF                                                                   
      ENDIF                                                                     
C                                                                               
      LINCNT(KNUM)=LINCNT(KNUM)+1                                               
      GRDWON(KNUM)=GRDWON(KNUM)+KAMOUNT                                         
      GOTO 350                                                                  

360   CONTINUE
C
C BINGO LOTTO GAME
C

      IF (LINCNT(GNUM) .GT. 55) THEN
        LINCNT(GNUM) = 7
        CALL TITLE (REPHDR(GNUM),REPNAM(GNUM),1,REPLU(GNUM),PAGE(GNUM),DAYCDC)
        WRITE(REPLU(GNUM),90013)
        WRITE(REPLU(GNUM),9000)
      END IF

      DATE(VCDC) = VALREC(VSCDC)

      CALL LCDATE (DATE)

      PDIV = NUMDIV(GNUM)

      JOKNAM = '   '
      TEMPLINE = ' '                             
      IF (FULSER) THEN
         WRITE(TEMPLINE,90023) 
     *                       IAGT_NO(AGTTAB(AGTNUM, VALREC(VSTER))),
     *                       VALREC(VSTER),VALREC(VSSER),
     *                       DATE(VJUL),SSER,SCHK,
     *                       VALREC(VSCDC),VALST(VALREC(VSTAT)),
     *                       VALREC(VBNKID),VALREC(VBNKNUM)/10,
     *                       MOD(VALREC(VBNKNUM),10),
     *                       DRAWS(GNUM),VALREC(VEXP),
     *                       CMONY(GAMOUNT,11,VALUNIT),
     *                       JOKNAM,VALREC(VFRAC)
         FIRST = .TRUE.
         DO DIV=1,BGODIV
           IF(GDIVS(DIV).NE.0) THEN
             IF(FIRST) THEN
               FIRST = .FALSE.
               WRITE(TEMPLINE(100:),9021) GDIVS(DIV),BNGDNAMES(DBNDNR(DIV)),DIV
               WRITE(REPLU(GNUM),'(A132)') TEMPLINE
               LINCNT(GNUM) = LINCNT(GNUM) + 1
             ELSE
               WRITE(REPLU(GNUM),9022) GDIVS(DIV),BNGDNAMES(DBNDNR(DIV)),DIV
               LINCNT(GNUM) = LINCNT(GNUM) + 1
             ENDIF
           ENDIF
        ENDDO    
      ELSE
         WRITE(TEMPLINE,90025) 
     *                       IAGT_NO(AGTTAB(AGTNUM, VALREC(VSTER))),
     *                       VALREC(VSTER),VALREC(VSSER),
     *                       DATE(VJUL),SSER,
     *                       VALREC(VSCDC),VALST(VALREC(VSTAT)),
     *                       VALREC(VBNKID),VALREC(VBNKNUM)/10,
     *                       MOD(VALREC(VBNKNUM),10),
     *                       DRAWS(GNUM),VALREC(VEXP),
     *                       CMONY(GAMOUNT,11,VALUNIT),
     *                       JOKNAM,VALREC(VFRAC)
         FIRST = .TRUE.
         DO DIV=1,BGODIV
           IF(GDIVS(DIV).NE.0) THEN
             IF(FIRST) THEN
               FIRST = .FALSE.
               WRITE(TEMPLINE(100:),9021) GDIVS(DIV),BNGDNAMES(DBNDNR(DIV)),DIV
               WRITE(REPLU(GNUM),'(A132)') TEMPLINE
               LINCNT(GNUM) = LINCNT(GNUM) + 1
             ELSE
               WRITE(REPLU(GNUM),9022) GDIVS(DIV),BNGDNAMES(DBNDNR(DIV)),DIV
               LINCNT(GNUM) = LINCNT(GNUM) + 1
             ENDIF
           ENDIF
        ENDDO                        
      END IF

      GRDWON(GNUM) = GRDWON(GNUM) + GAMOUNT

      GOTO 350
 
C
C GENERATE ODDSET GAME REPORTS
C
1000  CONTINUE

      GPRINT = .FALSE.
      IF((VALREC(VPAMT)+VALREC(VRAMT)).GE.RAMOUNT) GPRINT=.TRUE.
      IF (USECDC) THEN
         IF (VALREC(VWCDC).NE.DAYCDC) GPRINT=.FALSE.
      ELSE
         IF (VALREC(VEXP).NE.DRAWS(GNUM)) GPRINT=.FALSE.
      ENDIF

      DRAW=VALREC(VEXP)

      CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)
C
C
      IF(.NOT.GPRINT.OR. ((GAMACT(GNUM).EQ.0).AND.(.NOT.USECDC))) GOTO 350
      IF (FIRST_WRT(GNUM)) THEN
         WRITE(REPHDR(GNUM),8001) GTNAMES(GTYP),GIND,ITOC(DRAW,K)              
         FIRST_WRT(GNUM)=.FALSE.
      ENDIF
      IF(LINCNT(GNUM).GT.55) THEN
        LINCNT(GNUM)=7
        CALL TITLE(REPHDR(GNUM),REPNAM(GNUM),1,
     *                 REPLU(GNUM),PAGE(GNUM),DAYCDC)
        WRITE(REPLU(GNUM),90012)
        WRITE(REPLU(GNUM),9000)
      ENDIF
C
      DATE(VCDC) = VALREC(VSCDC)
      CALL LCDATE(DATE)

      ! TOTAL DIVISION WINS BY RECORD AND GRAND TOTAL
      CALL DLOGVAL(VALREC,VDETAIL)
      DIV1 = 0
      DIV2 = 0
      DIV3 = 0
      DIV4 = 0
      DO JJ = 1, VALREC(VPZOFF)
          IF (VDETAIL(VDIV,JJ) .EQ. 1) THEN
              DIV1 = DIV1 + 1
          ELSE IF (VDETAIL(VDIV,JJ) .EQ. 2) THEN
              DIV2 = DIV2 + 1
          ELSE IF (VDETAIL(VDIV,JJ) .EQ. 3) THEN
              DIV3 = DIV3 + 1
          ELSE IF (VDETAIL(VDIV,JJ) .EQ. 4) THEN
              DIV4 = DIV4 + 1
          END IF
      END DO
      TDIV1 = TDIV1 + DIV1
      TDIV2 = TDIV2 + DIV2
      TDIV3 = TDIV3 + DIV3
      TDIV4 = TDIV4 + DIV4


      IF(FULSER) THEN
         WRITE(REPLU(GNUM),90021) 
     *                       IAGT_NO(AGTTAB(AGTNUM, VALREC(VSTER))),
     *                       VALREC(VSTER),VALREC(VSSER),
     *                       DATE(VJUL),SSER,SCHK,
     *                       VALREC(VSCDC),VALST(VALREC(VSTAT)),
     *                       VALREC(VBNKID),VALREC(VBNKNUM)/10,
     *                       MOD(VALREC(VBNKNUM),10),
     *                       VALREC(VEXP),VALREC(VEXP),
     *                       CMONY(VALREC(VPAMT),11,VALUNIT),
     *                       CMONY(VALREC(VRAMT),8,BETUNIT),
     *                       VALREC(VFRAC),DIV1,DIV2,DIV3,DIV4
      ELSE
         WRITE(REPLU(GNUM),90022) 
     *                       IAGT_NO(AGTTAB(AGTNUM, VALREC(VSTER))),
     *                       VALREC(VSTER),VALREC(VSSER),
     *                       DATE(VJUL),SSER,
     *                       VALREC(VSCDC),VALST(VALREC(VSTAT)),
     *                       VALREC(VBNKID),VALREC(VBNKNUM)/10,
     *                       MOD(VALREC(VBNKNUM),10),
     *                       VALREC(VEXP),VALREC(VEXP),
     *                       CMONY(VALREC(VPAMT),11,VALUNIT),
     *                       CMONY(VALREC(VRAMT),8,BETUNIT),
     *                       VALREC(VFRAC),DIV1,DIV2,DIV3,DIV4
      ENDIF
C
      LINCNT(GNUM)=LINCNT(GNUM)+1
      GRDWON(GNUM)=GRDWON(GNUM)+VALREC(VPAMT)+VALREC(VRAMT)
      GOTO 350
C                                                                               
C                                                                               
2000   CONTINUE          
C                                                                               
C SPOOL ALL REPORTS TO THE PRINTER UNLESS NO BIG WINNERS FOUND                  
C                                                                               
      DO 3000 GNUM=1,MAXGAM      
      IF ((DRAWS(GNUM).EQ.0).AND.(.NOT.USECDC)) GOTO 3000
      GTYP=SCFGNT(GAMTYP,GNUM)                                                  
      IF ((GTYP.LE.0).OR.(GTYP.GT.MAXTYP)) GOTO 3000
      GIND=SCFGNT(GAMIDX,GNUM)
      IF ((GIND.LE.0).OR.(GIND.GT.MAXIND)) GOTO 3000
      WRITE(REPLU(GNUM),9003) GTNAMES(GTYP),GIND,                               
     *                        CMONY(GRDWON(GNUM),13,VALUNIT)           

      ! WRITE TOTO SELECT TOTAL NUMBER OF DIVISION WINNERS
      IF (GTYP .EQ. TTSL) THEN
          WRITE(REPLU(GNUM),9020)TDIV1,TDIV2,TDIV3,TDIV4
      END IF

      CLOSE(REPLU(GNUM))                                                        
      IF(COPY.NE.0) CALL SPOOL(REPNAM(GNUM),COPY,STATUS)                        
3000   CONTINUE                                 
C                                                                               
C     ==================== Format Statements ======================             
C                                                                               

C---- Page Header format statements.

8001  FORMAT(A8,I1,'  BIG PRIZE WINNERS REPORT FOR DRAW ',A5)                   
80011 FORMAT('RAVI V65  BIG PRIZE WINNERS REPORT FOR DRAW ',A5)                   
80012 FORMAT('RAVI V5   BIG PRIZE WINNERS REPORT FOR DRAW ',A5)                   

8002  FORMAT(A2,I1,'BIGWIN.REP')                                             

9000  FORMAT(1X,131('='),/)                                                     

C---- Game Column headings format statements.

9001  FORMAT(1X,'SELLING SELLING',
     *       7X,'TICKET SERIAL',8X,
     *       ' CDC TICKET  BANK   BANK',            
     *       8X,' DRAW',/,2X,'AGENT   TERM',
     *       3X,'INTERNAL',5X,'EXTERNAL'5X,
     *       ' SOLD STATUS   ID   ACCOUNT',5X,' WON/EXP',                     
     *       5X,'TOT WON',' FRAC',' DIV 1/06 2/07 3/08 4/09 5/10 ')                                                  

90011 FORMAT(1X,'SELLING SELLING',
     *       7X,'TICKET SERIAL',8X,
     *       ' CDC TICKET  BANK   BANK',        
     *       8X,' DRAW',14X,'JOKER',/,2X,                                        
     *       'AGENT   TERM',
     *       3X,'INTERNAL',5X,'EXTERNAL'5X,
     *       ' SOLD STATUS   ID   ACCOUNT',5X,' WON/EXP',                     
     *       5X,'TOT WON 1/2',' FR ',<TIMES>(' DV',I1))     

90012 FORMAT(1X,'SELLING SELLING',
     *       7X,'TICKET SERIAL',8X,
     *       ' CDC TICKET BANK   BANK',
     *       8X,' DRAW',14X,/,2X,
     *       'AGENT   TERM',
     *       3X,'INTERNAL',5X,'EXTERNAL'5X,
     *       ' SOLD STATUS  ID   ACCOUNT',4X,' WON/EXP',
     *       2X,'  TOT WON ','TOT REF',
     *       1X,' FR ',1X,'DIV1',1X,'DIV2',1X,'DIV3',1X,'DIV4')

90013 FORMAT(1X,'SELLING SELLING',
     *       7X,'TICKET SERIAL',8X,
     *       ' CDC TICKET  BANK   BANK',
     *       8X,' DRAW',14X,/,2X,
     *       'AGENT   TERM',
     *       3X,'INTERNAL',5X,'EXTERNAL'5X,
     *       ' SOLD STATUS   ID   ACCOUNT',5X,' WON/EXP',
     *       5X,' TOT WON    ',
     *       ' FR ',8X,'WINNING SHARES')
90014 FORMAT(1X,'SELLING SELLING',
     *       7X,'TICKET SERIAL',8X,
     *       ' CDC TICKET  BANK   BANK',    
     *       8X,' DRAW',14X,'JOKER',/,2X,                                        
     *       'AGENT   TERM',
     *       3X,'INTERNAL',5X,'EXTERNAL'5X,
     *       ' SOLD STATUS   ID   ACCOUNT',5X,' WON/EXP',                     
     *       5X,'TOT WON 1/2',' FR ',<NUMDIV(GNUM)>(A4,I1),' BNS')     
90015 FORMAT(1X,'SELLING SELLING',
     *       7X,'TICKET SERIAL',8X,
     *       ' CDC TICKET  BANK   BANK',    
     *       8X,' DRAW',14X,'JOKER',/,2X,                                        
     *       'AGENT   TERM',
     *       3X,'INTERNAL',5X,'EXTERNAL'5X,
     *       ' SOLD STATUS   ID   ACCOUNT',5X,' WON/EXP',                     
     *       5X,'TOT WON 1/2',' FR ',<TIMES>(' DV',I1),' REF')

C---- Data format Statements.

9002  FORMAT(1X,A8,1X,I5,I10,1X,I3.3,'-',I8.8,'-',I3.3,                  
     *       1X,I5,1X,A4,1X,I8.8,1X,I7.7,'-',I1,1X,I5,                          
     *       '/',I5,A11,1X,A3,1X,I2,1X,7I4)  

9006  FORMAT(1X,A8,1X,I5,I10,1X,I3.3,'-',I8.8,'-','***',
     *       1X,I5,1X,A4,1X,I8.8,1X,I7.7,'-',I1,1X,I5,                          
     *       '/',I5,A11,1X,A3,1X,I2,1X,7I4)  


90021 FORMAT(1X,A8,1X,I5,I10,1X,I3.3,'-',I8.8,'-',I3.3,
     *       1X,I5,1X,A4,1X,I8.8,1X,I7.7,'-',I1,1X,I5,
     *       '/',I5,A11,A8,1X,I2,1X,I4,1X,I4,1X,I4,1X,I4)

90022 FORMAT(1X,A8,1X,I5,I10,1X,I3.3,'-',I8.8,'-','***',
     *       1X,I5,1X,A4,1X,I8.8,1X,I7.7,'-',I1,1X,I5,
     *       '/',I5,A11,A8,1X,I2,1X,I4,1X,I4,1X,I4,1X,I4)

90023 FORMAT(1X,A8,2X,I5,I10,1X,I3.3,'-',I8.8,'-',I3.3,
     *       1X,I5,1X,A4,1X,I8.8,1X,I7.7,'-',I1,1X,I5,
     *       '/',I5,A11,1X,A3,1X,I2)

90025 FORMAT(1X,A8,1X,I5,I10,1X,I3.3,'-',I8.8,'-','***',
     *       1X,I5,1X,A4,1X,I8.8,1X,I7.7,'-',I1,1X,I5,
     *       '/',I5,A11,1X,A3,1X,I2)

C---- Total Format statements.

9003  FORMAT(//,' Total ',A8,I1,' Won ',A13)                                  

9004  FORMAT(97X,7I4,/)                                                         

9005  FORMAT(96X,A3,4X,7I4)

9020  FORMAT(/,1X,'TOTAL DIV1 ',I10,/,1X,
     *          'TOTAL DIV2 ',I10,/,1X,
     *          'TOTAL DIV3 ',I10,/,1X,
     *          'TOTAL DIV4 ',I10)
9021  FORMAT(I2,1X,'SHARES IN',1X,A8,1X,'(DIV ',I2,')')
9022  FORMAT(99X,I2,1X,'SHARES IN',1X,A8,1X,'(DIV ',I2,')')
C                                                                               
      RETURN                                                                    
      END                                                                       
