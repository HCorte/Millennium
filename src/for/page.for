C SUBROUTINE PAGE
C
C V72 04-FEB-2021 SCML NEW TERMINAL PROJECT - Added support for OLM
C V71 31-MAR-2016 SCML M16 PROJECT: changed EUROMIL snapshot command prompt
C                      usage
C V70 25-FEB-2014 SCML Placard Project
C V69 04-JAN-2011 FJG Lotto2 Project: Batch2 download BUG discovered
C V68 12-OCT-2010 MAC ePASSIVE INVOICE
C V67 01-JAN-2010 FJG ePASSIVE
C V66 05-FEB-2001 ANG CHANGED PROMPT FOR LOTTO, JOKER AND SPORTS. ADDED PASSIVE SNAPSHOTS
C V65 26-JAN-2001 UXN GUISNP added.
C V64 02-DEC-2000 UXN TGLSNP, TGNsnp added.
C V63 16-JUN-2000 OXK Cleanup w/ WARNINGS=ALL
C V62 31-MAY-2000 PXO Subroutine names changed for:ACT,LOT,JOK,TUL,VOI,SEL,VAK,
C                     TRA,FLW,SPD,OVR,VKST,TVOI,TTUL,SKL,PPL,TSK,TPR,SKO,PPO,
C                     MVE,TMV,TPT,MVO,TTR.
C V61 14-FEB-2000 OXK GIND added to VAKSTSNP (Vakio changes)
C V60 21-DEC-1999 OXK WINSNP added.
C V59 07-DEC-1999 OXK GTCSNP added.
C V58 22-NOV-1999 UXN ICSSNP added.
C V57 13-OCT-1999 RXK MYMSNP added for World Tour
C V56 21-SEP-1999 UXN SSCSNP, TROSNP, PTOSNP calling sequence changed.
C V55 18-MAY-1999 UXN TRPSNP,TROSNP, TTRSNP ADDED FOR SUPER TRIPLE.
C V54 06-MAR-1997 RXK Ind=0 allowed for AGTsnp 
C V53 18-DEC-1996 HXK Update from TEBE project (MXP,WXW,PXN,MJF)
C V52 28-NOV-1996 WXW Telebetting startup, changes MP/WXW.
C                     TEBSNP added.
C V51 20-MAY-1996 HXK Restoring X.25 / TEBE version
C V50 27-FEB-1996 RXK Different fixes
C V49 08-FEB-1996 RXK Menue tables changed
C V48 07-FEB-1996 RXK Different fixes 
C V47 01-FEB-1996 RXK Forgotten type statements removed
C V46 31-JAN-1996 RXK Rfss 249, change of some snapshot names
C V45 29-JAN-1996 RXK Different fixes connected to introduction of 4th menu
C V44 21-DEC-1995 PXB Added PAROSNP
C V43 12-DEC-1995 PXB Added new menu page
C V42 09-AUG-1995 HXK Changed name of V5 snapshot
C V41 01-AUG-1995 PXB Added RV5 snp and changed RAVI to be R65
C V40 24-APR-1995 HXK Initial revision.
C V39 21-FEB-1995 PXB Initial revision.
C V38 27-OCT-1994 PXB Added TTUL and TVOI snapshots.
C V37 24-OCT-1994 PXB Changed format statement after bl1snp and bl2snp.
C V36 18-OCT-1994 PXB Added Bingo and Fullhouse snapshot screens.
C V35 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V34 26-JUN-1994 HXK LATEST VERSION FROM SUSAN.
C V33 21-MAY-1994 HXK /XXXX should not appear on screen due to this routine.
C V32 06-MAY-1994 JXP New format line for rscsnp
C V31 28-APR-1994 JXP Updated RSCsnp
C V30 19-APR-1994 HXK ALLOW LTOSNP TO BE ACCESSED BY VIKsnp COMMAND.
C V29 25-FEB-1994 HXK PITKA LIABILITY LIMITATIONS CHANGE.
C V28 12-JAN-1994 JXP Changed WINtip to VOIsnp  & SCOre to TULos
C V27 11-JAN-1994 JXP Included extra page parameter for TATSNP
C V26 17-NOV-1993 SXH Disabled STG and STL snapshots
C V25 30-SEP-1993 HXK BUG FIX FOR VKSTSNP.
C V24 29-SEP-1993 HXK Added VAKSTSNP.
C V23 24-SEP-1993 GXA Changed Date display to be DD/MM/YY.
C V22 24-SEP-1993 GXA Changed name of Sport snap from SPT to VAK.
C V21 06-JUL-1993 SXH Initial Release for Finland
C V20 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V19 22-JAN-1993 DAB Comm Update
C V18 30-AUG-1992 HDB ADDED NEW SNAPSHOTS STL,STG
C                     CORRECTED MENU TO SHOW ALL MENU ITEMS
C V17 29-JUL-1992 WLM  ADDED NEW SNAPSHOTS: TSP, ATT, TAT
C V16 16-FEB-1992 GCAN ADDED REFRESH COMMAND
C V15 17-OCT-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C                      (CONVERTED FOR VAX, TAKEN FROM SWEDEN)
C                      REMOVED ALL DATEX RELATED SNAPSHOTS AND ADDED
C                      HRSSNP,NBRSNP,OUSSNP,LIBSNP
C V14 11-SEP-1991  JB  ADDED REQSNP                          
C V13-05-JUL-1991 GCAN ADDED 3'd MENUE FOR X2X SUBSYSTEM.    
C                      MADE A NEW SEPARATE VISION(X).        
C V12 27-JUN-1991 JAN ADDED LTP SNAPSHOT                     
C V11 04-JUN-1991 JAN TOOK OUT SAFARI ANF FLAX SNAPSHOT      
C V10 15-JAN-1991 Per ADDED SAFSNP                           
C V09 20-DEC-1990 Per ENCODE XLINSP                          
C V05 25-MAY-1990 MGM CHANGE DATE PROMPTS                    
C V04 26-APR-1990 MGM ADDED RMASNP&RTOSNP                    
C V02 10-MAR-1990 MGM ADDED FLXSNP                           
C V01 01-FEB-1989 MTK INITIAL RELEASE FOR SWEDEN             
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
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PAGE                                                 
        IMPLICIT NONE
C       
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'                                     
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'                                     
C                                                                               
        INTEGER*4 MENU(114)     !
        INTEGER*4 MENU1(114)    !what to display where for menu 1
        INTEGER*4 MENU2(114)    !,,          ,,             ,,  2
        INTEGER*4 MENU3(114)    !,,          ,,             ,,  3
        INTEGER*4 MENU4(114)    !,,          ,,             ,,  3
        INTEGER*4 SLINE(20)     !
        INTEGER*4 PRDAT(3)      !date record
        INTEGER*4 PRTIM(2)      !time record
        INTEGER*4 IND           !
        INTEGER*4 DAT           !
        INTEGER*4 NUM           !
        INTEGER*4 CMB(3)        !
        INTEGER*4 TXT           !
        INTEGER*4 PAG           !
        INTEGER*4 K             !
        INTEGER*4 XNUM          !
        INTEGER*4 L2            !
        INTEGER*4 ERR           !
        INTEGER*4 KDAT          !
        INTEGER*4 XBAMT         !
        INTEGER*4 BAMT          !
        INTEGER*4 XVAMT         !
        INTEGER*4 VAMT          !
        INTEGER*4 LNUM          !
        INTEGER*4 LKEY          !
        INTEGER*4 PDAT          !
        INTEGER*4 PNUM          !
        INTEGER*4 UNT           !
        INTEGER*4 J             !
        INTEGER*4 ST            !
        INTEGER*4 XMOPT/-1/     !
        INTEGER*4 BIGNUM        !
        INTEGER*4 XIND          !
        INTEGER*4 XKEY          !
        INTEGER*4 LQ1           !
        INTEGER*4 I             !
        INTEGER*4 KEY           !
        INTEGER*4 BLANK         !
        INTEGER*4 RECV          !
        INTEGER*4 STARTIME/0/   !
        INTEGER*4 PKEY          !
        INTEGER*4 LASTGOODMOPT
        INTEGER*4 LASTKEY,LASTMOPT
C       
        CHARACTER    CLINE(80)   !
        CHARACTER    SPACE       !                                 
        CHARACTER*30 COPFILNAM
C        CHARACTER*2 NCOPY       !
C        CHARACTER*2 YCOPY       !                                   

        LOGICAL     GOBACK
        CHARACTER*4 C4NAME      !
        DOUBLE PRECISION VISNAM
        EQUIVALENCE(C4NAME,CLINE,VISNAM)

        LOGICAL     PMODE       !
        LOGICAL     COPY        !

        DOUBLE PRECISION KEYLST(42)
        DOUBLE PRECISION KEYLST1(42)
        DOUBLE PRECISION KEYLST2(42)
        DOUBLE PRECISION KEYLST3(42)   
        DOUBLE PRECISION KEYLST4(42)  ! V72
        DOUBLE PRECISION ALLKEY(168)  ! V72
C        DOUBLE PRECISION KEYLST4(50)   ! V72
C        DOUBLE PRECISION ALLKEY(176)   ! V72                                   
C
        EQUIVALENCE(SLINE,CLINE)                                          
        EQUIVALENCE(KEYLST1(1),ALLKEY(1))                                 
        EQUIVALENCE(KEYLST2(1),ALLKEY(43))                                
        EQUIVALENCE(KEYLST3(1),ALLKEY(85))                                
        EQUIVALENCE(KEYLST4(1),ALLKEY(127))

        DATA PMODE/.FALSE./
        DATA COPY/.FALSE./                                 
        DATA RECV/'RECE'/                                                 
        DATA BLANK/'    '/                                                
        DATA SPACE/' '/                                                   
C        DATA YCOPY(1:1)/Z1B/
C        DATA YCOPY(2:2)/Z2A/                                  
C        DATA NCOPY(1:1)/Z1B/
C        DATA NCOPY(2:2)/Z3F/                                  
C
C       (1 TO 42)
        DATA KEYLST1/'PRInt   ','MENU1   ','MENU2   ','STOp    ',               
     *               'MENU3   ','REF     ','COPy    ','MENU4   ',
     *               'UNUSED  ','ACTagt  ','AGTsnp  ','INVsnp  ',
     *               'HOTsnp  ','HELp    ','TSOdsnp ','SPName  ',
     *               'TSPsnp  ','STGstat ','LOTto   ','TATsnp  ',               
     *               'ATTsnp  ','JOKer   ','SCRsnp  ','WITsnp  ',
     *               'SELect  ','SPTsnp  ','GAMsnp  ','CLOse   ',
     *               'SLIsnp  ','DATes   ','SCLosnp ','NBRsnp  ',
     *               'STLstat ','RSCsnp  ','RWTsnp  ','TWDsnp  ',
     *               '<index> ','<event#>','<draw #>','        ',
     *               '<number>','< date >'/               
C       (43 - 84)
        DATA KEYLST2/'PRInt   ','MENU1   ','MENU2   ','STOp    ',         
     *               'MENU3   ','UNUSED  ','COPy    ','MENU4   ',
     *               'UNUSED  ','PERform ','TYPsnp  ','SPEsnp  ',
     *               'VALid   ','MESsnp  ','MISsnp  ','TCFsnp  ',
     *               'TRAns   ','LPLsnp  ','WRFsnp  ','LOGsnp  ',
     *               'COMsnp  ','NETsnp  ','QUEsnp  ','HRSsnp  ',
     *               'SYStem  ','FLWsnp  ','GTNsnp  ','TSALes  ',         
     *               'BUFsnp  ','SALes   ','OVRsnp  ','UNDsnp  ',
     *               'TGLsnp  ','TGNsnp  ','GUIsnp  ','PSVsnp  ',
     *               '<index> ','<event#>','<draw #>','        ',
     *               '<number>','< date >'/         
C                                                   
C MENU 3 IS DEDICATED FOR X2X RELATED STUFF (85- 126)
C
        DATA KEYLST3/'PRInt   ','MENU1   ','MENU2   ','STOp    ',         
     *               'MENU3   ','UNUSED  ','COPy    ','MENU4   ',
     *               'UNUSED  ','X2Comsnp','X2Delay ','LANsnp  ',
     *               'CONsnp  ','ETHsnp  ','SAPsnp  ','PTLsnp  ',
     *               'X2ALLSTn','X2ALLSAp','X2STNsnp','X2SAPsnp',
     *               'X2STNPrt','X2NETsnp','X2Gblsnp','X2Relsnp',
     *               'X2ALLRel','REQsnp  ','SPSTsnp ','NACsnp  ',
     *               'X2AGTsnp','BL1snp  ','BL2snp  ','TWITsnp ',
     *               'TSCRsnp ','SNIFsnp ','UNUSED  ','UNUSED  ',
     *               '<index> ','<event#>','<draw #>','        ',
     *               '<number>','< date >'/         

C MENU 4  (127-168)
C
        DATA KEYLST4/'PRInt   ','MENU1   ','MENU2   ','STOp    ',         
     *               'MENU3   ','UNUSED  ','COPy    ','MENU4   ',
     *               'UNUSED  ','SKLsnp  ','PPLsnp  ','TSKsnp  ', 
     *               'TPRsnp  ','SKOsnp  ','PPOsnp  ','PVLsnp  ',
     *               'TCPSNP  ','INSSNP  ','DLLSNP  ','X2MONSNP',
c     *               'GTXSNP  ','UNUSED  ','MVEsnp  ','PTRsnp  ',
     *               'GTXSNP  ','EUROMIL ','MVEsnp  ','PTRsnp  ',
     *               'TMVSNP  ','TPTSNP  ','MVOsnp  ','PTOsnp  ',
     *               'IVPsnp  ','TTRsnp  ','TROsnp  ','TRPsnp  ',    !V68
!    *               'UNUSED  ','Unused  ','GTCsnp  ','WINsnp  ',    !V70
C     *               'UNUSED  ','IGSsnp  ','GTCsnp  ','WINsnp  ',    !V70
     *               'OLMsnp  ','IGSsnp  ','GTCsnp  ','WINsnp  ',    !V72     
C     *               'OLMsnp  ','UNUSED  ','UNUSED  ','UNUSED  ',    !V72
C     *               'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',    !V72
     *               '<index> ','<event#>','<draw #>','        ',
     *               '<number>','< date >'/         
C                                                                               
        DATA MENU1/01,40,40, 02,40,40, 07,40,40, 03,40,40,                
     *             04,40,40, 05,40,40, 06,40,40, 08,40,40, 
     *             10,41,40, 14,41,40, 11,42,41, 12,41,40, 
     *             13,41,40, 15,37,38, 19,38,40, 22,37,39, 
     *             24,37,38, 25,37,38, 26,37,38, 27,40,40, 
     *             28,41,40, 29,37,38, 30,38,40, 23,37,38, 
     *             31,37,38, 34,37,38, 35,37,38, 32,37,39, 
     *             36,37,41, 16,37,39, 17,37,41, 20,42,41, 
     *             21,41,40, 18,40,40, 40,40,40, 33,37,40, 
     *             40,40,40, 40,40,40/
C                                                                         
        DATA MENU2/01,40,40, 02,40,40, 07,40,40, 03,40,40,                
     *             04,40,40, 05,40,40, 40,40,40, 08,40,40, 
     *             11,41,40, 12,41,40, 13,42,41, 16,42,41, 
     *             22,40,40, 17,41,40, 18,37,41, 10,40,40, 
     *             20,40,40, 21,41,40, 23,40,40, 24,41,40, 
     *             35,41,40, 26,40,40, 27,41,40, 29,40,40, 
     *             30,42,41, 36,41,40, 28,42,40, 32,37,38, 
     *             31,37,38, 14,40,40, 33,39,40, 34,39,40, 
     *             15,42,41, 19,41,40, 40,40,40, 40,40,40, 
     *             40,40,40, 40,40,40/
C                                                                         
        DATA MENU3/01,40,40, 02,40,40, 07,40,40, 03,40,40,                
     *             04,40,40, 05,40,40, 40,40,40, 08,40,40, 
     *             10,41,40, 11,40,40, 12,41,40, 13,41,40, 
     *             14,41,40, 15,41,40, 16,41,40, 17,41,40, 
     *             18,41,40, 19,41,40, 20,41,40, 21,41,40, 
     *             22,41,40, 23,41,40, 24,40,40, 25,40,40, 
     *             26,37,41, 27,41,40, 28,40,40, 29,40,40, 
     *             30,41,37, 31,41,37, 32,40,40, 33,40,40, 
     *             40,40,40, 40,40,40, 40,40,40, 40,40,40, 
     *             40,40,40, 40,40,40/

        DATA MENU4/01,40,40, 02,40,40, 07,40,40, 03,40,40,                
     *             04,40,40, 05,40,40, 40,40,40, 08,40,40, 
     *             10,37,38, 11,37,38, 12,42,40, 13,42,40, 
     *             14,41,37, 15,41,37, 16,40,40, 17,40,40,
     *             18,39,40, 19,40,40, 20,40,40, 21,40,40,
C     *             22,42,40, 23,37,38, 24,37,38, 25,42,40,
     *             22,40,40, 23,37,38, 24,37,38, 25,42,40,
     *             26,42,40, 27,37,41, 28,37,41, 29,41,40,       !V68
     *             30,42,40, 31,37,41, 32,37,38, 33,40,40,       !V72
     *             34,40,40, 35,42,41, 36,40,40, 40,40,40,
     *             40,40,40, 40,40,40/

C
C INITIALIZE TIMES FOR AUTOMATIC SHUT OFF
C
        IF(STARTIME.EQ.0) THEN
           STARTIME=P(ACTTIM)
        ENDIF
C                                                                               
C INITIALIZE BUFFER                                                             
C                                                                               
        IF(VINIT.EQ.1) THEN 
           VINIT=0
           IF(LASTGOODMOPT.EQ.0) THEN
              MOPT=1
           ELSE
              MOPT=LASTGOODMOPT
           ENDIF
           CALL SPACES
           DEL=1000                                                     
           DO 1 I=1,42                                                  
              KEYLST(I)=KEYLST1(I)                                      
1          CONTINUE                                                     
           DO 2 I=1,114
              MENU(I)=MENU1(I)                                          
2          CONTINUE                                                     
           GOTO 22                                                      
        ENDIF                                                           
C                                                                               
C CHECK FOR INPUT                                                               
C                                                                               
        DO 3 I=1,80                                                     
           CLINE(I)=SPACE
3       CONTINUE                                               
        LQ1=LQ                                                          
C
C SET STARTING TIME FOR AUTOMATIC SHUT OFF
C
        IF(LQ.NE.0) THEN
           STARTIME=P(ACTTIM)
           GOTO 4
        ENDIF
C
C NO INPUT, TEST IF VISION TERMINAL HAS BEEN LEFT ON FOR MORE THEN
C THE REQUIRED TIME FOR INACTIVE TERMINALS.
C
        IF(SHUTS(PASS).GT.0) THEN
           IF(P(ACTTIM)-STARTIME.GE.SHUTS(PASS)) THEN
              STP=1
              CLR=1
              RETURN
           ENDIF
        ENDIF                                                        
C                                                                               
C NO INPUT                                                                      
C                                                                               
        IF(SMODE) RETURN          !STATIC DISPLAY NO UPDATE       
        GOTO 20                                                   
C                                                                               
C PROCESS INPUT                                                                 
C                                                                               
4       XKEY=KEY
        LASTGOODMOPT=MOPT
        IF(.NOT.(MOPT.EQ.1 .AND. KEY.EQ.14)) THEN
           LASTMOPT=MOPT
           LASTKEY=KEY
        ENDIF
        CALL LINTER(ALLKEY,XKEY,XIND,BIGNUM,KDAT,XBAMT,XVAMT,ERR,L2,CMB,TXT,PAG)
!       CALL OPS('XIND', XIND, XIND) ! DEBUG REMOVER
!       CALL OPS('BIGNUM', BIGNUM, BIGNUM) ! DEBUG REMOVER
        XNUM=BIGNUM
        LQ=0                                                      
        WRITE (CLIN23,99)                                         
99      FORMAT(80(' '))                                           
C                                                                               
C PROCESS PRIVILEGED COMMANDS                                                   
C                                                                               
        IF(XKEY.EQ.3) GOTO 12           !PRINT MENU               
        IF(ERR.LT.0.AND.XKEY.EQ.14.AND.MOPT.EQ.1) THEN                          
           K=1                                                    
           DO 10 I=L2,LQ1                                         
              CLINE(K)=LINE(I)                                    
              K=K+1                                               
10         CONTINUE                                               
           CALL SPACES                                            
           KEY=XKEY                                               
           GOTO 14                                                
        ENDIF
C                                                                     
        IF(ERR.EQ.-5.OR.ERR.EQ.-6) GOTO 12                        
        IF(ERR.GE.0) GOTO 12                                      
        DO 11 I=1,LQ1                                             
11         CLINE(I)=LINE(I)                                       
        GOTO 20                           !GO DECODE COMMAND      
C                                                                               
C CHECK FOR INVALID INPUT                                                       
C                                                                               
12      IF(ERR.GE.0) GOTO 14                                      
        WRITE (CLIN23,9000)                                       
        IF(ERR.EQ.-5) WRITE (CLIN23,9003)                         
        IF(ERR.EQ.-6) WRITE (CLIN23,9014)                         
        MOPT=LASTGOODMOPT
9000    FORMAT('Invalid input')                                   
9003    FORMAT('Bad check digits entered')                        
        GOTO 60                                                   
C                                                                               
C CONTROL FUNCTIONS                                                             
C                                                                               
14      IF(XKEY.LT.1.OR.XKEY.GT.9) GOTO 15                        
        GOTO (50,48,49,45,47,30,51,52,30) XKEY                          
15      NUM=XNUM                                                  
        DAT=KDAT                                                  
        IND=XIND                                                  
        BAMT=XBAMT
        VAMT=XVAMT
        PMODE=.FALSE.                                             
        IF(XKEY.NE.KEY.OR.MOPT.NE.XMOPT) THEN  !!! CHANGING MENU
	   CALL FASTSET(0,CMB,3)
	ENDIF
        IF(XKEY.EQ.KEY) GOTO 20
        KEY=XKEY                                                  
        XMOPT=MOPT        
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     KEY haves the value of 0 to 40 of the option chosen     C  
C     XMOPT haves the value of 1 to 4 that corresponds        C  
C     to the Menu of the option chosen                        C  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     vai ler de uma flag que indica se já esteve na tela olm
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        SMODE=.FALSE.                                             
        CALL SPACES                                               
        CLR=1                                                     
        RETURN                                                    
20      CONTINUE                                                  
        WRITE (CLIN24,99)                                         
C                                                                               
C BRANCH TO CORRECT MENUE                                                       
C
        GOTO (24,25,27,28) MOPT                                      
C                                                                               
C PROCESS MENU 1                                                                
C                                                                               
24      CONTINUE                                                  
        IF(KEY.LT.10.OR.KEY.GT.38) GOTO 22                         
        CALL CHKSNP(KEY,ST)                                       
        IF(ST.NE.0) THEN                                          
           WRITE (CLIN23,9053)                                    
           GOTO 60                                                
        ENDIF                                                     
        CALL LIB$MOVC5(0,0,ICHAR(' '),SIZEOF(NEW),NEW)
        GOTO (     0100,0200,0300,
     *        0400,0500,0600,0700,
     *        0800,0900,1000,1100,
     *        1200,1300,1400,1500,
     *        1600,1700,1800,1900,
     *        2000,2100,2200,2300,
     *        2400,2500,2600,2700) KEY-9
C                                                                               
C PROCESS MENU 2                                                                
C                                                                               
25      CONTINUE                                                  
        IF(KEY.LT.10.OR.KEY.GT.38) GOTO 22                         
        CALL CHKSNP(KEY+42,ST)                                    
        IF(ST.NE.0) THEN                                          
           WRITE (CLIN23,9053)                                    
           GOTO 60                                                
        ENDIF                                                     
        CALL LIB$MOVC5(0,0,ICHAR(' '),SIZEOF(NEW),NEW)
        GOTO (     4100,4200,4300,
     *        4400,4500,4600,4700,
     *        4800,4900,5000,5100,
     *        5200,5300,5400,5500,
     *        5600,5700,5800,5900,
     *        6000,6100,6200,6300,
     *        6400,6500,6600,6700) KEY-9 
C                                                                               
C PROCESS MENU 3                                                                
C                                                                               
27      CONTINUE         
        IF(KEY.LT.10.OR.KEY.GT.38) GOTO 22                         
        CALL CHKSNP(KEY+84,ST)                                    
        IF(ST.NE.0) THEN                                          
           WRITE (CLIN23,9053)                                    
           GOTO 60                                                
        ENDIF                                                     
        CALL LIB$MOVC5(0,0,ICHAR(' '),SIZEOF(NEW),NEW)
        GOTO (     7000,7050,7100,
     *        7150,7200,7250,7300,
     *        7350,7400,7450,7500,
     *        7550,7600,7650,7700,
     *        7750,7800,7850,7900,
     *        7950,7960,7970,7980,
     *        7990,7991,7992,7993 ) KEY-9
C 
C PROCESS MENU 4
CC
28      CONTINUE         
        IF(KEY.LT.10.OR.KEY.GT.38) GOTO 22                         
        CALL CHKSNP(KEY+126,ST)                                    
        IF(ST.NE.0) THEN                                          
           WRITE (CLIN23,9053)                                    
           GOTO 60                                                
        ENDIF      
C
C Initialize the screen variables.
C 
        CALL LIB$MOVC5(0,0,ICHAR(' '),SIZEOF(NEW),NEW)
        GOTO (     8000,8001,8002,
     *        8003,8004,8005,8006,
     *        8007,8008,8009,8010,
     *        8011,8012,8013,8014,
     *        8015,8016,8017,8018,
     *        8019,8020,8021,8022,
     *        8023,8024,8025,8026,
     *        8027 ) KEY-9                          
C                                                                               
22      CONTINUE
        XNUM=-1000                                                
        BIGNUM=-1000                                              
        KDAT=-1000                                                
        NUM=0                                                 
        XIND=-1000                                                
        IND=-1000                                                 
        DAT=-1000                                                 
        XBAMT=-1000
        BAMT=-1000
        XVAMT=-1000
        VAMT=-1000
        KEY=0                                                     
        XKEY=0       
        TXT=0                                             
        PMODE=.FALSE.                                             
        SMODE=.TRUE.                                              
        CALL SPACES                                               
        CLR=1                                                     
        WRITE (CLIN1,98001)                                        
98001   FORMAT('***** Vision Commands *****',52(' '))             
        WRITE (CLIN2,26) (KEYLST(MENU(J)),J=1,6)                  
        WRITE (CLIN3,26) (KEYLST(MENU(J)),J=7,12)                 
        WRITE (CLIN4,26) (KEYLST(MENU(J)),J=13,18)                
        WRITE (CLIN5,26) (KEYLST(MENU(J)),J=19,24)                
C                                                                               
        WRITE (CLIN6,98002) MOPT                                   
98002   FORMAT('***** Snapshots available in menu ',I1,' *****')  
        WRITE (CLIN7,26) (KEYLST(MENU(J)),J=25,30)                
        WRITE (CLIN8,26) (KEYLST(MENU(J)),J=31,36)                
        WRITE (CLIN9,26) (KEYLST(MENU(J)),J=37,42)                
        WRITE (CLIN10,26)(KEYLST(MENU(J)),J=43,48)                
        WRITE (CLIN11,26)(KEYLST(MENU(J)),J=49,54)                
        WRITE (CLIN12,26)(KEYLST(MENU(J)),J=55,60)                
        WRITE (CLIN13,26)(KEYLST(MENU(J)),J=61,66)                
        WRITE (CLIN14,26)(KEYLST(MENU(J)),J=67,72)                
        WRITE (CLIN15,26)(KEYLST(MENU(J)),J=73,78)                
        WRITE (CLIN16,26)(KEYLST(MENU(J)),J=79,84)                
        WRITE (CLIN17,26)(KEYLST(MENU(J)),J=85,90)                
        WRITE (CLIN18,26)(KEYLST(MENU(J)),J=91,96)                
        WRITE (CLIN19,26)(KEYLST(MENU(J)),J=97,102)                
        WRITE (CLIN20,26)(KEYLST(MENU(J)),J=103,108)
CCC     WRITE (CLIN21,26)(KEYLST(MENU(J)),J=109,114)
CCC     WRITE (CLIN22,98021)
        WRITE (CLIN22,98022)
98021   FORMAT('For help with vision commands, enter HELP SYNTAX')
98022   FORMAT('For help with snapshots, enter HELP MENU, ',
     *         'HELP [snpname] or HELP num')
26      FORMAT(3A8,16X,3A8,16X)                                   
C                                                                               
        WRITE (CLIN23,9001)                                       
9001    FORMAT('Enter vision command')                            
        GOTO 60                                                   
C                                                                               
C ENTER CHANGE OR OPERATOR MODE                                                 
C                                                                               
30      CONTINUE
        CALL LIB$MOVC5(0,0,ICHAR(' '),SIZEOF(OLD),OLD)              
9898    FORMAT(1X,20(A4))
C                                                                               
C AVAILABLE FOR USE                                                             
C                                                                               
        GOTO 60                                                   
C                   
C PROGRAM EXIT                                                                  
C                                                                               
45      CONTINUE    
        CLR=1                                                     
        STP=1                                                     
        RETURN                                                    
C                                                                               
C SET MENU 1                                                                    
C                                                                               
48      CONTINUE                                                  
        DO 148 I=1,42                                             
        KEYLST(I)=KEYLST1(I)                                      
148     CONTINUE                                                  
        DO 248 I=1,114                                             
        MENU(I)=MENU1(I)                                          
248     CONTINUE                                                  
        MOPT=1                                                    
        GOTO 22                                                   
C                                                                               
C SET MENU 2                                                                    
C                                                                               
49      CONTINUE                                                  
        DO 149 I=1,42                                             
        KEYLST(I)=KEYLST2(I)                                      
149     CONTINUE                                                  
        DO 249 I=1,114                                             
        MENU(I)=MENU2(I)                                          
249     CONTINUE                                                  
        MOPT=2                                                    
        GOTO 22                                                   
C
C SET MENU 3
C
47      CONTINUE
        DO 147 I=1,42
          KEYLST(I)=KEYLST3(I)
147     CONTINUE
        DO 247 I=1,114
          MENU(I)=MENU3(I)
247     CONTINUE
        MOPT=3
        GOTO 22
C                                                                               
C SET MENU 4                                                                    
C                                                                               
52      CONTINUE                                                  
        DO 152 I=1,42                                             
          KEYLST(I)=KEYLST4(I)                                      
152     CONTINUE                                                  
        DO 252 I=1,114                                            
          MENU(I)=MENU4(I)                                          
252     CONTINUE                                                  
        MOPT=4
        GOTO 22                                                   
C                                                                               
C COPY AND PRINT FUNCTION                                                       
C                                                                               
51      CONTINUE                                                  
        COPY=.TRUE.                                               
        PMODE=.TRUE.                                              
        UNT=6
C       UNT=0                                                             
        GOTO 55                                                   
50      UNT=6                                                     
        PMODE=.TRUE.                                              
        COPY=.FALSE.                                              
55      CONTINUE                                                  
        PNUM=XNUM                                                 
        PDAT=KDAT                                                 
        IF(NUM.LT.0) NUM=XNUM                                     
        IF(DAT.LT.0) DAT=KDAT                                     
C                                                                 
C COMMON RETURN FOR ALL FUNCTIONS                                               
C                                                                               
60      CONTINUE                                                     
        LKEY=KEY                                                     
        LNUM=NUM 
        IF(.NOT.PMODE) RETURN      
C        
        IF(COPY) THEN                                                
C           CALL WIMG(UNT,YCOPY)   
C           TYPE *,PNUM  
           PKEY=LASTKEY
           IF(PKEY.LT.2.OR.PKEY.GT.42) PKEY=2
           PKEY=(LASTMOPT-1)*42+PKEY                   
           WRITE(COPFILNAM,65) ALLKEY(PKEY)                                        
65         FORMAT('VIS_',A8)     
           K=0
           DO PKEY=1,20
             IF(COPFILNAM(PKEY:PKEY).EQ.' ' .AND. K.EQ.0) K=PKEY               	 
           ENDDO	 
           COPFILNAM(K:K+3)='.REP'    
           OPEN(6, FILE=COPFILNAM, STATUS='NEW') 
        ELSE                                                         
           OPEN(6, FILE='VISION.REP', STATUS='NEW', DISP='PRINT/DELETE') 
        ENDIF
        
        PKEY=LKEY
        IF(PKEY.LT.2.OR.PKEY.GT.42) PKEY=2
        PKEY=(MOPT-1)*42+PKEY        
        WRITE(UNT,66) ALLKEY(PKEY)                                        
66      FORMAT('1**** Vision Snapshot: ',A8,' ****')                             
C                                                                               
C PRINT PAGE                                                                    
C                                                                               
        CALL ICLOCK(1,PRTIM)                                             
        CALL XDAT(PRDAT)                                                 
        WRITE(UNT,9055) PRDAT(2),PRDAT(3),PRDAT(1),PRTIM                 
        DO 72 I=1,22                                                     
72      WRITE(UNT,74) (NEW(J,I),J=1,20)                                  
74      FORMAT(1X,20A4)                                                  
C                                                                               
C CHECK IF END OF PRINT MODE                                                    
C                                                                               
        IF(PNUM.GT.NUM) GOTO 76                                          
        IF(PDAT.GT.DAT) GOTO 78                                          
        PMODE=.FALSE.                                                    
        COPY=.FALSE.
C        IF(COPY) THEN                                                    
C           CALL WIMG(UNT,NCOPY)                                          
C           UNT=6                                                         
C        ELSE                                                             
         CLOSE(UNIT=6)                                                 
C        ENDIF                                                            
        RETURN                                                           
76      NUM=NUM+1                                                        
                                                                                
        CLOSE(UNIT=6)                                                    
        RETURN                                                           
78      DAT=DAT+1                                                        
        CLOSE(UNIT=6)                                                    
        RETURN                                                           
C                                                                               
C CALL MENU 1 SNAPSHOT ROUTINES
C
100     CONTINUE                                                         
        CALL ACTSNP(NUM,LKEY)                                            
        IF (LIN23(1).EQ.BLANK) WRITE (CLIN23,9013)                       
        GOTO 60                                                          
C                                                                               
200     CONTINUE                               
        IF(IND.LT.0) IND=0                          
        CALL AGTSNP(NUM,DAT,SLINE,LNUM,IND)                             
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9005)                        
        GOTO 60                                                          
C                                                                               
300     CONTINUE                     
        IF(IND.LT.1) IND=1                                    
        CALL INVSNP(NUM,LKEY,LNUM,IND)                                       
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9008)                        
        GOTO 60                                                          
C                                                                               
400     CONTINUE                           
        IF(IND.LT.1) IND=1                              
        CALL HOTSNP(NUM,SLINE,LKEY,LNUM,IND)                                 
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9008)                        
        GOTO 60                                                          
C                                                                               
500     CONTINUE
        GOBACK=.FALSE.
        IF(C4NAME.EQ.'    ') THEN
           IF(LASTKEY.LE.9 .OR. (NUM.GE.1.AND.NUM.LE.5)) THEN
              C4NAME='MENU'
           ELSE
              IF(LASTMOPT.EQ.1) VISNAM=KEYLST1(LASTKEY)
              IF(LASTMOPT.EQ.2) VISNAM=KEYLST2(LASTKEY)
              IF(LASTMOPT.EQ.3) VISNAM=KEYLST3(LASTKEY)
              IF(LASTMOPT.EQ.4) VISNAM=KEYLST4(LASTKEY)
              IF(C4NAME.NE.'MENU') GOBACK=.TRUE.
           ENDIF
        ENDIF

        CALL HLPSNP(SLINE,NUM)      
        IF(.NOT.GOBACK) WRITE (CLIN23,9011)
        IF(GOBACK) THEN
           SMODE=.FALSE.
           KEY=LASTKEY
           MOPT=LASTMOPT
        ENDIF
        GOTO 60
C                                                                        
600     CONTINUE                                                         
        IF(IND.LT.1) IND=1                                               
        CALL TSODSNP(NUM,IND,DAT)                                         
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9009)                        
        GOTO 60                                                          
C
700     CONTINUE                                 
        IF(IND.LT.1) IND=1      
        CALL SPNSNP(NUM,IND,DAT)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9009)
        GOTO 60                                                          
C
800     CONTINUE                                                         
        IF(IND.LT.1) IND=1                                               
        CALL TSPSNP(IND,DAT,SLINE)                                         
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9010)                        
        GOTO 60                                                          
C                                                                               
900     CONTINUE                                                         
C       CALL STGSNP(NUM)
C        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9015)          
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9070)           

        GOTO 60                                                          
C                                                                               
1000    CONTINUE                                                         
        CALL LOTSNP(NUM,IND,SLINE)                                             
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9246)                        
        GOTO 60                                                          
C                                                                               
1100    CONTINUE                                                         
        IF(IND.LT.1) IND=1  
        CALL TATSNP(NUM,IND,DAT,BAMT)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9025)                        
        GOTO 60                                                          
C                                                                               
1200    CONTINUE                                                         
        IF(IND.LT.1) IND=1                                               
        CALL ATTSNP(NUM,LNUM,LKEY)                                         
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9010)                        
        GOTO 60                                                                 
C                                                                               
1300    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                                      
        CALL JOKSNP(NUM,IND,SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,90171)
        GOTO 60                                                                 
C                                                                               
1400    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                                      
        CALL SCRSNP(NUM,IND)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9052)                               
        GOTO 60                                                                 
C                                                                               
1500    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                                      
        CALL WITSNP(NUM,IND,SLINE)                                              
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9052)                               
        GOTO 60                                                                 
C                                                                               
1600    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                                      
        CALL SELSNP(NUM,IND,DAT)                                                
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9009)                               
        GOTO 60                                                                 
C                                                                               
1700    CONTINUE
        IF(IND.LT.1) IND=1
        CALL SPTSNP(NUM,IND,SLINE)                                                
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9246)                               
        GOTO 60                                                                 
C                                                                               
1800    CONTINUE                                                                
        CALL GAMSNP(NUM)                                                             
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9059)                               
        GOTO 60                                                                 
C                                                                               
1900    CONTINUE                                                                
        CALL CLOSNP(NUM)  
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9059)                               
        GOTO 60                                                                 
C                                                                               
2000    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                                      
        CALL SLISNP(NUM,IND)                                                   
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9017)                               
        GOTO 60                                                                 
C                                                                              
2100    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                                      
        CALL DATSNP(NUM,IND)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9021)                               
        GOTO 60                                                                 
2200    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                                      
        CALL SCLOSNP(NUM,IND,DAT)                                               
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9009)                               
        GOTO 60                                                               
C                                                                               
2300    CONTINUE
        IF(IND.LT.1) IND=1
        CALL NBRSNP(NUM,IND,SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE(CLIN23,9017)
        GOTO 60                                 
C                                 
2400    CONTINUE                                                                
C       CALL STLSNP(NUM,IND)
C        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9015)      
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9070)       

        GOTO 60                                                                 
C
2500    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                                      
        CALL RSCSNP(NUM,IND,DAT)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9054)                               
        GOTO 60                                                                 
C
2600    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                                      
        CALL RWTSNP(NUM,IND)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9052)                               
        GOTO 60
C
2700    CONTINUE
        IF(IND.LT.1) IND=1
C       CALL LIBSNP(NUM,IND)
        CALL TWDSNP(NUM,IND,DAT)
        IF(LIN23(1).EQ.BLANK) WRITE(CLIN23,9010)
        GOTO 60
C                                                                               
C PUT NEXT MENU1 SNAPSHOT HERE                                                  
C                                                                               
C                                                                               
C                                                                               
C PROCESS MENU 2 SNAPSHOTS                                                      
C                                                                               
4100    CONTINUE       
        CALL PERSNP                                                             
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9015)                               
        GOTO 60                                                                 
C                                                                               
4200    CONTINUE                                                                
        CALL TYPSNP(NUM,SLINE,LKEY,LNUM,IND)                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9019)                               
        GOTO 60                                                                 
C                                                                               
4300    CONTINUE                                                                
        CALL SPESNP(SLINE,IND,NUM,DAT)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,90191)
        GOTO 60                                                                 
C                                                                               
4400    CONTINUE                                                                
        CALL VALSNP(DAT,NUM)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9012)                               
        GOTO 60                                                                 
C                                                                               
C Ticket message snap 
C
4500    CONTINUE     
        CALL MESSNP(NUM)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9059)                            
        GOTO 60                                                                 
C                                                                               
C  Miscellaneous items snap
C
4600    CONTINUE                                                                
C       CALL MISSNP(NUM,DAT,SLINE,LKEY,LNUM) 
C       IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9060)                            
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9070)                            
        GOTO 60                                                                 
C                                                                               
4700    CONTINUE                                                                
        CALL TCFSNP(DAT,NUM)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9012)                               
        GOTO 60                                                                 
C                                                                               
4800    CONTINUE                                                                
        CALL TRASNP(NUM)                                                  
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9018)                               
        GOTO 60                                                                 
C                                                                               
4900    CONTINUE                                                               
        CALL LPLSNP(NUM,IND)                                                   
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9245)                               
        GOTO 60                                                                 
C                                                                               
C Win Reserve Fund snap
C
5000    CONTINUE                                                                
        IF (IND.LT.1)IND=1
C       CALL WRFSNP(IND,SLINE)                                                    
C       IF(LIN23(1).EQ.BLANK) WRITE(CLIN23,9013)                                  
        IF(LIN23(1).EQ.BLANK) WRITE(CLIN23,9070)                            
        GOTO 60                                                                 
C                                                                               
5100    CONTINUE                                                                
        CALL LOGSNP                                                             
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9015)                               
        GOTO 60                                                                 
C                                                                               
5200    CONTINUE                                                                
        CALL COMSNP(NUM)                                                        
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9013)                               
        GOTO 60                                                                 
C                                                                               
5300    CONTINUE                                                                
        CALL NETSNP(SLINE)                                                      
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9051)                               
        GOTO 60                                                                 
C                                                                               
5400    CONTINUE                                                                
        CALL QUESNP(NUM)                                                        
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9015)                               
        GOTO 60                                                                 
C                                                                               
5500    CONTINUE
        IF(IND.LT.1) IND=1                                                      
        CALL HRSSNP(IND)                                                        
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9023)                               
        GOTO 60                                                                 
C                                                                               
5600    CONTINUE                                                                
        CALL SYSSNP(SLINE,IND)                                                  
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9019)                               
        GOTO 60                                                                 
C                                                                               
5700    CONTINUE                                                                
        CALL FLWSNP(SLINE)                                                     
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9051)                               
        IF(LIN23(1).EQ.RECV ) WRITE (CLIN23,9050)                               
        GOTO 60                                                                 
C                                                                               
5800    CONTINUE                                                                
        CALL GTNSNP(SLINE)                                                      
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9007)                               
        GOTO 60                                                                 
C                                                                               
C TSALes
C
5900    CONTINUE                                                                
        CALL TSALSNP(DAT)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9026)                              
        GOTO 60                                                                 
C                                                                               
6000    CONTINUE                                                                
        CALL BUFSNP(NUM)                                                        
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9015)                               
        GOTO 60                                                                 
C                                                                               
6100    CONTINUE                                                                
        CALL SALSNP(DAT,NUM)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9241)                              
        GOTO 60                                                                 
C
6200    CONTINUE
        CALL OVRSNP(1,NUM)
        IF(LIN23(1).EQ.BLANK) WRITE(CLIN23,9024)
        GOTO 60
C
6300    CONTINUE
        CALL OVRSNP(2,NUM)
        IF(LIN23(1).EQ.BLANK) WRITE(CLIN23,9024)
        GOTO 60
6400    CONTINUE 
        CALL TGLSNP(NUM,IND)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9017)
        GOTO 60                                                          
6500    CONTINUE  
        CALL TGNSNP(NUM,IND)                                                
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9017)                               
        GOTO 60	
6600    CONTINUE
	CALL GUISNP(SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE(CLIN23,9007)
        GOTO 60
6700    CONTINUE
	IF (IND.LT.1) IND = 1
	CALL PASSNP(NUM, SLINE,IND, PAG)                                      
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9247)  
        GOTO 60                                                                 
C                                                                               
C PROCESS MENU3 SNAPSHOTS                                                       
C                                                                               
7000    CONTINUE                                                                
        CALL X2COMSNP(SLINE)                                                    
        GOTO 60                                                                 
C                                                                               
7050    CONTINUE                                                                
        CALL X2DLYSNP(SLINE)                                                    
        GOTO 60                                                                 
C                                                                               
7100    CONTINUE                                                                
        CALL LANSNP(NUM,SLINE)                                                        
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9013)                               
        GOTO 60                                                                 
C                                                                               
7150    CONTINUE                                                                
        CALL CONSNP(NUM,SLINE)                                                        
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9013)                               
        GOTO 60                                                                 
C                                                                               
7200    CONTINUE                                                                
        CALL ETHSNP(SLINE)                                                      
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9015)                               
        GOTO 60                                                                 
C                                                                               
7250    CONTINUE                                                                
        CALL SAPSNP(NUM)                                                        
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9013)                               
        GOTO 60                                                                 
C                                                                               
7300    CONTINUE                                                                
        CALL PTLSNP(NUM,SLINE)                                                  
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9013)                              
        GOTO 60                                                                 
C                                                                               
7350    CONTINUE                                                                
        CALL X2ALLSTN(SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9155)                               
        GOTO 60                                                                 
C                                                                              
7400    CONTINUE                                                                
        CALL X2ALLSAP                                                           
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9160)                               
        GOTO 60                                                                 
C                                                                               
7450    CONTINUE                                                                
        CALL X2STNSNP(SLINE)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9170)                               
        GOTO 60                                                                 
C                                                                               
7500    CONTINUE                                                                
        CALL X2SAPSNP                                                           
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9180)                               
        GOTO 60                                                                 
C                                                                               
7550    CONTINUE                                                                
        CALL X2STNPRT(SLINE)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9190)                               
        GOTO 60                                                                 
C                                                                               
7600    CONTINUE                                                                
        CALL X2NETSNP                                                           
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9200)                               
        GOTO 60                                                                 
C                                                                               
7650    CONTINUE                                                                
        CALL X2GBLSNP(SLINE)                                                    
        GOTO 60                                                                 
C                                                                               
7700    CONTINUE                                                                
        CALL X2RELSNP                                                           
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9210)                               
        GOTO 60                                                                 
C                                                                               
7750    CONTINUE                                                                
        CALL X2ALLREL                                                           
        GOTO 60                                                                 
C                                                                               
7800    CONTINUE                                                                
        CALL REQSNP(NUM,IND)           
99999   FORMAT(A16)                                         
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9220)                               
        GOTO 60                                                                 
C
7850    CONTINUE
        CALL  SPSTSNP(NUM,IND)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9054)
        GOTO 60
C
7900    CONTINUE
        CALL NACSNP(PAG,IND,TXT)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9027)                              
        GOTO 60 
C
7950    CONTINUE
        CALL X2AGTSNP(SLINE)
        IF(LIN22(1).EQ.BLANK) WRITE (CLIN22,9230)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9240)
        GOTO 60
C
7960    CONTINUE
        IF(IND.LT.1) IND=1
        CALL BL1SNP (NUM,IND)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9016)
        GOTO 60

7970    CONTINUE
        IF(IND.LT.1) IND=1
        CALL BL2SNP (NUM,IND,SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9016)
        GOTO 60

7980    CONTINUE                                                                
        CALL TWITSNP(DAT)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9012)                              
        GOTO 60                                                                 

7990    CONTINUE                                                                
        CALL TSCRSNP(DAT)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9012)                              
        GOTO 60
        
7991    CONTINUE     
        CALL SNIFSNP(IND,PAG,NUM)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9250)                              
        GOTO 60         
C
C PUT NEXT MENU3 SNAPSHOT HERE
C
7992    CONTINUE                                                                
7993    CONTINUE                                                                
        GOTO 22
C
C PROCESS MENU4 SNAPSHOTS
C
C Super Double game snap
C
8000    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                                      
        CALL SKLSNP(NUM,IND,DAT,SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9242)
        GOTO 60                                                                 

C                                                                               
C Today's Couple game snap
C
8001    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                                      
        CALL PPLSNP(NUM,IND,DAT,SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9242)
        GOTO 60                                                                 
C                                                                               
C Super Double game sales snap
C
8002    CONTINUE                                                                
        CALL TSKSNP(DAT)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9026)                              
        GOTO 60                                                                 
C                                                                               
C Today's Couple game sales snap
C
8003    CONTINUE                                                                
        CALL TPRSNP(DAT)                                                    
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9026)                              
        GOTO 60                                                                 
C                                                                               
C Super Double Odds snap
C
8004    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                               
        CALL SKOSNP(NUM,DAT,IND,SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9061)                              
        GOTO 60
C                                                                               
C Today's Couple Odds snap
C
8005    CONTINUE                                                                
        IF(IND.LT.1) IND=1                                               
        CALL PPOSNP(NUM,DAT,IND,SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9061)                              
        GOTO 60                                                                 
C
C Passive validation  
C
8006    CONTINUE
	IF (IND.LT.1) IND = 1
        CALL PVLSNP(IND,PAG,NUM)
        IF(LIN23(1).EQ.BLANK) WRITE(CLIN23,9250)
	GOTO 60
C
C TCPSNP
C
8007    CONTINUE
        CALL TCPSNP(SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9007)
        GOTO 60
C
C INSSNP
C
8008    CONTINUE
        CALL INSSNP(DAT,SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9012)
        GOTO 60
C
8009    CONTINUE
        CALL DLLSNP(NUM,SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE(CLIN23,9231)
        GOTO 60
C
C X2MON snap
C
8010    CONTINUE
        CALL X2MONSNP(SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE(CLIN23,9015)
        GOTO 60
C
C GTX snap
C
8011    CONTINUE
        CALL X2GTXSNP(SLINE)
        IF(LIN23(1).EQ.BLANK) WRITE(CLIN23,9015)
        GOTO 60
C
C UNUSED                
C
C EURO MIL PROJECT - SNAP EUROMIL
C
C
8012    CONTINUE
CV71        CALL EURMILSNP(NUM,SLINE)
CV71        NUM = 0
CV71        WRITE(CLIN23,999)
        CALL EURMILSNP(SLINE,IND)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,999)                                !V71
C	GOTO 22
        GOTO 60
C
C Super score game snap
C
8013    CONTINUE              ! CONVERT HEX NUMBER INTO FORMAT XXXXXX
        CALL MVESNP(NUM,IND,CMB)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9243)                               
        GOTO 60
C
C Today's Trio game snap
C
8014    CONTINUE
        CALL PTRSNP(NUM,IND,DAT,SLINE)           
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9242)
        GOTO 60
C
C Super score sales snap
C
8015    CONTINUE
        CALL TMVSNP(DAT)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9244)                              
        GOTO 60
C
C Today's TRIO sales snap
C
8016    CONTINUE
        CALL TPTSNP(DAT)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9244)                              
        GOTO 60
C
C Super score odds snap
C
8017    CONTINUE
        CALL MVOSNP(NUM,IND)
        BIGNUM = NUM
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9232)                              
        GOTO 60
C
C Today's TRIO odds snap
C
8018    CONTINUE
        CALL PTOSNP(NUM,DAT,CMB,IND)
        BIGNUM = NUM
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9233)                              
        GOTO 60
C
C PASSIVE INVOICE                                        !V68...
C
8019    CONTINUE
        CALL IVPSNP(NUM,LKEY,LNUM)                                      
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,90081)                       
        GOTO 60                                          !...V68
C
C Super Triple sales
C
8020    CONTINUE
        CALL TTRSNP(DAT)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9244)                              
        GOTO 60
C
C Super Triple odds snapshot
C
8021    CONTINUE
        CALL TROSNP(NUM,DAT,CMB,IND)
        BIGNUM = NUM
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9233)                              
        GOTO 60
C
C Super Triple game snapshot
C
8022    CONTINUE
        CALL TRPSNP(NUM,IND,DAT,SLINE)           
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9242)
        GOTO 60
C
C Available to use             
C
C8023    CONTINUE
C        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9070)           
C        GOTO 60    
C
C V70 - START - PLACARD PROJECT - SNAP IGS
C
C Available to use             
C
8023    CONTINUE
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9260) 
        CALL OLMSNP(SLINE)
CCCCCCCCCCCCCCCCCCCCCC activa a flag que entrou na tela olm
CCCCCCCCCCCCCCCCCCCCCC poderia defenir um array que indicava 
CCCCCCCCCCCCCCCCCCCCCC pelo posição a tela proveniente                  
        GOTO 60          
C
8024    CONTINUE
        CALL IGSSNP(SLINE,XIND)
        NUM = 0
C         WRITE(CLIN23,999)
C         WRITE(CLIN23,9019)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9255)  
C	GOTO 22
        GOTO 60                                                                 
C
C V70 - END - PLACARD PROJECT - SNAP IGS
C
C ICSLOG snapshot
C
C8024    CONTINUE
C	CALL ICSSNP(NUM)
C        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9015)
C        GOTO 60
C
C GTC SNAPSHOT
C
8025    CONTINUE
	CALL GTCSNP(DAT,NUM)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9015)
        GOTO 60
C
C WIN SNAPSHOT (MULTIWIN)
C
8026    CONTINUE
	CALL WINSNP(NUM)
        IF(LIN23(1).EQ.BLANK) WRITE (CLIN23,9059)
        GOTO 60
C
C PUT NEXT MENU4 SNAPSHOT HERE
C (ACTUALLY THERE'S CURRENTLY NO ROOM FOR MORE MENU4 SNAPSHOTS)
8027    CONTINUE
        GOTO 22
C
C
C
9005    FORMAT('Enter #terminal or agent #, date (DD/MM/YYYY)',             
     *         ' !game number or vision command')
9006    FORMAT('Enter line number or vision command')                           
9007    FORMAT('Enter change or vision command')                                
9008    FORMAT('Enter #terminal or agent number, !game number ', 
     *         'or vision command')            
90081   FORMAT('Enter #terminal, agent number or vision command')          !V68
9009    FORMAT('Enter !game index, draw number, or /row number ')               
9010    FORMAT('Enter !game index, /row number or vision command')
9011    FORMAT('Enter help or vision command or menu page number')
9012    FORMAT('Enter number, date (DD/MM/YYYY) or vision command')               
9013    FORMAT('Enter number or vision command')                                
9014    FORMAT('Agent number not found')                                        
9015    FORMAT('Enter vision command')                                          
9016    FORMAT('Enter draw number ')                                            
9017    FORMAT('Enter !game index or draw number')       
90171   FORMAT('Enter !game index or draw number or /year-week')
9018    FORMAT('Enter transaction number, or vision command')                   
9019    FORMAT('Enter change, !game number, or vision command')                 
90191   FORMAT('Enter change, !game number or /page ')
9020    FORMAT('Enter #Terminal, Agent, or /Line')                              
9021    FORMAT('Enter !game number or draw number')                             
9022    FORMAT('Enter Row or !Minimum amount to be displaied')                  
9023    FORMAT('Enter !game number')
9024    FORMAT('Enter amount or vision command')
9025    FORMAT('Enter !game index, /row number, $amount or ',
     *         'vision command')
9026    FORMAT('Enter date (DD/MM/YYYY) or vision command')               
9027    FORMAT('Enter +page#, !ind, \subsnap or vision command')
9246    FORMAT('Enter !game index or draw number or /year-week')
9247    FORMAT('Enter emission#, !game index, +page# or /year-week')
9248    FORMAT('Enter /<year-week> !<gind> @<ticket><serie><tenth> or Vision command')
9050    FORMAT('Only on the master system ')                                    
9051    FORMAT('Enter change or vision command ')                               
9052    FORMAT('Enter !game index or event number')                             
9053    FORMAT('Snapshot requested not available at this',
     *         ' security level')
9054    FORMAT('Enter !game index or number or vision command ')
9055    FORMAT(' VISION SNAPSHOT PRINTED ON ',I2.2,'/',I2.2,'/',I2.2,           
     *         ' AT ',2A4,//)                                                   
9056    FORMAT('Enter !game index, /row number or change')                      
C                                                                               
9058    FORMAT('Enter !game index, draw number, or /race')         
9059    FORMAT('Enter vision command or page number')              
9060    FORMAT('Enter #terminal or agent #, date (DD/MM/YYYY)',             
     *         ' or vision command')
9061    FORMAT('Enter !game index or event number or /rank number')
9070    FORMAT('SORRY SNAPSHOT DISABLED, ENTER VISION COMMAND')    
9155    FORMAT('Enter XSTN, XFOR #, XBAK #, XTOP, XBOT, or XSRT')               
9160    FORMAT('Enter XSAP, XFOR #, XBAK #, XTOP, XBOT, or XSRT')               
9170    FORMAT('Enter XSTN, XPRT, XDRP')                                        
9180    FORMAT('Enter XSAP, XMNT ')                           
9190    FORMAT('Enter XSTN, XPRT, XFOR #, XBAK #, XTOP, XBOT, or XSRT')       
9200    FORMAT('Enter XNET, XFOR #, XBAK #, XTOP, XBOT, or XSRT')               
9210    FORMAT('Enter XREL, XGRP, or XSTN')                                     
9220    FORMAT('Enter !Subnetwork number or Interval number')                            
9230    FORMAT('Enter XSTN, XPRT, XFOR #, XBAK #, XTOP, XBOT, XSRT,',
     *         ' TER, SOFT, PRTS, HARD,')
9231    FORMAT('Enter vision command or subnetwork no')
9232    FORMAT('Enter !game index or page# or N for next or L for previous page')
9233    FORMAT('Enter !game index or /draw or page or N for next or L for previous page')
9240    FORMAT(' BRO4#')
9241    FORMAT('Enter date (DD/MM/YYYY) or page number')               
9242    FORMAT('Enter !game index or event number or /page number')
9243    FORMAT('Enter !game index or event number or score ?XX:XX-XX:XX-XX:XX')
9244    FORMAT('Enter date (DD/MM/YYYY) ')               
9245    FORMAT('Enter !game number or number')
9250    FORMAT('Enter !(game index) +(emission_index) and (NNNNNSSFF) of ticket')
C V70 - Start
9255    FORMAT('Enter change, !igs game number, or vision command')
C V70 - End
9260    FORMAT('Enter change, !OLM, or vision command')             !V72     
C
C EURO MIL PROJECT - SEND MESSAGE
C
CV71 999     FORMAT('Chose option: ')
999     FORMAT('Enter change, !eur game number, or vision command')             !V71
C
        END                                                                     
