C SUBROUTINE TRNSNP1
C
C V51 22-JUL-2021 VCF  PTHG-MTRA PASS THRU ARTE/IPS PROJECT
C
C V50 20-MAR-2014 SCML Placard Project.
C V49 07-JAN-2014 SCML Fix overflow problem in validation last transaction update
C V48 29-NOV-2013 SCML Added NIB/Player Id to Validation Transactions
C V47 16-MAR-2011 GPW NUMAGT=12288
C V46 30-NOV-2010 FJG TWEMSER/TWEMCHK replaced by TWLNKSER/TWLNKCHK
C V45 13-APR-2010 RXK Changes for ePassive 
C                 FJG Changes for ePassive
C V44 20-JUL-2009 FJG Added QP flag in WAGERS
C V38 25-MAR-2009 MMO CHANGED TWEMSER/TWEMCHK JOKER/EM.
C V37 16-MAR-2009 MMO JOKER -- SERIAL OF ASSOCIATED EM.
C V36 06-JUN-2005 FRP Modify for IPS Distribution.
C V35 29-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C V34 01-DEC-2000 UXN TOTOGOLO ADDED.
C V33 08-JUN-2000 UXN TTNAMES.DEF added.
C V32 14-MAR-2000 OXK Layout  fixed
C V31 07-MAR-2000 UXN Fix for promotions.
C V30 28-FEB-2000 RXK Promotion ("add 1 free week") added.
C V29 22-FEB-2000 OXK CBIMAGE passed to SPBET, not BIMAGE (Vakio changes)
C V28 01-FEB-2000 UXN TNFRAC added.
C V27 25-NOV-1999 OXK WAP added to TEBE.
C V26 13-OCT-1999 RXK World Tour added.
C V25 17-MAY-1999 UXN Super Triple added.
C V24 09-SEP-1998 RXK Changed for new Kicker
C V23 22-MAY-1997 UXN VRU/WWW added.
C V22 27-FEB-1997 RXK In game table request max 2 game numbers  
C V21 26-FEB-1997 RXK Display of number of instant tickets fixed 
C                     (ILOT transaction) 
C V20 14-FEB-1997 RXK Various fixes for Instant stuff
C V19 07-FEB-1997 RXK Fix for bank numbers and prize in instant validation
C V18 18-DEC-1996 HXK Update from TEBE project (MXP,WXW,PXN,MJF)
C V17 29-NOV-1996 WXW Telebetting startup, changes MP/PXN/WXW.
C                     Pitka system number set to 0.
C V16 17-MAY-1996 HXK Update from Wojtek, Siew Mun
C V15 13-FEB-1996 HXK Added statistics byte display
C V14 07-JAN-1996 HXK Allow for TREF transaction type
C V13 10-AUG-1995 RXK Couponid and "RANK" added for Ravi
C V12 07-NOV-1994 PXB Added bingo transaction format.
C V11 25-OCT-1994 HXK Display suberr
C V10 14-SEP-1993 GXA Added TCDC_SOLD to Wager display, 'none' to second 
C                     kicker if not played.
C V09 09-SEP-1993 GXA Corrected Bank info. usage for validations, 
C                     Use TVBNK... not TWBNK...
C V08 04-SEP-1993 WXS HTOA _> BCDASC
C V07 04-SEP-1993 WXS ADDRESSING CHANGE FOR X.21
C V06 03-AUG-1993 SXH Added TIMES BET for RAVI (V65)
C V05 03-AUG-1993 SXH Fixed previous comment non-typable character
C V04 02-AUG-1993 SXH Added error exception handler NOCONSIG
C V03 08-JUL-1993 SXH Released for Finland
C V02 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C
C SUBROUTINE TO FORMAT TRANSACTION SCREEN FOR
C VISION TRANSACTION AND CARRYOVER SNAPSHOTS.
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
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE TRNSNP1(TRABUF,BLOCK,INDEX)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:HASF.DEF'
C**     INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:NAMCMD.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:IGSTNAMES.DEF'
        INCLUDE 'INCLIB:TTNAMES.DEF'
        INCLUDE 'INCLIB:X2PTLMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:MXERROR.DEF'			!PTHG-MTRA. V51.
C
        ! arguments
        INTEGER*4  BLOCK                   !
        INTEGER*4  INDEX                   !

        ! variables
        INTEGER*4  CN                      !
        INTEGER*4  CT                      !
        INTEGER*4  S                       !
        INTEGER*4  CNT                     !
        INTEGER*4  LNS                     !
        INTEGER*4  I,J                       !
        INTEGER*4  GIND                    !
        INTEGER*4  GTYP                    !
        INTEGER*4  MIN                     !
        INTEGER*4  HR                      !
        INTEGER*4  SEC                     !
        INTEGER*4  BLANK                   !
        INTEGER*4  BIMAGE(14,12)           !
        INTEGER*4  STNUM /0/               !
        INTEGER*4  STOFF /0/               !
        INTEGER*4  BNKID                   !
        INTEGER*4  BNKNUM                  !
        INTEGER*4  DSPOLD(2),DSPNEW(2)
        INTEGER*4  PLCARD
        INTEGER*4  SPACE/'    '/

        REAL*8     COMMAND /'        '/    !

        LOGICAL    CALLKBET

        INTEGER*4  NIB(6) 
        CHARACTER*24 CNIB
        EQUIVALENCE (NIB,CNIB)
    
        CHARACTER*56 CBIMAGE(12)           !
        CHARACTER    SYST(0:5)*7           !

        CHARACTER*7  KICKER1
        CHARACTER*7  KICKER2

        CHARACTER*1  KIKSTAR1
        CHARACTER*1  KIKSTAR2
        CHARACTER*1  JOKYESNO(0:1)/'N','Y'/

        CHARACTER*1  FREEWK(0:1)/' ','+'/

	CHARACTER*8  IAGT_NO !FUNCTION
        CHARACTER*8  CHRSTR1
        CHARACTER*8  CHRSTR2
        INTEGER*4    ERR
        INTEGER*4    QPIND

        INTEGER*2    DBUF(LDATE_LEN) /LDATE_LEN*0/       !
C
	INTEGER*4    AGTOFF
        INTEGER*4    I4TEMP
        BYTE         I1TEMP(4)
        EQUIVALENCE  (I4TEMP,I1TEMP)
C
        INTEGER*4    NOCONSIG
        EXTERNAL     NOCONSIG

        EQUIVALENCE  (CBIMAGE,BIMAGE)

C V50 - Start

        INTEGER*8  I8TMP
        INTEGER*4  I4TMP(2)
        EQUIVALENCE (I8TMP,I4TMP)
        
        INTEGER*8  WEXSER ! WAGER EXTERNAL SERIAL
        INTEGER*8  CEXSER ! CANCEL EXTERNAL SERIAL
        INTEGER*8  PEXSER ! PAYMENT EXTERNAL SERIAL
        INTEGER*8  TMSGID ! TERMINAL MESSAGE ID
        INTEGER*8  TIGSR_MVR ! MEDIA VERSION

        CHARACTER*10 C10MONY
        
        INTEGER*4 CXERR_LEN
        PARAMETER (CXERR_LEN = 9) ! XERR HAS 9 CHARACTERS
        CHARACTER*12 CXERR ! XERR_I4LEN * 4
        
C V50 - End
C
        DATA      BLANK/'    '/
        DATA      SYST/'   NONE','   FULL','REDUCED',' U-SYS',
     *                 ' CH-SYS','UNKNOWN'/
C
        CHARACTER*14 RETTYP(0:4)

        DATA RETTYP/'--------------','ALL TICKET    ','BY FRACTION    ',
     *              'HALF TICKET   ','QUARTER TICKET'/

        INTEGER*4 TCKS, NUMTCKS,IND
        CHARACTER*4 VALST_PAS(0:20),RETST_PAS(0:20),STR

        DATA VALST_PAS/'NWIN','BSVL','REDP','NWSE','    ','    ','    ',
     *                 'VWIN','APAD','UTKT','RTKT','WEMI','BTCK','BSER',
     *                 'BTEN','PTCK','    ','    ','    ','    ','    '/

        DATA RETST_PAS/'RET ','HRTC','CLEM','NOEM','SLTC','RAFD','    ',
     *                 'VWIN','APAD','UTKT','RTKT','WEMI','BTCK','BSER',
     *                 'BTEN','PTCK','    ','    ','    ','    ','    '/


        INTEGER*4 HR1,MIN1,SEC1
        INTEGER*4 INSMIN(TIVMX)
        INTEGER*4 INSHR(TIVMX)
        INTEGER*4 INSSEC(TIVMX)
C
	INTEGER*4 DLEN,MLEN,DID,OFF
	BYTE      DINF(500)
C
	INTEGER*4 MXSTS					!PTHG-MTRA. V51.
C
        CHARACTER*3  NOYES(0:1)/' No','Yes'/		!PTHG-MTRA. V51.
        CHARACTER*3  YESNO(0:1)/'Yes','No '/	        !PTHG-MTRA. V51.
C
C
C-------->>V48 ---------------------------------------------------------
        CHARACTER*31 EMVALSTATUS(0:18)
        CHARACTER*31 EMVALSTATUSE(0:30)
        INTEGER*4 PLIDTYP
        INTEGER*4  EMVALAMT(2),EMNETVALAMT(2)
        INTEGER*8  EMVALAMTI8, EMNETVALAMTI8
        REAL*8     EMVALAMTR8, EMNETVALAMTR8
        EQUIVALENCE (EMVALAMT, EMVALAMTI8)
        EQUIVALENCE (EMNETVALAMT, EMNETVALAMTI8)
        
        EMVALSTATUS(5)  = '---------------'
        EMVALSTATUS(11) = 'Cashed With Exchange'
        EMVALSTATUS(10) = 'No Exchange Ticket'

        EMVALSTATUSE(0)   = 'No Results Yet Or Not A Winner'
        EMVALSTATUSE(1)   = 'Results Not Confirmed'
        EMVALSTATUSE(2)   = 'No Such Ticket'
        EMVALSTATUSE(3)   = 'Cant Pay Yet'
        EMVALSTATUSE(4)   = 'Already Cashed'
        EMVALSTATUSE(5)   = '---------------'
        EMVALSTATUSE(6)   = 'Prize Expired'
        EMVALSTATUSE(9)   = 'Cash At Lottery'
        EMVALSTATUSE(18)  = 'No Details Available'
        EMVALSTATUSE(30)  = 'Winner Holding Limit'
C-------- V48<<---------------------------------------------------------
C
        CALL LIB$ESTABLISH(NOCONSIG)
C
C CLEAR SCREEN IMAGE
C
        CALL FASTSET(BLANK,NEW(1,1),480)
C
C CALCULATE TIME
C
        SEC =  TRABUF(TTIM)
        HR  =  SEC/3600
        MIN = (SEC-HR*3600)/60
        SEC =  SEC-(HR*3600+MIN*60)
C
C FORMAT TRANSACTION SNAPSHOT
C
        GTYP = TRABUF(TGAMTYP)
        GIND = TRABUF(TGAMIND)
        DBUF(5) = DAYCDC
        CALL LCDATE(DBUF)
        WRITE(CLIN1,901) (DBUF(J),J=7,13)
        IF(GTYP.GE.1.AND.GTYP.LE.MAXTYP) THEN
            IF(TRABUF(TTYP).EQ.TIGS) THEN
              WRITE(CLIN3,9032) TRABUF(TSER),BLOCK,
     *                         GTNAMES(GTYP),GIND,TTNAMES(TRABUF(TTYP)),IGSTTYPE(TRABUF(TIGS_TTYP))
            ELSE
              WRITE(CLIN3,903) TRABUF(TSER),BLOCK,
     *                         GTNAMES(GTYP),GIND,TTNAMES(TRABUF(TTYP))
            ENDIF
        ELSE
            WRITE(CLIN3,9031) TRABUF(TSER),BLOCK,
     *                        TTNAMES(TRABUF(TTYP))
        ENDIF

        IF(TRABUF(TTYP).EQ.TWAG.AND.TRABUF(TWSYST).GT.0)THEN                      
            STOFF = TRABUF(TWSYST)                                                 
            IF(TRABUF(TGAMTYP).EQ.TLTO.OR.TRABUF(TGAMTYP).EQ.TSPT.OR.
     *         TRABUF(TGAMTYP).EQ.TBNG.OR.TRABUF(TGAMTYP).EQ.TTGL) THEN
               STNUM = TRABUF(TWSIMP)
            ELSE
               STNUM = TRABUF(TWSYSN)
            ENDIF                           
        ELSE                                               
            STOFF = 0                                 
            STNUM = 0                  
        ENDIF
C
C       DO NOT DISPLAY SYSTEM NUMBER FOR TOTO SELECT OR SCORE
C
        IF((TRABUF(TGAMTYP).EQ.TSCR).OR.(TRABUF(TGAMTYP).EQ.TTSL)) STNUM = 0
        
        BNKID  = TRABUF(TWBNKID)                                                     
        BNKNUM = TRABUF(TWBNKNM)                                                   
        IF(TRABUF(TTYP).EQ.TSPE) THEN                                             
            BNKID  = 0                                                                 
            BNKNUM = 0                                                                
        ENDIF
C
        PLCARD = 0
        CALL FASTSET(SPACE,NIB,6)

        IF(TRABUF(TTYP).EQ.TVAL .OR. TRABUF(TTYP).EQ.TREF) THEN
           BNKID  = TRABUF(TVBNKID)
           BNKNUM = TRABUF(TVBNKNUM)
           PLCARD = TRABUF(TVPLCARD)  
C-------->>V48 ---------------------------------------------------------
C           IF(TRABUF(TVNIBBB).GT.0) THEN     
	         PLIDTYP = TRABUF(TVPLIDTYP)
           IF(TRABUF(TVTYPE).EQ.VNBNK .OR. TRABUF(TVTYPE).EQ.VPNBNK) THEN
C-------- V48<<---------------------------------------------------------
              WRITE(CNIB,900) TRABUF(TVNIBBB),TRABUF(TVNIBBO),TRABUF(TVNIBBA1),
     *                        TRABUF(TVNIBBA2),TRABUF(TVNIBCD) 
           ENDIF
        ENDIF
C
C-------->>V48 ---------------------------------------------------------
        IF(TRABUF(TTYP).EQ.TEUR .AND. TRABUF(TEUTYP) .EQ. TVAL) THEN
           BNKID  = 0
           BNKNUM = 0
           PLIDTYP = TRABUF(TEUVPLIDTYP)
           PLCARD = TRABUF(TEUVPLCARD)
           IF(TRABUF(TEUVSBT) .EQ. VNBNK) THEN
              WRITE(CNIB,900) TRABUF(TEUVNIBBB),TRABUF(TEUVNIBBO),TRABUF(TEUVNIBBA1),
     *                        TRABUF(TEUVNIBBA2),TRABUF(TEUVNIBCD)
           ENDIF
        ENDIF
C-------- V48<<---------------------------------------------------------
C
        IF( TRABUF(TTYP) .EQ.TPTH  .AND.	      !PTHG-MTRA. V51.
     *      TRABUF(TITYP).EQ.IVAL) THEN
           BNKID  = 0
           BNKNUM = 0
C-------->>V48 ---------------------------------------------------------
           PLIDTYP = TRABUF(TIPLIDTYP)
           PLCARD = TRABUF(TIPLCARD)
           IF(TRABUF(TIVMT) .EQ. IBVMT .AND. TRABUF(TIVALM) .EQ. IVBM_BNK) THEN
              WRITE(CNIB,900) TRABUF(TINIBBB),TRABUF(TINIBBO),TRABUF(TINIBBA1),
     *                        TRABUF(TINIBBA2),TRABUF(TINIBCD)
           ENDIF
C-------- V48<<---------------------------------------------------------
        ENDIF
C
C V50 - Start
        IF(TRABUF(TTYP).EQ.TIGS .AND. TRABUF(TIGS_TTYP).EQ.IGSPAY) THEN
           BNKID  = 0
           BNKNUM = 0
           PLIDTYP = TRABUF(TIGSP_IDTY)
           PLCARD = TRABUF(TIGSP_PYID)
           IF(TRABUF(TIGSP_PMOD) .EQ. IGS_PMBNK) THEN
              WRITE(CNIB,900) TRABUF(TIGSP_NIBB),TRABUF(TIGSP_NIBO),TRABUF(TIGSP_NIA1),
     *                        TRABUF(TIGSP_NIA2),TRABUF(TIGSP_NICD)
           ENDIF
        ENDIF
C V50 - End
C
        I4TEMP = TRABUF(TTSTCS)
C
        WRITE(CLIN4,904) TTYPE(TRABUF(TTYP)),INDEX,
     *                   STAT(TRABUF(TSTAT)),I1TEMP(1)
C
        IF(TRABUF(TTYP).EQ.TPTH) THEN			  !PTHG-MTRA. V51.
           WRITE(CLIN5,9051) TRABUF(TAGT),HR,MIN,SEC,
     *                   ERROR(TRABUF(TERR))
        ELSE
           WRITE(CLIN5,905) TRABUF(TAGT),HR,MIN,SEC,
     *                   ERROR(TRABUF(TERR)),TRABUF(TSUBERR)
        ENDIF

        WRITE(CLIN6,906) TRABUF(TTRN),TRABUF(TTER), SYST(STOFF)
        WRITE(CLIN7,907)  TRABUF(TCDC),TRABUF(TSIZE), STNUM
        IF (TRABUF(TGAMTYP) .EQ. TWIT) THEN
            WRITE(CLIN8,9081) TRABUF(TGAMTYP),TRABUF(TGAMIND),BNKID,
     *      TRABUF(TWWCOUPID)
        ELSEIF (TRABUF(TGAMTYP) .EQ. TDBL) THEN
            WRITE(CLIN8,9081) TRABUF(TGAMTYP),TRABUF(TGAMIND),BNKID,
     *      TRABUF(TWDBCOUPID)
        ELSEIF (TRABUF(TGAMTYP) .EQ. TCPL) THEN
            WRITE(CLIN8,9081) TRABUF(TGAMTYP),TRABUF(TGAMIND),BNKID,
     *      TRABUF(TWCPCOUPID)
        ELSEIF (TRABUF(TGAMTYP) .EQ. TTRP) THEN
            WRITE(CLIN8,9081) TRABUF(TGAMTYP),TRABUF(TGAMIND),BNKID,
     *      TRABUF(TWTTCOUPID)
        ELSEIF (TRABUF(TGAMTYP) .EQ. TSTR) THEN
            WRITE(CLIN8,9081) TRABUF(TGAMTYP),TRABUF(TGAMIND),BNKID,
     *      TRABUF(TWSTCOUPID)
        ELSE
            WRITE(CLIN8,908) TRABUF(TGAMTYP),TRABUF(TGAMIND),(NIB(I),I=1,6)
        ENDIF
C-------->>V48 ---------------------------------------------------------
C       WRITE(CLIN9,909)TRABUF(TTKID),TRABUF(TCHK),PLCARD
        IF(PLIDTYP.EQ.PHONNBR) THEN
          WRITE(CLIN9,913)TRABUF(TTKID),TRABUF(TCHK),PLCARD
        ELSEIF(PLIDTYP.EQ.PLAYCRD) THEN
          WRITE(CLIN9,909)TRABUF(TTKID),TRABUF(TCHK),PLCARD
        ENDIF
C-------- V48<<---------------------------------------------------------
C
C WAGER BODY
C
        IF(TRABUF(TTYP).GE.TWAG.AND.TRABUF(TTYP).LE.TINC) THEN

            KICKER1  = '  none '                                  
            KICKER2  = '  none '                                  
            KIKSTAR1 = ' '                                        
            KIKSTAR2 = ' '         

            CALLKBET = .FALSE. 
            IF(TRABUF(TWKGME).NE.0) THEN
                   KIKSTAR1 = JOKYESNO(TRABUF(TWKFLG)) 
                   WRITE(KICKER1,1023) IAND('FFFFFF'X,TRABUF(TWKICK))
                   KIKSTAR2 = JOKYESNO(TRABUF(TWKFLG2)) 
                   WRITE(KICKER2,1023) IAND('FFFFFF'X,TRABUF(TWKICK2))
            ENDIF                                                   

            CALL FASTSET(BLANK,BIMAGE(1,1),168)
            IF(TRABUF(TGAMTYP).EQ.TLTO) CALL LTBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TPAS) CALL PABET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TSPT) CALL SPBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TTGL) CALL TGBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TTSL) CALL TSBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TWIT) CALL WIBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TSCR) CALL SCBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TBNG) CALL BGBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TDBL) CALL DBBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TCPL) CALL CPBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TSSC) CALL SSBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TTRP) CALL TRBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TSTR) CALL STBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TNBR) CALL NBET(TRABUF,CBIMAGE)
C
            IF(TRABUF(TWQPF).NE.0) THEN
              QPIND = 1
            ELSE
              QPIND = 0
            ENDIF
C
	    IF(TRABUF(TGAMTYP).EQ.TSPT .AND. TRABUF(TWSPFRG).GT.0) THEN
	      IND=TRABUF(TWNBET)+1
              WRITE(CLIN11,1010) TRABUF(TWBEG),CSMONY(TRABUF(TWAMT),8,BETUNIT),
     *                           BIMAGE(1,IND),(BIMAGE(K,IND),K=2,3)
	      CBIMAGE(IND)=' '
	      IND=1
	    ELSE
              WRITE(CLIN11,1011) TRABUF(TWBEG),CSMONY(TRABUF(TWAMT),8,BETUNIT),
     *                          (BIMAGE(K,1),K=1,14)
	      IND=2
	    ENDIF
            WRITE(CLIN12,1012) TRABUF(TWEND),FREEWK(TRABUF(TWADDFW)),
     *                         CMONY(TRABUF(TWTKC),8,BETUNIT),
     *                        (BIMAGE(K,IND+0),K=1,14)
            WRITE(CLIN13,1013) TRABUF(TWDUR),CSMONY(TRABUF(TWKAMT),8,BETUNIT),
     *                        (BIMAGE(K,IND+1),K=1,14)
            WRITE(CLIN14,1014) TRABUF(TWKGME),CSMONY(TRABUF(TWTOT),8,BETUNIT),
     *                        (BIMAGE(K,IND+2),K=1,14)
            WRITE(CLIN15,1015) TRABUF(TWSYST),TRABUF(TWSYSN),
     *                        (BIMAGE(K,IND+3),K=1,14)
            WRITE(CLIN16,1016) TRABUF(TWCTER),TRABUF(TWCSER),
     *                        (BIMAGE(K,IND+4),K=1,14)
            WRITE(CLIN17,1017) TRABUF(TWNBET),VALST(TRABUF(TWVSTS)),
     *                        (BIMAGE(K,IND+5),K=1,14)
            WRITE(CLIN18,1018) TRABUF(TWKBEG),TRABUF(TFRAC),
     *                        (BIMAGE(K,IND+6),K=1,14)
            IF(TRABUF(TGAMTYP).EQ.TKIK) THEN
              WRITE(CLIN19,10191) TRABUF(TWKEND),KIKSTAR1,KICKER1,TRABUF(TWLNKSER),
     *                          (BIMAGE(K,IND+7),K=1,10)
            ELSE
              WRITE(CLIN19,1019) TRABUF(TWKEND),KIKSTAR1,KICKER1,
     *                          (BIMAGE(K,IND+7),K=1,14)
            ENDIF
            WRITE(CLIN20,1020) TRABUF(TWKDUR),KIKSTAR2,KICKER2,
     *                        (BIMAGE(K,IND+8),K=1,14)
            WRITE(CLIN21,1021) TRABUF(TCDC_SOLD),JOKYESNO(QPIND),(BIMAGE(K,IND+9),K=1,14)
            WRITE(CLIN22,1022) (BIMAGE(K,IND+10),K=1,14)
            RETURN
        ENDIF

	IF (TRABUF(TTYP).EQ.TRET.OR.TRABUF(TTYP).EQ.TVAL.AND.GTYP.EQ.TPAS) THEN
	    IF (TRABUF(TPOFFTER).GT.0) THEN
	       AGTOFF = AGTTAB(AGTNUM,TRABUF(TPOFFTER))
	    ELSE
	       AGTOFF = 0
	    ENDIF
	    WRITE(CLIN10,910) RETTYP(TRABUF(TPRETYP)),IAGT_NO(AGTOFF),TRABUF(TPFRCNT)

	    WRITE(CLIN11,911) 
	    NUMTCKS = TRABUF(TPTCK)

	    IND = 12
            DO TCKS = 1, NUMTCKS
               IF (TRABUF(TTYP).EQ.TRET) THEN
                 STR = RETST_PAS(TRABUF(TPSTS1  + OFFTRA*(TCKS-1)))
               ELSE
                 STR = VALST_PAS(TRABUF(TPSTS1  + OFFTRA*(TCKS-1)))
               ENDIF
               WRITE(XNEW(IND),912) TRABUF(TPNUM1  + OFFTRA*(TCKS-1)),
     *                              TRABUF(TPEMIS1 + OFFTRA*(TCKS-1)),
     *                              TRABUF(TPSER1  + OFFTRA*(TCKS-1)),
     *                              TRABUF(TPTEN1  + OFFTRA*(TCKS-1)),
     *                              STR,
     *                              CMONY(TRABUF(TPPAY1  + OFFTRA*(TCKS-1)),10,VALUNIT),
     *                              99999-TRABUF(TPKEY1  + OFFTRA*(TCKS-1))
	       IND = IND + 1
	    ENDDO   

	ENDIF
C
C VALIDATION BODY
C
        IF( (TRABUF(TTYP).EQ.TVAL .OR. TRABUF(TTYP).EQ.TREF) .AND. 
     *       TRABUF(TGAMTYP).NE.TPAS) THEN
            WRITE(CLIN10,3010) VALST(TRABUF(TVSTS)),
     *                         CMONY(TRABUF(TVPAY),11,VALUNIT)

C
C         FOR CDC VALIDATIONS FORMAT IS DIFFERENT
C
            IF(TRABUF(TVCDC).GE.10000) THEN
                WRITE(CLIN11,3014) TRABUF(TVCDC)-10000,TRABUF(TVSER)
            ELSE
                WRITE(CLIN11,3011) TRABUF(TVCDC),TRABUF(TVSER)
            ENDIF

            WRITE(CLIN12,3012) TRABUF(TVKGME),CMONY(TRABUF(TVKPAY),11,VALUNIT)
            WRITE(CLIN13,3013) TRABUF(TVCODE),TRABUF(TVEXC)

            RETURN
        ENDIF
C
C EUROMILLION VALIDATION BODY
C
        IF(TRABUF(TTYP).EQ.TEUR .AND. TRABUF(TEUTYP) .EQ. TVAL) THEN
          EMVALAMT(2) = TRABUF(TEUVCAMH)
          EMVALAMT(1) = TRABUF(TEUVCAM)
          EMVALAMTR8  = DFLOAT(EMVALAMTI8)/100.0D0
C
          EMNETVALAMT(2) = TRABUF(TEUVRAMH)
          EMNETVALAMT(1) = TRABUF(TEUVRAM)
          EMNETVALAMTR8  = DFLOAT(EMNETVALAMTI8)/100.0D0
C
          WRITE(CLIN11,3015)  'VAL '
          WRITE(CLIN12,30151) TRABUF(TEUVSBT)
          IF(TRABUF(TEUVSBT) .NE. 15) THEN
            WRITE(CLIN13,30152) EMVALSTATUS(TRABUF(TEUVST))
          ELSE
            WRITE(CLIN13,30152) EMVALSTATUSE(TRABUF(TEUVST))
          ENDIF
          WRITE(CLIN14,30153) TRABUF(TEUSER)
          WRITE(CLIN15,30154) TRABUF(TEUMESSQ)
          WRITE(CLIN16,30155) TRABUF(TEUVWJUL)
          WRITE(CLIN17,30156) TRABUF(TEUVWSER)
          WRITE(CLIN18,30157) TRABUF(TEUVTIMEH),TRABUF(TEUVTIMEM),TRABUF(TEUVTIMES)
          WRITE(CLIN19,30158) EMVALAMTR8
C----+------------------------------------------------------------------
C V49| Fix overflow problem in validation last transaction update
C----+------------------------------------------------------------------
          IF(TRABUF(TEUVRAMH).NE.0 .OR. TRABUF(TEUVRAM).NE.0) THEN
C----+------------------------------------------------------------------
C V49| Fix overflow problem in validation last transaction update
C----+------------------------------------------------------------------
            WRITE(CLIN20,30159) EMNETVALAMTR8
          ELSE
            WRITE(CLIN20,30159) EMVALAMTR8 
          ENDIF
C
          RETURN
        ENDIF
        
C
C SPECIAL FUNCTION BODY
C
        IF(TRABUF(TTYP).EQ.TSPE) THEN
            IF(TRABUF(TSFUN).EQ.TSX2X) THEN
                WRITE(CLIN11,6000) TRABUF(TXIDX)
                WRITE(CLIN12,6010) TRABUF(TXLAY)
                WRITE(CLIN13,6020) X2X_PTLMES(TRABUF(TXPTL))
                WRITE(CLIN14,6030) TRABUF(TXSTN)
                WRITE(CLIN15,6040) TRABUF(TXSAP)
                IF(TRABUF(TXLAY).EQ.1) THEN
                    WRITE(CLIN16,6050) TRABUF(TXTFEID)
                    WRITE(CLIN17,6060) TRABUF(TXTDSAP)
                    WRITE(CLIN18,6070) TRABUF(TXTBTYP)
                ELSE IF(TRABUF(TXLAY).EQ.2) THEN
                    WRITE(CLIN16,6080) TRABUF(TXFPID), TRABUF(TXFSSAP)
                    WRITE(CLIN17,6090) TRABUF(TXFMDUT)
                    CALL BCDASC(CHRSTR1,1,8,TRABUF(TXFDAD1),ERR)
                    CALL BCDASC(CHRSTR2,1,8,TRABUF(TXFDAD2),ERR)
C****               WRITE(CLIN18,7000) TRABUF(TXFDAD1), TRABUF(TXFDAD2)
                    WRITE(CLIN19,7010) TRABUF(TXFLFID), TRABUF(TXFLMC)  !DATA
                ELSE IF(TRABUF(TXLAY).EQ.3) THEN
                    WRITE(CLIN16,7020) TRABUF(TXSPID)
                    WRITE(CLIN17,7030) TRABUF(TXSSDTU)
                    WRITE(CLIN18,7040) TRABUF(TXSCC), TRABUF(TXSSNUM)   !DATA
                ENDIF
	    ELSEIF(TRABUF(TSFUN).EQ.TAGTINF) THEN
	        WRITE(CLIN10,9000) TRABUF(TSNEW)
	        OFF = 1
		DO I=1,MIN0(12,TRABUF(TSNEW))
	           DLEN = 0
	           DID  = 0
                   CALL MOVBYT(TRABUF(TSDT1),OFF,DID,1,1)    ! 1 bytes of ID
                   OFF = OFF + 1
                   CALL MOVBYT(TRABUF(TSDT1),OFF,DLEN,1,1)   ! 1 bytes of lenght
                   OFF = OFF + 1
                   CALL MOVBYT(TRABUF(TSDT1),OFF,DINF,1,DLEN) 
                   OFF = OFF + DLEN
		   MLEN = MIN0(DLEN,40)
		   WRITE(XNEW(10+I),9001) I, FLDNAM(DID), DLEN, 
     *                                    (DINF(J),J=1,MLEN)
	        ENDDO
            ELSE
                WRITE(CLIN11,4011) SPFUN(TRABUF(TSFUN))
                WRITE(CLIN12,4012) TRABUF(TSOLD)
                WRITE(CLIN13,4013) TRABUF(TSNEW)

                LNS=13
                CNT=0
                DO S=TSDT1,TSDT7
                    LNS=LNS+1
                    CNT=CNT+1
                    WRITE(XNEW(  LNS),4014) CNT,TRABUF(S)
                END DO
            ENDIF

            RETURN
        ENDIF
C
C COMMAND BODY
C
        IF(TRABUF(TTYP).EQ.TCMD) THEN
            CT=TRABUF(TCMTYP)
            CN=TRABUF(TCMNUM)
            IF(CT.EQ.TCPAR) COMMAND = NAMPAR(CN)
            IF(CT.EQ.TCGEN) COMMAND = NAMGEN(CN)
            IF(CT.EQ.TCSPE) COMMAND = NAMSPE(CN)
            IF(CT.EQ.TCNET) COMMAND = NAMNET(CN)
            IF(CT.EQ.TCCOM) COMMAND = NAMCOM(CN)
            IF(CT.EQ.TCAGT) COMMAND = NAMAGT(CN)
            IF(CT.EQ.TCLTO) COMMAND = NAMLTO(CN)
            IF(CT.EQ.TCSPT) COMMAND = NAMSPT(CN)
            IF(CT.EQ.TCTGL) COMMAND = NAMTGL(CN)
            IF(CT.EQ.TCPAS) COMMAND = NAMPAS(CN)
            IF(CT.EQ.TCKIK) COMMAND = NAMKIK(CN)
            IF(CT.EQ.TCSCR) COMMAND = NAMSCR(CN)
            IF(CT.EQ.TCWIT) COMMAND = NAMWIT(CN)
            IF(CT.EQ.TCTSL) COMMAND = NAMTSL(CN)
            IF(CT.EQ.TCBNG) COMMAND = NAMBNG(CN)
            IF(CT.EQ.TCDBL) COMMAND = NAMDBL(CN)
            IF(CT.EQ.TCCPL) COMMAND = NAMCPL(CN)
            IF(CT.EQ.TCSSC) COMMAND = NAMSSC(CN)
            IF(CT.EQ.TCTRP) COMMAND = NAMTRP(CN)
            IF(CT.EQ.TCSTR) COMMAND = NAMSTR(CN)
C
            IF(CT.EQ.TCPAR.AND.(CN.EQ.SUPGWA.OR.CN.EQ.SUPGCA.
     *         OR.CN.EQ.SUPGVA.OR.CN.EQ.SUPRPT)) THEN
               DSPOLD(1)=JISHFT(TRABUF(TCMOLD),-1)
               IF(IAND(TRABUF(TCMDT1),1).EQ.1)
     *            DSPOLD(1)=IOR(DSPOLD(1),'80000000'X)
               DSPOLD(2)=JISHFT(TRABUF(TCMDT1),-1)
               DSPNEW(1)=JISHFT(TRABUF(TCMNEW),-1)
               IF(IAND(TRABUF(TCMDT2),1).EQ.1) 
     *            DSPNEW(1)=IOR(DSPNEW(1),'80000000'X)
               DSPNEW(2)=JISHFT(TRABUF(TCMDT2),-1)
               WRITE(CLIN11,5011)  NAMTYP(CT),COMMAND
               WRITE(CLIN12,50121) TRABUF(TCMLIN),DSPOLD(2),DSPOLD(1)
               WRITE(CLIN13,50131) TRABUF(TCMTER),DSPNEW(2),DSPNEW(1)
               WRITE(CLIN14,5014)  TRABUF(TCMSRC)
            ELSEIF(CT.GT.0) THEN 
               WRITE(CLIN11,5011) NAMTYP(CT),COMMAND
               WRITE(CLIN12,5012) TRABUF(TCMLIN),TRABUF(TCMOLD)
               WRITE(CLIN13,5013) TRABUF(TCMTER),TRABUF(TCMNEW)
               WRITE(CLIN14,5014) TRABUF(TCMSRC)
            ENDIF
            RETURN
        ENDIF
C
C V51... PTHG-MTRA ARTE/IPS PROJECT
C
C PASS THROUGH TRANSACTION (EX-INSTANT) BODY
C
        IF(TRABUF(TTYP).EQ.TPTH) THEN			  !PTHG-MTRA. V51.
           MXSTS = TRABUF(TPMXSTAT)
           IF(MXSTS.GT.6) MXSTS = 6

           WRITE(CLIN11,8000) TTYPE(TRABUF(TPTRATYP)),STAT(TRABUF(TPSTATUS)),
     *                        ERROR(TRABUF(TPERCODE)),TRABUF(TPMESLEN),
     *                        TRABUF(TPESTESN),TRABUF(TPCHKSUM)

           WRITE(CLIN12,8010) TRABUF(TPJULIAN), TRABUF(TPSERIAL),
     *                        TRABUF(TPTRNCHK), TRABUF(TPTRANID),
     *                        MXERROR(MXSTS)
C
C
C SPECIAL FUNCTIONS
C
          IF(TRABUF(TPTRATYP).EQ.TSPE) THEN

            IF(TRABUF(TPSFUN).EQ.TSON) THEN
              WRITE(CLIN14,12935) SPFUN(TRABUF(TPSFUN)),TRABUF(TPHARDREV1)
              WRITE(CLIN15,12936) TRABUF(TPTSOFTREV),   TRABUF(TPHARDREV2)
              WRITE(CLIN16,12938) NOYES(TRABUF(TPATREQSON))
              WRITE(CLIN17,12939) YESNO(TRABUF(TPTSONESTE))
            ENDIF

C
C INSTANTS TRANSACTIONS
C
          ELSE IF( TRABUF(TPTRATYP).EQ.TITS) THEN

            WRITE(CLIN13,8020) ITYPE(TRABUF(TITYP)), TRABUF(TIERR), 
     *                               TRABUF(TIXRF),  TRABUF(TAGT)
C
C
            IF(TRABUF(TITYP).EQ.IVAL) THEN
                WRITE(CLIN14,8030) (TRABUF(TIGAM1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(CLIN15,8040) (TRABUF(TIPCK1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(CLIN16,8050) (TRABUF(TIVRN1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(CLIN17,8060) (TRABUF(TILTX1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                DO K=1,TRABUF(TIBCH)
                    INSSEC(K)=TRABUF(TITIM1+K-1)
                    INSHR(K)=INSSEC(K)/3600
                    INSMIN(K)=(INSSEC(K)-INSHR(K)*3600)/60
                    INSSEC(K)=INSSEC(K)-(INSHR(K)*3600+INSMIN(K)*60)
                END DO
                WRITE(CLIN18,8070) (INSHR(K),INSMIN(K),INSSEC(K),
     *                              K=1,TRABUF(TIBCH))
                WRITE(CLIN19,8080) (TRABUF(TICDC1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(CLIN20,8090) (TRABUF(TISTS1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(CLIN21,8100) (CMONY(TRABUF(TIPRZ1+K),10,1),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(CLIN22,8091) (TRABUF(TIPCKSTS1+K),
     *                              K=0,TRABUF(TIBCH)-1)
C
            ELSE IF(TRABUF(TITYP).EQ.IISS) THEN
                WRITE(CLIN14,8510) TRABUF(TIREP)
                WRITE(CLIN15,8520) TRABUF(TINUM)

C V51... PTHG-MTRA PASS THRU ARTE/IPS PROJECT.
C V51... PTLA-16100.
C
                IF( TRABUF(TINUM).LE.5 )THEN
C
                  WRITE(CLIN16,8530) (TRABUF(TIGAM+K),
     *                                K=0,TRABUF(TINUM)-1)
                  WRITE(CLIN17,8550) (TRABUF(TIPCK+K),
     *                                K=0,TRABUF(TINUM)-1)
                  WRITE(CLIN18,8560) (TRABUF(TIRES+K),
     *                                K=0,TRABUF(TINUM)-1)
C
                ELSE  !(TINUM).GT.5
C
C V51... PTHG-MTRA Display first 5 Books: 
                  WRITE(CLIN16,8530) (TRABUF(TIGAM+K),K=0,5-1)
                  WRITE(CLIN17,8550) (TRABUF(TIPCK+K),K=0,5-1)
                  WRITE(CLIN18,8560) (TRABUF(TIRES+K),K=0,5-1)
C
C V51... PTHG-MTRA Display rest of the Books:
                  WRITE(CLIN19,8530) (TRABUF(TIGAM+K),
     *                                K=5,TRABUF(TINUM)-1)
                  WRITE(CLIN20,8550) (TRABUF(TIPCK+K),
     *                                K=5,TRABUF(TINUM)-1)
                  WRITE(CLIN21,8560) (TRABUF(TIRES+K),
     *                                K=5,TRABUF(TINUM)-1)
C
                ENDIF !IF( TRABUF(TINUM).LE.5 )
C
C ...V51 PTHG-MTRA PASS THRU ARTE/IPS PROJECT.
C
            ELSE IF(TRABUF(TITYP).EQ.ILOT) THEN
                WRITE(CLIN14,8120) TRABUF(TLREP)
                WRITE(CLIN15,8130) TRABUF(TLCLS)
                WRITE(CLIN16,8140) TRABUF(TLGAM)
                WRITE(CLIN17,8160) TRABUF(TLPCK)
                WRITE(CLIN18,8170) TRABUF(TLEND) - TRABUF(TLSTR)
                WRITE(CLIN20,8190) CSMONY(TRABUF(TLAMT),10,1)
                WRITE(CLIN21,8200) CSMONY(TRABUF(TLCOM),10,1)
C
            ELSE IF(TRABUF(TITYP).EQ.ICAR) THEN
                WRITE(CLIN14,8120) TRABUF(TCREP)
                WRITE(CLIN15,8130) TRABUF(TCCLS)
                WRITE(CLIN16,8140) TRABUF(TCGAM)
                WRITE(CLIN17,8161) TRABUF(TCCAR)
                WRITE(CLIN18,8171) TRABUF(TCEND) - TRABUF(TCSTA)
                WRITE(CLIN20,8191) TRABUF(TCCNT)
C
            ELSE IF(TRABUF(TITYP).EQ.IQTA.OR.
     *              TRABUF(TITYP).EQ.IINV.OR.
     *              TRABUF(TITYP).EQ.ISET) THEN
                WRITE(CLIN14,8240) TRABUF(TRGAM)
                WRITE(CLIN15,8250) TRABUF(TRCLS)
                WRITE(CLIN16,8260) TRABUF(TRNXT1)
                WRITE(CLIN17,8270) TRABUF(TRNXT2)
C
            ELSE IF(TRABUF(TITYP).EQ.IFIN) THEN
                WRITE(CLIN14,8290) TRABUF(TRTYP)
                WRITE(CLIN15,8280) TRABUF(TRSUB)
                WRITE(CLIN16,8296) TRABUF(TRCHN)
                WRITE(CLIN17,8297) TRABUF(TRCON1)
                WRITE(CLIN18,8298) TRABUF(TRCON2)
C
            ELSE IF(TRABUF(TITYP).EQ.IORD) THEN
                WRITE(CLIN14,8380) TRABUF(TGPGAM)
                WRITE(CLIN15,8260) TRABUF(TGPNXT)
		WRITE(CLIN16,8601) CSMONY(TRABUF(TGPRCL),10,1)
C
            ELSE IF(TRABUF(TITYP).EQ.IMNU) THEN
                WRITE(CLIN14,8315) (IAND(TRABUF(TSGAM+K),'00000FFF'X),K=0,9)
                WRITE(CLIN15,8315) (IAND(TRABUF(TSGAM+K),'00000FFF'X),K=10,19)
                WRITE(CLIN16,8315) (IAND(TRABUF(TSGAM+K),'00000FFF'X),K=20,29)
                WRITE(CLIN17,8315) (IAND(TRABUF(TSGAM+K),'00000FFF'X),K=30,39)
                WRITE(CLIN18,8320) (TRABUF(TSQTY+K),K=0,9)
                WRITE(CLIN19,8320) (TRABUF(TSQTY+K),K=10,19)
                WRITE(CLIN20,8320) (TRABUF(TSQTY+K),K=20,29)
                WRITE(CLIN21,8320) (TRABUF(TSQTY+K),K=30,39)
C
            ELSE IF(TRABUF(TITYP).EQ.ICNF) THEN
                WRITE(CLIN14,8210) TRABUF(TIINV1)
                WRITE(CLIN15,8220) TRABUF(TIINV2)
                WRITE(CLIN16,8230) TRABUF(TIINV3)
C
            ELSE IF(TRABUF(TITYP).EQ.IOACT) THEN
                WRITE(CLIN14,8235) TRABUF(TOINV1)
                WRITE(CLIN15,8220) TRABUF(TOINV2)
                WRITE(CLIN16,8230) TRABUF(TOINV3)
C
            ELSE IF(TRABUF(TITYP).EQ.IEST) THEN
                WRITE(CLIN14,8350) TRABUF(TISFT)
                WRITE(CLIN15,8360) TRABUF(TIRSTFLG)
                SEC1=TRABUF(TIRSTTIM)
                HR1=SEC1/3600
                MIN1=(SEC1-HR1*3600)/60
                SEC1=SEC1-(HR1*3600+MIN1*60)
                WRITE(CLIN16,8361) HR1,MIN1,SEC1
                WRITE(CLIN17,8362) TRABUF(TIMINCB)
                WRITE(CLIN18,8363) TRABUF(TICHKSUM)
                WRITE(CLIN19,8364) TRABUF(TIPHONE1_1), TRABUF(TIPHONE1_2),
     *                             TRABUF(TIPHONE1_3)
                WRITE(CLIN20,8365) TRABUF(TIPHONE2_1), TRABUF(TIPHONE2_2),
     *                             TRABUF(TIPHONE2_3)
C
            ELSE IF(TRABUF(TITYP).EQ.IGTB) THEN
                WRITE(CLIN14,8370) TRABUF(TIGMC)
                WRITE(CLIN15,8380) TRABUF(TIGMN+0)
                WRITE(CLIN16,8380) TRABUF(TIGMN+1)
                WRITE(CLIN17,8380) TRABUF(TIGMN+2)
                WRITE(CLIN18,8380) TRABUF(TIGMN+3)
                WRITE(CLIN19,8380) TRABUF(TIGMN+4)
C
            ELSE IF(TRABUF(TITYP).EQ.ISON) THEN
                WRITE(CLIN14,8390) TRABUF(TIGVT1),TRABUF(TIGVT2)
C
            ELSE IF(TRABUF(TITYP).EQ.IFSESON) THEN
                WRITE(CLIN14,8401) TRABUF(TIFSETYP)
                WRITE(CLIN15,8402) TRABUF(TIFSERSLT)
             IF ((TRABUF(TIFSETYP).EQ.0).AND.
     +          (TRABUF(TIFSERSLT).EQ.INOER)) THEN !SUCCESSFUL FSE SIGN-ON WITH
                WRITE(CLIN16,8403) TRABUF(TIFSEREP),
     *                            (TRABUF(TIFSENAMS+K),K=0,5)
                WRITE(CLIN17,8404) TRABUF(TIFSECLS)
                WRITE(CLIN18,8405) TRABUF(TIFSEOFF)
             ELSE
               WRITE(CLIN16,8403) TRABUF(TIFSEREP)
               WRITE(CLIN17,8404) TRABUF(TIFSECLS)
             ENDIF
C
            ENDIF !IF(TRABUF(TITYP).EQ.IVAL) THEN	      !PTHG-MTRA. V51.
C
          ENDIF   !IF(TRABUF(TPTRATYP).EQ.TSPE) THEN	      !PTHG-MTRA. V51.
        ENDIF     !IF(TRABUF(TTYP).EQ.TPTH) THEN              !PTHG-MTRA. V51.
        RETURN 
C
C V50 - Start
C
C IGS BODY
C
        IF(TRABUF(TTYP).EQ.TIGS) THEN

C WAGER
            IF(TRABUF(TIGS_TTYP).EQ.IGSWAG) THEN
            
              I4TMP(2) = TRABUF(TIGSW_MIDH)
              I4TMP(1) = TRABUF(TIGSW_MIDL)
              TMSGID = I8TMP
        
              I4TMP(2) = TRABUF(TIGSW_WRSH)
              I4TMP(1) = TRABUF(TIGSW_WRSL)
              WEXSER = I8TMP
        
              ! MESSAGE ID / NIF
              WRITE(CLIN11,8701) TMSGID, TRABUF(TIGSW_PNIF)
        
              WRITE(CLIN12,8710) TRABUF(TIGSW_XGID) ! ABP GAME ID
              WRITE(CLIN13,8720) TRABUF(TIGSW_STID) ! SUBTYPE ID
        
              ! UNIT STAKE OF THE BET
              WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSW_USTK),10,BETUNIT)
              WRITE(CLIN14,8730) TRIM(ADJUSTL(C10MONY)) 
        
              WRITE(CLIN15,8740) TRABUF(TIGSW_TBET) ! TOTAL BETS/NUMBER OF SELECTIONS (MAX = 8)
        
              WRITE(CLIN16,8750) TRABUF(TIGS_XREF) ! MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
        
              ! IGS ERROR CODE DESCRIPTION
C              IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUF(TERR) .EQ. NOER)) THEN
C                WRITE(CLIN17,8761)
C              ELSE
                WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1)
                WRITE(CLIN17,8760) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), CXERR
C              ENDIF

              ! WAGER BET REFERENCE
              WRITE(CLIN18,8770) TRABUF(TIGSW_WRDY),
     *                   TRABUF(TIGSW_WRDM),
     *                   TRABUF(TIGSW_WRDD),
     *                   TRABUF(TIGSW_WRGM),
     *                   WEXSER,
     *                   '***'

              ! BET CREATION DATETIME
              WRITE(CLIN19,8780) TRABUF(TIGSW_WCDY),
     *                   TRABUF(TIGSW_WCDM),
     *                   TRABUF(TIGSW_WCDD),
     *                   TRABUF(TIGSW_WCTH),
     *                   TRABUF(TIGSW_WCTM),
     *                   TRABUF(TIGSW_WCTS)
     
              ! BET LAST EVENT DATE
              WRITE(CLIN20,8790) TRABUF(TIGSW_LEDY),
     *                   TRABUF(TIGSW_LEDM),
     *                   TRABUF(TIGSW_LEDD)
     
              ! BET TOTAL STAKE
              WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSW_TSTK),10,BETUNIT)
              WRITE(CLIN21,8800) TRIM(ADJUSTL(C10MONY))
        
              ! BET MAXIMUM POSSIBLE RETURNS
              WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSW_MAXR),10,BETUNIT)
              WRITE(CLIN22,8810) TRIM(ADJUSTL(C10MONY)) ! 
              
C CANCEL
            ELSEIF(TRABUF(TIGS_TTYP).EQ.IGSCAN) THEN
            
              I4TMP(2) = TRABUF(TIGSC_MIDH)
              I4TMP(1) = TRABUF(TIGSC_MIDL)
              TMSGID = I8TMP
        
              I4TMP(2) = TRABUF(TIGSC_WRSH)
              I4TMP(1) = TRABUF(TIGSC_WRSL)
              WEXSER = I8TMP
        
              WRITE(CLIN11,8700) TMSGID ! MESSAGE ID

              ! CANCEL BET REFERENCE
              WRITE(CLIN12,8770) TRABUF(TIGSC_WRDY),
     *                   TRABUF(TIGSC_WRDM),
     *                   TRABUF(TIGSC_WRDD),
     *                   TRABUF(TIGSC_WRGM),
     *                   WEXSER,
     *                   '***'
     
              WRITE(CLIN13,8750) TRABUF(TIGS_XREF) ! MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
        
              ! IGS ERROR CODE DESCRIPTION
!              IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUF(TERR) .EQ. NOER)) THEN
!                WRITE(CLIN14,8761)
!              ELSE
                WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1)
                WRITE(CLIN14,8760) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), CXERR
!              ENDIF

              I4TMP(2) = TRABUF(TIGSC_CRSH)
              I4TMP(1) = TRABUF(TIGSC_CRSL)
              CEXSER = I8TMP
        
             ! CANCEL REFERENCE
             WRITE(CLIN16,8830) TRABUF(TIGSC_CRDY),
     *                   TRABUF(TIGSC_CRDM),
     *                   TRABUF(TIGSC_CRDD),
     *                   TRABUF(TIGSC_CRGM),
     *                   CEXSER,
     *                   '***'

              ! CANCEL CREATION DATETIME
              WRITE(CLIN16,8840) TRABUF(TIGSC_WCDY),
     *                   TRABUF(TIGSC_WCDM),
     *                   TRABUF(TIGSC_WCDD),
     *                   TRABUF(TIGSC_WCTH),
     *                   TRABUF(TIGSC_WCTM),
     *                   TRABUF(TIGSC_WCTS)
     
              ! CANCEL AMOUNT (WAGER UNITS)
              WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSC_CAMT),10,BETUNIT)
              WRITE(CLIN17,8850) TRIM(ADJUSTL(C10MONY))
            
C VALIDATION (INQUIRY)
            ELSEIF(TRABUF(TIGS_TTYP).EQ.IGSVAL) THEN
              I4TMP(2) = TRABUF(TIGSV_MIDH)
              I4TMP(1) = TRABUF(TIGSV_MIDL)
              TMSGID = I8TMP
        
              I4TMP(2) = TRABUF(TIGSV_WRSH)
              I4TMP(1) = TRABUF(TIGSV_WRSL)
              WEXSER = I8TMP
        
              WRITE(CLIN11,8700) TMSGID ! MESSAGE ID
        
              ! VALIDATION BET REFERENCE
              WRITE(CLIN12,8770) TRABUF(TIGSV_WRDY),
     *                   TRABUF(TIGSV_WRDM),
     *                   TRABUF(TIGSV_WRDD),
     *                   TRABUF(TIGSV_WRGM),
     *                   WEXSER,
     *                   '***'
     
              WRITE(CLIN13,8750) TRABUF(TIGS_XREF) ! MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
        
              ! IGS ERROR CODE DESCRIPTION
C              IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUF(TERR) .EQ. NOER)) THEN
C                WRITE(CLIN14,8761)
C              ELSE
                WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1)
                WRITE(CLIN14,8760) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), CXERR
C              ENDIF

              ! WAGER VALIDATION DATETIME
              WRITE(CLIN15,8870) TRABUF(TIGSV_WVDY),
     *                   TRABUF(TIGSV_WVDM),
     *                   TRABUF(TIGSV_WVDD),
     *                   TRABUF(TIGSV_WVTH),
     *                   TRABUF(TIGSV_WVTM),
     *                   TRABUF(TIGSV_WVTS)
     
              ! TOTAL PRIZE AMOUNT
              WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSV_TPRZ),10,VALUNIT)
              WRITE(CLIN16,8880) TRIM(ADJUSTL(C10MONY))
        
              ! TOTAL TAX AMOUNT
              WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSV_TTAX),10,VALUNIT)
              WRITE(CLIN17,8890) TRIM(ADJUSTL(C10MONY))
        
              ! NET PRIZE AMOUNT
              WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSV_NPRZ),10,VALUNIT)
              WRITE(CLIN18,8900) TRIM(ADJUSTL(C10MONY))
        
              ! PAYMENT MODE
              IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUF(TERR) .EQ. NOER)) THEN
                WRITE(CLIN19,8910) VALPAYMODE(TRABUF(TIGSV_PMOD))
              ELSE
                WRITE(CLIN19,8911)
              ENDIF
              
              !PORTUGUESE PLAYER VAT IDENTIFICATION CONFIRMATION NEEDED FLAG
              WRITE(CLIN20,8702) TRABUF(TIGSV_FNIF)
              
              !PORTUGUESE PLAYER VAT IDENTIFICATION NUMBER (9 DIGITS)
              WRITE(CLIN20,8703) TRABUF(TIGSV_PNIF)
              
C PAYMENT
            ELSEIF(TRABUF(TIGS_TTYP).EQ.IGSPAY) THEN
              I4TMP(2) = TRABUF(TIGSP_MIDH)
              I4TMP(1) = TRABUF(TIGSP_MIDL)
              TMSGID = I8TMP
        
              I4TMP(2) = TRABUF(TIGSP_WRSH)
              I4TMP(1) = TRABUF(TIGSP_WRSL)
              WEXSER = I8TMP
        
              WRITE(CLIN11,8700) TMSGID ! MESSAGE ID
        
              ! PAYMENT BET REFERENCE
              WRITE(CLIN12,8770) TRABUF(TIGSP_WRDY),
     *                   TRABUF(TIGSP_WRDM),
     *                   TRABUF(TIGSP_WRDD),
     *                   TRABUF(TIGSP_WRGM),
     *                   WEXSER,
     *                   '***'

              WRITE(CLIN13,8910) VALPAYMODE(TRABUF(TIGSP_PMOD)) ! PAYMENT MODE

              IF(TRABUF(TIGSP_PMOD).EQ.IGS_PMBNK) THEN
                CALL FASTSET(BLANK,NIB,6)
                ! PLAYER NIB
                WRITE(CNIB,8943) TRABUF(TIGSP_NIBB),
     *                    TRABUF(TIGSP_NIBO),
     *                    TRABUF(TIGSP_NIA1),
     *                    TRABUF(TIGSP_NIA2),
     *                    TRABUF(TIGSP_NICD)
                IF(TRABUF(TIGSP_IDTY).EQ.IGS_PTYPPHN) THEN 
                ! PLAYER ID IS PHONE NUMBER
                  WRITE(CLIN14,8941) TRABUF(TIGSP_PYID), (NIB(I),I=1,6)
C                  WRITE(PUNIT,9271) (NIB(I),I=1,6)
                ELSEIF(TRABUF(TIGSP_IDTY).EQ.IGS_PTYPCRD) THEN
                  ! PLAYER ID IS CARD NUMBER
                  WRITE(CLIN14,8942) TRABUF(TIGSP_PYID), (NIB(I),I=1,6)
C                  WRITE(PUNIT,9271) (NIB(I),I=1,6)
                ELSE
                  WRITE(CLIN14,8944) TRABUF(TIGSP_PYID), (NIB(I),I=1,6)
                ENDIF
              ELSE
                WRITE(CLIN14,8945)
              ENDIF
        
              WRITE(CLIN15,8750) TRABUF(TIGS_XREF) ! MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
        
              ! IGS ERROR CODE DESCRIPTION
C              IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUF(TERR) .EQ. NOER)) THEN
C                WRITE(CLIN16,8761)
C              ELSE
                WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1)
                WRITE(CLIN16,8760) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), CXERR
C              ENDIF

              I4TMP(2) = TRABUF(TIGSP_PRSH)
              I4TMP(1) = TRABUF(TIGSP_PRSL)
              PEXSER = I8TMP
        
              ! PAYMENT REFERENCE
              WRITE(CLIN17,8930) TRABUF(TIGSP_PRDY),
     *                   TRABUF(TIGSP_PRDM),
     *                   TRABUF(TIGSP_PRDD),
     *                   TRABUF(TIGSP_PRGM),
     *                   PEXSER,
     *                   '***'
        
              ! PRIZE PAYMENT DATETIME
              WRITE(CLIN18,8950) TRABUF(TIGSP_PPDY),
     *                   TRABUF(TIGSP_PPDM),
     *                   TRABUF(TIGSP_PPDD),
     *                   TRABUF(TIGSP_PPTH),
     *                   TRABUF(TIGSP_PPTM),
     *                   TRABUF(TIGSP_PPTS)
        
              ! TOTAL PRIZE AMOUNT
              WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSP_TPRZ),10,VALUNIT)
              WRITE(CLIN19,8880) TRIM(ADJUSTL(C10MONY))
        
              ! TOTAL TAX AMOUNT
              WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSP_TTAX),10,VALUNIT)
              WRITE(CLIN20,8890) TRIM(ADJUSTL(C10MONY))
        
              ! NET PRIZE AMOUNT
              WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSP_NPRZ),10,VALUNIT)
              WRITE(CLIN21,8900) TRIM(ADJUSTL(C10MONY))
              
              !PORTUGUESE PLAYER VAT IDENTIFICATION NUMBER (9 DIGITS)
              WRITE(CLIN22,8703) TRABUF(TIGSP_PNIF)
            
C GAME PROGRAMME REPORT
            ELSEIF(TRABUF(TIGS_TTYP).EQ.IGSREP) THEN
              I4TMP(2) = TRABUF(TIGSV_MIDH)
              I4TMP(1) = TRABUF(TIGSV_MIDL)
              TMSGID = I8TMP
        
              WRITE(CLIN11,8700) TMSGID ! MESSAGE ID
        
              WRITE(CLIN12,8960) TRABUF(TIGSR_SEGN) ! SEGMENT NUMBER REQUESTED
        
              WRITE(CLIN13,8970) TRABUF(TIGSR_MEID) ! MEDIA ID
        
              WRITE(CLIN14,8980) TRABUF(TIGSR_PTID) ! PROGRAMME TEMPLATE ID
        
              I4TMP(2) = TRABUF(TIGSR_MVRH)
              I4TMP(1) = TRABUF(TIGSR_MVRL)
              TIGSR_MVR = I8TMP
              WRITE(CLIN15,8985) TIGSR_MVR ! MEDIA VERSION
              
              WRITE(CLIN16,8750) TRABUF(TIGS_XREF) ! MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
        
              ! IGS ERROR CODE DESCRIPTION
C              IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUF(TERR) .EQ. NOER)) THEN
C                WRITE(CLIN16,8761)
C              ELSE
                WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1)
                WRITE(CLIN17,8760) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), CXERR
C              ENDIF

              ! PROGRAMME REPORT DATETIME
              WRITE(CLIN18,8990) TRABUF(TIGSR_PRDY),
     *                   TRABUF(TIGSR_PRDM),
     *                   TRABUF(TIGSR_PRDD),
     *                   TRABUF(TIGSR_PRTH),
     *                   TRABUF(TIGSR_PRTM),
     *                   TRABUF(TIGSR_PRTS)
     
              WRITE(CLIN19,8995) TRABUF(TIGSR_TSEG) ! TOTAL SEGMENTS
            
            ENDIF
            
          RETURN
        ENDIF  
C
        RETURN
C V50 - End
C
900     FORMAT(I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)
901     FORMAT(1X,'Transaction Snapshot For ',7A2)
903     FORMAT(1X,'Ser ',I9,' Block ',I8,9X,A8,1X,I2,2X,A8)
9031    FORMAT(1X,'Ser ',I9,' Block ',I8,9X,A8,2X,' transaction')
9032    FORMAT(1X,'Ser ',I9,' Block ',I8,9X,A8,1X,I2,2X,A8,2X,A13)
904     FORMAT(1X,'Type     ',A4,' Index ',I8,9X,
     *             'Status      ',6X,A4,1X,'Statistics ',Z2.2)
905     FORMAT(1X,'Agt  ',I8,' Time',I4.2,':',I2.2,':',I2.2,
     *         8X,' Error       ',6X,A4,1X,Z8.8)
9051    FORMAT(1X,'Agt  ',I8,' Time',I4.2,':',I2.2,':',I2.2,
     *         8X,' Error       ',6X,A4)
906     FORMAT(1X,'Seq      ',I4,' Term  ',I8,9X,'System Type ',3X,A7)
907     FORMAT(1X,'Cdc      ',I4,' Size  ',I8,9X,'System Number',5X,I4)
9071    FORMAT(1X,'Cdc      ',I4,' Size  ',I8,9X,'System Number',5X,I4,
     *         1X,'RANK')
908     FORMAT(1X,'Gamtyp   ',I4,' Gamind',I8,9X,'NIB        ',2X,6A4)
9081    FORMAT(1X,'Gamtyp   ',I4,' Gamind',I8,9X,'Bank ID    ',3X,I8.8,
     *         1X,'Couponid  'I3)
C-------->>V48 ---------------------------------------------------------
C909     FORMAT(1X,'Tktid    ',I4,' Chksum',I8,9X,'Player Card',2X,I4)
909     FORMAT(1X,'Tktid    ',I4,' Chksum',I8,9X,'Player Card ',2X,I10)
913     FORMAT(1X,'Tktid    ',I4,' Chksum',I8,9X,'Player Phone',2X,I10)
C-------- V48<<---------------------------------------------------------
910	FORMAT(1X,'Tra. Type ',A14,2X,'Agt. Off. ',A8,6X,'Total Tickets OK: ',I4)
911	FORMAT(1X,' Ticket    Extr.    Serie    Frac.     Status     Amount    Cntrl')
912	FORMAT(2X,I5.5,4X,I4,7X,I2.2,7X,I2.2,8X,A4,2X,A10,4X,I5.5)
9091    FORMAT(1X,'Tktid    ',I4,' Chksum',I8,9X,'Bank Account ',1X,I8.8,
     *         1X,'Times Bet ',I3)
C
1010    FORMAT('Beg  ',I4,' Amt  ',A8,X,A4,'SUPER 14: ',2A4)
1011    FORMAT('Beg  ',I4,' Amt  ',A8,X,14A4)
1012    FORMAT('End  ',I4,A1,'Tktc ',A8,X,14A4)
1013    FORMAT('Dur  ',I4,' J amt',A8,X,14A4)
1014    FORMAT('J gme',I4,' Total',A8,X,14A4)
1015    FORMAT('Systp',I4,' Sys# ',I8,X,14A4)
1016    FORMAT('Cter',I5,' Cser', I9,X,14A4)
1017    FORMAT('# bet',I4,' Vstat',4X,A4,1X,14A4)
1018    FORMAT('J beg',I4,' Fract',4X,I4,1X,14A4)
1019    FORMAT('J end',I4,' Jok1:',A1,A7,1X,14A4)
10191   FORMAT('J end',I4,' Jok1:',A1,A7,1X,'Linkd:',I10,10A4)
1020    FORMAT('J dur',I4,' Jok2:',A1,A7,1X,14A4)
1021    FORMAT('Sold ',I4,' QP:  ',A1,8X,14A4)
1022    FORMAT(24X,14A4)
1023    FORMAT(I7.7)
C
3010    FORMAT(1X,'Vstat  ',A4,' Pay',A11)
3011    FORMAT(1X,'Vcdc   ',I4,' Vser ',I9)
3012    FORMAT(1X,'Jokgam ',I4,' Kpy',A11)
3013    FORMAT(1X,'Vcode',I6,' Eser ',I9)
3014    FORMAT(1X,'V(CDC*)',I4,' Vser ',I9)
C
C-------->>V48 ---------------------------------------------------------
3015    FORMAT(1X,'Type      ',1X,A4)
30151   FORMAT(1X,'Subtype   ',1X,I4)
30152   FORMAT(1X,'Vstat     ',1X,A31)
30153   FORMAT(1X,'EM Ser#   ',4X,I10)
30154   FORMAT(1X,'MsgQue#   ',4X,I10)
30155   FORMAT(1X,'Vjul      ',10X,I4)
30156   FORMAT(1X,'Vser      ',5X,I9)
30157   FORMAT(1X,'Time      ',6X,I2.2,':',I2.2,':',I2.2)
30158   FORMAT(1X,'Prize     ',F14.2)
30159   FORMAT(1X,'Net Prize ',F14.2)
C-------- V48<<---------------------------------------------------------
C
4011    FORMAT(1X,'Function',3X,A4)
4012    FORMAT(1X,'Old    ',Z8)
4013    FORMAT(1X,'New    ',Z8)
4014    FORMAT(1X,'Data',I1,2X,Z8)
C
5011    FORMAT(1X,'Typ ',A8,' Cmd   ',A8)
5012    FORMAT(1X,'Line    ',I4,' Old   ',I10)
50121   FORMAT(1X,'Line    ',I4,' Old   ',2Z8.8)
5013    FORMAT(1X,'Ter    ',I5,' New   ',I10)
50131   FORMAT(1X,'Ter    ',I5,' New   ',2Z8.8)
5014    FORMAT(1X,'Source  ',A4)
C
6000    FORMAT(1X,'PTL index:......',I8)
6010    FORMAT(1X,'Layer:..........',I8)
6020    FORMAT(1X,'PTL code:.......',A40)
6030    FORMAT(1X,'Station:........',I8)
6040    FORMAT(1X,'SAP:............',I8)
6050    FORMAT(1X,'FE id:..........',I8)
6060    FORMAT(1X,'DSAP:...........',I8)
6070    FORMAT(1X,'Buffer type:....',I8)
6080    FORMAT(1X,'Protocol id:....',I8,2X,'Host id:..........',I8)
6090    FORMAT(1X,'Mess data unit:.',I8)
C****7000       FORMAT(1X,'Address 1.......',Z8,2X,'Address 2.........',Z8)
7000    FORMAT(1X,'Address 1.......',A8,2X,'Address 2.........',A8)
7010    FORMAT(1X,'Data:...........',Z8,2X,'Data:.............',Z8)
7020    FORMAT(1X,'Protocol id:....',I8)
7030    FORMAT(1X,'Stn data unit...',I8)
7040    FORMAT(1X,'Data:...........',Z8,2X,'Data:.............',Z8)
C
C
C V51... PTHG-MTRA ARTE/IPS PROJECT
C
C 8000    FORMAT(1X,'Type    ',1X,A4)
C 8010    FORMAT(1X,'Error   ',1X,I10)
C 8020    FORMAT(1X,'Crs Ref#',1X,I10,2X,'Retailer#',1X,I7.7)
C
8000    FORMAT(1X,'ARTE TrTyp: ',A4,1X,'TrSta: ',A4,1X,'TrErr: ',A4,
     *         1X,'Mes len: ',2X,I3.3,1X,'Seq: ',I5,1X,'Chk: ',I5)
!ARTE TrTyp: AAAAXTrSta: AAAAXTrErr: AAAAXMes len : XXiiiXSeq: iiiiiXChk: iiiii
C
8010    FORMAT(1X,'ARTE ser#: ',I4.4,'-',I8.8,'-',I3.3,1X,'TrID#: ',I15,
     *         1X,'MXClnt: ',A19)
!ARTE ser#: iiii-iiiiiiii-iiiXTrID#: iiiiiiiiiiiiiiiXMXClnt: AAAAAAAAAAAAAAAAAA*
C
8020    FORMAT(1X,'Type    ',1X,A4, 1X,'Error   ', 1X,I10,
     *         1X,'Crs Ref#',1X,I10,2X,'Retailer#',1X,I7.7)
!TypebbbbXAAAA XErrorbbbXiiiiiiiiii XCrs Ref#XiiiiiiiiiiXXRetailer#Xiiiiiii
C
C *SPECIAL FUNCTION - SIGNON*
12935   FORMAT(X,'SPE Function   : ', 1X, A4,  3X,'Term Hardware ID 1:   ', Z8)
12936   FORMAT(X,'Term Soft rev  : ', 1X, Z4.4,3X,'Term Hardware ID 2:   ', Z8)
12938   FORMAT(X,'Auto Sign-On   : ', 2X, A3)
12939   FORMAT(X,'Sign-On On ESTE: ', 2X, A3)
C
C ...V51 PTHG-MTRA ARTE/IPS PROJECT
C
C
8030    FORMAT(1X,'Game #  ',1X,7(I10.2))
8040    FORMAT(1X,'Book #  ',1X,7(I10.7))
8050    FORMAT(1X,'Virn    ',1X,7(I10))
8060    FORMAT(1X,'Latex   ',1X,7(I10))
8070    FORMAT(1X,'Time    ',1X,7(2X,I2.2,':',I2.2,':',I2.2))
8080    FORMAT(1X,'Cdc     ',1X,7(I10))
8090    FORMAT(1X,'Result  ',1X,7(I10))
8091    FORMAT(1X,'Pack sts',1X,7(I10))
8100    FORMAT(1X,'Prize   ',1X,7(A10))
C
8120    FORMAT(1X,'Concessionnaire #  ',1X,I10)
8130    FORMAT(1X,'Instant class #    ',1X,I2.2)
8140    FORMAT(1X,'Instant game #     ',I3.3)
8160    FORMAT(1X,'Instant book #     ',1X,I7.7)
8161    FORMAT(1X,'Instant carton #   ',1X,I7.7)
8170    FORMAT(1X,'Instant # tickets  ',1X,I10)
8171    FORMAT(1X,'Instant # packs    ',1X,I10)
8180    FORMAT(1X,'Instant end #      ',1X,I10)
8190    FORMAT(1X,'Instant amount     ',1X,A10)
8191    FORMAT(1X,'Instant pack count ',1X,I10)
8200    FORMAT(1X,'Instant commission ',1X,A10)
C
8210    FORMAT(1X,'Instant invoice #  ',1X,I10)
8220    FORMAT(1X,'Instant invoice #  ',1X,I10)
8230    FORMAT(1X,'Instant invoice #  ',1X,I10)
8235    FORMAT(1X,'Instant pack count ',1X,I10)
C
8240    FORMAT(1X,'Instant game #     ',1X,I10)
8250    FORMAT(1X,'Instant class #    ',1X,I10)
8260    FORMAT(1X,'Instant next game  ',1X,I10)
8270    FORMAT(1X,'Instant next book  ',1X,I10)
C
8280    FORMAT(1X,'Instant report clss',1X,I10)
8290    FORMAT(1X,'Instant report type',1X,I10)
8296    FORMAT(1X,'Instant chain #    ',1X,I10)
8297    FORMAT(1X,'Instant next game  ',1X,I10)
8298    FORMAT(1X,'Instant next book  ',1X,I10)
C
8315    FORMAT(1X,'Instant game     ',10(1X,I5))
8320    FORMAT(1X,'Instant quantity ',10(1X,I5))
C
8350    FORMAT(1X,'Instant revision                  ',A4)
8360    FORMAT(1X,'Instant GVT Reset Flag              ',I2)
8361    FORMAT(1X,'Instant GVT Reset Time        ',2(I2.2,':'),I2.2)
8362    FORMAT(1X,'Instant Minutes to call back     ',I5)
8363    FORMAT(1X,'Instant Checksum Flag         ',Z8)
8364    FORMAT(1X,'Instant Phone No. 1       ',3A4)
8365    FORMAT(1X,'Instant Phone No. 2       ',3A4)
C
8370    FORMAT(1X,'Instant class      ',1X,I10)
8380    FORMAT(1X,'Instant game number',1X,I10)
C
8390    FORMAT(1X,'Instant gvt id     ',1X,Z8.8,Z8.8)
8391    FORMAT(1X,'Instant agt no     ',1X,I7.7)
8392    FORMAT(1X,'Instant pass no    ',1X,I4.4)
8393    FORMAT(1X,'Instant class      ',1X,I2)
8394    FORMAT(1X,'Instant action     ',1X,I2)
8395    FORMAT(1X,'Instant dummy stn  ',1X,I4.4)
8396    FORMAT(1X,'Instant real  stn  ',1X,I4.4)
8397    FORMAT(1X,'Instant install sts',1X,I1)
C
8401    FORMAT(1X,'Signon/off',7X,I2)
8402    FORMAT(1X,'Result',3X,I10)
8403    FORMAT(1X,'FSE   ',3X,I10,1X,6A4)
8404    FORMAT(1X,'Class ',3X,I10)
8405    FORMAT(1X,'Office',3X,I10)
C
8510    FORMAT(1X,'Concessionnaire #  ',1X,I7.7)
8520    FORMAT(1X,'Number of books    ',1X,I10)
8530    FORMAT(1X,'Game #  ',1X,5(I7,x))
8550    FORMAT(1X,'Book #  ',1X,5(I7,x))
8560    FORMAT(1X,'Status  ',1X,5(I7,x))
C
8601    FORMAT(1X,'Instant credit lim ',1X,A10)
C
9000    FORMAT(1X,'Online Agent Update. ', I2,' items:')
9001    FORMAT(1X,I2,' - ',A8,1X,'(',I2,' bytes) ','"',<MLEN>A1,'"')
C
C V50 - Start
C WAGER
8700    FORMAT(1X,'Terminal Message ID             ',I0)
8701    FORMAT(1X,'Terminal Message ID             ',I0,'   NIF ',I9)
8702    FORMAT(1X,'NIF Confirmation Needed Flag    ',I0)
8703    FORMAT(1X,'NIF                             ',I9)
8710    FORMAT(1X,'ABP Game ID                     ',I0)
8720    FORMAT(1X,'Subtype ID                      ',I0)
8730    FORMAT(1X,'Unit Stake of the Bet           ',A)
8740    FORMAT(1X,'Number of Bets                  ',I0)
8750    FORMAT(1X,'MsgQ Reference #                ',I0)
C8760    FORMAT(1X,'Error Code                      ',A,' - ',A<CXERR_LEN>)
8760    FORMAT(1X,'Error Code                      ',A,A<CXERR_LEN>)
C8761    FORMAT(1X,'Error Code')
8770    FORMAT(1X,'Bet External Reference Serial # ',I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',A3)
8780    FORMAT(1X,'Bet Creation Datetime           ',I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)
8790    FORMAT(1X,'Bet Last Event Date             ',I4.4,'/',I2.2,'/',I2.2)
8800    FORMAT(1X,'Bet Total Stake                 ',A)
8810    FORMAT(1X,'Bet Maximum Possible Returns    ',A)
C CANCEL
8830    FORMAT(1X,'Cancel External Ref Serial #    ',I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',A3)
8840    FORMAT(1X,'Cancel Datetime                 ',I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)
8850    FORMAT(1X,'Cancel Amount                   ',A)
C VALIDATION
8870    FORMAT(1X,'Validation Datetime             ',I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)
8880    FORMAT(1X,'Total Prize Amount              ',A)
8890    FORMAT(1X,'Total Tax Amount                ',A)
8900    FORMAT(1X,'Net Prize Amount                ',A)
8910    FORMAT(1X,'Payment Mode                    ',A)
8911    FORMAT(1X,'Payment Mode')
C PAYMENT
8930    FORMAT(1X,'Payment External Ref Serial #   ',I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',A3)
8940    FORMAT(I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)
C8941    FORMAT(1X,'Phone Number                    ',I0)
8941    FORMAT(1X,'Phone Number                    ',I0,'  NIB  ',6A4)
C8943    FORMAT(1X,'NIB                             ',6A4)
C8942    FORMAT(1X,'Player Card                     ',I0)
8943    FORMAT(I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)
8942    FORMAT(1X,'Player Card                     ',I0,'  NIB  ',6A4)
8944    FORMAT(1X,'Player Id (Type Unknown)        ',I0,'  NIB  ',6A4)
8945    FORMAT(1X,'Player Id / NIB                 ','n/a')
8950    FORMAT(1X,'Payment Datetime                ',I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)
C GAME PROGRAMME REPORT
8960    FORMAT(1X,'Segment Number                  ',I0)
8970    FORMAT(1X,'Media Id                        ',I0)
8980    FORMAT(1X,'Programme Template Id           ',I0)
8985    FORMAT(1X,'Media Version                   ',I0)
8990    FORMAT(1X,'Programme Report Datetime       ',I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)
8995    FORMAT(1X,'Total Segments                  ',I0)
C V50 - End
C
        END
C
C End of TRNSNP1.FOR
C -------------------------------------------------------------------////
