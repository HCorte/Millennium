C
C SUBROUTINE SYSSNP
C
C
C SYSSNP.FOR
C
C V37 01-NOV-2010 FJG  Loto2 Batch: RFSS#0163 PRETTIM
C V36 01-JAN-2010 FJG  ePASSIVE
C V35 11-APR-2001 UXN  TMF file size fixed.
C V34 25-JUL-2000 UXN  Value 5 enabled for P(MESLOG)
C V33 04-FEB-1999 UXN  Fix for big sales.
C V32 05-DEC-1996 HXK  Updated for Finland IPS
C V31 14-SEP-1995 RXK  Refunds added (RFSS #95220)
C V30 05-MAY-1995 HXK  V5 entered into database again!!!!
C V29 21-FEB-1995 PXB  Added MAXV5 and WRNV5 to snapshot.
C V28 02-SEP-1994 HXK  Merge of May,June RFSS batch 
C V27 09-MAY-1994 HXK  CHANGED INCORRECT COMMENT.
C V26 17-APR-1994 HXK  ADDED REMOTE LOG SUPPRESSION
C V25 08-MAR-1994 JXP  Increased TSMXLI display length 
C V24 06-MAR-1994 HXK  ENLARGED TSMXLI, TSMXOD FIELDS.
C V23 05-MAR-1994 JXP  Changed input unit for TSMXOD
C V22 04-MAR-1994 HXK  ADDED TSMXLI.
C V21 25-FEB-1994 HXK  PITKA LIABILITY LIMITATIONS CHANGE.
C V20 18-FEB-1994 JXP  Include TSMXodd parameter for oodds liability limit
C V19 14-OCT-1993 SXH  Added TVTIME parameter
C V18 28-OCT-1993 GXA  Added CANDRW parameter.
C V17 23-SEP-1993 GXA  Customer does not like to input Amounts using the $ sign.
C                      All amount parameters are assumed to be inputted in 
C                      full amounts. Added some DOLVAL calls to REDMAX, 
C                      REDMIN... type parameters.
C V16 26-AUG-1993 SXH  WE ENTER MAXV65, WRNV65, REDMAX AND REDMIN IN UNITS AND
C                      DISPLAY IN MARKS
C V15 16-AUG-1993 HXN  Add REDMIN field to System snaphot.
C V14 06-AUG-1993 HXK  RENAMED MAXSPD TO MAXPPP, MXSPED TO AVOID CONFUSION
C V13 16-JUL-1993 SXH  Added commands MAXSPD,MAXSPT,SCNINT,VSCREEN,WRNV65,MAXV65
C V12 13-JUN-1993 HXK  added AGTINF.DEF
C V11 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V10 08-JUL-1992 GCAN ADDED TOTO SELECT LIABILITY WARNNG AND 
C                      POOL SUPPRESSION PARAMETERS.
C V09 26-FEB-1992 GCAN ADDED TV NEWS MESSAGE UPDATE PARAMETER.(TVNUPD)
C V08 27-JAN-1992 GCAN ADDED SENDING OF TV NEWS MESSAGES TO TV CONTROLLER.
C V07 02-DEC-1991 GCAN RELEASED FOR THE NETHERLANDS
C                      ADDED ODSUPD,JAKUPD,REDMIN,REDMAX PARS. AND REFUNDS.
C V06 02-APR-1991 WS   REMOVED ENCQUE AND DECDONE QUEUES
C V05 08-MAR-1991 JPJ  INITIAL RELEASE FOR MARYLAND
C V04 08-MAR-1991 TKO  ADDED % FULL FOR TMF
C V03 15-FEB-1991 WOL  USES NEW CHARACTER CMONY ROUTINES
C V02 30-JAN-1991 KWP  USE X2X OUTPUT QUEUE
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C
C SYSTEM CONTROL SNAPSHOT
C
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE SYSSNP(CLINE,GAM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:X2XQUE.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
C
C
        ! arguments
        INTEGER*4  CLINE(20)                     !
        INTEGER*4  GAM                           !

        ! variables
        INTEGER*4  TMFSIZE/-1/                   !
        INTEGER*4  SVAL                          !
        INTEGER*4  SCAN                          !
        INTEGER*4  SWAG                          !
        INTEGER*4  SRET                          !        
        INTEGER*4  AVGLIF                        !
        INTEGER*4  MAXLIF                        !
        INTEGER*4  MINLIF                        !
        INTEGER*4  BOUT                          !
        INTEGER*4  BINP                          !
        INTEGER*4  BAVAIL                        !
        INTEGER*4  BDIS                          !
        INTEGER*4  VALDOL                        !
        INTEGER*4  DOLVAL                        !
        INTEGER*4  BSOFT                         !
        INTEGER*4  BGAME_OUT                     !
        INTEGER*4  VALIDS                        !
        INTEGER*4  VOIDS                         !
        INTEGER*4  WAGERS                        !
        INTEGER*4  RETURNS                       !
        INTEGER*4  ST                            !
        INTEGER*4  BITMAP(2)                     !
        INTEGER*4  I                             !
        INTEGER*4  KEYNUM                        !
        INTEGER*4  POS                           !
        INTEGER*4  GIND                          !
        INTEGER*4  GTYP                          !
        INTEGER*4  BUF(CDLEN)                    !
        INTEGER*4  TLINE(20)                     !
        INTEGER*4  GBUF                          !
        INTEGER*4  TEMP                          !
        INTEGER*4  VALUE                         !

        REAL*8    PCNT/0.0/

        INTEGER*4 MAXPRM
        PARAMETER (MAXPRM=61)
        REAL*8    K(MAXPRM)                      !

        CHARACTER*9 GAMNAM                       !
        CHARACTER   CTEMP(4)                     !
        CHARACTER   CHRLIN(80)                   !

        EQUIVALENCE(TEMP,CTEMP)
        EQUIVALENCE(CHRLIN(1),TLINE(1))
C
        DATA   K/'cmdflu  ','LOGBlo  ','TAPesw  ','DISKsw  ',
     *           'cmdfrz  ','CHKtim  ','SUPCOm  ','STAPesw ',
     *           'DPTtim  ','MAXTra  ','SYSSts  ','ROMrev  ',
     *           'REDMAx  ','CHKPnt  ','DESFlg  ','CANtim  ',
     *           'SUPWAg  ','SUPCAn  ','SUPVAl  ','SUPSPe  ',
     *           'SUPFIl  ','SUPSUm  ','SUPPUd  ','SUPSYn  ',
     *           'REDMIn  ','TVNupd  ','WRNper  ','TVNEWS  ',
     *           'MAXMes  ','MESlog  ','ODSupd  ','JAKupd  ',
     *           'TSPMin  ','TSLIab  ','TSLWrn  ','TSPPer  ',
     *           'avgtim  ','mintim  ','maxtim  ','SUPTSp  ',
     *           'TSTLim  ','TSLMax  ','MAXSpt  ','CANDRW  ',
     *           'PRETTIM ','TSMXLI  ','TSMXOD  ','SUPREM  ',
     *           'SUPINs  ','SUPFPt  ','FPTTIm  ','GVTrev  ',
     *           'PCANTIM ','PMAXRTM ','PMAXCAN ','PMAXLOP ',
     *           'PMAXSEK ','PASTHRO ','PDAYRSL ','SUPRET  ',
     *           'RETCOM  '/
C
C On first call, open TMF to get size for % full calculation
C
	IF(PCNT.GT.100) TMFSIZE = -1
C
        IF(SFSIZES(PTMF).GT.0) THEN
           TMFSIZE = SFSIZES(PTMF)*2
           TMFSIZE = (TMFSIZE/(DBLOCK/64))*LBLK    !MAX SERIAL #
        ELSEIF(TMFSIZE.EQ.-1.AND.SFNAMES(1,PTMF).NE.0)THEN
            TMFSIZE = 0
            CALL OPENW(1,SFNAMES(1,PTMF),0,0,0,ST)
            IF(ST.EQ.0)THEN
                CALL GETFSIZ(1, TMFSIZE)
                TMFSIZE = (TMFSIZE/(DBLOCK/64))*LBLK    !MAX SERIAL #
            ENDIF
	    CLOSE(1)
        ENDIF
C
C GET GAME NUMBER
C
        IF(GAM.LT.1.OR.GAM.GT.MAXGAM) THEN
            GAM=0
        ELSE
            GTYP=GNTTAB(GAMTYP,GAM)
            GIND=GNTTAB(GAMIDX,GAM)
            IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) THEN
                WRITE(CLIN23,923) GAM
                RETURN
            ENDIF

            WRITE (GAMNAM,1600) GTNAMES(GTYP),GIND
        ENDIF
C
C SYSSNP INPUT
C
        VALUE = 0
        TEMP  = 0
        POS   = 1
        CALL KEY(CLINE,K,MAXPRM,POS,KEYNUM)
        IF(POS.GT.40 .AND. KEYNUM.EQ.14) GOTO 2
C
        IF(POS.GT.40) GOTO 300                     !NO INPUT
        IF(KEYNUM.EQ.0)GOTO 200                    !INPUT ERROR
        IF(KEYNUM.EQ.12.OR.KEYNUM.EQ.52) THEN
            DO I=1,20
                TLINE(I)=CLINE(I)
            END DO

            CTEMP(1)=CHRLIN(POS)
            CTEMP(2)=CHRLIN(POS+1)
            CTEMP(3)=CHRLIN(POS+2)
            CTEMP(4)=CHRLIN(POS+3)
            VALUE=TEMP

            IF(CTEMP(1).LT.'0'.OR.CTEMP(1).GT.'F') GOTO 205
            IF(CTEMP(2).LT.'0'.OR.CTEMP(2).GT.'F') GOTO 205
            IF(CTEMP(3).LT.'0'.OR.CTEMP(3).GT.'F') GOTO 205
            IF(CTEMP(2).EQ.'@'.OR.CTEMP(3).EQ.'@') GOTO 205
            GOTO 2
        ENDIF
        CALL NUMB(CLINE,POS,VALUE)                 !GET VALUE
        IF(VALUE.LT.0)  GOTO 205
C
C CLEAR COMMAND MESSAGE BUFFER
C
2       CONTINUE
        CALL FASTSET(0,BUF,CDLEN)
        GOTO(200,010,015,020,
     *       200,025,030,035,
     *       040,045,050,055,
     *       060,065,070,075,
     *       080,085,090,095,
     *       100,105,110,111,
     *       125,126,127,128,
     *       115,120,131,132,
     *       133,134,135,136,
     *       200,200,200,140,
     *       141,142,143,144,
     *       145,146,147,148,
     *       149,150,151,152,
     *       153,154,155,156,
     *       200,200,200,157,
     *       200)KEYNUM     
        GOTO 200
C
C LOGBLO CHANGE
C
10      CONTINUE
        IF(VALUE.LT.1.OR.VALUE.GT.125) GOTO 205
        BUF(1)=LOGBLO
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C PRIMARY TAPESW CHANGE
C
15      CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.9) GOTO 205
        BUF(1)=TAPESW
        BUF(2)=VALUE
        BUF(3)=TCPAR
        BUF(8)=0
        GOTO 250
C
C DISKSW CHANGE
C
20      CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.1) GOTO 205
        BUF(1)=DISKSW
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C CHKTIM CHANGE
C
25      CONTINUE
        IF(VALUE.LT.30.OR.VALUE.GT.9999) GOTO 205
        BUF(1)=CHKTIM
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C SUPCOM CHANGE
C
30      CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.1) GOTO 205
        BUF(1)=SUPCOM
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C SECONDARY TAPE SWITCH CHANGE
C
35      CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.9) GOTO 205
        BUF(1)=TAPESW
        BUF(2)=VALUE
        BUF(3)=TCPAR
        BUF(8)=1
        GOTO 250
C
C DPTTIM CHANGE
C
40      CONTINUE
        IF(VALUE.LT.1.OR.VALUE.GT.99) GOTO 205
        BUF(1)=DPTTIM
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C MAXTRA CHANGE
C
45      CONTINUE
        IF(VALUE.LT.1.OR.VALUE.GT.NUMPRO) GOTO 205
        BUF(1)=MAXTRA
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C SYSSTS CHANGE
C
50      CONTINUE
        IF(VALUE.LT.1.OR.VALUE.GT.3) GOTO 205
        BUF(1)=SYSSTS
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C CHANGE P(ROMREV)
C
55      CONTINUE
        BUF(1)=ROMREV
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C REDMAX CHANGE
C
60      CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.9999999.OR.GAM.EQ.0) GOTO 205
        BUF(8)=GAM
        BUF(1)=3
        BUF(2)= DOLVAL(VALUE)
        BUF(3)=TCGEN
        GOTO 250
C
C FORCE CHECKPOINT
C
65      CONTINUE
        BUF(1)=1
        BUF(2)=0
        BUF(3)=TCGEN
        GOTO 250
C
C DESFLG CHANGE
C
70      CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1)=DESFLG
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C CANTIM CHANGE
C
75      CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.9999) GOTO 205
        BUF(1)=CANTIM
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C SUPWAG CHANGE
C
80      CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        IF(GAM.EQ.0) THEN
          BUF(1)=SUPWAG
          BUF(2)=VALUE
        ELSE
          BITMAP(1)=P(SUPGWA)
          BITMAP(2)=P(SUPGWA1)
          IF(VALUE.EQ.0) THEN
            CALL BCLR(BITMAP,GAM)
          ELSE
            CALL BSET(BITMAP,GAM)
          ENDIF
          BUF(1)=SUPGWA
          BUF(2)=BITMAP(1)
          BUF(9)=BITMAP(2)
        ENDIF
        BUF(3)=TCPAR
        GOTO 250
C
C SUPCAN CHANGE
C
85      CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        IF(GAM.EQ.0) THEN
          BUF(1)=SUPCAN
          BUF(2)=VALUE
        ELSE
          BITMAP(1)=P(SUPGCA)
          BITMAP(2)=P(SUPGCA1)
          IF(VALUE.EQ.0) THEN
            CALL BCLR(BITMAP,GAM)
          ELSE
            CALL BSET(BITMAP,GAM)
          ENDIF
          BUF(1)=SUPGCA
          BUF(2)=BITMAP(1)
          BUF(9)=BITMAP(2)
        ENDIF
        BUF(3)=TCPAR
        GOTO 250
C
C SUPVAL CHANGE
C
90      CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        IF(GAM.EQ.0) THEN
          BUF(1)=SUPVAL
          BUF(2)=VALUE
        ELSE
          BITMAP(1)=P(SUPGVA)
          BITMAP(2)=P(SUPGVA1)
          IF(VALUE.EQ.0) THEN
            CALL BCLR(BITMAP,GAM)
          ELSE
            CALL BSET(BITMAP,GAM)
          ENDIF
          BUF(1)=SUPGVA
          BUF(2)=BITMAP(1)
          BUF(9)=BITMAP(2)
        ENDIF
        BUF(3)=TCPAR
        GOTO 250
C
C SUPSPE CHANGE
C
95      CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1)=SUPSPE
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C SUPFIL CHANGE
C
100     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1)=SUPFIL
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C SUPSUM CHANGE
C
105     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1)=SUPSUM
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C SUPPUD CHANGE
C
110     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1)=SUPPUD
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C SUPSYN CHANGE
C
111     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1)=SUPSYN
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C MAXMES CHANGE
C
115     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.100) GOTO 205
        BUF(1)=MAXMES
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C MESLOG CHANGE
C
120     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.5) GOTO 205
        BUF(1)=MESLOG
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C REDMIN CHANGE
C
125     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.9999999.OR.GAM.EQ.0) GOTO 205
        BUF(8)=GAM
        BUF(1)=4
        BUF(2)= DOLVAL(VALUE)
        BUF(3)=TCGEN
        GOTO 250
C
C TV NEWS MESSAGE UPADTE CHANGE.
C
126     CONTINUE
        IF(VALUE.NE.0.AND.(VALUE.LT.15.OR.VALUE.GT.99999)) GOTO 205
        BUF(1) = TVNUPD
        BUF(2) = VALUE
        BUF(3) = TCPAR
        GOTO 250
C
C WRNPER CHANGE
C
127     CONTINUE
        IF(VALUE.LT.10.OR.VALUE.GT.99) GOTO 205
        BUF(1)=WRNPER
        BUF(2)=VALUE*1000
        BUF(3)=TCPAR
        GOTO 250
C
C SEND TV NEWS MESSAGE
C
128     CONTINUE
        CALL GETBUF(GBUF)
        IF(GBUF.LE.0) THEN
           WRITE(5,1601)
           GOTO 300
        ENDIF
        HPRO(TRCODE,GBUF) = TYPUNS
        HPRO(INPLEN,GBUF) = 8
        PRO(INPTAB+0,GBUF) = -1             !Type Field of Message.
        PRO(INPTAB+2,GBUF) = TVNUPD         !Parameter Number.
        CALL QUETRA(UNS,GBUF)
        CALL RELSE(TSKNAM(UNS),ST)
        GOTO 300
C
C ODDS UPDATE CHANGE
C
131     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.99999) GOTO 205
        BUF(1) = ODSUPD
        BUF(2) = VALUE
        BUF(3) = TCPAR
        GOTO 250
C
C JACKPOT UPDATE CHANGE
C
132     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.99999) GOTO 205
        BUF(1) = JAKUPD
        BUF(2) = VALUE
        BUF(3) = TCPAR
        GOTO 250
C
C TOTO SELECT MINIMUM POOL CHANGE
C
133     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.99999999) GOTO 205
        BUF(1) = TSPMIN
        BUF(2) = DOLVAL(VALUE)
        BUF(3) = TCPAR
        GOTO 250
C
C TOTO SELECT LIABILITY LIMIT
C
134     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.99999999) GOTO 205
        BUF(1) = TSLIAB
        BUF(2) = DOLVAL(VALUE)
        BUF(3) = TCPAR
        GOTO 250
C
C TOTO SLECT WARNING LIMIT
C
135     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.99999999) GOTO 205
        BUF(1) = TSLWRN
        BUF(2) = DOLVAL(VALUE)
        BUF(3) = TCPAR
        GOTO 250
C
C TOTO SELECT POOL PERCENTAGE LIMIT
C
136     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.99) GOTO 205
        BUF(1) = TSPPER
        BUF(2) = VALUE * 1000
        BUF(3) = TCPAR
        GOTO 250
C
C TOTO SELECT POOL SUPPRESSION
C
140     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1) = SUPTSP
        BUF(2) = VALUE
        BUF(3) = TCPAR
        GOTO 250
C
C TOTO SELECT WARNING TIME LIMIT
C
141     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.99999)  GOTO 205
        BUF(1) = TSTLIM
        BUF(2) = VALUE
        BUF(3) = TCPAR
        GOTO 250
C
C TOTO SELECT MAX BET AMOUNT ALLOWED DURING WARNING TIME LIMIT
C
142     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.99999999)  GOTO 205
        BUF(1) = TSLMAX
        BUF(2) = DOLVAL(VALUE)
        BUF(3) = TCPAR
        GOTO 250
C                                                                               
C                                                                               
C MAXSPT CHANGE                                                                 
C                                                                               
143   CONTINUE                                                                  
      IF(VALUE.LT.0.OR.VALUE.GT.30000) GOTO 205                                 
      BUF(1)=MAXSPT                                                             
      BUF(2)=VALUE                                                              
      BUF(3)=TCPAR                                                              
      GOTO 250                                                                  
C
C CANDRW CHANGE
C
144     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.1440) GOTO 205
        BUF(1) = CANDRW
        BUF(2) = VALUE 
        BUF(3) = TCPAR
        GOTO 250
C
C PRETTIM CHANGE
C
145     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.120) GOTO 205
        BUF(1) = PRETTIM
        BUF(2) = VALUE 
        BUF(3) = TCPAR
        GOTO 250
C
C TSMXLI CHANGE
C
146     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.99999999) GOTO 205
        BUF(1) = TSMXLI
        BUF(2) = DOLVAL(VALUE)
        BUF(3) = TCPAR
        GOTO 250
C
C TSMXODD CHANGE
C
147     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.999999999) GOTO 205
        BUF(1) = TSMXODD
        BUF(2) = VALUE
        BUF(3) = TCPAR
        GOTO 250
C
C SUPREM CHANGE
C
148     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1)=REM_LOG
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C SUPINS CHANGE
C
149     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1)=SUPINS
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C SUPFPT CHANGE
C
150     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1)=SUPFPT
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C FPTTIM CHANGE
C
151     CONTINUE
        IF(VALUE.LT.1.OR.VALUE.GT.1440) GOTO 205
        BUF(1)=FPTTIM
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C CHANGE GVTREV
C
152     CONTINUE
        BUF(1)=GVTREV
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C CHANGE PCANTIM
C
153     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.120) GOTO 205
        BUF(1)=PCANTIM
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250        
C
C CHANGE PMAXRTM
C
154     CONTINUE
        IF(VALUE.LT.120.OR.VALUE.GT.3600) GOTO 205
        BUF(1)=PMAXRTM
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250        
C
C CHANGE PMAXCAN
C
155     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.100) GOTO 205
        BUF(1)=PMAXCAN
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250        
C
C CHANGE PMAXLOP
C
156     CONTINUE
        IF(VALUE.LT.50.OR.VALUE.GT.1000000) GOTO 205
        BUF(1)=PMAXLOP
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250          
C
C SUPRET CHANGE
C
157     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        IF(GAM.EQ.0) THEN
          BUF(1)=SUPRET
          BUF(2)=VALUE
        ELSE
          IF(GTYP.EQ.TPAS) THEN
            BITMAP(1)=P(SUPGRE)
            BITMAP(2)=P(SUPGRE1)
            IF(VALUE.EQ.0) THEN
              CALL BCLR(BITMAP,GAM)
            ELSE
              CALL BSET(BITMAP,GAM)
            ENDIF
            BUF(1)=SUPGRE
            BUF(2)=BITMAP(1)
            BUF(9)=BITMAP(2)
          ELSE
            WRITE(CLIN23,802) GAM
802         FORMAT('Not allowed for Game: ',I2)
            RETURN
          ENDIF
        ENDIF
        BUF(3)=TCPAR
        GOTO 250        
C
C INPUT ERROR
C
200     CONTINUE
        WRITE(CLIN23,800)
800     FORMAT('Input error')

        RETURN
C
C VALUE ERROR
C
205     CONTINUE
        WRITE(CLIN23,801)
801     FORMAT('Value error')

        RETURN
C
C QUEUE COMMAND BUFFER TO SYSTEM INPUT INPUT QUEUE
C
250     CONTINUE
        BUF(6)=IDNUM
        CALL VISCMD(BUF,ST)
        CALL XWAIT(2,1,ST)
C
C BUILD SYSSNP SCREEN IMAGE
C
300     CONTINUE
        WAGERS = 0
        VOIDS  = 0
        VALIDS = 0
        RETURNS = 0

        DO 310 I=1,MAXGAM
            WAGERS = WAGERS + DAYTYP(DOLAMT,TWAG,I)
            VOIDS  = VOIDS  + DAYTYP(DOLAMT,TCAN,I)
            VALIDS = VALIDS + DAYTYP(DOLAMT,TVAL,I)
            RETURNS = RETURNS + DAYTYP(DOLAMT,TRET,I)
310     CONTINUE
C
C GET BUFFER UTILIZATION INFORMATION
C
        CALL LISTSIZE(GAME_OUTQUE, BGAME_OUT)
        CALL LISTSIZE(SOFT_ENCQUE, BSOFT)
        CALL LISTSIZE(QUETAB(1,DIS), BDIS)
        CALL LISTSIZE(FREEQ, BAVAIL)
        CALL LISTSIZE(INQUE, BINP)
        CALL LISTSIZE(X2X_OUTPUT, BOUT)
C
        CALL TRNLIF(MINLIF,MAXLIF,AVGLIF)
C
C GET GAME FLAGS
C
        IF(GAM.NE.0) THEN
            SWAG=0
            SCAN=0
            SVAL=0         
            SRET=0
            IF(TSBIT(P(SUPGWA),GAM)) SWAG=1
            IF(TSBIT(P(SUPGCA),GAM)) SCAN=1
            IF(TSBIT(P(SUPGVA),GAM)) SVAL=1
            IF(TSBIT(P(SUPGRE),GAM)) SRET=1            
        ENDIF


C---- FORMAT SYSTEM SNAPSHOT


        WRITE(CLIN1,901)

C---- System 

        WRITE(CLIN2,903) K(1),P(CMDFLU),K(2),P(LOGBLO),
     *                   K(3),P(TAPESW),K(4),P(DISKSW)

        WRITE(CLIN3,904) K(5),P(CMDFRZ),K(6),P(CHKTIM),
     *                   K(14),K(8)

        WRITE(CLIN4,905) K(9),P(DPTTIM),K(10),P(MAXTRA),
     *                   K(11),P(SYSSTS),K(12),P(ROMREV)

        WRITE(CLIN5,906) K(15),P(DESFLG),
     *                   K(16),P(CANTIM),K(27),P(WRNPER)/1000,
     *                   K(26),P(TVNUPD)

        WRITE(CLIN6,907) K(29),P(MAXMES),K(30),P(MESLOG),
     *                   K(31),P(ODSUPD),K(32),P(JAKUPD)

        WRITE(CLIN7,909) K(51),P(FPTTIM),K(44),P(CANDRW),
     *                   K(45),P(PRETTIM),K(52),P(GVTREV)

        WRITE(CLIN8,9071) K(43),P(MAXSPT),K(53),P(PCANTIM),
     *                    K(54),P(PMAXRTM),K(55),P(PMAXCAN)        

        WRITE(CLIN9,9072) K(56),P(PMAXLOP),K(57),P(PMAXSEK),
     *                    K(58),P(PASTHRO),K(59),P(PDAYRSL)

C---- Supress

        WRITE(CLIN10,910) K(20),P(SUPSPE),K(21),P(SUPFIL),
     *                    K(22),P(SUPSUM),K(23),P(SUPPUD)

        WRITE(CLIN11,911) K(24),P(SUPSYN),K(7),P(SUPCOM),
     *                    K(48),P(REM_LOG),
     *                    K(50),P(SUPFPT)

C---- Game
        
        IF(GAM.EQ.0) THEN
            WRITE(CLIN12,912)  K(17),P(SUPWAG),K(18),P(SUPCAN),
     *                         K(19),P(SUPVAL),K(60),P(SUPRET)
            WRITE(CLIN13,9124) K(49),P(SUPINS)
        ELSE
            IF(GTYP.EQ.TTSL) THEN
                WRITE(CLIN12,9121) GAMNAM,K(17),SWAG,K(18),SCAN,K(19),SVAL              
                WRITE(CLIN13,9123)K(25),VALDOL(REDMIN(GAM)),
     *                            K(47),P(TSMXODD) 
                WRITE(CLIN14,913) K(33),
     *                            P(TSPMIN)*DYN_BETUNIT/DOLL_BASE,
     *                            K(34),
     *                            P(TSLIAB)*DYN_BETUNIT/DOLL_BASE,
     *                            K(35),
     *                            P(TSLWRN)*DYN_BETUNIT/DOLL_BASE,
     *                            K(36),P(TSPPER)/1000
                WRITE(CLIN15,914) K(40),P(SUPTSP),
     *                            K(41),P(TSTLIM),
     *                            K(42),
     *                            P(TSLMAX)*DYN_BETUNIT/DOLL_BASE,
     *                            K(46),
     *                            P(TSMXLI)*DYN_BETUNIT/DOLL_BASE
            ELSEIF(GTYP.EQ.TPAS) THEN
                WRITE(CLIN12,9126) GAMNAM,K(17),SWAG,K(18),SCAN,K(19),SVAL,K(60),SRET             
                WRITE(CLIN13,9125) K(25),VALDOL(REDMIN(GAM)),         
     *                             K(13),CMONY(REDMAX(GAM),14,BETUNIT),
     *                             K(61),DISPER(RETCOM(GIND))
                WRITE(CLIN15,900)    
            ELSE
                WRITE(CLIN12,9121) GAMNAM,K(17),SWAG,K(18),SCAN,K(19),SVAL              
                WRITE(CLIN13,9122) K(25),VALDOL(REDMIN(GAM)),         
     *                             K(13),CMONY(REDMAX(GAM),14,BETUNIT)
                WRITE(CLIN14,900)
                WRITE(CLIN15,900)
            ENDIF
        ENDIF

C---- Perform.

        WRITE(CLIN16,915) K(37),AVGLIF,K(38),MINLIF,K(39),MAXLIF

C---- System Performance.

        WRITE(CLIN17,917)

        WRITE(CLIN18,918)

        WRITE(CLIN19,919) NXTSER,BAVAIL,CMONY(WAGERS,12,BETUNIT)

        WRITE(CLIN20,920) P(NXTTRA),BINP,BDIS,CMONY(VOIDS,12,BETUNIT)

        WRITE(CLIN21,921) PERFRM(3,PERTRA),BGAME_OUT,BSOFT,
     *                    CMONY(VALIDS,12,VALUNIT)

        WRITE(CLIN22,922) BOUT,CMONY(RETURNS,12,VALUNIT)
C
C Put in % full for tmf
C
        IF(TMFSIZE.LE.0)THEN
            CLIN22(1)(1:27) = '*TMF NOT FOUND*'
        ELSE
            PCNT = ( DFLOAT(NXTSER)/DFLOAT(TMFSIZE) ) * 100.0D0
            WRITE(CLIN22(1)(1:27),9221) PCNT
        ENDIF
C
        RETURN


C
C----- FORMAT STATEMENTS
C
900     FORMAT(80(' '))
901     FORMAT('**** System control snapshot ****')
903     FORMAT('SYSTEM   >  ',' ',A7,I6,3X,3('*',A7,I6,3X))
904     FORMAT('            ',' ',A7,I6,3X,'*',A7,I6,3X,
     *                      '*',A7,9X,'*',A7,10X)
905     FORMAT('            ',3('*',A7,I6,3X),'*',A7,2X,A4)
906     FORMAT('            ',4('*',A7,I6,3X))
907     FORMAT('            ',4('*',A7,I6,3X))
9071    FORMAT('            ',4('*',A7,I6,3X))
9072    FORMAT('            ','*',A7,I6,3X,3(' ',A7,I6,3X))
908     FORMAT('            ',4('*',A7,I6,3X))
909     FORMAT('            ',3('*',A7,I6,3X),'*',A7,2X,A4)
910     FORMAT('SUPRESS  >  ',4('*',A7,I6,3X))
911     FORMAT('            ',4('*',A7,I6,3X))
912     FORMAT('GLOBAL   >  ',4('*',A7,I6,3X))
9121    FORMAT(A9,'>  ',3('*',A7,I6,3X))
9122    FORMAT('            ','*',A7,I6,3X,'*',A7,A14, 29(' '))
9123    FORMAT('            ','*',A7,I6,3X,'*',A6,I8,2X)
9124    FORMAT('            ','*',A7,I6,3X)
9125    FORMAT('            ','*',A7,I6,3X,'*',A7,A14,12X,' ',A7,1X,F5.2,'%',2X)
9126    FORMAT(A9,'>  ',4('*',A7,I6,3X))
913     FORMAT('            ','*',A7,I6,3X,'*',A6,I8,2X,
     *                        '*',A7,I6,3X,'*',A7,I6,'%')
914     FORMAT('            ',3('*',A7,I6,3X),'*',A6,I9)
915     FORMAT('PERFORM  >  ',3(' ',A7,I6,3X))
917     FORMAT(21('-'),' S Y S T E M   P E R F O R M A N C E ',
     *   21('-'))
918     FORMAT(5X,'Volume    ',14X,'Buffer utilization',21X,'Sales')
919     FORMAT('Serial number -',I12,1X,'Available     - ',I4,
     *   7X,'Wagered  -  ',A12)
920     FORMAT('Transaction  #-',I12,1X,'Input/dispat  - ',I4,
     *   '/',I4,2X,'Canceled -  ',A12)
921     FORMAT('Volume        -',I12,1X,'Game_out/soft - ',I4,
     *   '/',I4,2X,'Cashed   -  ',A12)
922     FORMAT('                ',12X,'X2X_Output    - ',I9,
     *   2X,'Returns  -  ',A12)
9221    FORMAT('% of TMF used -',F11.2,'%')
923     FORMAT('Sorry, game ',I2,' not active ')
1600    FORMAT(A8,I1)
1601    FORMAT(1X,'Unable to Send buffer, no buffer available')
        END
