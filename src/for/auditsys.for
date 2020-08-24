C
C PROGRAM AUDSYS
C
C  V02 30-NOV-2010 FJG TWEMSER/TWEMCHK replaced by TWLNKSER/TWLNKCHK
C  V01 10 MAR 2001  CS
C  Initial Release
C  DEC Baseline
C
C ** Source - auditsys.for **
C
C AUDITSYS.FOR
C
C V06 16-FEB-2011 RXK TWEMSER/TWEMCHK replaced by TWLNKSER/TWLNKCHK
C V05 11-MAR-2010 RXK Claims replaced with returns
C V04 23-MAR-2009 LRS Modify for EM Joker
C V03 02-JUN-2005 FRP Modify for IPS Distribution.
C V02 16-DEC-2003 FRP Modify for Batch2 Totobola Changes.
C V01 10-MAR-01 CS  INITIAL RELEASE FOR PORTUGAL
C
C PROGRAM TO SCAN ANY TMF LOGGED TRANSACTION
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	PROGRAM    AUDITSYS
	IMPLICIT   NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:STANDARD.DEF'
C
	INTEGER*4 BUF(LREC*3)
	INTEGER*4 I4MTMNAM(5)
	INTEGER*4 DUMMY, ST, OFF, INDEX, BLOCK, UNIT, NUM, CDC, SZ
	DATA      UNIT/1/
C
	CHARACTER*20 MTMNAM/'                    '/ 
	EQUIVALENCE(I4MTMNAM,MTMNAM)

	CHARACTER*80 C_SERIAL
	INTEGER*4    INISER, MIDSER, ENDSER

	INTEGER*4    YESNO
	INTEGER*4    CHKERR
        INTEGER*4    INPVER
        INTEGER*4    YEAR

	INTEGER*2    DATE(12)
C
	CALL COPYRITE


1001    CONTINUE     !LOOP FOR SERIAL
	CALL CLRSCR(6)
	WRITE(6,109) 
109	FORMAT(1X,79('='),/,1X,T23,'AUDITORIA DO SISTEMA ON-LINE DE JOGOS',/,1X,79('='))

C
C GET SERIAL NUMBER
C

	C_SERIAL = '                '
        TYPE*,' '
	TYPE*,'DIGITE O NUMERO SERIAL DA TRANSA플O NO FORMATO'
        TYPE*,' '
        TYPE*,'             999-99999999-999'
        TYPE*,' '
	TYPE*,'OU DIGITE APENAS <ENTER> PARA SAIR'
        TYPE*,' '
        TYPE*,' '

	READ(5,90) C_SERIAL
90	FORMAT(A80)
        IF (C_SERIAL.EQ.'                ') THEN
	   TYPE*, ' '
	   TYPE*, 'FIM DE EXECU플O DETERMINADO PELO OPERADOR'
	   TYPE*, ' '
	   STOP
        ENDIF

	IF (C_SERIAL(4:4).NE.'-' .OR. C_SERIAL(13:13).NE.'-') THEN
	   CALL DISPERR(6, 'ERRO NA DIGITACAO DO SERIAL (VERIFIQUE E DIGITE NOVAMENTE)', 0, ' ', 0, ' ', 0)
	   CALL XWAIT(3,2,ST)
           GOTO 1001
        ENDIF

	INISER = CTOI(C_SERIAL(1:3),SZ)
	MIDSER = CTOI(C_SERIAL(5:12),SZ)
	ENDSER = CTOI(C_SERIAL(14:16),SZ)

	IF (INISER.EQ.0 .OR. MIDSER.EQ.0) THEN
	   CALL DISPERR(6, 'ERRO NA DIGITACAO DO SERIAL (VERIFIQUE E DIGITE NOVAMENTE)', 0, ' ', 0, ' ', 0)
	   CALL XWAIT(3,2,ST)
	   GOTO 1001
        ENDIF

        TYPE*,' '
        TYPE*,' '
        TYPE*,' '
	TYPE*,'DIGITE O ANO EM QUE A TRANSA플O FOI REALIZADA (AAAA) '
	TYPE*,' '
        TYPE*,' '
        READ(5,55) YEAR
55      FORMAT(I4)	

C
C	OBTAIN CDC FROM JULIAN DATE
C
        DATE(3) = YEAR
        DATE(4) = INISER
        DATE(5) = 0
        CALL LJDATE(DATE)
        CDC     = DATE(5)
C
C	OBTAIN INTERNAL SERIAL FROM EXTERNAL
C
        CHKERR=INPVER(CDC,MIDSER,NUM,ENDSER)
	IF (CHKERR.NE.0) THEN
	   CALL DISPERR(6, 'ERRO NA DIGITACAO DO SERIAL', 0, 'DIGITOS DE VERIFICACAO NAO CONFEREM ', 0, ' ', 0)
	   CALL XWAIT(3,2,ST)
	   GOTO 1001
	ENDIF

	WRITE(MTMNAM,FMT='(A2,I4.4,A4)') 'MT', CDC, '.FIL'

C
C OPEN TMF FILE FROM APPROPRIATE VOLUME
C
	CALL OPENW(UNIT,I4MTMNAM,4,0,0,ST)
	IF (ST.NE.0) THEN
C           CALL FILERR(I4MTMNAM,OPEN_ERROR,ST,0)
	   CALL USRCLOS1(     1)
	   CALL DISPERR(6, 'ERRO NA ABERTURA DO FICHEIRO DE TRANSA합ES: '//MTMNAM, 0, 'ABANDONANDO O PROGRAMA... ', 0, ' ', 0)
	   STOP
	ENDIF
	CALL TOPEN(UNIT)

	IF(NUM.LE.0) NUM=1
	CALL GETBI(NUM,BLOCK,INDEX,OFF)

15	CONTINUE
	CALL RLOG(NUM,BUF,DUMMY,ST)
	IF(ST.GT.0) THEN
	   NUM=NUM-1
	   GOTO 15
	ENDIF
C
	CALL USRCLOS1(     1)
	IF(ST.LT.0) THEN
	   CALL DISPERR(6, 'ERRO NO FECHAMENTO DO FICHEIRO DE TRANSA합ES', 0, 'ABANDONANDO O PROGRAMA... ', 0, ' ', 0)
           STOP
C          CALL FILERR(I4MTMNAM,READ_ERROR,ST,NUM)
	ENDIF
C
	CALL LOGTRA (TRABUF,BUF)
C
C	SHOW THE TRANSACTION READ
C
	CALL SHOWTRN(TRABUF,BLOCK,INDEX, CDC)

	CALL PRMYESNO('DESEJA EXAMINAR OUTRA TRANSA플O (S/N) ? ',YESNO)
	IF (YESNO.EQ.1) THEN
	   GOTO 1001
        ENDIF

	STOP
	END

C
C ROUTINE TO SHOW ALL TRANSACTIONS TYPE
C
C=======OPTIONS    /CHECK=NOOVERFLOW
        SUBROUTINE SHOWTRN(TRABUF,BLOCK,INDEX,CDC)
        IMPLICIT   NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:HASF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:NAMCMD.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:TTNAMES.DEF'
        INCLUDE 'INCLIB:X2PTLMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:DESPAR.DEF'
	
	CHARACTER*80  LINHA(25)
	INTEGER*4     CDC

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

        REAL*8     COMMAND /'        '/    !

        LOGICAL    CALLKBET

        CHARACTER*56 CBIMAGE(12)           !
        CHARACTER    SYST(0:5)*7           !

        CHARACTER*7  KICKER1
        CHARACTER*7  KICKER2

        CHARACTER*1  KIKSTAR1
        CHARACTER*1  KIKSTAR2
        CHARACTER*1  JOKYESNO(0:1)/'N','Y'/

        CHARACTER*1  FREEWK(0:1)/' ','+'/

C	CHARACTER*8  IAGT_NO !FUNCTION
        CHARACTER*8  CHRSTR1
        CHARACTER*8  CHRSTR2
        INTEGER*4    ERR

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
C
        CALL LIB$ESTABLISH(NOCONSIG)
C
C CLEAR SCREEN IMAGE
C
	DO I=1,25
	   LINHA(I) = '                                                                                '
        ENDDO

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
        DBUF(5) = CDC 
        CALL LCDATE(DBUF)
        WRITE(LINHA(1),901) (DBUF(J),J=7,13)
        IF(GTYP.GE.1.AND.GTYP.LE.MAXTYP) THEN
            WRITE(LINHA(3),903) TRABUF(TSER),BLOCK,
     *                       GTNAMES(GTYP),GIND,TTNAMES(TRABUF(TTYP))
        ELSE
            WRITE(LINHA(3),9031) TRABUF(TSER),BLOCK,
     *                        TTNAMES(TRABUF(TTYP))
        ENDIF

        IF(TRABUF(TTYP).EQ.TWAG.AND.TRABUF(TWSYST).GT.0.AND.GTYP.NE.TPAS)THEN                      
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
        IF(TRABUF(TTYP).EQ.TVAL .OR. TRABUF(TTYP).EQ.TREF) THEN
           BNKID  = TRABUF(TVBNKID)
           BNKNUM = TRABUF(TVBNKNUM)
        ENDIF
C
        IF(TRABUF(TTYP).EQ.TCRS .AND. TRABUF(TITYP).EQ.IVAL) THEN
           BNKID  = 0
           BNKNUM = 0
        ENDIF
C
        I4TEMP = TRABUF(TTSTCS)
C
        WRITE(LINHA(4),904) TTYPE(TRABUF(TTYP)),INDEX,
     *                   STAT(TRABUF(TSTAT)),I1TEMP(1)
C
        IF(TRABUF(TTYP).EQ.TCRS) THEN
           WRITE(LINHA(5),9051) TRABUF(TAGT),HR,MIN,SEC,
     *                   ERROR(TRABUF(TERR))
        ELSE
           WRITE(LINHA(5),905) TRABUF(TAGT),HR,MIN,SEC,
     *                   ERROR(TRABUF(TERR)),TRABUF(TSUBERR)
        ENDIF

        WRITE(LINHA(6),906) TRABUF(TTRN),TRABUF(TTER), SYST(STOFF)
        WRITE(LINHA(7),907)  TRABUF(TCDC),TRABUF(TSIZE), STNUM
        IF (TRABUF(TGAMTYP) .EQ. TWIT) THEN
            WRITE(LINHA(8),9081) TRABUF(TGAMTYP),TRABUF(TGAMIND),BNKID,
     *      TRABUF(TWWCOUPID)
        ELSEIF (TRABUF(TGAMTYP) .EQ. TDBL) THEN
            WRITE(LINHA(8),9081) TRABUF(TGAMTYP),TRABUF(TGAMIND),BNKID,
     *      TRABUF(TWDBCOUPID)
        ELSEIF (TRABUF(TGAMTYP) .EQ. TCPL) THEN
            WRITE(LINHA(8),9081) TRABUF(TGAMTYP),TRABUF(TGAMIND),BNKID,
     *      TRABUF(TWCPCOUPID)
        ELSEIF (TRABUF(TGAMTYP) .EQ. TTRP) THEN
            WRITE(LINHA(8),9081) TRABUF(TGAMTYP),TRABUF(TGAMIND),BNKID,
     *      TRABUF(TWTTCOUPID)
        ELSEIF (TRABUF(TGAMTYP) .EQ. TSTR) THEN
            WRITE(LINHA(8),9081) TRABUF(TGAMTYP),TRABUF(TGAMIND),BNKID,
     *      TRABUF(TWSTCOUPID)
        ELSE
            WRITE(LINHA(8),908) TRABUF(TGAMTYP),TRABUF(TGAMIND),BNKID
        ENDIF
        WRITE(LINHA(9),909)TRABUF(TTKID),TRABUF(TCHK), BNKNUM
        IF(GTYP .EQ. TPAS) THEN
	  IF (TRABUF(TPOFFTER).GT.0) THEN
	     AGTOFF = TRABUF(TPOFFTER)
	  ELSE
	     AGTOFF = 0
	  ENDIF

	  WRITE(LINHA(10),910) RETTYP(TRABUF(TPRETYP)), AGTOFF
        ENDIF
C
C WAGER BODY
C
        IF(TRABUF(TTYP).GE.TWAG.AND.TRABUF(TTYP).LE.TINC) THEN

            KICKER1  = '  none '                                  
            KICKER2  = '  none '                                  
            KIKSTAR1 = ' '                                        
            KIKSTAR2 = ' '            

            CALLKBET = .FALSE. 
            IF(TRABUF(TWKGME).NE.0  .OR. TRABUF(TGAMTYP).EQ.TKIK) THEN
                   KIKSTAR1 = JOKYESNO(TRABUF(TWKFLG)) 
                   WRITE(KICKER1,1023) IAND('FFFFFF'X,TRABUF(TWKICK))
                   KIKSTAR2 = JOKYESNO(TRABUF(TWKFLG2)) 
                   WRITE(KICKER2,1023) IAND('FFFFFF'X,TRABUF(TWKICK2))
            ENDIF                                                   

            CALL FASTSET(BLANK,BIMAGE(1,1),168)
            IF(TRABUF(TGAMTYP).EQ.TLTO) CALL LTBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TSPT) CALL SPBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TTGL) CALL TGBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TTSL) CALL TSBET(TRABUF,CBIMAGE)
            IF(TRABUF(TGAMTYP).EQ.TPAS) CALL PABET(TRABUF,CBIMAGE)
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
C
            IF(TRABUF(TGAMTYP).EQ.TSPT .AND. TRABUF(TWSPFRG).GT.0) THEN
              IND=TRABUF(TWNBET)+1
              WRITE(LINHA(11),1010) TRABUF(TWBEG),CSMONY(TRABUF(TWAMT),8,BETUNIT),
     *                              BIMAGE(1,IND),(BIMAGE(K,IND),K=2,3)
              CBIMAGE(IND)=' '
              IND=1
            ELSE
              WRITE(LINHA(11),1011) TRABUF(TWBEG),CSMONY(TRABUF(TWAMT),8,BETUNIT),
     *                              (BIMAGE(K,1),K=1,14)
              IND=2
            ENDIF
            WRITE(LINHA(12),1012) TRABUF(TWEND),FREEWK(TRABUF(TWADDFW)),
     *                         CMONY(TRABUF(TWTKC),8,BETUNIT),
     *                        (BIMAGE(K,IND+0),K=1,14)
            WRITE(LINHA(13),1013) TRABUF(TWDUR),CSMONY(TRABUF(TWKAMT),8,BETUNIT),
     *                        (BIMAGE(K,IND+1),K=1,14)
            WRITE(LINHA(14),1014) TRABUF(TWKGME),CSMONY(TRABUF(TWTOT),8,BETUNIT),
     *                        (BIMAGE(K,IND+2),K=1,14)
            WRITE(LINHA(15),1015) TRABUF(TWSYST),TRABUF(TWSYSN),
     *                        (BIMAGE(K,IND+3),K=1,14)
            WRITE(LINHA(16),1016) TRABUF(TWCTER),TRABUF(TWCSER),
     *                        (BIMAGE(K,IND+4),K=1,14)
            WRITE(LINHA(17),1017) TRABUF(TWNBET),VALST(TRABUF(TWVSTS)),
     *                        (BIMAGE(K,IND+5),K=1,14)
            WRITE(LINHA(18),1018) TRABUF(TWKBEG),TRABUF(TFRAC),
     *                        (BIMAGE(K,IND+6),K=1,14)

            IF(TRABUF(TGAMTYP).NE.TKIK) THEN               
              WRITE(LINHA(19),1019) TRABUF(TWKEND),KIKSTAR1,KICKER1,
     *                        (BIMAGE(K,IND+7),K=1,14)
              WRITE(LINHA(20),1020) TRABUF(TWKDUR),KIKSTAR2,KICKER2,
     *                        (BIMAGE(K,IND+8),K=1,14)
            ELSE
              WRITE(LINHA(19),10191) TRABUF(TWKEND),KICKER1

              WRITE(LINHA(20),10201) TRABUF(TWKDUR),TRABUF(TWLNKSER),TRABUF(TWLNKCHK)

            ENDIF   

            WRITE(LINHA(21),1021) TRABUF(TCDC_SOLD),(BIMAGE(K,IND+9),K=1,14)
            WRITE(LINHA(22),1022) (BIMAGE(K,IND+10),K=1,14)
          
	    CALL SHOW_LINHA(LINHA)
            RETURN
        ENDIF

	IF ((TRABUF(TTYP).EQ.TRET.OR.TRABUF(TTYP).EQ.TVAL).AND.GTYP.EQ.TPAS) THEN
	    WRITE(LINHA(11),911) 
	    NUMTCKS = TRABUF(TPTCK)

	    IND = 12
            DO TCKS = 1, NUMTCKS
               IF (TRABUF(TTYP).EQ.TWAG) THEN
                 STR = RETST_PAS(TRABUF(TPSTS1  + OFFTRA*(TCKS-1)))
               ELSE
                 STR = VALST_PAS(TRABUF(TPSTS1  + OFFTRA*(TCKS-1)))
               ENDIF
               WRITE(LINHA(IND),912) TRABUF(TPNUM1  + OFFTRA*(TCKS-1)),
     *                              TRABUF(TPEMIS1 + OFFTRA*(TCKS-1)),
     *                              TRABUF(TPSER1  + OFFTRA*(TCKS-1)),
     *                              TRABUF(TPTEN1  + OFFTRA*(TCKS-1)),
     *                              STR,
     *                              CMONY(TRABUF(TPPAY1  + OFFTRA*(TCKS-1)),6,VALUNIT)
	       IND = IND + 1
	    ENDDO   

	ENDIF
C
C VALIDATION BODY
C
        IF( (TRABUF(TTYP).EQ.TVAL .OR. TRABUF(TTYP).EQ.TREF) .AND.
     *       TRABUF(TGAMTYP).NE.TPAS) THEN
            WRITE(LINHA(10),3010) VALST(TRABUF(TVSTS)),
     *                         CMONY(TRABUF(TVPAY),11,VALUNIT)

C
C         FOR CDC VALIDATIONS FORMAT IS DIFFERENT
C
            IF(TRABUF(TVCDC).GE.10000) THEN
                WRITE(LINHA(11),3014) TRABUF(TVCDC)-10000,TRABUF(TVSER)
            ELSE
                WRITE(LINHA(11),3011) TRABUF(TVCDC),TRABUF(TVSER)
            ENDIF

            WRITE(LINHA(12),3012) TRABUF(TVKGME),CMONY(TRABUF(TVKPAY),11,VALUNIT)
            WRITE(LINHA(13),3013) TRABUF(TVCODE),TRABUF(TVEXC)

	    CALL SHOW_LINHA(LINHA)
            RETURN
        ENDIF
C
C SPECIAL FUNCTION BODY
C
        IF(TRABUF(TTYP).EQ.TSPE) THEN
            IF(TRABUF(TSFUN).EQ.TSX2X) THEN
                WRITE(LINHA(11),6000) TRABUF(TXIDX)
                WRITE(LINHA(12),6010) TRABUF(TXLAY)
                WRITE(LINHA(13),6020) X2X_PTLMES(TRABUF(TXPTL))
                WRITE(LINHA(14),6030) TRABUF(TXSTN)
                WRITE(LINHA(15),6040) TRABUF(TXSAP)
                IF(TRABUF(TXLAY).EQ.1) THEN
                    WRITE(LINHA(16),6050) TRABUF(TXTFEID)
                    WRITE(LINHA(17),6060) TRABUF(TXTDSAP)
                    WRITE(LINHA(18),6070) TRABUF(TXTBTYP)
                ELSE IF(TRABUF(TXLAY).EQ.2) THEN
                    WRITE(LINHA(16),6080) TRABUF(TXFPID), TRABUF(TXFSSAP)
                    WRITE(LINHA(17),6090) TRABUF(TXFMDUT)
                    CALL BCDASC(CHRSTR1,1,8,TRABUF(TXFDAD1),ERR)
                    CALL BCDASC(CHRSTR2,1,8,TRABUF(TXFDAD2),ERR)
C****               WRITE(LINHA(18),7000) TRABUF(TXFDAD1), TRABUF(TXFDAD2)
                    WRITE(LINHA(19),7010) TRABUF(TXFLFID), TRABUF(TXFLMC)  !DATA
                ELSE IF(TRABUF(TXLAY).EQ.3) THEN
                    WRITE(LINHA(16),7020) TRABUF(TXSPID)
                    WRITE(LINHA(17),7030) TRABUF(TXSSDTU)
                    WRITE(LINHA(18),7040) TRABUF(TXSCC), TRABUF(TXSSNUM)   !DATA
                ENDIF
	    ELSEIF(TRABUF(TSFUN).EQ.TAGTINF) THEN
	        WRITE(LINHA(10),9000) TRABUF(TSNEW)
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
		   WRITE(LINHA(10+I),9001) I, FLDNAM(DID), DLEN, 
     *                                    (DINF(J),J=1,MLEN)
	        ENDDO
            ELSE
                WRITE(LINHA(11),4011) SPFUN(TRABUF(TSFUN))
                WRITE(LINHA(12),4012) TRABUF(TSOLD)
                WRITE(LINHA(13),4013) TRABUF(TSNEW)

                LNS=13
                CNT=0
                DO S=TSDT1,TSDT7
                    LNS=LNS+1
                    CNT=CNT+1
                    WRITE(LINHA(  LNS),4014) CNT,TRABUF(S)
                END DO
            ENDIF

	    CALL SHOW_LINHA(LINHA)
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
            IF(CT.EQ.TCKIK) COMMAND = NAMKIK(CN)
            IF(CT.EQ.TCSCR) COMMAND = NAMSCR(CN)
            IF(CT.EQ.TCWIT) COMMAND = NAMWIT(CN)
            IF(CT.EQ.TCTSL) COMMAND = NAMTSL(CN)
            IF(CT.EQ.TCBNG) COMMAND = NAMBNG(CN)
            IF(CT.EQ.TCDBL) COMMAND = NAMDBL(CN)
            IF(CT.EQ.TCCPL) COMMAND = NAMCPL(CN)
            IF(CT.EQ.TCSSC) COMMAND = NAMSSC(CN)
            IF(CT.EQ.TCTRP) COMMAND = NAMTRP(CN)
            IF(CT.EQ.TCPAS) COMMAND = NAMPAS(CN)
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
               WRITE(LINHA(11),5011)  NAMTYP(CT),COMMAND
               WRITE(LINHA(12),50121) TRABUF(TCMLIN),DSPOLD(2),DSPOLD(1)
               WRITE(LINHA(13),50131) TRABUF(TCMTER),DSPNEW(2),DSPNEW(1)
               WRITE(LINHA(14),5014)  TRABUF(TCMSRC)
            ELSEIF(CT.GT.0) THEN 
               WRITE(LINHA(11),5011) NAMTYP(CT),COMMAND
               WRITE(LINHA(12),5012) TRABUF(TCMLIN),TRABUF(TCMOLD)
               WRITE(LINHA(13),5013) TRABUF(TCMTER),TRABUF(TCMNEW)
               WRITE(LINHA(14),5014) TRABUF(TCMSRC)
            ENDIF
	    CALL SHOW_LINHA(LINHA)
            RETURN
        ENDIF
C
C INSTANT BODY
C
        IF(TRABUF(TTYP).EQ.TCRS) THEN
            WRITE(LINHA(11),8000) ITYPE(TRABUF(TITYP))
            WRITE(LINHA(12),8010) TRABUF(TIERR)
            WRITE(LINHA(13),8020) TRABUF(TIXRF), TRABUF(TIVAGT)
C
            IF(TRABUF(TITYP).EQ.IVAL) THEN
                WRITE(LINHA(14),8030) (TRABUF(TIGAM1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(LINHA(15),8040) (TRABUF(TIPCK1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(LINHA(16),8050) (TRABUF(TIVRN1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(LINHA(17),8060) (TRABUF(TILTX1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                DO K=1,TRABUF(TIBCH)
                    INSSEC(K)=TRABUF(TITIM1+K-1)
                    INSHR(K)=INSSEC(K)/3600
                    INSMIN(K)=(INSSEC(K)-INSHR(K)*3600)/60
                    INSSEC(K)=INSSEC(K)-(INSHR(K)*3600+INSMIN(K)*60)
                END DO
                WRITE(LINHA(18),8070) (INSHR(K),INSMIN(K),INSSEC(K),
     *                              K=1,TRABUF(TIBCH))
                WRITE(LINHA(19),8080) (TRABUF(TICDC1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(LINHA(20),8090) (TRABUF(TISTS1+K),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(LINHA(21),8100) (CMONY(TRABUF(TIPRZ1+K),10,1),
     *                              K=0,TRABUF(TIBCH)-1)
                WRITE(LINHA(22),8091) (TRABUF(TIPCKSTS1+K),
     *                              K=0,TRABUF(TIBCH)-1)
C
            ELSE IF(TRABUF(TITYP).EQ.IISS) THEN
                WRITE(LINHA(14),8510) TRABUF(TIREP)
                WRITE(LINHA(15),8520) TRABUF(TINUM)

                WRITE(LINHA(16),8530) (TRABUF(TIGAM+K),
     *                              K=0,TRABUF(TINUM)-1)
                WRITE(LINHA(17),8550) (TRABUF(TIPCK+K),
     *                              K=0,TRABUF(TINUM)-1)
                WRITE(LINHA(18),8560) (TRABUF(TIRES+K),
     *                              K=0,TRABUF(TINUM)-1)
C
            ELSE IF(TRABUF(TITYP).EQ.ILOT) THEN
                WRITE(LINHA(14),8120) TRABUF(TLREP)
                WRITE(LINHA(15),8130) TRABUF(TLCLS)
                WRITE(LINHA(16),8140) TRABUF(TLGAM)
                WRITE(LINHA(17),8160) TRABUF(TLPCK)
                WRITE(LINHA(18),8170) TRABUF(TLEND) - TRABUF(TLSTR)
                WRITE(LINHA(20),8190) CSMONY(TRABUF(TLAMT),10,1)
                WRITE(LINHA(21),8200) CSMONY(TRABUF(TLCOM),10,1)
C
            ELSE IF(TRABUF(TITYP).EQ.ICAR) THEN
                WRITE(LINHA(14),8120) TRABUF(TCREP)
                WRITE(LINHA(15),8130) TRABUF(TCCLS)
                WRITE(LINHA(16),8140) TRABUF(TCGAM)
                WRITE(LINHA(17),8161) TRABUF(TCCAR)
                WRITE(LINHA(18),8171) TRABUF(TCEND) - TRABUF(TCSTA)
                WRITE(LINHA(20),8191) TRABUF(TCCNT)
C
            ELSE IF(TRABUF(TITYP).EQ.IQTA.OR.
     *              TRABUF(TITYP).EQ.IINV.OR.
     *              TRABUF(TITYP).EQ.ISET) THEN
                WRITE(LINHA(14),8240) TRABUF(TRGAM)
                WRITE(LINHA(15),8250) TRABUF(TRCLS)
                WRITE(LINHA(16),8260) TRABUF(TRNXT1)
                WRITE(LINHA(17),8270) TRABUF(TRNXT2)
C
            ELSE IF(TRABUF(TITYP).EQ.IFIN) THEN
                WRITE(LINHA(14),8290) TRABUF(TRTYP)
                WRITE(LINHA(15),8280) TRABUF(TRSUB)
                WRITE(LINHA(16),8296) TRABUF(TRCHN)
                WRITE(LINHA(17),8297) TRABUF(TRCON1)
                WRITE(LINHA(18),8298) TRABUF(TRCON2)
C
            ELSE IF(TRABUF(TITYP).EQ.IORD) THEN
                WRITE(LINHA(14),8380) TRABUF(TGPGAM)
                WRITE(LINHA(15),8260) TRABUF(TGPNXT)
                WRITE(LINHA(16),8601) CSMONY(TRABUF(TGPRCL),10,1)
C
            ELSE IF(TRABUF(TITYP).EQ.IMNU) THEN
                WRITE(LINHA(15),8315) (TRABUF(TSGAM+K),K=0,5)
                WRITE(LINHA(16),8315) (TRABUF(TSGAM+K),K=6,11)
                WRITE(LINHA(17),8315) (TRABUF(TSGAM+K),K=12,17)
                WRITE(LINHA(18),8315) (TRABUF(TSGAM+K),K=18,23)
                WRITE(LINHA(19),8320) (TRABUF(TSQTY+K),K=0,5)
                WRITE(LINHA(20),8320) (TRABUF(TSQTY+K),K=6,11)
                WRITE(LINHA(21),8320) (TRABUF(TSQTY+K),K=12,17)
                WRITE(LINHA(22),8320) (TRABUF(TSQTY+K),K=18,23)
C
            ELSE IF(TRABUF(TITYP).EQ.ICNF) THEN
                WRITE(LINHA(14),8210) TRABUF(TIINV1)
                WRITE(LINHA(15),8220) TRABUF(TIINV2)
                WRITE(LINHA(16),8230) TRABUF(TIINV3)
C
            ELSE IF(TRABUF(TITYP).EQ.IOACT) THEN
                WRITE(LINHA(14),8235) TRABUF(TOINV1)
                WRITE(LINHA(15),8220) TRABUF(TOINV2)
                WRITE(LINHA(16),8230) TRABUF(TOINV3)
C
            ELSE IF(TRABUF(TITYP).EQ.IEST) THEN
                WRITE(LINHA(14),8350) TRABUF(TISFT)
                WRITE(LINHA(15),8360) TRABUF(TIRSTFLG)
                SEC1=TRABUF(TIRSTTIM)
                HR1=SEC1/3600
                MIN1=(SEC1-HR1*3600)/60
                SEC1=SEC1-(HR1*3600+MIN1*60)
                WRITE(LINHA(16),8361) HR1,MIN1,SEC1
                WRITE(LINHA(17),8362) TRABUF(TIMINCB)
                WRITE(LINHA(18),8363) TRABUF(TICHKSUM)
                WRITE(LINHA(19),8364) TRABUF(TIPHONE1_1), TRABUF(TIPHONE1_2),
     *                             TRABUF(TIPHONE1_3)
                WRITE(LINHA(20),8365) TRABUF(TIPHONE2_1), TRABUF(TIPHONE2_2),
     *                             TRABUF(TIPHONE2_3)
C
            ELSE IF(TRABUF(TITYP).EQ.IGTB) THEN
                WRITE(LINHA(14),8370) TRABUF(TIGMC)
                WRITE(LINHA(15),8380) TRABUF(TIGMN+0)
                WRITE(LINHA(16),8380) TRABUF(TIGMN+1)
                WRITE(LINHA(17),8380) TRABUF(TIGMN+2)
                WRITE(LINHA(18),8380) TRABUF(TIGMN+3)
                WRITE(LINHA(19),8380) TRABUF(TIGMN+4)
C
            ELSE IF(TRABUF(TITYP).EQ.ISON) THEN
                WRITE(LINHA(14),8390) TRABUF(TIGVT1),TRABUF(TIGVT2)
C
          ELSE IF(TRABUF(TITYP).EQ.IFSESON) THEN
            WRITE(LINHA(14),8401) TRABUF(TIFSETYP)
            WRITE(LINHA(15),8402) TRABUF(TIFSERSLT)
            IF ((TRABUF(TIFSETYP).EQ.0).AND.
     +          (TRABUF(TIFSERSLT).EQ.INOER)) THEN !SUCCESSFUL FSE SIGN-ON WITH
               WRITE(LINHA(16),8403) TRABUF(TIFSEREP),
     *                            (TRABUF(TIFSENAMS+K),K=0,5)
               WRITE(LINHA(17),8404) TRABUF(TIFSECLS)
               WRITE(LINHA(18),8405) TRABUF(TIFSEOFF)
            ELSE
               WRITE(LINHA(16),8403) TRABUF(TIFSEREP)
               WRITE(LINHA(17),8404) TRABUF(TIFSECLS)
            ENDIF
          ENDIF
	  CALL SHOW_LINHA(LINHA)
          RETURN
        ENDIF
C
	CALL SHOW_LINHA(LINHA)
        RETURN
C
C
901     FORMAT(1X,'Transaction Snapshot For ',7A2)
903     FORMAT(1X,'Ser ',I9,' Block ',I8,9X,A8,1X,I2,2X,A8)
9031    FORMAT(1X,'Ser ',I9,' Block ',I8,9X,A8,2X,' transaction')
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
908     FORMAT(1X,'Gamtyp   ',I4,' Gamind',I8,9X,'Bank ID    ',3X,I8.8)
9081    FORMAT(1X,'Gamtyp   ',I4,' Gamind',I8,9X,'Bank ID    ',3X,I8.8,
     *         1X,'Couponid  'I3)
909     FORMAT(1X,'Tktid    ',I4,' Chksum',I8,9X,'Bank Account ',1X,I8.8)
910	FORMAT(1X,'Ret. type ',A14,2X,'Term. Off. ',I4.4)
911	FORMAT(1X,' Ticket    Extr.    Serie    Frac.     Status     Amount')
912	FORMAT(2X,I5.5,6X,I2.2,7X,I2.2,7X,I2.2,8X,A4,6X,A6)
9091    FORMAT(1X,'Tktid    ',I4,' Chksum',I8,9X,'Bank Account ',1X,I8.8,
     *         1X,'Times Bet ',I3)
C
1010    FORMAT('Beg  ',I4,' Amt  ',A8,X,A4,'SUPER 14: ',2A4)
1011    FORMAT('Beg  ',I4,' Amt  ',A8,X,14A4)
1012    FORMAT('End  ',I4,A1,'Tktc ',A8,X,14A4)
1013    FORMAT('Dur  ',I4,' J amt',A8,X,14A4)
1014    FORMAT('J gme',I4,' Total',A8,X,14A4)
1015    FORMAT('Systp',I4,' Sys# ',I8,X,14A4)
1016    FORMAT('Cter ',I4,' Cser', I9,X,14A4)
1017    FORMAT('# bet',I4,' Vstat',4X,A4,1X,14A4)
1018    FORMAT('J beg',I4,' Fract',4X,I4,1X,14A4)
1019    FORMAT('J end',I4,' Jok1:',A1,A7,1X,14A4)
1020    FORMAT('J dur',I4,' Jok2:',A1,A7,1X,14A4)

10191    FORMAT('J end',I4,' EMJok: ',A7)
10201    FORMAT('J dur',I4,' EMser: ',I9,1X,I4)

1021    FORMAT('Sold ',I4,15X,14A4)
1022    FORMAT(24X,14A4)
1023    FORMAT(I7.7)
C
3010    FORMAT(1X,'Vstat  ',A4,' Pay',A11)
3011    FORMAT(1X,'Vcdc   ',I4,' Vser ',I9)
3012    FORMAT(1X,'Jokgam ',I4,' Kpy',A11)
3013    FORMAT(1X,'Vcode',I6,' Eser ',I9)
3014    FORMAT(1X,'V(CDC*)',I4,' Vser ',I9)
C
4011    FORMAT(1X,'Function',3X,A4)
4012    FORMAT(1X,'Old    ',Z8)
4013    FORMAT(1X,'New    ',Z8)
4014    FORMAT(1X,'Data',I1,2X,Z8)
C
5011    FORMAT(1X,'Typ ',A8,' Cmd   ',A8)
5012    FORMAT(1X,'Line    ',I4,' Old   ',I10)
50121   FORMAT(1X,'Line    ',I4,' Old   ',2Z8.8)
5013    FORMAT(1X,'Ter     ',I4,' New   ',I10)
50131   FORMAT(1X,'Ter     ',I4,' New   ',2Z8.8)
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
8000    FORMAT(1X,'Type    ',1X,A4)
8010    FORMAT(1X,'Error   ',1X,I10)
8020    FORMAT(1X,'Crs Ref#',1X,I10,2X,'Retailer#',1X,I7.7)
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
8315    FORMAT(1X,'Instant game       ',1X,6(I9))
8320    FORMAT(1X,'Instant quantity   ',1X,6(I9))
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
8530    FORMAT(1X,'Game #  ',1X,10(I5,x))
8550    FORMAT(1X,'Book #  ',1X,10(I5,x))
8560    FORMAT(1X,'Status  ',1X,10(I5,x))
C
8601	FORMAT(1X,'Instant credit lim ',1X,A10)
C
9000    FORMAT(1X,'Online Agent Update. ', I2,' items:')
9001    FORMAT(1X,I2,' - ',A8,1X,'(',I2,' bytes) ','"',<MLEN>A1,'"')
        END




C	**************************
	SUBROUTINE SHOW_LINHA(LINHA)
C	**************************
	CHARACTER*80 LINHA(*)
	INTEGER*4    I
	CALL CLRSCR(6)
	DO I = 1,23
	   WRITE(6,20) LINHA(I)(1:79)
20	   FORMAT(1X,A79)
	ENDDO
	RETURN
	END
