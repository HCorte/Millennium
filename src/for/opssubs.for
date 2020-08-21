C
C OPSSUBS.FOR
C
C V05 30-MAR-2011 FRP Taken from NRM_OPEN_OPS
C V04 22-DEC-2010 FRP Lotto2 Changes
C V03 13-DEC-2010 FJG Lotto2 Batch change of KEY
C V02 17-DEC-2003 FRP Modify for Batch2 Totobola Changes.
C V01 30-JAN-2001 EPH INITIAL RELEASE FOR PORTUGAL
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
C       ********************************************************************
	SUBROUTINE OPEN_OPSGEN (OPSGEN_LUN, TYPE_OP, ACCESS, YEAR, WEEK, GNUM, ST)
C       ********************************************************************
C       Routine to open OPSGENgg_tt_yyyywww.FIL 
C       ********************************************************************
	IMPLICIT NONE

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:OPS_REC.DEF'

        INTEGER*4    YEAR, WEEK    !(INPUT) YEAR AND WEEK 
        INTEGER*4    TYPE_OP       !(INPUT) TYPE OF OP ( 0=OP FOR HI PRIZES
                                   !                     0=REGULAR OP)
                                   !                    99=SEGUNDA VIA
        INTEGER*4    GNUM          !(INPUT) GAME NUMBER (0=ALL)
        INTEGER*4    OPSGEN_LUN    !(INPUT) LOGICAL UNIT FOR THE FILE
        CHARACTER*5  ACCESS        !(INPUT) 'READ' OR 'WRITE'
        INTEGER*4    ST            !(OUTPUT) STATUS FOR OPEN
        

	CHARACTER*30 FILENAME

        IF (TYPE_OP.EQ.99) THEN
C
C	   SEGUNDA VIA MAY CONTAIN DIFFERENT DRAWS WITH 12 DAY OR REGULAR OP
C          SET YEAR AND WEEK TO 999999   (OPSGENgg_99_999999.FIL)
C
	   WRITE (FILENAME, 90) GNUM, TYPE_OP, 9999, 999
	ELSE
	   WRITE (FILENAME, 90) GNUM, TYPE_OP, YEAR, WEEK 
        ENDIF

90      FORMAT ('FILE:OPSGEN',I2.2,'_',I2.2,'_', I4.4, I3.3, '.FIL')

	ST = 0

        CALL FIND_AVAILABLE_LUN(OPSGEN_LUN,ST)
        IF (ST.NE.0) RETURN

        IF (ACCESS(1:4).EQ.'READ') THEN
	   OPEN (UNIT           =  OPSGEN_LUN,
     *           FILE           =  FILENAME,
     *           STATUS         = 'OLD',
     *           ORGANIZATION   = 'SEQUENTIAL',
     *           ACCESS         = 'SEQUENTIAL',
     *           FORM           = 'UNFORMATTED',
     *           RECL           =  SIZEOF(OPS_REC),
     *           RECORDTYPE     = 'FIXED',
     *           DISPOSE        = 'DELETE',
     *           IOSTAT         =  ST)
        ELSE
	   OPEN (UNIT           =  OPSGEN_LUN,
     *           FILE           =  FILENAME,
     *           STATUS         = 'NEW',
     *           ORGANIZATION   = 'SEQUENTIAL',
     *           ACCESS         = 'SEQUENTIAL',
     *           FORM           = 'UNFORMATTED',
     *           RECL           =  SIZEOF(OPS_REC),
     *           RECORDTYPE     = 'FIXED',
     *           IOSTAT         =  ST)
        ENDIF

        RETURN
        END
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE INPUT_SORTEIO_PER_GAME(GAME_SEL,AAAACCC,AAAA,CCC)
      IMPLICIT NONE
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
C
      INTEGER*4 GAME_SEL(MAXGAM)
      INTEGER*4 AAAA(MAXGAM),CCC(MAXGAM)
      INTEGER*4 GNUM,GTYP,GIND,SORTEIO,ST,K
      CHARACTER AAAACCC(MAXGAM)*7,INPSTR*62
C
      DO 100 GNUM=1,MAXGAM
        GAME_SEL(GNUM) = 0
        AAAACCC(GNUM)=' '
C
        GIND = GNTTAB(GAMIDX,GNUM)
        GTYP = GNTTAB(GAMTYP,GNUM)
        IF(GTYP.LE.0 .OR.
     *     GTYP.EQ.TPAS .OR. GTYP.EQ.TTGL) GOTO 100
C
200     CONTINUE
        WRITE(INPSTR,1000) 'Entre Sorteio (AAAACCC) de ',(GLNAMES(K,GNUM),K=1,4),' [E - Nao incluir]:'
        CALL INPNUM(INPSTR,SORTEIO,0,9999999,ST)
        IF(ST.LT.0) GOTO 100
        AAAA(GNUM) = INT(SORTEIO/1000)
        CCC(GNUM)  = SORTEIO - (AAAA(GNUM)*1000)
        IF(AAAA(GNUM).LT.2000 .OR. CCC(GNUM).GT.106 .OR. CCC(GNUM).LE.0) THEN
          TYPE*,IAM(),'Sorteio Invalido'
          GOTO 200
        ENDIF
        GAME_SEL(GNUM)=1
        WRITE(AAAACCC(GNUM),FMT='(I7.7)') SORTEIO
C
100   CONTINUE
C
1000  FORMAT(A27,4A4,A19)
C
      RETURN
      END


C	**************************************************
C	*************** WORSAP PROCESSING ****************
C	**************************************************

C 	**************************************************
     	SUBROUTINE OPEN_WORSAP (LUN, PROC_CDC, ST)
C	**************************************************
   	IMPLICIT NONE                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:DATBUF.DEF' 
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	INTEGER*4 ST
	INTEGER*4 LUN
	INTEGER*4 PROC_CDC
	CHARACTER WORSAP_FILE*26
	INTEGER*2 DATE(12)

	DATE(VCDC) = PROC_CDC
	CALL LCDATE(DATE)
	DATE(VYEAR) = DATE(VYEAR)+2000

	WRITE(WORSAP_FILE,FMT='(A12,I4.4,I2.2,I2.2,A4)') 'FILE:WORSAP_', DATE(VYEAR), DATE(VMON), DATE(VDAY), '.ASC'

	WORSAP_REC.LUN = LUN
	WORSAP_REC.RECTYPE = '  '
	WORSAP_REC.RECNUM  = 0

        OPEN (UNIT            = WORSAP_REC.LUN,
     *        FILE            = WORSAP_FILE,
     *        STATUS          = 'NEW',
     *        CARRIAGECONTROL = 'LIST', 
     *        IOSTAT          =  ST)
	IF (ST.NE.0) THEN    
	   WORSAP_REC.ERRSTR  = 'Error opening file ' // WORSAP_FILE
	ENDIF

	RETURN
	END


C 	*************************************
     	SUBROUTINE WRITE_WORSAP (RECTYPE, ST)
C	*************************************
   	IMPLICIT NONE                
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF' 
        INCLUDE 'INCLIB:BANK_REC.DEF'
        INCLUDE 'INCLIB:INTERFACES_REC.DEF'

	CHARACTER*2   RECTYPE    ! 'HD'= HEADER / 'DT'=DETAIL / 'TL'=TRAILLER
	INTEGER*4     ST

	CHARACTER*80  ASCIIREC

	WRITE(ASCIIREC,22)
22	FORMAT(80(' ')) 

	ST = 0

	IF (RECTYPE.NE.'HD'.AND. RECTYPE.NE.'DT' .AND. RECTYPE.NE.'TL') THEN
           ST = -20
	   WORSAP_REC.ERRSTR = 'RECTYPE invalid = ' // RECTYPE
           RETURN
        ENDIF

	IF (WORSAP_REC.LUN.EQ.0) THEN 
           ST = -30
	   WORSAP_REC.ERRSTR = 'There is no WORSAP file opened'
           RETURN
        ENDIF
C
C	WRITE HEADER RECORD
C	-------------------
	IF (RECTYPE.EQ.'HD') THEN

	   IF (WORSAP_REC.RECTYPE.NE.'  ' .AND. WORSAP_REC.RECTYPE.NE.'TL') THEN
	      ST = -40
	      WORSAP_REC.ERRSTR = 'HEADER CAN NOT BE WRITTEN HERE (WRITE_WORSAP)'
	      RETURN
           ENDIF
	
	   ASCIIREC(01:03) = 'HW1'
	   ASCIIREC(04:11) = WORSAP_REC.DATA_GERACAO

	   WORSAP_REC.RECTYPE = RECTYPE

	ENDIF

C
C	WRITE DETAIL RECORD
C	-------------------
	IF (RECTYPE.EQ.'DT') THEN

	   IF (WORSAP_REC.RECTYPE.NE.'HD' .AND. WORSAP_REC.RECTYPE.NE.'DT') THEN
	      ST = -50
	      BNK_REC.ERRSTR = 'DETAIL RECORD CAN NOT BE WRITTEN HERE (WRITE_WORSAP)'
	      RETURN
           ENDIF
	
	   ASCIIREC(01:03) = 'HW2'
	   WRITE(ASCIIREC(4:5),FMT='(I2.2)') WORSAP_REC.JOGO
	   ASCIIREC(06:12) = WORSAP_REC.CONCURSO
	   WRITE(ASCIIREC(13:25),FMT='(I13.13)') WORSAP_REC.VALOR
	   WRITE(ASCIIREC(26:29),FMT='(I4.4)')   WORSAP_REC.BANCO
	   WRITE(ASCIIREC(30:35),FMT='(I6.6)')   WORSAP_REC.OCORRENCIAS_PREMIOS

	   WORSAP_REC.RECTYPE    = RECTYPE
	   WORSAP_REC.RECNUM     = WORSAP_REC.RECNUM  + 1

	ENDIF

C
C	WRITE TRAILLER RECORD
C	---------------------
	IF (RECTYPE.EQ.'TL') THEN

	   IF (WORSAP_REC.RECTYPE.NE.'HD' .AND. WORSAP_REC.RECTYPE.NE.'DT') THEN
	      ST = -70
	      BNK_REC.ERRSTR = 'TRAILLER RECORD CAN NOT BE WRITTEN HERE (WRITE_WORSAP)'
	      RETURN
           ENDIF

	   ASCIIREC(1:3) = 'HW9'
	   WRITE (ASCIIREC(4:9), FMT='(I6.6)') WORSAP_REC.RECNUM

	ENDIF


	WRITE (WORSAP_REC.LUN,FMT='(A35)',IOSTAT=ST) ASCIIREC(1:35)
	IF (ST.NE.0) THEN
	   BNK_REC.ERRSTR = 'Error during WRITE (WRITE_WORSAP)'
           RETURN
	ENDIF

	RETURN
	END
