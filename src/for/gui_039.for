C GUI_039.FOR
C
C TRANSACTION,CARRYOVER,VALIDATION DETAILS INFO
C
C V03 29-APR-2011 RXK Opening and closing of TCF and VLF moved here.
C V02 12-MAR-2010 RXK TCLM removed
C V01 07-FEB-2001 UXN Initial release.
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
C This subroutine returns GUI FUNCTION.
C
C Input parameters:
C	NONE               
C
C Output parameters:
C
C	BYTE		OUTBUF(*)    OUTPUT MESSAGE
C	INTEGER*4	MES_LEN	     MESSAGE LENGTH
C	INTEGER*4	RET_CODE:
C		0		-  no error, message accepted;
C		value >= 11	-  error number to be sent to Client.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUI_039(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:TTNAMES.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
        INCLUDE 'INCLIB:GUIFIL.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 WRKBUF(TRALEN)
	INTEGER*4 ST,I,BLOCK,INDEX,OFF
	INTEGER*4 NUM_COLS,NUM_ROWS
	INTEGER*4 CDC,JUL,INTSER,EXTSER,CHKDIG,YEAR
	INTEGER*2 DATE(LDATE_LEN)
	INTEGER*4 LOGBUF(LREC*3)
	INTEGER*4 KEY(2)
	INTEGER*4 TOTPAY,TOTTAX
	INTEGER*4 RCDC  !requested cdc    
        INTEGER*4 ST1,DUMMY
C
	LOGICAL*4 INT_SERIAL,IN_TMF

	INTEGER*4 INPVER
	EXTERNAL  INPVER
C
	RET_CODE = 0
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
        IF(ST.NE.0) THEN
           RET_CODE = 11
           RETURN
        ENDIF
C
	RCDC = GUI_ARGVAL(1)
	JUL  = GUI_ARGVAL(2)
	YEAR = GUI_ARGVAL(3)
C
	IF(RCDC.EQ.0 .AND. JUL.EQ.0) THEN  !if no date set then cdc = 1
	   RCDC = 1
	ENDIF

	IF(RCDC.EQ.0) THEN
	   IF(YEAR.EQ.0) YEAR = DAYYER 	
	   DATE(VYEAR) = YEAR
	   DATE(VJUL)  = JUL
	   CALL LJDATE(DATE)
	   CDC = DATE(VCDC)
	ELSE
	   CDC = RCDC
	ENDIF
C
	IF(CDC.LE.0.OR.CDC.GT.DAYCDC) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF
C
	INT_SERIAL = .FALSE.
	IF(GUI_ARGLEN(4).NE.0) THEN
	   INTSER = GUI_ARGVAL(4)
	   IF(INTSER.NE.0) INT_SERIAL = .TRUE.
        ENDIF
	IF(GUI_ARGLEN(5).NE.0 .AND. 
     *     GUI_ARGLEN(6).NE.0 .AND. .NOT.INT_SERIAL) THEN
	   EXTSER = GUI_ARGVAL(5)
	   CHKDIG = GUI_ARGVAL(6)
           ST = INPVER(CDC,EXTSER,INTSER,CHKDIG)
	   IF(ST.NE.0) THEN
	      RET_CODE = 11
	      RETURN
	   ENDIF
	ELSEIF(.NOT. INT_SERIAL) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF
C
	CALL GUIARG_INIT()
C
C RESULT SET 1 - transaction from TMF file.
C
	NUM_COLS = 20
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
C IF CDC IS TODAYS CDC, FIRST TRY TO READ TMF 
C
	IN_TMF = .FALSE.
	IF(CDC.EQ.DAYCDC) THEN 
10	   CONTINUE
	   CALL GUI_RLOG(INTSER,LOGBUF,ST)
	   IF(ST.GT.0) THEN
	      INTSER = INTSER - 1
	      GOTO 10
	   ENDIF
	   IF(ST.NE.0) THEN
	      CALL GUIARG_NO_DATA(OUTBUF,NUM_COLS)
	      GOTO 30
	   ENDIF
	   IN_TMF = .TRUE.
	   CALL LOGTRA(TRABUF,LOGBUF)

           CALL GETBI(INTSER,BLOCK,INDEX,OFF)

	   CALL TRNSNP1(TRABUF,BLOCK,INDEX)
C
	   CALL GUIARG_INT4(OUTBUF,TRABUF(TTER))
	   DO I=3,21
              CALL GUIARG_CHAR(OUTBUF,NEW(1,I),80)
           ENDDO
	ELSE
	   CALL GUIARG_NO_DATA(OUTBUF,NUM_COLS)
	   GOTO 30
	ENDIF
C
30	CONTINUE
C
	NUM_COLS = 20
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
C TRY TO READ TCF
C
	IF( CDC.NE.DAYCDC .OR. IN_TMF .AND.
     *      TRABUF(TTYP).EQ.TVAL .AND. TRABUF(TVEXC).NE.0) THEN

	   IF(TRABUF(TTYP).EQ.TVAL.AND.TRABUF(TVEXC).NE.0) INTSER=TRABUF(TVEXC)
	   KEY(1) = CDC
	   KEY(2) = INTSER
           CALL IOPEN(SFNAMES(1,TCF),TCFLUN,LREC*2,LCDC,LSER*2-1,ST)
           IF(ST.NE.0) THEN
              CALL OPS('Failed to open TCF.FIL',ST,0)
              RET_CODE = 11
              RETURN
           ENDIF  
	   CALL IREAD(KEY,LOGBUF,TCFLUN,ST)
           CALL ICLOSE(TCFLUN,DUMMY,ST1)
           IF(ST1.NE.0) CALL OPS('Failed to close TCF.FIL',ST,0)
	   IF(ST.NE.0) THEN
              CALL GETBI(INTSER,BLOCK,INDEX,OFF)
              CALL LOGTRA(WRKBUF,LOGBUF)
              CALL TRNSNP1(WRKBUF,BLOCK,INDEX)
	      CALL GUIARG_INT4(OUTBUF,WRKBUF(TTER))
              DO I=3,21
                 CALL GUIARG_CHAR(OUTBUF,NEW(1,I),80)
              ENDDO
           ELSE
              CALL GUIARG_NO_DATA(OUTBUF,NUM_COLS)
              GOTO 40
           ENDIF
	ELSE
           CALL GUIARG_NO_DATA(OUTBUF,NUM_COLS)
           GOTO 40
	ENDIF
C
40	CONTINUE
C
	NUM_COLS = 8
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
C TRY TO READ VLF
C
	IF( ( IN_TMF .AND. 
     *        (TRABUF(TTYP).EQ.TVAL.OR.TRABUF(TTYP).EQ.TREF).AND.
     *         TRABUF(TGAMTYP).NE.TPAS .AND.
     *         TRABUF(TVSER).NE.0) .OR. 
     *       (.NOT. IN_TMF .AND. .NOT. INT_SERIAL .AND. CDC.NE.DAYCDC)) THEN
             IF( IN_TMF ) THEN
	         CDC    = TRABUF(TVCDC)	
		 INTSER = TRABUF(TVSER)
	     ENDIF

	     KEY(1) = CDC
	     KEY(2) = INTSER
C
             CALL IOPEN(SFNAMES(1,VLF),VLFLUN,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
             IF(ST.NE.0) THEN
                CALL OPS('Failed to open VLF.FIL',ST,0)
                RET_CODE = 11
                RETURN
             ENDIF
             CALL IREAD(KEY,V4BUF,VLFLUN,ST)
             CALL ICLOSE(VLFLUN,DUMMY,ST1)
             IF(ST1.NE.0) CALL OPS('Failed to close VLF.FIL',ST1,0)
	     IF(ST.NE.0) THEN	
		CALL GUIARG_NO_DATA(OUTBUF,NUM_COLS)
		GOTO 50
             ENDIF
C
	     CALL LOGVAL(VALREC,V4BUF)
             TOTPAY = VALREC(VPAMT)+VALREC(VKPAMT)+VALREC(VRAMT)
             TOTTAX = VALREC(VTAMT)+VALREC(VKTAMT)
C
             WRITE(XNEW(3),902) VALST(VALREC(VSTAT)),VALREC(VGAM),
     *                     GTNAMES(VALREC(VGTYP)),
     *                     CSMONY(VALREC(VPAMT),11,VALUNIT)
             WRITE(XNEW(4),903) VALREC(VFRAC),VALREC(VPZOFF),
     *                     VALREC(VGIND),
     *                     CSMONY(VALREC(VKPAMT),11,VALUNIT)
             WRITE(XNEW(5),904) VALREC(VSCDC),VALREC(VSTER),
     *                     VALREC(VSSER),CSMONY(0,11,VALUNIT)
             WRITE(XNEW(6),905) VALREC(VCCDC),VALREC(VCTER),
     *                     VALREC(VCSER),CSMONY(TOTPAY,11,VALUNIT)
             WRITE(XNEW(7),906) VALREC(VLCDC),VALREC(VLTER),
     *                     VALREC(VLSER),CSMONY(TOTTAX,11,VALUNIT)
             WRITE(XNEW(8),907) VALREC(VEXP),VALREC(VKEXP),
     *                     VALREC(VWCDC), CSMONY(VALREC(VRAMT),11,BETUNIT)
             WRITE(XNEW(9),908) VALREC(VBNKID),VALREC(VBNKNUM),VALREC(VKGME)

	     CALL GUIARG_INT4(OUTBUF,VALREC(VCTER))
	     DO I=3,9	
		CALL GUIARG_CHAR(OUTBUF,NEW(1,I),80)
	     ENDDO 
	ELSE
	     CALL GUIARG_NO_DATA(OUTBUF,NUM_COLS)
	     GOTO 50
	ENDIF
C
50	CONTINUE

	CALL GUIARG_SET_MESLEN(MES_LEN)
C
902     FORMAT(1X,'Status     ',A4,5X,'Game       ',I4,5X,
     *            'Gtype  ',A8,5X,'Regpay ',A11)
903     FORMAT(1X,'Fractions  ',I4,5X,'# wins     ',I4,5X,
     *            'Gindx   ',I8,4X,'Kikpay1',A11)
904     FORMAT(1X,'Scdc       ',I4,5X,'Ster       ',I4,5X,
     *            'Sser   ',I9,4X,'Kikpay2',A11)
905     FORMAT(1X,'Ccdc       ',I4,5X,'Cter       ',I4,5X,
     *            'Cser   ',I9,4X,'Totpay ',A11)
906     FORMAT(1X,'Lcdc       ',I4,5X,'Lter       ',I4,5X,
     *            'Lser   ',I9,4X,'Wintax ',A11)
907     FORMAT(1X,'Expdrw     ',I4,5X,'Kexpdrw    ',I4,5X,
     *            'Winsel cdc  ',I4,4X,'Refund ',A11)
908     FORMAT(1X,'Bnk ID ',I8,5X,'Bank # ',I8,5X,'Kgame       ',I4)
	END
