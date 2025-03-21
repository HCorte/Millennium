C GUI_036.FOR
C
C V02 01-Jan-2010 FJG
C V01 01-MAR-2001 HXK Initial release.
C
C PASSIVE VALIDATION RECORD DETAILS
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
	SUBROUTINE GUI_036(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	! INCLUDE 'INCLIB:DESTRA.DEF'
	! INCLUDE 'INCLIB:PRMLOG.DEF'
	! INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:TTNAMES.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
        INCLUDE 'INCLIB:GUIFIL.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VDETAIL.DEF'
	INCLUDE 'INCLIB:VALPASFIL.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
C       INCLUDE 'INCLIB:FILESTRUCS.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 ST,J,K
	INTEGER*4 NUM_COLS,NUM_ROWS
	INTEGER*4 GNUM,DRAW
	INTEGER*4 WEEK,YEAR,TCKNBR,GIND,TCKSER,TCKFRC
	INTEGER*4 EMIOFF
	INTEGER*4 PRIZES(6,VMAX)
	INTEGER*4 PRZS,SHRS,DIV
	INTEGER*4 BLANK/'    '/
	INTEGER*4 VLUN
        INTEGER*4 VKEY(2)	
        INTEGER*4 VBUF(I4BUCSIZ)        
        INTEGER*4 PCDC
        CHARACTER*30 ERRM
        CHARACTER*80 MYLINE
C
	CHARACTER*24 CPRIZES(VMAX)
	EQUIVALENCE (CPRIZES,PRIZES)
C
	RET_CODE = 0
C
	CALL FASTSET(BLANK,PRIZES(1,1),6*VMAX)
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
        IF(ST.NE.0) THEN
           RET_CODE = 11
           RETURN
        ENDIF
C
	GIND = GUI_ARGVAL(1)
	YEAR = GUI_ARGVAL(2)
	WEEK = GUI_ARGVAL(3)
	TCKNBR = GUI_ARGVAL(4)
	TCKSER = GUI_ARGVAL(5)
	TCKFRC = GUI_ARGVAL(6)	

	YEAR = MOD(YEAR,10)
	
	NUM_COLS = 2
	NUM_ROWS = 1	

	CALL GUIARG_INIT()

	IF(GIND.LT.1 .OR. GIND.GT.NUMPAS) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
	   WRITE(ERRM,1000) 'Bad Game Index      ',GIND
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)	   
           GOTO 50
	ENDIF
C
	GNUM = GTNTAB(TPAS,GIND)
	IF(GNUM.LE.0) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
	   WRITE(ERRM,1000) 'Bad Game Number     ',GNUM
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)	   
           GOTO 50
	ENDIF
C
	IF(WEEK.LT.1 .OR. WEEK.GT.53) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)	  
	   WRITE(ERRM,1000) 'Bad WEEK         ',WEEK
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)
           GOTO 50
	ENDIF
C
	EMIOFF = PASEXTDRW(WEEK,YEAR,GIND)
C
	IF(EMIOFF.LT.1.OR.EMIOFF.GT.PAGEMI) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)	  
	   WRITE(ERRM,1000) 'Bad emission offset ',EMIOFF
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)
           GOTO 50
	ENDIF
C
        IF(TCKFRC.LE.0.OR.TCKFRC.GT.PASNOFFRA(EMIOFF,GIND)) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)          
	   WRITE(ERRM,1000) 'Bad ticket fraction ',TCKFRC
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)
           GOTO 50
        ENDIF
C
        IF(TCKSER.LE.0.OR.TCKSER.GT.PASNUMSER(EMIOFF,GIND)) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)          
	   WRITE(ERRM,1000) 'Bad ticket serie    ',TCKSER
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)
           GOTO 50
        ENDIF        
C
        IF(TCKNBR.LT.0.OR.TCKNBR.GT.PASNUMTCK(EMIOFF,GIND)) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)          
	   WRITE(ERRM,1000) 'Bad ticket number   ',TCKNBR
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)
           GOTO 50
        ENDIF        
C
        IF(.NOT.FILEXIST(CPASVPFFIL(EMIOFF,GIND))) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)          
	   WRITE(ERRM,1000) CPASVPFFIL(EMIOFF,GIND),-1
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)
           GOTO 50
	ENDIF	
C
        CALL FIND_AVAILABLE_LUN(VLUN,ST)
        IF(ST.NE.0) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)          
	   WRITE(ERRM,1000) 'No lun available    ',ST     
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)
           GOTO 50
        ENDIF
               
        CALL IOPEN(PASVPFFIL(1,EMIOFF,GIND),VLUN,VPFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF(ST.NE.0) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)          
	   WRITE(ERRM,1000) CPASVPFFIL(EMIOFF,GIND),-2
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)
           GOTO 50
        ENDIF   
        
        VKEY(1) = TCKFRC
        VKEY(2) = ISHFT(TCKSER,24) + TCKNBR       
        
        CALL IREAD(VKEY,V4BUF_PAS,VLUN,ST)
	IF(ST.EQ.ERRRNF) THEN	
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)	
	   WRITE(ERRM,1000) 'Record NOT FOUND    ',ST
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)	
 	   GOTO 50
	ELSE IF(ST.NE.0) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)	  
	   WRITE(ERRM,1000) 'Read ERROR          ',ST
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)	
 	   GOTO 50
        ENDIF

        CALL ICLOSE(VLUN,VBUF,ST)
	IF(ST.NE.0) THEN
           CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)	  
	   WRITE(ERRM,1000) CPASVPFFIL(EMIOFF,GIND),-3
	   CALL GUIARG_INT4(OUTBUF,0)
 	   CALL GUIARG_CHAR(OUTBUF,%REF(ERRM),20)
           GOTO 50
	ENDIF	
C
C DECODE FILE RECORD TO MEMORY RECORD
C
	CALL LOGPAS(VALREC,V4BUF_PAS)
	CALL DLOGPAS(VALREC,VDETAIL)
C
C ENCODE PRIZE DETAIL
C
        DO PRZS = 1, VALREC(VPZOFF)
          DRAW = VDETAIL(VDRW,PRZS)
          DIV  = VDETAIL(VDIV,PRZS)
          SHRS = VDETAIL(VSHR,PRZS)
          WRITE (CPRIZES(PRZS),820) PRZS, DRAW, DIV, SHRS
        ENDDO
!=======START PUSHING OUT=======================================================        
        NUM_COLS = 14
        CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
        CALL GUIARG_INT4(OUTBUF,VALREC(VCTER))
        WRITE(MYLINE,906) DRAW,TCKNBR,TCKSER,TCKFRC
        CALL GUIARG_CHAR(OUTBUF,%REF(MYLINE),80)
        WRITE(MYLINE,902) VALST(VALREC(VSTAT)),VALREC(VGAM),GTNAMES(VALREC(VGTYP)),CMONY(VALREC(VPAMT),11,VALUNIT)
        CALL GUIARG_CHAR(OUTBUF,%REF(MYLINE),80)
        WRITE(MYLINE,907) VALREC(VPZOFF),VALREC(VGIND),VALREC(VEXP),VALREC(VWCDC)
        CALL GUIARG_CHAR(OUTBUF,%REF(MYLINE),80)
        IF (VALREC(VSTAT).EQ.VCASH) THEN
            GNUM = GTNTAB(VALREC(VGTYP),VALREC(VGIND))
            PCDC = VALREC(VCCDC) + PRGDAY(GNUM)
        ELSE
            PCDC = VALREC(VPRGCDC)
        ENDIF
        WRITE(MYLINE,905) VALREC(VCCDC),VALREC(VCTER),VALREC(VCSER),PCDC
        CALL GUIARG_CHAR(OUTBUF,%REF(MYLINE),80)
        WRITE(MYLINE,909)
        CALL GUIARG_CHAR(OUTBUF,%REF(MYLINE),80)        
C
C WRITE DETAILED PRIZES
C
        DO PRZS = 1,23,3    !TOP=VMAX-2
           WRITE(MYLINE,910) ((PRIZES(J,K),J=1,6),K=PRZS,PRZS+2)
           CALL GUIARG_CHAR(OUTBUF,%REF(MYLINE),80)
        ENDDO
 
50	CONTINUE

	CALL GUIARG_SET_MESLEN(MES_LEN)
	RETURN
C
C =========================================================
C
820     FORMAT(I3,5X,I6,I3,I4)
902     FORMAT('Status ',A4,3X,'Game   ',I4,3X,
     *         'Gtype  ',A8,3X,'Regpay  ',A11)
905     FORMAT('Ccdc   ',I4,3X,'Cter   ',I5,3X,
     *         'Cser  ',I9,3X,'Prgcdc  ',I4)
906     FORMAT('Emission',I6,X,'Ticket ',1X,I6,X,
     *         'Serie   ',I9,3X,'Tenth  ', 10X, I4)
907     FORMAT('# priz.',I6, X,'GIND   ',1X,I6,X,
     *         'EmsExp ',I6, X,'WinCDC ',2X,I4)
909     FORMAT(2('Prz     Ems Div Shr       '),
     *           'Prz     Ems Div Shr ')

910     FORMAT(3(6A4,X))
1000    FORMAT(A20,I10)
	END
