C
C PROGRAM WINUPD
C
C
C V11 07-JAN-2010 FJG  Lotto2 Changes: Include OPS_REC.CWEEK
C     26-JAN-2011 FJG  Out of bounds issue
C V10 16-MAY-2001 EPH  INCLUDE JULIAN DATE IN EXTERNAL CDC IN OP
C V09 11-JAN-2001 EPH  RECEIVE ORDER INFORMATION, WRITE IT TO OPS.FIL 
C V08 11-APR-2000 UXN  WINUPD.DEF added.
C V07 15-FEB-2000 UXN  Added BIGWRL flas
C V06 10-SEP-1993 HXK  Added Win Reserve functionality
C V05 01-SEP-1993 HXK  Added BIGPPP flag
C V04 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V03 26-FEB-1992 GCAN ADDED CHECK FOR WINUPD ON POSTPONED EVENTS.
C V02 12-NOV-1991 MTK  INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C PROGRAM TO POST WINNING AMOUNTS TO VLF RECORDS
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
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM WINUPD
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:OPS_REC.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:WINUPD.DEF'
C
	INTEGER*4  TUBSIZ, VSIZE
	PARAMETER (TUBSIZ=I4BUCSIZ*7)
	PARAMETER (VSIZE=4000000)
C
	INTEGER*4 VST, CNTWRIT, CNTREAD, CNTUPD, ST
	INTEGER*4 VLFBUF(TUBSIZ),NEWBUF(TUBSIZ)
	BYTE      V1BUF(VFLEN*4*4)
	EQUIVALENCE (V4BUF,V1BUF)

	INTEGER*4 WINS(6), JOKERDIV, TOTAL, TOTALJOKER            !V09 - V11 Max DIVS to 6
        LOGICAL   HIPRIZE                                         !V09
        INTEGER*4 ORDER_NUMBER(MAXGAM)		                  !v09
	INTEGER*4 CLOSE_CDC(MAXGAM)                               !V09
        INTEGER*4 YEAR, WEEK                                      !V09

	CALL COPYRITE

C
C	GET LAST OP NUMBER
C
        CALL GET_ORDER_NUMBER (ORDER_NUMBER,ST)                           !v09
        IF (ST.NE.0) THEN						  !v09
           TYPE*,IAM(),'>>> Nao foi possivel obter ultimo numero de OP.'  !v09
           CALL GPAUSE          					  !v09
        ENDIF								  !v09
C
C	OPEN OP FILE
C
	CALL OPEN_OPS ('KEYED', ST)
        IF (ST.NE.0) THEN
            TYPE*,IAM(),' '
            TYPE*,IAM(),'=================================================='
            TYPE*,IAM(),'>> ERRO DE ABERTURA NO ARQUIVO  ** FILE:OPS.FIL **'       
            TYPE*,IAM(),'=================================================='
            TYPE*,IAM(),' '
            CALL GPAUSE
         ENDIF             

C
C INITIALIZE GAME RECORDS
C
	CALL FASTSET(0,WINTAB,NUMAGT*2)
	CALL UPDINT(CLOSE_CDC)                !V09
C
C OPEN VALIDATION FILES
C
	CALL IOPEN(SCFSFN(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
	IF(ST.NE.0) CALL FILERR(SCFSFN(1,VLF),1,ST,0)
	CALL ITUBSIZE(VLF,TUBSIZ)
C
C
	CALL IOPEN(SCFSFN(1,VLC),VLC,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
	IF(ST.NE.0) CALL FILERR(SCFSFN(1,VLC),1,ST,0)
	CALL ITUBSIZE(VLC,TUBSIZ)
C
C SCAN VALIDATION FILE
C
	TYPE*,IAM(),' Posting prize values to validation records'
	CNTUPD=0
	CNTREAD=0
	CNTWRIT=0
100	CONTINUE
	CALL ISREAD(V4BUF,VLF,VLFBUF,ST)
	IF(ST.EQ.ERREND) THEN
	  CALL ICLOSE(VLF,VLFBUF,ST)
	  IF(ST.NE.0) CALL FILERR(SCFSFN(1,VLF),4,ST,0)
	  CALL ICLOSE(VLC,NEWBUF,ST)
	  IF(ST.NE.0) CALL FILERR(SCFSFN(1,VLC),4,ST,0)
	  CALL POSTSTS
	  TYPE*,IAM(),' Number of tickets read        = ',CNTREAD
	  TYPE*,IAM(),' Number of tickets write       = ',CNTWRIT
	  TYPE*,IAM(),' Number of tickets update      = ',CNTUPD
	  TYPE*,IAM(),' Validation prize update complete'
          TYPE*,IAM(),' Updating win reserve file'
          CALL UPDWRF
          TYPE*,IAM(),' Win reserve file update complete'

C
C	  UPDATE OP NUMBER IN SCF
C
	  CALL UPD_ORDER_NUMBER(ORDER_NUMBER,ST)                                !V09
          IF (ST.NE.0) THEN                                                     !V09
C             TYPE*,IAM(),'>>> Nao foi possivel atualizar ultimo numero de OP.'  !v09
	     TYPE*,IAM(),'>>> Unable to update last OP number.'
             CALL GPAUSE  					        !v09
          ENDIF								        !V09
C
C	  CLOSE OP FILE	  
C
          CLOSE(OPS_LUN)      !V09

	  CALL GSTOP(GEXIT_SUCCESS)

	ENDIF

	CNTREAD=CNTREAD+1
	IF(ST.NE.0) CALL FILERR(SCFSFN(1,VLF),2,ST,0)
C
C
	VST=V1BUF(VFSTS)

	IF(VST.EQ.VNOPAY.OR.VST.EQ.VNOPRZ.OR.VST.EQ.VPPNPZ.OR.
     *     VST.EQ.VPOST.OR.VST.EQ.VPRPOST) THEN

          CALL WPRZUPD(V4BUF, WINS, JOKERDIV, TOTAL, TOTALJOKER, HIPRIZE, YEAR, WEEK)       !V09

	  IF (TOTAL.GT.0) THEN
             CALL FILL_ORDER (V4BUF, 
     *                        WINS, JOKERDIV, 
     *                        TOTAL, 
     *                        TOTALJOKER,
     *                        HIPRIZE, 
     *                        YEAR, WEEK,
     *                        CLOSE_CDC,
     *                        ORDER_NUMBER)

             WRITE(UNIT=OPS_LUN, IOSTAT=ST) OPS_REC                             !V09
             IF (ST.NE.0) THEN
                TYPE*,IAM(),'>>> Erro de escrita no arquivo de OPS'             !v09
                CALL GPAUSE 					        !v09
             ENDIF
          ENDIF

	  VST=V1BUF(VFSTS)
	  IF(VST.NE.VNOPAY.AND.VST.NE.VNOPRZ.AND.VST.NE.VPPNPZ.AND.
     *       VST.NE.VPOST.AND.VST.NE.VPRPOST) CNTUPD=CNTUPD+1
	ENDIF

	CALL ISWRIT(V4BUF,VLC,NEWBUF,ST)
	CNTWRIT=CNTWRIT+1
	IF(ST.NE.0) CALL FILERR(SCFSFN(1,VLC),3,ST,0)
	GOTO 100
	END



C	********************************************    !V09
        SUBROUTINE FILL_ORDER (V4BUF, 
     *                         WINS, 
     *                         JOKERDIV,				 
     *                         TOTAL, 
     *                         TOTALJOKER,
     *                         HIPRIZE,
     *                         YEAR,
     *                         WEEK, 
     *                         CLOSE_CDC,
     *                         ORDER_NUMBER)
C	********************************************
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:OPS_REC.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'


	INTEGER*4    WINS(*), JOKERDIV, TOTAL, TOTALJOKER
        LOGICAL      HIPRIZE
        INTEGER*4    YEAR, WEEK

	INTEGER*4    GNUM, ORDER_NUMBER(MAXGAM)
        INTEGER*4    STATUS, I
        INTEGER*4    GIND          ! V11
        INTEGER*4    GTYP          ! V11
        INTEGER*4    CCCWEK        ! V11 Function
        INTEGER*4    OWEK          ! V11

	INTEGER*4    CLOSE_CDC(*)
	INTEGER*4    EXTSER, SCHK

	INTEGER*2    DATE(12)

	INTEGER*4    SJUL

        STATUS = 0

        CALL LOGVAL(VALREC,V4BUF)

        GNUM = VALREC(VGAM)
        WRITE (OPS_REC.GAME, FMT='(I2.2)')  GNUM         

        WRITE (OPS_REC.YEARWEEK, FMT='(I4.4,I3.3)')  YEAR, WEEK         

	ORDER_NUMBER(GNUM) = ORDER_NUMBER(GNUM) + 1
        IF (ORDER_NUMBER(GNUM).GT.999999) THEN
           ORDER_NUMBER(GNUM) = 1
        ENDIF
        WRITE (OPS_REC.ORDER, FMT='(I6.6)')  ORDER_NUMBER(GNUM) 
C+++++++V11 EXCEPTION FOR NEW TOTOLOTO Q AND S++++++++++++++++++++++++++++++++++
        GTYP = VALREC(VGTYP)
        GIND = VALREC(VGIND)
        !Check first if is Totoloto Q or S (since WEEK contains the CCC value,
        !which goes from 1 to 105, so it can be 3-digits and does not fit in I2.2)
        IF(GTYP.EQ.TLTO) THEN
          IF(GIND.EQ.3.OR.GIND.EQ.4) THEN
            OWEK = CCCWEK(YEAR,WEEK,GNUM) !OWEK contains the civil week, so 2-digits fit in I2.2
            WRITE(OPS_REC.CWEEK, FMT='(I2.2)') OWEK ! OVERWRITE WITH OLD VALUE
            GOTO 1000 !we're done setting the civil week for with this order
          ENDIF
        ENDIF
        !For the rest of games other than Totoloto Q and S, WEEK contains the
        !civil week, so 2-digits fit in I2.2
        WRITE(OPS_REC.CWEEK, FMT='(I2.2)') WEEK
C+++++++V11+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

1000    CONTINUE        
        IF (HIPRIZE) THEN
           OPS_REC.HI_PRIZE = .TRUE.
C           OPS_REC.GENERATION_CDC = CLOSE_CDC(GNUM) + P(DAYHDPHI)
        ELSE
           OPS_REC.HI_PRIZE = .FALSE.
C           OPS_REC.GENERATION_CDC = DAYCDC
        ENDIF
	OPS_REC.GENERATED = .FALSE.

        OPS_REC.PAYABLE_CDC = CLOSE_CDC(GNUM) + PRGDAY(GNUM)       
	OPS_REC.PAID_SENT_SAP = .FALSE.
C
C	OBTER EXTERNAL SERIAL NUMBER
C
        CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),EXTSER,SCHK)
        DATE(VCDC)=VALREC(VSCDC)                                               
        CALL LCDATE(DATE)                                                       
        SJUL = DATE(VJUL)           
        WRITE (OPS_REC.BILHETE,FMT='(I3.3,I8.8,I3.3)') SJUL, EXTSER, SCHK

        IF (VALREC(VSTER).GT.0 .AND. VALREC(VSTER).LE.NUMAGT) THEN
           WRITE (OPS_REC.AGENT,FMT='(I7.7)') AGTTAB(AGTNUM,VALREC(VSTER))     
        ELSE
	   OPS_REC.AGENT='0000000'
	ENDIF

	DO I=1,6
           OPS_REC.WINS(I) = WINS(I) 
        ENDDO          

	OPS_REC.JOKER_DIV = JOKERDIV

	OPS_REC.TOTAL_GAME  = TOTAL - TOTALJOKER
	OPS_REC.TOTAL_JOKER = TOTALJOKER
       
  	OPS_REC.ONLINE_ORDER   = .TRUE.
  	OPS_REC.PRINTED_BY_OFF = .FALSE. 
  	OPS_REC.CLAIM          = .FALSE.
	
        RETURN
	END
