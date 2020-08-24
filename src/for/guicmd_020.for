C GUICMD_020.FOR
C
C V05 08-JAN-2016 FRP CR67 New UNSO subtype=3 for non-blocking message
C V04 22-DEC-2014 SCML Only send UNSO to MXTerminals
C V03 12-DEC-2013 FRP Send UNSO to all MXT in just 1 buffer
C V02 12-OCT-2013 FRP Include MXSRV Terminals
C V01 07-FEB-2001 HXK Initial release for AlphaGOLS
C
C UNMESS COMMAND
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
        SUBROUTINE GUICMD_020(OUTBUF,MES_LEN,RET_CODE)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:GUIMPRM.DEF'
        INCLUDE 'INCLIB:GUIARGS.DEF'
        INCLUDE 'INCLIB:GUIFIL.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
C
C
        BYTE OUTBUF(*)
        INTEGER*4 MES_LEN,RET_CODE
C
        INTEGER*4 ST,J,I
        INTEGER*4 NUM_COLS, NUM_ROWS
        INTEGER*4 STATUS,LEN
        CHARACTER*40 STATUS_STR

        INTEGER*4 AGT
        INTEGER*4 TER
        INTEGER*4 BUF
        INTEGER*4 TMPLEN
        INTEGER*4 LINE_LEN,NUM_OF_LINES
        PARAMETER(LINE_LEN=26)
        PARAMETER(NUM_OF_LINES=5)
        INTEGER*4 LIST(NUMAGT)          ! V02
        INTEGER*4 COUNT                 ! V02
        BYTE MSGTEXT_B((LINE_LEN*NUM_OF_LINES)+2) !132 bytes
        CHARACTER*25 MSGTEXT_L(LINE_LEN,NUM_OF_LINES)
        INTEGER*2 MSGTEXT_D(NUM_OF_LINES*LINE_LEN)
        INTEGER*2 CONTRL
        INTEGER*2 CONTRL_BLOCKING       ! V05
        INTEGER*2 CONTRL_NON_BLOCKING   ! V05
        LOGICAL   SEND_TO_MXT_ONLY      ! V04

        INTEGER*4 BLOCKING_FLAG_VALUE                 ! V05
        INTEGER*4 BLOCKING_VALUE, NON_BLOCKING_VALUE  ! V05
        PARAMETER(BLOCKING_VALUE=1)                   ! V05
        PARAMETER(NON_BLOCKING_VALUE=3)               ! V05


        EQUIVALENCE(MSGTEXT_B(1),MSGTEXT_L(1))
        EQUIVALENCE(MSGTEXT_B(1),MSGTEXT_D(1))
C
C       DATA CONTRL/ZB120/               ! V05
        DATA CONTRL_BLOCKING/ZB120/      ! V05
        DATA CONTRL_NON_BLOCKING/ZB320/  ! V05
C
        RET_CODE = 0
        STATUS   = 0
        STATUS_STR = ' '
        SEND_TO_MXT_ONLY = .TRUE. ! ONLY SEND UNSO TO MXTERMINALS !V04
C
        CALL GUI_GETPARAMS(OUTBUF,ST)
        IF(ST.NE.0) THEN
          RET_CODE = 11
          RETURN
        ENDIF
C
        AGT = GUI_ARGVAL(1)
        TER = GUI_ARGVAL(2)
        IF(TER.EQ.0 .AND. AGT.EQ.0) THEN  !unmess to all terminals
          COUNT=1                ! Send first buffer for Unso message to all X2X terminals
          LIST(1)=-1
          DO 60 J=1,NUMAGT
            IF(BTEST(AGTTAB(AGTTYP,J),AGTMXT)) THEN
              COUNT = 2          ! V12 Send a second buffer for Unso message to all MXS terminals
              LIST(2)=-1
              GOTO 50
            ENDIF
60        CONTINUE
          GOTO 50
        ENDIF
        IF(TER.GE.1.AND.TER.LE.NUMAGT) THEN
          COUNT = 1
          LIST(COUNT) = TER  !V02
          GOTO 50
        ENDIF
        DO J=1,NUMAGT
          IF(AGTTAB(AGTNUM,J).EQ.AGT) THEN
            TER = J
            COUNT = 1
            LIST(COUNT) = TER  !V02
            GOTO 50
          ENDIF
        ENDDO
        STATUS_STR = 'Invalid agent or terminal number'
        STATUS = 1
        GOTO 100

50      CONTINUE

        IF(GUI_ARGLEN(3).GT.0 .AND.
     *     GUI_ARGLEN(3).LE.LINE_LEN*NUM_OF_LINES) THEN
           CALL MOVBYT(B_GUI_ARGCHAR(1,3),1,MSGTEXT_B,1,GUI_ARGLEN(3))
        ELSE
           STATUS_STR = 'Invalid message length'
           STATUS = 2
           GOTO 100
        ENDIF

C       Set msg terminator !!this is currently handled by WebVision
C       ------------------ 
        IF(GUI_ARGLEN(3).LT.LINE_LEN*NUM_OF_LINES) THEN
           MSGTEXT_B(GUI_ARGLEN(3)+1) = '^'
        ELSE
           MSGTEXT_B(LINE_LEN*NUM_OF_LINES+1) = ' '
           MSGTEXT_B(LINE_LEN*NUM_OF_LINES+2) = '^'
        ENDIF

C (V05) GET BLOCKING FLAG VALUE FROM GUI
        BLOCKING_FLAG_VALUE = GUI_ARGVAL(4)
C       TYPE*, 'BLOCKING FLAG: ',BLOCKING_FLAG_VALUE
        CONTRL = CONTRL_BLOCKING        !BLOCKING AS DEFAULT (CURRENT B120)
        IF(BLOCKING_FLAG_VALUE .EQ. NON_BLOCKING_VALUE)
     *    CONTRL = CONTRL_NON_BLOCKING  !NON-BLOCKING (NEW B320)
C
        DO 90 I=1,COUNT
C
          IF(I.EQ.1 .AND. LIST(I).EQ.-1) THEN  !ALL TERMINALS THAT ARE NOT MXT !V04
            IF(SEND_TO_MXT_ONLY) GOTO 90       !SKIP ALL TERMINALS THAT ARE NOT MXT !V04
          ENDIF                                !V04
C
          IF(LIST(I).GT.0.AND.LIST(I).LE.NUMAGT) THEN           !V04
            IF(.NOT. BTEST(AGTTAB(AGTTYP,LIST(I)),AGTMXT)) THEN !TERMINAL IS NOT MXT !V04
              IF(SEND_TO_MXT_ONLY) GOTO 90                      !SKIP TERMINAL THAT IS NOT MXT !V04
            ENDIF                                               !V04
          ENDIF                                                 !V04
C
          CALL GETBUF(BUF)
          IF(BUF.LE.0) THEN
            STATUS_STR = 'Buffer allocation error'
            STATUS = 3
            CALL XWAIT(10,2,ST)
            GOTO 100
          ENDIF
C
C TRANSFER MESSAGE TO BUFFER
C
          LEN = LINE_LEN * NUM_OF_LINES
          HPRO(OUTLEN,BUF)=LEN+2
          HPRO(MSGNUM,BUF)=0
          HPRO(TRCODE,BUF)=TYPUNS
          HPRO(TERNUM,BUF)=LIST(I)             !V02
C
          IF(I.EQ.2 .AND. LIST(I).EQ.-1) THEN  ! V03 Second buffer for Unso message to all MXS terminals
             HPRO(PRCSRC,BUF)=MXS_COM          !MXSRV
          ENDIF
C	   
          IF(LIST(I).GT.0.AND.LIST(I).LE.NUMAGT) THEN      !MXSRV
            IF(BTEST(AGTTAB(AGTTYP,LIST(I)),AGTMXT)) THEN  !V02
              HPRO(PRCSRC,BUF)=MXS_COM                     !V02
            ENDIF                                          !V02
          ENDIF                                            !MXSRV
C
          HPRO(INPTAB*2-1,BUF)=CONTRL
          TMPLEN=LEN/2+1
          DO J=1,TMPLEN
            HPRO(INPTAB*2-1+J,BUF)=MSGTEXT_D(J)
          ENDDO
C
C QUEUE BUFFER TO THE OUTPUT QUEUE
C
          CALL QUETRA(LOG,BUF)

90      CONTINUE
C
C SEND DATA TO GUI
C
100     CONTINUE
        CALL GUIARG_INIT()
C
        NUM_COLS = 2
        NUM_ROWS = 1
        CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
C STATUS BACK
C
        CALL GUIARG_INT4(OUTBUF,STATUS)	
        CALL GUIARG_CHAR(OUTBUF,%REF(STATUS_STR),40)	
C
        CALL GUIARG_SET_MESLEN(MES_LEN)
C
        RETURN
        END
