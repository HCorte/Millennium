C
C SUBROUTINE DFE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DFE.FOV                                      $
C  $Date::   17 Apr 1996 12:51:52                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2decode.for;1 **
C
C DFE.FTN
C
C V02 12-DEC-94 GPR Integrate UK changes into X2X Baseline
C V01 24-OCT-89 MBK ORIGINAL RELEASE
C
C DECODE SOME FE LAYER MESSAGE INTO TRABUF
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DFE(MESSAGE,TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
C
	INTEGER*4 MESSAGE(*), ADRLEN, CHKVAL, OFFMES
        INTEGER*4 FLAGS, VSPADDR, AAPADDR, ALTLEN_ADDR

C
        LOGICAL VSPON, AAPON
        LOGICAL BAD_ADR							  !V02
C
        AAPON = .FALSE.  !eliminate warnings
C
C DECODE THE FOLLOWING: TXFPID, TXFSSAP, TXFMDUT
C
C IF TXFMDUT=1    GET ALSO TXFDAD1, TXFDAD2, TXFVS, TXFALT1, TXFALT2
C
C IF TXFMDUT=3    GET ALSO TXFCFID, TXFCC (0TH BIT = CMD RESPONSE FLAG)
C
C IF TXFMDUT=66   GET ALSO TXFDAD1, TXFDAD2
C
C IF TXFMDUT=130  GET ALSO TXFDEC, TXFDAD1, TXFDAD2
C
C IF TXFMDUT=240  GET ALSO TXFLFID, TXFLMC (0TH BIT = ALM FLAG)
C
	CALL ILBYTE(OFFMES,MESSAGE,X2PRO_OFFSET-1)
	IF(CHKVAL(OFFMES,11,90,' MESS OFF ').NE.0) THEN
	   TRABUF(TERR) =XERR
	   TRABUF(TSTAT)=REJT
	   TRABUF(TXPTL)=X2ERR_MOFF          !MESSAGE OFFSET
	   RETURN
	ENDIF
C
	CALL ILBYTE(TRABUF(TXFPID),MESSAGE,OFFMES+X2FEMES_PROTID-2)
	CALL ILBYTE(TRABUF(TXFSSAP),MESSAGE,OFFMES+X2FEMES_HOST_ID-2)
	CALL ILBYTE(TRABUF(TXFMDUT),MESSAGE,OFFMES+X2FEMES_MESTYP-2)
	TRABUF(TXFDAD1)=0
	TRABUF(TXFDAD2)=0
C
C	***** Start V02 changes *****
C
        BAD_ADR=.FALSE.
        IF (TRABUF(TXPTL).EQ.X2ERR_BADADR) THEN
          BAD_ADR=.TRUE.
          CALL ILBYTE(ADRLEN,MESSAGE,X2PRO_ADR_LEN-1)
          CALL LOGADDR(ADRLEN,MESSAGE,X2PRO_ADR,
     *                         TRABUF,TXFDAD1)
        ENDIF
C
C	***** End V02 changes *****
C
	IF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_CMD) THEN
C
	   CALL ILBYTE(TRABUF(TXFCFID),MESSAGE,OFFMES+X2FEMES_FORMAT-2)
	   CALL MOV2TOI4(TRABUF(TXFCC),MESSAGE,OFFMES+X2FEMES_MESCOD-2)
	   TRABUF(TXFCC)=IAND(TRABUF(TXFCC),'FF'X)
C
	ELSEIF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_ACK) THEN
C
C	   ***** Start V02 changes *****
C
           IF (.NOT.BAD_ADR) THEN
	      CALL ILBYTE(ADRLEN,MESSAGE,OFFMES+X2FEMES_ADRLEN-2)
C
              CALL LOGADDR(ADRLEN,MESSAGE,OFFMES+X2FEMES_ADR-1,
     *                            TRABUF,TXFDAD1)
           ENDIF
C
	ELSEIF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_UP) THEN
C
           IF (.NOT.BAD_ADR) THEN
	      CALL ILBYTE(ADRLEN,MESSAGE,OFFMES+X2FEMES_ADRLEN-2)
C
              CALL LOGADDR(ADRLEN,MESSAGE,OFFMES+X2FEMES_ADR-1,
     *                            TRABUF,TXFDAD1)
           ENDIF
C
C	   ***** End V02 changes *****
C
           TRABUF(TXFALT1) = 0
           TRABUF(TXFALT2) = 0
           CALL ILBYTE(FLAGS,MESSAGE,OFFMES+X2FEMES_FLAGS-2) !FLAGS
C
           IF((TRABUF(TXPTL) .EQ. X2ERR_GOODVS) .OR.
     *        (TRABUF(TXPTL) .EQ. X2ERR_BADVSP))THEN
              IF(IAND(FLAGS,X2FEMES_FLAGS_AA)  .NE. 0) AAPON = .TRUE.
              VSPADDR = X2FEMES_ADR + (ADRLEN+1)/2
              CALL MOV2TOI4(TRABUF(TXFVS),MESSAGE,OFFMES+VSPADDR-2)
              IF(AAPON) THEN
                ALTLEN_ADDR = VSPADDR + X2FEMES_VER_LEN
                AAPADDR = ALTLEN_ADDR + X2FEMES_ALT_ADDLEN
                CALL ILBYTE(ADRLEN,MESSAGE,OFFMES+ALTLEN_ADDR-2)
                CALL LOGADDR(ADRLEN,MESSAGE,OFFMES+AAPADDR-1,
     *                         TRABUF,TXFALT1)
              ENDIF
           ENDIF
C
	ELSEIF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_ERR) THEN
C
C	   ***** Start V02 changes *****
C
           IF (.NOT.BAD_ADR) THEN
	      CALL ILBYTE(ADRLEN,MESSAGE,OFFMES+X2FEMES_ADRLEN-2)
C
              CALL LOGADDR(ADRLEN,MESSAGE,OFFMES+X2FEMES_ADR-1,
     *                            TRABUF,TXFDAD1)
C
           ENDIF
C
C	   ***** End V02 changes *****
C
         CALL ILBYTE(TRABUF(TXFDEC),MESSAGE,OFFMES+X2FEMES_DELIVERY-2)
C
	ELSEIF(TRABUF(TXFMDUT).EQ.X2FEMES_MESTYP_ALARM) THEN
C
	   CALL ILBYTE(TRABUF(TXFLFID),MESSAGE,OFFMES+X2FEMES_FORMAT-2)
         CALL MOV2TOI4(TRABUF(TXFLMC),MESSAGE,OFFMES+X2FEMES_MESCOD-2)
C
	ELSE
C
	   CALL OPS('**** ILLEGAL FE MESSAGE DATA TYPE ****',
     *	            TRABUF(TSER),TRABUF(TXFMDUT))
	   TRABUF(TERR) =XERR
	   TRABUF(TSTAT)=REJT
	   TRABUF(TXPTL)=X2ERR_FMDUT          !WRONG FE MES DATA TYPE
C
	ENDIF
C
	RETURN
	END
