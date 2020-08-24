C  GXSRC:ENC_DESCRYPT.FOR
C  
C  $Log:   GXAGBR:[GOLS]ENC_DESCRYPT.FOV  $
C  
C     Rev 1.2   25 Jan 1994 15:43:08   JPJ
C  Updated procom TERNUM location from 2 bytes to 4 bytes
C  
C     Rev 1.1   03 Jan 1994 20:21:34   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:41:34   SYSTEM
C  Initial revision.
C
C
C
C V04 14-MAR-03 GPW DESENCR PRO(TERNUM -> HPRO(TERNUM
C V03 13-MAR-03 GPW DESENCR TAKEN FROM UK
C V02 25-MAR-92 TKO fix bug with length of decrypt
C V01 14-NOV-91 TKO Initial release
C
C	DES ENCRYPTION/DECRYPTION PROCESSING ROUTINES
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
C Copyright 1992 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C       ADDITIONAL ROUTINES ARE "BUILT IN" IN THIS MODULE
C
C       DESENCBF(BUFFER,STATUS)		        - PREPARE TO DES ENCRYPT BUFFER
C       DESDECBF(BUFFER,STATUS)		        - PREPARE TO DES DECRYPT BUFFER
C       DESCHKBF(BUFFER,ENCRYPTION_TYPE,STATUS) - CHECK IF ANYTHING
C                                                 WAS ENCRYPTED OR DECRYPTED
C                                                 BY DES ENCRYPTION/DECRYPTION
C
C
C***************************************************
C
C       DESENCBF(BUFNUM,STATUS)
C       IN:
C       BUFNUM     -    PROCOM BUFFER USED
C       OUT:
C       STATUS     -    0 IF OPERATION "QUEUED TO BE EXECUTED"
C
C       ENCRYPT   BUFFER WITH DATA WITH DATA, THIS SUBROUTINE ONLY STARTS
C       ENCRYPTION PROCESS, ACTUAL ENCRYPTION RESULTS ARE OBTAINED FROM
C       SUBROUTINE DESCHKBF
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DESENCBF(BUFNUM,STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
	INCLUDE 'INCLIB:DESCOM.DEF'
C
        INTEGER*4 BUFNUM	      !PROCOM BUFFER NUMBER TO ENCRYPT
	INTEGER*4 STATUS	      !STATUS: 0 = OK, ELSE ERROR
C
	INTEGER*4 BUFLEN
	INTEGER*4 TERM
C
C
C
C Check to see if list is full (should never happen)
C
	IF( MOD(PUTPOINTER, CTRLCNT)+1 .EQ. GETPOINTER )THEN
	  TYPE *,IAM(),'Control list full - CALL SOFTWARE ',
     *                                   PUTPOINTER,GETPOINTER
	  STATUS = -1
	  GOTO 9000
	ENDIF
C
C Check to be sure encryption length is at least 8 bytes, but not more
C than maximum output length.
C
	BUFLEN = HPRO(OUTLEN, BUFNUM) - 2
	IF(BUFLEN.LT.8 .OR. BUFLEN.GT.OUTLEN_MAX-2)THEN
	  STATUS = -2
	  GOTO 9000
	ENDIF
C
C Get terminal # so we can determine KEY
C
	TERM = HPRO(TERNUM, BUFNUM)
	IF( TERM.LT.1 .OR. TERM.GT.NUMAGT )THEN
	  STATUS = -3
	  GOTO 9000
	ENDIF
C
C Now put the buffer into the control list for encryption
C
	CTRLBUF(PUTPOINTER).CTRL_STAT   = 0
	CTRLBUF(PUTPOINTER).CTRL_FUNC   = CTRL_FUNC_K_DESECB
	CTRLBUF(PUTPOINTER).CTRL_ENCDEC = CTRL_ENCDEC_K_ENC
	CTRLBUF(PUTPOINTER).CTRL_MODE   = CTRL_MODE_K_GTECH
	CTRLBUF(PUTPOINTER).CTRL_SIZE	= BUFLEN
	CTRLBUF(PUTPOINTER).CTRL_INOFF	= %LOC( BPRO(BINPTAB+2,BUFNUM) )
     *			                 -%LOC( FRST_PROCOM(1) )
	CTRLBUF(PUTPOINTER).CTRL_OUTOFF	= %LOC( BPRO(BINPTAB+2,BUFNUM) )
     *			                 -%LOC( FRST_PROCOM(1) )
	CTRLBUF(PUTPOINTER).CTRL_KEY1	= KEYTAB(1,TERM)
	CTRLBUF(PUTPOINTER).CTRL_KEY2	= KEYTAB(2,TERM)
C
	CTRLBUF(PUTPOINTER).CTRL_USER1	= BUFNUM	      !Save for later
C
	CTRLBUF(PUTPOINTER).CTRL_FLAG	= CTRL_FLAG_K_DOIT    !START IT UP
C
C Now point to next available slot
C
	PUTPOINTER = MOD(PUTPOINTER,CTRLCNT) + 1
C
C Keep count of encryptions
C
	ENCRYPTCNT = ENCRYPTCNT + 1
C
	STATUS = 0
C
9000	CONTINUE
	RETURN
	END
C
C***************************************************************
C
C       DESDECBF(BUFNUM,STATUS)
C       IN:
C       BUFNUM     -    PROCOM BUFFER USED
C       OUT:
C       STATUS     -    0 IF OPERATION "QUEUED TO BE EXECUTED"
C
C       DECRYPT   BUFFER WITH DATA WITH DATA, THIS SUBROUTINE ONLY STARTS
C       DECRYPTION PROCESS, ACTUAL ENCRYPTION RESULTS ARE OBTAINED FROM
C       SUBROUTINE DESCHKBF
C
C       FOR DES ENCRYPTION
C       BUFFERS TO CRYPT RESIDE ON DES ENCQUE LIST,
C       BUFFERS TO ENCRYPT HAVE BUFFER POINTER POSITIVE
C       BUFFERS TO DECRYPT HAVE BUFFER POINTER NEGATIVE
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DESDECBF(BUFNUM,STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
	INCLUDE 'INCLIB:DESCOM.DEF'
C
        INTEGER*4 BUFNUM	      !PROCOM BUFFER NUMBER TO ENCRYPT
	INTEGER*4 STATUS	      !STATUS: 0 = OK, ELSE ERROR
C
	INTEGER*4 BUFLEN
	INTEGER*4 TERM
C
C
C
C Check to see if list is full (should never happen)
C
	IF( MOD(PUTPOINTER, CTRLCNT)+1 .EQ. GETPOINTER )THEN
	  TYPE *,IAM(),'Control list full - CALL SOFTWARE ',
     *                                   PUTPOINTER,GETPOINTER
	  STATUS = -1
	  GOTO 9000
	ENDIF
C
C Check to be sure encryption length is at least 8 bytes, but not more
C than maximum output length.
C
	BUFLEN = HPRO(INPLEN, BUFNUM) - 2
	IF(BUFLEN.LT.8 .OR. BUFLEN.GT.OUTLEN_MAX-2)THEN
	  STATUS = -2
	  GOTO 9000
	ENDIF
C
C Get terminal # so we can determine KEY
C
	TERM = HPRO(TERNUM, BUFNUM)
	IF( TERM.LT.1 .OR. TERM.GT.NUMAGT )THEN
	  STATUS = -3
	  GOTO 9000
	ENDIF
C
C Now put the buffer into the control list for decryption
C
	CTRLBUF(PUTPOINTER).CTRL_STAT   = 0
	CTRLBUF(PUTPOINTER).CTRL_FUNC   = CTRL_FUNC_K_DESECB
	CTRLBUF(PUTPOINTER).CTRL_ENCDEC = CTRL_ENCDEC_K_DEC
	CTRLBUF(PUTPOINTER).CTRL_MODE   = CTRL_MODE_K_GTECH
	CTRLBUF(PUTPOINTER).CTRL_SIZE	= BUFLEN
	CTRLBUF(PUTPOINTER).CTRL_INOFF	= %LOC( BPRO(BINPTAB+2,BUFNUM) )
     *			                 -%LOC( FRST_PROCOM(1) )
	CTRLBUF(PUTPOINTER).CTRL_OUTOFF	= %LOC( BPRO(BINPTAB+2,BUFNUM) )
     *			                 -%LOC( FRST_PROCOM(1) )
	CTRLBUF(PUTPOINTER).CTRL_KEY1	= KEYTAB(1,TERM)
	CTRLBUF(PUTPOINTER).CTRL_KEY2	= KEYTAB(2,TERM)
C
	CTRLBUF(PUTPOINTER).CTRL_USER1	= BUFNUM	      !Save for later
C
	CTRLBUF(PUTPOINTER).CTRL_FLAG	= CTRL_FLAG_K_DOIT    !START IT UP
C
C Now point to next available slot
C
	PUTPOINTER = MOD(PUTPOINTER,CTRLCNT) + 1
C
C Keep count of decryptions
C
	DECRYPTCNT = DECRYPTCNT + 1
C
	STATUS = 0
C
9000	CONTINUE
	RETURN
	END
C
C*****************************************************************
C
C       DESCHKBF(BUFNUM,TYPE,STATUS)
C       OUT:
C       BUFNUM  -   BUFFER WITH  DATA
C       OPTYPE  -   OPERATION TYPE
C       STATUS  -   0 IF ANY DATA RECEIVED
C
C       CHECK IF ANY DATA WAS DECRYPTED OR ENCRYPTED
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DESCHKBF(BUFNUM,OPTYPE,STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
	INCLUDE 'INCLIB:DESCOM.DEF'
C
        INTEGER*4 BUFNUM	      !PROCOM BUFFER NUMBER TO ENCRYPT
	INTEGER*4 OPTYPE	      !ENCRYPTION OR DECRYPTION FLAG
	INTEGER*4 STATUS	      !STATUS: 0 = OK, ELSE ERROR
C
C
C
C
C Check next entry & return with one of the following:
C	list is empty (return with status = -1)
C	list is not empty but no more done (status = -2)
C	we have found a completion (status = 0)
C
C
	IF( GETPOINTER.EQ.PUTPOINTER )THEN
	  STATUS = -1				!LIST IS EMPTY
	  GOTO 9000
	ENDIF
C
	IF( CTRLBUF(GETPOINTER).CTRL_FLAG.EQ.CTRL_FLAG_K_DOIT .OR.
     *      CTRLBUF(GETPOINTER).CTRL_FLAG.EQ.CTRL_FLAG_K_INIT )THEN
	  STATUS = -2
	  GOTO 9000
	ENDIF
C
C Anything other than a GOOD result means something very bad has happened.
C In this case we should disable encryption/decryption.
C
	IF( CTRLBUF(GETPOINTER).CTRL_FLAG.NE.CTRL_FLAG_K_GOOD )THEN
	  TYPE *,IAM(),'Encryption/decryption error '
	  CALL PRINTCTRL(GETPOINTER)
	  CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C Now return the buffer number & tell whether it was an encrypt or decrypt
C
	BUFNUM = CTRLBUF(GETPOINTER).CTRL_USER1
C
	IF( CTRLBUF(GETPOINTER).CTRL_ENCDEC.EQ.CTRL_ENCDEC_K_ENC )THEN
          OPTYPE = ENC_TYPE_ENCRYPT
	ELSE
	  OPTYPE = ENC_TYPE_DECRYPT
	ENDIF
C
C Now point to next 'get' slot
C
	GETPOINTER = MOD(GETPOINTER,CTRLCNT) + 1
c
	STATUS = 0
C
9000	CONTINUE
	RETURN
	END
