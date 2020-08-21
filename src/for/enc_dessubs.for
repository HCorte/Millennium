C  GXSRC:ENC_DESSUBS.FOR
C  
C  $Log:   GXAGBR:[GOLS]ENC_DESSUBS.FOV  $
C  
C     Rev 1.3   11 Feb 1998 18:54:50   NXA
C  Halt DESMBXQIO/DESMBXAST loop if ASTSTOP is True [RFC 2066]
C  
C     Rev 1.2   15 Feb 1994 11:15:52   JPJ
C  Now contains support for 1 or two encpro's
C  
C     Rev 1.1   03 Jan 1994 20:22:08   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:41:44   SYSTEM
C  Initial revision.
C
C
C DESSUBS.FOR
C
C V02 13-MAR-03 GPW DESENCR TAKEN FROM UK
C V01 18-MAR-91 TKO  Set of common subroutines for DES testing
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
C
C *** DESASSIGN
C
C This routine will assign the DES driver to a channel and create a mailbox
C for communicating with it.  It also does an asynchronous QIO read to the
C mailbox to receive any messages that might be sent.  (AST is handled by
C DESMBXAST).
C
C If successful, DESCHAN and MBXCHAN will be set to the DES channel and
C Mailbox channel respectively (see DESPARAMS).
C
C Calling sequence:
C
C	CALL DESASSIGN(OFFSET,STATUS)
C
C
C Input:
C
C	OFFSET = DEVICE OFFSET
C
C Output:
C
C	STATUS = VMS returned status
C
	SUBROUTINE DESASSIGN(OFFSET,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESPARAMS.DEF'
	INCLUDE 'INCLIB:DESCOM.DEF'
C
	INTEGER*4   OFFSET,STATUS
C
	INTEGER*4   LIB$ASN_WTH_MBX
	EXTERNAL    LIB$ASN_WTH_MBX
C
	INTEGER*4   DESMBXAST
	EXTERNAL    DESMBXAST
C
C
C Assign driver channel and create temporary mailbox
C
	STATUS = LIB$ASN_WTH_MBX( %DESCR(DESDEVNAME(OFFSET)), !DEVICE NAME
     *				  %REF  (32),                 !MAX MSG SIZE
     *				  %REF  (32),                 !BUFFER QUOTA
     *				  %REF  (DESCHAN),	      !DES CHANNEL #
     *				  %REF  (MBXCHAN) )           !MAILBOX CHANNEL #
C
	IF (STATUS) THEN
	  CALL DESMBXQIO				    !start mailbox read
	ENDIF
C
	RETURN
	END
C
C
C *** DESDASSGN
C
C This routine will:
C 1)	cancel the read on the mailbox
C 2)	deassign the mailbox channel
C 3)	deassign the channel to the DES
C
C Calling sequence:
C
C	CALL DESDASSGN(STATUS)
C
C Output:
C
C	STATUS = VMS returned status
C
	SUBROUTINE DESDASSGN(STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESPARAMS.DEF'
	INCLUDE 'INCLIB:DESCOM.DEF'
C
	INTEGER*4   STATUS
C
	INTEGER*4   SYS$CANCEL,SYS$DASSGN
	EXTERNAL    SYS$CANCEL,SYS$DASSGN
C
C
C
C Cancel I/O on mailbox
C
	STATUS = SYS$CANCEL( %VAL(MBXCHAN) )
	IF(.NOT.STATUS)THEN
	  TYPE *,IAM(),'ERROR CANCELLING I/O TO MAILBOX'
	  CALL LIB$SIGNAL( %VAL(STATUS) )
	ENDIF
C
C Deassign the mailbox (will delete it)
C
	STATUS = SYS$DASSGN( %VAL(MBXCHAN) )
	IF(.NOT.STATUS)THEN
	  TYPE *,IAM(),'ERROR DEASSIGNING MAILBOX'
	  CALL LIB$SIGNAL( %VAL(STATUS) )
	ENDIF
C
C Deassign the DES
C
	STATUS = SYS$DASSGN( %VAL(DESCHAN) )
	IF(.NOT.STATUS)THEN
	  TYPE *,IAM(),'ERROR DEASSIGNING DES CHANNEL'
	  CALL LIB$SIGNAL( %VAL(STATUS) )
	ENDIF
C
	RETURN
	END
C
C
C *** DESINICOD
C
C
C This routine will start the DES microcode
C
	SUBROUTINE DESINICOD(STATUS)
	IMPLICIT NONE
C
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($IODEF)'
	INCLUDE '($STSDEF)'
C
	INCLUDE 'INCLIB:DESPARAMS.DEF'		!GENERAL PARAMETERS
C
	INTEGER*4   STATUS
C
C
	RECORD /IOSB/ QIOIOSB
C
C
	STATUS = SYS$QIOW(,			  !EFN
     *                    %VAL(DESCHAN),	  !CHAN
     *                    %VAL(IO$_STARTMPROC),	  !FUNC
     *		          QIOIOSB,		  !IOSB
     *		          ,			  !ASTADR
     *                    ,			  !ASTPRM
     *		          ,			  !P1
     *			  ,			  !P2
     *			  ,			  !P3
     *			  ,			  !P4
     *			  ,			  !P5
     *			   )			  !P6
C
	IF (STATUS) THEN
	  STATUS = QIOIOSB.STAT
	  IF(STATUS.EQ.2600)STATUS = 1
	ENDIF
C
	RETURN
	END
C
C
C *** DESMBXAST
C
C This routine is executed upon receipt of a mailbox message.
C It simply prints out the message and issues another QIO.
C
C
C
	SUBROUTINE DESMBXAST(PARAM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESPARAMS.DEF'
	INCLUDE 'INCLIB:DESCOM.DEF'
C
	INTEGER*4   PARAM
C
C
C Type out contents of mailbox
C
C **** NOTE **** DO NOT USE IAM HERE BECAUSE THIS IS AN AST AND COULD
C INTERRUPT ANOTHER (MAINLINE) CALL TO IAM ****
C
	TYPE *,'ENCPRO **** MAILBOX ****'
	TYPE *,'ENCPRO ',MBXINPUT
        IF ( .NOT. ASTSTOP ) THEN
C
C Now issue another read
C
          CALL DESMBXQIO
        ENDIF
C
	RETURN
	END
C
C
C *** DESMBXQIO
C
C This routine will perform a QIO to the mailbox
C
C
	SUBROUTINE DESMBXQIO
	IMPLICIT NONE
C
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($IODEF)'
C
	INCLUDE 'INCLIB:DESPARAMS.DEF'		!GENERAL PARAMETERS
C
	INTEGER*4   STATUS
C
	INTEGER*4   DESMBXAST
	EXTERNAL    DESMBXAST
C
C
C
	STATUS = SYS$QIO (,			  !EFN
     *                    %VAL(MBXCHAN),	  !CHAN
     *                    %VAL(IO$_READVBLK),	  !FUNC
     *		          MBXIOSB,		  !IOSB
     *		          DESMBXAST,		  !ASTADR
     *                    ,			  !ASTPRM
     *		          %REF(MBXINPUT),	  !P1
     *			  %VAL(32),		  !P2
     *			  ,			  !P3
     *			  ,			  !P4
     *			  ,			  !P5
     *			   )			  !P6
C
	IF(.NOT.STATUS)THEN
	  CALL LIB$SIGNAL( %VAL(STATUS) )
	ENDIF
	RETURN
	END
C
C
C *** DESSNDADR
C
C This routine will send the starting and ending addresses of control list
C and the data area to the driver.
C
C
	SUBROUTINE DESSNDADR(STATUS)
	IMPLICIT NONE
C
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($IODEF)'
	INCLUDE '($STSDEF)'
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESPARAMS.DEF'
	INCLUDE 'INCLIB:DESCOM.DEF'
C
	INTEGER*4   STATUS
C
	RECORD /IOSB/ QIOIOSB
C
C The following is the item list which must be passed to the driver
C
	INTEGER*4   ITEMCTRL(6)
	INTEGER*4   ITEMDATA(4)
C
C
C Initialize the control list description
C
	ITEMCTRL(01) = ISHFT(XX$K_CONTROL,16) + 24
	ITEMCTRL(02) = 16
	ITEMCTRL(03) = %LOC(CTRLBUF(1).CTRL_ENT(1))
	ITEMCTRL(04) = %LOC(CTRLBUF(CTRLCNT).CTRL_ENT(CTRLSIZ)) + 3 !(in bytes)
	ITEMCTRL(05) = CTRLSIZ*4
	ITEMCTRL(06) = CTRLMSEC			    ! MSEC TIMER
C
C Initialize the data list description
C
	ITEMDATA(01) = ISHFT(XX$K_DATAPATH,16) + 16
	ITEMDATA(02) = 8
	ITEMDATA(03) = %LOC( PRO(1,1) )
	ITEMDATA(04) = %LOC( PRO(PROLEN,NUMPRO) ) + 3		!(in bytes)
C
C I don't think we need a terminator because the descriptor says how long
C this is.
C
C Now send this info to the driver and lock the global pages down.
C
	STATUS = SYS$QIO (,				    !EFN !!NO WAIT
     *                    %VAL(DESCHAN),		    !CHAN
     *                    %VAL(IO$_SETMODE + IO$M_LOCKBUF), !FUNC
     *		          QIOIOSB,			    !IOSB
     *		          ,				    !ASTADR
     *                    ,				    !ASTPRM
     *		          %DESCR(ITEMCTRL),		    !P1
     *			  ,				    !P2
     *			  ,				    !P3
     *			  ,				    !P4
     *			  ,				    !P5
     *			   )				    !P6
C
	IF (.NOT.STATUS) THEN
	  TYPE *,IAM(),'ERROR WHILE SENDING CONTROL LIST ',STATUS
	  GOTO 9000
	ENDIF
C
	STATUS = SYS$QIO (,				    !EFN !!NO WAIT
     *                    %VAL(DESCHAN),		    !CHAN
     *                    %VAL(IO$_SETMODE + IO$M_LOCKBUF), !FUNC
     *		          QIOIOSB,			    !IOSB
     *		          ,				    !ASTADR
     *                    ,				    !ASTPRM
     *		          %DESCR(ITEMDATA),		    !P1
     *			  ,				    !P2
     *			  ,				    !P3
     *			  ,				    !P4
     *			  ,				    !P5
     *			   )				    !P6
C
	IF (.NOT.STATUS) THEN
	  TYPE *,IAM(),'ERROR WHILE SENDING DATA LIST ',STATUS
	  GOTO 9000
	ENDIF
C
9000	CONTINUE
	RETURN
	END
C
C
C *** DESBEGIN
C
C This routine will start transaction processing
C
C
	SUBROUTINE DESBEGIN(STATUS)
	IMPLICIT NONE
C
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($IODEF)'
	INCLUDE '($STSDEF)'
C
	INCLUDE 'INCLIB:DESPARAMS.DEF'		!GENERAL PARAMETERS
C
	INTEGER*4   STATUS
C
C
	RECORD /IOSB/ QIOIOSB
C
C
	STATUS = SYS$QIOW(,				    !EFN
     *                    %VAL(DESCHAN),		    !CHAN
     *                    %VAL(IO$_SETMODE + IO$M_STARTUP), !FUNC
     *		          QIOIOSB,			    !IOSB
     *		          ,				    !ASTADR
     *                    ,				    !ASTPRM
     *		          ,				    !P1
     *			  ,				    !P2
     *			  ,				    !P3
     *			  ,				    !P4
     *			  ,				    !P5
     *			   )				    !P6
C
	IF (STATUS) THEN
	  STATUS = QIOIOSB.STAT
	ENDIF
C
	RETURN
	END
C
C
C *** DESEND
C
C This routine will stop transaction processing
C
C
	SUBROUTINE DESEND(STATUS)
	IMPLICIT NONE
C
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($IODEF)'
	INCLUDE '($STSDEF)'
C
	INCLUDE 'INCLIB:DESPARAMS.DEF'		!GENERAL PARAMETERS
C
	INTEGER*4   STATUS
C
C
	RECORD /IOSB/ QIOIOSB
C
C
	STATUS = SYS$QIOW(,				     !EFN
     *                    %VAL(DESCHAN),		     !CHAN
     *                    %VAL(IO$_SETMODE + IO$M_SHUTDOWN), !FUNC
     *		          QIOIOSB,			     !IOSB
     *		          ,				     !ASTADR
     *                    ,				     !ASTPRM
     *		          ,				     !P1
     *			  ,				     !P2
     *			  ,				     !P3
     *			  ,				     !P4
     *			  ,				     !P5
     *			   )				     !P6
C
	IF (STATUS) THEN
	  STATUS = QIOIOSB.STAT
	ENDIF
C
	RETURN
	END
C
C
C *** PRINTCTRL
C
C This will print the contents of a control list entry
C
	SUBROUTINE PRINTCTRL(POINTER)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:DESCOM.DEF'
C
	INTEGER*4   POINTER
C
C
	TYPE *,IAM()
	TYPE *,IAM(),'GETPOINTER =',GETPOINTER
	TYPE *,IAM(),'PUTPOINTER =',PUTPOINTER
	TYPE *,IAM(),'err pointer=',POINTER
	TYPE *,IAM()
C
	TYPE 9001,IAM(),'FLAG  ',CTRLBUF(POINTER).CTRL_FLAG
	TYPE 9001,IAM(),'STAT  ',CTRLBUF(POINTER).CTRL_STAT
	TYPE 9001,IAM(),'FUNC  ',CTRLBUF(POINTER).CTRL_FUNC
	TYPE 9001,IAM(),'ENCDEC',CTRLBUF(POINTER).CTRL_ENCDEC
	TYPE 9001,IAM(),'MODE  ',CTRLBUF(POINTER).CTRL_MODE
	TYPE 9001,IAM(),'NUSD  ',CTRLBUF(POINTER).CTRL_NUSD
	TYPE 9001,IAM(),'SIZE  ',CTRLBUF(POINTER).CTRL_SIZE
	TYPE 9001,IAM(),'INOFF ',CTRLBUF(POINTER).CTRL_INOFF
	TYPE 9001,IAM(),'OUTOFF',CTRLBUF(POINTER).CTRL_OUTOFF
	TYPE 9001,IAM(),'KEY1  ',CTRLBUF(POINTER).CTRL_KEY1
	TYPE 9001,IAM(),'KEY2  ',CTRLBUF(POINTER).CTRL_KEY2
	TYPE 9001,IAM(),'USER1 ',CTRLBUF(POINTER).CTRL_USER1
	TYPE 9001,IAM(),'USER2 ',CTRLBUF(POINTER).CTRL_USER2
C
	RETURN
C
9001	FORMAT(X,A,'CTRL_',A,X,Z8)
	END
