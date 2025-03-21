C
C SUBROUTINE GOLAN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]GOLAN.FOV                                    $
C  $Date::   17 Apr 1996 13:27:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - golan.for;1 **
C
C GOLAN.FOR
C
C V05 17-JUN-2000 UXN CDEC$ OPTIONS /WARNING=NOALIGNMENT added for P2_STRUCT
C V04 01-SEP-1995 DAS IMPROVED ERROR handling for Px2x      
C V03 05-DEC-1994 GPR Integrate UK changes into X2X Baseline
C V02 16-SEP-1990 MRM RELEASED FOR VAX
C
C 1. OPEN DEVICES
C 2. SET UP ADAPTER
C 3. START OUTSTANDING READ
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
	SUBROUTINE GOLAN(LAN,SAP,REPLY)
	IMPLICIT NONE
C
	INCLUDE '($SSDEF)'
	INCLUDE '($IODEF)'
	INCLUDE '($SYSSRVNAM)'
	INCLUDE 'INCLIB:NMADEF.DEF'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
C DECLARE STRUCTURE OF P2 FOR ASSIGN OF DEVICE.
C
CDEC$ OPTIONS /WARNING=NOALIGNMENT
	STRUCTURE /P2_STRUCT/
	  INTEGER*2 PARAM_BFN			!NUMBER OF ETHERNET BUFFERS
	  INTEGER*4 VALUE_BFN
	  INTEGER*2 PARAM_BUS			!LARGEST BUS TRANSFER SIZE
	  INTEGER*4 VALUE_BUS
	  INTEGER*2 PARAM_FMT			!ETHERNET FORMAT (802.2)
	  INTEGER*4 VALUE_FMT
	  INTEGER*2 PARAM_SAP			!SERVICE ACCESS POINT
	  INTEGER*4 VALUE_SAP
	  INTEGER*2 PARAM_SRV			!ETHERNET SERVICE
	  INTEGER*4 VALUE_SRV
	  INTEGER*2 PARAM_MCA			!MULTICASTING (REQUIRED FOR
	  INTEGER*2 VALUE_LEN		        !BROADCASTING)
	  INTEGER*2 VALUE_MOD
	  BYTE      VALUE_ADD(6)
	  INTEGER*2 PARAM_PHA                   !ETHERNET PHYSICAL ADDRESS
	  INTEGER*2 VALUE_PAL
	  INTEGER*2 VALUE_DEF
	  BYTE      VALUE_EAD(6)
	END STRUCTURE
CDEC$ END OPTIONS 
C
	RECORD /LN_IOSSTRUCT/    LOCAL_IOSB	!IO STATUS BLOCK
	RECORD /P2_STRUCT/     PARAM_P2		!P2 FOR DEVICE SETUP
C
	INTEGER*4       DESCRIP_P2(2)		!DESCRIPTOR OF P2 PARAMETERS
	CHARACTER*5	ETHDEV		        !ETHERNET DEVICE NAME
	INTEGER*4	SAP,LAN
	INTEGER*4	REPLY,I
	INTEGER*4	IOSTAT, IFUNC
C
C INITIALIZE P2 VALUES.
C
	PARAM_P2.PARAM_BFN = NMA$C_PCLI_BFN	!NUMBER OF ETHERNET BUFFERS
	PARAM_P2.VALUE_BFN = 96			!Increase the buffering on the
						!Ethernet card !V02
	PARAM_P2.PARAM_BUS = NMA$C_PCLI_BUS	!LARGEST BUS XFER SIZE
	PARAM_P2.VALUE_BUS = ETHLENDT
	PARAM_P2.PARAM_FMT = NMA$C_PCLI_FMT	!ETHERNET FORMAT (802.2)
	PARAM_P2.VALUE_FMT = NMA$C_LINFM_802
	PARAM_P2.PARAM_SAP = NMA$C_PCLI_SAP	!SSAP
	PARAM_P2.VALUE_SAP = SAP 		!MUST BE EVEN NUMBERS
	PARAM_P2.PARAM_SRV = NMA$C_PCLI_SRV	!SERVICE (USER SUPPLIED)
	PARAM_P2.VALUE_SRV = NMA$C_LINSR_USR
	PARAM_P2.PARAM_MCA = NMA$C_PCLI_MCA     !ENABLE MULTICASTING
	PARAM_P2.VALUE_LEN = 8			
	PARAM_P2.VALUE_MOD = NMA$C_LINMC_SET	!SET MULTICAST ADDRESS
	PARAM_P2.VALUE_ADD(1) = 'FF'X
	PARAM_P2.VALUE_ADD(2) = 'FF'X
	PARAM_P2.VALUE_ADD(3) = 'FF'X
	PARAM_P2.VALUE_ADD(4) = 'FF'X
	PARAM_P2.VALUE_ADD(5) = 'FF'X
	PARAM_P2.VALUE_ADD(6) = 'FF'X
	PARAM_P2.PARAM_PHA = NMA$C_PCLI_PHA     !PHYSICAL PORT ADDRESS
	PARAM_P2.VALUE_PAL = 8
	PARAM_P2.VALUE_DEF = NMA$C_LINMC_SDF    !SET DEFAULT ADDRESS
	PARAM_P2.VALUE_EAD(1) = '00'X           !CAN BE USED IF OTHER
	PARAM_P2.VALUE_EAD(2) = '00'X            !DEFAULT DESIRED
	PARAM_P2.VALUE_EAD(3) = '00'X
	PARAM_P2.VALUE_EAD(4) = '00'X
	PARAM_P2.VALUE_EAD(5) = '00'X
	PARAM_P2.VALUE_EAD(6) = '00'X
C
	DESCRIP_P2(1)=54			!LENGTH OF PARAMETERS
	DESCRIP_P2(2)=%LOC(PARAM_P2)		!POINTER TO PARAMETERS
C
C CHECK LAN NUMBER.
C
	REPLY=-1
	IF(LAN.GE.1.AND.LAN.LE.MAXLAN) THEN
C
	   IF(LOCLAN(SAP,LAN).EQ.LANUP) THEN
C
C IF OVERRIDE ETHERNET ADDRESS SPECIFIED FROM CONFIGURATION FILE,
C MODIFY DEFAULT.
C
              IF(LANLADDR(LAN).EQ.LADDRYES) THEN
        	 PARAM_P2.VALUE_DEF = NMA$C_LINMC_SET
		 DO 100 I=1,6
                   PARAM_P2.VALUE_EAD(I) = LANHOME(I,LAN)
100		 CONTINUE
	      ENDIF
C
C COPY THE DEVICE NAME FROM COMMON.
C
	      CALL OPS('ATTEMPTING CONNECTION',LAN,SAP)
	      WRITE(ETHDEV,9000)(LANDEV(I,1,LAN),I=1,5)
9000	      FORMAT(5A)
D	      WRITE(5,9001) (LANDEV(I,1,LAN),I=1,5)
9001	      FORMAT(1X,'DEVICE NAME:  ',5A)
C
C GET A CHANNEL NUMBER ASSIGNED TO THE DEVICE.
C
	      IOSTAT=SYS$ASSIGN(ETHDEV,%REF(LUNLAN(SAP,LAN)),,)
	      IF(.NOT.IOSTAT)THEN
                CALL  LIB$SIGNAL(%VAL(IOSTAT))
                CALL VMS_ERROR_OPS(IOSTAT)
              ENDIF
D	      TYPE *,'CHANNEL ASSIGNED .....',LUNLAN(SAP,LAN),LAN,SAP
C
C STARTUP THE ETHERNET DEVICE.
C
	      IFUNC=IO$_SETMODE .OR. IO$M_CTRL .OR. IO$M_STARTUP
	      IOSTAT=SYS$QIOW(,%VAL(LUNLAN(SAP,LAN)),
     *                         %VAL(IFUNC),
     *                         %REF(LOCAL_IOSB),,,,
     *                         %REF(DESCRIP_P2),,,,)
	      IF(LOCAL_IOSB.STAT.EQ.SS$_BADPARAM)
     *          CALL VMS_ERROR_OPS(LOCAL_IOSB.STAT)
C....*          TYPE *,'BAD PARAM:  ',LOCAL_IOSB.PARM
	      IF(.NOT.IOSTAT) THEN
	        CALL LIB$SIGNAL(%VAL(IOSTAT))
                CALL VMS_ERROR_OPS(IOSTAT)
		GOTO 1000
	      ELSE IF(LOCAL_IOSB.STAT .NE. SS$_NORMAL) THEN
	        CALL LIB$SIGNAL(%VAL(LOCAL_IOSB.STAT))
                CALL VMS_ERROR_OPS(LOCAL_IOSB.STAT)
	        GOTO 1000
	      ENDIF
D	      TYPE *,'** ETHERNET STARTED /CHANNEL **',LUNLAN(SAP,LAN)
C
C READ THE ETHERNET ADDRESS FROM THE DEVICE.
C
	      CALL OPS('READING ETHERNET ADDRESS',LAN,SAP)
 	      CALL LGETPHA(LAN,SAP,REPLY)
C
C START AN OUTSTANDING READ ON OPENED CHANNEL NUMBER.
C
	      CALL GOREAD(LAN,SAP)
C
D	      TYPE*,'**** STATION LAN ****[',LAN,SAP,']'
	      REPLY=0
	   ELSE
D	      TYPE *,'**** LAN NOT UP ****[',LAN,']'
	   ENDIF
	ELSE
	   CALL OPS('**** INVALID LAN ****',LAN,0)
	ENDIF
1000	CONTINUE
	IF(REPLY.EQ.0) CALL OPS('**** LAN STARTED ****',LAN,SAP)
	RETURN
	END
