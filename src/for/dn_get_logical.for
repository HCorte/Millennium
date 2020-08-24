C
C SUBROUTINE DN_GET_LOGICAL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_GET_LOGICAL.FOV                           $
C  $Date::   17 Apr 1996 12:58:04                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dn_init.for ***
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	THIS ROUTINE WILL INITIALIZE ALL THE DECNET HEADERS AND QUEUES.
C	IT WILL OPEN THE NETWORK MAILBOX AND DECLARE OUR NETWORK NAME
C	SO WE CAN ACCEPT CONNECTIONS FROM OTHER SYSTEMS.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_GET_LOGICAL(NAME,		! LOGICAL NAME TO TRANSLATE.
     *                            RESULT,	! WHAT WE GOT.
     *                            MAX_LEN,	! MAX SIZE OF RESULT.
     *                            ACT_LEN)	! ACTUAL SIZE OF RETURNED,
C						! STRING - 0 = NO TRANSLATION.
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'		! DECNET STRUCTURES.
	INCLUDE 'INCLIB:DN_BLOCK.DEF'		! DECNET DATA BLOCKS.
C
        INCLUDE '($LNMDEF)'
        INCLUDE '($SYSSRVNAM)'
C
C STRUCTURE THAT DEFINES AN ENTRY OF A $TRNLNM SYSTEM SERVICE ITEM LIST
C
	STRUCTURE	/DN_LNITEMS_STRUCT/
	  INTEGER*2	LENGTH			! LENGTH OF BUFFER.
	  INTEGER*2	ITEMCODE		! CODE FOR DESIRED INFORMATION.
	  INTEGER*4	BUFFER			! POINTER TO BUFFER FOR INFO.
	  INTEGER*4	ACTUAL			! POINTER TO ACTUAL LENGTH.
	END STRUCTURE
C
C LOCAL DECLARATIONS
C
	INTEGER*4	ACT_LEN,		! ACTUAL SIZE OF RESULT.
     *			MAX_LEN,		! MAX SIZE OF RESULT.
     *			STATUS			! RETURN CODE OF CALL.
C
	INTEGER*2	A_LEN			! ACTUAL SIZE OF RESULT.
C
	CHARACTER*(*)	NAME,			! LOGICAL NAME TO TRANSLATE.
     *			RESULT			! WHAT WE GOT.
C
	RECORD /DN_LNITEMS_STRUCT/ ITEM(2)	! ITEM LIST FOR $TRNLNM.
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GET OUR NETWORK NAME FROM THE LOGICAL DN_NETNAME.
C
	ITEM(1).ITEMCODE = LNM$_STRING		! CODE TO GET ACTUAL STRING.
	ITEM(1).LENGTH   = MAX_LEN		! LENGTH OF BUFFER.
	ITEM(1).BUFFER   = %LOC(RESULT)		! POINTER TO BUFFER.
	ITEM(1).ACTUAL   = %LOC(A_LEN)		! TBD - PTR TO ACTUAL LEN WORD.
C
	A_LEN            = 0			! INITIALIZE TO ZERO.
	ITEM(2).ITEMCODE = 0			! END OF LIST.
	ITEM(2).LENGTH   = 0			! LENGTH OF BUFFER.
C
	STATUS = SYS$TRNLNM(LNM$M_CASE_BLIND,
     *                      'LNM$FILE_DEV',	! SEARCH PROCESS, JOB, GROUP,
C						! AND SYSTEM TABLES IN ORDER.
     *                      NAME,		! LOGICAL WE ARE LOOKING FOR.
     *                      ,			! DEFAULT ACCESS MODE.
     *                      ITEM)		! PARAMETER LIST.
C
	ACT_LEN = A_LEN				! MOVE TO LONGWORD.
C
	IF (ACT_LEN .GT. 0)
     *    RESULT = RESULT(:ACT_LEN)		! CHOP OFF JUNK.
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
	RETURN
	END
