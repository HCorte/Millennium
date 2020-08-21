C
C SUBROUTINE SGNON
C
C SGNON.FOR
C
C V04 09-JUL-1999 UXN Password expiration added.
C V03 19-MAY-1996 HXK Wojtek's security stuff added
C V02 25-MAY-1992 WLM LOGGED TO CONSOLE EVERY SIGNON AS SUPERUSER
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C
C      CALLING SEQUENCE:
C
C
C          INPUT  :  INDEX  -   function index
C        OUTPUT  :  SECLEV  -   group level of authority
C                    ID    -   user who signed on
C                    SIND  -   index for transaction type
C                   STATUS  -
C                               successful signon      =  0
C                               default to superuser   =  1
C
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
	 SUBROUTINE SGNON(INDEX,SECLEV,ID,SIND,STATUS)
	 IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	CHARACTER *4 EXIT
	CHARACTER*6 CHANGE
	CHARACTER*8 PASSENT,UNSPWORD
	INTEGER *4 SECLEV,STATUS,SLEVEL,NIBINX
	INTEGER *4 INDEX,USERST,ID,SIND, ST, NOCHECK0
	CHARACTER*20 PASPAS
	EQUIVALENCE(PASPAS,PASSENT,EXIT,CHANGE)
	INTEGER*4  DATES(3)
	INTEGER*2  CURR_DAT(LDATE_LEN), LAST_DAT(LDATE_LEN)
	COMMON /NOCHECK0/ NOCHECK0
	NOCHECK0=-1
	USERST=0
C
C If USER file not on pack default to SUPERUSER
C
 	 CALL OPENX(2,'GXTSK:USER.FIL',4,0,0,ST)
 	 CALL IOINIT(SONFDB,2,USESEC*256)
 	 IF (ST.NE.0) THEN
	    CALL CLRSCR(5)
	    SECLEV=SUPERUSE
	    STATUS=1
	    RETURN
	 ENDIF
C
C Ask for user ID
C
 3	 CONTINUE
	 USERST=0
	 CALL CLRSCR(5)
	 CALL INPNUM('User ID: ',ID,1,9999,ST)
	 IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
	 IF(ID.EQ.9999) THEN
	    CALL CLRSCR(5)
	    SECLEV=SUPERUSE
	    STATUS=1
	    RETURN
	 ENDIF	
C
C Read USER file for password and group level
C
	 CALL READW(SONFDB,ID,USEREC,ST)
	 IF (ST.NE.0.OR.USERID.EQ.0) USERST=-1
C
C Ask for password regardless of USER ID entered
C
	CALL PASSWORD(5,PASPAS)
	CALL CHKPAS(PASSENT,ST)
	IF(ST.NE.0) GOTO 3
C
C If USERST is -1 then user not found
C Print rejected signon record
C
	IF (USERST .LT. 0) THEN
	   SIND=1
	   GOTO 9998
	ENDIF
C
C Store user id in common
C
	USENBR=ID
C
C Check password expiration date.
C
	LAST_DAT(VMON)  = DATECHA(1)
	LAST_DAT(VDAY)  = DATECHA(2)
	LAST_DAT(VYEAR) = DATECHA(3)

	CALL LBDATE(LAST_DAT)

	CALL XDAT(DATES)
	
	CURR_DAT(VMON)  = DATES(2)
	CURR_DAT(VDAY)  = DATES(3)
	CURR_DAT(VYEAR) = DATES(1)
	
	CALL LBDATE(CURR_DAT)

	IF(EXPDAY.GT.0 .AND.
     *     LAST_DAT(VCDC)+EXPDAY .LE. CURR_DAT(VCDC)) THEN	
	   TYPE*,IAM(),'Your password has expired. Please change your password!'
	   CALL XWAIT(3,2,ST)
	   CHANGE = 'CHANGE'
	ENDIF	
C
C if CHANGE  sent to change password routine
C
	IF(CHANGE.EQ.'CHANGE') THEN
	  CALL CLRSCR(5)
	  CALL CHGPASS(ID,INDEX,SLEVEL,SIND)
	  SECLEV=SLEVEL
	  SIND=3
	  GOTO 9999
	ENDIF
C
C If user wishes to exit sent status
C
	IF(EXIT.EQ.'EXIT') THEN
	  CALL CLRSCR(5)
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C Test if password entered is correct
C
C
C Unscramble password
C
	CALL ENCPAS(ID,PASWORD1,UNSPWORD)
	IF(UNSPWORD.EQ.PASSENT) THEN
C
C Levels are stored in nibbles  extract and put in SECLEV
C
	   NIBINX=INDEX-1
C***	   CALL GETNIBLE(NIBINX,USERSECL,SECLEV)
	   CALL GETNIBLE(SECLEV,USERSECL,NIBINX+1)
	   SIND=2
	   GOTO 9999
	ELSE
	   SIND=1
	   GOTO 9998
	ENDIF
C
 9998	CONTINUE
	CALL PRINTSON(INDEX,SIND)
	GOTO 3
C
 9999	CONTINUE
	CALL PRINTSON(INDEX,SIND)
C
C** TEMPORARILY DISCONNECTED FROM DISPLAYING ON THE 
C** OPERATORS CONSOLE (FOR TESTING IN WEST GREENWICH)
C**
C**	IF(SECLEV.EQ.SUPERUSE) CALL CONLOG(FUNCNAME(INDEX))   ! *V02*
C
	RETURN
	END
