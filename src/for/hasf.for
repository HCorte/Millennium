C
C PROGRAM HASF
C
C V07 04-OCT-2000 UXN USE_LOOKUP added.
C V06 19-MAY-1996 HXK Wojtek's security stuff added
C V05 05-NOV-1993 JWE Remove password prompt
C V04 10-JUN-1993 HXN Move HASF.DEF, which contains AGTINF.DEF, 
C                     before RECAGT.DEF.
C V03 01-MAR-1993 EBD DAS update 3/1/93
C                     Changing format of ASF file
C V02 26-APR-1992 WLM APPLIED NO-ACCESS CHECK
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
C=======OPTIONS /CHECK/EXT
	PROGRAM HASFF
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:HASF.DEF'    !THIS ONE INCLUDED AGTINF.DEF
	INCLUDE 'INCLIB:PRMHSF.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'

	INTEGER*4 FDB(7)
C
        INTEGER*4  SUPLOC
        PARAMETER (SUPLOC=1)   !Location of superuser in LEVELS file
C
        INTEGER*4 K, J, FLAG, I, ST, NOCHECK0
	INTEGER*4 STATUS, LEV, SECLEV, UID, SIND, EXIT
	CHARACTER*4 PROMPT
	CHARACTER*2 OPT(16)
	INTEGER*4 MIND
	CHARACTER*6 PAYPAS, PASS, PASSENT, DEFTPASS
	CHARACTER*20 PASSWRD, PASPAS
        EQUIVALENCE (PASPAS,PASSENT,EXIT)
	CHARACTER*2 FUN
	CHARACTER CZERO
	LOGICAL*4 USE_LOOKUP
	COMMON /HASF/ LOOKUP(NUMAGT),ASFREC,SCFREC,USE_LOOKUP
	INTEGER*4 LOOKUP
	COMMON /NOCHECK0/ NOCHECK0
	DATA PROMPT/'ASF '/
	DATA PAYPAS/'PERKIN'/
        DATA DEFTPASS/'DONUTS'/
	DATA CZERO/Z0/
	DATA OPT/'ID','BN','MK','HO','GT','CO','DA','LI','LB','LM','LH','LG',
     *           'LC','LD','PA','EX'/
C
        CALL COPYRITX(6)
C
C Get security levels 
C
        CALL BUILDLEV(HINDEX,STATUS)
        IF (STATUS.EQ.1) THEN
          GOTO 5
        ENDIF
C
C Prompt user to sign on with user ID and password
C
        CALL CLRSCR(6)
        CALL SGNON(HINDEX,SECLEV,UID,SIND,STATUS)
        CALL USRCLOS1(     2)
C
C If status equals to 1 then USER.FIL or LEVEL.FIL  not on pack
C Ask for default password
C
 5      CONTINUE
C
        IF (STATUS.EQ.1) THEN
15        CONTINUE
          CALL BUILDALL(HINDEX,STATUS)
          LEV=SUPLOC
          CALL CLRSCR(6)
          CALL PASSWORD(5,PASPAS)
          IF(EXIT.EQ.'EXIT') THEN
            CALL CLRSCR(6)
            CALL GSTOP(GEXIT_SUCCESS)
          ENDIF
          IF(PASSENT.EQ.DEFTPASS.AND.PASSENT.NE.'        ') GOTO 35
          GOTO 15
        ENDIF
C
C Store user id number in common
C
C**      USENBR=UID
C
C Call logging routine to write signon transaction to TMF
C
C**      CALL LOGSON(HINDEX,UID,SIND)
C
C Get proper group level
C
C
        DO 25 I=1,16
          IF (SECLEV.EQ.LEVELS(I)) THEN
            LEV=I
            GOTO 35
          ENDIF
25      CONTINUE
C
C Security level not declared in levels file
C
        CALL CLRSCR(6)
        WRITE(6,907)
        CALL XWAIT(2,2,ST)
        CALL GSTOP(GEXIT_SUCCESS)
C
35      CONTINUE
C
        NOCHECK0=-1
C
C READ SCF RECORD
C
	CALL GETSCONF(SCFREC,ST)
C
C OPEN AGENTS SALES FILE
C
	CALL OPENW(1,SCFSFN(1,ASF),4,0,0,ST)
	CALL IOINIT(FDB,1,ASFSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFSFN(1,ASF),1,ST,0)
C
C BUILD AGENT NUMBER LOOKUP TABLE
C
	CALL CLRSCR(6)
	DO I=1,NUMAGT           ! INITIALIZE FOR TERMINAL LOOKUP
	   LOOKUP(I) = I
	ENDDO
C
	TYPE *
	TYPE *,'<<<<< HASF Agent Sales File Update Utility V01 >>>>>'
	TYPE *
	USE_LOOKUP = .FALSE.
	CALL INPYESNO('Do you want to use Agent number lookup table? ',FLAG)
	IF(FLAG.NE.1) GOTO 100
C
C
	TYPE*,'Building agent number lookup table '
	USE_LOOKUP = .TRUE.
	DO 50 I=1,NUMAGT
  	   LOOKUP(I)=0
	   CALL READW(FDB,I,ASFREC,ST)
	   IF(ST.NE.0) CALL FILERR(SCFSFN(1,ASF),2,ST,I)

	   DO J=IDBEG(1),IDEND(1)
	      IF(ASFBYT(J).EQ.' '.OR.ASFBYT(J).EQ.CZERO) GOTO 50
	   END DO

	   CALL ASCBIN(ASFINF,IDBEG(1),LAGNO,LOOKUP(I),ST)
	   IF(ST.NE.0) THEN
	      WRITE(6,905) I,(ASFBYT(K),K=IDBEG(1),IDEND(1))
	      CALL BELLS(3)
	   ENDIF
50	CONTINUE
C
C PRINT MENU AND PROMPT FOR INPUT
C
100	CONTINUE
	CALL CLRSCR(6)
	WRITE(6,901)
	CALL WIMG(6,PROMPT)
	READ(5,902) FUN
	CALL STR$UPCASE(FUN,FUN)
C
C Get menu number for security level
C
         MIND = 0
         DO 4 I=1,15
            IF(FUN.EQ.OPT(I)) MIND=I
 4       CONTINUE
C
C Determine if the choice is allowed at this level
C
         CALL CHKHMENU(MIND,LEV,ST)
         IF (ST.NE.0) THEN
             CALL CLRSCR(6)
             WRITE(6,906)
             CALL XWAIT(2,2,ST)
             GOTO 100
         ENDIF
C
         CALL CLRSCR(6)
C
C PROCESS REQUEST
C
	IF(FUN.EQ.'ID') THEN
	  CALL AIDENT(FDB,0)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'BN') THEN
	  CALL AIDENT(FDB,1)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'MK') THEN
	  CALL AIDENT(FDB,2)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'HO') THEN
	  CALL AIDENT(FDB,3)
	  GOTO 100
	ENDIF
C
C
        IF(FUN.EQ.'GT') THEN
          CALL AIDENT(FDB,4)
          GOTO 100
        ENDIF
C
C
        IF(FUN.EQ.'CO') THEN
          CALL AIDENT(FDB,5)
          GOTO 100
        ENDIF

        IF(FUN.EQ.'DA') THEN
          CALL AIDENT(FDB,6)
          GOTO 100
        ENDIF
C
C
	IF(FUN.EQ.'LI') THEN
	  CALL ALIST(FDB,0)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'LB') THEN
	  CALL ALIST(FDB,1)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'LM') THEN
	  CALL ALIST(FDB,2)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'LH') THEN
	  CALL ALIST(FDB,3)
	  GOTO 100
	ENDIF
C
C      
	IF(FUN.EQ.'LG') THEN
          CALL ALIST(FDB,4)
          GOTO 100
        ENDIF
C
	IF(FUN.EQ.'LC') THEN
          CALL ALIST(FDB,5)
          GOTO 100
        ENDIF
C
	IF(FUN.EQ.'LD') THEN
          CALL ALIST(FDB,6)
          GOTO 100
        ENDIF
C
	IF(FUN.EQ.'PA') THEN
	  CALL CLRSCR(6)
	  CALL PASSWORD(6,PASSWRD)
	  PASS=PASSWRD(1:6)
	  IF(PASS.NE.PAYPAS) THEN
	    TYPE*,'Invalid password'
	    CALL BELLS(3)
	    CALL XWAIT(1,2,ST)
	    GOTO 100
	  ENDIF
	  CALL PAYAGT(FDB)
	  GOTO 100
	ENDIF
C
C
	IF(FUN.EQ.'EX') THEN
	  CALL CLOSEFIL(FDB)
	  CALL CLRSCR(6)
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C INVALID ENTRY
C
	CALL CLRSCR(6)
	WRITE(6,903)
	CALL BELLS(3)
	CALL XWAIT(1,2,ST)
	GOTO 100
C
C
901	FORMAT(/,' ASF update functions:',/,
     *	       /,T5,'IDE',5X,'- Enter or modify agent idenity data',
     *	       /,T5,'BNK',5X,'- Enter or modify agent banking data',
     *	       /,T5,'MKT',5X,'- Enter or modify agent marketing data',
     *	       /,T5,'HOT',5X,'- Enter or modify agent hotline data',
     *         /,T5,'GTK',5X,'- Enter or modify agent gtrack data',
     *         /,T5,'COM',5X,'- Enter or modify agent comm. data',
     *         /,T5,'DAT',5X,'- Enter or modify agent susp. dates',
     *	       /,T5,'LID',5X,'- List agent id and payment data',
     *	       /,T5,'LBK',5X,'- List agent banking and payment data',
     *	       /,T5,'LMK',5X,'- List agent marketing and payment data',
     *         /,T5,'LHT',5X,'- List agent hotline and payment data',
     *	       /,T5,'LGT',5X,'- List agent gtrack and payment data',
     *	       /,T5,'LCO',5X,'- List agent communications data',
     *	       /,T5,'LDA',5X,'- List agent susp. dates',
     *	       /,T5,'PAY',5X,'- Enter agent monetary transactions',
     *	       /,T5,'EXT',5X,'- Program exit',//)
902	FORMAT(A2)
903	FORMAT('  *** Entry error *** ')
905	FORMAT(' *** Terminal ',I4,' invalid agent num      ',7A1)
906     FORMAT(' This request cannot be granted on this security level')
907     FORMAT(' Security level not declared in LEVELS file')
	END
