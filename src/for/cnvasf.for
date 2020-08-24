C  
C
C V04 23-MAR-2011 RXK Added DIR command before and after conversion
C V03 16-MAR-2011 FRP EuroMillions: recalculate record according to MAXGAM=50.
C V02 11-MAY-1999 UXN Improved user interface. Files with proper size
C                     are created automatically.
C V01 XX-XXX-XXXX XXX INITIAL RELEASE.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
	PROGRAM CNVASF
	IMPLICIT NONE
C
        INCLUDE '(LIB$ROUTINES)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   OLD_MAXGAM, OLD_NUMAGT
	PARAMETER (OLD_MAXGAM=10)
        PARAMETER (OLD_NUMAGT=6144)
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:RECAGT_OLD.DEF'
C
C
	INTEGER*4   TER, LEN, I4LEN
	INTEGER*4   ST, I, J, K, OLD_FDB(7), FDB(7)
        INTEGER*4   YESNO

        INTEGER*4   FILE_SIZE           ! NEW FILE SIZE
        CHARACTER*20 NEW_FILE,OLD_FILE
        INTEGER*4    I4NEW_FILE(5),I4OLD_FILE(5)
        EQUIVALENCE (NEW_FILE,I4NEW_FILE)
        EQUIVALENCE (OLD_FILE,I4OLD_FILE)
        INTEGER*4   SIZE,MAXREC_CNV
        INTEGER*4   NOFTLSIG
        EXTERNAL    NOFTLSIG
C
	CALL COPYRITE
	CALL LIB$ESTABLISH(NOFTLSIG)
C
C
        TYPE*,IAM()
        TYPE*,IAM(),'CURRENT FILE:'
        ST = LIB$SPAWN('$ DIR ASF.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))

        NEW_FILE = 'FILE:ASF.NEW'
        OLD_FILE = 'FILE:ASF.FIL'
C
        CALL OPENX(1,OLD_FILE,4,0,0,ST)
        CALL VAXGETFSIZ(1,SIZE)
        CLOSE(1)
C
        MAXREC_CNV = OLD_NUMAGT
        FILE_SIZE  = NUMAGT*ASFSEC/2
        TYPE*,IAM(),'Old file size = ', MAXREC_CNV*OLD_ASFSEC/2
        TYPE*,IAM(),'New file size = ', FILE_SIZE
        TYPE*,IAM()
        TYPE*,IAM(),'Number of agents to convert >',MAXREC_CNV
        TYPE*,IAM()
        CALL PRMYESNO('Are you sure you want to convert ASF file [Y/N]',YESNO)
        IF(YESNO.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
        CALL CREATE_FILE(I4NEW_FILE,FILE_SIZE,ST)
        IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
        CALL OPENW(3,I4NEW_FILE,0,0,0,ST)
        CALL IOINIT(FDB,3,ASFSEC*256)
        IF(ST.NE.0) CALL FILERR(I4NEW_FILE,1,ST,0)
C
        CALL OPENW(4,I4OLD_FILE,0,0,0,ST)
        CALL IOINIT(OLD_FDB,4,OLD_ASFSEC*256)
        IF(ST.NE.0) CALL FILERR(I4OLD_FILE,1,ST,0)
C
        TYPE*,IAM(),'STARTING...'
C
C	Read record from file
C
	DO TER=1,OLD_NUMAGT
           CALL READW(OLD_FDB,TER,OLD_ASFREC,ST)
           IF(ST.NE.0) CALL FILERR(I4OLD_FILE,2,ST,TER)
	   IF(MOD(TER,1000).EQ.0) TYPE*,IAM(),TER,' records converted...'
	   CALL FASTSET(0, ASFREC, ASFLEN)
	   CALL FASTMOV(OLD_ASFREC,ASFREC,OLD_ASFDAY_OFF-1)
	   DO I=1,AGAMLEN
	     DO J=1,OLD_MAXGAM
	       DO K=1,ANUMDAY
		    ASFDAY(I,J,K)=OLD_ASFDAY(I,J,K)
	       END DO
	     END DO
	   END DO
	   DO I=1,AGAMLEN+MAXMLTD_SEL
	     DO J=1,OLD_MAXGAM
	       DO K=1,2
		    ASFBIL(I,J,K)=OLD_ASFBIL(I,J,K)
	       END DO
	     END DO
	   END DO
C
	   LEN=OLD_ASFGFL_OFF-OLD_ASFINV_OFF
	   I4LEN=LEN*4
C***	   CALL MOVBYT(OLD_ASFREC,OLD_ASFINV_OFF,ASFREC,ASFINV_OFF,I4LEN)
C**********
	   DO I=1,AINVLEN
	     DO J=1,2
		ASFINV(I,J)=OLD_ASFINV(I,J)
		ASFYTDINV(I,J)=OLD_ASFYTDINV(I,J)
	     END DO
	   END DO
	   DO I=1,6
	     DO J=1,15
		ASFLGR(I,J)=OLD_ASFLGR(I,J)
	     END DO
	   END DO
C**********           
           DO I=1,OLD_MAXGAM
             ASFGFL(I)=OLD_ASFGFL(I)
           END DO
C
	   LEN=OLD_ASFYTD_OFF-OLD_ASFWCT_OFF
	   I4LEN=LEN*4
C**	   CALL MOVBYT(OLD_ASFREC,OLD_ASFWCT_OFF,ASFREC,ASFWCT_OFF,I4LEN)
C*************
           ASFWCT=OLD_ASFWCT
           ASFSC1=OLD_ASFSC1
           ASFSC2=OLD_ASFSC2
C*************
	   DO I=1,AGAMLEN
	     DO J=1,OLD_MAXGAM
	       DO K=1,2
		    ASFYTD(I,J,K)=OLD_ASFYTD(I,J,K)
	       END DO
	     END DO
	   END DO
C
	   LEN=OLD_ASFSPE_OFF-OLD_ASFYTDINV_OFF
	   I4LEN=LEN*4
C***	   CALL MOVBYT(OLD_ASFREC,OLD_ASFYTDINV_OFF,ASFREC,ASFYTDINV_OFF,I4LEN)
C*************
           DO I=1,GUTLEN
             ASFGUT(I)=OLD_ASFGUT(I)
           END DO
           ASFHWN(1)=OLD_ASFHWN(1)
           ASFHWN(2)=OLD_ASFHWN(2)
C**************
	   DO I=1,ASPELEN
	     DO J=1,OLD_MAXGAM
	       DO K=1,ANUMDAY
		    ASFSPE(I,J,K)=OLD_ASFSPE(I,J,K)
	       END DO
	     END DO
	   END DO
C
	   LEN=OLD_ASFLEN-OLD_ASFMIS_OFF
	   I4LEN=LEN*4
C***	   CALL MOVBYT(OLD_ASFREC,OLD_ASFMIS_OFF,ASFREC,ASFMIS_OFF,I4LEN)
C***************
	   DO I=1,AMISLEN
	     DO J=1,2
	       DO K=1,ANUMDAY
		    ASFMIS(I,J,K)=OLD_ASFMIS(I,J,K)
	       END DO
	     END DO
	   END DO
           ASFDNM=OLD_ASFDNM
C***************
           CALL FASTMOV(OLD_ASFREC(OLD_ASFITINV_OFF),ASFREC(ASFITINV_OFF), 
     *                  AITINVLEN + 2*AITGAM)
           ASFGVT = OLD_ASFGVT 
           ASFGVTIM = OLD_ASFGVTIM
           ASFNCDC = OLD_ASFNCDC
           ASFLCDC = OLD_ASFLCDC
C 
C          Write record to file
C
	   CALL WRITEW(FDB,TER,ASFREC,ST)
           IF(ST.NE.0) CALL FILERR(I4NEW_FILE,3,ST,TER)
	ENDDO
100	CONTINUE
        TYPE*,IAM(),TER-1,' records converted in total.'
C
	CALL CLOSEFIL(FDB)
        CALL CLOSEFIL(OLD_FDB)
C
        TYPE*,IAM(),'Renaming FILE:ASF.FIL to FILE:ASF.OLD'
        CALL LIB$RENAME_FILE('FILE:ASF.FIL','FILE:ASF.OLD')

        TYPE*,IAM(),'Renaming FILE:ASF.NEW to FILE:ASF.FIL'
        CALL LIB$RENAME_FILE('FILE:ASF.NEW','FILE:ASF.FIL')
C
        TYPE*,IAM()
        TYPE*,IAM(),'Files after conversion:'
        ST = LIB$SPAWN('$ DIR ASF.%%%.* /DATE/SIZE=ALL')
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
        TYPE*,IAM(),'ASF file converted successfully!'
        CALL GSTOP(GEXIT_SUCCESS)
C	
	END

