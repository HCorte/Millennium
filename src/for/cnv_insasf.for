C $Log:   GXAFXT:[GOLS]CNV_INSASF.FOV  
C  
C     Rev 1.0   17 Apr 1996 12:41:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   16 Nov 1993 23:19:36   HXK
C  Initial revision.
C  
C
C Program to Modify Instant sales & validations fields
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM CNV_INSASF
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C	INCLUDE 'INCLIB:AGTCOM.DEF'

        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
C	INCLUDE 'INCLIB:RECSCF.DEF'
C
C
	INTEGER*4	TER		!Loop Variable
	INTEGER*4	STER		!Starting Terminal Number.
	INTEGER*4	ETER		!Ending Terminal Number.
	INTEGER*4	ST		!Subroutine Return Status.
	INTEGER*4	FDB(7)		!File Descriptor block for ASF.
	INTEGER*4	ANS		!Answer Flag.


	INTEGER*4   K,INST_COUNT,INST_SALES,OFF_VALID,
     *              CARTEL_CODE


C BEGIN CODE -------------------------------------------

	TYPE*,IAM(),'<<<<< CNV_INSASF V01  >>>>>'
	TYPE*,IAM()


	WRITE(5,2001) (SFNAMES(K,ASF),K=1,5)
 2001   FORMAT(1X,' Open ***',5A4,'***')


C OPEN THE AGENT SALES FILE
C -------------------------
	CALL OPENW(ASF,SFNAMES(1,ASF),4,0,0,ST)
	CALL IOINIT(FDB,ASF,ASFSEC*256)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),1,ST,0)




C GET PROGRAM OPTIONS
C
10	CONTINUE
	TYPE*,IAM(),'This Program will change the Instant sales & validations for a range'
        TYPE*,IAM(),'of terminals, in the ASF'
	TYPE*,IAM()
	TYPE*,IAM()
	TYPE*,IAM(),'Enter Range of Terminal #''s to affect with change'
	TYPE*,IAM(),'or ''E'' to Exit '
	TYPE*,IAM()


	CALL INPNUM('Enter Starting Terminal #: ',STER,1,NUMAGT,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
	CALL INPNUM('Enter Ending Terminal #  : ',ETER,STER,NUMAGT,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)



C DISPLAY PICKED VALUES
C
	TYPE*,IAM()
	TYPE*,IAM(),'Values Entered for Terminal ',STER,' to ',ETER
	CALL WIMG(5,'Is this correct Y/N ')
	CALL YESNO(ANS)
	IF(ANS.NE.1) GOTO 10
	


C LOOP FOR SELECTED RANGE OF TERMINALS
C ------------------------------------

	DO TER = STER,ETER

	   CALL READW(FDB,TER,ASFREC,ST)
	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,TER)


	   DO K=1,ANUMDAY  ! for all days
C	      Swap instant sales 1 and 3, for both count and amount
C	      -----------------------------------------------------
	      INST_COUNT = ASFMIS(1,1,K)
	      ASFMIS(1,1,K) = ASFMIS(3,1,K)
	      ASFMIS(3,1,K) = INST_COUNT

	      INST_SALES = ASFMIS(1,2,K)
	      ASFMIS(1,2,K) = ASFMIS(3,2,K)
	      ASFMIS(3,2,K) = INST_SALES


C	      Copy instant sales 4,5,6 --> validation 3,2,1, for both count and amount
C             -----------------------------------------------------------------
	      OFF_VALID  = PRM_NUMINS    !offset for instant validations fields

	      ASFMIS(OFF_VALID+3,1,K) = ASFMIS(4,1,K)
	      ASFMIS(OFF_VALID+3,2,K) = ASFMIS(4,2,K)

              ASFMIS(4,1,K)           = 0
              ASFMIS(4,2,K)           = 0


	      ASFMIS(OFF_VALID+2,1,K) = ASFMIS(5,1,K)
	      ASFMIS(OFF_VALID+2,2,K) = ASFMIS(5,2,K)

              ASFMIS(5,1,K)           = 0
              ASFMIS(5,2,K)           = 0


	      ASFMIS(OFF_VALID+1,1,K) = ASFMIS(6,1,K)
	      ASFMIS(OFF_VALID+1,2,K) = ASFMIS(6,2,K)

              ASFMIS(6,1,K)           = 0
              ASFMIS(6,2,K)           = 0

	   ENDDO



C	   Updated Cartel code
C	   -------------------
	   CARTEL_CODE=0
	   CALL ASCBIN (ASFINF,SCHAN,LCHAN,CARTEL_CODE,ST)
	   IF (ST.NE.0) THEN
	      TYPE*,' CARTEL CODE CONVERSION ERROR FOR TERM :',TER
	      CALL GPAUSE

	   ELSE
C	      TYPE*,' CARTEL CODE :',CARTEL_CODE
C	      WRITE (5,2002) (ASFBYT(K),K=SCHAN,ECHAN)
C	      CARTEL_CODE=77
C	      CALL BINASC(ASFINF,SCHAN,LCHAN,CARTEL_CODE)
C	      WRITE (5,2002) (ASFBYT(K),K=SCHAN,ECHAN)

	      IF (CARTEL_CODE.EQ.51)  THEN
		 TYPE*,' '
		 TYPE*,' TERMINAL, CARTEL_CODE ;',TER,CARTEL_CODE
	         CARTEL_CODE=1
	         CALL BINASC(ASFINF,SCHAN,LCHAN,CARTEL_CODE)
	         WRITE (5,2002) (ASFBYT(K),K=SCHAN,ECHAN)
 2002	         FORMAT(1X,' CARTEL CODE :***',4A,'***')
	      ENDIF

	      IF (CARTEL_CODE.EQ.500) THEN
		 TYPE*,' '
		 TYPE*,' TERMINAL, CARTEL_CODE ;',TER,CARTEL_CODE
                 CARTEL_CODE=5
	         CALL BINASC(ASFINF,SCHAN,LCHAN,CARTEL_CODE)
	         WRITE (5,2002) (ASFBYT(K),K=SCHAN,ECHAN)
	      ENDIF

	   ENDIF	      



C          WRITE RECORD BACK
C          -----------------
	   CALL WRITEW(FDB,TER,ASFREC,ST)
	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),3,ST,TER)

	ENDDO




	CALL USRCLOS1 (ASF)
	TYPE*,IAM(),'Update Completed for Terminals ',STER,' to ',ETER


	END
