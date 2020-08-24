C
C READ_STNDEF.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]READ_STNDEF.FOV                              $
C  $Date::   17 Apr 1996 14:39:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C
C X2X Upgrade: 22-FEB-96 wsm Added AGTINF.DEF for Finland.
C
C V05 09-NOV-95 WJK USE SYSTEM FLAG TO DETERMINE STNDEF.FIL PATH
C V04 23-AUG-95 WJK MODIFY THE WAY THE STNDEF FILE NAME IS PASSED, USE
C		    OPENX NOT OPENW, SO IT WORKS ON THE ALPHA
C V03 09-SEP-94 SCD ADD PROCESSING FOR BROADCAST SERVER - Integrate UK
C		    changes into X2X Baseline
C V02 02-AUG-94 SCD ADD PROCESSING FOR DUMMMY STATIONS TO SUPPORT GVT
C		    INSTALLATION
C V01 01-05-93 XXX INITIAL RELEASE FOR IRELAND
C
C This program will read the Station definition file and check for
C any inconsistancies. It is called by X2BLDNET. Any # sign in col. 
C one (1) will cause that record to be skipped. A $ sign in col. 1
C will indicate that an x25 address is to be used. This applies only
C to X25 remote stations. If the station range is not present for the 
C X25 remote stations then any $ will be ignored. A ! sign in col 1 will
C indicate that LAPB link ports are present. These are used for GSAT
C stations. A "D" in the first column flags the station as a Dummy 
C station. A "B" in the first column flags the station as a Broadcast
C server.
C 
C 
C File format:
c
C #  STATION RANGE FILE
C #  FORMAT (2X,I4,2X,I5,2X,I5)  for station ranges.
C #  FORMAT (2X,I5,2X,16A1)      for remote x25 station addresses
C #  FORMAT (2X,I5)              for GSAT station lapb link ports
C #
C #       START   END
C #  SCL   STN    STN
C   0001  00000  00000            # X.21
C   0002  00000  00000            # X.25
C   0003  00001  00002            # ASYNC
C   0004  00000  00000            # ISOCH (DIGITAL)
C   0005  00000  00000            # USAT
C   0006  00000  00000            # X25PVC
C   0007  00003  00003            # GTECH DIAL
C   0008  00010  00100            # GTX DIAL
C   0009  00005  00005            # X.28 PAD
C
C   See X2BLDNET.FOR for more information. 
C
CC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
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
C
        SUBROUTINE READ_STNDEF
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:X2BLDNET.DEF'
C
        LOGICAL     CLASS_DEFINED/.FALSE./
        LOGICAL     REMOTE_EXISTS/.FALSE./
        LOGICAL     ADDR_EXISTS(X2X_STATIONS)/X2X_STATIONS*.FALSE./
C
        CHARACTER*1  CHR80(80)
        CHARACTER*4  CHR_CLASS
        CHARACTER*5  CHR_START
        CHARACTER*5  CHR_LAPB
        CHARACTER*5  CHR_END
        CHARACTER*5  CHR_STN
        CHARACTER*50 CHR_EQUIV
        CHARACTER*16 CHR_X25ADR
C
        INTEGER*4   I, J, K, M, ST
        INTEGER*4   REMSTN, LAPB_INDEX
        INTEGER*4   BROADSTN					      !V03
        INTEGER*4   START,END,XCLASS
        INTEGER*4   START_COL, END_COL
        INTEGER*4   DUMMY_LOCAL_STN_CLASS(X2XC_CLASSES)	!Local vector - only
					!used to determine multiple dummy
					!station records for a given class -
					!MAY NEED TO BE MOVED TO X2BLDNET.DEF
C
C
        EQUIVALENCE (CHR80(3),CHR_CLASS)
        EQUIVALENCE (CHR80(3),CHR_LAPB)
        EQUIVALENCE (CHR80(9),CHR_START)
        EQUIVALENCE (CHR80(16),CHR_END)
        EQUIVALENCE (CHR80(3),CHR_STN)
        EQUIVALENCE (CHR80(10),CHR_X25ADR)
        EQUIVALENCE (CHR80(23),CHR_EQUIV)
C
C       Clear variables  
C
        LAPB_INDEX = 0
        CALL FASTSET(0,STN_CLASS,X2XC_CLASSES)
        CALL FASTSET(0,START_STN,X2XC_CLASSES)
        CALL FASTSET(0,END_STN,X2XC_CLASSES)
        CALL FASTSET(0,EQUIV_CLASS,X2XC_PERM_CLASSES*MAX_EQUIV)
        CALL FASTSET(0,LAPB_PORTS,X2X_MAXPVC_LINES)
        CALL FASTSET(0,IS_BROADCAST_SERVER,X2X_STATIONS)	      !V03
C
C       Open the station definition file
C
        IF (PX2X_TASK) THEN                                           !V05
          CALL OPENX(2,'PX2XFILES:STNDEF.FIL',4,0,0,ST)		      !V04
	ELSE							      !V05
          CALL OPENX(2,'STNDEF.FIL',4,0,0,ST)			      !V05
	ENDIF							      !V05
        IF(ST.NE.0) THEN
          CALL OS32ER(5,'STNDEF.FIL','OPENW',ST,0)
          CALL GPAUSE
        ENDIF
C
C       Read the station ranges for each class of station
C
10      CONTINUE
        CHR80(1) = ' '
        READ(2,9001,END=50) CHR80
C
C       Comments start with #
C
        IF(CHR80(1).EQ.'#') THEN
          GOTO 10
C
C       Lapb port numbers start with exclamation (!)
C
        ELSEIF(CHR80(1).EQ.'!') THEN
          LAPB_INDEX = LAPB_INDEX + 1
          IF(LAPB_INDEX.GT.X2X_MAXPVC_LINES) THEN
             TYPE *,IAM(),'Too many GSAT ports defined'
             CALL GSTOP(GEXIT_FATAL)
          ENDIF
          LAPB_PORTS(LAPB_INDEX) = CTOI(CHR_LAPB,K)
          GOTO 10
C
C       X25 remote station addresses start with dollar sign ($)
C
        ELSEIF (CHR80(1).EQ.'$') THEN
          IF(STN_CLASS(CLASS_X25_REMOTE).NE.0) THEN
            REMOTE_EXISTS = .TRUE.
            REMSTN = CTOI(CHR_STN,K)
            IF(REMSTN.LT.START_STN(CLASS_X25_REMOTE).OR.
     *         REMSTN.GT.END_STN(CLASS_X25_REMOTE)) THEN
               TYPE *,IAM(),'Invalid X25 remote station number',REMSTN
               TYPE *,IAM(),'Not within range ',
     *                     START_STN(CLASS_X25_REMOTE),' - ',
     *                     END_STN(CLASS_X25_REMOTE)
               CLOSE(2)
               CALL GSTOP(GEXIT_FATAL)
            ENDIF   
            X2XADR(REMSTN) = CHR_X25ADR 
            ADDR_EXISTS(REMSTN) = .TRUE.
          ENDIF   
          GOTO 10
C
C	***** Start V03 changes ***
C
        ELSEIF (CHR80(1).EQ.'B' .OR. CHR80(1).EQ.'b') THEN
	  BROADSTN = CTOI(CHR_STN,K)
	  IF((BROADSTN.GT.0).AND.(BROADSTN.LE.X2X_STATIONS)) THEN
	    IS_BROADCAST_SERVER(BROADSTN) = .TRUE.
	  TYPE 9010,IAM(),'Station ',BROADSTN,' defined as Broadcast Station '
	  ELSE
            TYPE *,IAM(),' Broadcast station ',BROADSTN, ' is invalid'
            CLOSE(2)
            CALL GSTOP(GEXIT_FATAL)
	  ENDIF
          GOTO 10
C
C	***** End V03 changes ***
C
C	***** Start V02 changes ***

C       Dummy station addresses start with 'D' or 'd'
C
        ELSEIF (CHR80(1).EQ.'D' .OR. CHR80(1).EQ.'d') THEN
          XCLASS = CTOI(CHR_CLASS,K)
          START  = CTOI(CHR_START,K)
          END    = CTOI(CHR_END,K)
C
C         Determine if class is multiply defined

          IF(DUMMY_LOCAL_STN_CLASS(XCLASS).NE.0) THEN
            TYPE *,IAM(),' DUMMY Class ',XCLASS, ' is multiply defined'
            CLOSE(2)
            CALL GSTOP(GEXIT_FATAL)
          ELSE
            DUMMY_LOCAL_STN_CLASS(XCLASS) = XCLASS
          ENDIF
C

C         Are classes declared within the valid ranges.
C
          IF(XCLASS.LT.1 .OR. XCLASS.GT.X2XC_CLASSES) THEN
            TYPE *,IAM(),' Invalid DUMMY station class: ',XCLASS
            CLOSE(2)
            CALL GSTOP(GEXIT_FATAL)
C
C         Classes may be declared in file but have no stations
C         associated with them just ignore the record
C
          ELSEIF(START.LE.0 .OR. END.LE.0) THEN
	    TYPE *,IAM(),' Invalid DUMMY station range: ',START,' - ',END
            TYPE *,IAM(),' for station class: ',XCLASS,
     *                   ' Record ignored ' 
            GOTO 10
C
C         Station number exceeds maximum number configured
C
          ELSEIF(START.GT.X2X_STATIONS .OR. END.GT.X2X_STATIONS) THEN
	    TYPE *,IAM(),' DUMMY Station range assigned exceeds ',
     *		    X2X_STATIONS
            CALL GSTOP(GEXIT_FATAL)
            CLOSE(2)
C
C         Station ranges have been declared for a class
C
          ELSEIF(START.GT.0 .AND. END.LE.X2X_STATIONS .AND.
     *           END .GE. START) THEN
            X2XC_DUMMY_START_STN(XCLASS) = START
            X2XC_DUMMY_END_STN(XCLASS)   = END
	    X2XC_DUMMY_STN_COUNT(XCLASS) = X2XC_DUMMY_END_STN(XCLASS) -
     *					   X2XC_DUMMY_START_STN(XCLASS) + 1
C
C
C         unknown error .....
C
          ELSE
            TYPE *,IAM(),'Unknown error for DUMMY class: ',XCLASS
            TYPE *,IAM(),'Range ',X2XC_DUMMY_START_STN(XCLASS),':',
     *			  X2XC_DUMMY_END_STN(XCLASS)
            CLOSE(2)
            CALL GSTOP(GEXIT_FATAL)
          ENDIF

C	***** End V02 changes ***

C       Station ranges start with blanks
        ELSE
          XCLASS = CTOI(CHR_CLASS,K)
          START  = CTOI(CHR_START,K)
          END    = CTOI(CHR_END,K)
C
C         Determine if class is multiply defined
C
          IF(STN_CLASS(XCLASS).NE.0) THEN
            TYPE *,IAM(),' Class ',XCLASS, ' is multiply defined'
            CLOSE(2)
            CALL GSTOP(GEXIT_FATAL)
          ELSE
            STN_CLASS(XCLASS) = XCLASS
	    TYPE 9020,IAM(),'Station Class ',XCLASS,		!V03
     *	      ', Range: ',START,' - ',END			!V03
          ENDIF
C
C         Are classes declared within the valid ranges.
C
          IF(XCLASS.LT.1 .OR. XCLASS.GT.X2XC_CLASSES) THEN
            TYPE *,IAM(),' Invalid station class: ',XCLASS
            CLOSE(2)
            CALL GSTOP(GEXIT_FATAL)
C
C         Classes may be declared in file but have no stations
C         associated with them just ignore the record
C
          ELSEIF(START.LE.0 .OR. END.LE.0) THEN
            TYPE *,IAM(),' Invalid station range: ',START,' - ',END
            TYPE *,IAM(),' for station class: ',XCLASS,
     *                   ' Record ignored ' 
            GOTO 10
C
C         Station number exceeds maximum number configured
C
          ELSEIF(START.GT.X2X_STATIONS .OR. END.GT.X2X_STATIONS) THEN
            TYPE *,IAM(),' Station range assigned exceeds ',X2X_STATIONS
            CALL GSTOP(GEXIT_FATAL)
            CLOSE(2)
C
C         Station ranges have been declared for a class
C
          ELSEIF(START.GT.0 .AND. END.LE.X2X_STATIONS .AND.
     *           END .GE. START) THEN
            START_STN(XCLASS) = START
            END_STN(XCLASS)   = END
            CLASS_DEFINED     = .TRUE.
C
C           Get equivalence classes (CURRENTLY MAXIMUM OF 10)
C
            START_COL = 1
            END_COL   = 4
            DO 25 I = 1,MAX_EQUIV
               EQUIV_CLASS(XCLASS,I) = 
     *                CTOI(CHR_EQUIV(START_COL:END_COL),K)
C
C              Test that equivalence class is valid  
C
               IF((EQUIV_CLASS(XCLASS,I).GT.X2XC_PERM_CLASSES .AND.
     *             EQUIV_CLASS(XCLASS,I).LE.X2XC_CLASSES) .OR.
     *             EQUIV_CLASS(XCLASS,I).EQ.0) THEN
                  START_COL = START_COL + 5
                  END_COL   = END_COL   + 5
C..               TYPE *,'EQUIV_CLASS(',XCLASS,',',I,') = ',
C...     *                EQUIV_CLASS(XCLASS,I)
               ELSE
                  TYPE *,IAM(),'Equivalence class exceeds max. ',
     *               EQUIV_CLASS(XCLASS,I),' for class ',XCLASS
                  CLOSE(2)
                  CALL GSTOP(GEXIT_FATAL)
               ENDIF
25          CONTINUE
            GOTO 10
C
C         unknown error .....
C
          ELSE
            TYPE *,IAM(),'Unknown error for class: ',XCLASS
            TYPE *,IAM(),'Range ',START_STN(XCLASS),':',END_STN(XCLASS)
            CLOSE(2)
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
        ENDIF
50      CONTINUE
C
C       verify that stations ranges have been declared
C
        IF(.NOT. CLASS_DEFINED) THEN
          TYPE *,IAM(),' No stations have been defined for any class '
          TYPE *,IAM(),' Check the stations definition file please   '
          CLOSE(2)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C       Check for any overlapping stations ranges
C
        DO 65 I = 1,X2XC_PERM_CLASSES
           IF(START_STN(I).EQ.0 .OR. END_STN(I).EQ.0) GOTO 65 
           DO 60 J = I+1,X2XC_PERM_CLASSES
              IF(J.EQ.I) GOTO 60
              IF((START_STN(J).GE.START_STN(I) 
     *           .AND. END_STN(J) .LE.END_STN(I)) .OR.
     *            (START_STN(J).LE.START_STN(I) 
     *           .AND. END_STN(J) .GE.END_STN(I))) THEN
                TYPE *,IAM(),' Range for station class ',J,' invalid.'
                TYPE *,IAM(),' Already defined for class ',I
                CLOSE(2)
                CALL GSTOP(GEXIT_FATAL)
              ENDIF
60         CONTINUE
65      CONTINUE 
C
C	SCD - V03 - The following block of code defeats the purpose of using 
C	STNDEF.FIL to define the starting and ending range of station 
C	numbers for a class. It was used in the UK because the range of X.25
C	stations numbers was not contiguous.
C
C	Start of UK STNDEF.FIL range checking workaround - V03
C
C	allow for all station numbers if the range not defined 0,0
C
CV03	DO 66, I=1,X2XC_PERM_CLASSES
CV03            IF(STN_CLASS(I).EQ.0) GOTO 66
CV03	    IF (START_STN(I).EQ.0 .AND. END_STN(I).EQ.0) THEN
CV03		TYPE 9030,IAM(),'Station Class ',I,' Redefined range: ',
CV03     *		  1,' - ', X2X_STATIONS
CV03		START_STN(I)=1
CV03		END_STN(I)=X2X_STATIONS
CV03	    ENDIF
CV0366	CONTINUE
C
C	End of UK STNDEF.FIL range checking workaround - V03
C
C       Check for any duplicate equivalence classes
C
        DO 80 I = 1,X2XC_PERM_CLASSES
           IF(STN_CLASS(I).EQ.0) GOTO 80 
C
C          Check own equivalence class
C
           DO 68 K = 1,MAX_EQUIV
              IF(EQUIV_CLASS(I,K).EQ.0) GOTO 68
              DO 67 M = K+1,MAX_EQUIV
                 IF(EQUIV_CLASS(I,K).EQ.EQUIV_CLASS(I,M)) THEN
                   TYPE *,IAM(),'Duplicate equivalence class exists: '
     *                      ,EQUIV_CLASS(I,K),' within class ',I
                   CLOSE(2)
                   CALL GSTOP(GEXIT_FATAL)
                 ENDIF
67            CONTINUE
68         CONTINUE
C
C          Check other equivalence class
C
           DO 75 J = I+1,X2XC_PERM_CLASSES
              IF(STN_CLASS(J).EQ.0) GOTO 75 
              DO 70 K = 1,MAX_EQUIV
                 IF(EQUIV_CLASS(I,K).EQ.0) GOTO 70
                 DO 69 M = 1,MAX_EQUIV
                    IF(EQUIV_CLASS(I,K).EQ.EQUIV_CLASS(J,M)) THEN
                      TYPE *,IAM()
     *                      ,'Duplicate equivalence classes exist:'
     *                      ,EQUIV_CLASS(J,M),' in class ',J
                      CLOSE(2)
                      CALL GSTOP(GEXIT_FATAL)
                    ENDIF
69               CONTINUE
70             CONTINUE
75         CONTINUE
80      CONTINUE
C
C       Check that x25 addresses exist for each remote station in 
C       defined range.
C
        IF(REMOTE_EXISTS) THEN
          TYPE *,IAM(),'Testing for X25 remote addresses'
          DO 200 I = START_STN(CLASS_X25_REMOTE),
     *               END_STN(CLASS_X25_REMOTE)
             IF(.NOT.ADDR_EXISTS(I)) THEN
               TYPE *,IAM(),'X25 address not defined for station ',I
               CLOSE(2)
               CALL GSTOP(GEXIT_FATAL)
             ENDIF
200       CONTINUE
        ENDIF
        CLOSE(2)
C
C       Format statements
C
9001    FORMAT(80A1)
9010    FORMAT(X,A,A,I5,A)
9020    FORMAT(X,A,A,I3,A,I5,A,I5)
9030    FORMAT(X,A,A,I3,A,I5,A,I5)
        RETURN
        END
