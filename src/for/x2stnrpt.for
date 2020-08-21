C
C $Log:   GXAFXT:[GOLS]X2STNRPT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:36:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   02 Sep 1994 18:36:28   HXK
C  Initial revision.
C  
C     Rev 1.2   26 Jun 1994 14:50:40   HXK
C  NEW X2X STATION REPORT 
C  
C     Rev 1.0   19 Oct 1993 16:32:08   LMK
C  Initial revision.
C
C X2STNRPT.FTN 
C 
C V05 25-MAY-94 SCD  Rewrite program based upon requirements in Finland's
C		     SITE RFSS # 154.  This program no longer reads the
C                    station file; instead, it extracts the data to be
C                    sorted and displayed from the installed COMMONs.
C                    Agent information is still read from the ASF.  Logic
C                    is similar to that in X2STNPRT.FOR.
C V04 21-DEC-92 JWE  Sort XSTNCHR should be the same length as station          
C        # in ASF.          
C V03 21-DEC-92 JWE  Modify for the "new X2X"         
C V02 23-OCT-91 HJK  ADDED NUM_OF_TERMS OPTION        
C V01 01-SEP-91 HUGH INITIAL RELEASE FOR FINLAND      
C 
C X2STN ADDRESS AND GROUP LISTING REPORT 
C 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
C Island, and contains confidential and trade secret information. It            
C may not be transferred from the custody or control of GTECH except            
C as authorized in writing by an officer of GTECH. Neither this item            
C nor the information it contains may be used, transferred,        
C reproduced, published, or disclosed, in whole or in part, and    
C directly or indirectly, except as expressly authorized by an     
C officer of GTECH, pursuant to written agreement.    
C 
C Copyright 1991, 1992, 1993, 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C  
      OPTIONS /CHECK=NOOVERFLOW/EXT
C 
      PROGRAM X2STNRPT      
      IMPLICIT NONE 
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF' 
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:AGTINF.DEF'
      INCLUDE 'INCLIB:PRMAGT.DEF'
      INCLUDE 'INCLIB:RECAGT.DEF'   
      INCLUDE 'INCLIB:X2XCOM.DEF'   
      INCLUDE 'INCLIB:X2XSTN.DEF'   
C 
      INTEGER*4  OPTION,OPTION2,OPTION3	!V05
      INTEGER*4  REPLU /7/
C 
      INTEGER*4  STN    !Rec #/station count         
      INTEGER*4  I,ST,K,ERR,J        !Work variables 
      INTEGER*4  NUMTER    
      INTEGER*4  PRT,EXT,LINCNT
      INTEGER*4  ACTAGT,ACTTER,PAGE,TEMP
      INTEGER*4  REC,M,Q,COPY
      INTEGER*4  FDB(7),
     *           SCFNAM(5)  /'GXTS','K:SC','F.FI','L   ','    '/
C
      INTEGER*4  STRLEN
      PARAMETER  (STRLEN=20)
      INTEGER*4  ZIPLEN

C     Indices into DISTBL and SORTDATA
      INTEGER*4  STNIDX 	!V05
      INTEGER*4  TERIDX 	!V05
      INTEGER*4  ADRIDX 	!V05
      INTEGER*4  AD2IDX 	!V05
      INTEGER*4  DRPIDX 	!V05
      INTEGER*4  ZIPIDX 	!V05
      INTEGER*4  CLSIDX 	!V05
      INTEGER*4  NUMIDX 	!V05
      PARAMETER  (STNIDX  = 1)	!V05
      PARAMETER  (TERIDX  = 2)	!V05
      PARAMETER  (ADRIDX  = 3)	!V05
      PARAMETER  (AD2IDX  = 4)	!V05
      PARAMETER  (DRPIDX  = 5)	!V05
      PARAMETER  (ZIPIDX  = 6)	!V05
      PARAMETER  (CLSIDX  = 7)	!V05
      PARAMETER  (NUMIDX  = 8)	!V05

C     Sort key values for SORTDATA
      INTEGER*4  STNSRT 	!V05
      INTEGER*4  TERSRT 	!V05
      INTEGER*4  ADRSRT 	!V05
      INTEGER*4  AD2SRT 	!V05
      INTEGER*4  DRPSRT 	!V05
      INTEGER*4  ZIPSRT 	!V05
      INTEGER*4  CLSSRT 	!V05
      INTEGER*4  NUMSRT	        !V05

      PARAMETER  (STNSRT  = 1)	!V05
      PARAMETER  (TERSRT  = 2)	!V05
      PARAMETER  (ADRSRT  = 3)	!V05
      PARAMETER  (AD2SRT  = 4)	!V05
      PARAMETER  (DRPSRT  = 5)	!V05
      PARAMETER  (ZIPSRT  = 6)	!V05
      PARAMETER  (CLSSRT  = 7)	!V05
      PARAMETER  (NUMSRT  = 8)	!V05

      INTEGER*4  NUM_SORT_IDX	!V05-Total # of sort keys
      PARAMETER  (NUM_SORT_IDX = 8)	!V05

      INTEGER*4  LEVEL1_SORT_VALUES	        !V05
      PARAMETER  (LEVEL1_SORT_VALUES = 8)	!V05-#of possible level 1 sort
						!options
      INTEGER*4  KEY1,KEY2,KEY3 !Sort keys - V05
      
      INTEGER*4  SORTDATA(NUM_SORT_IDX,NUMAGT) !V05 - array to be sorted and 
                                               !written to report
      INTEGER*4  DISCNT                        !V05 - total # of raw data
                                               !records
      INTEGER*4  SORTCNT                       !V05 - # of data records we
                                               !have to sort based upon the
                                               !station class (Level 1 
                                               !option); SORTCNT <= DISCNT
      INTEGER*4  TERM, PORT, TER

      INTEGER*4  DISDIM                        !V05 - 1st dimension of raw 
      PARAMETER  (DISDIM  = NUM_SORT_IDX)      !data table
      INTEGER*4  DISSIZ                        !V05 - Max # of raw data records
      PARAMETER  (DISSIZ  =                    !2nd dimension of raw data table
     *		  X2X_MAXTERMS*X2X_STATIONS*X2X_MAXPORT)
      INTEGER*4  DISTBL(DISDIM,DISSIZ)        !V05 - raw data table

      INTEGER*4  IDBEG(6), IDEND(6), IDLEN(6)       
      DATA IDBEG  /SAGNO,SNAME,SCITY,SZIPC,STELE,     
     *             SDROP/   
C 
      DATA IDEND  /EAGNO,ENAME,ECITY,EZIPC,ETELE,     
     *             EDROP/   
C 
      DATA IDLEN  /LAGNO,LNAME,LCITY,LZIPC,LTELE,     
     *             LDROP/   
C 

C     THE STATION CLASS VALUES IN STN_CLASS_TYPES1 AND STN_CLASS_TYPES2
C     MUST MATCH THE STATION CLASS VALUE DEFINITIONS IN THE STATION CLASS 
C     FILE.  IT IS ASSUMED THAT THE STATION CLASS FILE DEFINES THESE AS 
C	STN CLASS VALUE		STATION CLASS
C   	     	 1   		X.21 Single
C       	 2   		X.25 CLASS
C       	 3   		ASYNC CLASS
C       	 4   		ISOCH CLASS
C	      	 9   		X28GVT
C       	10   		X.21 Multi
C       	11   		Dial-up async
C               14              X.21 Timed
C     WE USE 2 STATION CLASS VECTORS (STN_CLASS_TYPES1 AND 
C     STN_CLASS_TYPES2)  SO WE CAN GROUP CLASSES TOGETHER FOR SORTING.  FOR
C     EXAMPLE, SELECTING OPTION 1=X.21 STATIONS SELECTS BOTH THE X.21 MULTI
C     AND THE X.21 SINGLE STATIONS FOR THE REPORT.  LEASED LINES ARE 
C     ASSUMED TO BE EITHER ASYNCH OR ISOCH.

      INTEGER*4 STN_CLASS_TYPES1 (LEVEL1_SORT_VALUES) /1,10,1,3,11,2,9,14/ !V05
      INTEGER*4 STN_CLASS_TYPES2 (LEVEL1_SORT_VALUES) /10,0,0,4,0,0,0,0/  !V05
      INTEGER*4 STN_CLASS_TYPES3 (LEVEL1_SORT_VALUES) /14,0,0,0,0,0,0,0/

      INTEGER*2   ADR_LEN             !V05 - # of BCD digits in stn 
                                      !address
C
      CHARACTER   REPNAM*11
      CHARACTER   REPNM2*11
C 
      INTEGER*4   DROP
      BYTE	  BDROP(4)
      CHARACTER   C2DROP*2	      !Drop address
      EQUIVALENCE (DROP,BDROP,C2DROP)

      CHARACTER    CZERO*1  
      DATA CZERO/Z0/        

      CHARACTER   ADDRCHAR*(STRLEN)
      CHARACTER   CHRSTR*20           !ASCII for BCD address       
      EQUIVALENCE (ADDRCHAR,CHRSTR)          

      CHARACTER   AGNOCHR(7)
      CHARACTER   AGNOSTR*7 
      EQUIVALENCE (AGNOCHR,AGNOSTR)      

      CHARACTER   XSTNSTR*(LXSTN)       
      CHARACTER   XSTNCHR(LXSTN)        
      EQUIVALENCE (XSTNSTR,XSTNCHR)      

      CHARACTER   XZIPSTR*(LZIPC)       
      CHARACTER   XZIPCHR(LZIPC)
      EQUIVALENCE (XZIPSTR,XZIPCHR)
C 
C OPEN CONSOLE, AGENT SALES FILE AND REPORT FILE      
C
      CALL COPYRITE 
C 
      TYPE *   
      TYPE *,'<<<<< X2STNRPT Station Address Report    V02 >>>>>'  
      TYPE *   
      TYPE *,'Getting X.21 data ... '    
C 
C
C READ SCF RECORD
C
	CALL OPENW(1,SCFNAM,4,0,0,ST)
	CALL IOINIT(FDB,1,SCFSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
	CALL READW(FDB,1,SCFREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
	CALL CLOSEFIL(FDB)
	DO 10 I=1,MAXFIL
	IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
10	CONTINUE
C
C OPEN AGENTS SALES FILE
C
	CALL OPENW(1,SCFSFN(1,ASF),4,0,0,ST)
	CALL IOINIT(FDB,1,ASFSEC*256)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,ASF),1,ST,0)
	ENDIF
C 
C 
C 
500   CONTINUE 
      TYPE *   
      TYPE *,'Level 1 Sort Options: '            
      TYPE *   
      TYPE *,'      1 - X.21 Stations        2 - X.21 Multi      '  
      TYPE *,'      3 - X.21 Single          4 - Leased line     ' 
      TYPE *,'      5 - Mobiles              6 - X.25 Stations   ' 
      TYPE *,'      7 - X.28 Stations        8 - X.21 Timed      '
      TYPE *,'      E - Exit                                     '
      TYPE *   
      CALL INPNUM('Enter desired sort option     ',OPTION,1,8,EXT) 
      IF(EXT.LT.0) STOP     
      CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)  
      IF(EXT.LT.0) STOP     
      KEY1 = 0	    !V05 - ALWAYS SORT BY CLASS FIRST IN CASE WE HAVE
			!MULTIPLE CLASSES, E.G. OPTIONS 1 AND 4
      KEY2 = 0		!KEYS 2 AND 3 MAY BE ALTERED BELOW
      KEY3 = 0

      IF (OPTION.EQ.1 .OR. OPTION.EQ.2 .OR. OPTION.EQ.3 .OR. OPTION.EQ.8) THEN
          TYPE *   
          TYPE *,'Level 2 Sort Options: '            
          TYPE *   
          TYPE *,
     *	'      1 - X.21 Address         2 - Station Number' 
          TYPE *,
     *	'      3 - Term number          E - Exit          '
          TYPE *   
          CALL INPNUM('Enter desired sort option     ',OPTION2,1,3,EXT) 
          IF(EXT.LT.0) STOP     
	  IF (OPTION2 .EQ. 1 ) THEN
              KEY1 = AD2SRT     
	  ELSEIF (OPTION2 .EQ. 2 ) THEN
              KEY1 = STNSRT     
	  ELSEIF (OPTION2 .EQ. 3 ) THEN
              KEY1 = TERSRT     
          ENDIF
      ELSEIF (OPTION.EQ.4 .OR. OPTION.EQ.5) THEN
          TYPE *   
          TYPE *,'Level 2 Sort Options: '            
          TYPE *,
     *	'      1 - Station Number       2 - Term number        ' 
          TYPE*,
     *  '      E - Exit                                        '
          TYPE *   
          CALL INPNUM('Enter desired sort option     ',OPTION2,1,2,EXT) 
          IF(EXT.LT.0) STOP     
	  IF (OPTION2 .EQ. 1 ) THEN
              KEY1 = STNSRT     
          ELSEIF (OPTION2 .EQ. 2 ) THEN
              KEY1 = TERSRT
          ENDIF
      ELSE		!OPTIONS 6 AND 7 REQUIRE NO FURTHER SORTING OPTIONS
	  OPTION2 = 0
      ENDIF

      IF (OPTION.EQ.1.OR.OPTION.EQ.2.OR.OPTION.EQ.4.OR.OPTION.EQ.5.OR. 
     *        OPTION.EQ.8) THEN
          TYPE *   
          TYPE *,'Level 3 Sort Options: '            
          TYPE *   
          TYPE *,'      1 - Drop Address         2 - Postal Code     ' 
          TYPE *,'      E - Exit                                     ' 
          TYPE *   
          CALL INPNUM('Enter desired sort option     ',OPTION3,1,2,EXT) 
          IF(EXT.LT.0) STOP     
	  IF (OPTION3 .EQ. 1 ) THEN
              KEY2 = DRPSRT     
	  ELSEIF (OPTION3 .EQ. 2 ) THEN
              KEY2 = ZIPSRT
          ENDIF
      ELSE		!OPTIONS 3,6 AND 7 REQUIRE NO FURTHER SORTING 
			!OPTIONS
	  OPTION3 = 0
      ENDIF

C
C LOAD THE DISPLAY TABLE.
C
	DISCNT=0
	DO 20 STN = 1,X2X_STATIONS
	  DO 120 PORT=1,X2X_MAXPORT
	    DO 200 TER=1,X2X_MAXTERMS
	      TERM=X2XS_TERMS(TER,PORT,STN)
	      IF(TERM.NE.0) THEN
	        DISCNT=DISCNT+1
	        DISTBL(STNIDX,DISCNT)=STN
	        DISTBL(TERIDX,DISCNT)=TERM
                NUMTER=0 
                DO 1010 PRT=1,X2X_MAXPORT          
                   NUMTER=NUMTER+X2XS_NUM_TERMS(PRT,STN)         
 1010           CONTINUE 
	        DISTBL(NUMIDX,DISCNT)=NUMTER

C
C V05 - COMPENSATE FOR ASCBCD'S INCORRECT CONVERSIONS BY CONVERTING THE 
C BCD ADDRESS STORED IN ARRAY X2XS_ADRESS TO ASCII FIRST, THEN CORRECTLY 
C CONVERTING THE RESULTING ASCII STRING TO A HEX VALUE FOR SORTING.
C
C
C CONVERT BCD ADDRESS TO ASCII.
C
	        ADR_LEN=X2XS_ADRESS_LEN(STN)
	        CALL BCDASC(CHRSTR,1,ADR_LEN,X2XS_ADRESS(1,STN),ERR)

C CONVERT ASCII TO HEX - NOTE THAT THIS WILL MODIFY DISTBL(ADRIDX,DISCNT)
C	                 AND DISTBL(AD2IDX,DISCNT)
C
	        CALL X2ALATOH(CHRSTR,1,ADR_LEN,DISTBL(ADRIDX,DISCNT),ERR)
C
C END OF V05 CHANGE BLOCK
CV05	        DISTBL(ADRIDX,DISCNT)=X2XS_ADRESS(1,STN)
	        C2DROP=X2XT_DROP_AD(TERM)
	        DISTBL(DRPIDX,DISCNT)=DROP
C
                IF(TERM.NE.0 .AND. KEY2.EQ.ZIPSRT) THEN 
                   CALL READW(FDB, TERM, ASFREC, ST)
                   DO 650 J=SZIPC,EZIPC
                      IF(ASFBYT(J).EQ.CZERO) ASFBYT(J)=' '
650                CONTINUE
	        ENDIF
C
                DO 220 M=SZIPC,EZIPC
                   XZIPCHR(M-(SZIPC-1))=ASFBYT(M)            
220             CONTINUE
                ZIPLEN = LZIPC
                DISTBL(ZIPIDX,DISCNT)=CTOI(XZIPSTR,ZIPLEN)
                DISTBL(CLSIDX,DISCNT)= X2XS_STNCLS(STN)
	      ENDIF
200	    CONTINUE
120	  CONTINUE
 20     CONTINUE
C 
C INITIALIZE FOR EACH REPORT
C 
      LINCNT = 70           
      ACTAGT = 0            
      ACTTER = 0            
      PAGE = 0 
      TEMP = 0 
      CALL FASTSET(0,SORTDATA,NUM_SORT_IDX*NUMAGT)      
C 
C OPEN CORRESPONDING REPORT OPTION FILE  
C 
      ENCODE(11,8000,REPNAM) OPTION         
      ENCODE(11,8001,REPNM2) OPTION         
      CALL ROPEN('X2STN.REP',REPLU,ST)   
      IF(ST.NE.0)THEN       
         TYPE *,' Error opening  ',REPNAM,' > ',ST    
         PAUSE 
      ENDIF    
C 
C SET UP SORT BASED ON OPTION            
C 
      TYPE*,'Matching X.21 data with Agents data ... '
C 
C 
C EXTRACT AND SORT DATA    
C 
      TYPE*,'Sorting ... '  
      SORTCNT=0			!COUNTER FOR # OF RECORDS WE WILL ACTUALLY
				!NEED TO SORT
      DO 2000 REC = 1,DISCNT	!CHECK ALL POSSIBLE RECORDS
C     NEW CONDITIONS FOR V05.  ONLY PROCESS THIS STN AND EXTRACT ITS DATA 
C     TO BE SORTED UPON IF THE STATION CLASS IS CORRECT FOR THE SPECIFIED
C     OPTION.
         IF (
     *      ( (OPTION.EQ.1) .AND.		!MATCHES EITHER
     *	      (DISTBL(CLSIDX,REC) .EQ. STN_CLASS_TYPES1 (OPTION) .OR. !CLASS
     *         DISTBL(CLSIDX,REC) .EQ. STN_CLASS_TYPES2 (OPTION) .OR. !CLASS
     *	       DISTBL(CLSIDX,REC) .EQ. STN_CLASS_TYPES3 (OPTION) ) )
     *     .OR*
     *      ( (OPTION.EQ.4) .AND.               !MATCHES EITHER
     *        (DISTBL(CLSIDX,REC) .EQ. STN_CLASS_TYPES1 (OPTION) .OR. !CLASS
     *         DISTBL(CLSIDX,REC) .EQ. STN_CLASS_TYPES2 (OPTION) ) )
     * 	   .OR.						!MATCHES 1ST CLASS
     *      ( (OPTION.EQ.2 .OR. OPTION.EQ.3 .OR. OPTION.EQ.5 .OR. OPTION.EQ.6 
     *         .OR. OPTION.EQ.7 .OR. OPTION.EQ.8) .AND.
     *	      (DISTBL(CLSIDX,REC) .EQ. STN_CLASS_TYPES1 (OPTION) ) )
     *      ) THEN					!KEEP THIS STN 
							!RECORD AND ALL ITS
	         SORTCNT=SORTCNT+1			!DATA TO BE SORTED
		 SORTDATA(STNIDX,SORTCNT) = DISTBL(STNIDX,REC)
		 SORTDATA(TERIDX,SORTCNT) = DISTBL(TERIDX,REC)
		 SORTDATA(ADRIDX,SORTCNT) = DISTBL(ADRIDX,REC)
		 SORTDATA(AD2IDX,SORTCNT) = DISTBL(AD2IDX,REC)
		 SORTDATA(DRPIDX,SORTCNT) = DISTBL(DRPIDX,REC)
		 SORTDATA(ZIPIDX,SORTCNT) = DISTBL(ZIPIDX,REC)
		 SORTDATA(CLSIDX,SORTCNT) = DISTBL(CLSIDX,REC)
		 SORTDATA(NUMIDX,SORTCNT) = DISTBL(NUMIDX,REC)
         ENDIF
 2000 CONTINUE

      ACTAGT = SORTCNT			!# OF ACTUAL AGENTS = # OF RECORDS
					!WE'RE ACTUALLY GOING TO SORT

      IF (ACTAGT .EQ. 0) THEN		!NO RECORDS MATCH CRITERIA..TRY 
					!AGAIN
          TYPE *, 'NO records found matching your sort options'
          GOTO 400			!CLOSE OUTPUT FILE BEFORE LOOPING
      ELSEIF (ACTAGT .EQ. 1) THEN	!DON'T BOTHER TO SORT...IF WE TRY
	  CONTINUE			!TO SORT I4XSORT GENERATES ERRORS
      ELSE				!WE MUST HAVE AT LEASE 2 RECORDS TO
          CALL I4XSORT(SORTDATA,NUM_SORT_IDX,ACTAGT,KEY1,KEY2,KEY3)  !SORT
      ENDIF
      TYPE*,'Sorting completed.'         
      TYPE*    
      TYPE*,'Generating report ... '     
C 
C PRINT REPORT 
C 
      DO 300 I=1,ACTAGT
	 REC = SORTDATA(TERIDX,I)

C 
         CALL READW(FDB, REC, ASFREC, ST)
C 
C CHECK FOR VALID AGENT NUMBER IF SORT BY TERMINAL OPTION          
C 
            DO 310 K=SAGNO,EAGNO         
               IF(ASFBYT(K).NE.' '.AND.ASFBYT(K).NE.CZERO)THEN     
                  GOTO 312  
               ENDIF        
310         CONTINUE        
            GOTO 300        
312         CONTINUE        
            DO 311 K=SSCLS,ESCLS         
               IF(ASFBYT(K).NE.' '.AND.ASFBYT(K).NE.CZERO)THEN     
                  ACTTER = ACTTER + 1    
                  GOTO 320  
               ENDIF        
311         CONTINUE        
            GOTO 300        
C 
C SUBSTITUTE BLANKS FOR NULLS            
C 
320      CONTINUE           
         DO 150 J=SZIPC,EZIPC   
            IF(ASFBYT(J).EQ.CZERO) ASFBYT(J)=' '      
150      CONTINUE           
C 
C GET AGNUMBER 
C 
         DO 157 Q=SAGNO,EAGNO            
            AGNOCHR(Q)=ASFBYT(Q)         
157      CONTINUE           
C 
C CHECK IF PAGE IS FULL IF SO GOTO NEW PAGE AND PRINT HEADER       
C 
         IF(LINCNT.GT.50) THEN           
            CALL TITLE('X21 STATION LISTING ',REPNM2,1,7,PAGE,DAYCDC)           
            WRITE(REPLU,9020)         
            LINCNT=7        
         ENDIF 
C 

C	NEED FOR DISPLAY ONLY...NOT FOR SORTING	!!!
C	        DISTBL(AGTIDX,DISCNT)=AGTTAB(AGTNUM,TERM)
C		X2XS_GROUP(STN)
C	        DISTBL(NUMIDX,DISCNT)=X2XS_NUM_TERMS(PORT,STN)
C	END OF DISPLAY			!!!

         STN = SORTDATA(STNIDX,I)

C V05 CONVERT HEX ADDRESS TO ASCII.
        ADR_LEN=X2XS_ADRESS_LEN(STN)
CV05    CALL BCDASC(CHRSTR,1,ADR_LEN,X2XS_ADRESS(1,STN),ERR)
        CALL X2ALHTOA(CHRSTR,1,ADR_LEN,SORTDATA(ADRIDX,I),ERR)        !V05

	WRITE(REPLU,9011) STN,CHRSTR(1:ADR_LEN),
     *	 SORTDATA(NUMIDX,I),X2XS_GROUP(STN),SORTDATA(TERIDX,I),
     *   (ASFBYT(K),K=IDBEG(6),IDEND(6)),
     *   AGNOSTR,           
     *   (ASFBYT(K),K=IDBEG(2),IDEND(2)),
     *   (ASFBYT(K),K=IDBEG(3),IDEND(3)),
     *   (ASFBYT(K),K=IDBEG(4),IDEND(4)),
     *   (ASFBYT(K),K=IDBEG(5),IDEND(5)) 
         LINCNT=LINCNT+1    
300   CONTINUE 
C 
      WRITE(REPLU,9002) ACTAGT           
400   CLOSE(UNIT=REPLU)     
      CALL SPOOL('X2STN.REP',COPY,ST)    
      GOTO 500 
C 
C     ==================== FORMAT STATEMENTS =================     
C 
8000  FORMAT('X2STN',I1,'.REP')          
8001  FORMAT('X2STN',I1)    
9002  FORMAT(/,'TOTAL NUMBER OF AGENTS: ',I6)         
9010  FORMAT(2X,'Stn',1X,'Address',1X,'Num',1X,'Grp',1X,'Term',1X, 
     *       'Drp',         
     *       1X,'Agt no.',1X,'Name',24X,'City',17X,   
     *       'Postal',1X,'Phone',2X,/,1X,131('='))    
9011  FORMAT(1X,I4,1X,A7,1X,I3,1X,I3,1X,I4,2X,
     *       <IDLEN(6)>A,1X,A7,1X,<IDLEN(2)>A,1X,<IDLEN(3)>A,1X,
     *	     <IDLEN(4)>A,1X,<IDLEN(5)>A)     
9020  FORMAT(2X,'Stn',1X,'Address',1X,'Num',1X,'Grp',1X,'Term',1X, 
     *       'Drp',         
     *       1X,'Agt no.',1X,'Name',24X,'City',17X,   
     *       'Postal',1X,'Phone',2X,/,1X,131('='))    
9030  FORMAT(1X,'Grp',2X,'Stn',1X,'Address',1X,'Num',1X,'Term',1X, 
     *       'Drp',         
     *       1X,'Agt no.',1X,'Name',24X,'City',17X,   
     *       'Postal',1X,'Phone',2X,/,1X,131('='))    
9040  FORMAT(1X,'Address',2X,'Stn',1X,'Num',1X,'Grp',1X,'Term',1X, 
     *       'Drp',         
     *       1X,'Agt no.',1X,'Name',24X,'City',17X,   
     *       'Postal',1X,'Phone',2X,/,1X,131('='))    
9021  FORMAT(1X,A4,1X,I7,1X,I3,1X,I3,1X,I4,2X,        
     *       2A,1X,A7,1X,27A,1X,20A,1X,6A,1X,12A)     
9031  FORMAT(1X,A3,1X,I4,1X,I7,1X,I3,1X,I4,2X,        
     *       2A,1X,A7,1X,27A,1X,20A,1X,6A,1X,12A)     
9041  FORMAT(1X,A7,1X,I4,1X,I3,1X,I3,1X,I4,2X,        
     *       2A,1X,A7,1X,27A,1X,20A,1X,6A,1X,12A)     
9050  FORMAT(1X,'Num',2X,'Stn',1X,'Address',1X,'Grp',1X,'Term',1X, 
     *       'Drp',         
     *       1X,'Agt no.',1X,'Name',24X,'City',17X,   
     *       'Postal',1X,'Phone',2X,/,1X,131('='))    
9051  FORMAT(1X,A3,1X,I4,1X,I7,1X,I3,1X,I4,2X,        
     *       2A,1X,A7,1X,27A,1X,20A,1X,6A,1X,12A)     
      END      
