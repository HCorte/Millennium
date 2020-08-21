C
C READ_ROM_REV.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]READ_ROM_REV.FOV                             $
C  $Date::   17 Apr 1996 14:39:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C READ_ROM_REV.FOR
C
C V08 27-JAN-00 KW  Add new record type and processing for Altura terminal.
C                   The format is as following:
C                   * MCP 1
C                   AB** MCPI 02 FCLI ISYS
C                   * ALTURA
C
C V03 12-DEC-95 DAS Added code to handle background load (ALLDLL)
C V02 09-NOV-95 WJK USE SYSTEM FLAG TO DETERMINE ROMREV.FIL PATH
C V01 13-SEP-95 SCD RELEASED FOR VAX
C
C CALLING SEQUENCE:
C     CALL READ_ROM_REV
C INPUT
C     NO FORMAL INPUTS
C OUTPUT
C     STATUS - ERROR STATUS
C	       0 = NO ERROR
C	       1 = OPEN ERROR (AUX_STATUS = SYSTEM ERROR CODE)
C	       2 = INVALID BLOCK TYPE (AUX_STATUS = 0)
C	       3 = COUNT EXCEEDS EITHER MAXMCP OR MAXFCL PARAMETER 
C		   (AUX_STATUS = COUNT FROM BAD DATA RECORD)
C	       4 = COUNT EXCEEDS EITHER MAXSFT OR MAXGVT PARAMETER 
C		   (AUX_STATUS = COUNT FROM BAD DATA RECORD)
C     AUX_STATUS - AUXILIARY ERROR STATUS - DEPENDS UPON VALUE OF STATUS
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
C
	SUBROUTINE READ_ROM_REV (STATUS,AUX_STATUS)
C
C	AUTHOR: S. DANIELS 12-SEP-1995
C
C	The purpose of this subroutine is to populate the arrays which
C	were formerly initialized by DATA STATEMENTS in PRMDLL.DEF based
C	upon the contents of the specified data file.  The intention is
C	to make more flexible the changing of terminal ROM REVS by using
C	a data file instead of using data statements which precipitate
C	recompiling source code.  The terminal ROM REV, MCP file name, terminal
C	identification number, FCL file name, GVT SOFTLOADER ROM REV, 
C	SOFTLOADER file name, GVT application ROM REV and the GVT application 
C	file name are now read from a data file and are returned to the calling
C	via the LOCAL COMMON defined in PRMDLL.DEF.  Comments in the data file
C	start with '#'.  Flag records, which indicate the type and number of 
C	records to follow, start with a '*'.  For more information on the
C	file format, refer to the comments in the ROM REVISION file read 
C	by this routine.
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMDLL.DEF'
        INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE '($RMSDEF)'
C
C	Output arguments
	INTEGER*4	STATUS			!return status
	INTEGER*4	AUX_STATUS		!auxiliary return status

C	Error status values
	INTEGER*4	OPEN_ERROR		!file open error 
	PARAMETER      (OPEN_ERROR = 1)
	INTEGER*4	INV_BLOCK_TYPE		!invalid block type error 
	PARAMETER      (INV_BLOCK_TYPE = 2)
	INTEGER*4	MAX_MCP_REC_EXCEEDED	!number of records exceeds 
	PARAMETER      (MAX_MCP_REC_EXCEEDED = 3)!MAXMCP or MAXFCL error 
	INTEGER*4	MAX_SFT_REC_EXCEEDED	!number of records exceeds 
	PARAMETER      (MAX_SFT_REC_EXCEEDED = 4)!MAXSFT or MAXGVT error 

	INTEGER*4	ST			!open or read status
	INTEGER*4	I,J
	INTEGER*4	TEMP			!variable used to convert
						!ASCII ROM REVs to hex
	INTEGER*4	SHIFT			!number of bit positions for
						!shifting operations
	INTEGER*4	IDX,SUBIDX		!values returned by 
						!STR$FIND_FIRST_SUBSTRING - used
						!to determine type of FIELD rec

	INTEGER*4	T_SFTNAM		!SOFTLOADER name as it appears
						!in the file - used as temp
						!storage until we determine
						!whether this is a unique name
	INTEGER*4 	T_GVTNAM		!GVT APPLICATION  name as it 
						!appears in the file - used as 
						!temp storage until we determine
						!whether this is a unique name
        INTEGER*4       TMPNAM(MAXFCL)
        INTEGER*4       TMPTIN(MAXFCL)
        
        INTEGER*4       NAME 
        INTEGER*4       ACLMASK/Z00FFFFFF/      !ACL MASK
        INTEGER*4       ACLSTRING/Z004C4341/

	LOGICAL 	END_OF_FILE
	LOGICAL		MATCH_FOUND		!indicates whether a file name
						!is unique

	INTEGER*2	MATCH_INDEX		!offset into GVT-related name
						!arrays to determine unique
						!SOFTLOADER or GVT APPLICATION
						!file names
	INTEGER*2 	ROMID_LEN		!length of ROMID in characters
	PARAMETER      (ROMID_LEN = 4)
	INTEGER*2	BLOCK_TYPE		!tells which type of records
						!follow
	INTEGER*2	MCP_BLOCK, SFT_BLOCK	!valid block types
	PARAMETER      (MCP_BLOCK = 1)
	PARAMETER      (SFT_BLOCK = 2)
	INTEGER*2 	NUM_REC			!number of records of BLOCK_TYPE
						!which follow in the file
	INTEGER*2	SFT_FILE_POINTER	!pointer into SFTLOADER name 
						!array - counter for number of
						!unique bin file names
	INTEGER*2	GVT_FILE_POINTER	!pointer into GVT application 
						!name  array - counter for 
						!number of unique bin file names
	CHARACTER*8	FIELD_REC		!1st 8 characters of a record -
						!equivalenced to make FIELD
						!record processing easier

	CHARACTER*3     MCP_STRING, SFT_STRING	!type of record block we're
	PARAMETER      (MCP_STRING = 'MCP')	!going to read
	PARAMETER      (SFT_STRING = 'SFT')

        CHARACTER*1	CHR80(80)		!full ROM REV record string

	CHARACTER*1	CHAR_ROM_ID(4)		!temp variables used to read
	CHARACTER*1	CHAR_SFT_ID(4)		!the file record so we can
	CHARACTER*1	CHAR_GVT_APP_ID(4)	!convert ASCII characters to hex

C       START OF V08

        CHARACTER*6    ALTURA_STRING            !type of Altura block we are
        PARAMETER      (ALTURA_STRING = 'ALTURA')!going to read
        INTEGER*2      ALTURA_BLOCK             !Altura block type
        PARAMETER      (ALTURA_BLOCK = 3)

C       END OF V08
 	EQUIVALENCE    (CHR80,FIELD_REC)	!for easier FIELD record
						!processing

	STATUS = 0
	AUX_STATUS = 0

        IF (PX2X_TASK) THEN                                     ! V02
          CALL OPENX(2,'PX2XFILES:ROMREV.FIL',4,0,0,ST)
        ELSE                                                    ! V02
          CALL OPENX(2,'ROMREV.FIL',4,0,0,ST)                   ! VO2
        ENDIF                                                   ! V02
C	OPENX OVERWRITES THE DEFAULT SYSTEM ERROR WHEN A FILE IS NOT
C	FOUND, IE. IT SETS ST TO -1 FOR FILE NOT FOUND.  IF WE GET
C	A FILE NOT FOUND CONDITION, RESET ST TO THE SYSTEM ERROR SO
C	WE CAN RETURN VMS ERROR CODE TO CORRECTLY DISPLAY THE ERROR
	IF (ST .EQ. -1) ST = RMS$_FNF
        IF (ST.NE.0) THEN
           STATUS = OPEN_ERROR
           AUX_STATUS = ST
	   RETURN
        ENDIF


	END_OF_FILE = .FALSE.
C       START OF V08--INITIALIZE VARIABLES

        ALTURA_ROM = 0
        ALTURA_TIN = 0

C       END OF V08

	DO WHILE (.NOT. END_OF_FILE)

C
C       Read the records
C
           CHR80(1) = ' '
           READ(2,9001,END=50,ERR=50,IOSTAT=ST) CHR80
	   CALL STR$UPCASE (CHR80,CHR80)

C       Comment records start with #
C
           IF (CHR80(1).EQ.'#') THEN
            
	      CONTINUE

C       Flag records start with *
C
           ELSEIF (CHR80(1).EQ.'*') THEN		!check for MCP record
	      CALL STR$FIND_FIRST_SUBSTRING (CHR80,IDX,SUBIDX,MCP_STRING)
	      IF (IDX .EQ. 0 .AND . SUBIDX .EQ.0) THEN	!not MCP record...
							!check for SFT record
	          CALL STR$FIND_FIRST_SUBSTRING (CHR80,IDX,SUBIDX,SFT_STRING)
	          IF (IDX .EQ. 0 .AND . SUBIDX .EQ.0) THEN	!not SFT record 
							!either
C                     START OF V08

                      CALL STR$FIND_FIRST_SUBSTRING     !check for ALTURA record
     *                     (CHR80,IDX,SUBIDX,ALTURA_STRING)
                      IF (IDX .EQ. 0 .AND . SUBIDX .EQ. 0) THEN !not Altura
                          STATUS = INV_BLOCK_TYPE               !record either
                          AUX_STATUS = 0
                          RETURN
                      ELSE                      !this is an Altura record
                          BLOCK_TYPE = ALTURA_BLOCK
                      ENDIF

C                     END OF V08

	          ELSE					!this is an SFT record

		      BLOCK_TYPE = SFT_BLOCK

	          ENDIF
	      ELSE					!this is an MCP record
	          BLOCK_TYPE = MCP_BLOCK
	      ENDIF

C	Extract record count from the record we've just read.  If we get here,
C	then block type must be valid since we immediately return of block
C	type is invalid
              IF (BLOCK_TYPE .EQ. MCP_BLOCK .OR.   !V08--Only MCP or SFT
     *            BLOCK_TYPE .EQ. SFT_BLOCK)       !records need to extract
     *            READ (FIELD_REC,9002) NUM_REC    !record count


	      IF (BLOCK_TYPE .EQ. MCP_BLOCK) THEN	!MCP records follow
	          IF (NUM_REC .GT. MAXMCP .OR. NUM_REC .GT. MAXFCL) THEN
           	      STATUS = MAX_MCP_REC_EXCEEDED
           	      AUX_STATUS = NUM_REC
		      RETURN

	          ELSE 
C	Read the remaining records.  The MCP ROM REV is read in as character
C	data and converted to hex for storage in MCPROM vector, one character
C	at a time.
		     DO I = 1,NUM_REC
		        READ (2,9003) CHAR_ROM_ID,MCPNAM(I),
C....*				      FCLTIN(I),FCLNAM(I)
     *				      TMPTIN(I),TMPNAM(I)
			MCPROM(I) = 0
			SHIFT = 24
			DO J = 1,ROMID_LEN
	   		   TEMP = ICHAR(CHAR_ROM_ID(J))
	   		   MCPROM(I) = MCPROM(I) + ISHFT(TEMP,SHIFT)
	   		   SHIFT = SHIFT - 8
			END DO		!on J
		     END DO		!on I
	          ENDIF			!on NUM_REC check
	      ENDIF			!on MCP TYPE check
C
C             Determine if name is for an ACL or FCL and stop the name in the appropiate variable
C
              DO I = 1, NUM_REC
                 NAME = IAND(TMPNAM(I),ACLMASK)
                 IF (NAME.EQ.ACLSTRING)THEN 
                    ACLNAM(I) = TMPNAM(I)
                 ELSE
                    FCLNAM(I) = TMPNAM(I)
                    FCLTIN(I) = TMPTIN(I)
                 ENDIF
               ENDDO  
C
            
	      IF (BLOCK_TYPE .EQ. SFT_BLOCK) THEN	!GVT records follow
	          IF (NUM_REC .GT. MAXSFT .OR. NUM_REC .GT. MAXGVT) THEN
           	      STATUS = MAX_SFT_REC_EXCEEDED
           	      AUX_STATUS = NUM_REC
		      RETURN

                  ELSE
C	Read the remaining records.  GVT ROM revs use only the LOW 4 bits
C	of the ASCII representation for each character in the ROM rev so we
C	end up with a 16-bit ROM rev for a 4 character GVT ROM ID.
C	Due to the possible mapping of one SOFTLOADER or GVT APPLICATION file
C	file name to many ROM revs, we also need to fill in the SFT_LIST 
C	and/or GVT_LIST vectors.  The entries in SFT_LIST and GVT_LIST give the
C	index into the SFTNAM and GVTNAM vectors, respectively, so we can
C	map multiple ROM revisions to one filename.
C	

		     SFT_FILE_POINTER = 0	!initialize pointers for keeping
		     GVT_FILE_POINTER = 0	!track of unique file names

		     DO I = 1,NUM_REC
		        READ (2,9004) CHAR_SFT_ID,T_SFTNAM,
     *				      CHAR_GVT_APP_ID,T_GVTNAM
			SFTROM(I) = 0
			GVTROM(I) = 0
			SHIFT = 12
			DO J = 1,ROMID_LEN
	   		   TEMP = ICHAR(CHAR_SFT_ID(J))	!do SOFTLOADER first
			   TEMP = IAND(TEMP,'0F'X)	!only use LOW 4 bits
	   		   SFTROM(I) = SFTROM(I) + ISHFT(TEMP,SHIFT)

	   		   TEMP = ICHAR(CHAR_GVT_APP_ID(J)) !do APPLICATION next
			   TEMP = IAND(TEMP,'0F'X)	!only use LOW 4 bits
	   		   GVTROM(I) = GVTROM(I) + ISHFT(TEMP,SHIFT)

	   		   SHIFT = SHIFT - 4
			END DO		!ON J

C	Now check for uniqueness of file names.  If a bin file is used
C	for multiple SFT revs or GAP revs, then adjust the pointers in the
C	SFT_LIST or GAP_LIST to point to the correct file name in the SFTNAM
C	or GVTNAM vectors.

			IF (I .EQ. 1) THEN	!first record...name must be
						!unique
			    SFT_FILE_POINTER = SFT_FILE_POINTER + 1
			    SFTNAM(SFT_FILE_POINTER) = T_SFTNAM
			    SFT_LIST(I) = 1

			    GVT_FILE_POINTER = GVT_FILE_POINTER + 1
			    GVTNAM(GVT_FILE_POINTER) = T_GVTNAM
			    GVT_LIST(I) = 1
			ELSE			!not first record so we must
						!check for uniqueness
C	Check SOFTLOADER files first
			    MATCH_FOUND = .FALSE.
			    J = 1
			    MATCH_INDEX = 0
			    DO WHILE ((.NOT.MATCH_FOUND) .AND. 
     *				      (J .LE. SFT_FILE_POINTER))
			       IF (T_SFTNAM .EQ. SFTNAM(J)) THEN !same name
				   MATCH_FOUND = .TRUE.
				   MATCH_INDEX = J
			       ELSE				 !keep looking
				   J = J + 1
			       ENDIF
			    END DO

			    IF (MATCH_FOUND) THEN		 !same as a 
				SFT_LIST(I) = MATCH_INDEX	 !previous name
				SFTNAM(I) = '    '
			    ELSE				 !new name
			        SFT_FILE_POINTER = SFT_FILE_POINTER + 1
				SFT_LIST(I) = SFT_FILE_POINTER
			        SFTNAM(SFT_FILE_POINTER) = T_SFTNAM
			    ENDIF				!on match_found

C	Now check GVT application files
			    MATCH_FOUND = .FALSE.
			    J = 1
			    MATCH_INDEX = 0
			    DO WHILE ((.NOT.MATCH_FOUND) .AND. 
     *				      (J .LE. GVT_FILE_POINTER))
			       IF (T_GVTNAM .EQ. GVTNAM(J)) THEN !same name
				   MATCH_FOUND = .TRUE.
				   MATCH_INDEX = J
			       ELSE				 !keep looking
				   J = J + 1
			       ENDIF
			    END DO
			    IF (MATCH_FOUND) THEN		 !same as a
				GVT_LIST(I) = MATCH_INDEX	 !previous name
				GVTNAM(I) = '    '
			    ELSE				 !new name
			        GVT_FILE_POINTER = GVT_FILE_POINTER + 1
				GVT_LIST(I) = GVT_FILE_POINTER
			        GVTNAM(GVT_FILE_POINTER) = T_GVTNAM
			    ENDIF				!on match_found
			ENDIF 			!on I = 1 check
	 	     END DO		!on I
	          ENDIF			!on NUM_REC check
	      ENDIF			!on SFT TYPE check
C
C             START OF V08--Read the ALTURA record.  The ROM REV is read in
C             as character data and converted to hex one character at a time
C             for storage in ALTURA_ROM

              IF (BLOCK_TYPE .EQ. ALTURA_BLOCK) THEN   !Altura record follows
                  READ (2,9005) CHAR_ROM_ID, ALTURA_TIN
                  ALTURA_ROM = 0
                  SHIFT = 24
                  DO J = 1,ROMID_LEN
                      TEMP = ICHAR(CHAR_ROM_ID(J))
                      ALTURA_ROM = ALTURA_ROM + ISHFT(TEMP,SHIFT)
                      SHIFT = SHIFT - 8
                  END DO          !on J
              ENDIF

C             END OF V08
C
           ENDIF 			!on CHR80 check

  50	   IF (ST .NE. 0) END_OF_FILE = .TRUE.
	END DO

	CLOSE(2)

9001    FORMAT(80A1)
9002	FORMAT (5X,1X,I2)
9003	FORMAT (4A1,1X,A4,1X,Z2,1X,A4)
9004	FORMAT (4A1,1X,A4,1X,4A1,1X,A4)
9005    FORMAT (4A1,1X,Z2)              !V08
 	RETURN
	END
