C
C SUBROUTINE PRTHLP
C $Log:   GXAFXT:[GOLS]PRTHLP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:33:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:23:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - fixfil.for **
C
C
C
C
C *** PRTHLP ***
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PRTHLP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 X
C
	WRITE(5,*)' '
	WRITE(5,*)' '
	WRITE(5,*)' '
	WRITE(5,*)' '
C
	WRITE(5,1001)
1001	FORMAT(/,
     *	 ' The output format is: ',/,
     *	 ' RECORD WRDOFF HLFOFF BYTOFF BIAS+ LOC (FM)= VALUE :',/,
     *	 ' where  RECORD is the record number',/,
     *	 '        WRDOFF is the fullword offset (& byte #)',/,
     *	 '        HLFOFF is the halfword offset (& byte #)',/,
     *	 '        BYTOFF is the   byte   offset',/,
     *	 '          BIAS is the current bias',/,
     *	 '           LOC is the offset from the current bias',/,
     *	 '            FM is the format',/,
     *	 '          VALUE is the value at that location',/,
     *	 ' ',/,
     *	 ' Offsets and values change depending on display format.',/,
     *	 ' You can change format by typing DSP=FULL or DSP=SHORT')
C
	WRITE(5,*)' '
	CALL WIMG(5,'PRESS ANY KEY TO CONTINUE: ')
	ACCEPT 9001,X
9001	FORMAT(A1)
C
	WRITE(5,1002)
1002	FORMAT(/,
     *	 ' Each time you hit return, the next offset will be shown.',/,
     * ' A minus sign (-) will cause the prior offset to be shown.',/,
     *	 '  +nnn goes forward  nnn locations',/,
     *	 '  -nnn goes backward nnn locations',/,
     *	 ' ',/,
     *	 ' All offsets are displayed relative to the bias.',/,
     *	 ' When first started, the bias is set to 0 so that',/,
     *	 ' if you want to display a location, simply enter the',/,
     *	 ' location number followed by return.',/,
     *	 ' ',/,
     *	 ' You may change the bias by using BI=. or BI=nnn.',/,
     *	 ' If you want to reference an absolute location without',/,
     *	 ' changing the bias, enter the location as Annnn.')
C
	WRITE(5,*)' '
	CALL WIMG(5,'PRESS ANY KEY TO CONTINUE: ')
	ACCEPT 9001,X
C
	WRITE(5,1003)
1003	FORMAT(/,
     *	 ' You may change the contents of a location by entering',/,
     *	 ' the new value preceded by an equals sign (=).',/,
     * ' The new value must be appropriate for the format in which',/,
     * ' you are displaying.  For example, if you are in I2 format',/,
     *	 ' the value must be between -32768 and +32767.  If you are',/,
     *	 ' in C1 format, you must enter a single ascii character.',/,
     *	 ' If you are in Z2, Z4, or Z8 format, you must enter',/,
     *	 ' exactly 2, 4, or 8 digits (i.e., use leading 0''s).')
C
	WRITE(5,1004)
1004	FORMAT(/,
     *	 ' If you wish to change to a different format, enter the',/,
     *	 ' desired format preceded by FMT=.  For example, to',/,
     *	 ' change to I2 format, enter FMT=I2.')
C
	WRITE(5,1005)
1005	FORMAT(/,
     *	 ' When you are done, enter E to exit without changing',/,
     *	 ' the record.  Enter W if you want to re-write the',/,
     *	 ' record before exiting.')
C
	WRITE(5,1999)
1999	FORMAT(/,
     *	 ' If you have any questions, call Tom Oram.',/,
     *	 '    ***** G O O D   L U C K *****',//)
C
	RETURN
	END
