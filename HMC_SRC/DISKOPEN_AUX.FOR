        INTEGER*4 FUNCTION DISKOPEN_AUX(FAB, RAB, LUN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE	'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DISKIO.DEF'
C
        INCLUDE	'($SYSSRVNAM)'
        INCLUDE '($FABDEF)'
        INCLUDE '($RABDEF)'
        INCLUDE '($RMSDEF)'
C
        INTEGER*4 LUN
C
        RECORD /FABDEF/ FAB
        RECORD /RABDEF/ RAB
C
C Set the block I/O bit in the FAC
C
C !!!OpenVMS 7.3-1 Record Management Services Reference Manual - RMS .pdf!!!
C
C 4.14 FAB$B_FAC Field, RMS uses this field, together with the share
C field (SHR) in each potential accessor’s FAB, to determine whether to permit a
C process to access a file
C
C FAB$V_BRO
C Requests file access for doing either block I/O or record I/O as determined by the
C state of the RAB$V_BIO bit in the RAB at connect time. Mixed block and record
C I/O operations are restricted to sequential files. For additional information, see
C Section 7.19 and Section B.3.10.
C This option corresponds to the FDL attribute ACCESS RECORD_IO
C
        FAB.FAB$B_FAC = FAB.FAB$B_FAC .OR. FAB$M_BIO
C
C Set complete access privileges
C
C FAB$V_UPI
C This option is used when the user wants to assume responsibility for interlocking
C of multiple, simultaneous accessors of a file. This option disables all RMS locking
C for the current access of the file. Except for block I/O, the FAB$V_MSE option
C overrides the FAB$V_UPI option. Usually, the FAB$V_UPI option is used for a
C file that is open for block I/O (FAB$V_BIO or FAB$V_BRO).
C
C When you select the FAB$V_UFO option, you must also select the FAB$V_UPI
C option if the file is write shared. A file is specified as being write shared when
C you select either the FAB$V_PUT option, the FAB$V_DEL option, the FAB$V_
C TRN option, or the FAB$V_UPD option in the FAB$B_SHR field
C
        FAB.FAB$B_SHR = FAB.FAB$B_SHR .OR. FAB$M_UPI
C
C Now open the file and connect to the record stream
C
        DISKOPEN_AUX = SYS$OPEN(FAB)
        IF(.NOT.DISKOPEN_AUX) THEN
          CALL LIB$SIGNAL(%VAL(DISKOPEN_AUX))
          GOTO 9000
        ENDIF
C
        DISKOPEN_AUX = SYS$CONNECT(RAB)
        IF(.NOT.DISKOPEN_AUX) THEN
          CALL LIB$SIGNAL(%VAL(DISKOPEN_AUX))
          GOTO 9000
        ENDIF
C
9000	CONTINUE
        RETURN
        END
