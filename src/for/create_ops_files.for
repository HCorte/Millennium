C
C	PROGRAM TO CREATE NEW FILES ASSOCIATED TO OP'S TO PORTUGAL BASELINE (RUNS ONCE)
C  
C       V01 13-DEC-2010 FJG Change KEYs
C
	PROGRAM CTOPSFIL

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:OPS_REC.DEF'
	INCLUDE 'INCLIB:BRANCH_REC.DEF'

	INTEGER*4 OPCAO

	type*,'===================================================================='	
	type*,'  1) CREATE BRANCH FILE '
	type*,'  2) CREATE OP     FILE '
	type*,'  3) CREATE BOTH BRANCH AND OP FILES '
	type*,'  '
	type*,'OPCAO:'
	ACCEPT*, OPCAO
	
	IF (OPCAO.EQ.2 .OR. OPCAO.EQ.3) THEN
C
C	CREATE ORDENS DE PAGAMENTO FILE
C
	OPEN (UNIT           = 1,
     *        FILE           = 'FILE:OPS.FIL',
     *        STATUS         = 'NEW',
     *        ORGANIZATION   = 'INDEXED',
     *        ACCESS         = 'KEYED',
     *        FORM           = 'UNFORMATTED',
     *        RECL           = SIZEOF(OPS_REC),
     *        KEY            = (1:15:CHARACTER:ASCENDING, 16:29:CHARACTER:ASCENDING),
     *        RECORDTYPE     = 'FIXED',
     *        INITIALSIZE    = 50000)

        CLOSE(1)
	ENDIF


	IF (OPCAO.EQ.1 .OR. OPCAO.EQ.3) THEN
C
C	CREATE BRANCH FILE
C
	OPEN (UNIT           = 3,
     *        FILE           = 'FILE:BRANCH.FIL',
     *        STATUS         = 'NEW',
     *        ORGANIZATION   = 'INDEXED',
     *        ACCESS         = 'KEYED',
     *        FORM           = 'UNFORMATTED',
     *        RECL           = SIZEOF(BRANCH_REC),
     *        KEY            = (1:8:CHARACTER:ASCENDING),
     *        RECORDTYPE     = 'FIXED',
     *        INITIALSIZE    = 2000)
	CLOSE(3)
	ENDIF


	STOP
	END
