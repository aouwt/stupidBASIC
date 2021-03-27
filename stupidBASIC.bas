$NOPREFIX
TYPE PrgType
    l AS STRING
    c AS STRING
    a AS STRING
END TYPE
TYPE SymType
    n AS STRING
    l AS UNSIGNED LONG
END TYPE
TYPE VarType
    n AS STRING
    v AS STRING
END TYPE
'$DYNAMIC
DIM SHARED prg(1000) AS PrgType
DIM SHARED subs(100) AS SymType
DIM SHARED vars(100) AS VarType
DIM SHARED a AS STRING
DIM SHARED stk(100) AS SymType, StkPtr AS UNSIGNED BYTE

LoadPrg "prg.txt"
InterpretPrg


SUB LoadPrg (f$)
    f% = FREEFILE
    OPEN f$ FOR INPUT AS #f%

    DIM i AS UNSIGNED LONG
    DO
        LINE INPUT #f%, prg(i).l
        prg(i).l = LTRIM$(prg(i).l)

        s~% = INSTR(prg(i).l, " ")
        IF s~% = 0 GOTO skipsplit

        prg(i).c = LEFT$(prg(i).l, s~% - 1)
        prg(i).a = MID$(prg(i).l, s~% + 1)

        SELECT CASE prg(i).c
            CASE "program"
                subs(lastprg%).n = prg(i).a
                subs(lastprg%).l = i
                lastprg% = lastprg% + 1
        END SELECT

        skipsplit:
        i = i + 1
    LOOP UNTIL EOF(f%)
    REDIM PRESERVE prg(i) AS PrgType
    CLOSE f%
END SUB

SUB InterpretPrg
    DIM i AS UNSIGNED LONG

    FOR i = 0 TO UBOUND(prg)
        SELECT CASE prg(i).c
            CASE "get"
                SELECT CASE prg(i).a
                    CASE "random"
                        a = LTRIM$(STR$(RND))
                    CASE "key"
                        a = INKEY$
                    CASE ELSE
                        a = ""
                END SELECT

            CASE "load"
                a = GetVar(prg(i).a)

            CASE "store"
                SetVar prg(i).a, a

            CASE "print"
                PRINT a;

            CASE "say"
                PRINT prg(i).a;

            CASE "newline"
                PRINT

            CASE "ask"
                PRINT prg(i).a;
                LINE INPUT ; a

            CASE "if"
                IF a <> GetVar(prg(i).a) THEN CALL ExitBlock(i, "")

            CASE "ifne"
                IF a = GetVar(prg(i).a) THEN CALL ExitBlock(i, "")

            CASE "ifgt"
                IF a <= GetVar(prg(i).a) THEN CALL ExitBlock(i, "")

            CASE "iflt"
                IF a >= GetVar(prg(i).a) THEN CALL ExitBlock(i, "")

            CASE "ifge"
                IF a < GetVar(prg(i).a) THEN CALL ExitBlock(i, "")

            CASE "ifle"
                IF a > GetVar(prg(i).a) THEN CALL ExitBlock(i, "")

            CASE "loop"
                Push "loop", i

            CASE "program"
                subs(lastsub%).n = prg(i).a
                subs(lastsub%).l = i
                lastsub% = lastsub% + 1

            CASE "do"
                I% = 0
                DO
                    IF subs(I%).n = "" THEN
                        EXIT DO
                    ELSEIF subs(I%).n = prg(i).a THEN
                        Push "program", i
                    END IF
                    I% = I% + 1
                LOOP

            CASE "end"
                CALL Pull(s$, l~&)
                SELECT CASE s$
                    CASE "loop"
                        i = l~&
                    CASE "program"
                        i = l~&
                END SELECT

            CASE "multiply"
                a = LTRIM$(STR$(VAL(a) * VAL(prg(i).a)))
            CASE "divide"
                a = LTRIM$(STR$(VAL(a) / VAL(prg(i).a)))
            CASE "subtract"
                a = LTRIM$(STR$(VAL(a) - VAL(prg(i).a)))
            CASE "add"
                a = LTRIM$(STR$(VAL(a) + VAL(prg(i).a)))

            CASE "exit"
                CALL ExitBlock(i, prg(i).a)
        END SELECT
    NEXT
END SUB

FUNCTION GetVar$ (n AS STRING)
    DIM i AS UNSIGNED LONG
    DO
        IF vars(i).n = "" THEN
            vars(i).n = n
            EXIT FUNCTION
        ELSEIF vars(i).n = n THEN
            GetVar$ = vars(i).v
            EXIT FUNCTION
        END IF
        i = i + 1
    LOOP
END FUNCTION

SUB SetVar (n AS STRING, v AS STRING)
    DIM i AS UNSIGNED LONG
    DO
        IF vars(i).n = "" THEN
            vars(i).n = n
            EXIT SUB
        ELSEIF vars(i).n = n THEN
            vars(i).v = v
            EXIT SUB
        END IF
        i = i + 1
    LOOP
END SUB

SUB ExitBlock (i AS UNSIGNED LONG, block AS STRING)
    IF block = "" THEN
        DO
            i = i + 1
            SELECT CASE prg(i).c
                CASE "if", "ifne", "ifgt", "iflt", "ifge", "ifle", "program", "loop"
                    level% = level% + 1
                CASE "end"
                    IF level% <= 0 THEN EXIT SUB
            END SELECT
        LOOP

    ELSE
        DO
            i = i + 1
            SELECT CASE prg(i).c
                CASE "if", "ifne", "ifgt", "iflt", "ifge", "ifle", "program", "loop"
                    level% = level% + 1
                CASE "end"
                    IF (level% <= 0) AND (prg(i).a = block) THEN EXIT SUB
            END SELECT
        LOOP
    END IF
END SUB

SUB Push (n AS STRING, l AS UNSIGNED LONG)
    stk(StkPtr).n = n
    stk(StkPtr).l = l
    StkPtr = StkPtr + 1
END SUB

SUB Pull (n AS STRING, l AS UNSIGNED LONG)
    StkPtr = StkPtr - 1
    n = stk(StkPtr).n
    l = stk(StkPtr).l
END SUB
