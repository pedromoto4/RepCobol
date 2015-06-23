      *----------------------------------------------------------------
      *| Realizado por..:                          Fecha:
      *| Aplicacion.....:
      *| Sistema........:
      *| Area...........:
      *| Descripcion....:
      *| Funcion........:
      *|
      *----------------------------------------------------------------
      *| Mantenimientos efectuados
      *| --FECHA-- --RESPONSABLE--  --------DESCRIPCION DE AJUSTE------
      *| xx/xx/xx
      *| xx/xx/xx
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PL0010.
       ENVIRONMENT DIVISION.
       special-names.

       call-convention 74 is winapi.

       FILE-CONTROL.

       SELECT TEXTVAR
                ASSIGN          TO TEXTVAR-PATH
                ORGANIZATION    IS LINE SEQUENTIAL
                FILE STATUS     IS RESERVADA
                ACCESS          IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD TEXTVAR.

       01 REG-TEXTVAR.
           02 CODTBL &LIKE CODTBL
           02 ESPACO PIC XX VALUE "0A".
           02 ARGBUS &LIKE ARGBUS
           02 FUNCIO &LIKE FUNCIO



       WORKING-STORAGE SECTION.
       01 FF-EURO-G PIC X(6) VALUE "200482".
       01 FF-EURO PIC S9(3)V999
          REDEFINES FF-EURO-G.
       77 OAS-EURO001 PIC S9(12)V999.
       77 OAS-EURO002 PIC S9(12)V999.
       77 OAS-EURO003 PIC S9(12)V999.
       77 OAS-EURO004 PIC S9(12)V999.
       77 OAS-EURO005 PIC S9(12)V999.
       77 OAS-EURO006 PIC S9(12)V999.
       77 OAS-EURO007 PIC S9(12)V999.
       77 OAS-EURO008 PIC S9(12)V999.
       77 OAS-EURO009 PIC S9(12)V999.

      * Registo com nomes dos campos pretendidos no ecran - alterar
       01 FROW-WK.
           &CAMPOS MSTTBL

       77 TEXTVAR-PATH PIC X(256) VALUE SPACES.
       77 RESERVADA    PIC XX VALUE "00".


      * VARIAVEIS AUXILIARES
       77 BYTE-1       PIC X.
      * Variavel para mensagem de erro

       LINKAGE SECTION.
       01 PARAM-WRITE.
           02 OPERACAO     PIC X(6).
           02 FICHEIRO     PIC X(20).
           02 TEXTO        PIC X(160).
       01 NOVA-VAR         PIC X(10).

       PROCEDURE DIVISION USING PARAM-WRITE.
       INICIO.
      *-------
           PERFORM INICIALIZACIONES
           PERFORM TRATA-PESQ
           .

       FIN-PROGRAMA.
      *-------------
           CLOSE TEXTVAR

           &COMIT WORK
           EXIT PROGRAM.
           STOP RUN.

       INICIALIZACIONES.
      *-----------------
           &LDAREA1
           STRING  "C:\"
                   "OUTPUTT1" ".TXT"
                   INTO TEXTVAR-PATH
           END-STRING
           OPEN OUTPUT  TEXTVAR

           .

       TRATA-PESQ.
      *-----------
         INITIALIZE REG-MSTTBL
         MOVE "USERID" TO CODTBL OF REG-MSTTBL

         &STR MSTTBL,MSTTBL01,NL,[1500],CODTBL,

         PERFORM VARYING PTR-MSTTBL FROM 1 BY 1 UNTIL PTR-MSTTBL >
            IO-NUMREC
            MOVE CORR ARR-REG-MSTTBL(PTR-MSTTBL) TO REG-MSTTBL
            PERFORM WRITE-TXT
               IF PTR-MSTTBL = 1500 THEN
                  MOVE ARR-REG-MSTTBL(PTR-MSTTBL) TO REG-MSTTBL
                  &STR MSTTBL,MSTTBL01,GT,[1500],CODTBL,ARGBUS
                  INITIALIZE PTR-MSTTBL
         END-PERFORM
         .

       WRITE-TXT.
      *----------
           MOVE CORR REG-MSTTBL TO REG-TEXTVAR
      *        MOVE X"0D"    TO BYTE-1
      *        MOVE BYTE-1   TO REG-TEXTVAR(4:1)
      *        MOVE X"0A"    TO BYTE-1
      *        MOVE BYTE-1   TO REG-TEXTVAR(6:1)
           WRITE  REG-TEXTVAR
           .


       NADA.
      *-----
           DISPLAY "NADA"
           .
