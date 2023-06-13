      ******************************************************************
      * Author:JEFFERSON MOTA SILVA(GERO)
      * Date:26/03/23
      * Purpose:PRATICAR CRUD EM COBOL/CONSULTAR USUARIO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSUSER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADUSER ASSIGN TO
           "C:\Users\PC\Desktop\CRUD\CADUSER.cbl"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FD-CD-USER
           FILE STATUS IS WS-FS-CADUSER.

       DATA DIVISION.
       FILE SECTION.
       FD CADUSER.
       01 REG-USER.
           03 FD-CD-USER                 PIC 9(6).
           03 FD-NM                      PIC X(25).
           03 FD-EMAIL                   PIC X(30).
           03 FD-PHONE                   PIC 9(12).
           03 FD-PASSWORD                PIC X(8).

       WORKING-STORAGE SECTION.
       77 WS-FS-CADUSER                  PIC 99.
        88 FS-CADUSER-OK                 VALUE 0.
       77 WS-OPCAO                       PIC X.
       77 WS-EOF                         PIC X.
        88 EOF-OK                        VALUE "S" FALSE "N".
       77 WS-COUNT                       PIC 9.
       01 CAD-USER.
           03 WS-CD-USER                 PIC 9(6).
           03 WS-NM                      PIC X(25).
           03 WS-EMAIL                   PIC X(30).
           03 WS-PHONE                   PIC 9(12).
           03 WS-PASSWORD                PIC X(8).

       PROCEDURE DIVISION.

       P400-CONSULTAR.
            OPEN INPUT CADUSER

            IF FS-CADUSER-OK THEN
                DISPLAY "DIGITE O CODIGO DO USUARIO"
                ACCEPT FD-CD-USER

                MOVE FD-CD-USER TO WS-CD-USER

                READ CADUSER INTO CAD-USER
                KEY IS FD-CD-USER
                INVALID KEY
                DISPLAY "CADASTRO NAO ENCONTRADO "
                DISPLAY "ERRO NO REGISTRO, WS-FS "WS-FS-CADUSER
                PERFORM P401-M-CONSULTA THRU P401-FIM
            NOT INVALID KEY
               DISPLAY "CADASTRO ENCONTRADO COM SUCESSO"
           DISPLAY "==================================================="
               DISPLAY "REGISTRO:"
                        WS-CD-USER
               DISPLAY "NOME:"
                        WS-NM
               DISPLAY "EMAIL:"
                       WS-EMAIL
               DISPLAY "PHONE:"
                        WS-PHONE
               DISPLAY "PASSWORD:"
                        WS-PASSWORD
           DISPLAY "==================================================="
               END-IF
               CLOSE CADUSER.
       P401-M-CONSULTA.
               DISPLAY "<N> PARA NOVA CONSULTA"
               DISPLAY "<F> PARA FINALIZAR"

           ACCEPT WS-OPCAO.
           IF WS-OPCAO EQUAL "N" THEN
               PERFORM P400-CONSULTAR
           ELSE
               IF WS-OPCAO EQUAL "F" THEN
                   PERFORM FINALIZAR.
       P401-FIM.
       P400-FIM.

           FINALIZAR.
            STOP RUN.
       END PROGRAM CONSUSER.
