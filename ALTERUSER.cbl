      ******************************************************************
      * Author:JEFFERSON MOTA SILVA(GERO)
      * Date:26/03/23
      * Purpose:PRATICAR CRUD EM COBOL/ALTERAR UM REGISTRO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTERUSER.

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

       77 WS-S-MASTER                           PIC X(6) VALUE "ABC123".
       77 WS-S-GESTOR                           PIC X(6) VALUE "DEF456".
       77 WS-S-ANALISTA                         PIC X(6) VALUE "GHI789".

       77 WS-S-ALTE-USER                        PIC X(6).

       01 CAD-USER.
           03 WS-CD-USER                 PIC 9(6).
           03 WS-NM                      PIC X(25).
           03 WS-EMAIL                   PIC X(30).
           03 WS-PHONE                   PIC 9(12).
           03 WS-PASSWORD                PIC X(8).

       PROCEDURE DIVISION.
       P500-ALTERAR.
           OPEN I-O CADUSER

           IF FS-CADUSER-OK THEN
               DISPLAY "DIGITE O CODIGO DE REGISTRO"
               ACCEPT WS-CD-USER

               MOVE WS-CD-USER  TO  FD-CD-USER

                READ CADUSER INTO CAD-USER
                KEY IS FD-CD-USER
                INVALID KEY
                DISPLAY "ERRO NO CODIGO DE ACESSO "WS-FS-CADUSER
                PERFORM P502-M-ALTERAR THRU P502-FIM
            NOT INVALID KEY

           DISPLAY "==================================================="
                   DISPLAY "CADASTRO ATUAL"
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
           END-READ
           DISPLAY "==================================================="
           DISPLAY "SOMENTE USUARIO COM PERMISSAO PODE ALTERAR"
           DISPLAY "DIGITE A SENHA PARA ALTERAR O CADASTRO"
           ACCEPT WS-S-ALTE-USER.

           IF WS-S-ALTE-USER EQUAL WS-S-MASTER OR WS-S-GESTOR OR
                                                  WS-S-ANALISTA

                DISPLAY "ATUALIZE O CADASTRO"

                   DISPLAY "ALTERE O CODIGO:"
                   ACCEPT   FD-CD-USER
                   DISPLAY "ALTERE NOME:"
                   ACCEPT   FD-NM
                   DISPLAY "ALTERE EMAIL:"
                   ACCEPT   FD-EMAIL
                   DISPLAY "ALTERE O PHONE:"
                   ACCEPT   FD-PHONE
                   DISPLAY "ALTERE O PASSWORD:"
                   ACCEPT   FD-PASSWORD

                   REWRITE REG-USER
                   DISPLAY "ALTERADO COM SUCESSO"
                   ELSE
                   DISPLAY "USUARIO SEM PERMISSAO PARA ALTERAR"
                   DISPLAY "FILE STATUS: "WS-FS-CADUSER

             END-IF

             CLOSE CADUSER.
      *P501-FIM.
       P502-M-ALTERAR.
             DISPLAY "<1> PARA NOVA ALTERACAO"

             DISPLAY "<F> PARA FINALIZAR"

               ACCEPT WS-OPCAO.

               IF WS-OPCAO EQUAL "F" THEN
              PERFORM FINALIZAR
               ELSE
               IF WS-OPCAO EQUAL 1 THEN
               PERFORM P500-ALTERAR.
       P502-FIM.

       P500-FIM.

           FINALIZAR.
            STOP RUN.
       END PROGRAM ALTERUSER.
