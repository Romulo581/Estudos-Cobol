       IDENTIFICATION DIVISION
           PROGRAM-ID. COMOVAI.
           AUTHOR. ROMULO CESAR.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  NOME  PIC A (020).

       01  DATA-ATUAL. 
           05 ANO-ATUAL PIC 9 (004).
           05 MES-ATUAL PIC 9 (002).
           05 DIA-ATUAL PIC 9 (002).

       PROCEDURE DIVISION.
       PRINCIPAL.
           DISPLAY "Olá, seja bem vindo".
           DISPLAY "Entre com seu nome: ".
           ACCEPT NOME.
           DISPLAY "É um prazer lhe conhecer " NOME.
           ACCEPT DATA-ATUAL FROM DATE YYYYMMDD.
           DISPLAY "Bem Vindo " NOME.
           DISPLAY "Sabia que hoje é " DIA-ATUAL "/" MES-ATUAL "/"
           ANO-ATUAL.

       STOP RUN.
       END PROGRAM COMOVAI.
