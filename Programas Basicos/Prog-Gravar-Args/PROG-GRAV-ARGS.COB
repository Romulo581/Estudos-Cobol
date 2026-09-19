       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG-GERA-ARG.
       AUTHOR. ROMULO CESAR.
       
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-DADOS-PESSOAIS-CLIENT
               ASSIGN TO "CPF-NOMES"
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT ARQ-ID-CEP-CLI
               ASSIGN TO "CPF-CEP"
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT ARQ-ID-CEP-CIDADES
               ASSIGN TO "CEP-Endereco"
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT ARQ-DADOS-BANC-CLIENT
               ASSIGN TO "CPF-Conta-Agencia"
               ORGANIZATION IS LINE SEQUENTIAL.
               
           SELECT  ARQ-DADOS-DO-BANCO
               ASSIGN TO "Conta-banco-divida"
               ORGANIZATION IS LINE SEQUENTIAL.
               
       DATA DIVISION.
       FILE SECTION.
      * Neste arquivo iremos encontrar o CPF do cliente e o nome do mesmo.    
       FD ARQ-DADOS-PESSOAIS-CLIENT.
       
       01 DETALHES-DADOS-CLIENT.
       
           88  FINAL-DET-CLIENT                    VALUE SPACE.
           05  CPF-COMPL.
               10  CGC-CLIEN.
                   15 CGC-CLIEN-1D                 PIC 9(003).
                   15 CGC-CLIEN-2D                 PIC 9(003).
                   15 CGC-CLIEN-3D                 PIC 9(003).
               10  CGC-DIG                         PIC 9(002).
               
           05  NOME-CLIENT.
               10  PRIMEIRO-NOME                   PIC X(020).
               10  SEGUNDO-NOME                    PIC X(020).
               
      * Neste arquivo iremos encantar o CPF que foi cadastrado no CEP
       FD  ARQ-ID-CEP-CLI.
       
       01 ENDE-CEP-CLI.
       
           88 FINAL-DET-CEP                        VALUE SPACE.
           05  CPF-CAD-CEP.
               10  CGC-CAD-CEP.
                   15 CGC-CAD-CEP-1D               PIC 9(003).
                   15 CGC-CAD-CEP-2D               PIC 9(003).
                   15 CGC-CAD-CEP-3D               PIC 9(003).
               10  CGC-DIG-CAD-CEP                 PIC 9(002).
           05  CEP-CLIENT                          PIC 9(008).
       
      * Neste arquivo vamos encontrar o endereco que corresponde ao CEP
       FD ARQ-ID-CEP-CIDADES.
       
       01  ENDE-DO-CEP.
       
           88  FINAL-ENDE-CEP                      VALUE SPACE.
           05  ENDE-CEP                            PIC 9(008).
           05  ENDERECO-CEP                        PIC X(036).
           05  BAIRRO-CEP                          PIC X(044).
           05  CIDADE-CEP                          PIC X(015).
        
      * Neste arquivo vamos encontrar os CPF cadastrado na conta agencia
       FD ARQ-DADOS-BANC-CLIENT.
       
       01  INF-BANC-CLIENT.
           
           88 FINAL-BANC-CLIENT                         VALUE SPACE.
           05  CPF-CAD-CONT.
               10  CGC-CONT-CLIEN.
                   15 CGC-CONT-CLIEN-1D                 PIC 9(003).
                   15 CGC-CONT-CLIEN-2D                 PIC 9(003).
                   15 CGC-CONT-CLIEN-3D                 PIC 9(003).
               10  CGC-CONT-DIG                         PIC 9(002).
           
           05  AGEN-CONT-CLIEN                          PIC 9(004).
           05  CONTA-CLIENT                             PIC 9(010).
           
      * Neste arquivo vamos encontrar agencia, conta,banco e dividas
        FD ARQ-DADOS-DO-BANCO.
       01 INF-DO-BANCO.
           
           88 FINAL-DADOS-BANC                          VALUE SPACE.
           05 AGN-BANC                                  PIC 9(004).
           05 CONTA-BANC                                PIC 9(010).
           05 NOME-BANC                                 PIC X(024).
           05 DIVI-CONT.
               10 VAL-DESCIMAL                          PIC 9(005).
               10 VAL-CENTAVOS                          PIC 9(002).
           
      * Finalizada a estrutura dos arquivo de leitura.
       WORKING-STORAGE SECTION.
           01 WS-NOME-COMPLETO.
               05 WS-PRIMEIRO-NOME             PIC X(020) VALUE SPACES
               05 WS-SOBRENOME             PIC X(020) VALUE SPACES.
               
       
       PROCEDURE DIVISION.
       STOP RUN.
       