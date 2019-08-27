Text
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

CREATE PROCEDURE DBO.SANPP_RF_CALC_TIR_DIA
   (
   @RF_CARACTERISTICA   CHAR(20),      -- OPERACAO A SER CALCULADA OU CODIGO DE CONTROLE
   @DT_CALC    DATETIME,      -- DATA EM QUE A TIR ESTA SENDO CALCULADA
   @TITULO_ID     INT,        -- IDENTIFICADOR DO TITULO
   @ID_RPAC    INT,        -- IDENTIFICADOR DO PERIODO DE REPACTUACAO OU ZERO PARA PADRONIZADOS
   @IC_OPRC_NAO_PADR CHAR(01),      -- INDICADOR DE OPERACAO NAO PADRONIZADA
   @TIT_EMISSAO      DATETIME,      -- EMISSAO DO TITULO
   @TIT_VENCTO    DATETIME,      -- VENCIMENTO DO TITULO
   @TIT_CUPOM     FLOAT,         -- CUPOM DO TITULO
   @RFX_FCALC     CHAR(03),      -- FORMA DE CALCULO DE JUROS
   @FCALC_CUPOM      CHAR(03),      -- FORMA DE CALCULO DO CUPOM
   @IDX_CODIGO    VARCHAR(15),      -- INDEXADOR DE CORRECAO
   @DT_AQUIS      DATETIME,      -- DATA DE AQUISICAO DO TITULO
   @DT_INIC_RPAC     DATETIME,      -- DATA INICIAL DA REPACTUACAO OU EMISSAO PARA PADRONIZADOS
   @DT_PROX_RPAC     DATETIME,      -- DATA DA PROXIMA REPACTUACAO
   @PU_AQUIS      FLOAT,         -- PU DE AQUISICAO DO TITULO
   @PMIO_AQUIS    FLOAT,         -- PREMIO DADO NA BOLETAGEM NPDR OU ZERO NOS DEMAIS CASOS
   @V_PU_PMIO     FLOAT,         -- PREMIO DADO NA REPACTUACAO NPDR OU ZERO NOS DEMAIS CASOS
   @RF_BASE_CALC     DATETIME,      -- DATA BASE DE CALCULO DO TITULO
   @IDX_PC        FLOAT,         -- PERCENTUAL DO INDEXADOR
   @FER_CHAVE     VARCHAR(15),      -- POSICAO PARA FERIADOS
   @CETIP_SELIC      CHAR(01),      -- 'C'ETIP PARA PRIVADOS OU 'S'ELIC PARA PUBLICOS
   @ATV_PRZ_PGJUROS  INT,        -- PRAZO DE PAGAMENTO DE JUROS PARA NTNs E NBC-Es
   @ATV_LOTE      FLOAT,         -- NOMINAL DO TITULO PARA NTNs E NBC-Es
   @TIR_U         FLOAT    OUTPUT,  -- RETORNA A TIR UTEIS
   @TIR_C         FLOAT    OUTPUT,  -- RETORNA A TIR CORRIDOS
   @ERRO       INT      OUTPUT,  -- RETORNA -1 => SEM ERROS
   @ERR_MSG    VARCHAR(100)   OUTPUT,  -- MENSAGEM DE ERRO
   @VB_SQL        INT   = NULL         ,  -- QUANDO NAO NULO, RETORNA RECORDSET PARA VB
   @FL_DESCAP     CHAR(1) = NULL          ,  -- INDICA SE VAI ACHAR O PU DESCAPITALIZANDO PELA BASE DA TAXA DO TITULO OU 252/360
   @FL_IDX_CONTABIL  CHAR(1)  = NULL    ,  -- INDICA QUE VAMOS UTILIZAR O FATOR DO INDEXADOR CORRIDOS E UTEIS PARA O CALCULO DO PU (PROJETO CRI/LH)
                     -- *** NA NEGOCIACAO DEVEMOS UTILIZAR SEMPRE A BASE DO TITULO (CORRIDOS OU UTEIS)
                     -- *** NA VALORIZACAO (CONTABIL) VAMOS UTILIZAR SEMPRE 360 PARA O SAFRABM E EMPRESAS DO GRUPO
   
   -- KUBA 10/01/2017 - QUANDO O PU MÉDIO DE AQUISIÇÃO É RECALCULADO UTILIZAR ESTES CAMPOS DE PU AO INVÉS DO PU DE AQUISIÇÃO
   @PU_MED_U      FLOAT  = NULL ,
   @PU_MED_C      FLOAT  = NULL 
   )
AS
BEGIN
   /*** RF0886.PRC ***/

   DECLARE
      @ID_U       INT,
      @ID_C       INT,
      @FLOAT0        FLOAT,
      @FLOAT1        FLOAT,
      @FLOAT2        FLOAT,
      @FLOAT100      FLOAT,
      @FLOAT252      FLOAT,
      @FLOAT360      FLOAT,
      @FLOAT365      FLOAT,
      @EVE_JURS_PCPL    CHAR(01),
      @EVE_JURS_AMTC    CHAR(01),
      @EVE_JURS_DTBS    CHAR(01),
      @EVE_CORC_PCPL    CHAR(01),
      @EVE_CORC_AMTC    CHAR(01),
      @EVE_AMTC_PCPL    CHAR(01),
      @EVE_CORRENTE     CHAR(01),
      @CD_CE         INT,

      @DT_EVE        DATETIME,
      @DT_EVE_LQDC      DATETIME,
      @DT_FNAL_EVE      DATETIME,
      @DT_FNAL_EVE_LQDC     DATETIME,
      @DT_BASE_CALC     DATETIME,
      @DT_B_JURS     DATETIME,
      @IDX_FCALC     CHAR(03),
      @CUPOM_TIR     FLOAT,
      @DT_VAR_TIR    DATETIME,
      @DT_INI        DATETIME,
      @DT_FIM        DATETIME,
      @DT_INI_ORI    DATETIME,
      @DT_FIM_ORI    DATETIME,
      @PU_PMIO_TIR      FLOAT,
      @CORRECAO_U    FLOAT,
      @CORRECAO_C    FLOAT,
      @ID_NUM_PARC      INT,
      @Q_DIAS_U_PARC    INT,
      @Q_DIAS_C_PARC    INT,
      @V_FUT_U_PARC     FLOAT,
      @V_FUT_C_PARC     FLOAT,
      @V_NOML_PCPL      FLOAT,
      @V_NOML_AMTC      FLOAT,
      @ID_SEQ_EVE_COR      INT,
      @ID_SEQ_EVE_JUR      INT,
      @EH_RESERVA    INT,
      @EH_FUNDO      CHAR(01),
      @BASE       FLOAT,
      @DT20000801    DATETIME,
      @IC_EVE_CORC_TIT  CHAR(01),
      @ID_RPAC_LOOP     INT,
      @DT_FIM_VAR_LOOP  DATETIME,
      @DT_INI_VAR_LOOP  DATETIME,
      @IDX_PC_LOOP      FLOAT,
      @CORRECAO_LOOP    FLOAT,
      @RFSC_VALOR_LOOP  FLOAT,
      @IC_COR_TIT_LOOP  CHAR(01),

      @TX_CENARIO    FLOAT,
      @CEN_DATA_BASE    DATETIME,
      @CEN_DATA      DATETIME,
      @SGL_B_EXPS    CHAR(03),

      @FL_ARRED      CHAR(01),   --INDICA SE UTILIZA ARREDONDAMENTO CETIP
      @CUPOM_TIR_CASAS  SMALLINT,   --VARIAVEIS PARA ARMAZENAR AS CASAS DECIMAIS E
      @CUPOM_TIR_ARRED  SMALLINT,   --SE ARREDONDA OU TRUNCA NO CALCULO CETIP.
      @VALORES_CASAS    SMALLINT,
      @VALORES_ARRED    SMALLINT,
      @CORRECAO_CASAS      SMALLINT,
      @CORRECAO_ARRED      SMALLINT,
      @DESCAP_CASAS     SMALLINT,
      @DESCAP_ARRED     SMALLINT,
      @VALOR_PRS_CASAS  SMALLINT,
      @VALOR_PRS_ARRED  SMALLINT,
      @PU_CASAS      SMALLINT,
      @PU_ARRED      SMALLINT,
      @ARREDONDA     SMALLINT,
      @TRUNCA        SMALLINT,
      @TIPO_ATU_CETIP      SMALLINT,   --INDICA O TIPO DE ATUALIZACAO CETIP (1,2 ou 3)
      @DT_ATU        DATETIME,   --AUXILIAR PARA ENCONTRAR DATA DE CALC ATUALIZACAO
      @UTILIZ_PREVIA    CHAR(01),   --INDICA QUE PARA DETERMINADO ATIVO UTILIZA PRÉVIA IGPM/IPCA
      @ATU_FLUXOS_FUT      CHAR(01),   --INDICA SE OS FLUXOS FUTUROS (POSTERIORES) AO ANO DE ATUALIZACAO 
                     --ATUAL SAO ATUALIZADOS COM O INDICE ATUAL
      @DECIMAIS_TAXA    INT,
      @TIPO_RESERVA     CHAR(03),   --INDICA FORMA DE CONTAGEM DAS RESERVAS (DIF. TIR 252/360)
      @TIPO_COTACAO_C      CHAR(01),   --CONTABIL DO BANCO PARA TR EH PRORATA DE DIAS CORRIDOS,
                     --ESTE FLAG INDICA ESTE TIPO DE CALCULO.
      @CGC_EMPRESA      FLOAT,
      @CGC_EMISSOR      FLOAT,
      @ATV_AP        CHAR(01),

      -- Liana - 22/06/2011
      @DT_ANIV_ANT      DATETIME,
      @DT_ANIV_PROX     DATETIME,
      @DT_PROX_PAGTO_EVE   DATETIME,
      @DT_CORRECAO_U    DATETIME,
      @DT_CORRECAO_C    DATETIME,
      @CUPOM_TIR_C      FLOAT,
      @DT_INI_C      DATETIME,
      @DT_FIM_C      DATETIME,
      @TIPO_ATU_CETIP_MENSAL     SMALLINT,
      @TIPO_ATU_CETIP_ANUAL_EMIS    SMALLINT,
      @TIPO_ATU_CETIP_ANUAL_VENCTO  SMALLINT,
      @PCH_PAGA_CORRECAO   CHAR(01),
      @DT_FIM_VAR_LOOP_C   DATETIME
      , @VCH_FC_DT_ANV        VARCHAR(3)
      , @VIT_Q_DEFS_IDXR      INT       
      , @VDT_DATA_INI            DATETIME
      , @VDT_DATA_FIM            DATETIME

   select
      @TIPO_ATU_CETIP_MENSAL     = 1,
      @TIPO_ATU_CETIP_ANUAL_EMIS    = 2,
      @TIPO_ATU_CETIP_ANUAL_VENCTO  = 3,
      @PCH_PAGA_CORRECAO = 'N'      


   -----------------------------------------------------
   -- PARA ESTOQUE DE ENCARTEIRAMENTO,  
   -- AS TAXAS DEVEM SER IGUAL AO DO ESTOQUE 
   ----------------------------------------------------

   SELECT @CGC_EMPRESA = B.PFPJ_CGC,
      @CGC_EMISSOR   = C.PFPJ_CGC,
      @ATV_AP = A.ATV_AP
   FROM RENDA_FIXA A, PF_PJ B, PF_PJ C 
   WHERE RF_CARACTERISTICA = @RF_CARACTERISTICA
   AND A.PFPJ_APELIDO_EMPRESA = B.PFPJ_APELIDO
   AND A.RF_AGENTE = C.PFPJ_APELIDO

-- SELECT @CGC_EMPRESA '@CGC_EMPRESA', @CGC_EMISSOR '@CGC_EMISSOR', @ATV_AP '@ATV_AP'

   IF @CGC_EMPRESA = @CGC_EMISSOR AND @ATV_AP = 'A'
   BEGIN
      SELECT @PU_AQUIS = @ATV_LOTE,
         @DT_AQUIS = @TIT_EMISSAO,
         @DT_CALC = @TIT_EMISSAO 
   END


   -- ASSOCIA OS VALORES DAS CONSTANTES

   SELECT
      @ID_U    = @@SPID,
      @ID_C    = -@@SPID,
      @FLOAT0     = CONVERT(FLOAT, 0),
      @FLOAT1     = CONVERT(FLOAT, 1),
      @FLOAT2     = CONVERT(FLOAT, 2),
      @FLOAT100  = CONVERT(FLOAT, 100),
      @FLOAT252   = CONVERT(FLOAT, 252),
      @FLOAT360   = CONVERT(FLOAT, 360),
      @FLOAT365   = CONVERT(FLOAT, 365),
      @EVE_JURS_AMTC = '1',   -- JUROS SOBRE AMORTIZACAO
      @EVE_JURS_PCPL = '2',   -- JUROS SOBRE PRINCIPAL
      @EVE_CORC_PCPL = '3',   -- ATUALIZACAO SOBRE PRINCIPAL
      @EVE_AMTC_PCPL = '4',   -- AMORTIZACAO DO PRINCIPAL
      @EVE_CORC_AMTC = '5',   -- ATUALIZACAO SOBRE AMORTIZACAO
      @EVE_JURS_DTBS  = '6',  -- JUROS SOBRE PRINCIPAL CORRIG. DESDE A DATABASE - DESENV PROJ. CRI-LH
      @CD_CE      = CONVERT(INT, @RF_CARACTERISTICA),
      @EH_FUNDO   = 'N',
      @ERRO    = -1,
      @ERR_MSG = '',
      @DT20000801 = CONVERT(DATETIME, '20000801'),
      @FL_ARRED   = 'N',
      @ARREDONDA  = 0,
      @TRUNCA     = 1,
      @TIPO_ATU_CETIP = 0,
      @UTILIZ_PREVIA  = 'N',
      @ATU_FLUXOS_FUT = 'N',
      @TIPO_RESERVA  = '001',
      @TIPO_COTACAO_C = 'C'

   -- BUSCA INFORMACOES PARA VALORIZACAO DE CRI-LH
   SELECT  @FL_ARRED     = B.IC_ARRD_CTP,    -- UTILIZA METODOLOGIA DO CADERNO CETIP
      @TIPO_ATU_CETIP  = A.ID_T_ATC_NOML,       -- TIPO DE ATUALIZACAO CETIP (1 = MENSAL/2 = ANUAL BASE EMISSAO/3 = ANUAL BASE VENC)
      @UTILIZ_PREVIA  = B.IC_UTLC_PREV_IDXR,    -- UTILIZA PREVIA INDEXADOR
      @ATU_FLUXOS_FUT  = B.IC_ATC_IDXR_FLUX_FUT, -- UTILIZA INDEXADOR ATUAL PARA ATUALIZAR OS CICLOS ANUAIS FUTUROS
      ----------------------------------------------------------------------------------------------------
      @CUPOM_TIR_CASAS = B.Q_DEC_FATR_JURS,     -- BUSCA PRECISAO DE CASAS E SE ARREDONDA OU TRUNCA:  
      @CUPOM_TIR_ARRED = CASE B.IC_TRUNC_DEC_FATR_JURS 
               WHEN 'S' THEN @TRUNCA
               ELSE @ARREDONDA END,
      @VALORES_CASAS     = B.Q_DEC_VLS,
      @VALORES_ARRED   = CASE B.IC_TRUNC_DEC_VLS
               WHEN 'S' THEN @TRUNCA
               ELSE @ARREDONDA END,
      @CORRECAO_CASAS  = B.Q_DEC_FATR_CORC,
      @CORRECAO_ARRED  = CASE B.IC_TRUNC_DEC_FATR_CORC
               WHEN 'S' THEN @TRUNCA
               ELSE @ARREDONDA END,
      @DESCAP_CASAS    = B.Q_DEC_FATR_DCAZC,
      @DESCAP_ARRED   = CASE B.IC_TRUNC_DEC_FATR_DCAZC
               WHEN 'S' THEN @TRUNCA
               ELSE @ARREDONDA END,
      @VALOR_PRS_CASAS = B.Q_DEC_V_PRST,
      @VALOR_PRS_ARRED = CASE B.IC_TRUNC_DEC_V_PRST
               WHEN 'S' THEN @TRUNCA
               ELSE @ARREDONDA END,
      @PU_CASAS    = B.Q_DEC_PU,
      @PU_ARRED    = CASE B.IC_TRUNC_DEC_PU
               WHEN 'S' THEN @TRUNCA
               ELSE @ARREDONDA END,
      @DECIMAIS_TAXA  = B.TAX_DECIMAIS
      ----------------------------------------------------------------------------------------------------
   FROM  RF_TITULO A,
      CODIGO_TITULO B
   WHERE    A.ATV_CODIGO = B.ATV_CODIGO
   AND   A.IDX_CODIGO = B.IDX_CODIGO
   AND   A.LOCAL_NEGOCIACAO = B.LOCAL_NEGOCIACAO
   AND   B.IC_ARRD_CTP = 'S'
   AND   TITULO_ID = @TITULO_ID
   AND   CD_TIT_ART IN (6000, 6500)

   -- Liana - 22/06/2011
   SELECT  @TIPO_ATU_CETIP  = ID_T_ATC_NOML     -- TIPO DE ATUALIZACAO CETIP (1 = MENSAL/2 = ANUAL BASE EMISSAO/3 = ANUAL BASE VENC)
   FROM RF_TITULO 
   WHERE TITULO_ID = @TITULO_ID

   -- PARA CRI E LH COM INDEXADOR TR UTILIZAMOS O TIPO 'X' (PRORATA CORRIDOS) PARA O CONTABIL 
   IF @FL_ARRED = 'S' AND @IDX_CODIGO = 'TR' AND @FL_IDX_CONTABIL = 'S'
      SELECT   @TIPO_COTACAO_C = 'X'

   -- VERIFICA SE A POSICAO EH UM FUNDO
   SELECT
      @EH_FUNDO   = 'S'
   FROM
      FDO_POSICAO_FUNDO
   WHERE
      POS_APELIDO = @FER_CHAVE


    SELECT @EH_FUNDO = 'S'

    IF NOT EXISTS (SELECT 1 FROM POSICAO WHERE POS_APELIDO = @FER_CHAVE)
    BEGIN
      	SELECT @FER_CHAVE = 'SAFRABM'
    END 



   -- BUSCA A FORMA DE CALCULO DO INDEXADOR

   SELECT
      @IDX_FCALC  = IDX_FCALC
   FROM
      INDEXADOR
   WHERE
      IDX_CODIGO  = @IDX_CODIGO

   -- Liana - 03/08/2010
   IF @CETIP_SELIC = 'C' AND @IDX_FCALC = '007'
   BEGIN
      SELECT   @UTILIZ_PREVIA = 'S'
   END

   -- 01.09.2005
   -- BUSCA A DATA BASE DE CALCULO PARA ATUALIZACAO DOS JUROS

   SELECT
      @DT_B_JURS = ISNULL(DT_B_JURS, @RF_BASE_CALC),
      @IC_EVE_CORC_TIT = ISNULL(IC_EVE_CORC_TIT, 'N')
    , @VCH_FC_DT_ANV        = ISNULL(FC_DT_ANV, '001')    -- SELECT * FROM FORMA_CALCULO WHERE CAL_TIPO = '57'
    , @VIT_Q_DEFS_IDXR      = ISNULL(Q_DEFS_IDXR, 0)
   FROM
      SANT643_RF_REPAC_TITULO
   WHERE
      TITULO_ID   =  @TITULO_ID  AND
      ID_RPAC     =  @ID_RPAC


   -- VARIACAO DO INDEXADOR POR DIAS UTEIS NA DATA DE AQUISICAO OU REPACTUACAO
   -- SE POSSUIR REPACTUACAO APOS A AQUISICAO, MAS NO MESMO MES, CONSIDERAR A REPACTUACAO

   IF @IC_OPRC_NAO_PADR <> 'S' OR
       @DT_INIC_RPAC < @DT_AQUIS

      SELECT
         @DT_VAR_TIR = @DT_AQUIS,
         @PU_PMIO_TIR   = @PMIO_AQUIS

   ELSE
      SELECT
         @DT_VAR_TIR = @DT_INIC_RPAC,
         @PU_PMIO_TIR   = @V_PU_PMIO

   -------------------------------------------------------------------------------------------
   -----------------------------------------------------------------------------------
   -- LIANA - 22/06/2011
   -- Se Tipo de Atualização igual a "ANUAL", calcula a Data de Atualização do Indexador,
   -- a Data do Último Aniversário e a Data do Próximo Aniversário do Papel.
   -----------------------------------------------------------------------------------
-- select @TIPO_ATU_CETIP '@TIPO_ATU_CETIP'

   IF @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS
   BEGIN
      SELECT @DT_CORRECAO_C = @DT_VAR_TIR

      -- CALCULA A DATA DO ÚLTIMO ANIVERSÁRIO
      EXEC SANPP_RF_CALC_ANIVERSARIO_ANTERIOR @TIPO_ATU_CETIP, @DT_CORRECAO_C, @TIT_VENCTO, @TIT_EMISSAO, '-1', @DT_ANIV_ANT OUTPUT
      
--    select @DT_ANIV_ANT '@DT_ANIV_ANT'

      IF NOT EXISTS(SELECT DT_EVE
            FROM SANT644_RF_AGENDA_EVENTO
            WHERE 
            TITULO_ID = @TITULO_ID
            and DT_EVE >= @DT_ANIV_ANT
            AND DT_EVE < @DT_VAR_TIR
            AND ID_RPAC =  @ID_RPAC
            and ID_T_EVE in (@EVE_CORC_PCPL, @EVE_CORC_AMTC)
--          AND V_PU_CALD > 0
            )
         AND @DT_ANIV_ANT <> @TIT_EMISSAO
      BEGIN

--       print 'vai mudar data atu'
         SELECT @DT_CORRECAO_C = @DT_ANIV_ANT

         -- chama funcao de calculo do ultimo aniv de novo
         EXEC SANPP_RF_CALC_ANIVERSARIO_ANTERIOR @TIPO_ATU_CETIP, @DT_CORRECAO_C, @TIT_VENCTO, @TIT_EMISSAO, '-1', @DT_ANIV_ANT OUTPUT
      END

      SELECT @DT_CORRECAO_U = @DT_CORRECAO_C

      EXEC @EH_RESERVA = SIAN_E_RESERVA   @DT_CORRECAO_U,
                     'A',
                     @FER_CHAVE,
                     0

      IF @EH_RESERVA <> -1 -- NAO EH DIA UTIL

         EXEC SIAN_SP_PROXIMA_RESERVA  'A',
                     @FER_CHAVE,
                     @DT_CORRECAO_U OUTPUT,
                     0

      -- CALCULA A DATA DO PRÓXIMO ANIVERSÁRIO
      EXEC SANPP_RF_CALC_PROXIMO_ANIVERSARIO @TIPO_ATU_CETIP, @DT_CORRECAO_C, @TIT_VENCTO, @TIT_EMISSAO, '-1', @DT_ANIV_PROX OUTPUT       

--    SELECT @DT_ANIV_ANT '@DT_ANIV_ANT', @DT_ANIV_PROX '@DT_ANIV_PROX', @DT_CORRECAO_C '@DT_CORRECAO_C', @DT_CORRECAO_U '@DT_CORRECAO_U'
   END

   -------------------------------------------------------------------------------------------

   -- CALCULA AS TAXAS UTEIS E CORRIDOS

   DELETE FROM SANT646_RF_TIR WHERE ID_USR IN (@ID_U, @ID_C)


   SELECT
      @DT_EVE     = @DT_VAR_TIR,
      @EVE_CORRENTE  = '',
      @ID_NUM_PARC   = @FLOAT0,
      @Q_DIAS_U_PARC = @FLOAT0,
      @Q_DIAS_C_PARC = @FLOAT0,
      @V_FUT_U_PARC  = @FLOAT0,
      @V_FUT_C_PARC  = @FLOAT0,
      @V_NOML_PCPL   = @FLOAT0,
      @V_NOML_AMTC   = @FLOAT0,
      @ID_SEQ_EVE_COR   = @FLOAT0,
      @ID_SEQ_EVE_JUR   = @FLOAT0



   -- OPERACOES NAO PADRONIZADAS

   IF @IC_OPRC_NAO_PADR = 'S'
   BEGIN
      -- FLUXO INICIAL D0 (DIAS UTEIS)

      INSERT INTO SANT646_RF_TIR
         (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
      VALUES
         (
         @ID_U,
         @CD_CE,
         @ID_NUM_PARC,
         @Q_DIAS_U_PARC,
          @PU_PMIO_TIR - ISNULL (@PU_MED_U, @PU_AQUIS),  -- OU REPACTUACAO / CORRECAO DO TITULO --KUBA - 10/01/2017
         @FLOAT0,
         @FLOAT0,
         @FLOAT0,
         @FLOAT0,
         @FLOAT0
         )

      -- FLUXO INICIAL D0 (DIAS CORRIDOS)

      INSERT INTO SANT646_RF_TIR
         (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
      VALUES
         (
         @ID_C,
         @CD_CE,
         @ID_NUM_PARC,
         @Q_DIAS_C_PARC,
         @PU_PMIO_TIR - ISNULL(@PU_MED_C,  @PU_AQUIS),  -- OU REPACTUACAO / CORRECAO DO TITULO --KUBA - 10/01/2017
         @FLOAT0,
         @FLOAT0,
         @FLOAT0,
         @FLOAT0,
         @FLOAT0
         )

      ---------------------------------------------------------------------------------
      -- VERIFICA SE O TITULO TEM EVENTOS APENAS NO VENCIMENTO           --
      -- UTILIZADO PARA TRATAR DIFERENCA NA TIR 252/360               --
      ---------------------------------------------------------------------------------
      IF NOT EXISTS (
         SELECT
            *
         FROM
            SANT644_RF_AGENDA_EVENTO
         WHERE
            TITULO_ID   = @TITULO_ID   AND
            DT_EVE      < @TIT_VENCTO  ) AND @FL_ARRED = 'S'
      begin
         select   @TIPO_RESERVA = '003'
      end

      ---------------------------------------------------------------------------------
      -- CONSISTE SE O TITULO TEM PELO MENOS UM FLUXO DE AMORTIZACAO PARA O CALCULO  --
      -- DA TIR NAO ENTRAR EM LOOPING.                    --
      ---------------------------------------------------------------------------------
      IF NOT EXISTS (
         SELECT
            *
         FROM
            SANT644_RF_AGENDA_EVENTO
         WHERE
            TITULO_ID   = @TITULO_ID   AND
            DT_EVE      >= @DT_EVE  AND
                V_PU_AMTC   <> 0 AND
            ID_T_EVE = @EVE_AMTC_PCPL)
      BEGIN
         SELECT  @ERRO = 1,
            @ERR_MSG = @RF_CARACTERISTICA + ': TITULO SEM AMORTIZACAO CADASTRADA, IMPOSSIVEL CALCULAR TIR.' + 
                  ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

         IF @VB_SQL IS NOT NULL
         BEGIN
            SELECT
               @TIR_U      TIR_U,
               @TIR_C      TIR_C,
               @ERRO    ERRO,
               @ERR_MSG ERR_MSG

            RETURN
         END
         ELSE
            RETURN
      END

      -- CALCULA O VALOR FUTURO DE TODOS OS FLUXOS

      WHILE EXISTS (
         SELECT
            *
         FROM
            SANT644_RF_AGENDA_EVENTO
         WHERE
            TITULO_ID   = @TITULO_ID   AND
            ID_RPAC     = @ID_RPAC  AND
            DT_EVE      > @DT_EVE   
         )
      BEGIN

         SELECT
            @EVE_CORRENTE  = '',
            @V_FUT_U_PARC  = @FLOAT0,
            @V_FUT_C_PARC  = @FLOAT0,
            @V_NOML_PCPL   = @FLOAT0,
            @V_NOML_AMTC   = @FLOAT0,
            @CUPOM_TIR  = @TIT_CUPOM,
            @ID_NUM_PARC   = @ID_NUM_PARC + 1


         -- BUSCA O NOMINAL DEVIDO

         SELECT
            @V_NOML_PCPL = SUM(V_PU_AMTC)
         FROM
            SANT644_RF_AGENDA_EVENTO
         WHERE
            TITULO_ID   = @TITULO_ID      AND
            ID_RPAC     = @ID_RPAC     AND
            ID_T_EVE = @EVE_AMTC_PCPL  AND
            DT_EVE      > @DT_EVE



         -- BUSCA A DATA DO PROXIMO EVENTO

         SELECT
            @DT_EVE = MIN(DT_EVE)
         FROM
            SANT644_RF_AGENDA_EVENTO
         WHERE
            TITULO_ID   = @TITULO_ID   AND
            ID_RPAC     = @ID_RPAC  AND
            DT_EVE      > @DT_EVE


         SELECT
            @DT_EVE_LQDC = CASE WHEN ISNULL(DT_LQDC_EVE, '1900-01-01') = '1900-01-01' THEN DT_EVE ELSE DT_LQDC_EVE END
         FROM
            SANT644_RF_AGENDA_EVENTO
         WHERE
            TITULO_ID   = @TITULO_ID   AND
            ID_RPAC     = @ID_RPAC  AND
            DT_EVE      = @DT_EVE



         -- SE O EVENTO VENCER APOS A DATA DE REPACTUACAO, CALCULAR ATE A DATA DE REPACTUACAO

         IF @DT_EVE > @DT_PROX_RPAC
            SELECT
               @DT_FNAL_EVE   = @DT_PROX_RPAC
              , @DT_FNAL_EVE_LQDC = @DT_PROX_RPAC
         ELSE
            SELECT
               @DT_FNAL_EVE   = @DT_EVE
             , @DT_FNAL_EVE_LQDC = @DT_EVE_LQDC

         -- CALCULA A QUANTIDADE DE DIAS ATE A DATA DO EVENTO
         EXEC @Q_DIAS_U_PARC = SIAN_SP_QUANTAS_RESERVAS  @DT_VAR_TIR,
                           @DT_FNAL_EVE_LQDC,
                           @TIPO_RESERVA,
                           'A',
                           @FER_CHAVE,
                           0

         IF (@FL_DESCAP = 'S') AND --ALTERACAO CRI 12/07/2006
            (@RFX_FCALC = '032' OR (@RFX_FCALC = '035' AND @TIT_EMISSAO > @DT20000801))   -- EXPONENCIAL 30 / 360
            EXEC SIAN_SP_TRINTA_DIAS   @DT_FNAL_EVE_LQDC,
                        @DT_VAR_TIR,
                        @Q_DIAS_C_PARC OUTPUT,
                        0,
                        'A'   -- PADRAO AMERICANO
         ELSE
            SELECT
               @Q_DIAS_C_PARC = DATEDIFF(day, @DT_VAR_TIR, @DT_FNAL_EVE_LQDC) 



         -- BUSCA O NOMINAL DE AMORTIZACAO NA DATA

         SELECT
            @V_NOML_AMTC = SUM(V_PU_AMTC)
         FROM
            SANT644_RF_AGENDA_EVENTO
         WHERE
            TITULO_ID   = @TITULO_ID      AND
            ID_RPAC     = @ID_RPAC     AND
            ID_T_EVE = @EVE_AMTC_PCPL  AND
            DT_EVE      = @DT_EVE



         -- FLUXOS DE JUROS

         IF EXISTS (
            SELECT
               *
            FROM
               SANT644_RF_AGENDA_EVENTO
            WHERE
               TITULO_ID   =  @TITULO_ID                 AND
               ID_RPAC     =  @ID_RPAC                AND
               ID_T_EVE IN (@EVE_JURS_AMTC, @EVE_JURS_PCPL,@EVE_JURS_DTBS) AND
               DT_EVE      =  @DT_EVE
            )
         BEGIN


            SELECT
               @ID_SEQ_EVE_JUR   = @ID_SEQ_EVE_JUR + 1



            -- BUSCA O EVENTO CORRENTE

            SELECT
               @EVE_CORRENTE  = ID_T_EVE
            FROM
               SANT644_RF_AGENDA_EVENTO
            WHERE
               TITULO_ID   =  @TITULO_ID                 AND
               ID_RPAC     =  @ID_RPAC                AND
               ID_T_EVE IN (@EVE_JURS_AMTC, @EVE_JURS_PCPL,@EVE_JURS_DTBS) AND
               DT_EVE      =  @DT_EVE



            SELECT
               @DT_FIM  = @DT_FNAL_EVE


            IF @IDX_FCALC <> '007' AND @RFX_FCALC = '022'
            BEGIN

                EXEC @EH_RESERVA = SIAN_E_RESERVA  @DT_FIM,
                           'A',
                           @FER_CHAVE,
                           0

                IF @EH_RESERVA <> -1   -- NAO EH DIA UTIL

               EXEC SIAN_SP_PROXIMA_RESERVA  'A',
                           @FER_CHAVE,
                           @DT_FIM     OUTPUT,
                           0
            END



            -- BUSCA A DATA INICIAL PARA O CALCULO DA VARIACAO DO CUPOM

            IF (@EVE_CORRENTE = @EVE_JURS_PCPL OR @EVE_CORRENTE = @EVE_JURS_DTBS) AND NOT (@IC_EVE_CORC_TIT = 'S' AND @ID_SEQ_EVE_JUR = 1)
            BEGIN

               -- PARA EVENTO DE JUROS SOBRE PRINCIPAL, CARECA O JUROS A CADA PAGAMENTO

               SELECT
                  @DT_INI  = MAX(DT_EVE)
               FROM
                  SANT644_RF_AGENDA_EVENTO
               WHERE
                  TITULO_ID   = @TITULO_ID      AND
                  ID_RPAC     = @ID_RPAC     AND
                  (ID_T_EVE   = @EVE_JURS_PCPL  OR
                   ID_T_EVE   = @EVE_JURS_DTBS) AND
                  DT_EVE      < @DT_EVE

                IF @DT_INI IS NULL
                   SELECT
                      @DT_INI = @DT_B_JURS
            END
            ELSE

               -- PARA EVENTO DE JUROS SOBRE AMORTIZACAO, O CALCULO COMECA NA DATA BASE DE CALCULO

               SELECT
                  @DT_INI  = @DT_B_JURS


            IF @DT_INI IS NULL
               SELECT
                  @DT_INI = @DT_INIC_RPAC

            EXEC @EH_RESERVA = SIAN_E_RESERVA   @DT_INI,
                           'A',
                           @FER_CHAVE,
                           0

            IF @EH_RESERVA <> -1 -- NAO EH DIA UTIL
               AND NOT (@IDX_FCALC = '007' AND @CETIP_SELIC = 'S') AND @RFX_FCALC = '022'

               EXEC SIAN_SP_PROXIMA_RESERVA  'A',
                           @FER_CHAVE,
                           @DT_INI     OUTPUT,
                           0



            -- VARIACAO DO CUPOM

            -- RF0887.PRC
            EXEC SANPP_RF_CALC_CUPOM_TIR  @TIT_EMISSAO,
                        @TIT_VENCTO,
                        @DT_INI,
                        @DT_FIM,
                        @FER_CHAVE,
                        @FCALC_CUPOM,
                        @TIT_CUPOM,
                        @RFX_FCALC,
                        @TIT_EMISSAO,
                        @ATV_PRZ_PGJUROS,
                        @CUPOM_TIR  OUTPUT,
                        @TIPO_RESERVA -- AJUSTE TIR 252/360

            -- LIANA - 22/06/2011 - VERIFICA SE O EVENTO PAGA CORRECAO
            EXEC SANPP_RF_VERIFICA_PAGTO_CORRECAO @TIPO_ATU_CETIP, @DT_EVE, @TITULO_ID, @ID_RPAC, @PCH_PAGA_CORRECAO OUTPUT            

--             select @TIPO_ATU_CETIP '@TIPO_ATU_CETIP', @DT_EVE '@DT_EVE', @PCH_PAGA_CORRECAO '@PCH_PAGA_CORRECAO', @FL_ARRED '@FL_ARRED'
--             select @EVE_CORRENTE '@EVE_CORRENTE', @ID_SEQ_EVE_COR '@ID_SEQ_EVE_COR'


            -- CALCULA A ATUALIZACAO SOBRE OS JUROS
            IF (@EVE_CORRENTE = @EVE_JURS_DTBS AND @IDX_CODIGO <> 'PRE') OR --JUROS SOBRE PRINCIPAL CORRIG. DESDE A DATABASE - DESENV PROJ. CRI-LH
                (@EVE_CORRENTE = @EVE_JURS_AMTC) OR
               (@EVE_CORRENTE = @EVE_JURS_PCPL 
               AND (((@ID_SEQ_EVE_COR < 1 and @TIPO_ATU_CETIP <> @TIPO_ATU_CETIP_ANUAL_EMIS) OR (@ID_SEQ_EVE_COR < 1 and @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS and @PCH_PAGA_CORRECAO = 'S')) -- Liana - 26/05/2011
                  OR (@IDX_CODIGO <> 'PRE' AND @EH_FUNDO = 'S'))) OR -- SE AINDA NAO PAGOU A CORRECAO OU EH CDI
--             (@EVE_CORRENTE = @EVE_JURS_PCPL AND @IDX_CODIGO <> 'PRE' AND @FL_ARRED = 'S')
               (@EVE_CORRENTE = @EVE_JURS_PCPL AND @IDX_CODIGO <> 'PRE' 
               -- Liana - 06/06/2011 - Caso exista Correção sobre amortização na mesma data, o juros sobre principal deve ser calculado
               AND EXISTS(SELECT DT_EVE FROM SANT644_RF_AGENDA_EVENTO
                     WHERE TITULO_ID   = @TITULO_ID      AND
                      ID_RPAC    = @ID_RPAC     AND
                      ID_T_EVE   = @EVE_CORC_AMTC  AND
                      DT_EVE     = @DT_EVE)
               )
            BEGIN

--             print 'entrou ATUALIZACAO SOBRE OS JUROS'

               SELECT
                  @DT_BASE_CALC = NULL,
                  -- Liana - 22/06/2011
                  @CORRECAO_U  = @FLOAT1,
                  @CORRECAO_C  = @FLOAT1

               -- SE POSSUIR BASE DE CALCULO SO UTILIZA ATE O PRIMEIRO PAGTO DE CORRECAO

               IF @EVE_CORRENTE = @EVE_JURS_PCPL
               BEGIN
                  -- Liana - 22/06/2011 -  se for atualizacao anual e correcao sobre principal , a data de início deve ser o último aniversário do papel
                  IF @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS
                  AND EXISTS(SELECT DT_EVE FROM SANT644_RF_AGENDA_EVENTO
                        WHERE TITULO_ID   = @TITULO_ID      AND
                         ID_RPAC    = @ID_RPAC     AND
                         ID_T_EVE   = @EVE_CORC_PCPL  AND
                         DT_EVE     = @DT_EVE)
                  BEGIN    
                     SELECT
                        @DT_BASE_CALC = @DT_ANIV_ANT     
                  END
                  ELSE
                  BEGIN
                     SELECT
                        @DT_BASE_CALC = MAX(DT_EVE)
                     FROM
                        SANT644_RF_AGENDA_EVENTO
                     WHERE
                        TITULO_ID   = @TITULO_ID      AND
                        ID_RPAC     = @ID_RPAC     AND
                        ID_T_EVE = @EVE_CORC_PCPL  AND
                        DT_EVE      < @DT_EVE
                  END
               end

--             select @DT_BASE_CALC '@DT_BASE_CALC'

               -- PARA EVENTO DE JUROS SOBRE AMORTIZACAO, CALCULA A VARIACAO DE 
               -- FORMA DECRESCENTE POR ID_RPAC. ISTO PORQUE A CADA REPACTUACAO PODE-SE
               -- MUDAR O PERCENTUAL DE INDEXADOR PARA CDI. SE OCORREU UMA CORRECAO DO 
               -- TITULO, CONSIDERAR O PERCENTUAL CORRIGIDO.

               IF @EVE_CORRENTE = @EVE_JURS_AMTC AND @IDX_FCALC = '000' AND @ID_RPAC > 1 AND @IC_EVE_CORC_TIT = 'N'
                  and (@TIPO_ATU_CETIP <> @TIPO_ATU_CETIP_ANUAL_EMIS OR @PCH_PAGA_CORRECAO = 'S') -- Liana - 22/06/2011
               BEGIN

                  SELECT
                     @ID_RPAC_LOOP   = @ID_RPAC,
                     @CORRECAO_U  = @FLOAT1,
                     @CORRECAO_C  = @FLOAT1,
                     @DT_FIM_VAR_LOOP = @DT_VAR_TIR,     -- DATA DE CALCULO
                     @DT_FIM_VAR_LOOP_C = @DT_VAR_TIR,   -- DATA DE CALCULO - Liana - 22/06/2011
                     @DT_INI_VAR_LOOP = @DT_INIC_RPAC,   -- DATA BASE DE CALCULO OU EMISSAO
                     @IDX_PC_LOOP     = @IDX_PC,      -- PERCENTUAL (CDI OU REF)
                     @IC_COR_TIT_LOOP = @IC_EVE_CORC_TIT -- EVENTO DE CORRECAO DO TITULO

                  -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE DEVE SER A DO ANIVERSARIO DO PAPEL
                  if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR < 1
                  BEGIN
                     SELECT @DT_FIM_VAR_LOOP = @DT_CORRECAO_U
                     SELECT @DT_FIM_VAR_LOOP_C = @DT_CORRECAO_C
--                   select @DT_FIM_VAR_LOOP_C '@DT_FIM_VAR_LOOP_C', @DT_FIM_VAR_LOOP '@DT_FIM_VAR_LOOP'
                  END


                  WHILE @ID_RPAC_LOOP > 0
                  BEGIN

                     -- 06.09.2005
                     -- SE FOR CDI E ESTIVER NO ID ATUAL, CALCULA ATRAVÉS DE PROJECOES

                     IF @IDX_CODIGO = 'CDI' AND @EH_FUNDO = 'S' AND @ID_RPAC_LOOP = @ID_RPAC
                     BEGIN


                        SELECT
                           @CORRECAO_LOOP  = @FLOAT0


                        -- RF2333.PRC

                        EXEC SANPP_RF_CALC_CDI_PROJETADO @TITULO_ID,    -- IDENTIFICADOR DO TITULO
                                       @ID_RPAC_LOOP,    -- IDENTIFICADOR DO PERIODO DE REPACTUACAO
                                       @EVE_CORRENTE,    -- TIPO DO EVENTO A SER UTILIZADO NA PROJECAO
                                       @DT_FIM_VAR_LOOP, -- DATA DE CALCULO DA PROJECAO
                                       @DT_EVE,    -- DATA DO EVENTO A PROJETAR O CDI
                                       @DT_INI_VAR_LOOP, -- DATA BASE DE CALCULO
                                       @DT_PROX_RPAC,    -- DATA DA PROXIMA REPACTUACAO OU VENCIMENTO
                                       @IDX_CODIGO,      -- INDEXADOR
                                       @IDX_PC_LOOP,     -- PERCENTUAL DO INDEXADOR
                                    @FER_CHAVE,    -- POSICAO PARA FERIADOS
                                       @CORRECAO_LOOP OUTPUT,  -- CDI PROJETADO
                                       @ERRO    OUTPUT,  -- RETORNA -1 => SEM ERROS
                                       @ERR_MSG OUTPUT,  -- MENSAGEM DE ERRO
                                       NULL        -- QUANDO NAO NULO, RETORNA RECORDSET PARA VB


                        IF @ERRO <> -1
                        BEGIN

                           IF @VB_SQL IS NOT NULL
                           BEGIN
                              SELECT
                                 @TIR_U      TIR_U,
                                 @TIR_C      TIR_C,
                                 @ERRO    ERRO,
                                 @ERR_MSG ERR_MSG

                              RETURN
                           END
                           ELSE
                              RETURN

                        END


                        -- ACUMULA A CORRECAO

                        SELECT
                           @CORRECAO_U = @CORRECAO_U * @CORRECAO_LOOP,
                           @CORRECAO_C = @CORRECAO_C * @CORRECAO_LOOP

                     END
                     ELSE
                     BEGIN


                        -- VARIACAO DO INDEXADOR POR DIAS UTEIS

                        SELECT
                           @CORRECAO_LOOP  = @FLOAT0,
                           @RFSC_VALOR_LOOP = @FLOAT0

                        EXEC SIAN_SP_VARIACAO
                              @IDX_CODIGO,         -- INDEXADOR
                              @DT_FIM_VAR_LOOP,    -- DATA DE CALCULO
                              @DT_INI_VAR_LOOP,    -- DATA BASE DE CALCULO OU EMISSAO
                              @TIT_VENCTO,         -- DATA DE VENCIMENTO
                              @IDX_PC_LOOP,        -- PERCENTUAL (CDI OU REF)
                              @FLOAT0,       -- 0.0
                              @FLOAT0,       -- 0.0
                              '000',            -- '000'
                              @FER_CHAVE,       -- POSICAO PARA CALCULO DE FERIADOS
                              @TIT_VENCTO,         -- ANIVERSARIO
                              @FLOAT0,       -- 0.0
                              @FLOAT1,       -- 1.0
                              @CORRECAO_LOOP OUTPUT,     -- VARIAVEL PARA RETORNAR A VARIACAO
                              @ERRO    OUTPUT,     -- RETORNA EVENTUAIS ERROS
                              'U',           -- 'U' -> UTEIS; 'C' -> CORRIDOS
                              0,          -- 0 PARA RETORAR NA VARIAVEL
                              @CETIP_SELIC,        -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                              @DT_FIM_VAR_LOOP,    -- DATA PARA IGPM PREVIO
                              @SGL_SISTEMA = 'RDF',      -- LIANA - 22/11/2010
                              @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                            , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                            , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                        IF @ERRO <> -1
                        BEGIN
                           SELECT
                              @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                    RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                           IF @VB_SQL IS NOT NULL
                           BEGIN
                              SELECT
                                 @TIR_U      TIR_U,
                                 @TIR_C      TIR_C,
                                 @ERRO    ERRO,
                                 @ERR_MSG ERR_MSG

                              RETURN
                           END
                           ELSE
                              RETURN

                        END


                        -- ACUMULA A CORRECAO

                        SELECT
                           @CORRECAO_U = @CORRECAO_U * @CORRECAO_LOOP


                        -- VARIACAO DO INDEXADOR POR DIAS CORRIDOS

                        SELECT
                           @CORRECAO_LOOP  = @FLOAT0,
                           @RFSC_VALOR_LOOP = @FLOAT0

                        EXEC SIAN_SP_VARIACAO
                              @IDX_CODIGO,         -- INDEXADOR
                              @DT_FIM_VAR_LOOP_C,     -- DATA DE CALCULO -- Liana - 22/06/2011
                              @DT_INI_VAR_LOOP,    -- DATA BASE DE CALCULO OU EMISSAO
                              @TIT_VENCTO,         -- DATA DE VENCIMENTO
                              @IDX_PC_LOOP,        -- PERCENTUAL (CDI OU REF)
                              @FLOAT0,       -- 0.0
                              @FLOAT0,       -- 0.0
                              '000',            -- '000'
                              @FER_CHAVE,       -- POSICAO PARA CALCULO DE FERIADOS
                              @TIT_VENCTO,         -- ANIVERSARIO
                              @FLOAT0,       -- 0.0
                              @FLOAT1,       -- 1.0
                              @CORRECAO_LOOP OUTPUT,     -- VARIAVEL PARA RETORNAR A VARIACAO
                              @ERRO    OUTPUT,     -- RETORNA EVENTUAIS ERROS
                              'C',           -- 'U' -> UTEIS; 'C' -> CORRIDOS
                              0,          -- 0 PARA RETORAR NA VARIAVEL
                              @CETIP_SELIC,        -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                              @DT_FIM_VAR_LOOP_C,     -- DATA PARA IGPM PREVIO
                              @SGL_SISTEMA = 'RDF',      -- LIANA - 22/11/2010
                              @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                            , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                            , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                        IF @ERRO <> -1
                        BEGIN
                           SELECT
                              @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                    RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                           IF @VB_SQL IS NOT NULL
                           BEGIN
                              SELECT
                                 @TIR_U      TIR_U,
                                 @TIR_C      TIR_C,
                                 @ERRO    ERRO,
                                 @ERR_MSG ERR_MSG

                              RETURN
                           END
                           ELSE
                              RETURN

                        END

                        -- ACUMULA A CORRECAO

                        SELECT
                           @CORRECAO_C = @CORRECAO_C * @CORRECAO_LOOP

                     END


                     -- BUSCA O PERIODO ANTERIOR

                     SELECT
                        @ID_RPAC_LOOP   = @ID_RPAC_LOOP - 1

                     SELECT
                        @DT_FIM_VAR_LOOP = @DT_INI_VAR_LOOP,   -- DATA DE CALCULO
                        @DT_FIM_VAR_LOOP_C = @DT_INI_VAR_LOOP, -- Liana - 22/06/2011
                        @DT_INI_VAR_LOOP = DT_INIC_RPAC, -- DATA BASE DE CALCULO OU EMISSAO
                        @IDX_PC_LOOP     = CASE WHEN @IC_EVE_CORC_TIT = 'S' THEN @IDX_PC_LOOP
                                 ELSE IDX_PC
                                 END,        -- SE FOR UM EVENTO DE CORRECAO, UTILIZAR O PERCENTUAL DE CORRECAO
                        @IC_COR_TIT_LOOP = IC_EVE_CORC_TIT  -- EVENTO DE CORRECAO DO TITULO
                     FROM
                        SANT643_RF_REPAC_TITULO
                     WHERE
                        TITULO_ID   = @TITULO_ID      AND
                        ID_RPAC     = @ID_RPAC_LOOP

                     -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE DEVE SER A DO ANIVERSARIO DO PAPEL
                     if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR < 1
                     BEGIN
                        IF @DT_FIM_VAR_LOOP_C > @DT_CORRECAO_C
                        BEGIN
                           SELECT @DT_FIM_VAR_LOOP = @DT_CORRECAO_U
                           SELECT @DT_FIM_VAR_LOOP_C = @DT_CORRECAO_C
--                         select @DT_FIM_VAR_LOOP_C '@DT_FIM_VAR_LOOP_C', @DT_FIM_VAR_LOOP '@DT_FIM_VAR_LOOP'
                        END
                     END



                  END
               END
               -- Liana - 22/06/2011
               ELSE IF (@TIPO_ATU_CETIP <> @TIPO_ATU_CETIP_ANUAL_EMIS OR @PCH_PAGA_CORRECAO = 'S') -- JUROS SOBRE O PRINCIPAL
               BEGIN

                  IF @DT_BASE_CALC IS NULL

                     SELECT
                        @DT_BASE_CALC = @DT_B_JURS

--                select @DT_BASE_CALC '@DT_BASE_CALC - 2'

                  -- SE FOR INDEXADO PELO CDI, UTILIZAR O PROJETADO

                  IF @IDX_CODIGO = 'CDI' AND @EH_FUNDO = 'S'
                  BEGIN

                     -- RF2333.PRC

                     EXEC SANPP_RF_CALC_CDI_PROJETADO @TITULO_ID,    -- IDENTIFICADOR DO TITULO
                                    @ID_RPAC,      -- IDENTIFICADOR DO PERIODO DE REPACTUACAO
                                    @EVE_CORRENTE,    -- TIPO DO EVENTO A SER UTILIZADO NA PROJECAO
                                    @DT_VAR_TIR,      -- DATA DE CALCULO DA PROJECAO
                                    @DT_EVE,    -- DATA DO EVENTO A PROJETAR O CDI
                                    @DT_BASE_CALC,    -- DATA BASE DE CALCULO
                                    @DT_PROX_RPAC,    -- DATA DA PROXIMA REPACTUACAO OU VENCIMENTO
                                    @IDX_CODIGO,      -- INDEXADOR
                                    @IDX_PC,    -- PERCENTUAL DO INDEXADOR
                                    @FER_CHAVE,    -- POSICAO PARA FERIADOS
                                    @CORRECAO_U OUTPUT,  -- CDI PROJETADO
                                    @ERRO    OUTPUT,  -- RETORNA -1 => SEM ERROS
                                    @ERR_MSG OUTPUT,  -- MENSAGEM DE ERRO
                                    NULL        -- QUANDO NAO NULO, RETORNA RECORDSET PARA VB


                     IF @ERRO <> -1
                     BEGIN

                        IF @VB_SQL IS NOT NULL
                        BEGIN
                           SELECT
                              @TIR_U      TIR_U,
                              @TIR_C      TIR_C,
                              @ERRO    ERRO,
                              @ERR_MSG ERR_MSG

                           RETURN
                        END
                        ELSE
                           RETURN

                     END

                     SELECT 
                        @CORRECAO_C = @CORRECAO_U

                  END
                  ELSE
                  BEGIN

                     SELECT  @DT_ATU      = @DT_VAR_TIR

                     -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO DO LOOP, A DATA LIMITE DEVE SER O ANIVERSARIO DO PAPEL
                     if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR < 1
                     BEGIN
                        SELECT @DT_ATU = @DT_CORRECAO_U
--                      select @DT_ATU '@DT_ATU'
                     END


--                      IF (@FL_ARRED = 'S' and @TIPO_ATU_CETIP > 0)
--                         EXEC SANPP_RF_CALC_DT_ATUAL_CORRECAO
--                            @TIPO_ATU_CETIP,
--                            @DT_EVE,
--                            @DT_VAR_TIR,
--                            @TIT_VENCTO,
--                            @DT_BASE_CALC,
--                            @DT_ATU OUTPUT,
--                            @ATU_FLUXOS_FUT

                     -- VARIACAO DO INDEXADOR POR DIAS UTEIS

                    IF @VCH_FC_DT_ANV = '002'
                    BEGIN
                        SELECT @VDT_DATA_INI = @DT_BASE_CALC
                        SELECT @VDT_DATA_FIM = (  SELECT MIN(DT_EVE) 
                                                  FROM SANT644_RF_AGENDA_EVENTO 
                                                  WHERE TITULO_ID =  @TITULO_ID
                                                    AND ID_RPAC   =  @ID_RPAC
                                                    AND DT_EVE    >= @DT_ATU
                                               )
                    END
                    ELSE
                    BEGIN
                        SELECT @VDT_DATA_INI = @DT_BASE_CALC
                        SELECT @VDT_DATA_FIM = @TIT_VENCTO
                    END

                     EXEC SIAN_SP_VARIACAO
                           @IDX_CODIGO,         -- INDEXADOR
                           @DT_ATU,       -- DATA DE CALCULO
                           @VDT_DATA_INI,       -- DATA BASE DE CALCULO OU EMISSAO
                           @VDT_DATA_FIM,         -- DATA DE VENCIMENTO
                           @IDX_PC,       -- PERCENTUAL (CDI OU REF)
                           @FLOAT0,       -- 0.0
                           @FLOAT0,       -- 0.0
                           '000',            -- '000'
                           @FER_CHAVE,       -- POSICAO PARA CALCULO DE FERIADOS
                           @TIT_VENCTO,         -- ANIVERSARIO
                           @FLOAT0,       -- 0.0
                           @FLOAT1,       -- 1.0
                           @CORRECAO_U OUTPUT,     -- VARIAVEL PARA RETORNAR A VARIACAO
                           @ERRO    OUTPUT,     -- RETORNA EVENTUAIS ERROS
                           'U',           -- 'U' -> UTEIS; 'C' -> CORRIDOS
                           0,          -- 0 PARA RETORAR NA VARIAVEL
                           @CETIP_SELIC,        -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                           @DT_CALC,         -- DATA PARA IGPM PREVIO
                           NULL,
                           '001',
                           @UTILIZ_PREVIA,         -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                           @SGL_SISTEMA = 'RDF',      -- LIANA - 22/11/2010
                           @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011                         
                            , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                            , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR


                     IF @ERRO <> -1
                     BEGIN
                        SELECT
                           @ERR_MSG = @RF_CARACTERISTICA + ': TIR - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                 RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                        IF @VB_SQL IS NOT NULL
                        BEGIN
                           SELECT
                              @TIR_U      TIR_U,
                              @TIR_C      TIR_C,
                              @ERRO    ERRO,
                              @ERR_MSG ERR_MSG

                           RETURN
                        END
                        ELSE
                           RETURN

                     END


                     SELECT  @DT_ATU      = @DT_VAR_TIR

                     -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO DO LOOP, A DATA LIMITE DEVE SER O ANIVERSARIO DO PAPEL
                     if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR < 1
                     BEGIN
                        SELECT @DT_ATU = @DT_CORRECAO_C
                     END


--                      IF (@FL_ARRED = 'S' and @TIPO_ATU_CETIP > 0)
--                         EXEC SANPP_RF_CALC_DT_ATUAL_CORRECAO
--                            @TIPO_ATU_CETIP,
--                            @DT_EVE,
--                            @DT_VAR_TIR,
--                            @TIT_VENCTO,
--                            @DT_BASE_CALC,
--                            @DT_ATU OUTPUT,
--                            @ATU_FLUXOS_FUT

                     -- VARIACAO DO INDEXADOR POR DIAS CORRIDOS

                    IF @VCH_FC_DT_ANV = '002'
                    BEGIN
                        SELECT @VDT_DATA_INI = @DT_BASE_CALC
                        SELECT @VDT_DATA_FIM = (  SELECT MIN(DT_EVE) 
                                                  FROM SANT644_RF_AGENDA_EVENTO 
                                                  WHERE TITULO_ID =  @TITULO_ID
                                                    AND ID_RPAC   =  @ID_RPAC
                                                    AND DT_EVE    >= @DT_ATU
                                               )
                    END
                    ELSE
                    BEGIN
                        SELECT @VDT_DATA_INI = @DT_BASE_CALC
                        SELECT @VDT_DATA_FIM = @TIT_VENCTO
                    END

                     EXEC SIAN_SP_VARIACAO
                           @IDX_CODIGO,         -- INDEXADOR
                           @DT_ATU,       -- DATA DE CALCULO
                           @VDT_DATA_INI,       -- DATA BASE DE CALCULO OU EMISSAO
                           @VDT_DATA_FIM,         -- DATA DE VENCIMENTO
                           @IDX_PC,       -- PERCENTUAL (CDI OU REF)
                           @FLOAT0,       -- 0.0
                           @FLOAT0,       -- 0.0
                           '000',            -- '000'
                           @FER_CHAVE,       -- POSICAO PARA CALCULO DE FERIADOS
                           @TIT_VENCTO,         -- ANIVERSARIO
                           @FLOAT0,       -- 0.0
                           @FLOAT1,       -- 1.0
                           @CORRECAO_C OUTPUT,     -- VARIAVEL PARA RETORNAR A VARIACAO
                           @ERRO    OUTPUT,     -- RETORNA EVENTUAIS ERROS
                           @TIPO_COTACAO_C,     -- 'U' -> UTEIS; 'C' -> CORRIDOS
                           0,          -- 0 PARA RETORAR NA VARIAVEL
                           @CETIP_SELIC,        -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                           @DT_CALC,         -- DATA PARA IGPM PREVIO
                           NULL,
                           '001',
                           @UTILIZ_PREVIA,         -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                           @SGL_SISTEMA = 'RDF',      -- LIANA - 22/11/2010
                           @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                            , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                            , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                     IF @ERRO <> -1
                     BEGIN
                        SELECT
                           @ERR_MSG = @RF_CARACTERISTICA + ': TIR - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                 RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                        IF @VB_SQL IS NOT NULL
                        BEGIN
                           SELECT
                              @TIR_U      TIR_U,
                              @TIR_C      TIR_C,
                              @ERRO    ERRO,
                              @ERR_MSG ERR_MSG

                           RETURN
                        END
                        ELSE
                           RETURN
                     END
                  END
               END

               -- CASO O TITULO UTILIZE ARREDONDAMENTO CETIP
               IF @FL_ARRED = 'S'
               BEGIN
                  IF @FL_IDX_CONTABIL IS NULL OR @FL_IDX_CONTABIL = 'N'
                  BEGIN
                     -- CORRECAO DEVE SER IGUAL PARA A MONTAGEM DOS FLUXOS 252/360 DOS TITULOS NA NEGOCIACAO
                     IF @IDX_CODIGO = 'TR' OR @IDX_CODIGO = 'CDI'
                        SELECT @CORRECAO_C = @CORRECAO_U
                     ELSE -- IGPM/IPCA
                        SELECT @CORRECAO_U = @CORRECAO_C
                  END

                  SELECT   @CUPOM_TIR = ROUND(@CUPOM_TIR,@CUPOM_TIR_CASAS,@CUPOM_TIR_ARRED),
                     @V_NOML_PCPL = ROUND(@V_NOML_PCPL,@VALORES_CASAS,@VALORES_ARRED),
                     @CORRECAO_C = ROUND(@CORRECAO_C,@CORRECAO_CASAS,@CORRECAO_ARRED),
                     @CORRECAO_U = ROUND(@CORRECAO_U,@CORRECAO_CASAS,@CORRECAO_ARRED)
               END

               -- SE FOR UM EVENTO DE JUROS SOBRE AMORTIZACAO APLICA A VARIACAO SOBRE A AMORTIZACAO

               IF @EVE_CORRENTE = @EVE_JURS_AMTC

                  SELECT
                     @V_FUT_U_PARC = @V_FUT_U_PARC + (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_AMTC * @CORRECAO_U ),
                     @V_FUT_C_PARC = @V_FUT_C_PARC + (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_AMTC * @CORRECAO_C )
               ELSE

                  SELECT
                     @V_FUT_U_PARC = @V_FUT_U_PARC + (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL * @CORRECAO_U ),
                     @V_FUT_C_PARC = @V_FUT_C_PARC + (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL * @CORRECAO_C )

            END
            ELSE
            BEGIN
               -- CASO O TITULO UTILIZE ARREDONDAMENTO CETIP
               IF @FL_ARRED = 'S'
                  SELECT   @CUPOM_TIR = ROUND(@CUPOM_TIR,@CUPOM_TIR_CASAS,@CUPOM_TIR_ARRED),
                     @V_NOML_PCPL = ROUND(@V_NOML_PCPL,@VALORES_CASAS,@VALORES_ARRED)


               SELECT
                  @V_FUT_U_PARC = @V_FUT_U_PARC + (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL ),
                  @V_FUT_C_PARC = @V_FUT_C_PARC + (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL )
            END
         END



         -- FLUXOS DE ATUALIZACAO

         IF EXISTS (
            SELECT
               *
            FROM
               SANT644_RF_AGENDA_EVENTO
            WHERE
               TITULO_ID   =  @TITULO_ID           AND
               ID_RPAC     =  @ID_RPAC          AND
               ID_T_EVE IN (@EVE_CORC_AMTC, @EVE_CORC_PCPL) AND
               DT_EVE      =  @DT_EVE
            )
         BEGIN

            SELECT
               @ID_SEQ_EVE_COR   = @ID_SEQ_EVE_COR + 1



            -- BUSCA O EVENTO CORRENTE

            SELECT
               @EVE_CORRENTE  = ID_T_EVE
            FROM
               SANT644_RF_AGENDA_EVENTO
            WHERE
               TITULO_ID   =  @TITULO_ID           AND
               ID_RPAC     =  @ID_RPAC          AND
               ID_T_EVE IN (@EVE_CORC_AMTC, @EVE_CORC_PCPL) AND
               DT_EVE      =  @DT_EVE



            -- CALCULA O VALOR DO FLUXO PARA ATUALIZACAO SOBRE AMORTIZACAO OU PARA O
            -- PRIMEIRO FLUXO DE ATUALIZACAO SOBRE O PRINCIPAL CASO NAO SEJA CDI
            -- SE FOR CDI UTILIZA O PROJETADO

            -- Liana - 22/06/2011 - Para Atualização Anual, deve calcular a correção para o próximo evento anual
            IF (@EVE_CORRENTE = @EVE_CORC_AMTC  AND (@TIPO_ATU_CETIP <> @TIPO_ATU_CETIP_ANUAL_EMIS OR @PCH_PAGA_CORRECAO = 'S'))
               OR ((@ID_SEQ_EVE_COR = 1  and @TIPO_ATU_CETIP <> @TIPO_ATU_CETIP_ANUAL_EMIS) OR (@ID_SEQ_EVE_COR = 1 AND @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS and @PCH_PAGA_CORRECAO = 'S')) 
               OR (@IDX_CODIGO = 'CDI' AND @EH_FUNDO = 'S')

            BEGIN

--             print 'calcula a correcao'

               -- PARA ATUALIZACAO SOBRE AMORTIZACAO, SEMPRE UTILIZA A DATA BASE DE CALCULO

               IF @EVE_CORRENTE = @EVE_CORC_AMTC
               BEGIN

                  SELECT
                     @DT_BASE_CALC = @RF_BASE_CALC

               END
               ELSE
               BEGIN

                  -- SE POSSUIR BASE DE CALCULO SO UTILIZA NO PRIMEIRO PAGAMENTO

                  IF @DT_EVE = ( SELECT
                           MIN(DT_EVE)
                        FROM
                           SANT644_RF_AGENDA_EVENTO
                        WHERE
                           TITULO_ID   = @TITULO_ID      AND
                           ID_RPAC     = @ID_RPAC     AND
                           ID_T_EVE = @EVE_CORC_PCPL )
                  BEGIN

                     SELECT
                        @DT_BASE_CALC = @RF_BASE_CALC

                  END
                  ELSE
                  BEGIN

                     SELECT
                        @DT_BASE_CALC  = MAX(DT_EVE)
                     FROM
                        SANT644_RF_AGENDA_EVENTO
                     WHERE
                        TITULO_ID   = @TITULO_ID      AND
                        ID_RPAC     = @ID_RPAC     AND
                        ID_T_EVE = @EVE_CORC_PCPL  AND
                        DT_EVE      < @DT_EVE

                     IF @DT_BASE_CALC IS NULL
                        SELECT
                           @DT_BASE_CALC = @DT_INIC_RPAC

                  END

                  -- Liana - 22/06/2011
                  if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS
                     select @DT_BASE_CALC = @DT_ANIV_ANT

               END



               -- PARA EVENTO DE JUROS SOBRE AMORTIZACAO, CALCULA A VARIACAO DE 
               -- FORMA DECRESCENTE POR ID_RPAC. ISTO PORQUE A CADA REPACTUACAO PODE-SE
               -- MUDAR O PERCENTUAL DE INDEXADOR PARA CDI. SE OCORREU UMA CORRECAO DO 
               -- TITULO, CONSIDERAR O PERCENTUAL CORRIGIDO.

               IF @EVE_CORRENTE = @EVE_CORC_AMTC AND @IDX_FCALC = '000' AND @ID_RPAC > 1 AND @IC_EVE_CORC_TIT = 'N'
               BEGIN

                  SELECT
                     @ID_RPAC_LOOP   = @ID_RPAC,
                     @CORRECAO_U  = @FLOAT1,
                     @CORRECAO_C  = @FLOAT1,
                     @DT_FIM_VAR_LOOP = @DT_VAR_TIR,     -- DATA DE CALCULO
                     @DT_FIM_VAR_LOOP_C = @DT_VAR_TIR,   -- DATA DE CALCULO -- Liana - 22/06/2011
                     @DT_INI_VAR_LOOP = @DT_INIC_RPAC,   -- DATA BASE DE CALCULO OU EMISSAO
                     @IDX_PC_LOOP     = @IDX_PC,      -- PERCENTUAL (CDI OU REF)
                     @IC_COR_TIT_LOOP = @IC_EVE_CORC_TIT -- EVENTO DE CORRECAO DO TITULO

                  -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE EH A DATA DO ANIVERSARIO DO PAPEL
                  if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR = 1
                  BEGIN
                     SELECT @DT_FIM_VAR_LOOP = @DT_CORRECAO_U
                     SELECT @DT_FIM_VAR_LOOP_C = @DT_CORRECAO_C
--                   select @DT_FIM_VAR_LOOP_C '@DT_FIM_VAR_LOOP_C', @DT_FIM_VAR_LOOP '@DT_FIM_VAR_LOOP'
                  END

                  WHILE @ID_RPAC_LOOP > 0
                  BEGIN



                     -- 06.09.2005
                     -- SE FOR CDI E ESTIVER NO ID ATUAL, CALCULA ATRAVÉS DE PROJECOES

                     IF @IDX_CODIGO = 'CDI' AND @EH_FUNDO = 'S' AND @ID_RPAC_LOOP = @ID_RPAC
                     BEGIN


                        SELECT
                           @CORRECAO_LOOP  = @FLOAT0


                        -- RF2333.PRC

                        EXEC SANPP_RF_CALC_CDI_PROJETADO @TITULO_ID,    -- IDENTIFICADOR DO TITULO
                                       @ID_RPAC_LOOP,    -- IDENTIFICADOR DO PERIODO DE REPACTUACAO
                                       @EVE_CORRENTE,    -- TIPO DO EVENTO A SER UTILIZADO NA PROJECAO
                                       @DT_FIM_VAR_LOOP, -- DATA DE CALCULO DA PROJECAO
                                       @DT_EVE,    -- DATA DO EVENTO A PROJETAR O CDI
                                       @DT_INI_VAR_LOOP, -- DATA BASE DE CALCULO
                                       @DT_PROX_RPAC,    -- DATA DA PROXIMA REPACTUACAO OU VENCIMENTO
                                       @IDX_CODIGO,      -- INDEXADOR
                                       @IDX_PC_LOOP,     -- PERCENTUAL DO INDEXADOR
                                       @FER_CHAVE,    -- POSICAO PARA FERIADOS
                                       @CORRECAO_LOOP OUTPUT,  -- CDI PROJETADO
                                       @ERRO    OUTPUT,  -- RETORNA -1 => SEM ERROS
                                       @ERR_MSG OUTPUT,  -- MENSAGEM DE ERRO
                                       NULL        -- QUANDO NAO NULO, RETORNA RECORDSET PARA VB


                        IF @ERRO <> -1
                        BEGIN

                           IF @VB_SQL IS NOT NULL
                           BEGIN
                              SELECT
                                 @TIR_U      TIR_U,
                                 @TIR_C      TIR_C,
                                 @ERRO    ERRO,
                                 @ERR_MSG ERR_MSG

                              RETURN
                           END
                           ELSE
                              RETURN

                        END


                        -- ACUMULA A CORRECAO

                        SELECT
                           @CORRECAO_U = @CORRECAO_U * @CORRECAO_LOOP,
                           @CORRECAO_C = @CORRECAO_C * @CORRECAO_LOOP

                     END
                     ELSE
                     BEGIN


                        -- VARIACAO DO INDEXADOR POR DIAS UTEIS

                        SELECT
                           @CORRECAO_LOOP  = @FLOAT0,
                           @RFSC_VALOR_LOOP = @FLOAT0

                        EXEC SIAN_SP_VARIACAO
                              @IDX_CODIGO,         -- INDEXADOR
                              @DT_FIM_VAR_LOOP,    -- DATA DE CALCULO
                              @DT_INI_VAR_LOOP,    -- DATA BASE DE CALCULO OU EMISSAO
                              @TIT_VENCTO,         -- DATA DE VENCIMENTO
                              @IDX_PC_LOOP,        -- PERCENTUAL (CDI OU REF)
                              @FLOAT0,       -- 0.0
                              @FLOAT0,       -- 0.0
                              '000',            -- '000'
                              @FER_CHAVE,       -- POSICAO PARA CALCULO DE FERIADOS
                              @TIT_VENCTO,         -- ANIVERSARIO
                              @RFSC_VALOR_LOOP OUTPUT,   -- RETORNA A ULTIMA COTACAO
                              @FLOAT1,       -- 1.0
                              @CORRECAO_LOOP OUTPUT,     -- VARIAVEL PARA RETORNAR A VARIACAO
                              @ERRO    OUTPUT,     -- RETORNA EVENTUAIS ERROS
                              'U',           -- 'U' -> UTEIS; 'C' -> CORRIDOS
                              0,          -- 0 PARA RETORAR NA VARIAVEL
                              @CETIP_SELIC,        -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                              @DT_FIM_VAR_LOOP,    -- DATA PARA IGPM PREVIO
                              @SGL_SISTEMA = 'RDF',      -- LIANA - 22/11/2010
                              @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                            , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                            , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                        IF @ERRO <> -1
                        BEGIN
                           SELECT
                              @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                    RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                           IF @VB_SQL IS NOT NULL
                           BEGIN
  SELECT
                                 @TIR_U      TIR_U,
                                 @TIR_C      TIR_C,
                                 @ERRO    ERRO,
                                 @ERR_MSG ERR_MSG

                              RETURN
                           END
                           ELSE
                              RETURN

                        END

                        -- ACUMULA A CORRECAO

                        SELECT
                           @CORRECAO_U = @CORRECAO_U * @CORRECAO_LOOP



                        -- VARIACAO DO INDEXADOR POR DIAS CORRIDOS

                        SELECT
                           @CORRECAO_LOOP  = @FLOAT0,
                           @RFSC_VALOR_LOOP = @FLOAT0

                        EXEC SIAN_SP_VARIACAO
                              @IDX_CODIGO,         -- INDEXADOR
                              @DT_FIM_VAR_LOOP_C,     -- DATA DE CALCULO -- Liana - 22/06/2011
                              @DT_INI_VAR_LOOP,    -- DATA BASE DE CALCULO OU EMISSAO
                              @TIT_VENCTO,         -- DATA DE VENCIMENTO
                              @IDX_PC_LOOP,        -- PERCENTUAL (CDI OU REF)
                              @FLOAT0,       -- 0.0
                              @FLOAT0,       -- 0.0
                              '000',            -- '000'
                              @FER_CHAVE,       -- POSICAO PARA CALCULO DE FERIADOS
                              @TIT_VENCTO,         -- ANIVERSARIO
                              @RFSC_VALOR_LOOP OUTPUT,   -- RETORNA A ULTIMA COTACAO
                              @FLOAT1,       -- 1.0
                              @CORRECAO_LOOP OUTPUT,     -- VARIAVEL PARA RETORNAR A VARIACAO
                              @ERRO    OUTPUT,     -- RETORNA EVENTUAIS ERROS
                              'C',           -- 'U' -> UTEIS; 'C' -> CORRIDOS
                              0,          -- 0 PARA RETORAR NA VARIAVEL
                              @CETIP_SELIC,        -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                              @DT_FIM_VAR_LOOP_C,     -- DATA PARA IGPM PREVIO
                              @SGL_SISTEMA = 'RDF',      -- LIANA - 22/11/2010
                              @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                            , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                            , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                        IF @ERRO <> -1
                        BEGIN
                           SELECT
                              @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                    RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                           IF @VB_SQL IS NOT NULL
                           BEGIN

                              SELECT
                                 @TIR_U      TIR_U,
                                 @TIR_C      TIR_C,
                                 @ERRO    ERRO,
                                 @ERR_MSG ERR_MSG

                              RETURN
                           END
                           ELSE
                              RETURN

                        END

                        -- ACUMULA A CORRECAO

                        SELECT
                           @CORRECAO_C = @CORRECAO_C * @CORRECAO_LOOP

                     END


                     -- BUSCA O PERIODO ANTERIOR

                     SELECT
                        @ID_RPAC_LOOP   = @ID_RPAC_LOOP - 1


                     SELECT
                        @DT_FIM_VAR_LOOP = @DT_INI_VAR_LOOP,   -- DATA DE CALCULO
                        @DT_FIM_VAR_LOOP_C = @DT_INI_VAR_LOOP, -- DATA DE CALCULO
                        @DT_INI_VAR_LOOP = DT_INIC_RPAC, -- DATA BASE DE CALCULO OU EMISSAO
                        @IDX_PC_LOOP     = CASE WHEN @IC_EVE_CORC_TIT = 'S' THEN @IDX_PC_LOOP
                                 ELSE IDX_PC
                                 END,        -- SE FOR UM EVENTO DE CORRECAO, UTILIZAR O PERCENTUAL DE CORRECAO
                        @IC_COR_TIT_LOOP = IC_EVE_CORC_TIT  -- EVENTO DE CORRECAO DO TITULO
                     FROM
                        SANT643_RF_REPAC_TITULO
                     WHERE
                        TITULO_ID   = @TITULO_ID      AND
                        ID_RPAC     = @ID_RPAC_LOOP

                     -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE EH A DATA DO ANIVERSARIO DO PAPEL
                     if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR = 1
                     BEGIN
                        IF @DT_FIM_VAR_LOOP_C > @DT_CORRECAO_C
                        BEGIN
                           SELECT @DT_FIM_VAR_LOOP = @DT_CORRECAO_U
                           SELECT @DT_FIM_VAR_LOOP_C = @DT_CORRECAO_C
--                         select @DT_FIM_VAR_LOOP_C '@DT_FIM_VAR_LOOP_C', @DT_FIM_VAR_LOOP '@DT_FIM_VAR_LOOP'
                        END
                     END



                  END
               END
               ELSE
               BEGIN

                  IF @DT_BASE_CALC IS NULL

                     SELECT
                        @DT_BASE_CALC = @RF_BASE_CALC

                  -- SE FOR INDEXADO PELO CDI, UTILIZAR O PROJETADO

                  IF @IDX_CODIGO = 'CDI' AND @EH_FUNDO = 'S'
                  BEGIN

                     -- RF2333.PRC

                     EXEC SANPP_RF_CALC_CDI_PROJETADO @TITULO_ID,    -- IDENTIFICADOR DO TITULO
                                    @ID_RPAC,      -- IDENTIFICADOR DO PERIODO DE REPACTUACAO
                                    @EVE_CORRENTE,    -- TIPO DO EVENTO A SER UTILIZADO NA PROJECAO
                                    @DT_VAR_TIR,      -- DATA DE CALCULO DA PROJECAO
                                    @DT_EVE,    -- DATA DO EVENTO A PROJETAR O CDI
                                    @DT_BASE_CALC,    -- DATA BASE DE CALCULO
                                    @DT_PROX_RPAC,    -- DATA DA PROXIMA REPACTUACAO OU VENCIMENTO
                                    @IDX_CODIGO,      -- INDEXADOR
                                    @IDX_PC,    -- PERCENTUAL DO INDEXADOR
                                    @FER_CHAVE,    -- POSICAO PARA FERIADOS
                                    @CORRECAO_U OUTPUT,  -- CDI PROJETADO
                                    @ERRO    OUTPUT,  -- RETORNA -1 => SEM ERROS
                                    @ERR_MSG OUTPUT,  -- MENSAGEM DE ERRO
                                    NULL        -- QUANDO NAO NULO, RETORNA RECORDSET PARA VB


                     IF @ERRO <> -1
                     BEGIN

                        IF @VB_SQL IS NOT NULL
                        BEGIN
                           SELECT
                              @TIR_U      TIR_U,
                              @TIR_C      TIR_C,
                              @ERRO    ERRO,
                              @ERR_MSG ERR_MSG

                           RETURN
                        END
                        ELSE
                           RETURN

                     END

                     SELECT 
                        @CORRECAO_C = @CORRECAO_U

                  END
                  ELSE
                  BEGIN
                     SELECT  @DT_ATU      = @DT_VAR_TIR

                     -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE EH A DATA DO ANIVERSARIO DO PAPEL
                     if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR = 1
                     BEGIN
                        SELECT @DT_ATU = @DT_CORRECAO_U
                     END


--                      IF (@FL_ARRED = 'S' and @TIPO_ATU_CETIP > 0)
--                         EXEC SANPP_RF_CALC_DT_ATUAL_CORRECAO
--                            @TIPO_ATU_CETIP,
--                            @DT_EVE,
--                            @DT_VAR_TIR,
--                            @TIT_VENCTO,
--                            @DT_BASE_CALC,
--                            @DT_ATU OUTPUT,
--                            @ATU_FLUXOS_FUT

                     -- VARIACAO DO INDEXADOR POR DIAS UTEIS

                    IF @VCH_FC_DT_ANV = '002'
                    BEGIN
                        SELECT @VDT_DATA_INI = @DT_BASE_CALC
                        SELECT @VDT_DATA_FIM = (  SELECT MIN(DT_EVE) 
                                                  FROM SANT644_RF_AGENDA_EVENTO 
                                                  WHERE TITULO_ID =  @TITULO_ID
                                                    AND ID_RPAC   =  @ID_RPAC
                                                    AND DT_EVE    >= @DT_ATU
                                               )
                    END
                    ELSE
                    BEGIN
                        SELECT @VDT_DATA_INI = @DT_BASE_CALC
                        SELECT @VDT_DATA_FIM = @TIT_VENCTO
                    END

                     EXEC SIAN_SP_VARIACAO
                           @IDX_CODIGO,         -- INDEXADOR
                           @DT_ATU,       -- DATA DE AQUISICAO OU DE REPACTUACAO
                           @VDT_DATA_INI,       -- DATA BASE DE CALCULO OU EMISSAO
                           @VDT_DATA_FIM,         -- DATA DE VENCIMENTO
                           @IDX_PC,       -- PERCENTUAL (CDI OU REF)
                           @FLOAT0,       -- 0.0
                           @FLOAT0,       -- 0.0
                           '000',            -- '000'
                           @FER_CHAVE,       -- POSICAO PARA CALCULO DE FERIADOS
                           @TIT_VENCTO,         -- ANIVERSARIO
                           @FLOAT0,       -- 0.0
                           @FLOAT1,       -- 1.0
                           @CORRECAO_U OUTPUT,     -- VARIAVEL PARA RETORNAR A VARIACAO
                           @ERRO    OUTPUT,     -- RETORNA EVENTUAIS ERROS
                           'U',           -- 'U' -> UTEIS; 'C' -> CORRIDOS
                           0,          -- 0 PARA RETORAR NA VARIAVEL
                           @CETIP_SELIC,        -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                           @DT_CALC,         -- DATA DO IGPM PREVIO
                           NULL,
                           '001',
                           @UTILIZ_PREVIA,         -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                           @SGL_SISTEMA = 'RDF',      -- LIANA - 22/11/2010
                           @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 30/05/2011
                            , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                            , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR


                     IF @ERRO <> -1
                     BEGIN
                        SELECT
                           @ERR_MSG = @RF_CARACTERISTICA + ': TIR - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                 RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_AQUIS, 103)

                        IF @VB_SQL IS NOT NULL
                        BEGIN
                           SELECT
                              @TIR_U      TIR_U,
                              @TIR_C      TIR_C,
                              @ERRO    ERRO,
                              @ERR_MSG ERR_MSG

                           RETURN
                        END
                        ELSE
                           RETURN

                     END


                     SELECT  @DT_ATU      = @DT_VAR_TIR

                     -- LIANA - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE EH A DATA DO ANIVERSARIO DO PAPEL
                     IF @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR = 1
                     BEGIN
                        SELECT @DT_ATU = @DT_CORRECAO_C
                     END


--                      IF (@FL_ARRED = 'S' and @TIPO_ATU_CETIP > 0)
--                         EXEC SANPP_RF_CALC_DT_ATUAL_CORRECAO
--                            @TIPO_ATU_CETIP,
--                            @DT_EVE,
--                            @DT_VAR_TIR,
--                            @TIT_VENCTO,
--                            @DT_BASE_CALC,
--                            @DT_ATU OUTPUT,
--                            @ATU_FLUXOS_FUT
-- 
                     -- VARIACAO DO INDEXADOR POR DIAS CORRIDOS

                    IF @VCH_FC_DT_ANV = '002'
                    BEGIN
                        SELECT @VDT_DATA_INI = @DT_BASE_CALC
                        SELECT @VDT_DATA_FIM = (  SELECT MIN(DT_EVE) 
                                                  FROM SANT644_RF_AGENDA_EVENTO 
                                                  WHERE TITULO_ID =  @TITULO_ID
                                                    AND ID_RPAC   =  @ID_RPAC
                                                    AND DT_EVE    >= @DT_ATU
                                               )
                    END
                    ELSE
                    BEGIN
                        SELECT @VDT_DATA_INI = @DT_BASE_CALC
                        SELECT @VDT_DATA_FIM = @TIT_VENCTO
                    END

                     EXEC SIAN_SP_VARIACAO
                           @IDX_CODIGO,         -- INDEXADOR
                           @DT_ATU,       -- DATA DE AQUISICAO OU DE REPACTUACAO
                           @VDT_DATA_INI,       -- DATA BASE DE CALCULO OU EMISSAO
                           @VDT_DATA_FIM,         -- DATA DE VENCIMENTO
                           @IDX_PC,       -- PERCENTUAL (CDI OU REF)
                           @FLOAT0,       -- 0.0
                           @FLOAT0,       -- 0.0
                           '000',            -- '000'
                           @FER_CHAVE,       -- POSICAO PARA CALCULO DE FERIADOS
                           @TIT_VENCTO,         -- ANIVERSARIO
                           @FLOAT0,       -- 0.0
                           @FLOAT1,       -- 1.0
                           @CORRECAO_C OUTPUT,     -- VARIAVEL PARA RETORNAR A VARIACAO
                           @ERRO    OUTPUT,     -- RETORNA EVENTUAIS ERROS
                           @TIPO_COTACAO_C,     -- 'U' -> UTEIS; 'C' -> CORRIDOS
                           0,          -- 0 PARA RETORAR NA VARIAVEL
                           @CETIP_SELIC,        -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                           @DT_CALC,         -- DATA DO IGPM PREVIO
                           NULL,
                           '001',
                           @UTILIZ_PREVIA,         -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                           @SGL_SISTEMA = 'RDF',      -- LIANA - 22/11/2010
                           @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                            , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                            , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                     IF @ERRO <> -1
                     BEGIN
                        SELECT
                           @ERR_MSG = @RF_CARACTERISTICA + ': TIR - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                 RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_AQUIS, 103)

                        IF @VB_SQL IS NOT NULL
                        BEGIN
                           SELECT
                              @TIR_U      TIR_U,
                              @TIR_C      TIR_C,
                              @ERRO    ERRO,
                              @ERR_MSG ERR_MSG

                           RETURN
                        END
                        ELSE
                           RETURN

                     END
  END
               END

               -- CASO O TITULO UTILIZE ARREDONDAMENTO CETIP
               IF @FL_ARRED = 'S'
               BEGIN
                  IF @FL_IDX_CONTABIL IS NULL OR @FL_IDX_CONTABIL = 'N'
                  BEGIN
                     -- CORRECAO DEVE SER IGUAL PARA A MONTAGEM DOS FLUXOS 252/360 DOS TITULOS NA NEGOCIACAO
                     IF @IDX_CODIGO = 'TR' OR @IDX_CODIGO = 'CDI'
                        SELECT @CORRECAO_C = @CORRECAO_U
                     ELSE -- IGPM/IPCA
                        SELECT @CORRECAO_U = @CORRECAO_C
                  END

                  SELECT   @V_NOML_PCPL = ROUND(@V_NOML_PCPL,@VALORES_CASAS,@VALORES_ARRED),
                     @CORRECAO_C = ROUND(@CORRECAO_C,@CORRECAO_CASAS,@CORRECAO_ARRED),
                     @CORRECAO_U = ROUND(@CORRECAO_U,@CORRECAO_CASAS,@CORRECAO_ARRED)
               END

               -- SE FOR UM EVENTO DE ATUALIZACAO SOBRE AMORTIZACAO APLICA A VARIACAO SOBRE A AMORTIZACAO

               IF @EVE_CORRENTE = @EVE_CORC_AMTC

                  SELECT
                     @V_FUT_U_PARC = @V_FUT_U_PARC + (@V_NOML_AMTC * (@CORRECAO_U - @FLOAT1)),
                     @V_FUT_C_PARC = @V_FUT_C_PARC + (@V_NOML_AMTC * (@CORRECAO_C - @FLOAT1))

               ELSE

                  SELECT
                     @V_FUT_U_PARC = @V_FUT_U_PARC + (@V_NOML_PCPL * (@CORRECAO_U - @FLOAT1)),
                     @V_FUT_C_PARC = @V_FUT_C_PARC + (@V_NOML_PCPL * (@CORRECAO_C - @FLOAT1))

            END
         END



         -- FLUXOS DE AMORTIZACAO

         IF EXISTS (
            SELECT
               *
            FROM
               SANT644_RF_AGENDA_EVENTO
            WHERE
               TITULO_ID   = @TITULO_ID      AND
               ID_RPAC     = @ID_RPAC     AND
               ID_T_EVE = @EVE_AMTC_PCPL  AND
               DT_EVE      = @DT_EVE
            )
         BEGIN

            SELECT
               @V_FUT_U_PARC  = @V_FUT_U_PARC + V_PU_AMTC,
               @V_FUT_C_PARC  = @V_FUT_C_PARC + V_PU_AMTC
            FROM
               SANT644_RF_AGENDA_EVENTO
            WHERE
               TITULO_ID   = @TITULO_ID      AND
               ID_RPAC     = @ID_RPAC     AND
               ID_T_EVE = @EVE_AMTC_PCPL  AND
               DT_EVE      = @DT_EVE

            -- CASO O TITULO UTILIZE ARREDONDAMENTO CETIP
            IF @FL_ARRED = 'S'
               SELECT   @V_FUT_U_PARC = ROUND(@V_FUT_U_PARC,@VALORES_CASAS,@VALORES_ARRED),
                  @V_FUT_C_PARC = ROUND(@V_FUT_C_PARC,@VALORES_CASAS,@VALORES_ARRED)
         END



         -- 06.09.2005
         -- APURAR O VALOR FUTURO PARA CDI

         IF @IDX_CODIGO = 'CDI' AND @EH_FUNDO = 'S'

         BEGIN


            -- BUSCAR CENARIO PRE NA DATA DO EVENTO

            SELECT
               @CEN_DATA_BASE = @DT_CALC,
               @CEN_DATA   = @DT_EVE_LQDC,
               @SGL_B_EXPS = '022',
               @TX_CENARIO = @FLOAT0



            -- SE A DATA DO EVENTO FOR APOS A REPACTUACAO ASSUMIR A DATA DE REPACTUACAO 

            IF @DT_EVE > @DT_PROX_RPAC

               SELECT
                  @CEN_DATA = @DT_PROX_RPAC



            -- RF0885.PRC

            EXEC SANPS_RF_BUSCA_CENARIO   'R$',       -- SIGLA DA MOEDA OU INDEXADOR
                        @CEN_DATA_BASE OUTPUT,  -- DATA BASE
                        @CEN_DATA   OUTPUT,  -- DATA DE PROJECAO DO CENARIO
                        @DT_PROX_RPAC,    -- VENCIMENTO DO TITULO
                        'N',        -- INDICADOR DE CENARIO NO VENCIMENTO
                        @FER_CHAVE,    -- POSICAO PARA FERIADOS
                        'S',        -- INDICADOR DE POSICAO SER FUNDO
                        '000',         -- FORMA DE CALCULO DO INDEXADOR
                        'N',        -- INDICADOR PARA OPERACOES PRE
                        '001',         -- FORMA DE CALCULO DA MTM
                        @FLOAT0,    -- TAXA UTEIS DA OPERACAO
                        @FLOAT0,    -- TAXA CORRIDOS DA OPERACAO
                        @SGL_B_EXPS OUTPUT,  -- EXPRESSAO DO CENARIO
                        @TX_CENARIO OUTPUT,  -- CENARIO PARA MARCACAO A MERCADO
                        @ERRO    OUTPUT,  -- -1 = OK; 0 = ERRO; 1 = NAO ACHOU CENARIO
                        @ERR_MSG OUTPUT   -- MENSAGEM DE ERRO

            IF @ERRO <> -1
            BEGIN
               SELECT
                  @ERR_MSG = @RF_CARACTERISTICA + ': 1-PROBLEMAS NO CALCULO DA TIR.' + 
                        ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

               IF @VB_SQL IS NOT NULL
               BEGIN
                  SELECT
                     @TIR_U      TIR_U,
                     @TIR_C      TIR_C,
                     @ERRO    ERRO,
                     @ERR_MSG ERR_MSG

                  RETURN
               END
               ELSE
                  RETURN

            END



            SELECT
               @V_FUT_U_PARC = @V_FUT_U_PARC / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
                     ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC )


            SELECT
               @V_FUT_C_PARC = @V_FUT_U_PARC

         END



         -- INSERE OS FLUXOS UTEIS

         INSERT INTO SANT646_RF_TIR
            (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
         VALUES
            (
            @ID_U,
            @CD_CE,
            @ID_NUM_PARC,
            @Q_DIAS_U_PARC,
            @V_FUT_U_PARC,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0
            )


         -- INSERE OS FLUXOS CORRIDOS

         INSERT INTO SANT646_RF_TIR
            (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
         VALUES
            (
            @ID_C,
            @CD_CE,
            @ID_NUM_PARC,
            @Q_DIAS_C_PARC,
            @V_FUT_C_PARC,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0
            )

      END

      -- APURA A TIR UTEIS

      -- RF0797.PRC

      EXEC SANPP_RF_TIR @ID_U,
               @CD_CE,
               @FLOAT252,
               @ERRO    OUTPUT,
               @TIR_U      OUTPUT

      IF @ERRO <> -1
      BEGIN
         SELECT
            @ERR_MSG = @RF_CARACTERISTICA + ': 2-PROBLEMAS NO CALCULO DA TIR.' + 
                  ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

         IF @VB_SQL IS NOT NULL
         BEGIN
            SELECT
               @TIR_U      TIR_U,
               @TIR_C      TIR_C,
               @ERRO    ERRO,
               @ERR_MSG ERR_MSG

            RETURN
         END
         ELSE
            RETURN

      END

      --ALTERACAO CRI 12/07/2006
      IF (@FL_DESCAP = 'S') AND (@RFX_FCALC = '002' OR @RFX_FCALC = '035')
         SELECT @BASE = @FLOAT365
      ELSE
         SELECT @BASE = @FLOAT360


      -- APURA A TIR CORRIDOS

      -- RF0797.PRC
      EXEC SANPP_RF_TIR @ID_C,
               @CD_CE,
               @BASE,
               @ERRO    OUTPUT,
               @TIR_C      OUTPUT

      IF @ERRO <> -1
      BEGIN
         SELECT
            @ERR_MSG = @RF_CARACTERISTICA + ': 3-PROBLEMAS NO CALCULO DA TIR.' + 
                  ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

         IF @VB_SQL IS NOT NULL
         BEGIN
            SELECT
               @TIR_U      TIR_U,
               @TIR_C      TIR_C,
               @ERRO    ERRO,
               @ERR_MSG ERR_MSG

            RETURN
         END
         ELSE
            RETURN

      END

   END
   ELSE
   BEGIN -- OPERACAOES PADRONIZADAS


      -- VARIACAO DO INDEXADOR POR DIAS UTEIS

      EXEC SIAN_SP_VARIACAO
            @IDX_CODIGO,         -- INDEXADOR
            @DT_VAR_TIR,         -- DATA DE AQUISICAO OU DE REPACTUACAO
            @RF_BASE_CALC,       -- DATA BASE DE CALCULO OU EMISSAO
            @TIT_VENCTO,         -- DATA DE VENCIMENTO
            @IDX_PC,       -- PERCENTUAL (CDI OU REF)
            @FLOAT0,       -- 0.0
            @FLOAT0,       -- 0.0
            '000',            -- '000'
            @FER_CHAVE,       -- POSICAO PARA CALCULO DE FERIADOS
            @TIT_VENCTO,         -- ANIVERSARIO
            @FLOAT0,       -- 0.0
            @FLOAT1,       -- 1.0
            @CORRECAO_U OUTPUT,     -- VARIAVEL PARA RETORNAR A VARIACAO
            @ERRO    OUTPUT,     -- RETORNA EVENTUAIS ERROS
            'U',           -- 'U' -> UTEIS; 'C' -> CORRIDOS
            0,          -- 0 PARA RETORAR NA VARIAVEL
            @CETIP_SELIC,        -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
            @DT_CALC,         -- DATA DO IGPM PREVIO
            @SGL_SISTEMA = 'RDF'    -- LIANA - 22/11/2010

      IF @ERRO <> -1
      BEGIN
         SELECT
            @ERR_MSG = @RF_CARACTERISTICA + ': TIR - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                  RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_AQUIS, 103)

         IF @VB_SQL IS NOT NULL
         BEGIN
            SELECT
               @TIR_U      TIR_U,
               @TIR_C      TIR_C,
               @ERRO    ERRO,
               @ERR_MSG ERR_MSG

            RETURN
         END
         ELSE
            RETURN

      END


      -- VARIACAO DO INDEXADOR POR DIAS CORRIDOS

      EXEC SIAN_SP_VARIACAO
            @IDX_CODIGO,         -- INDEXADOR
            @DT_VAR_TIR,         -- DATA DE AQUISICAO OU DE REPACTUACAO
            @RF_BASE_CALC,       -- DATA BASE DE CALCULO OU EMISSAO
            @TIT_VENCTO,         -- DATA DE VENCIMENTO
            @IDX_PC,       -- PERCENTUAL (CDI OU REF)
            @FLOAT0,       -- 0.0
            @FLOAT0,       -- 0.0
            '000',            -- '000'
            @FER_CHAVE,       -- POSICAO PARA CALCULO DE FERIADOS
            @TIT_VENCTO,         -- ANIVERSARIO
            @FLOAT0,       -- 0.0
            @FLOAT1,       -- 1.0
            @CORRECAO_C OUTPUT,     -- VARIAVEL PARA RETORNAR A VARIACAO
            @ERRO    OUTPUT,     -- RETORNA EVENTUAIS ERROS
            'C',           -- 'U' -> UTEIS; 'C' -> CORRIDOS
            0,          -- 0 PARA RETORAR NA VARIAVEL
            @CETIP_SELIC,        -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
            @DT_CALC,         -- DATA DO IGPM PREVIO
            @SGL_SISTEMA = 'RDF'    -- LIANA - 22/11/2010

      IF @ERRO <> -1
      BEGIN
         SELECT
            @ERR_MSG = @RF_CARACTERISTICA + ': TIR - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                  RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_AQUIS, 103)

         IF @VB_SQL IS NOT NULL
         BEGIN
            SELECT
               @TIR_U      TIR_U,
               @TIR_C      TIR_C,
               @ERRO    ERRO,
               @ERR_MSG ERR_MSG

            RETURN
         END
         ELSE
            RETURN

      END



      -- TAXA UTEIS

      SELECT
         @ID_NUM_PARC   = @FLOAT0,
         @Q_DIAS_U_PARC = @FLOAT0,
         @V_FUT_U_PARC  = @FLOAT0


      SELECT
         @DT_FIM_ORI = @TIT_VENCTO,
         @DT_INI_ORI = @TIT_VENCTO,
         @DT_FIM     = @TIT_VENCTO


      EXEC @EH_RESERVA = SIAN_E_RESERVA   @DT_FIM,
                     'A',
                     @FER_CHAVE,
                     0

      IF @EH_RESERVA <> -1 -- NAO EH DIA UTIL

         EXEC SIAN_SP_PROXIMA_RESERVA  'A',
                     @FER_CHAVE,
                     @DT_FIM     OUTPUT,
                     0


      SELECT
         @DT_INI     = @DT_FIM


      INSERT INTO SANT646_RF_TIR
         (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
      VALUES
         (
         @ID_U,
         @CD_CE,
         @ID_NUM_PARC,
         @Q_DIAS_U_PARC,
         ISNULL(@PU_MED_U, @PU_AQUIS) * -1, -- KUBA 10/01/2017
         @FLOAT0,
         @FLOAT0,
         @FLOAT0,
         @FLOAT0,
         @FLOAT0
         )


      WHILE @DT_INI > @DT_VAR_TIR
      BEGIN

         SELECT
            @DT_INI_ORI = DateAdd(month, (-1 * @ATV_PRZ_PGJUROS), @DT_FIM_ORI)


         IF @DT_INI_ORI < @TIT_EMISSAO AND NOT (@IDX_FCALC = '007' AND @CETIP_SELIC = 'S')

            SELECT
               @DT_INI_ORI = @TIT_EMISSAO


         SELECT
            @DT_INI = @DT_INI_ORI

         EXEC @EH_RESERVA = SIAN_E_RESERVA   @DT_INI,
                        'A',
                        @FER_CHAVE,
                        0

         IF @EH_RESERVA <> -1 -- NAO EH DIA UTIL
            AND NOT (@IDX_FCALC = '007' AND @CETIP_SELIC = 'S')

            EXEC SIAN_SP_PROXIMA_RESERVA  'A',
                        @FER_CHAVE,
                        @DT_INI     OUTPUT,
                        0


         IF @IDX_FCALC = '007'

            SELECT
               @DT_FIM = @DT_FIM_ORI


         -- VARIACAO DO CUPOM

         -- RF0887.PRC

         EXEC SANPP_RF_CALC_CUPOM_TIR  @TIT_EMISSAO,
                     @TIT_VENCTO,
                     @DT_INI,
                     @DT_FIM,
                     @FER_CHAVE,
                     @FCALC_CUPOM,
                     @TIT_CUPOM,
                     @RFX_FCALC,
                     @TIT_EMISSAO,
                     @ATV_PRZ_PGJUROS,
                     @CUPOM_TIR  OUTPUT


         SELECT
            @ID_NUM_PARC = @ID_NUM_PARC + 1

         EXEC @Q_DIAS_U_PARC = SIAN_SP_QUANTAS_RESERVAS  @DT_AQUIS,
                           @DT_FIM,
                           '001',
                           'A',
                           @FER_CHAVE,
                           0

         IF @DT_FIM_ORI = @TIT_VENCTO
            SELECT
               @V_FUT_U_PARC = (@CUPOM_TIR * @CORRECAO_U * @ATV_LOTE)
         ELSE
            SELECT
               @V_FUT_U_PARC = ((@CUPOM_TIR - @FLOAT1) * @CORRECAO_U * @ATV_LOTE)


         INSERT INTO SANT646_RF_TIR
            (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
         VALUES
            (
            @ID_U,
            @CD_CE,
            @ID_NUM_PARC,
            @Q_DIAS_U_PARC,
            @V_FUT_U_PARC,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0
            )


         SELECT
            @DT_FIM     = @DT_INI,
            @DT_FIM_ORI = @DT_INI_ORI

      END


      -- RF0797.PRC

      EXEC SANPP_RF_TIR @ID_U,
               @CD_CE,
               @FLOAT252,
               @ERRO    OUTPUT,
               @TIR_U      OUTPUT

      IF @ERRO <> -1
      BEGIN
         SELECT
            @ERR_MSG = @RF_CARACTERISTICA + ': 4-PROBLEMAS NO CALCULO DA TIR.' + 
                  ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

         IF @VB_SQL IS NOT NULL
         BEGIN
            SELECT
               @TIR_U      TIR_U,
               @TIR_C      TIR_C,
               @ERRO    ERRO,
               @ERR_MSG ERR_MSG

            RETURN
         END
         ELSE
            RETURN

      END


      -- TAXA CORRIDOS

      SELECT
         @ID_NUM_PARC   = @FLOAT0,
         @Q_DIAS_C_PARC = @FLOAT0,
         @V_FUT_C_PARC  = @FLOAT0


      SELECT
         @DT_FIM_ORI = @TIT_VENCTO,
         @DT_INI_ORI = @TIT_VENCTO,
         @DT_FIM     = @TIT_VENCTO


      EXEC @EH_RESERVA = SIAN_E_RESERVA   @DT_FIM,
                     'A',
                     @FER_CHAVE,
                     0

      IF @EH_RESERVA <> -1 -- NAO EH DIA UTIL

         EXEC SIAN_SP_PROXIMA_RESERVA  'A',
                     @FER_CHAVE,
                     @DT_FIM     OUTPUT,
                     0


      SELECT
         @DT_INI = @DT_FIM


      INSERT INTO SANT646_RF_TIR
         (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
      VALUES
         (
         @ID_C,
         @CD_CE,
         @ID_NUM_PARC,
         @Q_DIAS_C_PARC,
         ISNULL(@PU_MED_C, @PU_AQUIS) * -1, -- KUBA 10/01/2017
         @FLOAT0,
         @FLOAT0,
         @FLOAT0,
         @FLOAT0,
         @FLOAT0
         )


      WHILE @DT_INI > @DT_VAR_TIR
      BEGIN

         SELECT
            @DT_INI_ORI = DateAdd(month, (-1 * @ATV_PRZ_PGJUROS), @DT_FIM_ORI)


         IF @DT_INI_ORI < @TIT_EMISSAO AND NOT (@IDX_FCALC = '007' AND @CETIP_SELIC = 'S')

            SELECT
               @DT_INI_ORI = @TIT_EMISSAO


         SELECT
            @DT_INI = @DT_INI_ORI

         EXEC @EH_RESERVA = SIAN_E_RESERVA   @DT_INI,
                        'A',
                        @FER_CHAVE,
                        0

         IF @EH_RESERVA <> -1 -- NAO EH DIA UTIL
            AND NOT (@IDX_FCALC = '007' AND @CETIP_SELIC = 'S')

            EXEC SIAN_SP_PROXIMA_RESERVA  'A',
                        @FER_CHAVE,
                        @DT_INI     OUTPUT,
                        0


         IF @IDX_FCALC = '007'

            SELECT
               @DT_FIM = @DT_FIM_ORI


         -- VARIACAO DO CUPOM

         -- RF0887.PRC

         EXEC SANPP_RF_CALC_CUPOM_TIR  @TIT_EMISSAO,
                     @TIT_VENCTO,
                     @DT_INI,
                     @DT_FIM,
                     @FER_CHAVE,
                     @FCALC_CUPOM,
                     @TIT_CUPOM,
                     @RFX_FCALC,
                     @TIT_EMISSAO,
                     @ATV_PRZ_PGJUROS,
                     @CUPOM_TIR  OUTPUT


         SELECT
            @ID_NUM_PARC = @ID_NUM_PARC + 1


         IF @RFX_FCALC = '032' OR (@RFX_FCALC = '035' AND @TIT_EMISSAO > @DT20000801)  -- EXPONENCIAL 30 / 360
            EXEC SIAN_SP_TRINTA_DIAS   @DT_FIM,
                        @DT_AQUIS,
                        @Q_DIAS_C_PARC OUTPUT,
                        0,
                        'A'   -- PADRAO AMERICANO
         ELSE
            SELECT
               @Q_DIAS_C_PARC = Datediff (day, @DT_AQUIS, @DT_FIM)


         IF @DT_FIM_ORI = @TIT_VENCTO
            SELECT
               @V_FUT_C_PARC = (@CUPOM_TIR * @CORRECAO_C * @ATV_LOTE)
         ELSE
            SELECT
               @V_FUT_C_PARC = ((@CUPOM_TIR - @FLOAT1) * @CORRECAO_C * @ATV_LOTE)


         INSERT INTO SANT646_RF_TIR
            (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
         VALUES
            (
            @ID_C,
            @CD_CE,
            @ID_NUM_PARC,
            @Q_DIAS_C_PARC,
            @V_FUT_C_PARC,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0,
            @FLOAT0
            )


         SELECT
            @DT_FIM     = @DT_INI,
            @DT_FIM_ORI = @DT_INI_ORI

      END


      --RICARDO 12/03/2004
      --CALCULO UTILIZANDO CAPTALIZACAO SEMESTRAL PARA A BOLETAGEM COM OS MESMOS
      --CRITERIOS DO MOS( BASE 365)
      IF @RFX_FCALC = '035'
         SELECT @BASE = @FLOAT365
      ELSE
         SELECT @BASE = @FLOAT360

      -- RF0797.PRC
      EXEC SANPP_RF_TIR @ID_C,
               @CD_CE,
               @BASE,
               @ERRO    OUTPUT,
               @TIR_C      OUTPUT

      IF @ERRO <> -1
      BEGIN
         SELECT
            @ERR_MSG = @RF_CARACTERISTICA + ': 5-PROBLEMAS NO CALCULO DA TIR.' + 
                  ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

         IF @VB_SQL IS NOT NULL
         BEGIN
            SELECT
               @TIR_U      TIR_U,
              @TIR_C      TIR_C,
               @ERRO    ERRO,
               @ERR_MSG ERR_MSG

            RETURN
         END
         ELSE
            RETURN

      END

      --RICARDO 12/03/2004
      --CALCULO UTILIZANDO CAPTALIZACAO SEMESTRAL PARA A BOLETAGEM COM OS MESMOS
      --CRITERIOS DO MOS
      IF @RFX_FCALC = '035' AND @TIT_EMISSAO > @DT20000801 AND @IC_OPRC_NAO_PADR <> 'S' 
      BEGIN

         SELECT @TIR_C = POWER((@TIR_C / @FLOAT100 + @FLOAT1), @FLOAT360 / @FLOAT365) - @FLOAT1
         SELECT @TIR_C = ROUND((POWER(@TIR_C + @FLOAT1, @FLOAT1 / @FLOAT2) - @FLOAT1) * @FLOAT2 * @FLOAT100, 4) 

      END
   END

   DELETE FROM SANT646_RF_TIR WHERE ID_USR IN (@ID_U, @ID_C)

   IF @VB_SQL IS NOT NULL
   BEGIN
      SELECT
         @TIR_U      TIR_U,
         @TIR_C      TIR_C,
         @ERRO    ERRO,
         @ERR_MSG ERR_MSG

      RETURN
   END

END

 

