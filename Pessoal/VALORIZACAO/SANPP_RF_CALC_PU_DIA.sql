Text
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- IF EXISTS (SELECT * FROM SYSOBJECTS WHERE NAME = 'SANPP_RF_CALC_PU_DIA')
-- DROP PROCEDURE SANPP_RF_CALC_PU_DIA
-- go
-- 
-- SET QUOTED_IDENTIFIER ON 
-- GO
-- SET ANSI_NULLS ON 
-- GO



CREATE PROCEDURE DBO.SANPP_RF_CALC_PU_DIA
        (
        @RF_CARACTERISTICA      CHAR(20),               -- OPERACAO A SER CALCULADA. SE NAO FOR VALORIZACAO PASSAR ''
        @DT_CALC                DATETIME,               -- DATA DE CALCULO DO PU
        @TITULO_ID              INT,                    -- IDENTIFICADOR DO TITULO
        @ID_RPAC                INT,                    -- IDENTIFICADOR DO PERIODO DE REPACTUACAO OU ZERO PARA PADRONIZADOS
        @IC_OPRC_NAO_PADR       CHAR(01),               -- INDICADOR DE OPERACAO NAO PADRONIZADA
        @TIT_EMISSAO            DATETIME,               -- EMISSAO DO TITULO
        @TIT_VENCTO             DATETIME,               -- VENCIMENTO DO TITULO
        @TIT_CUPOM              FLOAT,                  -- CUPOM DO TITULO
        @RFX_FCALC              CHAR(03),               -- FORMA DE CALCULO DE JUROS
        @FCALC_CUPOM            CHAR(03),               -- FORMA DE CALCULO DO CUPOM
        @IDX_CODIGO             VARCHAR(15),            -- INDEXADOR DE CORRECAO
        @DT_AQUIS               DATETIME,               -- DATA DE AQUISICAO DO TITULO
        @DT_INIC_RPAC           DATETIME,               -- DATA DE INICIAL DA REPACTUACAO
        @DT_PROX_RPAC           DATETIME,               -- DATA DA PROXIMA REPACTUACAO
        @RF_BASE_CALC           DATETIME,               -- DATA BASE DE CALCULO DO TITULO
        @IDX_PC                 FLOAT,                  -- PERCENTUAL DO INDEXADOR
        @FER_CHAVE              VARCHAR(15),            -- POSICAO PARA FERIADOS
        @EH_FUNDO               CHAR(01),               -- FLAG PARA FUNDO OU NAO
        @CETIP_SELIC            CHAR(01),               -- 'C'ETIP PARA PRIVADOS OU 'S'ELIC PARA PUBLICOS
        @ATV_PRZ_PGJUROS        INT,                    -- PRAZO DE PAGAMENTO DE JUROS PARA NTNs E NBC-Es
        @ATV_LOTE               FLOAT,                  -- NOMINAL DO TITULO PARA NTNs E NBC-Es
        @CTB_CURVA              CHAR(01),               -- CURVA DE CONTABILIZACAO
        @RFX_RESULTADO          CHAR(03),               -- FORMA DE CALCULO DA CURVA MERCADO
        @SGL_MEDA               VARCHAR(15),            -- SIGLA DA MOEDA APC
        @IC_CEN_VCMT            CHAR(01),               -- INDICADOR DE CENARIO DO VENCIMENTO
        @IDX_GERENCIAL          VARCHAR(15),            -- INDEXADOR GERENCIAL PARA PU MANUAL
        @ATV_IDX_CUSTO          VARCHAR(15),            -- INDEXADOR DE CUSTO PARA MTM PRE
        @MERC_DT_AQUIS          CHAR(01),               -- INDICADOR DE CALCULO DO CENARIO EM DATA DE AQUISICAO
        @TIR_U                  FLOAT,                  -- TIR UTEIS
        @TIR_C                  FLOAT,                  -- TIR CORRIDOS
        @VAR_CORRECAO           FLOAT           OUTPUT, -- VARIACAO PARA O CALCULO DA CORRECAO
        @RFS_PU_UTEIS           FLOAT           OUTPUT, -- RETORNA O PU UTEIS
        @RFS_PU_CORRIDOS        FLOAT           OUTPUT, -- RETORNA O PU CORRIDOS
        @RFS_PU_MERCADO         FLOAT           OUTPUT, -- RETORNA O PU MERCADO
        @RFSC_VALOR             FLOAT           OUTPUT, -- RETORNA A ULTIMA COTACAO
        @TIR_M                  FLOAT           OUTPUT, -- RETORNA A TIR MERCADO
        @DT_ACABOU_CEN          DATETIME        OUTPUT, -- RETORNA A DATA QUE ACABOU O CENARIO
        @ERRO                   INT             OUTPUT, -- RETORNA -1 => SEM ERROS
        @ERR_MSG                VARCHAR(100)    OUTPUT, -- MENSAGEM DE ERRO
        @VB_SQL                 INT     = NULL,         -- QUANDO NAO NULO, RETORNA RECORDSET PARA VB
        @V_PZ_MDIO_CALD         FLOAT   = NULL  OUTPUT,
        @V_DURT_CALD            FLOAT   = NULL  OUTPUT,
        @FL_DESCAP              CHAR(1) = NULL,   -- INDICA SE VAI ACHAR O PU DESCAPITALIZANDO PELA BASE DA TAXA DO TITULO OU 252/360
        @FL_IDX_CONTABIL        CHAR(1) = NULL,         -- INDICA QUE VAMOS UTILIZAR O FATOR DO INDEXADOR CORRIDOS E UTEIS PARA O CALCULO DO PU (PROJETO CRI/LH)
                                                        -- *** NA NEGOCIACAO DEVEMOS UTILIZAR SEMPRE A BASE DO TITULO (CORRIDOS OU UTEIS)
                                                        -- *** NA VALORIZACAO (CONTABIL) VAMOS UTILIZAR SEMPRE 360 PARA O SAFRABM E EMPRESAS DO GRUPO
        @DT_VLRZ_TERMO DATETIME = NULL,
        @V_DURT_CALD_VOLTA FLOAT        = NULL --PAÇOCA
        )

AS
BEGIN

        /*** RF0884.PRC ***/

        DECLARE
                @FLOAT0                 FLOAT,
                @FLOAT1                 FLOAT,
                @FLOAT100               FLOAT,
                @FLOAT252               FLOAT,
                @FLOAT360               FLOAT,
                @EVE_JURS_AMTC          CHAR(01),
                @EVE_JURS_PCPL          CHAR(01),
                @EVE_JURS_DTBS          CHAR(01),
                @EVE_CORC_AMTC          CHAR(01),
                @EVE_CORC_PCPL          CHAR(01),
                @EVE_AMTC_PCPL          CHAR(01),
                @EVE_CORRENTE           CHAR(01),
                @CD_CE                  INT,

                @DT_UTIL                DATETIME,
                @DT_EVE                 DATETIME,
                @DT_UTIL_EVE            DATETIME,
                @DT_UTIL_ANT            DATETIME,
                @DT_FNAL_EVE            DATETIME,
                @DT_BASE_CALC           DATETIME,
                @DT_B_JURS              DATETIME,
                @DT_VCMT_UTIL           DATETIME,
                @IDX_PRE                CHAR(01),
                @IDX_FCALC              CHAR(03),
                @CUPOM_TIR              FLOAT,
                @CUPOM_TIR_C            FLOAT,
                @ID_T_EVE               CHAR(01),
                @EVE_POR_DATA           VARCHAR(20),
                @DT_INI                 DATETIME,
                @DT_FIM                 DATETIME,
                @DT_INI_ORI             DATETIME,
                @DT_FIM_ORI             DATETIME,
                @CORRECAO_U             FLOAT,
                @CORRECAO_C             FLOAT,
                @CORRECAO_CDI           FLOAT,
                @V_NOML_PCPL            FLOAT,
                @V_NOML_AMTC            FLOAT,
                @ID_SEQ_EVE_COR         INT,
                @ID_SEQ_EVE_JUR         INT,
                @EH_RESERVA             INT,
                @DT_INI_CEN             DATETIME,
                @CEN_DATA_BASE          DATETIME,
                @CEN_DATA               DATETIME,
                @SGL_B_EXPS             CHAR(03),
                @IC_APC                 CHAR(01),
                @V_FUT_U_JURS           FLOAT,
                @V_FUT_C_JURS           FLOAT,
                @V_FUT_M_JURS           FLOAT,
                @V_FUT_U_CORC           FLOAT,
                @V_FUT_C_CORC           FLOAT,
                @V_FUT_M_CORC           FLOAT,
                @V_FUT_U_PCPL           FLOAT,
                @V_FUT_C_PCPL           FLOAT,
                @V_FUT_M_PCPL           FLOAT,
                @V_PRS_U_JURS           FLOAT,
                @V_PRS_C_JURS           FLOAT,
                @V_PRS_M_JURS           FLOAT,
                @V_PRS_U_CORC           FLOAT,
                @V_PRS_C_CORC           FLOAT,
                @V_PRS_M_CORC           FLOAT,
                @V_PRS_U_PCPL           FLOAT,
                @V_PRS_C_PCPL           FLOAT,
                @V_PRS_M_PCPL           FLOAT,
                @VAR_DESCAP_U           FLOAT,
                @VAR_DESCAP_C           FLOAT,
                @VAR_DESCAP_M           FLOAT,
                @BASE_TIR               FLOAT,
                @Q_DIAS_PARC  INT,
       @ID_NUM_PARC            INT,
                @V_PU_CALD_JURS         FLOAT,
                @V_PU_CALD_CORC         FLOAT,
                @V_PU_CALD_PCPL         FLOAT,
                @FL_PAGA_EVE            CHAR(01),
                @FORMA_CALC             CHAR(03),
                @DT20000801             DATETIME,
                @DATA                   DATETIME,
                @SGL_FCALC              CHAR(03),
                @SGL_BASE               CHAR(03), 
                @SGL_MEDA_BASE          CHAR(15),               
                @TX_PRE                 FLOAT,
                @TX_EMISSAO             FLOAT,
                @TX_PRE_EMIS            FLOAT,
                @TX_PRE_MTM             FLOAT,
                @DT_PROX_CEN            DATETIME,
                @CALC_MTM_CDI           CHAR(01),
                @CUPOM_PZ_MDIO          FLOAT,
                @CORC_PZ_MDIO           FLOAT,
                @V_FUT_PZ_MDIO          FLOAT,
                @V_FUT_TOTL             FLOAT,
                @IC_PZ_MDIO_RPAC        CHAR(01),
                @Q_DC_PZ_MEDIO          INT,
                @Q_DU_DURATION          INT,
                @DT_2_RPAC              DATETIME,
                @MAX_RPAC               INT,
                @IC_EVE_CORC_TIT        CHAR(01),
                @ID_RPAC_LOOP           INT,
                @DT_FIM_VAR_LOOP        DATETIME,
                @DT_FIM_VAR_LOOP_C      DATETIME,       -- Liana - 22/06/2011
                @DT_INI_VAR_LOOP        DATETIME,
                @IDX_PC_LOOP            FLOAT,
                @CORRECAO_LOOP          FLOAT,
                @RFSC_VALOR_LOOP        FLOAT,
                @IC_COR_TIT_LOOP        CHAR(01),

                @CENARIO_PRE            VARCHAR(15),
                @TX_CENARIO             FLOAT,
                @Q_DIAS_U_PARC          INT,

                @FL_ARRED               CHAR(01),       --INDICA SE UTILIZA ARREDONDAMENTO CETIP
                @CUPOM_TIR_CASAS        SMALLINT,       --VARIAVEIS PARA ARMAZENAR AS CASAS DECIMAIS E
                @CUPOM_TIR_ARRED        SMALLINT,       --SE ARREDONDA OU TRUNCA NO CALCULO CETIP.
                @VALORES_CASAS          SMALLINT,
                @VALORES_ARRED          SMALLINT,
                @CORRECAO_CASAS         SMALLINT,
                @CORRECAO_ARRED         SMALLINT,
                @DESCAP_CASAS           SMALLINT,
                @DESCAP_ARRED           SMALLINT,
                @VALOR_PRS_CASAS        SMALLINT,
                @VALOR_PRS_ARRED        SMALLINT,
                @PU_CASAS               SMALLINT,
                @PU_ARRED               SMALLINT,
                @ARREDONDA              SMALLINT,
                @TRUNCA                 SMALLINT,
                @TIPO_ATU_CETIP         SMALLINT,       --INDICA O TIPO DE ATUALIZACAO CETIP (1,2 ou 3)
                @DT_ATU                 DATETIME,       --AUXILIAR PARA ENCONTRAR DATA DE CALC ATUALIZACAO
                @UTILIZ_PREVIA          CHAR(01),       --INDICA QUE PARA DETERMINADO ATIVO UTILIZA PRÉVIA IGPM/IPCA
                @ATU_FLUXOS_FUT         CHAR(01),       --INDICA SE OS FLUXOS FUTUROS (POSTERIORES) AO ANO DE ATUALIZACAO 
                                                        --ATUAL SAO ATUALIZADOS COM O INDICE ATUAL
                @DT_PG_EFT              DATETIME,       --DATA AUXILIAR PARA TRATAMENTO DE PGTO DE EVENTO EM DIA NAO UTIL 
                                                        --PARA TITULOS COM BASE EM DIAS CORRIDOS
                @TIPO_RESERVA           CHAR(03),       --INDICA FORMA DE CONTAGEM DAS RESERVAS (DIF. TIR 252/360)
                @TIPO_COTACAO_C         CHAR(01),       --CONTABIL DO BANCO PARA TR EH PRORATA DE DIAS CORRIDOS,
                                                        --ESTE FLAG INDICA ESTE TIPO DE CALCULO.
                @ATV_CODIGO             CHAR(15),

         @POS_DIA_CENARIO INT,
                @ACUMULAR_CDI           CHAR(01),
                @INFLACAO_IMPLICITA     FLOAT,
                @SGL_MEDA_AJUSTADO      CHAR(15),
                @SGL_MEDA_CLASSE        CHAR(15),
                @CENARIO_CLASSE         FLOAT,
                @PREMIO                 FLOAT,
                @DATA_COMPRA            DATETIME,
                @PU_COMPRA              FLOAT,
                @CORRECAO_U_AUX         FLOAT,
                -- Liana - 22/06/2011
                @DT_ANIV_ANT            DATETIME,
                @DT_ANIV_PROX           DATETIME,
                @DT_PROX_PAGTO_EVE      DATETIME,
                @DT_CORRECAO_U          DATETIME,
                @DT_CORRECAO_C          DATETIME,
                @DT_INI_C               DATETIME,
                @DT_FIM_C               DATETIME,
                @TIPO_ATU_CETIP_MENSAL          SMALLINT,
                @TIPO_ATU_CETIP_ANUAL_EMIS      SMALLINT,
                @TIPO_ATU_CETIP_ANUAL_VENCTO    SMALLINT,
                @PCH_PAGA_CORRECAO      CHAR(01),
                @VIT_PRZ_UTEIS FLOAT,
                @VIT_PRZ_CORRIDOS FLOAT,
                @DT_EVE_ANT DATETIME,
                @RFX_FALC_CORRIDOS CHAR(03)
      , @VCH_FC_DT_ANV        VARCHAR(3)
      , @VIT_Q_DEFS_IDXR      INT       
      , @VDT_DATA_INI            DATETIME
      , @VDT_DATA_FIM            DATETIME
      , @EH_CUPONADA            VARCHAR(1)    = 'N'

        -- ASSOCIA OS VALORES DAS CONSTANTES
        SELECT
                @FLOAT0         = CONVERT(FLOAT, 0),
                @FLOAT1         = CONVERT(FLOAT, 1),
                @FLOAT100       = CONVERT(FLOAT, 100),
                @FLOAT252       = CONVERT(FLOAT, 252),
                @FLOAT360       = CONVERT(FLOAT, 360),
                @EVE_JURS_AMTC  = '1',  -- JUROS SOBRE AMORTIZACAO
                @EVE_JURS_PCPL  = '2',  -- JUROS SOBRE PRINCIPAL
                @EVE_CORC_PCPL  = '3',  -- ATUALIZACAO SOBRE PRINCIPAL
                @EVE_AMTC_PCPL  = '4',  -- AMORTIZACAO DO PRINCIPAL
                @EVE_CORC_AMTC  = '5',  -- ATUALIZACAO SOBRE AMORTIZACAO
                @EVE_JURS_DTBS  = '6',  -- JUROS SOBRE PRINCIPAL CORRIG. DESDE A DATABASE - DESENV PROJ. CRI-LH
                @CD_CE          = CONVERT(INT, @RF_CARACTERISTICA),
                @DT_UTIL        = @DT_CALC,
                @DT_UTIL_ANT    = @DT_CALC,
                @DT_VCMT_UTIL   = @TIT_VENCTO,
                @DT20000801     = CONVERT(DATETIME, '20000801'),
                @FL_ARRED       = 'N',
                @ARREDONDA      = 0,
                @TRUNCA         = 1,
                @TIPO_ATU_CETIP = 0,
                @VAR_CORRECAO   = 0,
                @UTILIZ_PREVIA  = 'N',
                @ATU_FLUXOS_FUT = 'N',
                @TIPO_RESERVA   = '001',
                @TIPO_COTACAO_C = 'C',
                -- Liana - 22/06/2011
                @TIPO_ATU_CETIP_MENSAL = 1,
                @TIPO_ATU_CETIP_ANUAL_EMIS = 2,
                @TIPO_ATU_CETIP_ANUAL_VENCTO = 3,
                @PCH_PAGA_CORRECAO = 'N'


    SELECT @EH_FUNDO = 'S'

    IF NOT EXISTS (SELECT 1 FROM POSICAO WHERE POS_APELIDO = @FER_CHAVE)
    BEGIN
        SELECT @FER_CHAVE = 'SAFRABM'
    END 


    -- ----------------------------------------------------------------------
    -- ESSA MESMA REGRA ESTA EM 3 LUGARES DIFERENTES... MANTER SEMPRE
    -- RF5775.PRC
    -- RF3179.PRC
    -- RF0884.PRC
    -- ----------------------------------------------------------------------
    SELECT @EH_CUPONADA = 'N'
    IF NOT EXISTS (SELECT 1 FROM SANT515_GE_PARAMETRO WHERE CH_PARM = 'VALZ.CUPOM' AND CN_PARM = 'N')
    BEGIN
        IF EXISTS (
            SELECT RFT.TITULO_ID
            FROM RF_TITULO RFT
            JOIN SANT643_RF_REPAC_TITULO T643
              ON T643.TITULO_ID = RFT.TITULO_ID
              AND T643.ID_RPAC = (SELECT MAX(ID_RPAC) FROM SANT643_RF_REPAC_TITULO 
                         WHERE TITULO_ID = T643.TITULO_ID 
                                    AND DT_INIC_RPAC <= @DT_CALC
                                    AND DT_FIM_RPAC > @DT_CALC)
              AND ISNULL(T643.V_TXA_RPAC, 0) <> 0  -- QUE TENHA CUPOM
            JOIN SANT644_RF_AGENDA_EVENTO T644
              ON T644.TITULO_ID = T643.TITULO_ID
              AND T644.ID_RPAC = T643.ID_RPAC
              AND T644.ID_T_EVE IN (1,2)  -- SOMENTE PAGAMENTO DE JUROS (SANT709_RF_TIPO_EVENTO)
            WHERE RFT.TITULO_ID = @TITULO_ID
              AND RFT.PFPJ_EMITENTE  IN (SELECT PFPJ_APELIDO FROM EMPRESA)
            GROUP BY RFT.TITULO_ID
            HAVING MIN(DT_EVE) <> MAX(DT_EVE)
        )
        BEGIN
            SELECT @EH_CUPONADA = 'S'
        END
    END



        CREATE TABLE #DURATION
        (       
                DATA                    DATETIME,
                PRAZO                   INT,
                VALOR_FUTURO            FLOAT,
                VALOR_EXATO             FLOAT, --(FATOR CENARIO)
                CEN_PRE                 FLOAT,
                CENARIO_CLASSE          FLOAT,
                CEN_PRE_AJUSTADO        FLOAT,
                CP_IND_PRECO            FLOAT,
                CP_IND_PRECO_AJUST      FLOAT,
                VALOR_SUBORDINADO       FLOAT
        )


        IF @TIT_VENCTO = @DT_CALC 
        BEGIN
                SELECT @ACUMULAR_CDI = 'S'
        END 
        ELSE
        BEGIN
                SELECT @ACUMULAR_CDI = 'N'
        END

        -- BUSCA INFORMACOES PARA VALORIZACAO DE CRI-LH
        SELECT  @FL_ARRED        = B.IC_ARRD_CTP,          -- UTILIZA METODOLOGIA DO CADERNO CETIP
                @TIPO_ATU_CETIP  = A.ID_T_ATC_NOML,        -- TIPO DE ATUALIZACAO CETIP (1 = MENSAL/2 = ANUAL BASE EMISSAO/3 = ANUAL BASE VENC)
                @UTILIZ_PREVIA   = B.IC_UTLC_PREV_IDXR,    -- UTILIZA PREVIA INDEXADOR
                @ATU_FLUXOS_FUT  = B.IC_ATC_IDXR_FLUX_FUT, -- UTILIZA INDEXADOR ATUAL PARA ATUALIZAR OS CICLOS ANUAIS FUTUROS
                ----------------------------------------------------------------------------------------------------
                @CUPOM_TIR_CASAS = B.Q_DEC_FATR_JURS,      -- BUSCA PRECISAO DE CASAS E SE ARREDONDA OU TRUNCA: 
                @CUPOM_TIR_ARRED = CASE B.IC_TRUNC_DEC_FATR_JURS 
                                   WHEN 'S' THEN @TRUNCA
                                   ELSE @ARREDONDA END,
                @VALORES_CASAS   = B.Q_DEC_VLS,
                @VALORES_ARRED   = CASE B.IC_TRUNC_DEC_VLS
                                   WHEN 'S' THEN @TRUNCA
                                   ELSE @ARREDONDA END,
                @CORRECAO_CASAS  = B.Q_DEC_FATR_CORC,
                @CORRECAO_ARRED  = CASE B.IC_TRUNC_DEC_FATR_CORC
                                   WHEN 'S' THEN @TRUNCA
                                   ELSE @ARREDONDA END,
                @DESCAP_CASAS    = B.Q_DEC_FATR_DCAZC,
                @DESCAP_ARRED    = CASE B.IC_TRUNC_DEC_FATR_DCAZC
                                   WHEN 'S' THEN @TRUNCA
                                   ELSE @ARREDONDA END,
                @VALOR_PRS_CASAS = B.Q_DEC_V_PRST,
                @VALOR_PRS_ARRED = CASE B.IC_TRUNC_DEC_V_PRST
                                   WHEN 'S' THEN @TRUNCA
                                   ELSE @ARREDONDA END,
                @PU_CASAS        = B.Q_DEC_PU,
                @PU_ARRED        = CASE B.IC_TRUNC_DEC_PU
                                   WHEN 'S' THEN @TRUNCA
                                   ELSE @ARREDONDA END,
                ----------------------------------------------------------------------------------------------------
                @ATV_CODIGO     =  A.ATV_CODIGO
        FROM    RF_TITULO A,
                CODIGO_TITULO B
        WHERE   A.ATV_CODIGO = B.ATV_CODIGO
        AND     A.IDX_CODIGO = B.IDX_CODIGO
        AND    A.LOCAL_NEGOCIACAO = B.LOCAL_NEGOCIACAO
        AND     B.IC_ARRD_CTP = 'S'
        AND     TITULO_ID = @TITULO_ID
        AND     CD_TIT_ART IN (6000, 6500)  

        -- Liana - 22/06/2011
        SELECT  @TIPO_ATU_CETIP  = ID_T_ATC_NOML           -- TIPO DE ATUALIZACAO CETIP (1 = MENSAL/2 = ANUAL BASE EMISSAO/3 = ANUAL BASE VENC)
        FROM RF_TITULO 
        WHERE TITULO_ID = @TITULO_ID

-- if @@spid = 80 select 263 'rf0884', @TITULO_ID TITULO_ID

        -- PARA CRI E LH COM INDEXADOR TR UTILIZAMOS O TIPO 'X' (PRORATA CORRIDOS) PARA O CONTABIL 
        IF @FL_ARRED = 'S' AND @IDX_CODIGO = 'TR' AND @FL_IDX_CONTABIL = 'S'
                SELECT  @TIPO_COTACAO_C = 'X'

        -- BUSCA A FORMA DE CALCULO DO INDEXADOR
        SELECT
                @IDX_FCALC      = IDX_FCALC,
                @IDX_PRE        = IDX_PRE
        FROM
                INDEXADOR
        WHERE
                IDX_CODIGO      = @IDX_CODIGO


        -- Liana - 03/08/2010
        IF @CETIP_SELIC = 'C' AND @IDX_FCALC = '007'
        BEGIN
                SELECT  @UTILIZ_PREVIA = 'S'
        END

        -- INICIALIZA AS VARIAVEIS

        SELECT
                @ERRO                   = -1,
                @ERR_MSG                = '',
                @CORRECAO_U             = @FLOAT0,
                @CORRECAO_C             = @FLOAT0,
                @EVE_CORRENTE           = '',
                @EVE_POR_DATA           = '',
                @ID_SEQ_EVE_COR         = @FLOAT0,
                @ID_SEQ_EVE_JUR         = @FLOAT0,
                @CUPOM_TIR              = @FLOAT0,
                @V_FUT_U_JURS           = @FLOAT0,
                @V_FUT_C_JURS           = @FLOAT0,
                @V_FUT_M_JURS           = @FLOAT0,
                @V_FUT_U_CORC           = @FLOAT0,
                @V_FUT_C_CORC           = @FLOAT0,
                @V_FUT_M_CORC           = @FLOAT0,
                @V_FUT_U_PCPL           = @FLOAT0,
                @V_FUT_C_PCPL           = @FLOAT0,
                @V_FUT_M_PCPL           = @FLOAT0,
                @V_PRS_U_JURS           = @FLOAT0,
                @V_PRS_C_JURS           = @FLOAT0,
                @V_PRS_M_JURS           = @FLOAT0,
                @V_PRS_U_CORC           = @FLOAT0,
                @V_PRS_C_CORC           = @FLOAT0,
                @V_PRS_M_CORC           = @FLOAT0,
                @V_PRS_U_PCPL           = @FLOAT0,
                @V_PRS_C_PCPL           = @FLOAT0,
                @V_PRS_M_PCPL           = @FLOAT0,
                @VAR_DESCAP_U           = @FLOAT0,
                @VAR_DESCAP_C           = @FLOAT0,
                @VAR_DESCAP_M           = @FLOAT0,
                @ID_NUM_PARC            = @FLOAT0,
                @Q_DIAS_PARC            = @FLOAT0,
                @V_PU_CALD_JURS         = @FLOAT0,
                @V_PU_CALD_CORC         = @FLOAT0,
                @V_PU_CALD_PCPL         = @FLOAT0,

                @SGL_FCALC              = '',
                @SGL_BASE               = '', 
                @SGL_MEDA_BASE          = '',
                @IC_APC                 = '',
                @TX_PRE                 = @FLOAT0,
                @TX_EMISSAO             = @FLOAT0,
                @TX_PRE_EMIS            = @FLOAT0,
                @TX_PRE_MTM             = @FLOAT0,

                @V_PZ_MDIO_CALD         = @FLOAT0,
                @V_DURT_CALD            = @FLOAT0,
                @CUPOM_PZ_MDIO          = @FLOAT0,
                @CORC_PZ_MDIO           = @FLOAT0,
                @V_FUT_PZ_MDIO          = @FLOAT0,
                @V_FUT_TOTL             = @FLOAT0


        --INICIALIZA VARIAVEIS APC
        SELECT 
                @SGL_FCALC      = A.IDX_FCALC,
                @SGL_B_EXPS     = A.SGL_B_EXPS,
                @IC_APC         = ISNULL(A.IC_APC,'S')
        FROM 
                SANT269_RF_CENARIO_MOS A,
                SANT447_GE_CEN_SAN_APC B
        WHERE 
        A.SGL_MEDA      = B.SGL_MEDA    AND
                B.IDX_CODIGO    = @SGL_MEDA

--      SELECT @IC_APC IC_APC, @SGL_MEDA SGL_MEDA
        
        IF @SGL_FCALC = '007' AND @EH_FUNDO = 'S' 
                SELECT @SGL_FCALC = '001'

        
        SELECT
                @TX_EMISSAO     =  ISNULL(RFX_PERC, 0)
        FROM
                RF_INDEXACAO
        WHERE
                RF_CARACTERISTICA = @RF_CARACTERISTICA



        -- CALCULA O DIA UTIL ANTERIOR A DATA DE CALCULO

        EXEC SIAN_SP_RESERVA_ANTERIOR   @FER_CHAVE,
                                        'A',
                                        @DT_UTIL_ANT    OUTPUT,
                                        0



        -- CASO NAO SEJA DIA UTIL, O CALCULO EH SIMILAR AO DIA ANTERIOR

        EXEC @EH_RESERVA = SIAN_E_RESERVA       @DT_UTIL,
                                                'A',
                                                @FER_CHAVE,
                                                0

        IF @EH_RESERVA <> -1    -- NAO EH DIA UTIL

                SELECT
                        @DT_UTIL        = @DT_UTIL_ANT



        -- BUSCA A DATA UTIL DE VENCIMENTO

        EXEC @EH_RESERVA = SIAN_E_RESERVA       @DT_VCMT_UTIL,
                                                'A',
                                                @FER_CHAVE,
                                                0

        IF @EH_RESERVA <> -1 AND @IDX_FCALC <> '007' AND @RFX_FCALC = '022'     -- NAO EH DIA UTIL

                EXEC SIAN_SP_PROXIMA_RESERVA    'A',
                                                @FER_CHAVE,
                                                @DT_VCMT_UTIL   OUTPUT,
                                                0



        -- LIMPA A TABELA DA TIR

        DELETE FROM SANT646_RF_TIR WHERE ID_USR = @@SPID


  DELETE FROM SANT651_RF_SALDOS_EVENTO
  WHERE RF_CARACTERISTICA = @RF_CARACTERISTICA
                AND RFS_DATA          = @DT_CALC
    AND CTB_CURVA         = @CTB_CURVA

        -- CALCULA OS PUs
        
        IF @RFX_RESULTADO <> '006'
        BEGIN

            IF @IC_OPRC_NAO_PADR = 'S'
            BEGIN

                SELECT
                        @EVE_CORRENTE           = '',
                        @DT_EVE                 = @DT_UTIL,
                        @V_NOML_PCPL            = @FLOAT0,
                        @V_NOML_AMTC            = @FLOAT0,
                        @ID_SEQ_EVE_COR         = @FLOAT0,
                        @ID_SEQ_EVE_JUR         = @FLOAT0,
                        @FL_PAGA_EVE            = 'N',
                        @IC_PZ_MDIO_RPAC        = 'S',
                        @DT_PG_EFT              = @DT_CALC



                -- BUSCA A MAIOR REPACTUACAO

                SELECT
                        @MAX_RPAC = MAX(ID_RPAC)
                FROM
                        SANT643_RF_REPAC_TITULO
                WHERE
                        TITULO_ID       =  @TITULO_ID



                -- FLAG PARA CALCULAR O PRAZO MEDIO UTILIZANDO REPACTUACAO OU VENCIMENTO
                -- BUSCA A DATA BASE DE CALCULO PARA ATUALIZACAO DOS JUROS

                SELECT
                        @IC_PZ_MDIO_RPAC = ISNULL(IC_PZ_MDIO_RPAC, 'S'),
                        @TX_EMISSAO      = ISNULL(IDX_PC, 100),
                        @IC_EVE_CORC_TIT = ISNULL(IC_EVE_CORC_TIT, 'N'),
                        @DT_B_JURS = ISNULL(DT_B_JURS, @RF_BASE_CALC)
          , @VCH_FC_DT_ANV        = ISNULL(FC_DT_ANV, '001')    -- SELECT * FROM FORMA_CALCULO WHERE CAL_TIPO = '57'
          , @VIT_Q_DEFS_IDXR      = ISNULL(Q_DEFS_IDXR, 0)
                FROM
                        SANT643_RF_REPAC_TITULO
                WHERE
                        TITULO_ID       =  @TITULO_ID   AND
                        ID_RPAC         =  @ID_RPAC
                

                -- FLAG PARA SABER SE EXISTE FLUXO A SER PAGO NA DATA
                -- *** Cleber - 11/08/2006 - pgto de eventos em dias nao uteis para tit. com base dias corridos                 
                IF      (@DT_CALC = @DT_UTIL 
                        OR (@FL_ARRED = 'S' AND (@RFX_FCALC = '001' OR @RFX_FCALC = '002' OR @RFX_FCALC = '032' OR @ATV_CODIGO = 'L H')))-- *** Cleber - 11/08/2006
                        AND @RF_CARACTERISTICA <> ''
                        SELECT
                                @FL_PAGA_EVE    = 'S',
                                @DT_PG_EFT      = DT_EVE
                        FROM
                                SANT644_RF_AGENDA_EVENTO
                        WHERE
                                TITULO_ID       =  @TITULO_ID   AND
                                ID_RPAC         =  @ID_RPAC     AND
                                DT_EVE          >  @DT_UTIL_ANT AND
                                DT_EVE          <= @DT_CALC


                -- SE EXISTIR FLUXO A DATA DE EVENTO EH A DATA UTIL ANTERIOR PARA CONSIDERAR ESTE FLUXO
                IF @FL_PAGA_EVE = 'S'
                        SELECT
                                @DT_EVE         = @DT_UTIL_ANT

                ---------------------------------------------------------------------------------
                -- VERIFICA SE O TITULO TEM EVENTOS APENAS NO VENCIMENTO                       --
                -- UTILIZADO PARA TRATAR DIFERENCA NA TIR 252/360                              --
                ---------------------------------------------------------------------------------
                IF NOT EXISTS (
                        SELECT
                                *
                        FROM
                                SANT644_RF_AGENDA_EVENTO
                        WHERE
                                TITULO_ID       = @TITULO_ID    AND
                                DT_EVE          < @TIT_VENCTO   ) AND @FL_ARRED = 'S'
                begin
                        select  @TIPO_RESERVA = '003'
                end


                -------------------------------------------------------------------------------------------
                -----------------------------------------------------------------------------------
                -- LIANA - 22/06/2011
                -- Se Tipo de Atualização igual a "ANUAL", calcula a Data de Atualização do Indexador,
                -- a Data do Último Aniversário e a Data do Próximo Aniversário do Papel.
                -----------------------------------------------------------------------------------
--              select @TIPO_ATU_CETIP '@TIPO_ATU_CETIP'
        
                IF @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS
                BEGIN
                        SELECT @DT_CORRECAO_C = @DT_CALC

                        -- CALCULA A DATA DO ÚLTIMO ANIVERSÁRIO
                        EXEC SANPP_RF_CALC_ANIVERSARIO_ANTERIOR @TIPO_ATU_CETIP, @DT_CORRECAO_C, @TIT_VENCTO, @TIT_EMISSAO, '-1', @DT_ANIV_ANT OUTPUT
                        
--                      select @DT_ANIV_ANT '@DT_ANIV_ANT'
        
                        IF NOT EXISTS(SELECT DT_EVE
                                        FROM SANT644_RF_AGENDA_EVENTO
                                        WHERE 
                                        TITULO_ID = @TITULO_ID
                                        and DT_EVE >= @DT_ANIV_ANT
                                        AND DT_EVE < @DT_CALC
                                        AND ID_RPAC =  @ID_RPAC
                                        and ID_T_EVE in (@EVE_CORC_PCPL, @EVE_CORC_AMTC)
--                                      AND V_PU_CALD > 0
                                        )
                                AND @DT_ANIV_ANT <> @TIT_EMISSAO
                        BEGIN

--                              print 'vai mudar data atu'
                                SELECT @DT_CORRECAO_C = @DT_ANIV_ANT

                   -- chama funcao de calculo do ultimo aniv de novo
                                EXEC SANPP_RF_CALC_ANIVERSARIO_ANTERIOR @TIPO_ATU_CETIP, @DT_CORRECAO_C, @TIT_VENCTO, @TIT_EMISSAO, '-1', @DT_ANIV_ANT OUTPUT
                        END

                        SELECT @DT_CORRECAO_U = @DT_CORRECAO_C

                        EXEC @EH_RESERVA = SIAN_E_RESERVA       @DT_CORRECAO_U,
                                                                'A',
                                                                @FER_CHAVE,
                                                                0

                        IF @EH_RESERVA <> -1    -- NAO EH DIA UTIL

                                EXEC SIAN_SP_PROXIMA_RESERVA    'A',
                                                                @FER_CHAVE,
                                                                @DT_CORRECAO_U  OUTPUT,
                                                                0

                        -- CALCULA A DATA DO PRÓXIMO ANIVERSÁRIO
                        EXEC SANPP_RF_CALC_PROXIMO_ANIVERSARIO @TIPO_ATU_CETIP, @DT_CORRECAO_C, @TIT_VENCTO, @TIT_EMISSAO, '-1', @DT_ANIV_PROX OUTPUT                   

--                      SELECT @DT_ANIV_ANT '@DT_ANIV_ANT', @DT_ANIV_PROX '@DT_ANIV_PROX', @DT_CORRECAO_C '@DT_CORRECAO_C', @DT_CORRECAO_U '@DT_CORRECAO_U'
                END

                -------------------------------------------------------------------------------------------

                WHILE EXISTS (
                        SELECT
                                *
                        FROM
                                SANT644_RF_AGENDA_EVENTO
                        WHERE
                                TITULO_ID       = @TITULO_ID    AND
                                ID_RPAC         = @ID_RPAC      AND
                                DT_EVE          > @DT_EVE
                        )
                BEGIN           -- INICIO LOOP EVENTOS

                        SELECT
                                @EVE_CORRENTE   = '',
                                @V_FUT_U_JURS   = @FLOAT0,
                                @V_FUT_C_JURS   = @FLOAT0,
                                @V_FUT_M_JURS   = @FLOAT0,
                                @V_FUT_U_CORC   = @FLOAT0,
                                @V_FUT_C_CORC   = @FLOAT0,
                                @V_FUT_M_CORC   = @FLOAT0,
                                @V_FUT_U_PCPL   = @FLOAT0,
                                @V_FUT_C_PCPL   = @FLOAT0,
                                @V_FUT_M_PCPL   = @FLOAT0,
                                @V_PRS_U_JURS   = @FLOAT0,
                                @V_PRS_C_JURS   = @FLOAT0,
                                @V_PRS_M_JURS   = @FLOAT0,
                                @V_PRS_U_CORC   = @FLOAT0,
                                @V_PRS_C_CORC   = @FLOAT0,
                                @V_PRS_M_CORC   = @FLOAT0,
                                @V_PRS_U_PCPL   = @FLOAT0,
                                @V_PRS_C_PCPL   = @FLOAT0,
                                @V_PRS_M_PCPL   = @FLOAT0,
                                @V_NOML_PCPL    = @FLOAT0,
                                @V_NOML_AMTC    = @FLOAT0,
                                @CUPOM_TIR      = @TIT_CUPOM,
                                @CUPOM_PZ_MDIO  = @TIT_CUPOM,
                                @CORC_PZ_MDIO   = @FLOAT0,
                                @V_FUT_PZ_MDIO  = @FLOAT0,
                                @ID_T_EVE       = '0',
                                @EVE_POR_DATA   = '',
                                @CEN_DATA_BASE  = @DT_UTIL,
                                @Q_DIAS_PARC    = @FLOAT0,
                                @V_PU_CALD_JURS = @FLOAT0,
                                @V_PU_CALD_CORC = @FLOAT0,
                                @V_PU_CALD_PCPL = @FLOAT0,
@Q_DC_PZ_MEDIO  = @FLOAT0,
                                @Q_DU_DURATION  = @FLOAT0,
                                @DT_2_RPAC      = NULL



                        -- SE FOR MUDANCA DE PERIODO BUSCAR O CUPOM PARA O CALCULO DO PRAZO MEDIO

                        IF @DT_CALC = @DT_PROX_RPAC

                                SELECT
                                        @CUPOM_PZ_MDIO = V_TXA_RPAC
                                FROM
                                        SANT643_RF_REPAC_TITULO
                                WHERE
                                        TITULO_ID       = @TITULO_ID    AND
                                        ID_RPAC         = @ID_RPAC + 1

                        -- BUSCA O NOMINAL DEVIDO

                        SELECT
                                @V_NOML_PCPL = SUM(V_PU_AMTC)
                        FROM
                                SANT644_RF_AGENDA_EVENTO
                        WHERE
                                TITULO_ID       = @TITULO_ID            AND
                                ID_RPAC         = @ID_RPAC              AND
                                ID_T_EVE        = @EVE_AMTC_PCPL        AND
                                DT_EVE          > @DT_EVE

                        -- DATA DO EVENTO ANTERIOR

            SELECT @DT_EVE_ANT = @DT_EVE


                        -- DATA DO PROXIMO EVENTO

                        SELECT
                                @DT_EVE         = MIN(DT_EVE),
                                @DT_UTIL_EVE    = MIN(DT_EVE)
                        FROM
                                SANT644_RF_AGENDA_EVENTO
                        WHERE
                                TITULO_ID       = @TITULO_ID    AND
                                ID_RPAC         = @ID_RPAC      AND
                                DT_EVE          > @DT_EVE



                        -- BUSCA A DATA DE LIQUIDACAO DO EVENTO

                        EXEC @EH_RESERVA = SIAN_E_RESERVA       @DT_UTIL_EVE,
                                                                'A',
                                                                @FER_CHAVE,
                                                                0

                        IF @EH_RESERVA <> -1    -- NAO EH DIA UTIL

                                EXEC SIAN_SP_PROXIMA_RESERVA    'A',
                                                                @FER_CHAVE,
                                                                @DT_UTIL_EVE    OUTPUT,
                                                                0



                        -- SE O EVENTO VENCER APOS A DATA DE REPACTUACAO, CALCULAR ATE A DATA DE REPACTUACAO

                        IF @DT_EVE > @DT_PROX_RPAC
                                SELECT
                                        @DT_FNAL_EVE    = @DT_PROX_RPAC
                        ELSE
                                SELECT
                                        @DT_FNAL_EVE    = @DT_EVE

                        -- CALCULA A QUANTIDADE DE DIAS ATE A DATA DO EVENTO
                        EXEC @Q_DIAS_U_PARC = SIAN_SP_QUANTAS_RESERVAS  @DT_UTIL,
                                                                        @DT_FNAL_EVE,
                                                                        @TIPO_RESERVA,
                                                                        'A',
                                                                        @FER_CHAVE,
                                                                        0

                        -- BUSCA O NOMINAL DE AMORTIZACAO NA DATA

                        SELECT
                                @V_NOML_AMTC = SUM(V_PU_AMTC)
                        FROM
                                SANT644_RF_AGENDA_EVENTO
                        WHERE
                                TITULO_ID       = @TITULO_ID            AND
                          ID_RPAC         = @ID_RPAC              AND
                                ID_T_EVE        = @EVE_AMTC_PCPL        AND
                                DT_EVE          = @DT_EVE



                        -- FLUXOS DE JUROS

                        IF EXISTS (
                                SELECT
                                        *
                                FROM
                                        SANT644_RF_AGENDA_EVENTO
                                WHERE
                                        TITULO_ID       =  @TITULO_ID                                           AND
                                        ID_RPAC         =  @ID_RPAC                                             AND
                                        ID_T_EVE        IN (@EVE_JURS_AMTC,@EVE_JURS_PCPL,@EVE_JURS_DTBS)       AND
                                        DT_EVE          =  @DT_EVE)
                        BEGIN


                                SELECT
                                        @ID_SEQ_EVE_JUR = @ID_SEQ_EVE_JUR + 1



                                -- BUSCA O EVENTO CORRENTE

                                SELECT
                                        @EVE_CORRENTE   = ID_T_EVE
                                FROM
                                        SANT644_RF_AGENDA_EVENTO
                                WHERE
                                        TITULO_ID       =  @TITULO_ID                                           AND
                                        ID_RPAC         =  @ID_RPAC                                             AND
                                        ID_T_EVE        IN (@EVE_JURS_AMTC, @EVE_JURS_PCPL,@EVE_JURS_DTBS)      AND
                                        DT_EVE          =  @DT_EVE


                                SELECT
                                        @EVE_POR_DATA = RTRIM(@EVE_POR_DATA) + @EVE_CORRENTE


                                -- BUSCA A DATA INICIAL PARA O CALCULO DA VARIACAO DO CUPOM

                                IF @EVE_CORRENTE = @EVE_JURS_PCPL OR @EVE_CORRENTE = @EVE_JURS_DTBS
                                BEGIN

                                        -- PARA EVENTO DE JUROS SOBRE PRINCIPAL, CARECA O JUROS A CADA PAGAMENTO

                                        SELECT
                                                @DT_INI = MAX(DT_EVE)
                                        FROM
                                                SANT644_RF_AGENDA_EVENTO
                                        WHERE
                                                TITULO_ID       = @TITULO_ID            AND
                                                ID_RPAC         = @ID_RPAC              AND
                                                (ID_T_EVE       = @EVE_JURS_PCPL        OR
                                                 ID_T_EVE       = @EVE_JURS_DTBS)       AND
                                                DT_EVE          < @DT_EVE


                                        IF @IC_EVE_CORC_TIT = 'S' AND @ID_SEQ_EVE_JUR = 1 AND @DT_INI IS NULL

                                                SELECT
                                                        @DT_INI = @DT_B_JURS

                                END
                                ELSE

                                        -- PARA EVENTO DE JUROS SOBRE AMORTIZACAO, O CALCULO COMECA NA DATA BASE DE CALCULO

                                        SELECT
                                                @DT_INI = @DT_B_JURS

                                IF @DT_INI IS NULL
                                        SELECT
                                                @DT_INI = @DT_B_JURS

                                EXEC @EH_RESERVA = SIAN_E_RESERVA       @DT_INI,
                                                                        'A',
                                        @FER_CHAVE,
                                                                        0

                                IF @EH_RESERVA <> -1    -- NAO EH DIA UTIL
                                   AND NOT (@IDX_FCALC = '007' AND @CETIP_SELIC = 'S') AND @RFX_FCALC = '022'

                                        EXEC SIAN_SP_PROXIMA_RESERVA    'A',
                                                                        @FER_CHAVE,
                                                                        @DT_INI         OUTPUT,
                                                                        0



                                -- BUSCA A DATA FINAL PARA O CALCULO DA VARIACAO DO CUPOM

                                SELECT
                                        @DT_FIM = @DT_FNAL_EVE


                                IF @IDX_FCALC <> '007' AND @RFX_FCALC = '022'
                                BEGIN

                                    EXEC @EH_RESERVA = SIAN_E_RESERVA   @DT_FIM,
                                                                        'A',
                                                                        @FER_CHAVE,
                                                                        0

                                    IF @EH_RESERVA <> -1        -- NAO EH DIA UTIL

                                        EXEC SIAN_SP_PROXIMA_RESERVA    'A',
                                                                        @FER_CHAVE,
                                                                        @DT_FIM         OUTPUT,
                                                                        0
                                END



                                -- VARIACAO DO CUPOM

                                -- RF0887.PRC

                                EXEC SANPP_RF_CALC_CUPOM_TIR            @TIT_EMISSAO, 
                                                                        @DT_VCMT_UTIL,
                                                                        @DT_INI,
                                                                        @DT_FIM,
                                                                        @FER_CHAVE,
                                                                        @FCALC_CUPOM,
                                                                        @TIT_CUPOM,
                                                                        @RFX_FCALC,
                                                                        @TIT_EMISSAO,
                                                                        @ATV_PRZ_PGJUROS,
                                                                        @CUPOM_TIR      OUTPUT,
                                                                        @TIPO_RESERVA


                                IF @RFX_FCALC = '022' 
                                        SELECT  @RFX_FALC_CORRIDOS = '001'
                                ELSE
                                        SELECT @RFX_FALC_CORRIDOS = @RFX_FCALC
                                -- CALCULO CORRIDOS
                                EXEC SANPP_RF_CALC_CUPOM_TIR            @TIT_EMISSAO, 
                                                                        @DT_VCMT_UTIL,
                                                                        @DT_INI,
                                                                        @DT_FIM,
                                                                        @FER_CHAVE,
                                                                        @RFX_FALC_CORRIDOS,
                                                                        @TIT_CUPOM,
                                                                        @RFX_FALC_CORRIDOS,
                     @TIT_EMISSAO,
                           @ATV_PRZ_PGJUROS,
                                                                        @CUPOM_TIR_C    OUTPUT,
                                                                        @TIPO_RESERVA

                                -- O PRAZO MEDIO EH CALCULADO CONSIDERANDO O PARAMETRO

                                IF @DT_FNAL_EVE <> @DT_EVE AND @IC_PZ_MDIO_RPAC = 'N'
                                        BEGIN

                                        -- BUSCA A DATA FINAL PARA O CALCULO DA VARIACAO DO CUPOM

                                        SELECT
                                                @DT_FIM = @DT_EVE

                                        IF @IDX_FCALC <> '007'
                                        BEGIN

                                                EXEC @EH_RESERVA = SIAN_E_RESERVA       @DT_FIM,
                                                                                        'A',
                                                                                        @FER_CHAVE,
                                                                                        0

                                                IF @EH_RESERVA <> -1    -- NAO EH DIA UTIL

                                                        EXEC SIAN_SP_PROXIMA_RESERVA    'A',
                                                                                        @FER_CHAVE,
                                                                                        @DT_FIM         OUTPUT,
                                                                                        0
                                        END


                                        -- VARIACAO DO CUPOM

                                        -- RF0887.PRC

                                        EXEC SANPP_RF_CALC_CUPOM_TIR    @TIT_EMISSAO,
                                                                        @DT_VCMT_UTIL,
                                                                        @DT_INI,
                                                                        @DT_FIM,
                                                                        @FER_CHAVE,
                                                                        @FCALC_CUPOM,
                                                                        @CUPOM_PZ_MDIO,
                                                                        @RFX_FCALC,
                                                                        @TIT_EMISSAO,
                                                                        @ATV_PRZ_PGJUROS,
                                                                        @CUPOM_PZ_MDIO  OUTPUT,
                                                                        @TIPO_RESERVA

                                END
                                ELSE
                                BEGIN

                                        SELECT
                                                @CUPOM_PZ_MDIO  = @CUPOM_TIR

                                END

                                -- LIANA - 22/06/2011 - VERIFICA SE O EVENTO PAGA CORRECAO
                                EXEC SANPP_RF_VERIFICA_PAGTO_CORRECAO @TIPO_ATU_CETIP, @DT_EVE, @TITULO_ID, @ID_RPAC, @PCH_PAGA_CORRECAO OUTPUT

--                              select @PCH_PAGA_CORRECAO '@PCH_PAGA_CORRECAO', @DT_EVE '@DT_EVE'

                                -----------------------------------------------------------------------------------------
                                -- CALCULA A ATUALIZACAO SOBRE OS JUROS
                                -----------------------------------------------------------------------------------------
                                IF (@EVE_CORRENTE = @EVE_JURS_DTBS AND @IDX_CODIGO <> 'PRE') OR --JUROS SOBRE PRINCIPAL CORRIG. DESDE A DATABASE - DESENV PROJ. CRI-LH
                                 (@EVE_CORRENTE = @EVE_JURS_AMTC) OR
                                   (@EVE_CORRENTE = @EVE_JURS_PCPL 
                                        AND (((@ID_SEQ_EVE_COR < 1 and @TIPO_ATU_CETIP <> @TIPO_ATU_CETIP_ANUAL_EMIS) OR (@ID_SEQ_EVE_COR < 1 and @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS and @PCH_PAGA_CORRECAO = 'S')) -- Liana - 26/05/2011
                                                OR (@IDX_CODIGO <> 'PRE' AND @EH_FUNDO = 'S'))) OR -- SE AINDA NAO PAGOU A CORRECAO OU EH CDI
--                                 (@EVE_CORRENTE = @EVE_JURS_PCPL AND @IDX_CODIGO <> 'PRE' AND @FL_ARRED = 'S')
                                   (@EVE_CORRENTE = @EVE_JURS_PCPL AND @IDX_CODIGO <> 'PRE' 
                                        -- Liana - 22/06/2011 - Caso exista Correção sobre amortização na mesma data, o juros sobre principal deve ser calculado
                                        AND EXISTS(SELECT DT_EVE FROM SANT644_RF_AGENDA_EVENTO
                                                   WHERE TITULO_ID      = @TITULO_ID            AND
                                                         ID_RPAC                = @ID_RPAC              AND
                                                         ID_T_EVE       = @EVE_CORC_AMTC        AND
                                                         DT_EVE         = @DT_EVE)
                                        )
                                BEGIN

                                        SELECT
                                                @DT_BASE_CALC = NULL,
                                                @CORC_PZ_MDIO   = @FLOAT1,
                                                -- Liana - 22/06/2011
                                                @CORRECAO_U      = @FLOAT1,
                                                @CORRECAO_C      = @FLOAT1

-- if @@spid in (93, 105) select 1020 'rf0884', @DT_EVE DT_EVE, @CORRECAO_U CORRECAO_U

                                        -- SE POSSUIR BASE DE CALCULO SO UTILIZA ATE O PRIMEIRO PAGTO DE CORRECAO SOBRE O PRINCIPAL

                                        IF @EVE_CORRENTE = @EVE_JURS_PCPL -- OR @EVE_CORRENTE = @EVE_JURS_DTBS Cleber 23/10/2006
                                        BEGIN
                                                IF @TIPO_ATU_CETIP = 4
            BEGIN
                                                        SELECT
                                                                @DT_BASE_CALC = @TIT_EMISSAO
            END
                                                -- Liana - 22/06/2011 -  se for atualizacao anual e correcao sobre principal , a data de início deve ser o último aniversário do papel
                                                ELSE IF @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS
                                                AND EXISTS(SELECT DT_EVE FROM SANT644_RF_AGENDA_EVENTO
                                                           WHERE TITULO_ID      = @TITULO_ID            AND
                                                                 ID_RPAC                = @ID_RPAC              AND
                                                                 ID_T_EVE       = @EVE_CORC_PCPL        AND
                                                                 DT_EVE         = @DT_EVE)
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
                                                                TITULO_ID       = @TITULO_ID            AND
                                                                ID_RPAC         = @ID_RPAC              AND
                                                                ID_T_EVE        = @EVE_CORC_PCPL        AND
                                                                DT_EVE          < @DT_EVE
                                                END
                                        END

                                        -- PARA EVENTO DE JUROS SOBRE AMORTIZACAO, CALCULA A VARIACAO DE 
                                        -- FORMA DECRESCENTE POR ID_RPAC. ISTO PORQUE A CADA REPACTUACAO PODE-SE
                                        -- MUDAR O PERCENTUAL DE INDEXADOR PARA CDI. SE OCORREU UMA CORRECAO DO 
                                        -- TITULO, CONSIDERAR O PERCENTUAL CORRIGIDO.

                                        IF @EVE_CORRENTE = @EVE_JURS_AMTC AND @IDX_FCALC = '000' AND @ID_RPAC > 1 AND @IC_EVE_CORC_TIT = 'N'
                                                and (@TIPO_ATU_CETIP <> @TIPO_ATU_CETIP_ANUAL_EMIS OR @PCH_PAGA_CORRECAO = 'S') -- Liana - 22/06/2011
                                        BEGIN

                                                SELECT
                                                        @ID_RPAC_LOOP    = @ID_RPAC,
                                                        @CORRECAO_U      = @FLOAT1,
                                                        @CORRECAO_C      = @FLOAT1,
                                                        @DT_FIM_VAR_LOOP = @DT_UTIL,            -- DATA DE CALCULO
                                                        @DT_FIM_VAR_LOOP_C = @DT_UTIL,          -- Liana - 22/06/2011
                                                        @DT_INI_VAR_LOOP = @DT_INIC_RPAC,       -- DATA BASE DE CALCULO OU EMISSAO
                                                        @IDX_PC_LOOP     = @IDX_PC,             -- PERCENTUAL (CDI OU REF)
                                                        @IC_COR_TIT_LOOP = @IC_EVE_CORC_TIT     -- EVENTO DE CORRECAO DO TITULO

-- if @@spid in (93, 105) select 1076 'rf0884', @DT_EVE DT_EVE, @CORRECAO_U CORRECAO_U

                                                -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE DEVE SER A DO ANIVERSARIO DO PAPEL
                                                if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR < 1
                                                BEGIN
                                                        SELECT @DT_FIM_VAR_LOOP = @DT_CORRECAO_U
                                                        SELECT @DT_FIM_VAR_LOOP_C = @DT_CORRECAO_C
--                                                      select @DT_FIM_VAR_LOOP_C '@DT_FIM_VAR_LOOP_C', @DT_FIM_VAR_LOOP '@DT_FIM_VAR_LOOP'
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

        EXEC SANPP_RF_CALC_CDI_PROJETADO        @TITULO_ID,             -- IDENTIFICADOR DO TITULO
                                                                                                        @ID_RPAC_LOOP,          -- IDENTIFICADOR DO PERIODO DE REPACTUACAO
                                                                                                        @EVE_CORRENTE,          -- TIPO DO EVENTO A SER UTILIZADO NA PROJECAO
                                                                                                        @DT_FIM_VAR_LOOP,       -- DATA DE CALCULO DA PROJECAO
                                                                                                        @DT_EVE,                -- DATA DO EVENTO A PROJETAR O CDI
                                                                                                        @DT_INI_VAR_LOOP,       -- DATA BASE DE CALCULO
                                                                                                        @DT_PROX_RPAC,          -- DATA DA PROXIMA REPACTUACAO OU VENCIMENTO
                                                                                                        @IDX_CODIGO,            -- INDEXADOR
                                                                                                        @IDX_PC_LOOP,           -- PERCENTUAL DO INDEXADOR
                                                                                                        @FER_CHAVE,             -- POSICAO PARA FERIADOS
                                                                                                        @CORRECAO_LOOP  OUTPUT, -- CDI PROJETADO
                                                                                                        @ERRO           OUTPUT, -- RETORNA -1 => SEM ERROS
                                                                                                        @ERR_MSG        OUTPUT, -- MENSAGEM DE ERRO
                                                                                                        NULL                    -- QUANDO NAO NULO, RETORNA RECORDSET PARA VB


                                                                IF @ERRO <> -1
                                                                BEGIN

                                                                        IF @VB_SQL IS NOT NULL
                                                                        BEGIN
                                                                                SELECT
                                                                                        @TIR_U          TIR_U,
                                                                                        @TIR_C          TIR_C,
                                                                                        @ERRO           ERRO,
                                                                                        @ERR_MSG        ERR_MSG

                                                                                RETURN
                                                                        END
                                                                        ELSE
                                                                                RETURN

                                                                END


                                                                -- ACUMULA A CORRECAO

                                                                SELECT
                                                                        @CORRECAO_U = @CORRECAO_U * @CORRECAO_LOOP,
                                                                        @CORRECAO_C = @CORRECAO_C * @CORRECAO_LOOP


-- if @@spid in (93, 105) select 1145 'rf0884', @DT_EVE DT_EVE, @CORRECAO_U CORRECAO_U

    -- CALCULA A CORRECAO PARA CALCULO DO PRAZO MEDIO

                                                                SELECT
                                                                        @CORRECAO_LOOP   = @FLOAT1,
                                                                        @RFSC_VALOR_LOOP = @FLOAT0


                                                                IF @EVE_CORRENTE = @EVE_JURS_AMTC       OR
                                                                   (@EVE_CORRENTE = @EVE_JURS_PCPL AND @ID_SEQ_EVE_COR < 1)
                                                                BEGIN
                                                                        --PAÇOCA - 02/02/2010
                                                                        IF @DT_VLRZ_TERMO IS NOT NULL   
                                                                        BEGIN
                                                                                EXEC SIAN_SP_VARIACAO
                                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                                @DT_FIM_VAR_LOOP,               -- DATA DE CALCULO
                                                                                                @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                                @IDX_PC_LOOP,                   -- PERCENTUAL (CDI OU REF)
                                                                                                @FLOAT0,                        -- 0.0
                                                                                                @FLOAT0,                        -- 0.0
                                                                                                '000',                          -- '000'
                                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                                @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
                                                                                                @FLOAT1,                        -- 1.0
                                                                                                @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                                @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                                @ACUMULAR_CDI,                  -- ACUMULAR O CDI DESDE INICIO P/ CRAVAR COM A CETIP
                                       @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                                              , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                              , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR
                                                                        END
                                                                        ELSE
                                                                        BEGIN

                                                                                EXEC SIAN_SP_VARIACAO
                                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                                @DT_FIM_VAR_LOOP,               -- DATA DE CALCULO
                                                                                                @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                                @IDX_PC_LOOP,                   -- PERCENTUAL (CDI OU REF)
                                                                                                @FLOAT0,                        -- 0.0
                                                                                                @FLOAT0,                        -- 0.0
                                                                                                '000',                          -- '000'
                                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                                @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
                                                                                                @FLOAT1,                        -- 1.0
                                                                                                @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                                @DT_FIM_VAR_LOOP,               -- DATA PARA IGPM PREVIO
                                                                                                @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                                              , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                              , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR
                                                                        END
                                                                        

              IF @ERRO <> -1
                                                                        BEGIN
                      SELECT
                                                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                                   RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                                IF @VB_SQL IS NOT NULL
                                                                                BEGIN
                                                                                        SELECT
                                                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                                @ERRO                   ERRO,
                                                                                                @ERR_MSG                ERR_MSG

                                                                                        RETURN
                                                                                END
                                                                                ELSE
                                                                                        RETURN

                                                                        END
                                                                END


                                                                -- BUSCA O PERIODO ANTERIOR

                                                                SELECT
                                                                        @ID_RPAC_LOOP    = @ID_RPAC_LOOP - 1


                                                                -- ACUMULA A CORRECAO

                                                                SELECT
                                                                        @CORC_PZ_MDIO = @CORC_PZ_MDIO * @CORRECAO_LOOP


                                                        END
                                                        ELSE  -- INDEXADORES DIFERENTE DE CDI
                                                        BEGIN


                                                                -- VARIACAO DO INDEXADOR POR DIAS UTEIS

                                                                SELECT
                                                                        @CORRECAO_LOOP   = @FLOAT0,
                                                                        @RFSC_VALOR_LOOP = @FLOAT0

                                                                --PAÇOCA - 02/02/2010
                                                                IF @DT_VLRZ_TERMO IS NOT NULL   
                                                                BEGIN

                                                                        EXEC SIAN_SP_VARIACAO
                                                                                        @IDX_CODIGO,                    -- INDEXADOR
                                                                                        @DT_FIM_VAR_LOOP,               -- DATA DE CALCULO
                                                                                        @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                        @IDX_PC_LOOP,                 -- PERCENTUAL (CDI OU REF)
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        '000',                          -- '000'
                                                                                        @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                        @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                        @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
                                                                                        @FLOAT1,                        -- 1.0
                                                                                        @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                        @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                        'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                        0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                        @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                        @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                        @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                        @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                          , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                          , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                                                                END
                                                                ELSE
                                                                BEGIN

                                                                        EXEC SIAN_SP_VARIACAO
                                                                                        @IDX_CODIGO,                    -- INDEXADOR
                                                                                        @DT_FIM_VAR_LOOP,               -- DATA DE CALCULO
                                                                                        @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                                                        @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                        @IDX_PC_LOOP,                   -- PERCENTUAL (CDI OU REF)
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        '000',                          -- '000'
                                                                                      @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                  @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                        @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
                                                                                        @FLOAT1,                        -- 1.0
                                                                                        @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                        @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                        'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                        0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                        @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                        @DT_FIM_VAR_LOOP,               -- DATA PARA IGPM PREVIO
                                                                                        @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                        @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                          , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                          , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                                                                END
                                                                        

                                                                IF @ERRO <> -1
                                                                BEGIN
                                                                        SELECT
                                                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                           RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                        IF @VB_SQL IS NOT NULL
                                                                        BEGIN
                                                                                SELECT
                                                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                        @ERRO                   ERRO,
                                                                                        @ERR_MSG                ERR_MSG

                                                                                RETURN
                                                                        END
                                                                        ELSE
                                                                                RETURN

                                                                END

   -- ACUMULA A CORRECAO

                                                                SELECT
                                                                        @CORRECAO_U = @CORRECAO_U * @CORRECAO_LOOP,
                   @CORC_PZ_MDIO = @CORC_PZ_MDIO * @CORRECAO_LOOP

-- if @@spid in (93, 105) select 1346 'rf0884', @DT_EVE DT_EVE, @CORRECAO_U CORRECAO_U

                                                                -- GUARDA A ULTIMA COTACAO

                                                                IF @ID_RPAC_LOOP = @ID_RPAC

                                                                        SELECT
                                                                                @RFSC_VALOR = @RFSC_VALOR_LOOP

                                                                -- VARIACAO DO INDEXADOR POR DIAS CORRIDOS

                                                                SELECT
                                                                        @CORRECAO_LOOP   = @FLOAT0,
                                                                        @RFSC_VALOR_LOOP = @FLOAT0
                                                                
                                                                --PAÇOCA - 02/02/2010
                                                                IF @DT_VLRZ_TERMO IS NOT NULL   
                                                                BEGIN

                                                                        EXEC SIAN_SP_VARIACAO
                                                                                        @IDX_CODIGO,                    -- INDEXADOR
                                                                                        @DT_FIM_VAR_LOOP_C,             -- DATA DE CALCULO  -- Liana - 22/06/2011
                                                                                        @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                                                        @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                        @IDX_PC_LOOP,                   -- PERCENTUAL (CDI OU REF)
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        '000',                          -- '000'
                                                                                        @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                        @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                        @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
                                                                                        @FLOAT1,                        -- 1.0
                                                                                        @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                        @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                        'C',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                        0,                              -- 0 PARA RETORAR NA VARIAVEL
             @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                        @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                        @SGL_SISTEMA = 'RDF',         -- LIANA - 22/11/2010
                                                                                        @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                          , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                          , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                                                                END
                                                                ELSE
                                                                BEGIN

                                                                        EXEC SIAN_SP_VARIACAO
                                                                                        @IDX_CODIGO,                    -- INDEXADOR
                                                                                        @DT_FIM_VAR_LOOP_C,             -- DATA DE CALCULO  -- Liana - 22/06/2011
                                                                                        @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                                                        @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                        @IDX_PC_LOOP,                   -- PERCENTUAL (CDI OU REF)
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        '000',                          -- '000'
                                                                                        @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                        @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                        @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
                                                                                        @FLOAT1,                        -- 1.0
                                                                                        @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                        @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                        'C',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                        0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                        @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                        @DT_FIM_VAR_LOOP_C,             -- DATA PARA IGPM PREVIO -- Liana - 22/06/2011
                                                                                        @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                        @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                          , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                          , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                                                                END
                                                                        


                                                                IF @ERRO <> -1
                                           BEGIN
                                                                        SELECT
                                                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                           RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                        IF @VB_SQL IS NOT NULL
                                                                        BEGIN
                                                                                SELECT
                                                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                        @ERRO                   ERRO,
                                                                                        @ERR_MSG                ERR_MSG

                                                                                RETURN
                                                                        END
                                                                        ELSE
                                                                                RETURN

                                                                END

                                                                -- ACUMULA A CORRECAO

                                                                SELECT
                                                                        @CORRECAO_C = @CORRECAO_C * @CORRECAO_LOOP



                                                                -- BUSCA O PERIODO ANTERIOR

                                                                SELECT
                                                                        @ID_RPAC_LOOP    = @ID_RPAC_LOOP - 1

                                                                SELECT
                                                                        @DT_FIM_VAR_LOOP = @DT_INI_VAR_LOOP,    -- DATA DE CALCULO
                                                                        @DT_FIM_VAR_LOOP_C = @DT_INI_VAR_LOOP, -- LIANA - DATA CORRIDOS - 22/06/2011
                                                                        @DT_INI_VAR_LOOP = DT_INIC_RPAC,        -- DATA BASE DE CALCULO OU EMISSAO
                                                                        @IDX_PC_LOOP     = CASE WHEN @IC_EVE_CORC_TIT = 'S' THEN @IDX_PC_LOOP
                                                                                                ELSE IDX_PC
                                                                                           END,                 -- SE FOR UM EVENTO DE CORRECAO, UTILIZAR O PERCENTUAL DE CORRECAO
                                                                        @IC_COR_TIT_LOOP = IC_EVE_CORC_TIT      -- EVENTO DE CORRECAO DO TITULO
                                                                FROM
                                                                        SANT643_RF_REPAC_TITULO
                                                                WHERE
                                            TITULO_ID       = @TITULO_ID            AND
                                                                        ID_RPAC         = @ID_RPAC_LOOP

                                                                -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE DEVE SER A DO ANIVERSARIO DO PAPEL
                                                                if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR < 1
                                                                BEGIN
                                                                        IF @DT_FIM_VAR_LOOP_C > @DT_CORRECAO_C
                                                                        BEGIN
                                                                                SELECT @DT_FIM_VAR_LOOP = @DT_CORRECAO_U
                                                                                SELECT @DT_FIM_VAR_LOOP_C = @DT_CORRECAO_C
--                                                                              select @DT_FIM_VAR_LOOP_C '@DT_FIM_VAR_LOOP_C', @DT_FIM_VAR_LOOP '@DT_FIM_VAR_LOOP'
                                                                        END
                                                                END


                                                        END
                                                END -- fim loop
                                        END
                                        -- Liana - 22/06/2011
                                        ELSE IF (@TIPO_ATU_CETIP <> @TIPO_ATU_CETIP_ANUAL_EMIS OR @PCH_PAGA_CORRECAO = 'S') -- JUROS SOBRE O PRINCIPAL
                                        BEGIN

                                                IF @DT_BASE_CALC IS NULL

                                                        SELECT
                                                                @DT_BASE_CALC = @DT_B_JURS

                                                -- SE FOR INDEXADO PELO CDI, UTILIZAR O PROJETADO

                                                IF @IDX_CODIGO = 'CDI' AND @EH_FUNDO = 'S'
                                                BEGIN

                                                        -- RF2333.PRC

                                                        EXEC SANPP_RF_CALC_CDI_PROJETADO        @TITULO_ID,             -- IDENTIFICADOR DO TITULO
                                                                                                @ID_RPAC,               -- IDENTIFICADOR DO PERIODO DE REPACTUACAO
                                                                                                @EVE_CORRENTE,          -- TIPO DO EVENTO A SER UTILIZADO NA PROJECAO
                                                                                                @DT_UTIL,               -- DATA DE CALCULO DA PROJECAO
                                                                                                @DT_EVE,                -- DATA DO EVENTO A PROJETAR O CDI
                                                                                                @DT_BASE_CALC,          -- DATA BASE DE CALCULO
                                                                                                @DT_PROX_RPAC,          -- DATA DA PROXIMA REPACTUACAO OU VENCIMENTO
                                                                                                @IDX_CODIGO,            -- INDEXADOR
                                                                                                @IDX_PC,                -- PERCENTUAL DO INDEXADOR
                                                                                                @FER_CHAVE,             -- POSICAO PARA FERIADOS
                                                                                                @CORRECAO_U     OUTPUT, -- CDI PROJETADO
                                                                                        @ERRO           OUTPUT, -- RETORNA -1 => SEM ERROS
                                                                                                @ERR_MSG        OUTPUT, -- MENSAGEM DE ERRO
                                                                                                NULL                    -- QUANDO NAO NULO, RETORNA RECORDSET PARA VB

-- if @@spid in (93, 105) select 1515 'rf0884', @DT_EVE DT_EVE, @CORRECAO_U CORRECAO_U


                                                        IF @ERRO <> -1
                                                        BEGIN

                                                                IF @VB_SQL IS NOT NULL
                                                                BEGIN
                                                                        SELECT
                                                                                @TIR_U          TIR_U,
                                                                                @TIR_C          TIR_C,
                                                                                @ERRO           ERRO,
                                                                                @ERR_MSG        ERR_MSG

                                                                        RETURN
                                                                END
                                                                ELSE
                                                                        RETURN

                                                        END

              -- By Charlie - 27/08/2015 - Ajuste CRI
              IF @ATV_CODIGO <> 'CRI'
              BEGIN
                                                        SELECT 
                                                                @CORRECAO_C = @CORRECAO_U
              END


-- if @@spid = 80 select 1174 'rf0884', @CORRECAO_U CORRECAO_U

                                                        -- CALCULA A CORRECAO PARA O CALCULO DO PRAZO MEDIO

                                                        --PAÇOCA - 02/02/2010
                                                        IF @DT_VLRZ_TERMO IS NOT NULL   
                                                        BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_UTIL,                       -- DATA DE CALCULO
                                                                                @DT_BASE_CALC,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @FLOAT0,                        -- RETORNA A ULTIMA COTACAO
                                 @FLOAT1,                        -- 1.0
                                                                                @CORC_PZ_MDIO   OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
               'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR
                                                        END
                                                        ELSE
                                                        BEGIN


                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_UTIL,                       -- DATA DE CALCULO
                                                                                @DT_BASE_CALC,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @FLOAT0,                        -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORC_PZ_MDIO   OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_UTIL,                       -- DATA PARA IGPM PREVIO
                                                  @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR
                                                        END

                                                                        

    IF @ERRO <> -1
                                                        BEGIN
                                                                SELECT
                                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                   RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                IF @VB_SQL IS NOT NULL
                                                                BEGIN
                                                                        SELECT
                                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                @ERRO                   ERRO,
                                                                                @ERR_MSG                ERR_MSG

                                                                        RETURN
                                                                END
                                                                ELSE
                                                                        RETURN

                                                        END

                                                END
                                                ELSE
                                                BEGIN

                                                        -- VARIACAO DO INDEXADOR POR DIAS UTEIS

                                                        SELECT
                                                                @RFSC_VALOR     = @FLOAT0

                                                        SELECT  @DT_ATU         = @DT_UTIL

                                                        -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO DO LOOP, A DATA LIMITE DEVE SER O ANIVERSARIO DO PAPEL
                                                        if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR < 1
                                                        BEGIN
                                                                SELECT @DT_ATU = @DT_CORRECAO_U
--                                                              select @DT_ATU '@DT_ATU'
                                                        END


--                                                      IF (@FL_ARRED = 'S' and @TIPO_ATU_CETIP > 0)
--                                                              EXEC SANPP_RF_CALC_DT_ATUAL_CORRECAO
--                                                                      @TIPO_ATU_CETIP,
--                                                                      @DT_EVE,
--                                                                      @DT_UTIL,
--                                                                      @TIT_VENCTO,
--                                                                      @DT_BASE_CALC,
--                                                                      @DT_ATU OUTPUT,
--                                         @ATU_FLUXOS_FUT
--select "DATA_ATU" = @DT_ATU, "DATA_EVE" = @DT_EVE, "DATA_CALC" = @DT_CALC

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
                                SELECT @VDT_DATA_FIM = @DT_VCMT_UTIL
                            END
                                                        
                                                        --PAÇOCA - 02/02/2010
                                                        IF @DT_VLRZ_TERMO IS NOT NULL   
                                                        BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_ATU,                        -- DATA DE CALCULO
                                                                                @VDT_DATA_INI,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @VDT_DATA_FIM,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORRECAO_U     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
        
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                NULL,
                                                                    '001',
                                                                                @UTILIZ_PREVIA,                 -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                                                                                @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

-- if @@spid in (93, 105) select 1705 'rf0884', @DT_EVE DT_EVE, @CORRECAO_U CORRECAO_U, @VDT_DATA_INI VDT_DATA_INI, @VDT_DATA_FIM VDT_DATA_FIM

                                                        END
                                                        ELSE
                                                        BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_ATU,                        -- DATA DE CALCULO
                                                                                @VDT_DATA_INI,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @VDT_DATA_FIM,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORRECAO_U     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
        
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_UTIL,                       -- DATA PARA IGPM PREVIO
                                                                                NULL,
                                                                                '001',
                                                                                @UTILIZ_PREVIA,                 -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                                                                                @ACUMULAR_CDI,                  -- ACUMULAR O CDI DESDE INICIO P/ CRAVAR COM A CETIP
                                                                                @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

-- if @@spid in (93, 105) select 1740 'rf0884', @DT_EVE DT_EVE, @CORRECAO_U CORRECAO_U, @VDT_DATA_INI VDT_DATA_INI, @VDT_DATA_FIM VDT_DATA_FIM
                                                        END
                                                        
                                                        IF @ERRO <> -1
                                                        BEGIN
                                                                SELECT
                                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                   RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                IF @VB_SQL IS NOT NULL
                                                                BEGIN
                                                                        SELECT
                                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                @ERRO                   ERRO,
                                                                                @ERR_MSG                ERR_MSG

                                                                        RETURN
                                                                END
                                                                ELSE
                                                                        RETURN

                                                        END


                                                        -- UTILIZA A MESMA CORRECAO PARA O PRAZO MEDIO

                                                        SELECT
                                                                @CORC_PZ_MDIO = @CORRECAO_U


                                                        -- VARIACAO DO INDEXADOR POR DIAS CORRIDOS

                                                        SELECT
                                                                @RFSC_VALOR     = @FLOAT0

                                                        SELECT  @DT_ATU         = @DT_CALC

                                                        -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO DO LOOP, A DATA LIMITE DEVE SER O ANIVERSARIO DO PAPEL
                                                        if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR < 1
                                                        BEGIN
                                                                SELECT @DT_ATU = @DT_CORRECAO_C
                                                        END


--                                                      IF (@FL_ARRED = 'S' and @TIPO_ATU_CETIP > 0)
--                                                              EXEC SANPP_RF_CALC_DT_ATUAL_CORRECAO
--                                                                      @TIPO_ATU_CETIP,
--                                                                      @DT_EVE,
--                                              @DT_CALC,
--                                                                      @TIT_VENCTO,
--                                                                      @DT_BASE_CALC,
--                                                                      @DT_ATU OUTPUT,
--                                                                      @ATU_FLUXOS_FUT

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
                                SELECT @VDT_DATA_FIM = @DT_VCMT_UTIL
                            END
                                                        
                                                        --PAÇOCA - 02/02/2010
                                                        IF @DT_VLRZ_TERMO IS NOT NULL   
                                                        BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_ATU,                        -- DATA DE CALCULO
                                                                                @VDT_DATA_INI,                  -- DATA BASE DE CALCULO OU EMISSAO 
                                                                                @VDT_DATA_FIM,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORRECAO_C     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                @TIPO_COTACAO_C,                -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                NULL,
                                                                                '001',
                                                                                @UTILIZ_PREVIA,                 -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                                                    @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                                                        END
                                                        ELSE
                                                        BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_ATU,                        -- DATA DE CALCULO
                                                                                @VDT_DATA_INI,                  -- DATA BASE DE CALCULO OU EMISSAO 
                                                                                @VDT_DATA_FIM,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORRECAO_C     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                @TIPO_COTACAO_C,                -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_CALC,                       -- DATA PARA IGPM PREVIO
                                                                                NULL,
                                                                                '001',
                                                                                @UTILIZ_PREVIA,                 -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                                                                  @ACUMULAR_CDI,
                                                                                @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR


                                     END

                                                        IF @ERRO <> -1
                                                        BEGIN
                                                                SELECT
                                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                   RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                IF @VB_SQL IS NOT NULL
                                                                BEGIN
                                                                        SELECT
                                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                @ERRO                   ERRO,
                                                                                @ERR_MSG                ERR_MSG

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

-- if @@spid in (93, 105) select 1913 'rf0884', @DT_EVE DT_EVE, @CORRECAO_U CORRECAO_U

                                                END

                                                SELECT  @CUPOM_TIR = ROUND(@CUPOM_TIR,@CUPOM_TIR_CASAS,@CUPOM_TIR_ARRED),
                                                        @V_NOML_PCPL = ROUND(@V_NOML_PCPL,@VALORES_CASAS,@VALORES_ARRED),
                                                        @CORRECAO_C = ROUND(@CORRECAO_C,@CORRECAO_CASAS,@CORRECAO_ARRED),
                                                        @CORRECAO_U = ROUND(@CORRECAO_U,@CORRECAO_CASAS,@CORRECAO_ARRED)

-- if @@spid in (93, 105) select 1922 'rf0884', @DT_EVE DT_EVE, @CORRECAO_U CORRECAO_U

                                        END

--SELECT @CORRECAO_C CORRECAO_C, 'BICUDO'

                                        -- SE FOR UM EVENTO DE JUROS SOBRE AMORTIZACAO APLICA A VARIACAO SOBRE A AMORTIZACAO
                                        IF @EVE_CORRENTE = @EVE_JURS_AMTC
begin
                                                SELECT
                                                        @V_FUT_U_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_AMTC * @CORRECAO_U ),
                                @V_FUT_C_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_AMTC * @CORRECAO_C ),
                                                        @V_FUT_M_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_AMTC * @CORRECAO_U ),
                                                        -- Cleber - 11/08/2006 - pgto de eventos em dias nao uteis para tit. com base dias corridos
                                                        @V_PU_CALD_JURS = 
                                                        (CASE WHEN (@FL_ARRED = 'S' AND (@RFX_FCALC = '001' OR @RFX_FCALC = '002' OR @RFX_FCALC = '032' OR @ATV_CODIGO = 'L H'))
                                                        THEN    
                                                                (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_AMTC * @CORRECAO_C )
                                                        ELSE
                                                                (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_AMTC * @CORRECAO_U )
                                                        END),
                                                        @V_FUT_PZ_MDIO  = @V_FUT_PZ_MDIO + (( @CUPOM_PZ_MDIO - @FLOAT1 ) * @V_NOML_AMTC * @CORC_PZ_MDIO )

if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@V_FUT_C_JURS @EVE_JURS_AMTC'
	, valor				= @V_FUT_C_JURS
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@CORRECAO_C @EVE_JURS_AMTC'
	, valor				= @CORRECAO_C
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@CUPOM_TIR @EVE_JURS_AMTC'
	, valor				= @CUPOM_TIR
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@V_NOML_AMTC @EVE_JURS_AMTC'
	, valor				= @V_NOML_AMTC
end
--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_JURS @EVE_JURS_AMTC'
	, valor				= @V_FUT_U_JURS
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@CORRECAO_U @EVE_JURS_AMTC'
	, valor				= @CORRECAO_U
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@CUPOM_TIR @EVE_JURS_AMTC'
	, valor				= @CUPOM_TIR
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@V_NOML_AMTC @EVE_JURS_AMTC'
	, valor				= @V_NOML_AMTC
end

end
                                        ELSE

                                        BEGIN
                                                IF @EH_FUNDO = 'N' AND @IDX_CODIGO = 'CDI'
                                                BEGIN           
                                                        SELECT
                                                                @V_FUT_M_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL )
                                                END
                                                ELSE
                                                BEGIN
                                                        SELECT
                                                                @V_FUT_M_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL * @CORRECAO_U )

                                                END

                                                SELECT
                                                        @V_FUT_U_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL * @CORRECAO_U ),
                                                        @V_FUT_C_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL * @CORRECAO_C ), 
--                                                      @V_FUT_M_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL * @CORRECAO_U ),
                                                        -- Cleber - 11/08/2006 - pgto de eventos em dias nao uteis para tit. com base dias corridos
                                                        @V_PU_CALD_JURS = 
                                                        (CASE WHEN (@FL_ARRED = 'S' AND (@RFX_FCALC = '001' OR @RFX_FCALC = '002' OR @RFX_FCALC = '032' OR @ATV_CODIGO = 'L H'))
                                                        THEN    
                                                                (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL * (CASE WHEN @TIPO_ATU_CETIP = 4 AND @PCH_PAGA_CORRECAO = 'N' THEN 1 ELSE @CORRECAO_C END) ) 
                                                        ELSE
                                                                (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL * (CASE WHEN @TIPO_ATU_CETIP = 4 AND @PCH_PAGA_CORRECAO = 'N' THEN 1 ELSE @CORRECAO_U END) )
                                                        END),
                                                        @V_FUT_PZ_MDIO  = @V_FUT_PZ_MDIO + (( @CUPOM_PZ_MDIO - @FLOAT1 ) * @V_NOML_PCPL * @CORC_PZ_MDIO )

if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@V_FUT_C_JURS @EVE_JURS_PCPL'
	, valor				= @V_FUT_C_JURS
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@CORRECAO_C @EVE_JURS_PCPL'
	, valor				= @CORRECAO_C
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@CUPOM_TIR @EVE_JURS_PCPL'
	, valor				= @CUPOM_TIR
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@V_NOML_PCPL @EVE_JURS_PCPL'
	, valor				= @V_NOML_PCPL
end
--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_JURS @EVE_JURS_PCPL'
	, valor				= @V_FUT_U_JURS
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@CORRECAO_U @EVE_JURS_PCPL'
	, valor				= @CORRECAO_U
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@CUPOM_TIR @EVE_JURS_PCPL'
	, valor				= @CUPOM_TIR
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@V_NOML_PCPL @EVE_JURS_PCPL'
	, valor				= @V_NOML_PCPL
end
-- if @@spid in (93, 105) select 1974 'rf0884', @DT_EVE DT_EVE, @V_FUT_U_JURS V_FUT_U_JURS, @CORRECAO_U CORRECAO_U
                                        END
                                END
                                ELSE
                                BEGIN
                                        -- CASO O TITULO UTILIZE ARREDONDAMENTO CETIP
                                        IF @FL_ARRED = 'S'
                                                SELECT  @CUPOM_TIR = ROUND(@CUPOM_TIR,@CUPOM_TIR_CASAS,@CUPOM_TIR_ARRED),
                                                        @V_NOML_PCPL = ROUND(@V_NOML_PCPL,@VALORES_CASAS,@VALORES_ARRED)

                                        SELECT
                                                @V_FUT_U_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL ),
                                                @V_FUT_C_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL ),
                                                @V_FUT_M_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL ),
                                                @V_PU_CALD_JURS = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL ),
                                                @V_FUT_PZ_MDIO  = @V_FUT_PZ_MDIO + (( @CUPOM_PZ_MDIO - @FLOAT1 ) * @V_NOML_PCPL )

if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@V_FUT_C_JURS 2019'
	, valor				= @V_FUT_C_JURS
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@CUPOM_TIR 2019'
	, valor				= @CUPOM_TIR
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@V_NOML_PCPL 2019'
	, valor				= @V_NOML_PCPL
end
--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_JURS 2019'
	, valor				= @V_FUT_U_JURS
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@CUPOM_TIR 2019'
	, valor				= @CUPOM_TIR
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@V_NOML_PCPL 2019'
	, valor				= @V_NOML_PCPL
end

                                END
                        END


                        --------------------------------------------------------------------------------------------------
                        -- FLUXOS DE ATUALIZACAO (CALCULA O VALOR DAS ATUALIZACOES, SOBRE AMORTIZACAO OU PRINCIPAL)
                        --------------------------------------------------------------------------------------------------

                        IF EXISTS (
                                SELECT
                                        *
                                FROM
                                        SANT644_RF_AGENDA_EVENTO
                                WHERE
                                        TITULO_ID       =  @TITULO_ID                           AND
                                        ID_RPAC         =  @ID_RPAC                             AND
                                        ID_T_EVE        IN (@EVE_CORC_AMTC, @EVE_CORC_PCPL)     AND
                                        DT_EVE          =  @DT_EVE
                                )
                        BEGIN

                                SELECT
                                        @ID_SEQ_EVE_COR = @ID_SEQ_EVE_COR + 1,
                                        @CORC_PZ_MDIO   = @FLOAT1

                                -- BUSCA O EVENTO CORRENTE
                                SELECT
                                        @EVE_CORRENTE   = ID_T_EVE
                                FROM
                                        SANT644_RF_AGENDA_EVENTO
                                WHERE
                                        TITULO_ID       =  @TITULO_ID                           AND
                                        ID_RPAC         =  @ID_RPAC                             AND
                                        ID_T_EVE        IN (@EVE_CORC_AMTC, @EVE_CORC_PCPL)     AND
                                        DT_EVE          =  @DT_EVE

                                SELECT
                                        @EVE_POR_DATA = RTRIM(@EVE_POR_DATA) + @EVE_CORRENTE

                                --------------------------------------------------------------------------------------------------------
                                -- CALCULA O VALOR DO FLUXO PARA ATUALIZACAO SOBRE AMORTIZACAO OU PARA O
                                -- PRIMEIRO FLUXO DE ATUALIZACAO SOBRE O PRINCIPAL CASO NAO SEJA CDI
                                -- SE FOR CDI UTILIZA O PROJETADO
                                --------------------------------------------------------------------------------------------------------

                                --IF @EVE_CORRENTE = @EVE_CORC_AMTC OR @ID_SEQ_EVE_COR = 1 OR (@IDX_CODIGO = 'CDI' AND @EH_FUNDO = 'S')
--                              IF @EVE_CORRENTE = @EVE_CORC_AMTC OR @ID_SEQ_EVE_COR = 1 OR (@IDX_CODIGO = 'CDI')
                                IF (@EVE_CORRENTE = @EVE_CORC_AMTC  AND (@TIPO_ATU_CETIP <> @TIPO_ATU_CETIP_ANUAL_EMIS OR @PCH_PAGA_CORRECAO = 'S'))
                                        -- Liana - 22/06/2011 - Para Atualização Anual, deve calcular a correção para o próximo evento anual
                                        OR ((@ID_SEQ_EVE_COR = 1  and @TIPO_ATU_CETIP <> @TIPO_ATU_CETIP_ANUAL_EMIS) OR (@ID_SEQ_EVE_COR = 1 AND @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS and @PCH_PAGA_CORRECAO = 'S')) 
                                        OR (@IDX_CODIGO = 'CDI')

                BEGIN

                                        -- PARA ATUALIZACAO SOBRE AMORTIZACAO, SEMPRE UTILIZA A DATA BASE DE CALCULO

                                        IF @EVE_CORRENTE = @EVE_CORC_AMTC
                                        BEGIN

                                                SELECT
                                                        @DT_BASE_CALC = @RF_BASE_CALC

                                        END
                                        ELSE -- CORRECAO SOBRE PRINCIPAL
                                        BEGIN

                                                -- SE POSSUIR BASE DE CALCULO SO UTILIZA NO PRIMEIRO PAGAMENTO

                                                IF @DT_EVE = (  SELECT
                                                                        MIN(DT_EVE)
                                                                FROM
                                                                        SANT644_RF_AGENDA_EVENTO
                                                                WHERE
                                                                        TITULO_ID       = @TITULO_ID            AND
                                                                        ID_RPAC         = @ID_RPAC              AND
                                                                        ID_T_EVE        = @EVE_CORC_PCPL )
                                                BEGIN

                                                        SELECT
                                                                @DT_BASE_CALC = @RF_BASE_CALC

                                                END
                                                ELSE
                                                BEGIN

                                                        SELECT
                                                                @DT_BASE_CALC   = MAX(DT_EVE)
                                                        FROM
                                                                SANT644_RF_AGENDA_EVENTO
                                                        WHERE
                                                                TITULO_ID       = @TITULO_ID            AND
                                                                ID_RPAC         = @ID_RPAC              AND
                                                                ID_T_EVE        = @EVE_CORC_PCPL        AND
                                                                DT_EVE          < @DT_EVE

                                                        IF @DT_BASE_CALC IS NULL
                                                                SELECT
                                                                        @DT_BASE_CALC = @DT_INIC_RPAC

                                                END

                                           -- Liana - 22/06/2011
                                                if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS
                                                        select @DT_BASE_CALC = @DT_ANIV_ANT
            ELSE IF @TIPO_ATU_CETIP = 4
                                                        SELECT @DT_BASE_CALC = @TIT_EMISSAO

                    END



                                        -- PARA EVENTO DE JUROS SOBRE AMORTIZACAO, CALCULA A VARIACAO DE 
                                        -- FORMA DECRESCENTE POR ID_RPAC. ISTO PORQUE A CADA REPACTUACAO PODE-SE
                                        -- MUDAR O PERCENTUAL DE INDEXADOR PARA CDI. SE OCORREU UMA CORRECAO DO 
                                        -- TITULO, CONSIDERAR O PERCENTUAL CORRIGIDO.

-- if @@spid = 80 select 1525 'rf0884', @EVE_CORRENTE EVE_CORRENTE, @EVE_CORC_AMTC EVE_CORC_AMTC, @IDX_FCALC IDX_FCALC, @ID_RPAC ID_RPAC

                                        IF @EVE_CORRENTE = @EVE_CORC_AMTC AND @IDX_FCALC = '000' AND @ID_RPAC > 1 AND @IC_EVE_CORC_TIT = 'N'
                                        BEGIN

                                                SELECT
                                                        @ID_RPAC_LOOP    = @ID_RPAC,
                                                        @CORRECAO_U      = @FLOAT1,
                                                        @CORRECAO_C      = @FLOAT1,
                                                        @DT_FIM_VAR_LOOP = @DT_UTIL,            -- DATA DE CALCULO
                                                        @DT_FIM_VAR_LOOP_C = @DT_UTIL,          -- DATA DE CALCULO -- Liana - 22/06/2011
                                                        @DT_INI_VAR_LOOP = @DT_INIC_RPAC,       -- DATA BASE DE CALCULO OU EMISSAO
                                                        @IDX_PC_LOOP     = @IDX_PC,             -- PERCENTUAL (CDI OU REF)
                                                        @IC_COR_TIT_LOOP = @IC_EVE_CORC_TIT     -- EVENTO DE CORRECAO DO TITULO


                                                -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE EH A DATA DO ANIVERSARIO DO PAPEL
                                                if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR = 1
                                                BEGIN
                                                        SELECT @DT_FIM_VAR_LOOP = @DT_CORRECAO_U
                                                        SELECT @DT_FIM_VAR_LOOP_C = @DT_CORRECAO_C
--                                                      select @DT_FIM_VAR_LOOP_C '@DT_FIM_VAR_LOOP_C', @DT_FIM_VAR_LOOP '@DT_FIM_VAR_LOOP'
                                                END



                                                WHILE @ID_RPAC_LOOP > 0
                                                BEGIN


                                                        -- 06.09.2005
                                                        -- SE FOR CDI E ESTIVER NO ID ATUAL, CALCULA ATRAVÉS DE PROJECOES

--                                                      IF @IDX_CODIGO = 'CDI' AND @EH_FUNDO = 'S' AND @ID_RPAC_LOOP = @ID_RPAC
                                                        IF @IDX_CODIGO = 'CDI' AND @ID_RPAC_LOOP = @ID_RPAC 
                                                        BEGIN


                                                                SELECT
                                                                        @CORRECAO_LOOP  = @FLOAT0


                                                                -- RF2333.PRC

                                                                EXEC SANPP_RF_CALC_CDI_PROJETADO        @TITULO_ID,             -- IDENTIFICADOR DO TITULO
                                        @ID_RPAC_LOOP,          -- IDENTIFICADOR DO PERIODO DE REPACTUACAO
                                                                                                        @EVE_CORRENTE,          -- TIPO DO EVENTO A SER UTILIZADO NA PROJECAO
                                                                                                        @DT_FIM_VAR_LOOP,       -- DATA DE CALCULO DA PROJECAO
                                                                       @DT_EVE,                -- DATA DO EVENTO A PROJETAR O CDI
                                                                                                        @DT_INI_VAR_LOOP,       -- DATA BASE DE CALCULO
                                                                                                        @DT_PROX_RPAC,          -- DATA DA PROXIMA REPACTUACAO OU VENCIMENTO
                                                                                                        @IDX_CODIGO,            -- INDEXADOR
                                                                                                        @IDX_PC_LOOP,           -- PERCENTUAL DO INDEXADOR
                                                                                                        @FER_CHAVE,             -- POSICAO PARA FERIADOS
                                                                                                        @CORRECAO_LOOP  OUTPUT, -- CDI PROJETADO
                                                                                                        @ERRO           OUTPUT, -- RETORNA -1 => SEM ERROS
                                                                                                        @ERR_MSG        OUTPUT, -- MENSAGEM DE ERRO
                                                                                                        NULL                    -- QUANDO NAO NULO, RETORNA RECORDSET PARA VB


                                                                IF @ERRO <> -1
                                                                BEGIN

                                                                        IF @VB_SQL IS NOT NULL
                                                                        BEGIN
                                                                                SELECT
                                                                                        @TIR_U          TIR_U,
                                                                                        @TIR_C          TIR_C,
                                                                                        @ERRO           ERRO,
                                                                                        @ERR_MSG        ERR_MSG

                                                                                RETURN
                                                                        END
                                                                        ELSE
                                                                                RETURN

                                                                END


                                                                -- ACUMULA A CORRECAO

                                                                SELECT
                                                                        @CORRECAO_U = @CORRECAO_U * @CORRECAO_LOOP,
                                                                        @CORRECAO_C = @CORRECAO_C * @CORRECAO_LOOP



                                                                -- CALCULA A CORRECAO PARA CALCULO DO PRAZO MEDIO

                                                                SELECT
                                                                        @CORRECAO_LOOP   = @FLOAT1,
                                                  @RFSC_VALOR_LOOP = @FLOAT0


                                                                IF @EVE_CORRENTE = @EVE_CORC_AMTC       OR
                                                                   (@EVE_CORRENTE = @EVE_CORC_PCPL AND @ID_SEQ_EVE_COR < 1)
                                                                BEGIN
                                                                        --PAÇOCA - 02/02/2010
                                                       IF @DT_VLRZ_TERMO IS NOT NULL   
                                                                        BEGIN

                                                                                EXEC SIAN_SP_VARIACAO
                                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                                @DT_FIM_VAR_LOOP,               -- DATA DE CALCULO
                                                                                                @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                                @IDX_PC_LOOP,                   -- PERCENTUAL (CDI OU REF)
                                                                                                @FLOAT0,                        -- 0.0
                                                                                                @FLOAT0,                        -- 0.0
                                                                                                '000',                          -- '000'
                                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                                @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
                                                                                                @FLOAT1,                        -- 1.0
                                                                                                @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                                @DT_VLRZ_TERMO,         -- DATA PARA IGPM PREVIO
                                                                                                @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                                              , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                              , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR
                                                                        END
                                                                        ELSE
                                                           BEGIN

                                                                                EXEC SIAN_SP_VARIACAO
                                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                                @DT_FIM_VAR_LOOP,               -- DATA DE CALCULO
                                              @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                                @IDX_PC_LOOP,                   -- PERCENTUAL (CDI OU REF)
                                                                                                @FLOAT0,                        -- 0.0
                                                                                                @FLOAT0,                        -- 0.0
                                                                                                '000',                          -- '000'
                                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                                @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
                                                                                                @FLOAT1,                        -- 1.0
                                                                                                @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                                @DT_FIM_VAR_LOOP,               -- DATA PARA IGPM PREVIO
                                                                                                @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                                              , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                              , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR
                                                                        END


                                                                        IF @ERRO <> -1
                                                                        BEGIN
                                                                                SELECT
                                                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                                   RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                                IF @VB_SQL IS NOT NULL
                                                                                BEGIN
                                                                                        SELECT
                                                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                            @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                                @ERRO                   ERRO,
                                                                                                @ERR_MSG                ERR_MSG

                                                                                        RETURN
                                                                                END
                                                                                ELSE
                                                                                        RETURN

                                                                        END
                                                                END


                                                                -- BUSCA O PERIODO ANTERIOR

                                                                SELECT
                                                                        @ID_RPAC_LOOP    = @ID_RPAC_LOOP - 1


                                                                -- ACUMULA A CORRECAO

                                                                SELECT
                                                                        @CORC_PZ_MDIO = @CORC_PZ_MDIO * @CORRECAO_LOOP

                                                        END
                                                        ELSE -- CORRECAO DOS DEMAIS INDEXADORES
                                                        BEGIN


                                                                -- VARIACAO DO INDEXADOR POR DIAS UTEIS

                                                                SELECT
                                                                        @CORRECAO_LOOP   = @FLOAT0,
                                                                        @RFSC_VALOR_LOOP = @FLOAT0
                                                                --PAÇOCA - 02/02/2010
                                                                IF @DT_VLRZ_TERMO IS NOT NULL   
                                                                BEGIN

                                                                        EXEC SIAN_SP_VARIACAO
                                                                                        @IDX_CODIGO,                    -- INDEXADOR
                                                                                        @DT_FIM_VAR_LOOP,               -- DATA DE CALCULO
                                                                                        @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                                                        @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                        @IDX_PC_LOOP,                   -- PERCENTUAL (CDI OU REF)
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        '000',     -- '000'
                                                                                        @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                        @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                        @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
            @FLOAT1,                        -- 1.0
                                                                                        @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                        @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                        'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                        0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                        @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                        @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                        @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                        @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 30/05/2011
                                          , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                          , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                                                                END
                                                                ELSE
                                                                BEGIN

                                                                        EXEC SIAN_SP_VARIACAO
                                                                                        @IDX_CODIGO,                    -- INDEXADOR
                                                                                        @DT_FIM_VAR_LOOP,               -- DATA DE CALCULO
                                                                                        @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                                                        @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                        @IDX_PC_LOOP,                   -- PERCENTUAL (CDI OU REF)
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        '000',                          -- '000'
                                                                                        @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                        @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                        @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
                                                                                        @FLOAT1,                        -- 1.0
                                 @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                        @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                        'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                        0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                         @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                        @DT_FIM_VAR_LOOP,               -- DATA PARA IGPM PREVIO
                                                                                        @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                        @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 30/05/2011
                                          , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                          , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR


                                                                END
                                                                
                                                                IF @ERRO <> -1
                                                                BEGIN
                                                                        SELECT
                                                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                           RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                        IF @VB_SQL IS NOT NULL
                                                                        BEGIN
                                                                                SELECT
                                                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                        @ERRO                   ERRO,
                                                                                        @ERR_MSG                ERR_MSG

                                                                                RETURN
                                                                        END
                                                                        ELSE
                                                                                RETURN

                                                                END

                                                                -- ACUMULA A CORRECAO

                                                                SELECT
                                                                        @CORRECAO_U = @CORRECAO_U * @CORRECAO_LOOP,
                                                                        @CORC_PZ_MDIO = @CORC_PZ_MDIO * @CORRECAO_LOOP


                                                                -- GUARDA A ULTIMA COTACAO

                                                                IF @ID_RPAC_LOOP = @ID_RPAC

                                                                        SELECT
                                                           @RFSC_VALOR = @RFSC_VALOR_LOOP



                                                                -- VARIACAO DO INDEXADOR POR DIAS CORRIDOS

                                                                SELECT
                                                                        @CORRECAO_LOOP   = @FLOAT0,
                                                                        @RFSC_VALOR_LOOP = @FLOAT0
                                                                
  --PAÇOCA - 02/02/2010
                                                                IF @DT_VLRZ_TERMO IS NOT NULL   
                                                                BEGIN

                                                                        EXEC SIAN_SP_VARIACAO
                                                                                        @IDX_CODIGO,                    -- INDEXADOR
                                                                                        @DT_FIM_VAR_LOOP_C,             -- DATA DE CALCULO -- Liana - 22/06/2011
                                                                                        @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                                                        @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                        @IDX_PC_LOOP,                   -- PERCENTUAL (CDI OU REF)
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        '000',                          -- '000'
                                                                                        @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                        @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                        @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
                                                                                        @FLOAT1,                        -- 1.0
                                                                                        @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                        @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                        'C',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                        0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                        @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                        @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                        @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                        @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                          , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                          , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                                       END
                                                                ELSE
                                                                BEGIN

                                                                        EXEC SIAN_SP_VARIACAO
                                                                                        @IDX_CODIGO,                    -- INDEXADOR
                                                                                        @DT_FIM_VAR_LOOP_C,             -- DATA DE CALCULO -- Liana - 22/06/2011
                                 @DT_INI_VAR_LOOP,               -- DATA BASE DE CALCULO OU EMISSAO
                                                                                        @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                        @IDX_PC_LOOP,                   -- PERCENTUAL (CDI OU REF)
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        @FLOAT0,                        -- 0.0
                                                                                        '000',                          -- '000'
                                                                                        @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                        @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                        @RFSC_VALOR_LOOP OUTPUT,        -- RETORNA A ULTIMA COTACAO
                                                                                        @FLOAT1,                        -- 1.0
                                                                                        @CORRECAO_LOOP  OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                        @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                        'C',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                        0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                        @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                        @DT_FIM_VAR_LOOP_C,             -- DATA PARA IGPM PREVIO
                                                                                        @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                        @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                          , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                          , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                                                                END                                                             

                                                                IF @ERRO <> -1
                                                                BEGIN
                                                                        SELECT
                                                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                           RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                        IF @VB_SQL IS NOT NULL
                                                                        BEGIN
                                                                                SELECT
                                                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                   @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                        @ERRO                   ERRO,
                                                                                        @ERR_MSG                ERR_MSG

                                                                                RETURN
                                                                        END
                                                                        ELSE
                                                                                RETURN

                                                                END

                                                                -- ACUMULA A CORRECAO

                                                                SELECT
                                                                        @CORRECAO_C = @CORRECAO_C * @CORRECAO_LOOP



                                                                -- BUSCA O PERIODO ANTERIOR

                                                                SELECT
                                                                        @ID_RPAC_LOOP    = @ID_RPAC_LOOP - 1

                                                                SELECT
                                                                        @DT_FIM_VAR_LOOP = @DT_INI_VAR_LOOP,    -- DATA DE CALCULO
                                                                        @DT_FIM_VAR_LOOP_C = @DT_INI_VAR_LOOP, -- Liana - 22/06/2011
                                                                        @DT_INI_VAR_LOOP = DT_INIC_RPAC,        -- DATA BASE DE CALCULO OU EMISSAO
                                                                        @IDX_PC_LOOP     = CASE WHEN @IC_EVE_CORC_TIT = 'S' THEN @IDX_PC_LOOP
                                                                                                ELSE IDX_PC
                                                                                           END,                 -- SE FOR UM EVENTO DE CORRECAO, UTILIZAR O PERCENTUAL DE CORRECAO
                                                                        @IC_COR_TIT_LOOP = IC_EVE_CORC_TIT      -- EVENTO DE CORRECAO DO TITULO
                                                                FROM
                                                                        SANT643_RF_REPAC_TITULO
                                                                WHERE
                                                                        TITULO_ID       = @TITULO_ID            AND
                                                                        ID_RPAC         = @ID_RPAC_LOOP

                                                                -- Liana - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE EH A DATA DO ANIVERSARIO DO PAPEL
                                                                if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR = 1
                                                                BEGIN
                                                                        IF @DT_FIM_VAR_LOOP_C > @DT_CORRECAO_C
                                                                        BEGIN
                                                                                SELECT @DT_FIM_VAR_LOOP = @DT_CORRECAO_U
                                                                                SELECT @DT_FIM_VAR_LOOP_C = @DT_CORRECAO_C
--                                                                              select @DT_FIM_VAR_LOOP_C '@DT_FIM_VAR_LOOP_C', @DT_FIM_VAR_LOOP '@DT_FIM_VAR_LOOP'
                                                                        END
                                                                END


                                                        END
                                         END
                                        END
                                        ELSE -- CORRECAO SOBRE PRINCIPAL
                                        BEGIN

                                                IF @DT_BASE_CALC IS NULL

                                                        SELECT
                                                                @DT_BASE_CALC = @RF_BASE_CALC

                                                -- SE FOR INDEXADO PELO CDI, UTILIZAR O PROJETADO

--                                              IF @IDX_CODIGO = 'CDI' AND @EH_FUNDO = 'S'
                                                IF @IDX_CODIGO = 'CDI' 
                                                BEGIN

--1
                                                        SELECT
                                                                @RFSC_VALOR     = @FLOAT0


                                                        SELECT  @DT_ATU         = @DT_CALC

                                                        -- LIANA - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE EH A DATA DO ANIVERSARIO DO PAPEL
                                                        IF @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR = 1
                                                        BEGIN
                                                                SELECT @DT_ATU = @DT_CORRECAO_C
                                                        END



-- if @@spid = 80 select 2003 'rf0884', 'CORRECAO_C' = @CORRECAO_C,@DT_CALC, @DT_BASE_CALC DT_BASE_CALC, @DT_ATU DT_ATU
                                                        --PAÇOCA - 02/02/2010
                                                        IF @DT_VLRZ_TERMO IS NOT NULL   
                                                        BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_ATU,                        -- DATA DE CALCULO
                                                                                @DT_BASE_CALC,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                  @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORRECAO_C     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                @TIPO_COTACAO_C,                -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                    0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                NULL,
                                                                                '001',
                                                                                @UTILIZ_PREVIA,                 -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                                                                                @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                                                        END
                                                        ELSE
                                                        BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_ATU,                        -- DATA DE CALCULO
                                                                                @DT_BASE_CALC,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORRECAO_C     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                           @TIPO_COTACAO_C,                -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_CALC,                       -- DATA PARA IGPM PREVIO
                                                                                NULL,
                                                           '001',
                                                                                @UTILIZ_PREVIA,                 -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                                                                                @ACUMULAR_CDI,
                                                                                @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                                                        END                                                     

--2
                                                        -- GUARDAR A CORRECAO PARA UTILIZAR NO CALCULO DO PU UTEIS DE OPERACOES QUE NAO SEJAM DE FUNDOS
                                                        -- O CDI PROJETADO SERA UTILIZADO NO MTM, INDEPENDENTE DE SER FUNDO OU NAO
                                                        SELECT @CORRECAO_U_AUX = @CORRECAO_U
--SELECT @CORRECAO_U_AUX CORRECAO_U_AUX, @CORRECAO_U CORRECAO_U, @CORRECAO_C CORRECAO_C
--                                                      IF @CORRECAO_U_AUX = 0 
--                                                      BEGIN
                                                        SELECT @RFSC_VALOR = 0

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_UTIL,                       -- DATA DE CALCULO
                                                                                @DT_BASE_CALC,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORRECAO_U_AUX OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                               @DT_UTIL,                       -- DATA PARA IGPM PREVIO
                                                                                NULL,
                                                                                '001',
                                                                                @UTILIZ_PREVIA,                 -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                                                                                @ACUMULAR_CDI,
                                                                                @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

--SELECT @IDX_CODIGO IDX_CODIGO, @DT_ATU DT_ATU, @DT_VCMT_UTIL DT_VCMT_UTIL, @IDX_PC IDX_PC, @FER_CHAVE FER_CHAVE, @DT_UTIL DT_UTIL, @ACUMULAR_CDI ACUMULAR_CDI
---                                                     END
--select 'CORRECAO CLIENTE', @CORRECAO_U CORRECAO_U

                                                        -- RF2333.PRC

                                                        EXEC SANPP_RF_CALC_CDI_PROJETADO        @TITULO_ID,             -- IDENTIFICADOR DO TITULO
                                                                                                @ID_RPAC,               -- IDENTIFICADOR DO PERIODO DE REPACTUACAO
                                                                                                @EVE_CORRENTE,          -- TIPO DO EVENTO A SER UTILIZADO NA PROJECAO
                                                                                                @DT_UTIL,               -- DATA DE CALCULO DA PROJECAO
                                                                                                @DT_EVE,                -- DATA DO EVENTO A PROJETAR O CDI
                                                                                                @DT_BASE_CALC,          -- DATA BASE DE CALCULO
                                                                                                @DT_PROX_RPAC,          -- DATA DA PROXIMA REPACTUACAO OU VENCIMENTO
                                                                                                @IDX_CODIGO,            -- INDEXADOR
                                                                                                @IDX_PC,                -- PERCENTUAL DO INDEXADOR
                                                                                                @FER_CHAVE,             -- POSICAO PARA FERIADOS
                                                                                                @CORRECAO_U     OUTPUT, -- CDI PROJETADO
                                                                                                @ERRO           OUTPUT, -- RETORNA -1 => SEM ERROS
                                                                                                @ERR_MSG        OUTPUT, -- MENSAGEM DE ERRO
                                                                                                NULL,                   -- QUANDO NAO NULO, RETORNA RECORDSET PARA VB
                                                                               @EH_FUNDO



                                                        IF @ERRO <> -1
                                                        BEGIN

                                                                IF @VB_SQL IS NOT NULL
                                                                BEGIN
                                                                        SELECT
                                                                                @TIR_U          TIR_U,
                                                                                @TIR_C          TIR_C,
                                                           @ERRO           ERRO,
                                                                                @ERR_MSG        ERR_MSG

                                                                        RETURN
                                                                END
                                                                ELSE
                                                                        RETURN

                                                        END

                                                        --PARA OPERACOES DE CDI MAIS TAXA PRE UTILIZAR O CDI PROJETADO 
                                                        IF @EH_FUNDO = 'N' 
                                                        BEGIN
                                                                SELECT @V_FUT_M_JURS = @V_FUT_M_JURS * @CORRECAO_U
                                                        END 
                                                        ELSE IF @ATV_CODIGO <> 'CRI'
              -- By Charlie - 27/08/2015 - Ajuste CRI
                                                        BEGIN
                                                                SELECT 
                                                                        @CORRECAO_C = @CORRECAO_U
                                                        END


                                                        -- CALCULA A CORRECAO PARA O CALCULO DO PRAZO MEDIO
                                                        --PAÇOCA - 02/02/2010
                                                        IF @DT_VLRZ_TERMO IS NOT NULL   
                                                        BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_UTIL,                       -- DATA DE CALCULO
                                                                                @DT_BASE_CALC,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @FLOAT0,                        -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORC_PZ_MDIO   OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR
                                                        END
                                                        ELSE
                                                        BEGIN
                                                        

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_UTIL,                       -- DATA DE CALCULO
                                                                                @DT_BASE_CALC,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @DT_VCMT_UTIL,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @FLOAT0,                        -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORC_PZ_MDIO   OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                        @DT_UTIL,                       -- DATA PARA IGPM PREVIO
                                                                                @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR
                                                        END

                                                        IF @ERRO <> -1
                                                        BEGIN
                                                                SELECT
                                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                   RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                IF @VB_SQL IS NOT NULL
                                                                BEGIN
                                                                        SELECT
                                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                @ERRO                   ERRO,
                                                                                @ERR_MSG                ERR_MSG

                                                                        RETURN
                                                                END
                                                                ELSE
                                                                        RETURN

                                                        END

                                                END
                                                ELSE
                                                BEGIN


                                                        -- VARIACAO DO INDEXADOR POR DIAS UTEIS

                                                        SELECT
                                                                @RFSC_VALOR     = @FLOAT0

                                                        SELECT  @DT_ATU         = @DT_UTIL

                                                        -- Liana - 27/05 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE EH A DATA DO ANIVERSARIO DO PAPEL
                                                        if @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR = 1
                                                        BEGIN
                                                                SELECT @DT_ATU = @DT_CORRECAO_U
                                                        END

--                                                      IF (@FL_ARRED = 'S' and @TIPO_ATU_CETIP > 0)
--                                                              EXEC SANPP_RF_CALC_DT_ATUAL_CORRECAO
--                                                                      @TIPO_ATU_CETIP,
--                                                                      @DT_EVE,
--                                                                      @DT_UTIL,
--                                                                      @TIT_VENCTO,
--                                                                      @DT_BASE_CALC,
--                                                                      @DT_ATU OUTPUT,
--                                                                      @ATU_FLUXOS_FUT

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
                                SELECT @VDT_DATA_FIM = @DT_VCMT_UTIL
                            END

                                                        --PAÇOCA - 02/02/2010
                                                        IF @DT_VLRZ_TERMO IS NOT NULL   
                                                        BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_ATU,                        -- DATA DE CALCULO
                                                                                @VDT_DATA_INI,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @VDT_DATA_FIM,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORRECAO_U     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                NULL,
                                                                                '001',
                                                                                @UTILIZ_PREVIA,                 -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                                                                                @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 30/05/2011
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

-- if @@spid in (93, 105) select 2909 'rf0884', @DT_EVE DT_EVE, @CORRECAO_U CORRECAO_U, @VDT_DATA_INI VDT_DATA_INI, @VDT_DATA_FIM VDT_DATA_FIM
                                                        END
                                                        ELSE
                                                      BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_ATU,                        -- DATA DE CALCULO
                                                                                @VDT_DATA_INI,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @VDT_DATA_FIM,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORRECAO_U     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_UTIL,                       -- DATA PARA IGPM PREVIO
                                                                                NULL,
                                                                                '001',
                                                                                @UTILIZ_PREVIA,                 -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                                                                                @ACUMULAR_CDI,
                                                                                @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 30/05/2011
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

-- if @@spid in (93, 105) select 2942 'rf0884', @DT_EVE DT_EVE, @CORRECAO_U CORRECAO_U, @VDT_DATA_INI VDT_DATA_INI, @VDT_DATA_FIM VDT_DATA_FIM
                                                        END

                                                        IF @ERRO <> -1
                                                        BEGIN
                                                                SELECT
                                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                   RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                IF @VB_SQL IS NOT NULL
                                                                BEGIN
                                                                        SELECT
                                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                @ERRO                   ERRO,
                                                                                @ERR_MSG                ERR_MSG

                                                                        RETURN
                                                                END
                                                                ELSE
                                                                        RETURN

                                                        END


                                                        -- UTILIZA A MESMA CORRECAO PARA O PRAZO MEDIO

                                                        SELECT
                                                                @CORC_PZ_MDIO = @CORRECAO_U


                                                        -- VARIACAO DO INDEXADOR POR DIAS CORRIDOS

                                                        SELECT
                                                                @RFSC_VALOR     = @FLOAT0


                                                        SELECT  @DT_ATU         = @DT_CALC

                                                        -- LIANA - 22/06/2011 - PARA ATUALIZACAO ANUAL E PRIMEIRO EVENTO, A DATA LIMITE EH A DATA DO ANIVERSARIO DO PAPEL
                                                        IF @TIPO_ATU_CETIP = @TIPO_ATU_CETIP_ANUAL_EMIS AND @ID_SEQ_EVE_COR = 1
                                                        BEGIN
                                                                SELECT @DT_ATU = @DT_CORRECAO_C
                                                        END


--                                                      IF (@FL_ARRED = 'S' and @TIPO_ATU_CETIP > 0)
--                                                              EXEC SANPP_RF_CALC_DT_ATUAL_CORRECAO
--                                                                      @TIPO_ATU_CETIP,
--                                                                      @DT_EVE,
--                                                                      @DT_CALC,
--                                                                      @TIT_VENCTO,
--                                                                      @DT_BASE_CALC,
--                                                                      @DT_ATU OUTPUT,
--                                                          @ATU_FLUXOS_FUT

-- if @@spid = 80 select 2003 'rf0884', 'CORRECAO_C' = @CORRECAO_C,@DT_CALC, @DT_BASE_CALC DT_BASE_CALC, @DT_ATU DT_ATU

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
                                SELECT @VDT_DATA_FIM = @DT_VCMT_UTIL
                            END

                                                        --PAÇOCA - 02/02/2010
                                                        IF @DT_VLRZ_TERMO IS NOT NULL   
                                                        BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_ATU,                        -- DATA DE CALCULO
                                                                                @VDT_DATA_INI,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @VDT_DATA_FIM,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORRECAO_C     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                @TIPO_COTACAO_C,                -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                                                                NULL,
                                                                 '001',
                                                                                @UTILIZ_PREVIA,                 -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                                                                                @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

-- if @@spid in (93, 105) select 3057 'rf0884', @DT_EVE DT_EVE, @CORRECAO_C CORRECAO_C, @VDT_DATA_INI VDT_DATA_INI, @VDT_DATA_FIM VDT_DATA_FIM
                                                        END
                                                        ELSE
                                                        BEGIN

                                                                EXEC SIAN_SP_VARIACAO
                                                                                @IDX_CODIGO,                    -- INDEXADOR
                                                                                @DT_ATU,                        -- DATA DE CALCULO
                                                                                @VDT_DATA_INI,                  -- DATA BASE DE CALCULO OU EMISSAO
                                                                                @VDT_DATA_FIM,                  -- DATA DE VENCIMENTO
                                                                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                                                                @FLOAT0,                        -- 0.0
                                                                                @FLOAT0,                        -- 0.0
                                                                                '000',                          -- '000'
                                                                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                                                                @TIT_VENCTO,                    -- ANIVERSARIO
                                                                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                                                                @FLOAT1,                        -- 1.0
                                                                                @CORRECAO_C     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                                                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                                                                @TIPO_COTACAO_C,                -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                                                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                                                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                                                                @DT_CALC,                       -- DATA PARA IGPM PREVIO
                                                                                NULL,
                                                                                '001',
                                                                                @UTILIZ_PREVIA,                 -- INDICA SE UTILIZA PREVIA PARA O ATIVO
                                                                                @ACUMULAR_CDI,
                                     @SGL_SISTEMA = 'RDF',           -- LIANA - 22/11/2010
                                                                                @IC_T_ATC_CTP = @TIPO_ATU_CETIP -- LIANA - 22/06/2011
                                      , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                                      , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

-- if @@spid in (93, 105) select 3090 'rf0884', @DT_EVE DT_EVE, @CORRECAO_C CORRECAO_C, @VDT_DATA_INI VDT_DATA_INI, @VDT_DATA_FIM VDT_DATA_FIM
                                                        END                                                     

-- -- if @@spid = 58 select 2811 'rf0884', 'CORRECAO_C' = @CORRECAO_C,@DT_CALC

                                                        IF @ERRO <> -1
                                                  BEGIN
                                                                SELECT
                                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                                                                   RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                IF @VB_SQL IS NOT NULL
                                                                BEGIN
                                                                        SELECT
                                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                @ERRO                   ERRO,
                                                                                @ERR_MSG                ERR_MSG

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

                                                SELECT  @V_NOML_PCPL = ROUND(@V_NOML_PCPL,@VALORES_CASAS,@VALORES_ARRED),
                                                        @CORRECAO_C = ROUND(@CORRECAO_C,@CORRECAO_CASAS,@CORRECAO_ARRED),
                                                        @CORRECAO_U = ROUND(@CORRECAO_U,@CORRECAO_CASAS,@CORRECAO_ARRED)
                                        END

                                        -- SE FOR UM EVENTO DE ATUALIZACAO SOBRE AMORTIZACAO APLICA A VARIACAO SOBRE A AMORTIZACAO
                                        IF @EVE_CORRENTE = @EVE_CORC_AMTC
begin
                                                SELECT
                                                        @V_FUT_U_CORC   = ( @V_NOML_AMTC * (@CORRECAO_U - @FLOAT1)),
                                                        @V_FUT_C_CORC   = ( @V_NOML_AMTC * (@CORRECAO_C - @FLOAT1)),
                                                        @V_FUT_M_CORC   = ( @V_NOML_AMTC * (@CORRECAO_U - @FLOAT1)),
                                                        -- Cleber - 11/08/2006 - pgto de eventos em dias nao uteis para tit. com base dias corridos
                                                        @V_PU_CALD_CORC = 
                                                        (CASE WHEN (@FL_ARRED = 'S' AND (@RFX_FCALC = '001' OR @RFX_FCALC = '002' OR @RFX_FCALC = '032'  OR @ATV_CODIGO = 'L H'))
                                                        THEN    
                                                                ( @V_NOML_AMTC * (@CORRECAO_C - @FLOAT1))
                                                        ELSE
                                                                ( @V_NOML_AMTC * (@CORRECAO_U - @FLOAT1))
                                                        END),
                                                        @V_FUT_PZ_MDIO  = @V_FUT_PZ_MDIO + ( @V_NOML_AMTC * (@CORC_PZ_MDIO - @FLOAT1))

if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'C'
	, campo				= '@V_FUT_C_CORC @EVE_CORC_AMTC'
	, valor				= @V_FUT_C_CORC
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'C'
	, campo				= '@CORRECAO_C @EVE_CORC_AMTC'
	, valor				= @CORRECAO_C
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'C'
	, campo				= '@V_NOML_AMTC @EVE_CORC_AMTC'
	, valor				= @V_NOML_AMTC
end
--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_CORC @EVE_CORC_AMTC'
	, valor				= @V_FUT_U_CORC
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@CORRECAO_U @EVE_CORC_AMTC'
	, valor				= @CORRECAO_U
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@V_NOML_AMTC @EVE_CORC_AMTC'
	, valor				= @V_NOML_AMTC
end

end
                                        ELSE
                    BEGIN

-- if @@spid = 80 select 2090 'rf0884', @V_NOML_PCPL V_NOML_PCPL, @CORRECAO_U CORRECAO_U, @FL_ARRED FL_ARRED

                                                SELECT
                                                        @V_FUT_U_CORC   = ( @V_NOML_PCPL * (@CORRECAO_U - @FLOAT1)),
                                                        @V_FUT_C_CORC   = ( @V_NOML_PCPL * (@CORRECAO_C - @FLOAT1)),
                                                        @V_FUT_M_CORC   = ( @V_NOML_PCPL * (@CORRECAO_U - @FLOAT1)),
                                                        -- Cleber - 11/08/2006 - pgto de eventos em dias nao uteis para tit. com base dias corridos
                                                        @V_PU_CALD_CORC = 
                                                        (CASE WHEN (@FL_ARRED = 'S' AND (@RFX_FCALC = '001' OR @RFX_FCALC = '002' OR @RFX_FCALC = '032'  OR @ATV_CODIGO = 'L H'))
                                                        THEN    
          ( @V_NOML_PCPL * (@CORRECAO_C - @FLOAT1))
                                                        ELSE
                                                                ( @V_NOML_PCPL * (@CORRECAO_U - @FLOAT1)) 
                                                        END),
                                                        @V_FUT_PZ_MDIO  = @V_FUT_PZ_MDIO + ( @V_NOML_PCPL * (@CORC_PZ_MDIO - @FLOAT1))

if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'C'
	, campo				= '@V_FUT_C_CORC @EVE_CORC_PCPL'
	, valor				= @V_FUT_C_CORC
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'C'
	, campo				= '@CORRECAO_C @EVE_CORC_PCPL'
	, valor				= @CORRECAO_C
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'C'
	, campo				= '@V_NOML_PCPL @EVE_CORC_PCPL'
	, valor				= @V_NOML_PCPL
end
--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_CORC @EVE_CORC_PCPL'
	, valor				= @V_FUT_U_CORC
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@CORRECAO_U @EVE_CORC_PCPL'
	, valor				= @CORRECAO_U
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@V_NOML_PCPL @EVE_CORC_PCPL'
	, valor				= @V_NOML_PCPL
end

              IF @TIPO_ATU_CETIP = 4
                  SELECT @V_PU_CALD_CORC = 0

                    END

-- if @@spid = 58 SELECT 2894 'rf0884', @V_FUT_C_CORC V_FUT_C_CORC, @V_NOML_PCPL V_NOML_PCPL, @CORRECAO_C CORRECAO_C, @CORRECAO_U_AUX CORRECAO_U_AUX, @CORRECAO_U CORRECAO_U

                                END

                        END



                        -- FLUXOS DE AMORTIZACAO

                        IF EXISTS (
                                SELECT
                                        *
                                FROM
                                        SANT644_RF_AGENDA_EVENTO
                                WHERE
                                        TITULO_ID       = @TITULO_ID            AND
                                        ID_RPAC         = @ID_RPAC              AND
                                        ID_T_EVE        = @EVE_AMTC_PCPL        AND
                                        DT_EVE          = @DT_EVE
                 )
                        BEGIN

                                SELECT
                                        @V_FUT_U_PCPL   = V_PU_AMTC,
                                        @V_FUT_C_PCPL   = V_PU_AMTC,
                                        @V_FUT_M_PCPL   = V_PU_AMTC,
     @V_PU_CALD_PCPL = V_PU_AMTC,
                                        @V_FUT_PZ_MDIO  = @V_FUT_PZ_MDIO + V_PU_AMTC
                                FROM
                                        SANT644_RF_AGENDA_EVENTO
                                WHERE
                                        TITULO_ID       = @TITULO_ID            AND
                                        ID_RPAC         = @ID_RPAC              AND
                                        ID_T_EVE        = @EVE_AMTC_PCPL        AND
                                        DT_EVE          = @DT_EVE

                                -- CASO O TITULO UTILIZE ARREDONDAMENTO CETIP
                                IF @FL_ARRED = 'S'
                                        SELECT  @V_FUT_C_PCPL = ROUND(@V_FUT_C_PCPL,@VALORES_CASAS,@VALORES_ARRED),
                                                @V_FUT_U_PCPL = ROUND(@V_FUT_U_PCPL,@VALORES_CASAS,@VALORES_ARRED),
                                                @V_FUT_M_PCPL = ROUND(@V_FUT_M_PCPL,@VALORES_CASAS,@VALORES_ARRED)
                                SELECT
                                        @EVE_POR_DATA = RTRIM(@EVE_POR_DATA) + @EVE_AMTC_PCPL


if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'PCPL'
	, curva 			= 'C'
	, campo				= '@V_FUT_C_PCPL'
	, valor				= @V_FUT_C_PCPL
end
--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'PCPL'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_PCPL'
	, valor				= @V_FUT_U_PCPL
end

                        END

                        -- SE FOR INDEXADO PELO CDI, ACERTAR O VF

                        IF @IDX_CODIGO = 'CDI' --AND @EH_FUNDO = 'S'
                        BEGIN

                                -- BUSCAR CENARIO PRE NA DATA DO EVENTO

                                SELECT
                                        @CENARIO_PRE    = 'R$',
                                        @SGL_B_EXPS     = '022',
                                        @TX_CENARIO     = @FLOAT0,
                                        @CEN_DATA_BASE  = @DT_UTIL,
                                        @CEN_DATA       = @DT_FNAL_EVE


                                -- RF0885.PRC
-- 
                                
                                
                                IF @DT_VLRZ_TERMO IS NOT NULL
                                        SELECT @CEN_DATA_BASE = @DT_VLRZ_TERMO

                                EXEC SANPS_RF_BUSCA_CENARIO     @CENARIO_PRE,           -- SIGLA DA MOEDA OU INDEXADOR
                                                                @CEN_DATA_BASE  OUTPUT, -- DATA BASE
                                                                @CEN_DATA       OUTPUT, -- DATA DE PROJECAO DO CENARIO
                                                                @DT_PROX_RPAC,          -- VENCIMENTO DO TITULO
                                                                'N',                    -- INDICADOR DE CENARIO NO VENCIMENTO
                                                                @FER_CHAVE,             -- POSICAO PARA FERIADOS
                                                                'S', --@EH_FUNDO,               -- INDICADOR DE POSICAO SER FUNDO
                                                                @IDX_FCALC,             -- FORMA DE CALCULO DO INDEXADOR
                                                                @IDX_PRE,               -- INDICADOR PARA OPERACOES PRE
                                                                '001',                  -- FORMA DE CALCULO DA MTM
                                                                @FLOAT0,                -- TAXA UTEIS DA OPERACAO
                                                                @FLOAT0,                -- TAXA CORRIDOS DA OPERACAO
                                                                @SGL_B_EXPS     OUTPUT, -- EXPRESSAO DO CENARIO
                                                                @TX_CENARIO     OUTPUT, -- CENARIO PARA MARCACAO A MERCADO
                                                                @ERRO           OUTPUT, -- -1 = OK; 0 = ERRO; 1 = NAO ACHOU CENARIO
                                                                @ERR_MSG        OUTPUT  -- MENSAGEM DE ERRO

                                SELECT @CEN_DATA_BASE = @DT_UTIL

                                IF @ERRO <> -1
                                BEGIN
                                        SELECT
                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - ' + @ERR_MSG + ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                        IF @VB_SQL IS NOT NULL
                                        BEGIN
                                                SELECT
                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                        @ERRO                   ERRO,
                                                        @ERR_MSG                ERR_MSG

                                                RETURN
                                        END
                                        ELSE
                                                RETURN

                                END
-- SELECT @V_FUT_C_PCPL V_FUT_C_PCPL
-- SELECT @V_FUT_U_PCPL V_FUT_U_PCPL

--                      SELECT
--                                      @V_FUT_U_JURS = @V_FUT_U_JURS / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
--                                                      ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC ),
--                                      @V_FUT_U_CORC = @V_FUT_U_CORC / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
--                                                      ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC ),
--                                      @V_FUT_U_PCPL = @V_FUT_U_PCPL / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
--                                                      ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC ),
--                                      @V_FUT_C_JURS = @V_FUT_C_JURS / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
--                                                      ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC ),
--                                      @V_FUT_C_CORC = @V_FUT_C_CORC / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
--                                                      ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC ),
--                                      @V_FUT_C_PCPL = @V_FUT_C_PCPL / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
--                                                      ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC )
                                IF @EH_FUNDO = 'S' 
                                BEGIN
                                        SELECT
                                                @V_FUT_U_JURS = @V_FUT_U_JURS / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
                                                                ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC ),
                                                @V_FUT_U_CORC = @V_FUT_U_CORC / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
                                                             ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC ),
                                                @V_FUT_U_PCPL = @V_FUT_U_PCPL / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
                                                                ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC )

--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@TX_CENARIO CENARIO'
	, valor				= @TX_CENARIO
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@Q_DIAS_U_PARC CENARIO'
	, valor				= @Q_DIAS_U_PARC
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_JURS CENARIO'
	, valor				= @V_FUT_U_JURS
end




--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@TX_CENARIO CENARIO'
	, valor				= @TX_CENARIO
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@Q_DIAS_U_PARC CENARIO'
	, valor				= @Q_DIAS_U_PARC
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_CORC CENARIO'
	, valor				= @V_FUT_U_CORC
end



--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'PCPL'
	, curva 			= 'U'
	, campo				= '@TX_CENARIO CENARIO'
	, valor				= @TX_CENARIO
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'PCPL'
	, curva 			= 'U'
	, campo				= '@Q_DIAS_U_PARC CENARIO'
	, valor				= @Q_DIAS_U_PARC
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'PCPL'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_PCPL CENARIO'
	, valor				= @V_FUT_U_PCPL
end

                                END
                                ELSE
                                BEGIN
                                        SELECT @V_FUT_U_CORC = 0
--                                      SELECT @V_FUT_C_CORC = 0
                                        IF @EVE_CORRENTE = @EVE_CORC_AMTC OR @ID_SEQ_EVE_COR = 1
                                        BEGIN

                                                IF @CUPOM_TIR = 0 
                                                        SELECT @CUPOM_TIR = 1
                                                SELECT
                                                        @V_FUT_U_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL ),
                                                        @V_FUT_C_JURS   = (( @CUPOM_TIR - @FLOAT1 ) * @V_NOML_PCPL )

if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@V_FUT_C_JURS 3323'
	, valor				= @V_FUT_C_JURS
end
--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_JURS 3323'
	, valor				= @V_FUT_U_JURS
end

                                                IF @EVE_CORRENTE = @EVE_CORC_AMTC  
                                                BEGIN

                                                        -- BUSCAR O NOMINAL AMORTIZADO NA DATA
                                                        SELECT
                                                                @V_NOML_AMTC = SUM(V_PU_AMTC)
                                                        FROM
                                                                SANT644_RF_AGENDA_EVENTO
                                                        WHERE
                                                                TITULO_ID       = @TITULO_ID            AND
                                                                ID_RPAC         = @ID_RPAC              AND
                                                                ID_T_EVE        = @EVE_AMTC_PCPL        AND
                                                                DT_EVE          = @DT_EVE
                                
                                                        SELECT
                                                                @V_FUT_U_CORC   = ( @V_NOML_AMTC * (@CORRECAO_U_AUX - @FLOAT1))
--                                                              , @V_FUT_C_CORC = ( @V_NOML_AMTC * (@CORRECAO_U_AUX - @FLOAT1))

--                                                              @V_FUT_U_PCPL   = @V_NOML_PCPL

--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_CORC 3323 AMTC'
	, valor				= @V_FUT_U_CORC
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@CORRECAO_U_AUX 3323'
	, valor				= @CORRECAO_U_AUX
end

                                                END
                    ELSE
                                                BEGIN

                                                        SELECT
                                                                @V_FUT_U_CORC   = ( @V_NOML_PCPL * (@CORRECAO_U_AUX - @FLOAT1))
--                                                              , @V_FUT_C_CORC = ( @V_NOML_PCPL * (@CORRECAO_U_AUX - @FLOAT1))

--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@V_FUT_U_CORC 3323 PCPL'
	, valor				= @V_FUT_U_CORC
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@CORRECAO_U_AUX 3323'
	, valor				= @CORRECAO_U_AUX
end

-- if @@spid = 58 SELECT 3066 'rf0884', @V_FUT_C_CORC V_FUT_C_CORC, @V_NOML_PCPL V_NOML_PCPL, @CORRECAO_C CORRECAO_C, @CORRECAO_U_AUX CORRECAO_U_AUX, @CORRECAO_U CORRECAO_U
                                                END

--SELECT @CORRECAO_U_AUX CORRECAO_U_AUX, @CORRECAO_C CORRECAO_C_AUX


--select 2090 'rf0884', @V_NOML_AMTC V_NOML_AMTC, @CORRECAO_U_AUX CORRECAO_U, @FL_ARRED FL_ARRED
        
                                        END
                                END

                                IF @SGL_FCALC = '005'

                                        SELECT
                                                @V_FUT_M_JURS = @V_FUT_M_JURS / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
                                                      ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC ),
                                                @V_FUT_M_CORC = @V_FUT_M_CORC / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
                                                                ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC ),
                                                @V_FUT_M_PCPL = @V_FUT_M_PCPL / POWER( ( POWER( ( @TX_CENARIO / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
                                                                ( @IDX_PC / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC )



                        END




                        -- BUSCA O CENARIO

                        SELECT
                                @CALC_MTM_CDI   = 'S',
                                @CEN_DATA_BASE  = @DT_UTIL,
                                @CEN_DATA       = @DT_FNAL_EVE



                        -- VERIFICA A FORMA DE CALCULO

                        IF @SGL_MEDA <> '' AND @RFX_RESULTADO = '001'
                                AND NOT (@MERC_DT_AQUIS <> 'S' AND @DT_UTIL = @DT_AQUIS)
                        BEGIN

                                -- RF0885.PRC


                                IF @DT_VLRZ_TERMO IS NOT NULL
                                        SELECT @CEN_DATA_BASE = @DT_VLRZ_TERMO


                                EXEC SANPS_RF_BUSCA_CENARIO     @SGL_MEDA,              -- SIGLA DA MOEDA OU INDEXADOR
                                                                @CEN_DATA_BASE  OUTPUT, -- DATA BASE
                                                                @CEN_DATA       OUTPUT, -- DATA DE PROJECAO DO CENARIO
                                                                @DT_PROX_RPAC,          -- VENCIMENTO DO TITULO
                                                                @IC_CEN_VCMT,           -- INDICADOR DE CENARIO NO VENCIMENTO
                           @FER_CHAVE,             -- POSICAO PARA FERIADOS
                                                                'S', --@EH_FUNDO,               -- INDICADOR DE POSICAO SER FUNDO
                                                                @IDX_FCALC,             -- FORMA DE CALCULO DO INDEXADOR
                                                                @IDX_PRE,               -- INDICADOR PARA OPERACOES PRE
                                                                @RFX_RESULTADO,         -- FORMA DE CALCULO DA MTM
                                                                @TIR_U,                 -- TAXA UTEIS DA OPERACAO
                                                                @TIR_C,                 -- TAXA CORRIDOS DA OPERACAO
                                                                @SGL_B_EXPS     OUTPUT, -- EXPRESSAO DO CENARIO
                                                                @TIR_M          OUTPUT, -- CENARIO PARA MARCACAO A MERCADO
                                                                @ERRO           OUTPUT, -- -1 = OK; 0 = ERRO; 1 = NAO ACHOU CENARIO
                                                                @ERR_MSG        OUTPUT  -- MENSAGEM DE ERRO

                                IF @V_DURT_CALD_VOLTA IS NOT NULL
                                BEGIN
                                        SELECT @POS_DIA_CENARIO = POS_DIA_CENARIO FROM POSICAO (NOLOCK) WHERE POS_APELIDO = @FER_CHAVE
                                        EXEC SANPS_RF_CALCULA_TAXA_AJ_TERMO     @V_DURT_CALD_VOLTA,
                                                                                                                                                @TIR_M,
                                                                                                                                                @DT_VLRZ_TERMO,
                                                                                                                                                @DT_CALC,
                                                                                                                                                @TIT_VENCTO     ,
                                                                                                                                                @IC_CEN_VCMT    ,
                                                                                                                                                @FER_CHAVE      ,
                                                                                                                                                @EH_FUNDO       ,
                                                                                                                                                @IDX_CODIGO     ,       
                                                                                                                                                @TIR_M OUTPUT,@POS_DIA_CENARIO
                                        
                                END

                                SELECT @CEN_DATA_BASE = @DT_UTIL

                                IF @ERRO = 0
                                BEGIN
                                        SELECT
                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - ' + @ERR_MSG + ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                        IF @VB_SQL IS NOT NULL
                                        BEGIN
                                                SELECT
                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                     @ERRO                   ERRO,
                                                        @ERR_MSG                ERR_MSG

                                                RETURN
                                        END
                                        ELSE
                                                RETURN

                                END
                                ELSE
                                BEGIN   -- SE NAO ENCONTROU CENARIO E CONTABILIZA MERCADO, PARA!
                                        IF @ERRO = 1 AND @CTB_CURVA = 'M'
                                        BEGIN
                                                SELECT
                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - ' + @ERR_MSG + ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                IF @VB_SQL IS NOT NULL
                                                BEGIN
                                                        SELECT
                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                @ERRO                   ERRO,
                                                                @ERR_MSG                ERR_MSG

                                                        RETURN
                                                END
                                                ELSE
                                                        RETURN

                                        END
                                        ELSE

                         IF @ERRO = 1 AND @DT_ACABOU_CEN IS NULL
                                                        SELECT
                                                                @DT_ACABOU_CEN  = @CEN_DATA_BASE,
                                                                @CALC_MTM_CDI   = 'N'
                                                ELSE
                                                        IF @ERRO = 2 
                                                                SELECT 
                                                                        @CALC_MTM_CDI = 'N'


                                        SELECT
                                                @ERRO = -1


                                        /* Patricia 16/08/2004 - MtM CDI */
                                        IF @ERRO = -1 -- SEM ERRO
                                           IF @SGL_FCALC = '003' OR @SGL_FCALC = '006'  OR @SGL_FCALC = '007' AND @CALC_MTM_CDI = 'S' --CDI/PERC. CDI
                                           BEGIN
                                                SELECT
                                                        @SGL_MEDA_BASE  =       A.SGL_MEDA_B_ES_CMBL,
                                                        @SGL_BASE       =       A.SGL_B_EXPS
                                                FROM
                                                        SANT269_RF_CENARIO_MOS A,
                                                        SANT447_GE_CEN_SAN_APC B
                                                WHERE
                                                        A.SGL_MEDA      =       B.SGL_MEDA      AND
                                                        B.IDX_CODIGO    =       @SGL_MEDA


                                                SELECT  
                                                        @CEN_DATA_BASE  = @DT_UTIL,
                                                        @CEN_DATA       = @DT_FNAL_EVE



                                                /* BUSCA CENARIO PRE */
                                                -- RF0885.PRC
                                
                                                IF @DT_VLRZ_TERMO IS NOT NULL
                                                        SELECT @CEN_DATA_BASE = @DT_VLRZ_TERMO
                                                EXEC SANPS_RF_BUSCA_CENARIO     @SGL_MEDA_BASE,         -- SIGLA DA MOEDA OU INDEXADOR
                                                                                @CEN_DATA_BASE  OUTPUT, -- DATA BASE
                                                                                @CEN_DATA       OUTPUT, -- DATA DE PROJECAO DO CENARIO
                                                                                @DT_PROX_RPAC,          -- VENCIMENTO DO TITULO
                                                                                @IC_CEN_VCMT,           -- INDICADOR DE CENARIO NO VENCIMENTO
                                                                                @FER_CHAVE,             -- POSICAO PARA FERIADOS
                                                                                'S', --@EH_FUNDO,               -- INDICADOR DE POSICAO SER FUNDO
                                                                                @IDX_FCALC,             -- FORMA DE CALCULO DO INDEXADOR
                                                                                @IDX_PRE,               -- INDICADOR PARA OPERACOES PRE
                                                                                @RFX_RESULTADO,         -- FORMA DE CALCULO DA MTM
                                                                                @TIR_U,                 -- TAXA UTEIS DA OPERACAO
                                                                                @TIR_C,                 -- TAXA CORRIDOS DA OPERACAO
                                                                @SGL_BASE       OUTPUT, -- EXPRESSAO DO CENARIO
                                                                                @TX_PRE         OUTPUT, -- CENARIO ESTRUTURA CAMBIAL
                                                                                @ERRO           OUTPUT, -- -1 = OK; 0 = ERRO; 1 = NAO ACHOU CENARIO
                                                                                @ERR_MSG        OUTPUT  -- MENSAGEM DE ERRO

                                                SELECT  
                                                        @CEN_DATA_BASE  = @DT_UTIL

                                                IF @ERRO = 0
                                                BEGIN
                                                        SELECT
                                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - ' + @ERR_MSG + ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                        IF @VB_SQL IS NOT NULL
                                                        BEGIN
                                                                SELECT
                                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                        @ERRO                   ERRO,
                                                                        @ERR_MSG                ERR_MSG

                                                                RETURN
                                                        END
                                                        ELSE
                                                                RETURN

                                                END
                                   ELSE
                                                BEGIN   -- SE NAO ENCONTROU CENARIO E CONTABILIZA MERCADO, PARA!
                                                        IF @ERRO = 1 AND @CTB_CURVA = 'M'
                                                        BEGIN
                                                                SELECT
                                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - ' + @ERR_MSG + ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                                IF @VB_SQL IS NOT NULL
                                                                BEGIN
                                                                        SELECT
                                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                @ERRO                   ERRO,
                                                                                @ERR_MSG                ERR_MSG

                                                                        RETURN
                                                                END
                                                                ELSE
                                                                        RETURN

                                                        END
                                                        ELSE
                                                                IF @ERRO = 1 AND @DT_ACABOU_CEN IS NULL
                                                                        SELECT
                                                                                @DT_ACABOU_CEN  = @CEN_DATA_BASE

                                                        SELECT
                                                                @ERRO = -1
                                                END
                                        END
                                END
                        END



                        -- SE NAO POSSUIR CENARIO OU SIGLA ASSOCIADA E CTB CURVA NAO EH MERCADO

                        IF (@SGL_MEDA = '' OR @RFX_RESULTADO <> '001' OR @DT_ACABOU_CEN IS NOT NULL
                           OR (@MERC_DT_AQUIS <> 'S' AND @DT_UTIL = @DT_AQUIS)) AND @SGL_FCALC <> '007'
                        BEGIN

                                -- MERCADO = UTEIS

                                IF @RFX_RESULTADO = '007' OR @EH_FUNDO = 'S'

                                        SELECT
                                                @V_FUT_M_JURS   = @V_FUT_U_JURS,
                                                @V_FUT_M_CORC   = @V_FUT_U_CORC,
                                                @V_FUT_M_PCPL   = @V_FUT_U_PCPL,
                                                @TIR_M          = @TIR_U,
                                                @SGL_B_EXPS     = '022',
                                                @SGL_FCALC      = '001'

                                ELSE    -- MERCADO = CORRIDOS

                                        SELECT
                                                @V_FUT_M_JURS   = @V_FUT_C_JURS,
                                                @V_FUT_M_CORC   = @V_FUT_C_CORC,
                                                @V_FUT_M_PCPL   = @V_FUT_C_PCPL,
                                                @TIR_M          = @TIR_C,
                                                @SGL_B_EXPS     = '001',
                                                @SGL_FCALC      = '001'


                         SELECT
                                        @CEN_DATA_BASE  = @DT_UTIL,
                                        @CEN_DATA       = @DT_FNAL_EVE

                        END

                        -- DESCAPTALIZACAO PELA TIR
                        SELECT
                                @VAR_DESCAP_U   = @TIR_U,
                                @VAR_DESCAP_C   = @TIR_C,
                                @VAR_DESCAP_M   = @TIR_M

                        SELECT TOP 1 
                                @PU_COMPRA = RFM_PU
                        FROM 
                                RF_MOVIMENTACAO A,
                                RENDA_FIXA      B
                        WHERE
                                A.RF_CARACTERISTICA     =       B.RF_CARACTERISTICA     AND
                                A.RF_CARACTERISTICA     =       @RF_CARACTERISTICA      AND
                                A.RFM_OK                =       'S'                     AND
                                ((A.RFM_DT = 'A' AND B.ATV_AP = 'A') OR (A.RFM_DT = 'C' AND B.ATV_AP = 'P'))
                        ORDER BY
                                RFM_DATA


--                      -- SE OPERACAO FOI COMPRADA DE EMISSAO NAO DEVE HAVER DESCAPITALIZACAO
--                      IF @TIT_EMISSAO = @DT_AQUIS AND ABS(@ATV_LOTE - @PU_COMPRA) < 0.00001
--                              SELECT @VAR_DESCAP_U = 0

-- if @@spid = 105 select 2470 'rf0884', @DT_EVE DT_EVE, @TIR_U TIR_U


                        EXEC SIAN_CALC_JUROS    '022', 
                                                @VAR_DESCAP_U   OUTPUT,
                                                @DT_UTIL,
                                                @TIT_EMISSAO,
                                                @DT_FNAL_EVE,
                                                0,
                                                'A',
                                                @FER_CHAVE,
                                                'U',
                                                0,
                                                NULL,
                                                NULL,
                                                NULL,
                                                @TIPO_RESERVA

-- if @@spid = 105 select 2487 'rf0884', @DT_EVE DT_EVE, @VAR_DESCAP_U VAR_DESCAP_U

                        --UTILIZA A MESMA BASE DA TAXA DA F.C. JUROS DO TITULO PARA DESCAPITALIZAR 
                        --CRI NA BOLETAGEM (OPCIONAL)
                        IF (@RFX_FCALC = '035' OR @FL_DESCAP = 'S')
                                SELECT @FORMA_CALC = @RFX_FCALC
                        ELSE
                                SELECT @FORMA_CALC = '001'

                        EXEC SIAN_CALC_JUROS            @FORMA_CALC,
                                                        @VAR_DESCAP_C   OUTPUT,
                                                        @DT_CALC,
                                                        @TIT_EMISSAO,
                                                        @DT_FNAL_EVE,
                                                        0,
                                                        'A',
                                                        @FER_CHAVE,
                                                        'C',
                                                        0

                        -- PARA PRE, A DATA DE CALCULO EH A DATA BASE DO CENARIO        
                        IF @IDX_PRE <> 'N' AND ISNULL(RTRIM(@ATV_IDX_CUSTO), '') <> ''
                                SELECT
                                        @DT_INI_CEN = @CEN_DATA_BASE
                        ELSE
                                SELECT
                                        @DT_INI_CEN = @DT_UTIL


                        --Patricia 16/08/2004 - MtM CDI
                        IF @SGL_FCALC = '003' OR @SGL_FCALC = '006'  OR @SGL_FCALC = '007' AND  @CALC_MTM_CDI = 'S' --CDI / PERC. CDI
                        BEGIN                   

                                IF @SGL_FCALC IN ('006', '007')
                                BEGIN

--SELECT @TX_PRE TX_PRE, @Q_DIAS_U_PARC Q_DIAS_U_PARC
                                        IF @SGL_FCALC = '006'
                                        BEGIN
                                                SELECT
                                                        @VAR_DESCAP_M = POWER( ( POWER( ( @TX_PRE / @FLOAT100 + @FLOAT1 ), ( @FLOAT1 / @FLOAT252 ) ) - @FLOAT1 ) * 
                                                                        ( @TIR_M / @FLOAT100 ) + @FLOAT1, @Q_DIAS_U_PARC )
                                                        SELECT @CENARIO_CLASSE = @TIR_M
                                        END
                                        ELSE
                                        BEGIN


                                                -- CURVA UTILIZADA PARA MTM DO BANCO COM CALCULO BASEADO NO PREMIO
                                                SELECT 
                                                        @SGL_MEDA_AJUSTADO = CASE
                                                                                WHEN CD_ETTG = 1 THEN 'CDPR4'
                                                                                WHEN CD_ETTG = 3 THEN 'CDPR3'
                                                                                WHEN CD_ETTG = 4 THEN 'CDPR2'
                                                                                ELSE 'CDPR1'
                                                                             END
                                                FROM 
                                                        RENDA_FIXA A, 
                                                        SANT099_GE_EMITENTE B 
                                                WHERE 
                                                        A.RF_AGENTE = B.PFPJ_APELIDO AND 
                                                        A.RF_CARACTERISTICA = @RF_CARACTERISTICA

                                                -- PARA DEBENTURES  E CRI DEVE-SE UTILIZAR UMA CURVA ESPECIFICA
--                                              IF @ATV_cODIGO = 'DEBENTURES'
--                                              SELECT 
--                                                      @SGL_MEDA_AJUSTADO = CASE       
--                                                                              WHEN CD_ETTG = 1 THEN 'DEBR4'
--                                                                              WHEN CD_ETTG = 3 THEN 'DEBR3'
--                                                                              WHEN CD_ETTG = 4 THEN 'DEBR2'
--                                                                              ELSE 'CDPR1'
--                                                                           END
--                                              FROM 
--                                                      RENDA_FIXA A, 
--                                                      SANT099_GE_EMITENTE B 
--                                              WHERE 
--                                                      A.RF_AGENTE = B.PFPJ_APELIDO AND 
--                                                      A.RF_CARACTERISTICA = @RF_CARACTERISTICA
-- 
-- 
--                                              IF @ATV_cODIGO = 'CRI'
--                                              SELECT 
--                                                      @SGL_MEDA_AJUSTADO = 'CDPR0'
--                                              FROM 
--                                                      RENDA_FIXA A, 
--                                                      SANT099_GE_EMITENTE B 
--                                              WHERE 
--                                                      A.RF_AGENTE = B.PFPJ_APELIDO AND 
--                                                      A.RF_CARACTERISTICA = @RF_CARACTERISTICA

                                                EXEC SANPS_RF_BUSCA_CENARIO     @SGL_MEDA_AJUSTADO,     -- SIGLA DA MOEDA OU INDEXADOR
                                                                                @CEN_DATA_BASE  OUTPUT, -- DATA BASE
                                                                                @CEN_DATA       OUTPUT, -- DATA DE PROJECAO DO CENARIO
                                                                                @DT_PROX_RPAC,          -- VENCIMENTO DO TITULO
                                                                                @IC_CEN_VCMT,           -- INDICADOR DE CENARIO NO VENCIMENTO
                                                                                @FER_CHAVE,             -- POSICAO PARA FERIADOS
                                                                                'S', --@EH_FUNDO,               -- INDICADOR DE POSICAO SER FUNDO
                                                                                @IDX_FCALC,             -- FORMA DE CALCULO DO INDEXADOR
                                                                                @IDX_PRE,               -- INDICADOR PARA OPERACOES PRE
                                                                                @RFX_RESULTADO,         -- FORMA DE CALCULO DA MTM
                                                                                @TIR_U,                 -- TAXA UTEIS DA OPERACAO
                                                                                @TIR_C,                 -- TAXA CORRIDOS DA OPERACAO
  0               ,       -- EXPRESSAO DO CENARIO
                                                                                @CENARIO_CLASSE OUTPUT, -- CENARIO ESTRUTURA CAMBIAL
                                                                                @ERRO           OUTPUT, -- -1 = OK; 0 = ERRO; 1 = NAO ACHOU CENARIO
                                                                                @ERR_MSG        OUTPUT  -- MENSAGEM DE ERRO

                                                
                                                SELECT 
                                                        @INFLACAO_IMPLICITA =   POWER(@FLOAT1 + @TX_PRE / @FLOAT100,  @Q_DIAS_U_PARC / @FLOAT252) 
                                                                                / 
                                                                                POWER (@FLOAT1 + @TIR_M / @FLOAT100, @Q_DIAS_U_PARC / @FLOAT252) -@FLOAT1

                                                SELECT
                                                        @VAR_DESCAP_M = POWER((POWER(@FLOAT1 + @TX_PRE / @FLOAT100,  @FLOAT1 / @FLOAT252) -@FLOAT1) * @CENARIO_CLASSE / @FLOAT100 + 1, @Q_DIAS_U_PARC)
                                                                        /
                                                                        (@FLOAT1 + @INFLACAO_IMPLICITA)
                                                --NAO E NECESSARIO ESSA PASSAGEM POIS E APENAS UMA CONTA INVERSA, O RESULTADO SERA O MESMO
--                                              SELECT
--                                                      @VAR_DESCAP_M = POWER(@VAR_DESCAP_M, @FLOAT252 / @Q_DIAS_U_PARC) - @FLOAT1
--                                              SELECT
--                                                      @VAR_DESCAP_M = POWER(@VAR_DESCAP_M + @FLOAT1 , @Q_DIAS_U_PARC / @FLOAT252 ) 




                                        END


                                END
                                ELSE
                                BEGIN

                                        SELECT @DT_PROX_CEN = DATEADD(DD,1,@DT_INI_CEN)


                           -- DESCAPTALIZACAO TAXA PRE                             
                                        EXEC SIAN_CALC_JUROS    @SGL_BASE,
                                                                @TX_PRE         OUTPUT,
                                                                @DT_INI_CEN,
                                                                @DT_INI_CEN,
                                                                @DT_PROX_CEN,
                                                                0,
                                                                'A',
                                                                @FER_CHAVE,
                                                                'C',
                                                                0


                                        SELECT 
                                                @TX_PRE_EMIS  = (@TX_PRE - 1) *  (@TX_EMISSAO/@FLOAT100 ) + 1,
                                                @TX_PRE_MTM   = (@TX_PRE - 1) *  (@TIR_M/@FLOAT100) + 1


                                         EXEC   @Q_DIAS_PARC = SIAN_SP_QUANTAS_RESERVAS @DT_INI_CEN,
                                                                                        @DT_FNAL_EVE,
                                                                                        '003',
                                                                                        'A',
                                                                                        @FER_CHAVE,
                                                                                        0               

                                        IF @TX_PRE_EMIS <> 0 
                 SELECT 
                                                        @VAR_DESCAP_M   = POWER(@TX_PRE_MTM / @TX_PRE_EMIS, @Q_DIAS_PARC)

                                END
                        END
                        ELSE
                        BEGIN

                                -- DESCAPTALIZACAO PELO CENARIO 

                                IF @SGL_FCALC = '002' OR @SGL_FCALC = '005' --DESAGIO           

                                        SELECT 
                                                @CEN_DATA = @DT_FNAL_EVE


                                EXEC SIAN_CALC_JUROS    @SGL_B_EXPS,
                                                        @VAR_DESCAP_M   OUTPUT,
                                                        @DT_INI_CEN,
                                                        @DT_INI_CEN,
                                                        @CEN_DATA,
                                                        0,
                                                        'A',
                                                        @FER_CHAVE,
                                                        'C',
                                                        0
                        END



                        IF @MERC_DT_AQUIS <> 'S' AND @DT_UTIL = @DT_AQUIS

                                SELECT
                                        @V_FUT_M_JURS = @V_FUT_U_JURS,
                                        @V_FUT_M_CORC = @V_FUT_U_CORC,
                                        @V_FUT_M_PCPL = @V_FUT_U_PCPL,
                                        @VAR_DESCAP_M = @VAR_DESCAP_U

                        -- CASO O TITULO UTILIZE ARREDONDAMENTO CETIP
                        IF @FL_ARRED = 'S'
                                SELECT  @VAR_DESCAP_C = ROUND(@VAR_DESCAP_C,@DESCAP_CASAS,@DESCAP_ARRED),
                                        @VAR_DESCAP_U = ROUND(@VAR_DESCAP_U,@DESCAP_CASAS,@DESCAP_ARRED),
                                        @VAR_DESCAP_M = ROUND(@VAR_DESCAP_M,@DESCAP_CASAS,@DESCAP_ARRED)

                        -- CALCULA OS VPs
                        SELECT
         @V_PRS_U_JURS   = @V_FUT_U_JURS / @VAR_DESCAP_U,
                                @V_PRS_C_JURS   = @V_FUT_C_JURS / @VAR_DESCAP_C,
                                @V_PRS_M_JURS   = @V_FUT_M_JURS / @VAR_DESCAP_M,
                                @V_PRS_U_CORC   = @V_FUT_U_CORC / @VAR_DESCAP_U,
                                @V_PRS_C_CORC   = @V_FUT_C_CORC / @VAR_DESCAP_C,
                                @V_PRS_M_CORC   = @V_FUT_M_CORC / @VAR_DESCAP_M,
                                @V_PRS_U_PCPL   = @V_FUT_U_PCPL / @VAR_DESCAP_U,
                                @V_PRS_C_PCPL   = @V_FUT_C_PCPL / @VAR_DESCAP_C,
                                @V_PRS_M_PCPL   = @V_FUT_M_PCPL / @VAR_DESCAP_M

if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@VAR_DESCAP_C'
	, valor				= @VAR_DESCAP_C
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'C'
	, campo				= '@V_PRS_C_JURS'
	, valor				= @V_PRS_C_JURS
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'C'
	, campo				= '@VAR_DESCAP_C'
	, valor				= @VAR_DESCAP_C
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'C'
	, campo				= '@V_PRS_C_CORC'
	, valor				= @V_PRS_C_CORC
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'PCPL'
	, curva 			= 'C'
	, campo				= '@VAR_DESCAP_C'
	, valor				= @VAR_DESCAP_C
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'PCPL'
	, curva 			= 'C'
	, campo				= '@V_PRS_C_PCPL'
	, valor				= @V_PRS_C_PCPL
end
--
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@VAR_DESCAP_U'
	, valor				= @VAR_DESCAP_U
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'JURS'
	, curva 			= 'U'
	, campo				= '@V_PRS_U_JURS'
	, valor				= @V_PRS_U_JURS
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@VAR_DESCAP_U'
	, valor				= @VAR_DESCAP_U
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'CORC'
	, curva 			= 'U'
	, campo				= '@V_PRS_U_CORC'
	, valor				= @V_PRS_U_CORC
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'PCPL'
	, curva 			= 'U'
	, campo				= '@VAR_DESCAP_U'
	, valor				= @VAR_DESCAP_U
end
if exists (select 1 from tempdb..sysobjects where name = '##RF0884_MEMORIA')
begin
	insert into ##RF0884_MEMORIA select 
	  id_reg			= isnull((select count(*) from ##RF0884_MEMORIA), 0) +1
	, data_calculo		= @DT_CALC
	, data_evento		= @DT_EVE
	, tipo_evento		= 'PCPL'
	, curva 			= 'U'
	, campo				= '@V_PRS_U_PCPL'
	, valor				= @V_PRS_U_PCPL
end

-- select @DT_EVE DT_EVE, @VAR_DESCAP_U VAR_DESCAP_U

    -- Ajuste para calculo de operacao CDI + TAXA
    IF @IDX_CODIGO = 'CDI' and @EH_FUNDO = 'N' and @TIT_CUPOM > 0
    BEGIN
                                SELECT
                                @V_PRS_U_CORC   = @V_FUT_U_CORC,
                                @V_PRS_C_CORC   = @V_FUT_C_CORC,
                                @V_PRS_M_CORC   = @V_FUT_M_CORC
    END

-- if @@spid = 58 SELECT 3602 'rf0884', @V_PRS_C_CORC V_PRS_C_CORC, @V_FUT_C_CORC V_FUT_C_CORC, @VAR_DESCAP_C VAR_DESCAP_C

--                              SELECT
--                                      @V_FUT_U_JURS '@V_FUT_U_JURS',
--                                      @V_FUT_C_JURS '@V_FUT_C_JURS',
-- --                                   @V_FUT_M_JURS '@V_FUT_M_JURS',
--                                      @V_FUT_U_CORC '@V_FUT_U_CORC',
--                                      @V_FUT_C_CORC '@V_FUT_C_CORC',
-- --                                   @V_FUT_M_CORC '@V_FUT_M_CORC',
--          @V_FUT_U_PCPL '@V_FUT_U_PCPL',
--                                      @V_FUT_C_PCPL '@V_FUT_C_PCPL',
-- --                                   @V_FUT_M_PCPL '@V_FUT_M_PCPL',
--                                      @VAR_DESCAP_C '@VAR_DESCAP_C',
--                                      @V_PRS_C_JURS '@V_PRS_C_JURS',
--                                      @V_PRS_U_JURS '@V_PRS_U_JURS',
--                                      @V_PRS_C_PCPL '@V_PRS_C_PCPL'


-- if @@spid = 80 select 2617 'rf0884', @DT_CALC DT_CALC, @DT_EVE DT_EVE, @V_FUT_U_JURS V_FUT_U_JURS, @V_FUT_U_CORC V_FUT_U_CORC, @V_FUT_U_PCPL V_FUT_U_PCPL, @TIR_U TIR_U, @VAR_DESCAP_U VAR_DESCAP_U
--select 2618 'rf0884', @V_FUT_U_CORC V_FUT_U_CORC, @V_FUT_U_JURS V_FUT_U_JURS, @V_FUT_U_PCPL V_FUT_U_PCPL, @VAR_DESCAP_U VAR_DESCAP_U, @DT_EVE DT_EVE


                        -- PARA PRE, DA UM DIA DE CDI

                        IF @RFX_RESULTADO = '001' AND @IDX_PRE <> 'N'
                           AND @CEN_DATA_BASE <> @DT_CALC
                           AND ISNULL(RTRIM(@ATV_IDX_CUSTO), '') <> ''

                        BEGIN

                                -- CENARIO D-1 COM CUSTO CDI


                                EXEC SIAN_SP_VARIACAO
                                                        @ATV_IDX_CUSTO,
                                                        @DT_CALC,
                                                        @CEN_DATA_BASE,
                                                        @DT_CALC,
                                                        @FLOAT100,
                                                        @FLOAT1,
                                                        0,
                                                        '000',
                                                        @FER_CHAVE,
                                                        @DT_CALC,
                                         0,
                                                        1,
                                                        @CORRECAO_CDI   OUTPUT,
                                                        @ERRO           OUTPUT,
                                                        'C',
                                                        0,
                                                        @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                          , @PCH_FC_DT_ANV        = @VCH_FC_DT_ANV
                          , @PIT_Q_DEFS_IDXR      = @VIT_Q_DEFS_IDXR

                                IF @ERRO <> -1
                                BEGIN
                                        SELECT
                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR DE CUSTO: ' + 
                                                           RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                        IF @VB_SQL IS NOT NULL
                                        BEGIN
                                                SELECT
                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                        @ERRO                   ERRO,
                                                        @ERR_MSG                ERR_MSG

                                                RETURN
                                        END
                                        ELSE
                                                RETURN

                                END

                                SELECT
                                        @V_PRS_M_JURS   = @V_PRS_M_JURS * @CORRECAO_CDI,
                            @V_PRS_M_CORC   = @V_PRS_M_CORC * @CORRECAO_CDI,
                                        @V_PRS_M_PCPL   = @V_PRS_M_PCPL * @CORRECAO_CDI
                        END


                        -- SE ESTIVER PAGANDO ALGUM EVENTO NA DATA, GRAVA O VALOR
                        IF @FL_PAGA_EVE = 'S'
                        BEGIN

                                -- PONTO 1 - FAZ AQUI OS CALCULOS PARA CADA TIPO DE EVENTO, SE EXISTIR,
                                --           UTILIZANDO AS VARIAVEIS ALIMENTADAS NO COMECO DA PROC.

                                IF NOT (@FL_ARRED = 'S' AND (@RFX_FCALC = '001' OR @RFX_FCALC = '002' OR @RFX_FCALC = '032'  OR @ATV_CODIGO = 'L H') AND @DT_CALC > @DT_PG_EFT)
                                BEGIN

 --if @@spid = 93 
 --select 2686 'rf0884' , @FL_PAGA_EVE FL_PAGA_EVE, @V_PU_CALD_JURS V_PU_CALD_JURS, @V_PU_CALD_CORC V_PU_CALD_CORC, @V_PU_CALD_PCPL V_PU_CALD_PCPL

                                        UPDATE
                                                SANT644_RF_AGENDA_EVENTO
                                        SET
                                                V_PU_CALD =     CASE    WHEN ID_T_EVE = @EVE_JURS_AMTC THEN @V_PU_CALD_JURS
                                                                        WHEN ID_T_EVE = @EVE_JURS_PCPL THEN @V_PU_CALD_JURS
                                                                        WHEN ID_T_EVE = @EVE_JURS_DTBS THEN @V_PU_CALD_JURS
                                                                        WHEN ID_T_EVE = @EVE_CORC_AMTC THEN @V_PU_CALD_CORC
                                                                        WHEN ID_T_EVE = @EVE_CORC_PCPL THEN @V_PU_CALD_CORC
                                                                        WHEN ID_T_EVE = @EVE_AMTC_PCPL THEN @V_PU_CALD_PCPL
                                                                END,
                                              V_PU_PG =       CASE    WHEN IC_LBRC_EVE = 'N' AND ID_T_EVE = @EVE_JURS_AMTC THEN @V_PU_CALD_JURS
                                                                        WHEN IC_LBRC_EVE = 'N' AND ID_T_EVE = @EVE_JURS_PCPL THEN @V_PU_CALD_JURS
                                                                        WHEN IC_LBRC_EVE = 'N' AND ID_T_EVE = @EVE_JURS_DTBS THEN @V_PU_CALD_JURS
                                                                        WHEN IC_LBRC_EVE = 'N' AND ID_T_EVE = @EVE_CORC_AMTC THEN @V_PU_CALD_CORC
                                                                        WHEN IC_LBRC_EVE = 'N' AND ID_T_EVE = @EVE_CORC_PCPL THEN @V_PU_CALD_CORC
                                                                        WHEN IC_LBRC_EVE = 'N' AND ID_T_EVE = @EVE_AMTC_PCPL THEN @V_PU_CALD_PCPL
                                                                        ELSE V_PU_PG
                                                                END
                                                WHERE
                                                TITULO_ID       =  @TITULO_ID   AND
                                                ID_RPAC         =  @ID_RPAC     AND
                                                DT_EVE          >  @DT_UTIL_ANT AND
                                                DT_EVE          <= @DT_CALC
                                END


                                SELECT
                                        @FL_PAGA_EVE    = 'X'

                                -----------------------------------------------------------------------------------------
                                -- Liana - 28/07/2010 
                                -- No dia do vencto deve gravar os pus 
                                IF @TIT_VENCTO = @DT_CALC 
                                BEGIN
                                        -- CASO O TITULO UTILIZE ARREDONDAMENTO CETIP
                                        IF @FL_ARRED = 'S'
                                                SELECT  @V_PRS_C_JURS = ROUND(@V_PRS_C_JURS,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                        @V_PRS_C_CORC = ROUND(@V_PRS_C_CORC,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                        @V_PRS_C_PCPL = ROUND(@V_PRS_C_PCPL,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                        @V_PRS_U_JURS = ROUND(@V_PRS_U_JURS,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                        @V_PRS_U_CORC = ROUND(@V_PRS_U_CORC,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                        @V_PRS_U_PCPL = ROUND(@V_PRS_U_PCPL,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                        @V_PRS_M_JURS = ROUND(@V_PRS_M_JURS,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                        @V_PRS_M_CORC = ROUND(@V_PRS_M_CORC,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                        @V_PRS_M_PCPL = ROUND(@V_PRS_M_PCPL,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED)
        
                                        -- CALCULA OS PUs
        
                                        SELECT
                                                @RFS_PU_UTEIS    = @RFS_PU_UTEIS + @V_PRS_U_JURS + @V_PRS_U_CORC + @V_PRS_U_PCPL,
                                                @RFS_PU_CORRIDOS = @RFS_PU_CORRIDOS + @V_PRS_C_JURS + @V_PRS_C_CORC + @V_PRS_C_PCPL,
                                                --@RFS_PU_MERCADO        = @RFS_PU_MERCADO + @V_PRS_M_JURS + @V_PRS_M_CORC + @V_PRS_M_PCPL
                                                @RFS_PU_MERCADO = @RFS_PU_UTEIS 
                                END     
                                -----------------------------------------------------------------------------------------


                        END

      IF @FL_PAGA_EVE = 'X' AND @TIPO_ATU_CETIP <> 4
      BEGIN
          SELECT @FL_PAGA_EVE = 'N'
      END
                        ELSE IF @FL_PAGA_EVE    = 'N' OR @TIPO_ATU_CETIP = 4
                        BEGIN

                                -- CASO O TITULO UTILIZE ARREDONDAMENTO CETIP
                                IF @FL_ARRED = 'S'
                                        SELECT  @V_PRS_C_JURS = ROUND(@V_PRS_C_JURS,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                @V_PRS_C_CORC = ROUND(@V_PRS_C_CORC,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                @V_PRS_C_PCPL = ROUND(@V_PRS_C_PCPL,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                @V_PRS_U_JURS = ROUND(@V_PRS_U_JURS,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                @V_PRS_U_CORC = ROUND(@V_PRS_U_CORC,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                @V_PRS_U_PCPL = ROUND(@V_PRS_U_PCPL,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                @V_PRS_M_JURS = ROUND(@V_PRS_M_JURS,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                @V_PRS_M_CORC = ROUND(@V_PRS_M_CORC,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED),
                                                @V_PRS_M_PCPL = ROUND(@V_PRS_M_PCPL,@VALOR_PRS_CASAS,@VALOR_PRS_ARRED)

-- if @@spid = 80 select 2733 'rf0884', @RFS_PU_UTEIS RFS_PU_UTEIS, @V_PRS_U_JURS V_PRS_U_JURS, @V_PRS_U_CORC V_PRS_U_CORC, @V_PRS_U_PCPL V_PRS_U_PCPL

                                -- CALCULA OS PUs
--                              SELECT
--                                      @RFS_PU_UTEIS    = @RFS_PU_UTEIS + @V_PRS_U_JURS + @V_PRS_U_CORC + @V_PRS_U_PCPL,
--                                      @RFS_PU_CORRIDOS = @RFS_PU_CORRIDOS + @V_PRS_C_JURS + @V_PRS_C_CORC + @V_PRS_C_PCPL,
--                                      @RFS_PU_MERCADO  = @RFS_PU_MERCADO + @V_PRS_M_JURS + @V_PRS_M_CORC + @V_PRS_M_PCPL

          IF @TIPO_ATU_CETIP = 4 AND @DT_EVE = @DT_CALC
          BEGIN
              IF @DT_CALC <> @TIT_VENCTO
                                        SELECT
                                                @RFS_PU_UTEIS    = @RFS_PU_UTEIS + @V_PRS_U_CORC + @V_PRS_U_PCPL,
                                                @RFS_PU_CORRIDOS = @RFS_PU_CORRIDOS + @V_PRS_C_CORC + @V_PRS_C_PCPL,
                                                @RFS_PU_MERCADO  = @RFS_PU_MERCADO + @V_PRS_M_CORC + @V_PRS_M_PCPL
          END
          ELSE
                                SELECT
                                        @RFS_PU_UTEIS    = @RFS_PU_UTEIS + @V_PRS_U_JURS + @V_PRS_U_CORC + @V_PRS_U_PCPL,
                                        @RFS_PU_CORRIDOS = @RFS_PU_CORRIDOS + @V_PRS_C_JURS + @V_PRS_C_CORC + @V_PRS_C_PCPL,
                                        @RFS_PU_MERCADO  = @RFS_PU_MERCADO + @V_PRS_M_JURS + @V_PRS_M_CORC + @V_PRS_M_PCPL

-- if @@spid = 58 SELECT 3792 'rf0884', @RFS_PU_CORRIDOS RFS_PU_CORRIDOS, @V_PRS_C_JURS V_PRS_C_JURS, @V_PRS_C_CORC V_PRS_C_CORC, @V_PRS_C_PCPL V_PRS_C_PCPL
-- if @@spid = 80 select 2741 'rf0884', @RFS_PU_UTEIS RFS_PU_UTEIS

                                -- Cleber 02/10/2006 - Separacao de Juros e Atualiz. Monetaria para a proc RF0888.PRC
                                IF @EH_FUNDO <> 'S'
                                        SELECT @VAR_CORRECAO = @VAR_CORRECAO + @V_PRS_C_CORC

                                -- SE FOR A DATA FINAL DO PERIODO, CONSIDERAR PROXIMA REPACTUACAO

                                IF @DT_CALC < @DT_FNAL_EVE
                                BEGIN

                                        -- DIAS UTEIS PARA O CALCULO DA DURATION

                                        EXEC @Q_DU_DURATION = SIAN_SP_QUANTAS_RESERVAS  @DT_CALC,
                                   @DT_FNAL_EVE,
                                                                                        '003',
                                                                                        'A',
                                                                                        @FER_CHAVE,
                                                                                        0

                                SELECT 
                                        @SGL_MEDA_AJUSTADO = CASE
                                                                WHEN CD_ETTG = 1 THEN 'CDPR4'
                                                                WHEN CD_ETTG = 3 THEN 'CDPR3'
                                                                WHEN CD_ETTG = 4 THEN 'CDPR2'
                                                                ELSE 'CDPR1'
                                                             END
                                FROM 
                                        RENDA_FIXA A, 
                                        SANT099_GE_EMITENTE B 
                                WHERE 
                                        A.RF_AGENTE = B.PFPJ_APELIDO AND 
                                        A.RF_CARACTERISTICA = @RF_CARACTERISTICA




--SELECT @TIR_M TIR_M, @SGL_MEDA SGL_MEDA, @SGL_MEDA_AJUSTADO SGL_MEDA_AJUSTADO
                                SELECT 
                                        @INFLACAO_IMPLICITA =   POWER(@FLOAT1 + @TX_PRE / @FLOAT100,  @Q_DIAS_U_PARC / @FLOAT252) 
                                                                / 
                                                                POWER (@FLOAT1 + @TIR_M / @FLOAT100, @Q_DIAS_U_PARC / @FLOAT252) -@FLOAT1

--SELECT @VAR_DESCAP_M VAR_DESCAP_M, @INFLACAO_IMPLICITA INFLACAO_IMPLICITA

                                SELECT
                                        @VAR_DESCAP_M = POWER((POWER(@FLOAT1 + @TX_PRE / @FLOAT100,  @FLOAT1 / @FLOAT252) -@FLOAT1) * (@CENARIO_CLASSE / @FLOAT100) + 1, @Q_DIAS_U_PARC)
                                                        /
                                                        (@FLOAT1 + @INFLACAO_IMPLICITA)

--SELECT 'INSERINDO DURATION', @VAR_DESCAP_M VAR_DESCAP_M

                                INSERT INTO #DURATION 
                                        (
                                        DATA            ,
                                        PRAZO           ,
                                        VALOR_FUTURO    ,
                                        VALOR_EXATO,
                                        CEN_PRE,
                                        CENARIO_CLASSE,
                                        CEN_PRE_AJUSTADO,
                                        CP_IND_PRECO,
                                        CP_IND_PRECO_AJUST,
                                        VALOR_SUBORDINADO
                                        )
                                VALUES 
                                        (
                                        @DT_EVE,
                                        ISNULL(@Q_DU_DURATION, 0),
                                        --@V_PRS_M_JURS + @V_PRS_M_CORC + @V_PRS_M_PCPL,
                                        ISNULL(@V_FUT_M_JURS + @V_FUT_M_CORC + @V_FUT_M_PCPL, 0),
                                        0,
                                        ISNULL(@TX_PRE, 0),
                                        ISNULL(@CENARIO_CLASSE, 0),
                                        ISNULL(POWER((POWER(@FLOAT1 + @TX_PRE / @FLOAT100, @FLOAT1 / @FLOAT252) -@FLOAT1 )* (@CENARIO_CLASSE / @FLOAT100) + @FLOAT1, @FLOAT252) - @FLOAT1, 0),
                                        ISNULL(@TIR_M, 0),
                                        ISNULL(@VAR_DESCAP_M, 0),
                                        0
                                        )



--                              SELECT 
--                                      @VAR_DESCAP_M = POWER(@VAR_DESCAP_M, @FLOAT252 / @Q_DIAS_U_PARC) - @FLOAT1


                                        -- DIAS CORRIDOS PARA O CALCULO DO PRAZO MEDIO

                                        IF @IC_PZ_MDIO_RPAC = 'S'
                                                SELECT
                                                        @Q_DC_PZ_MEDIO  = DATEDIFF(Day, @DT_CALC, @DT_FNAL_EVE)
                                        ELSE
                                                SELECT
                                                        @Q_DC_PZ_MEDIO  = DATEDIFF(Day, @DT_CALC, @DT_EVE)

                                END
                                ELSE
                                BEGIN

                                        SELECT
                                                @DT_2_RPAC =    CASE    WHEN ID_RPAC = @MAX_RPAC THEN DT_FIM_RPAC
                                                                        ELSE DATEADD(DAY, 1, DT_FIM_RPAC)
                                                                END
                                        FROM
                                                SANT643_RF_REPAC_TITULO
                                        WHERE
                                                TITULO_ID       =       @TITULO_ID      AND
                                                ID_RPAC         =       (
                                                                        SELECT
                                                                                MIN(ID_RPAC)
                                                                        FROM
                                                                                SANT643_RF_REPAC_TITULO
                                                                        WHERE
                                                                                TITULO_ID       = @TITULO_ID    AND
                                                                         ID_RPAC         > @ID_RPAC      AND
                                                                                IC_EVE_CORC_TIT = 'N'
                                                                        )


                                        IF @DT_2_RPAC IS NULL
                                                SELECT
                                                        @DT_2_RPAC = @TIT_VENCTO



                                        -- DIAS UTEIS PARA O CALCULO DA DURATION

                                        IF @DT_EVE > @DT_2_RPAC
                                        BEGIN
                                                EXEC @Q_DU_DURATION = SIAN_SP_QUANTAS_RESERVAS  @DT_CALC,
                                                                                                @DT_2_RPAC,
                                                                                                '003',
                                                                                                'A',
                                                                                                @FER_CHAVE,
                                                                                                0
                                        END
                                        ELSE
                                        BEGIN
                                                EXEC @Q_DU_DURATION = SIAN_SP_QUANTAS_RESERVAS  @DT_CALC,
                                                                                                @DT_EVE,
                                                                                                '003',
                                                                                                'A',
                       @FER_CHAVE,
                                                                                                0
                                        END


                                        -- DIAS CORRIDOS PARA O CALCULO DO PRAZO MEDIO

                                        IF @IC_PZ_MDIO_RPAC = 'S'
                                                SELECT
                                                        @Q_DC_PZ_MEDIO  = DATEDIFF(Day, @DT_CALC, @DT_2_RPAC)
                                        ELSE
                                                SELECT
                                                        @Q_DC_PZ_MEDIO  = DATEDIFF(Day, @DT_CALC, @DT_EVE)

                                END







                                -- CALCULA O PRAZO MEDIO E A DURATION

                                SELECT
                                        @V_PZ_MDIO_CALD = @V_PZ_MDIO_CALD + @V_FUT_PZ_MDIO * @Q_DC_PZ_MEDIO,
                                        @V_DURT_CALD    = @V_DURT_CALD + (@V_PRS_M_JURS + @V_PRS_M_CORC + @V_PRS_M_PCPL) * @Q_DU_DURATION,
                                        @V_FUT_TOTL     = @V_FUT_TOTL + @V_FUT_PZ_MDIO


                                -- GRAVA NA TABELA DE SALDOS DOS EVENTOS
                                WHILE @ID_T_EVE < '6' --ALTERACAO CRI-LH 17/07/2006
                                BEGIN

                                        SELECT
                                                @ID_T_EVE = CONVERT(CHAR(1), CONVERT(INT, @ID_T_EVE) + 1)


                                        IF CHARINDEX (@ID_T_EVE , @EVE_POR_DATA) > 0 AND @RF_CARACTERISTICA <> ''
                                        BEGIN

                                                INSERT INTO SANT651_RF_SALDOS_EVENTO
                                                        (
                                                        RF_CARACTERISTICA,
                                                        RFS_DATA,
                                                DT_EVE,
                                                        ID_T_EVE,
                                                        V_PRST_EVE,
                                                        V_FUT_EVE
                                                        , CTB_CURVA
                                                        )
                                                SELECT
                                                        @RF_CARACTERISTICA,
                                                        @DT_CALC,
                                                        @DT_EVE,
                                                        @ID_T_EVE,
                                                        CASE    WHEN @ID_T_EVE = @EVE_JURS_AMTC AND @CTB_CURVA = 'U' THEN ISNULL(@V_PRS_U_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_PCPL AND @CTB_CURVA = 'U' THEN ISNULL(@V_PRS_U_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_DTBS AND @CTB_CURVA = 'U' THEN ISNULL(@V_PRS_U_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_CORC_PCPL AND @CTB_CURVA = 'U' THEN ISNULL(@V_PRS_U_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_CORC_AMTC AND @CTB_CURVA = 'U' THEN ISNULL(@V_PRS_U_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_AMTC_PCPL AND @CTB_CURVA = 'U' THEN ISNULL(@V_PRS_U_PCPL,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_AMTC AND @CTB_CURVA = 'C' THEN ISNULL(@V_PRS_C_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_PCPL AND @CTB_CURVA = 'C' THEN ISNULL(@V_PRS_C_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_DTBS AND @CTB_CURVA = 'C' THEN ISNULL(@V_PRS_C_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_CORC_PCPL AND @CTB_CURVA = 'C' THEN ISNULL(@V_PRS_C_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_CORC_AMTC AND @CTB_CURVA = 'C' THEN ISNULL(@V_PRS_C_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_AMTC_PCPL AND @CTB_CURVA = 'C' THEN ISNULL(@V_PRS_C_PCPL,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_AMTC AND @CTB_CURVA = 'M' THEN ISNULL(@V_PRS_M_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_PCPL AND @CTB_CURVA = 'M' THEN ISNULL(@V_PRS_M_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_DTBS AND @CTB_CURVA = 'M' THEN ISNULL(@V_PRS_M_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_CORC_PCPL AND @CTB_CURVA = 'M' THEN ISNULL(@V_PRS_M_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_CORC_AMTC AND @CTB_CURVA = 'M' THEN ISNULL(@V_PRS_M_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_AMTC_PCPL AND @CTB_CURVA = 'M' THEN ISNULL(@V_PRS_M_PCPL,0)
                                                                ELSE 0 
                                                        END     V_PRST_EVE,
                                                        CASE    WHEN @ID_T_EVE = @EVE_JURS_AMTC AND @CTB_CURVA = 'U' THEN ISNULL(@V_FUT_U_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_PCPL AND @CTB_CURVA = 'U' THEN ISNULL(@V_FUT_U_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_DTBS AND @CTB_CURVA = 'U' THEN ISNULL(@V_FUT_U_JURS,0)
                       WHEN @ID_T_EVE = @EVE_CORC_PCPL AND @CTB_CURVA = 'U' THEN ISNULL(@V_FUT_U_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_CORC_AMTC AND @CTB_CURVA = 'U' THEN ISNULL(@V_FUT_U_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_AMTC_PCPL AND @CTB_CURVA = 'U' THEN ISNULL(@V_FUT_U_PCPL,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_AMTC AND @CTB_CURVA = 'C' THEN ISNULL(@V_FUT_C_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_PCPL AND @CTB_CURVA = 'C' THEN ISNULL(@V_FUT_C_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_DTBS AND @CTB_CURVA = 'C' THEN ISNULL(@V_FUT_C_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_CORC_PCPL AND @CTB_CURVA = 'C' THEN ISNULL(@V_FUT_C_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_CORC_AMTC AND @CTB_CURVA = 'C' THEN ISNULL(@V_FUT_C_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_AMTC_PCPL AND @CTB_CURVA = 'C' THEN ISNULL(@V_FUT_C_PCPL,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_AMTC AND @CTB_CURVA = 'M' THEN ISNULL(@V_FUT_M_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_PCPL AND @CTB_CURVA = 'M' THEN ISNULL(@V_FUT_M_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_JURS_DTBS AND @CTB_CURVA = 'M' THEN ISNULL(@V_FUT_M_JURS,0)
                                                                WHEN @ID_T_EVE = @EVE_CORC_PCPL AND @CTB_CURVA = 'M' THEN ISNULL(@V_FUT_M_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_CORC_AMTC AND @CTB_CURVA = 'M' THEN ISNULL(@V_FUT_M_CORC,0)
                                                                WHEN @ID_T_EVE = @EVE_AMTC_PCPL AND @CTB_CURVA = 'M' THEN ISNULL(@V_FUT_M_PCPL,0)
                                                                ELSE 0
                                                        END     V_FUT_EVE
                                                        , @CTB_CURVA

            IF @CTB_CURVA <> 'U' AND @EH_CUPONADA = 'S'
            BEGIN
                                                INSERT INTO SANT651_RF_SALDOS_EVENTO
                                                        (
                    RF_CARACTERISTICA
                  , RFS_DATA
                  , CTB_CURVA
                  , DT_EVE
                  , ID_T_EVE
                  , V_PRST_EVE
                  , V_FUT_EVE
                                                        )
                                                SELECT
                    RF_CARACTERISTICA  = @RF_CARACTERISTICA
                  , RFS_DATA           = @DT_CALC
                  , CTB_CURVA          = 'U'
                  , DT_EVE             = @DT_EVE
                  , ID_T_EVE           = @ID_T_EVE
                  , V_PRST_EVE         = CASE
                                                                                        WHEN @ID_T_EVE = @EVE_JURS_AMTC THEN ISNULL(@V_PRS_U_JURS,0)
                                                                                        WHEN @ID_T_EVE = @EVE_JURS_PCPL THEN ISNULL(@V_PRS_U_JURS,0)
                                                                                        WHEN @ID_T_EVE = @EVE_JURS_DTBS THEN ISNULL(@V_PRS_U_JURS,0)
                                                                                        WHEN @ID_T_EVE = @EVE_CORC_PCPL THEN ISNULL(@V_PRS_U_CORC,0)
                                                                                        WHEN @ID_T_EVE = @EVE_CORC_AMTC THEN ISNULL(@V_PRS_U_CORC,0)
                                                                                     WHEN @ID_T_EVE = @EVE_AMTC_PCPL THEN ISNULL(@V_PRS_U_PCPL,0)
                                                                                 END
                  , V_FUT_EVE          = CASE
                                                                                        WHEN @ID_T_EVE = @EVE_JURS_AMTC THEN ISNULL(@V_FUT_U_JURS,0)
                                                                                        WHEN @ID_T_EVE = @EVE_JURS_PCPL THEN ISNULL(@V_FUT_U_JURS,0)
                                                                                        WHEN @ID_T_EVE = @EVE_JURS_DTBS THEN ISNULL(@V_FUT_U_JURS,0)
                                                                                        WHEN @ID_T_EVE = @EVE_CORC_PCPL THEN ISNULL(@V_FUT_U_CORC,0)
                                                                                        WHEN @ID_T_EVE = @EVE_CORC_AMTC THEN ISNULL(@V_FUT_U_CORC,0)
                                                                                        WHEN @ID_T_EVE = @EVE_AMTC_PCPL THEN ISNULL(@V_FUT_U_PCPL,0)
                                                                                 END
                                        END
                                        END
                                END



                                -- INCLUI OS FLUXOS PARA CALCULAR A TIR MERCADO

                                IF @SGL_MEDA <> '' AND @RFX_RESULTADO = '001'
                                BEGIN
                
                                        SELECT
                                                @ID_NUM_PARC    = @ID_NUM_PARC + 1


                                        IF @RFX_FCALC IN ('009', '022', '036')

                                                EXEC @Q_DIAS_PARC = SIAN_SP_QUANTAS_RESERVAS    @DT_UTIL,
                                @DT_FNAL_EVE,
                                                                                                '003',
                                                                                                'A',
                                                                                                @FER_CHAVE,
                                                                                                0
                                        ELSE

                                                SELECT
                                                        @Q_DIAS_PARC = DATEDIFF(day, @DT_UTIL, @DT_FNAL_EVE)


                                        -- INSERE OS FLUXOS

                                        INSERT INTO SANT646_RF_TIR
                                                (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
                                        SELECT
                                                @@SPID,
                                                @CD_CE,
                                                @ID_NUM_PARC,
                                                @Q_DIAS_PARC,
                                                @V_FUT_U_JURS + @V_FUT_U_CORC + @V_FUT_U_PCPL,
                                                @FLOAT0,
                                                @FLOAT0,
                                                @FLOAT0,
                                                @FLOAT0,
                                                @FLOAT0

                                END
                        END
                END     -- FINAL LOOP EVENTOS


    -- TRATAMENTO PARA NÂO DAR ERRO NO CÁLCULO DA TIR MERCADO
    SELECT @RFS_PU_MERCADO = ISNULL(@RFS_PU_MERCADO, @RFS_PU_UTEIS)


                IF @SGL_FCALC = '006'
                        SELECT @SGL_MEDA_AJUSTADO = @SGL_MEDA

                SELECT @PREMIO = 0 
                SELECT TOP 1 
                        @PREMIO         =       RFM_PREMIO,
                        @DATA_COMPRA    =       RFM_DATA,
                        @PU_COMPRA      =       RFM_PU,
                        @ATV_CODIGO     =       B.ATV_CODIGO
                        
                FROM 
                        RF_MOVIMENTACAO A, 
                        RENDA_FIXA B 
                WHERE 
                        A.RF_CARACTERISTICA     =       B.RF_CARACTERISTICA     AND 
                        A.RF_CARACTERISTICA     =       @RF_CARACTERISTICA      AND
                        A.RFM_OK                =       'S'                     AND
                        ((RFM_DT = 'A' AND ATV_AP = 'A') OR (RFM_dT = 'C' AND ATV_AP = 'P'))    AND
                        A.RFM_DATA              <=      @DT_CALC
                ORDER BY 
                        RFM_DATA DESC

--SELECT @ATV_CODIGO ATV_CODIGO
--SELECT @IC_APC IC_APC, @SGL_MEDA SGL_MEDA

--              IF (@ATV_CODIGO = 'LFS' OR @ATV_CODIGO = 'CRI') AND @EH_FUNDO <> 'S'
                if @EH_FUNDO <> 'S' AND @IC_APC = 'S'
                BEGIN

--SELECT @IDX_FCALC IDX_FCALC

                        --SE HOUVE COMPRA NA DATA, RECALCULA O PREMIO
                        IF @DT_CALC = @DATA_COMPRA AND @RFX_RESULTADO = '001' 
                        BEGIN
                                EXEC SANPP_RF_CALC_PREMIO_MTM @IDX_FCALC, @PU_COMPRA, @PREMIO OUTPUT, @TIT_CUPOM
                                UPDATE RF_MOVIMENTACAO SET RFM_PREMIO = @PREMIO WHERE RF_CARACTERISTICA = @RF_CARACTERISTICA AND RFM_DATA = @DATA_COMPRA
        
--SELECT @PREMIO PREMIO

                        END

                        IF @IDX_FCALC = '007'
        
                        BEGIN
                        
--                              SELECT @PREMIO PREMIO
        
                                UPDATE #DURATION SET VALOR_SUBORDINADO = VALOR_FUTURO / (CP_IND_PRECO_AJUST * POWER(@FLOAT1 + @PREMIO, PRAZO/@FLOAT252)) WHERE (CP_IND_PRECO_AJUST * POWER(@FLOAT1 + @PREMIO, PRAZO/@FLOAT252)) <> 0
        
                        END
        
                        IF @IDX_FCALC = '' 
        
                        BEGIN
        
        
                        
--                              SELECT @PREMIO PREMIO
        
        
                                UPDATE #DURATION SET VALOR_SUBORDINADO = VALOR_FUTURO / POWER((CEN_PRE_AJUSTADO + @FLOAT1) * (@PREMIO + @FLOAT1), PRAZO / @FLOAT252)
        
                        END 
        
        
                        IF @IDX_FCALC = '000' 
        
                        BEGIN
--                              SELECT @TIT_CUPOM TIT_CUPOM
--                              SELECT CENARIO_CLASSE , @PREMIO PREMIO, CASE WHEN (CENARIO_CLASSE / @FLOAT100 + @PREMIO) >=1 THEN (CENARIO_CLASSE / @FLOAT100 + @PREMIO) ELSE 1 END  FROM #DURATION
                                IF @TIT_CUPOM = 0 
--                                      UPDATE #DURATION SET VALOR_SUBORDINADO = VALOR_FUTURO / POWER((POWER(CEN_PRE / @FLOAT100 + @FLOAT1, @FLOAT1 / @FLOAT252) - @FLOAT1) * (CENARIO_CLASSE / @FLOAT100 + @PREMIO) + @FLOAT1, PRAZO)
                                        UPDATE #DURATION SET VALOR_SUBORDINADO = VALOR_FUTURO / POWER((POWER(CEN_PRE / @FLOAT100 + @FLOAT1, @FLOAT1 / @FLOAT252) - @FLOAT1) * CASE WHEN (CENARIO_CLASSE / @FLOAT100 + @PREMIO) >=1 THEN (CENARIO_CLASSE / @FLO
AT100 + @PREMIO) ELSE @FLOAT1 END + @FLOAT1, PRAZO)
                                ELSE
--                                      UPDATE #DURATION SET VALOR_SUBORDINADO = VALOR_FUTURO / (POWER((POWER(@FLOAT1 + CEN_PRE / @FLOAT100, @FLOAT1 / @FLOAT252) - @FLOAT1) * (CENARIO_CLASSE / @FLOAT100) + @FLOAT1, PRAZO) * POWER(@FLOAT1 + @PREMIO, PRAZO 
/ @FLOAT252))
                                        UPDATE #DURATION SET VALOR_SUBORDINADO = VALOR_FUTURO / (POWER((POWER(@FLOAT1 + CEN_PRE / @FLOAT100, @FLOAT1 / @FLOAT252) - @FLOAT1) * CASE WHEN (CENARIO_CLASSE / @FLOAT100 + @PREMIO) >=1 THEN (CENARIO_CLASSE / @FLO
AT100 + @PREMIO) ELSE @FLOAT1 END + @FLOAT1, PRAZO) * POWER(@FLOAT1 + @PREMIO, PRAZO / @FLOAT252))
                        END
        
--      SELECT * FROM #DURATION
                        -- Liana - 07/12/2010
                        IF @TIT_VENCTO <> @DT_CALC 
                        BEGIN
                                SELECT @RFS_PU_MERCADO = SUM(ISNULL(VALOR_SUBORDINADO, 0))  FROM #DURATION

                        END
--SELECT @RFS_PU_MERCADO RFS_PU_MERCADO
                END

                -- CALCULA O PRAZO MEDIO E A DURATION

                IF @V_FUT_TOTL <> @FLOAT0

                        SELECT
                                @V_PZ_MDIO_CALD = @V_PZ_MDIO_CALD / @V_FUT_TOTL


                IF @RFS_PU_MERCADO <> @FLOAT0

                        SELECT
                                @V_DURT_CALD = @V_DURT_CALD / @RFS_PU_MERCADO





                -- GRAVA O FLUXO INICIAL E CALCULA A TIR MERCADO

                IF @SGL_MEDA <> '' AND @RFX_RESULTADO = '001' AND @DT_UTIL < @TIT_VENCTO
                BEGIN
                
                        SELECT
                                @ID_NUM_PARC    = @FLOAT0,
                                @Q_DIAS_PARC    = @FLOAT0


                        IF @RFX_FCALC IN ('009', '022', '036')

                                SELECT
                                        @BASE_TIR       = @FLOAT252
                        ELSE

                                SELECT
                                        @BASE_TIR       = @FLOAT360



                        -- INSERE O FLUXO INICIAL

                        INSERT INTO SANT646_RF_TIR
                                (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
                        SELECT
                                @@SPID,
                                @CD_CE,
                                @ID_NUM_PARC,
                                @Q_DIAS_PARC,
                                - @RFS_PU_MERCADO,
                                @FLOAT0,
                                @FLOAT0,
                                @FLOAT0,
                                @FLOAT0,
                                @FLOAT0



                        -- APURA A TIR MERCADO

                        -- RF0797.PRC
                        IF @RFS_PU_MERCADO <> 0
                        BEGIN
                                EXEC SANPP_RF_TIR       @@SPID,
                                                        @CD_CE,
                                                        @BASE_TIR,
                                                        @ERRO           OUTPUT,
                                                        @TIR_M          OUTPUT


                                IF @ERRO <> -1
                                BEGIN
                                        SELECT
                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - PROBLEMAS NO CALCULO DA TIR MERCADO.' + 
                                                           ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)
        
                                        IF @VB_SQL IS NOT NULL
                                        BEGIN
                                                SELECT
                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                        @ERRO                   ERRO,
                                                        @ERR_MSG                ERR_MSG
   
                                                RETURN
                                        END
                                        ELSE
                                                RETURN
        
                                END

                        END

                END
                ELSE
                        SELECT
                                @TIR_M  = @FLOAT0
            END
            ELSE
            BEGIN       -- OPERACOES PADRONIZADAS
                
                SELECT
                        @DT_EVE         = @DT_CALC,
                        @Q_DIAS_PARC    = @FLOAT0       


                -- CASO NAO SEJA DIA UTIL, O CALCULO EH SIMILAR AO DIA ANTERIOR

                EXEC @EH_RESERVA = SIAN_E_RESERVA       @DT_EVE,
                                                        'A',
                                                        @FER_CHAVE,
                                                        0

                IF @EH_RESERVA <> -1    -- NAO EH DIA UTIL

                        EXEC SIAN_SP_RESERVA_ANTERIOR   @FER_CHAVE,
                                                        'A',
                                                        @DT_EVE         OUTPUT,
                                                        0


                -- VARIACAO DO INDEXADOR POR DIAS UTEIS

                SELECT
                        @RFSC_VALOR     = @FLOAT0



-- if @@spid = 80 SELECT 3075 'rf0884', @CORRECAO_U CORRECAO_U
                --PAÇOCA - 02/02/2010
                IF @DT_VLRZ_TERMO IS NOT NULL   
                BEGIN
                        EXEC SIAN_SP_VARIACAO
                                        @IDX_CODIGO,                    -- INDEXADOR
                                        @DT_EVE,                        -- DATA DE CALCULO
                                        @RF_BASE_CALC,                  -- DATA BASE DE CALCULO OU EMISSAO
            @TIT_VENCTO,                    -- DATA DE VENCIMENTO
                                        @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                        @FLOAT0,                        -- 0.0
                                        @FLOAT0,                        -- 0.0
                                        '000',                          -- '000'
                                        @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                        @TIT_VENCTO,                    -- ANIVERSARIO
                                        @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                        @FLOAT1,                        -- 1.0
                                        @CORRECAO_U     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                        @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                        'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                        0,                              -- 0 PARA RETORAR NA VARIAVEL
                                        @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                        @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                        @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                END
                ELSE
                BEGIN

                        EXEC SIAN_SP_VARIACAO
                                        @IDX_CODIGO,                    -- INDEXADOR
                                        @DT_EVE,                        -- DATA DE CALCULO
                                        @RF_BASE_CALC,                  -- DATA BASE DE CALCULO OU EMISSAO
                                        @TIT_VENCTO,                    -- DATA DE VENCIMENTO
                                        @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                        @FLOAT0,                        -- 0.0
                                        @FLOAT0,                        -- 0.0
                                        '000',                          -- '000'
                                        @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                        @TIT_VENCTO,                    -- ANIVERSARIO
                                        @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                        @FLOAT1,                        -- 1.0
                                        @CORRECAO_U     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                        @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                        'U',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                        0,                              -- 0 PARA RETORAR NA VARIAVEL
                                        @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                        @DT_CALC,                       -- DATA PARA IGPM PREVIO
                                        @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010

                END


-- if @@spid = 80 SELECT 3097 'rf0884', @CORRECAO_U CORRECAO_U

                IF @ERRO <> -1
                BEGIN
                        SELECT
                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                           RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                        IF @VB_SQL IS NOT NULL
                        BEGIN
                                SELECT
                     @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                        @ERRO                   ERRO,
                                        @ERR_MSG                ERR_MSG

                                RETURN
                        END
                        ELSE
                                RETURN
                END

                -- VARIACAO DO INDEXADOR POR DIAS CORRIDOS
                SELECT
                        @RFSC_VALOR     = @FLOAT0

                

                --PAÇOCA
                IF @DT_VLRZ_TERMO IS NOT NULL   
                BEGIN



                        EXEC SIAN_SP_VARIACAO
                                @IDX_CODIGO,                    -- INDEXADOR
                                @DT_CALC,                       -- DATA DE CALCULO
                                @RF_BASE_CALC,                  -- DATA BASE DE CALCULO OU EMISSAO
                                @TIT_VENCTO,                    -- DATA DE VENCIMENTO
                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                @FLOAT0,                        -- 0.0
                                @FLOAT0,                        -- 0.0
                                '000',                          -- '000'
                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                @TIT_VENCTO,                    -- ANIVERSARIO
                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                @FLOAT1,                        -- 1.0
                                @CORRECAO_C     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                @ERRO        OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                'C',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                @DT_VLRZ_TERMO,                 -- DATA PARA IGPM PREVIO
                                @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                END
                ELSE
                BEGIN


                        EXEC SIAN_SP_VARIACAO
                                @IDX_CODIGO,                    -- INDEXADOR
                                @DT_CALC,                       -- DATA DE CALCULO
                                @RF_BASE_CALC,                  -- DATA BASE DE CALCULO OU EMISSAO
                                @TIT_VENCTO,                    -- DATA DE VENCIMENTO
                                @IDX_PC,                        -- PERCENTUAL (CDI OU REF)
                                @FLOAT0,                        -- 0.0
                                @FLOAT0,                        -- 0.0
                                '000',                          -- '000'
                                @FER_CHAVE,                     -- POSICAO PARA CALCULO DE FERIADOS
                                @TIT_VENCTO,                    -- ANIVERSARIO
                                @RFSC_VALOR     OUTPUT,         -- RETORNA A ULTIMA COTACAO
                                @FLOAT1,                        -- 1.0
                                @CORRECAO_C     OUTPUT,         -- VARIAVEL PARA RETORNAR A VARIACAO
                                @ERRO           OUTPUT,         -- RETORNA EVENTUAIS ERROS
                                'C',                            -- 'U' -> UTEIS; 'C' -> CORRIDOS
                                0,                              -- 0 PARA RETORAR NA VARIAVEL
                                @CETIP_SELIC,                   -- TIT. PRIVADOS (C) E TIT. PUBLICOS (S)
                                @DT_CALC,                       -- DATA PARA IGPM PREVIO
                                @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010
                END


                IF @ERRO <> -1
                BEGIN
                        SELECT
                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR: ' + 
                                           RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                        IF @VB_SQL IS NOT NULL
                        BEGIN
                                SELECT
                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                        @ERRO                   ERRO,
                                        @ERR_MSG                ERR_MSG

                                RETURN
                        END
                        ELSE
                                RETURN

                END


                -- PARA CALCULO DA CORRECAO

                SELECT
                        @VAR_CORRECAO   = @CORRECAO_C


                SELECT
                        @DT_INI_ORI     = @TIT_VENCTO,
                        @DT_FIM_ORI     = @TIT_VENCTO,
                        @DT_FIM         = @TIT_VENCTO


                EXEC @EH_RESERVA = SIAN_E_RESERVA       @DT_FIM,
                                                        'A',
                                                        @FER_CHAVE,
                                                        0

                IF @EH_RESERVA <> -1    -- NAO EH DIA UTIL

EXEC SIAN_SP_PROXIMA_RESERVA    'A',
                                                        @FER_CHAVE,
                                                        @DT_FIM         OUTPUT,
                                                        0

                SELECT
                        @DT_INI = @DT_FIM


                -- CALCULA OS FLUXOS
                

                WHILE @DT_INI > @DT_EVE OR
                     (@DT_INI = @DT_CALC AND @DT_CALC >= @TIT_VENCTO)
                BEGIN

                        SELECT
                                @DT_INI_ORI = DateAdd(month, (-1 * @ATV_PRZ_PGJUROS), @DT_FIM_ORI)
                                

                        IF @DT_INI_ORI < @TIT_EMISSAO AND NOT (@IDX_FCALC = '007' AND @CETIP_SELIC = 'S')

                                SELECT
                                        @DT_INI_ORI = @TIT_EMISSAO


                        SELECT
                                @DT_INI = @DT_INI_ORI

                        EXEC @EH_RESERVA = SIAN_E_RESERVA       @DT_INI,
                                                                'A',
                                                                @FER_CHAVE,
                                                                0

                        IF @EH_RESERVA <> -1    -- NAO EH DIA UTIL
                           AND NOT (@IDX_FCALC = '007' AND @CETIP_SELIC = 'S')

                                EXEC SIAN_SP_PROXIMA_RESERVA    'A',
                                                                @FER_CHAVE,
                                                                @DT_INI         OUTPUT,
                                                                0


                        IF @IDX_FCALC = '007'

                                SELECT
                                        @DT_FIM = @DT_FIM_ORI


-- if @@spid = 80 SELECT 3234 'rf0884', @CUPOM_TIR CUPOM_TIR, @TIT_CUPOM TIT_CUPOM

                        -- VARIACAO DO CUPOM

                  -- RF0887.PRC

                        EXEC SANPP_RF_CALC_CUPOM_TIR    @TIT_EMISSAO,
                                                        @TIT_VENCTO,
                                                        @DT_INI,
                                                        @DT_FIM,
                                                        @FER_CHAVE,
                                                        @FCALC_CUPOM,
                                                        @TIT_CUPOM,
                                                        @RFX_FCALC,
                                                        @TIT_EMISSAO,
                                                        @ATV_PRZ_PGJUROS,
                                                        @CUPOM_TIR              OUTPUT


-- if @@spid = 80 SELECT 3252 'rf0884', @CUPOM_TIR CUPOM_TIR

                        -- BUSCA O CENARIO

                        SELECT
                                @CEN_DATA       = @DT_FIM,
                                @CEN_DATA_BASE  = @DT_EVE

                        IF @SGL_MEDA <> '' AND @RFX_RESULTADO = '001' AND @DT_ACABOU_CEN IS NULL
                           AND NOT (@MERC_DT_AQUIS <> 'S' AND @DT_EVE = @DT_AQUIS)
                        BEGIN
-- 
                                
                                IF @DT_VLRZ_TERMO IS NOT NULL
                                        SELECT @CEN_DATA_BASE = @DT_VLRZ_TERMO --+1
                                        --SELECT @CEN_DATA_BASE = '2009-10-16'

-- if @@spid = 80 SELECT 3263 'rf0884', @IC_OPRC_NAO_PADR IC_OPRC_NAO_PADR
                                -- RF0885.PRC
                                EXEC SANPS_RF_BUSCA_CENARIO     @SGL_MEDA,              -- SIGLA DA MOEDA OU INDEXADOR
                                                                @CEN_DATA_BASE  OUTPUT, -- DATA BASE
                 @CEN_DATA       OUTPUT, -- DATA DE PROJECAO DO CENARIO
                                                                @TIT_VENCTO,            -- VENCIMENTO DO TITULO
                                                                @IC_CEN_VCMT,           -- INDICADOR DE CENARIO NO VENCIMENTO
                                                                @FER_CHAVE,             -- POSICAO PARA FERIADOS
                                                                'S', --@EH_FUNDO,               -- INDICADOR DE POSICAO SER FUNDO
                                                                @IDX_FCALC,             -- FORMA DE CALCULO DO INDEXADOR
                                                                @IDX_PRE,               -- INDICADOR PARA OPERACOES PRE
                                                                @RFX_RESULTADO,         -- FORMA DE CALCULO DA MTM
                                                                @TIR_U,                 -- TAXA UTEIS DA OPERACAO
                                                                @TIR_C,                 -- TAXA CORRIDOS DA OPERACAO
                                                                @SGL_B_EXPS     OUTPUT, -- EXPRESSAO DO CENARIO
                                                                @TIR_M          OUTPUT, -- CENARIO PARA MARCACAO A MERCADO
                                                                @ERRO           OUTPUT, -- -1 = OK; 0 = ERRO; 1 = NAO ACHOU CENARIO
                                                                @ERR_MSG        OUTPUT  -- MENSAGEM DE ERRO
                                
                                

                                IF @V_DURT_CALD_VOLTA IS NOT NULL
                                BEGIN
                                        SELECT @POS_DIA_CENARIO = POS_DIA_CENARIO FROM POSICAO (NOLOCK) WHERE POS_APELIDO = @FER_CHAVE
                                        EXEC SANPS_RF_CALCULA_TAXA_AJ_TERMO     @V_DURT_CALD_VOLTA,
                                                                                                    @TIR_M,
                                                                                                                                                @DT_VLRZ_TERMO,
                                                                                                                                                @DT_CALC,
                                                                                                                                                @TIT_VENCTO     ,
                                                                                                                                                @IC_CEN_VCMT    ,
                                                                                                                                                @FER_CHAVE      ,
                                                                                                                                                @EH_FUNDO       ,
                                                                                                                                                @IDX_CODIGO     ,       
                                                                                                                                                @TIR_M OUTPUT,@POS_DIA_CENARIO--PROVISÓRIO PARA FUNDO ABERTURA
                                        

                                END
                                

                        SELECT @CEN_DATA_BASE = @DT_EVE

-- if @@spid = 80 SELECT 3282 'rf0884', @TIR_M TIR_M, @SGL_B_EXPS SGL_B_EXPS, @CEN_DATA CEN_DATA, @CEN_DATA_BASE CEN_DATA_BASE, @SGL_MEDA SGL_MEDA

                                IF @ERRO = 0
                                BEGIN

                                        SELECT
                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - ' + @ERR_MSG + ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                        IF @VB_SQL IS NOT NULL
                                        BEGIN
                                                SELECT
                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                        @ERRO                   ERRO,
                                                        @ERR_MSG                ERR_MSG

                                                RETURN
                                        END
                                        ELSE
                                                RETURN

                                END
                                ELSE
                                BEGIN   -- SE NAO ENCONTROU CENARIO E CONTABILIZA MERCADO, PARA!

                                        IF @ERRO = 1 AND @CTB_CURVA = 'M'
                                        BEGIN
                                        
                                                SELECT
                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - ' + @ERR_MSG + ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                                IF @VB_SQL IS NOT NULL
                                                BEGIN
                                                        SELECT
                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                              @ERRO                   ERRO,
                                                                @ERR_MSG                ERR_MSG

                                                        RETURN
                                                END
                                                ELSE
                                                        RETURN

                                        END
                                        ELSE
                                                
                                                IF @ERRO = 1 AND @DT_ACABOU_CEN IS NULL
                                                        SELECT
                                                                @DT_ACABOU_CEN  = @CEN_DATA_BASE

                                        SELECT
                                                @ERRO = -1

                                        /* Patricia 16/08/2004 - MtM CDI */

                                        IF @ERRO = -1 -- SEM ERRO
                                                
                                           IF @SGL_FCALC = '003' OR @SGL_FCALC = '006' --CDI/PERC. CDI
                                           BEGIN
                                                SELECT
                                                        @SGL_MEDA_BASE  =       A.SGL_MEDA_B_ES_CMBL,
                                                        @SGL_BASE       =       A.SGL_B_EXPS
                                                FROM
                                                        SANT269_RF_CENARIO_MOS A,
                                                        SANT447_GE_CEN_SAN_APC B
                                                WHERE
                                                        A.SGL_MEDA      =       B.SGL_MEDA      AND
                     B.IDX_CODIGO    =       @SGL_MEDA
                                                                                        /* BUSCA CENARIO PRE */
-- 

                                                IF @DT_VLRZ_TERMO IS NOT NULL   
                                                        
                                                        SELECT @CEN_DATA_BASE = @DT_VLRZ_TERMO

                                                -- RF0885.PRC
                                                EXEC SANPS_RF_BUSCA_CENARIO     @SGL_MEDA_BASE,         -- SIGLA DA MOEDA OU INDEXADOR
                                                                                @CEN_DATA_BASE  OUTPUT, -- DATA BASE
                                                                                @CEN_DATA       OUTPUT, -- DATA DE PROJECAO DO CENARIO
                                                                                @TIT_VENCTO,            -- VENCIMENTO DO TITULO
                                                                                @IC_CEN_VCMT,           -- INDICADOR DE CENARIO NO VENCIMENTO
                                                                                @FER_CHAVE,             -- POSICAO PARA FERIADOS
                                                                                'S', --@EH_FUNDO,               -- INDICADOR DE POSICAO SER FUNDO
                                                                                @IDX_FCALC,             -- FORMA DE CALCULO DO INDEXADOR
                                                                                @IDX_PRE,               -- INDICADOR PARA OPERACOES PRE
                                                                                @RFX_RESULTADO,         -- FORMA DE CALCULO DA MTM
                                                                                @TIR_U,                 -- TAXA UTEIS DA OPERACAO
                                                                                @TIR_C,                 -- TAXA CORRIDOS DA OPERACAO
                                               @SGL_BASE       OUTPUT, -- EXPRESSAO DO CENARIO
                                                                                @TX_PRE         OUTPUT, -- CENARIO ESTRUTURA CAMBIAL
                                                                                @ERRO           OUTPUT, -- -1 = OK; 0 = ERRO; 1 = NAO ACHOU CENARIO
                                                                                @ERR_MSG        OUTPUT  -- MENSAGEM DE ERRO
                                                SELECT @CEN_DATA_BASE = @DT_EVE

                                                IF @ERRO = 0
                                                BEGIN
                                                        SELECT
                                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - ' + @ERR_MSG + ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)
                
                                                        IF @VB_SQL IS NOT NULL
                                                        BEGIN
                                                                SELECT
                                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                        @ERRO                   ERRO,
                                                                        @ERR_MSG                ERR_MSG
                
                                                                RETURN
                                                        END
                                        ELSE
                                                                RETURN
                
                                                END
                                                ELSE
                                                BEGIN   -- SE NAO ENCONTROU CENARIO E CONTABILIZA MERCADO, PARA!
                                                        IF @ERRO = 1 AND @CTB_CURVA = 'M'
                                                        BEGIN
                                                                SELECT
                                                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - ' + @ERR_MSG + ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)
                
                                                                IF @VB_SQL IS NOT NULL
                                                                BEGIN
                                                                        SELECT
                                                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                                                @ERRO                   ERRO,
                                                                                @ERR_MSG                ERR_MSG
                
                                                                        RETURN
                                                                END
                                                                ELSE
                                                                        RETURN
                
                                                        END
                                                        ELSE
                                                                IF @ERRO = 1 AND @DT_ACABOU_CEN IS NULL
                                                                        SELECT
                                                                                @DT_ACABOU_CEN  = @CEN_DATA_BASE
                                                        SELECT
                                                                @ERRO = -1
                                                END
                                        END                             
                                END
                        END

                        -- SE NAO POSSUIR CENARIO OU SIGLA ASSOCIADA E CTB CURVA NAO EH MERCADO

                        IF @SGL_MEDA = '' OR @RFX_RESULTADO <> '001' OR @DT_ACABOU_CEN IS NOT NULL
                           OR (@MERC_DT_AQUIS <> 'S' AND @DT_EVE = @DT_AQUIS)
                        BEGIN
                                
                                -- MERCADO = UTEIS

                                IF @RFX_RESULTADO = '007' OR @EH_FUNDO = 'S'

                                        SELECT
                                                @TIR_M          = @TIR_U,
                                                @CEN_DATA_BASE  = @DT_EVE,
                                                @CEN_DATA       = @DT_FIM,
                                                @SGL_B_EXPS     = '022'

                                ELSE    -- MERCADO = CORRIDOS

                                        SELECT
                                                @TIR_M          = @TIR_C,
                                                @CEN_DATA_BASE  = @DT_CALC,
                                                @CEN_DATA       = @DT_FIM,
                                                @SGL_B_EXPS     = '001'

                        END



                        -- DESCAPTALIZACAO PELA TIR

                        SELECT
                                @VAR_DESCAP_U   = ROUND(@TIR_U, 4, @TRUNCA),    -- @TIR_U,
                                @VAR_DESCAP_C   = ROUND(@TIR_C, 4, @TRUNCA),    -- @TIR_C,
                                @VAR_DESCAP_M   = ROUND(@TIR_M, 4, @TRUNCA)     -- @TIR_M


-- if @@spid = 80 SELECT 3455 'rf0884', @VAR_DESCAP_U VAR_DESCAP_U, @VAR_DESCAP_C VAR_DESCAP_C, @VAR_DESCAP_M VAR_DESCAP_M

                        EXEC SIAN_CALC_JUROS    '022',
                                                @VAR_DESCAP_U   OUTPUT,
                                                @DT_EVE,
                                                @TIT_EMISSAO,
                                                @DT_FIM,
                                                0,
                                                'A',
                                                @FER_CHAVE,
                                                'U',
                                                0

-- if @@spid = 80 SELECT 3468 'rf0884', @VAR_DESCAP_U VAR_DESCAP_U, @VAR_DESCAP_C VAR_DESCAP_C, @VAR_DESCAP_M VAR_DESCAP_M, @DT_EVE DT_EVE, @TIT_EMISSAO TIT_EMISSAO, @DT_FIM DT_FIM

                        IF @RFX_FCALC = '035' 
                        BEGIN
                                
                                IF @TIT_EMISSAO < @DT20000801
                                BEGIN
                                        SELECT @FORMA_CALC = '002'
                                END

                                ELSE

                                BEGIN
                                        SELECT @FORMA_CALC = @RFX_FCALC
                                END
                        END

                        ELSE

                        BEGIN
                                
                                SELECT @FORMA_CALC = '001'
                        END

-- if @@spid = 80 SELECT 3491 'rf0884', @RFX_FCALC RFX_FCALC, @FORMA_CALC FORMA_CALC, @VAR_DESCAP_C VAR_DESCAP_C

                        IF @RFX_FCALC = '032' 
                BEGIN
                                
                                SELECT @DATA = DATEADD(DAY, 1, @DT_FIM) 
        
                                EXEC SIAN_CALC_JUROS    @RFX_FCALC, --'001',
                                                        @VAR_DESCAP_C   OUTPUT,
                                                        @DT_FIM,
                                                        @DT_CALC,
                                                        @DATA,
                                                        0,
                                                        'A',
                                                        @FER_CHAVE,
                                                        'C',
                                                        0
        
                                END
        
                        ELSE
        
                                BEGIN
                                
                                EXEC SIAN_CALC_JUROS    @FORMA_CALC, --'001',
                                                        @VAR_DESCAP_C   OUTPUT,
                                                        @DT_CALC,       -- @DT_FIM
                                                        @TIT_EMISSAO,
                                                        @DT_FIM,        -- @DT_CALC
                                                        0,
                                                        'A',
                                                        @FER_CHAVE,
                                                        'C',
                                                        0
                        END

-- if @@spid = 80 SELECT 3527 'rf0884', @RFX_FCALC RFX_FCALC, @FORMA_CALC FORMA_CALC, @VAR_DESCAP_C VAR_DESCAP_C, @DT_FIM DT_FIM, @TIT_EMISSAO TIT_EMISSAO, @DT_CALC DT_CALC, @FER_CHAVE FER_CHAVE

                        -- PARA PRE, A DATA DE CALCULO EH A DATA BASE DO CENARIO        
                        IF @IDX_PRE <> 'N' AND ISNULL(RTRIM(@ATV_IDX_CUSTO), '') <> ''
                        BEGIN
                                
                                SELECT
                                        @DT_INI_CEN = @CEN_DATA_BASE
                        END
                        ELSE
                        BEGIN
                        
                                SELECT
                                        @DT_INI_CEN = @DT_EVE
                        END
                        --Patricia 16/08/2004 - MtM CDI
                        IF @SGL_FCALC = '003' OR @SGL_FCALC = '006' --CDI / PERC. CDI
                        BEGIN
                                
                                SELECT @DT_PROX_CEN = DATEADD(DD,1,@DT_INI_CEN)

                                -- DESCAPTALIZACAO TAXA PRE
                                EXEC SIAN_CALC_JUROS    @SGL_BASE,
                                                        @TX_PRE         OUTPUT,
                                                        @DT_INI_CEN,
                                                        @DT_INI_CEN,
                                                        @DT_PROX_CEN,
                                                        0,
                                                        'A',
                                                        @FER_CHAVE,
                                                        'C',
                                                        0                               
                                SELECT 
                                        @TX_PRE_EMIS  = (@TX_PRE - 1) *  (@TX_EMISSAO/@FLOAT100 ) + 1,
                                        @TX_PRE_MTM   = (@TX_PRE - 1) *  (@TIR_M/@FLOAT100) + 1

                                 EXEC   @Q_DIAS_PARC = SIAN_SP_QUANTAS_RESERVAS @DT_UTIL,
                                                                                @DT_EVE,
                                                                                '003',
                                                                                'A',
                                                                                @FER_CHAVE,
                                                                                0       
                                IF @TX_PRE_EMIS <> 0                            
                                        SELECT 
                                                @VAR_DESCAP_M   = POWER(@TX_PRE_MTM / @TX_PRE_EMIS, @Q_DIAS_PARC)
                        END
                        ELSE
                        BEGIN
                        
                                -- DESCAPTALIZACAO PELO CENARIO 
                                IF @SGL_FCALC = '002' OR @SGL_FCALC = '005' --DESAGIO           
                                        SELECT  
                                                @DT_INI_CEN = @DT_UTIL,
                                                @CEN_DATA   = @DT_EVE                                           
                                        
                                EXEC SIAN_CALC_JUROS    @SGL_B_EXPS,
                                                        @VAR_DESCAP_M   OUTPUT,
                                                        @DT_INI_CEN,
                                                        @DT_INI_CEN,
                                                        @CEN_DATA,
                                                        0,
                                                        'A',
                                                        @FER_CHAVE,
                                                        'C',
                                                        0

                        END     

                        -- PARA PRE, DA UM DIA DE CDI
                        IF @RFX_RESULTADO = '001' AND @IDX_PRE <> 'N'
                           AND @CEN_DATA_BASE <> @DT_CALC
                           AND ISNULL(RTRIM(@ATV_IDX_CUSTO), '') <> ''
                        BEGIN


                                -- CENARIO D-1 COM CUSTO CDI
                                EXEC SIAN_SP_VARIACAO
                            @ATV_IDX_CUSTO,
                                                        @DT_CALC,
                                                        @CEN_DATA_BASE,
                                                        @DT_CALC,
                                                        @FLOAT100,
                                                        @FLOAT1,
                                                        0,
                                                        '000',
                                                        @FER_CHAVE,
                                                        @DT_CALC,
                                                        0,
                                                        1,
                                                        @CORRECAO_CDI   OUTPUT,
                                                        @ERRO           OUTPUT,
                                                        'C',
                                                        0,
                                                        @SGL_SISTEMA = 'RDF'            -- LIANA - 22/11/2010


                                IF @ERRO <> -1
                                BEGIN
                                        SELECT
                                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - NAO EXISTE COTACAO PARA INDEXADOR DE CUSTO: ' + 
                                                           RTRIM (@IDX_CODIGO) + '. DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                        IF @VB_SQL IS NOT NULL
                                        BEGIN
         SELECT
                                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                        @ERRO                   ERRO,
                                                        @ERR_MSG                ERR_MSG

                                                RETURN
                                        END
                                        ELSE
                                                RETURN

                                END
                        END
                        ELSE
                                SELECT
                                        @CORRECAO_CDI   = @FLOAT1



                        -- DIAS UTEIS PARA O CALCULO DA DURATION

                        EXEC @Q_DU_DURATION = SIAN_SP_QUANTAS_RESERVAS  @DT_CALC,
                                                                        @DT_FIM_ORI,
                                                                        '003',
                                                                        'A',
                                                                        @FER_CHAVE,
                                                                        0


                        -- CALCULA OS PUs
-- if @@spid = 80 SELECT 3651 'rf0884', @CUPOM_TIR CUPOM_TIR, @CORRECAO_U CORRECAO_U, @ATV_LOTE ATV_LOTE, @VAR_DESCAP_M VAR_DESCAP_M, @CORRECAO_CDI CORRECAO_CDI, @Q_DU_DURATION Q_DU_DURATION
                        IF @DT_FIM_ORI = @TIT_VENCTO
                        BEGIN
                                
-- if @@spid = 80 SELECT 3654 'rf0884', @CUPOM_TIR CUPOM_TIR, @VAR_DESCAP_U VAR_DESCAP_U, @VAR_DESCAP_C VAR_DESCAP_C, @VAR_DESCAP_M VAR_DESCAP_M, @CORRECAO_CDI CORRECAO_CDI
                                

                                SELECT
                                        @RFS_PU_UTEIS    = @RFS_PU_UTEIS    + ROUND( (ROUND(@CUPOM_TIR, 8, @ARREDONDA) / @VAR_DESCAP_U) * 100, 10, @ARREDONDA), 
                                        @RFS_PU_CORRIDOS = @RFS_PU_CORRIDOS + ROUND( (ROUND(@CUPOM_TIR, 8, @ARREDONDA) / @VAR_DESCAP_C) * 100, 10, @ARREDONDA), 
                                        @RFS_PU_MERCADO  = @RFS_PU_MERCADO  + ROUND( (ROUND(@CUPOM_TIR, 8, @ARREDONDA) / @VAR_DESCAP_M) * 100, 10, @ARREDONDA) -- * @CORRECAO_CDI



                                SELECT
                                        @V_PZ_MDIO_CALD  = @V_PZ_MDIO_CALD +   (@CUPOM_TIR * @CORRECAO_U * @ATV_LOTE) * DATEDIFF(Day, @DT_CALC, @DT_FIM_ORI),
                                        @V_DURT_CALD     = @V_DURT_CALD    + (((@CUPOM_TIR * @CORRECAO_U * @ATV_LOTE) / @VAR_DESCAP_M) * @CORRECAO_CDI) * @Q_DU_DURATION,
                                        @V_FUT_TOTL          = @V_FUT_TOTL     +   (@CUPOM_TIR * @CORRECAO_U * @ATV_LOTE)
-- if @@spid = 80 SELECT 3667 'rf0884', @V_DURT_CALD V_DURT_CALD, @CUPOM_TIR CUPOM_TIR, @CORRECAO_U CORRECAO_U, @ATV_LOTE ATV_LOTE, @DT_CALC DT_CALC, @DT_FIM_ORI DT_FIM_ORI, @VAR_DESCAP_M VAR_DESCAP_M, @CORRECAO_CDI CORRECAO_CDI, @Q_DU_DURATION Q_DU_DURA
TION
                        END
                        ELSE
                        BEGIN


-- if @@spid = 80 SELECT 3671 'rf0884', @CUPOM_TIR CUPOM_TIR, @VAR_DESCAP_U VAR_DESCAP_U, @VAR_DESCAP_C VAR_DESCAP_C, @VAR_DESCAP_M VAR_DESCAP_M, @CORRECAO_CDI CORRECAO_CDI
                                SELECT
--                                      @RFS_PU_UTEIS    = @RFS_PU_UTEIS + (((@CUPOM_TIR - @FLOAT1) * @CORRECAO_U * @ATV_LOTE) / @VAR_DESCAP_U),
--                                      @RFS_PU_CORRIDOS = @RFS_PU_CORRIDOS + (((@CUPOM_TIR - @FLOAT1) * @CORRECAO_C * @ATV_LOTE) / @VAR_DESCAP_C),
--                                      @RFS_PU_MERCADO  = @RFS_PU_MERCADO + ((((@CUPOM_TIR - @FLOAT1) * @CORRECAO_U * @ATV_LOTE) / @VAR_DESCAP_M) * @CORRECAO_CDI)
                                        @RFS_PU_UTEIS    = @RFS_PU_UTEIS    + ROUND( (ROUND(@CUPOM_TIR - @FLOAT1, 8, @ARREDONDA) / @VAR_DESCAP_U) * 100, 10, @ARREDONDA),
                                        @RFS_PU_CORRIDOS = @RFS_PU_CORRIDOS + ROUND( (ROUND(@CUPOM_TIR - @FLOAT1, 8, @ARREDONDA) / @VAR_DESCAP_C) * 100, 10, @ARREDONDA),
                                        @RFS_PU_MERCADO  = @RFS_PU_MERCADO  + ROUND( (ROUND(@CUPOM_TIR - @FLOAT1, 8, @ARREDONDA) / @VAR_DESCAP_M) * 100, 10, @ARREDONDA) -- * @CORRECAO_CDI

                                SELECT
                                        @V_PZ_MDIO_CALD  = @V_PZ_MDIO_CALD +   ((@CUPOM_TIR - @FLOAT1) * @CORRECAO_U * @ATV_LOTE) * DATEDIFF(Day, @DT_CALC, @DT_FIM_ORI),
                                        @V_DURT_CALD     = @V_DURT_CALD    + ((((@CUPOM_TIR - @FLOAT1) * @CORRECAO_U * @ATV_LOTE) / @VAR_DESCAP_M) * @CORRECAO_CDI) * @Q_DU_DURATION,
                                        @V_FUT_TOTL          = @V_FUT_TOTL     +   ((@CUPOM_TIR - @FLOAT1) * @CORRECAO_U * @ATV_LOTE)

                                
-- if @@spid = 80 SELECT 3684 'rf0884', @V_DURT_CALD V_DURT_CALD, @CUPOM_TIR CUPOM_TIR, @CORRECAO_U CORRECAO_U, @ATV_LOTE ATV_LOTE, @DT_CALC DT_CALC, @DT_FIM_ORI DT_FIM_ORI, @VAR_DESCAP_M VAR_DESCAP_M, @CORRECAO_CDI CORRECAO_CDI, @Q_DU_DURATION Q_DU_DURAT
ION
                        END



                        -- INCLUI OS FLUXOS PARA CALCULAR A TIR MERCADO

                        IF @SGL_MEDA <> '' AND @RFX_RESULTADO = '001'
                        BEGIN
                
                                SELECT
                                        @ID_NUM_PARC    = @ID_NUM_PARC + 1


                                IF @RFX_FCALC = '009' OR @RFX_FCALC = '022'

                                        EXEC @Q_DIAS_PARC = SIAN_SP_QUANTAS_RESERVAS    @DT_EVE,
                                                                                        @DT_FIM,
                                                                                        '003',
                                                                                        'A',
                                                                                        @FER_CHAVE,
                                                                                        0
                                ELSE
                                BEGIN
                                        IF @RFX_FCALC = '032'   -- EXPONENCIAL 30 / 360

                                                EXEC SIAN_SP_TRINTA_DIAS        @DT_FIM,
                                                                                @DT_EVE,
                                                                                @Q_DIAS_PARC    OUTPUT,
                                                                                0,
                                                                                'A'     -- PADRAO AMERICANO
                                        ELSE

                                                SELECT
                                                        @Q_DIAS_PARC = DATEDIFF(day, @DT_EVE, @DT_FIM)
                                END


                                -- INSERE OS FLUXOS

                                INSERT INTO SANT646_RF_TIR
                                        (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
                                SELECT
                                        @@SPID,
                                        @CD_CE,
                                        @ID_NUM_PARC,
                                        @Q_DIAS_PARC,
                                        CASE    WHEN @DT_FIM_ORI = @TIT_VENCTO THEN (@CUPOM_TIR * @CORRECAO_U * @ATV_LOTE)
                                        ELSE ((@CUPOM_TIR - @FLOAT1) * @CORRECAO_U * @ATV_LOTE)
                                        END,
                                        @FLOAT0,
                                        @FLOAT0,
                                        @FLOAT0,
                                        @FLOAT0,
                                        @FLOAT0

                        END


                        SELECT
                                @DT_FIM         = @DT_INI,
                                @DT_FIM_ORI     = @DT_INI_ORI

                END


        IF @IDX_CODIGO <> 'PRE'    -- NTN-F
        BEGIN
            -- NTN-B (IPCA) / NTN-C (IGPM)
            SELECT @RFS_PU_UTEIS    = ROUND(@RFS_PU_UTEIS   , 4, @TRUNCA)
            SELECT @RFS_PU_CORRIDOS = ROUND(@RFS_PU_CORRIDOS, 4, @TRUNCA)
            SELECT @RFS_PU_MERCADO  = ROUND(@RFS_PU_MERCADO , 4, @TRUNCA)
        END
                  
        SELECT @RFS_PU_UTEIS    = ROUND(@RFS_PU_UTEIS    / 100 * @CORRECAO_U * @ATV_LOTE                , 6, @TRUNCA)
        SELECT @RFS_PU_CORRIDOS = ROUND(@RFS_PU_CORRIDOS / 100 * @CORRECAO_C * @ATV_LOTE                , 6, @TRUNCA)
        SELECT @RFS_PU_MERCADO  = ROUND(@RFS_PU_MERCADO  / 100 * @CORRECAO_U * @ATV_LOTE * @CORRECAO_CDI, 6, @TRUNCA)
        


                -- CALCULO DO DURATION PARA OPERACOES SUBORDINADAS
--              SELECT TOP 1 
--                      @TAXA = RFM_TAXA / @FLOAT100
--              FROM 
--                      RF_MOVIMENTACAO A, 
--                      RENDA_FIXA B 
--              WHERE 
--                      A.RF_CARACTERISTICA = B.RF_CARACTERISTICA       AND 
--                      A.RF_CARACTERISTICA = @RF_CARACTERISTICA        AND
--                      ((RFM_DT = 'A' AND ATV_AP = 'A') OR (RFM_dT = 'C' AND ATV_AP = 'P'))
--              ORDER BY 
--                      RFM_DATA DESC

--              UPDATE #DURATION SET VALOR = ((CUPOM_TIR - 1) * PRINCIPAL_A_AMORTIZAR + V_FUT_M_PCPL) / POWER(@TAXA + @FLOAT1, PRAZO / @FLOAT252)
--              UPDATE #DURATION SET VALOR2 = VALOR * PRAZO

--*************************
--              DECLARE @DURATION INT, @TX_CENARIO2 FLOAT, @TAXA_OPERADA FLOAT, @CP_IND_PRECO FLOAT, @PREMIO FLOAT


--              SELECT @DURATION = ROUND(SUM(VALOR2) / SUM(VALOR), 0)  FROM #DURATION 

--              SELECT @CEN_DATA = @DT_CALC
--              EXEC SIAN_SP_DATA_FIN @CEN_DATA OUTPUT  , @DURATION, 'A', 'SAFRABM', 0
-- SELECT @CEN_DATA CEN_DATA, @SGL_MEDA_AJUSTADO SGL_MEDA_AJUSTADO

--              SELECT 'DURATION', * FROM #DURATION

                IF @SGL_FCALC = '006'
                        SELECT @SGL_MEDA_AJUSTADO = @SGL_MEDA

--              EXEC SANPS_RF_BUSCA_CENARIO     @SGL_MEDA_AJUSTADO,     -- SIGLA DA MOEDA OU INDEXADOR
--                                              @CEN_DATA_BASE  OUTPUT, -- DATA BASE
--                                              @CEN_DATA       OUTPUT, -- DATA DE PROJECAO DO CENARIO
--                                              @DT_PROX_RPAC,          -- VENCIMENTO DO TITULO
--                                              @IC_CEN_VCMT,           -- INDICADOR DE CENARIO NO VENCIMENTO
--                                              @FER_CHAVE,             -- POSICAO PARA FERIADOS
--                                              @EH_FUNDO,              -- INDICADOR DE POSICAO SER FUNDO
--                                              @IDX_FCALC,             -- FORMA DE CALCULO DO INDEXADOR
--                                              @IDX_PRE,               -- INDICADOR PARA OPERACOES PRE
--                                              @RFX_RESULTADO,         -- FORMA DE CALCULO DA MTM
--                                              @TIR_U,                 -- TAXA UTEIS DA OPERACAO
--                                              @TIR_C,                 -- TAXA CORRIDOS DA OPERACAO
--                      0               ,       -- EXPRESSAO DO CENARIO
--                                              @CENARIO_CLASSE OUTPUT, -- CENARIO ESTRUTURA CAMBIAL
--                                              @ERRO           OUTPUT, -- -1 = OK; 0 = ERRO; 1 = NAO ACHOU CENARIO
-- 
--                                              @ERR_MSG        OUTPUT  -- MENSAGEM DE ERRO             
-- 
--              EXEC SANPS_RF_BUSCA_CENARIO     'R$',   -- SIGLA DA MOEDA OU INDEXADOR
--                                              @CEN_DATA_BASE  OUTPUT, -- DATA BASE
--                                              @CEN_DATA       OUTPUT, -- DATA DE PROJECAO DO CENARIO
--                                              @DT_PROX_RPAC,          -- VENCIMENTO DO TITULO
--                                              @IC_CEN_VCMT,           -- INDICADOR DE CENARIO NO VENCIMENTO
--                                              @FER_CHAVE,             -- POSICAO PARA FERIADOS
--                                              @EH_FUNDO,              -- INDICADOR DE POSICAO SER FUNDO
--                                              @IDX_FCALC,             -- FORMA DE CALCULO DO INDEXADOR
--                                              @IDX_PRE,               -- INDICADOR PARA OPERACOES PRE
--                                              @RFX_RESULTADO,         -- FORMA DE CALCULO DA MTM
--                                              @TIR_U,                 -- TAXA UTEIS DA OPERACAO
--                                              @TIR_C,                 -- TAXA CORRIDOS DA OPERACAO
--                                              0               ,       -- EXPRESSAO DO CENARIO
--                                              @TX_CENARIO     OUTPUT, -- CENARIO ESTRUTURA CAMBIAL
--                                              @ERRO         OUTPUT, -- -1 = OK; 0 = ERRO; 1 = NAO ACHOU CENARIO
--                                              @ERR_MSG        OUTPUT  -- MENSAGEM DE ERRO             
--              IF @IDX_FCALC = '007'
--                      EXEC SANPS_RF_BUSCA_CENARIO     @SGL_MEDA,      -- SIGLA DA MOEDA OU INDEXADOR
--                                                      @CEN_DATA_BASE  OUTPUT, -- DATA BASE
--                                                      @CEN_DATA       OUTPUT, -- DATA DE PROJECAO DO CENARIO
--                                                      @DT_PROX_RPAC,          -- VENCIMENTO DO TITULO
--                                                      @IC_CEN_VCMT,           -- INDICADOR DE CENARIO NO VENCIMENTO
--                                                      @FER_CHAVE,             -- POSICAO PARA FERIADOS
--                                                      @EH_FUNDO,              -- INDICADOR DE POSICAO SER FUNDO
--                                                      @IDX_FCALC,             -- FORMA DE CALCULO DO INDEXADOR
--                                                      @IDX_PRE,               -- INDICADOR PARA OPERACOES PRE
--                                                      @RFX_RESULTADO,         -- FORMA DE CALCULO DA MTM
--                                                      @TIR_U,                 -- TAXA UTEIS DA OPERACAO
--                                                      @TIR_C,                 -- TAXA CORRIDOS DA OPERACAO
--                                                      0               ,       -- EXPRESSAO DO CENARIO
--                                                      @TX_CENARIO2    OUTPUT, -- CENARIO ESTRUTURA CAMBIAL
--                                                      @ERRO           OUTPUT, -- -1 = OK; 0 = ERRO; 1 = NAO ACHOU CENARIO
--                                                      @ERR_MSG        OUTPUT  -- MENSAGEM DE ERRO     



                
                SELECT TOP 1 
                        @PREMIO         =       RFM_PREMIO,
                        @DATA_COMPRA    =       RFM_DATA,
                   @PU_COMPRA      =       RFM_PU
                        
                FROM 
                        RF_MOVIMENTACAO A, 
                        RENDA_FIXA B 
                WHERE 
                        A.RF_CARACTERISTICA     =       B.RF_CARACTERISTICA     AND 
                        A.RF_CARACTERISTICA     =       @RF_CARACTERISTICA      AND
                        A.RFM_OK                =       'S'                     AND
                        ((RFM_DT = 'A' AND ATV_AP = 'A') OR (RFM_dT = 'C' AND ATV_AP = 'P'))    AND
                        A.RFM_DATA              <=      @DT_CALC
                ORDER BY 
                        RFM_DATA DESC

                --SE HOUVE COMPRA NA DATA, RECALCULA O PREMIO
                IF @DT_CALC = @DATA_COMPRA
                BEGIN

--              SELECT * INTO #DURATION FROM #DURATION

--INSERT INTO ##A SELECT GETDATE(), 'A' + @IDX_FCALC + CONVERT(CHAR(100), @PU_COMPRA)
                        --EXEC SANPP_RF_CALC_PREMIO_MTM @IDX_FCALC, @PU_COMPRA, @PREMIO OUTPUT
                        UPDATE RF_MOVIMENTACAO SET RFM_PREMIO = @PREMIO WHERE RF_CARACTERISTICA = @RF_CARACTERISTICA AND RFM_DATA = @DATA_COMPRA

                END

                IF @IDX_FCALC = '007'

                BEGIN

--                      SELECT @PREMIO PREMIO

                        UPDATE #DURATION SET VALOR_SUBORDINADO = VALOR_FUTURO / (CP_IND_PRECO_AJUST * POWER(@FLOAT1 + @PREMIO, PRAZO/@FLOAT252))
--                      UPDATE #DURATION SET VALOR_SUBORDINADO = VALOR_PRESENTE /POWER(@FLOAT1 + @PREMIO, PRAZO/@FLOAT252)

                END

                IF @IDX_FCALC = '' 

                BEGIN

                
--                      SELECT @PREMIO PREMIO


                        -- O CAMPO CEN_PRE_AJUSTADO JA ESTA DIVIDIDO POR 100
                        UPDATE #DURATION SET VALOR_SUBORDINADO = VALOR_FUTURO / POWER((CEN_PRE_AJUSTADO + @FLOAT1) * (@PREMIO * @FLOAT1), PRAZO / @FLOAT252)

                END 


                IF @IDX_FCALC = '000' 

                BEGIN

                        UPDATE #DURATION SET VALOR_SUBORDINADO = VALOR_FUTURO / POWER((POWER(CEN_PRE / @FLOAT100 + @FLOAT1, @FLOAT1 / @FLOAT252) - @FLOAT1) * (CENARIO_CLASSE / @FLOAT100 + @PREMIO) + @FLOAT1, PRAZO)

                END
-- 
--              SELECT * FROM #DURATION
-- 
--              SELECT SUM(VALOR_SUBORDINADO) VALOR_SUBORDINADO FROM #DURATION

                -- CALCULA O PRAZO MEDIO E A DURATION

                IF @V_FUT_TOTL <> @FLOAT0

                SELECT
                        @V_PZ_MDIO_CALD = @V_PZ_MDIO_CALD / @V_FUT_TOTL


                IF @RFS_PU_MERCADO <> @FLOAT0

                SELECT
                        @V_DURT_CALD = @V_DURT_CALD / @RFS_PU_MERCADO

-- if @@spid = 80 select 3777 'rf0884', @V_DURT_CALD V_DURT_CALD, @RFS_PU_MERCADO RFS_PU_MERCADO


                -- GRAVA O FLUXO INICIAL E CALCULA A TIR MERCADO

                IF @SGL_MEDA <> '' AND @RFX_RESULTADO = '001'
                BEGIN
                
                        SELECT
                                @ID_NUM_PARC    = @FLOAT0,
                                @Q_DIAS_PARC    = @FLOAT0


                        IF @RFX_FCALC = '009' OR @RFX_FCALC = '022'

                                SELECT
                                        @BASE_TIR       = @FLOAT252
                        ELSE

                                SELECT
                                        @BASE_TIR       = @FLOAT360



                        -- INSERE O FLUXO INICIAL

                        INSERT INTO SANT646_RF_TIR
                                (ID_USR, CD_CE, ID_NUM_PARC, Q_DIAS_PARC, V_FUT_PARC, V_PRST_TXA_1, V_PRST_TXA_2, V_TXA_1, V_TXA_2, V_TXA_PRCL)
                        SELECT
                                @@SPID,
                                @CD_CE,
                                @ID_NUM_PARC,
                                @Q_DIAS_PARC,
                                - @RFS_PU_MERCADO,
                                @FLOAT0,
                                @FLOAT0,
                                @FLOAT0,
                                @FLOAT0,
                                @FLOAT0         


                        -- APURA A TIR MERCADO

                        -- RF0797.PRC

                        EXEC SANPP_RF_TIR       @@SPID,
                                                @CD_CE,
                                                @BASE_TIR,
                                                @ERRO           OUTPUT,
                                                @TIR_M          OUTPUT


                        IF @ERRO <> -1
                        BEGIN
                                SELECT
                                        @ERR_MSG = @RF_CARACTERISTICA + ': PU - PROBLEMAS NO CALCULO DA TIR MERCADO.' + 
                                                   ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                                IF @VB_SQL IS NOT NULL
                                BEGIN
                                        SELECT
                                                @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                                @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                                @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                                @ERRO                   ERRO,
                                                @ERR_MSG                ERR_MSG

                                        RETURN
                                END
                                ELSE
                                        RETURN

                        END

  END

            END
        END
        ELSE    -- FORMA DE CALCULO DE RESULTADO 006 - NORMAL PU
        BEGIN

                -- RF0885.PRC

--                              select @V_DURT_CALD '@V_DURT_CALD'

                EXEC SANPS_RF_BUSCA_CENARIO     @IDX_GERENCIAL,         -- INDEXADOR GERENCIAL
                                                @DT_CALC,               -- DATA BASE
                                                @TIT_VENCTO,            -- DATA DE PROJECAO DO CENARIO
                                                @TIT_VENCTO,            -- VENCIMENTO DO TITULO
                                                @IC_CEN_VCMT,           -- INDICADOR DE CENARIO NO VENCIMENTO
                                                @FER_CHAVE,             -- POSICAO PARA FERIADOS
                                                'S', --@EH_FUNDO,               -- INDICADOR DE POSICAO SER FUNDO
                                                @IDX_FCALC,             -- FORMA DE CALCULO DO INDEXADOR
                                                @IDX_PRE,               -- INDICADOR PARA OPERACOES PRE
                                                @RFX_RESULTADO,         -- FORMA DE CALCULO DA MTM
                                                @TIR_U,                 -- TAXA UTEIS DA OPERACAO
                                                @TIR_C,                 -- TAXA CORRIDOS DA OPERACAO
                                                @SGL_B_EXPS,            -- EXPRESSAO DO CENARIO
                                                @RFS_PU_MERCADO OUTPUT, -- RETORNA O PU
                                                @ERRO           OUTPUT, -- -1 = OK; 0 = ERRO; 1 = NAO ACHOU CENARIO
                                                @ERR_MSG        OUTPUT  -- MENSAGEM DE ERRO

                IF @ERRO = 0
                BEGIN
                        SELECT
                                @ERR_MSG = @RF_CARACTERISTICA + ': PU - ' + @ERR_MSG + ' DATA: ' + CONVERT(CHAR(10), @DT_CALC, 103)

                        IF @VB_SQL IS NOT NULL
                        BEGIN
                    SELECT
                                        @RFS_PU_UTEIS           RFS_PU_UTEIS,
                                        @RFS_PU_CORRIDOS        RFS_PU_CORRIDOS,
                                        @RFS_PU_MERCADO         RFS_PU_MERCADO,
                                        @ERRO                   ERRO,
                                        @ERR_MSG                ERR_MSG

                                RETURN
                        END
                        ELSE
                                RETURN
                END

                SELECT
                        @RFS_PU_UTEIS    = @RFS_PU_MERCADO,
                        @RFS_PU_CORRIDOS = @RFS_PU_MERCADO
        END

        -- LIMPA A TABELA DA TIR

        DELETE FROM SANT646_RF_TIR WHERE ID_USR = @@SPID

-- if @@spid = 80 select 3906 'rf0884', @RFS_PU_UTEIS RFS_PU_UTEIS, @RFS_PU_CORRIDOS RFS_PU_CORRIDOS, @RFS_PU_MERCADO RFS_PU_MERCADO, @ATV_LOTE ATV_LOTE, @IDX_CODIGO IDX_CODIGO, @V_PZ_MDIO_CALD V_PZ_MDIO_CALD, @V_DURT_CALD V_DURT_CALD, @V_FUT_TOTL V_FUT_
TOTL

        -- CASO O TITULO UTILIZE ARREDONDAMENTO CETIP
        IF @FL_ARRED = 'S'
                SELECT  @RFS_PU_CORRIDOS = ROUND(@RFS_PU_CORRIDOS,@PU_CASAS,@PU_ARRED),
                        @RFS_PU_UTEIS = ROUND(@RFS_PU_UTEIS,@PU_CASAS,@PU_ARRED),
                        @RFS_PU_MERCADO = ROUND(@RFS_PU_MERCADO,@PU_CASAS,@PU_ARRED)


        IF @VB_SQL IS NOT NULL
        BEGIN
                SELECT
                        @RFS_PU_UTEIS                   RFS_PU_UTEIS,
                        @RFS_PU_CORRIDOS                RFS_PU_CORRIDOS,
                        @RFS_PU_MERCADO                 RFS_PU_MERCADO,
                        @ERRO                           ERRO,
                        @ERR_MSG   ERR_MSG

                RETURN
        END


END

