---------------------------------------------------------------------------
-- ALTERACAO:	CRISTIANO MONEGATTO
-- DATA:		13/05/2015
-- DESCRICAO:	BUSCANDO PU DE COMPRA/VENDA DO DIA PARA VALORIZAR OS ESTOQUES DE ATIVOS 
--				PUBLICOS E QUE SEJA FUNDO CONSOLIDADO.
-- PROBLEMA:	QUANDO O ESTOQUE ESTAVA ZERADO, O SISTEMA UTILIZAVA O PU DE AQUISICAO
--				DA PRIMEIRA COMPRA E COM ISSO O PU_MERCADO FICAVA ERRADO PARA O DIA.
--				NO OUTRO DIA, O PU_MERCADO SE AJUSTAVA.
---------------------------------------------------------------------------

ALTER PROCEDURE [dbo].[SIAN_RF_VAL_FIS_003_THIAGO]
    (
    @CARAC          CHAR(20),
    @DTINI          SMALLDATETIME,
    @DTFIM          SMALLDATETIME,
    @LOCAL_NEGOC    CHAR(15)     =   NULL,
    @ERRO_CENARIO   CHAR(01)     =   'S',
    @RETORNA_MSG    CHAR(01) 	 =   'N',	-- INDICA SE RETORNA MENSAGEM DE ERRO NA VARIÁVEL DE OUTPUT
    @MENSAGEM_ERRO  VARCHAR(500) =   OUTPUT	-- MENSAGEM DE ERRO
    )
AS

DECLARE
    @ATV_AP         	CHAR(1),
    @ATV_COMPOSTO       CHAR(1),
    @ATV_RENDA      	CHAR(1),
    @IDX_PRE        	CHAR(1),
    @IDX_TIPO       	CHAR(1),
    @RF_TRIB_IR     	CHAR(1),
    @AC         		CHAR(1),
    @CTB_CURVA      	CHAR(1),
    @FIN_TRUNCA     	CHAR(1),
    @MERCADO_1DIA       CHAR(1),
    @ACHOU_COTACAO      CHAR(1),
    @ACHOU_CENARIO      CHAR(1),
    @ISENTO_IR      	CHAR(1),
    @FCALC_AUX      	CHAR(3),
    @RF_MOEDA       	CHAR(3),
    @RFX_FCALC      	CHAR(3),
    @RFX_RESULTADO      CHAR(3),
    @FC_IR          	CHAR(3),
    @ATV_FCVALGER       CHAR(3),
    @IDX_FCALC      	CHAR(3),
    @ATV_CODIGO     	CHAR(15),
    @IDX_CODIGO     	CHAR(15),
    @IDX_GERENCIAL      CHAR(15),
    @IDX_MERCADO        CHAR(15),
    @RF_AGENTE      	CHAR(15),
    @CUSTODIANTE        CHAR(15),
    @EMPRESA        	CHAR(15),
    @EMP_ADM        	CHAR(15),
    @CORRENTISTA        CHAR(15),
    @FER_CHAVE      	CHAR(15),
    @DATAHORA       	CHAR(23),
    @RF_DTLIQ       	DATETIME,
    @RF_EMISSAO     	DATETIME,
    @RF_EMISSAO_P1      DATETIME,
    @RF_VENCTO      	DATETIME,
    @RF_VENCTO_IR       DATETIME,
    @RFX_PARTIDA        DATETIME,
    @RFX_DTERMO     	DATETIME,
    @DTAUX          	DATETIME,
    @DTRESANT       	DATETIME,
    @RFX_DTANIV     	DATETIME,
    @DT_AUXILIAR        DATETIME,
    @DT_AUXILIAR2       DATETIME,
    @DT_FIM_VAR     	DATETIME,
    @DTCAUTELA      	DATETIME,
    @RFX_TAXA       	FLOAT,
    @RFX_PU_INICIAL     FLOAT,
    @PUINI          	FLOAT,
    @TAXA_ADIC      	FLOAT,
    @RF_CUPOM       	FLOAT,
    @ATV_LOTE       	FLOAT,
    @ATV_FCVALORIZ      CHAR(03),
    @RFX_PERC       	FLOAT,
    @RFX_PRINCIPAL      FLOAT,
    @RFX_COTACAO_PARTIDA    FLOAT,
    @RFX_COTACAO_VOLTA  FLOAT,
    @CORRECAO     		FLOAT,
    @CORRECAO_ACRU      FLOAT,
    @CORRECAO_ACRU_S    FLOAT,
    @CORRECAO_ACRU_T    FLOAT,
    @CORRECAO_ANT       FLOAT,
    @CORRECAO_MOV       FLOAT,
    @CORRECAO_BX        FLOAT,
    @CORRECAO_BASE      FLOAT,
    @CURVA         		FLOAT,
    @FIN           		FLOAT,
    @FIN_COMPRA    		FLOAT,
    @FIN_VENDA     		FLOAT,
    @JUROS         		FLOAT,
    @JUROS_ANT     		FLOAT,
    @JUROS_MOV     		FLOAT,
    @JUROS_BX      		FLOAT,
    @JUROS_BASE    		FLOAT,
    @PRINCIPAL     		FLOAT,
    @PRINCIPAL_MOV      FLOAT,
    @PRINCIPAL_BX       FLOAT,
    @PRINCIPAL_BASE     FLOAT,
    @P1         		FLOAT,
    @P2         		FLOAT,
    @PU_PARTIDA     	FLOAT,
    @PU_CURVA       	FLOAT,
    @PU_CURVA_C     	FLOAT,
    @PU_CURVA_U     	FLOAT,
    @PU_CURVA_N     	FLOAT, -- LIGIA MUTO - 21/11/2008
    @PU_MERCADO     	FLOAT,
    @PU_MERC_ANT        FLOAT,
    @PU_VOLTA       	FLOAT,
    @PU_PAR         	FLOAT,
    @VAR_CUPOM_EMISSAO  FLOAT, -- 08.09.2004
    @PU_CURVA_EMISSAO   FLOAT, -- 08.09.2004
    @RESULTADO      	FLOAT,
    @RENDIMENTO     	FLOAT,
    @TAXA_MEDIA     	FLOAT,
    @TAXA_MEDIA_U       FLOAT,
    @VARIACAO       	FLOAT,
    @VAR_CUPOM      	FLOAT,
    @VAR_CUPOM_ANBID    FLOAT,
    @VAR_JUROS_C        FLOAT,
    @VAR_JUROS_U        FLOAT,
    @VAR_CORRECAO       FLOAT,
    @VAR_CORRECAO_P1    FLOAT,
    @VAR_CORRECAO_C     FLOAT,
    @VAR_CORRECAO_U     FLOAT,
    @VALOR_IR       	FLOAT,
    @VALOR_IR_ANT       FLOAT,
    @FIN_AUX        	FLOAT,
    @PU_AUX         	FLOAT,
    @VAR_AUX        	FLOAT,
    @PU_MERC_P      	FLOAT,
    @PU_CORR_P      	FLOAT,
    @PU_UTEIS_P     	FLOAT,
    @PRINCIPAL_ABERTURA FLOAT,
    @IR_ABERTURA        FLOAT,
    @FIN_DAYTRADE       FLOAT,
    @RFX_IR_INICIAL     FLOAT,
    @FATOR_IR_INICIAL   FLOAT,
    @CORRECAO_C_ANT     FLOAT,
    @CORRECAO_U_ANT     FLOAT,
    @TAXA_MEDIA_BBC     FLOAT,
    @TX_AGIO_DESAGIO    FLOAT,
    @FIN_1COMPRA        FLOAT,
    @QTD_1COMPRA        FLOAT,
    @FIN_MERCADO        FLOAT,
    @E_RESERVA      	INT,
    @CORTE          	INT,
    @VAR_ERRO       	INT,
    @TITULO_ID      	INT,
    @CDI_ANO        	INT,
    @RFX_QTDE       	MONEY,
    @QTD            	MONEY,
    @QTD_C          	MONEY,
    @QTD_SALDO      	MONEY,
    @QTD_ABE        	MONEY,
    @QTD_COMPRA     	MONEY,
    @QTD_VENDA      	MONEY,
    @QTDE_DAYTRADE      MONEY,
    @CALC_TR        	CHAR(01),
    @CURVA_CTB      	CHAR(01),
    @VAL_IOF        	FLOAT,
    @IOF_C          	FLOAT,
    @IOF_V          	FLOAT,
    @IOF_MOV        	FLOAT,
    @MSG            	VARCHAR(255),
    @MSGAUX         	VARCHAR(255),
    @MSGDT          	VARCHAR(255),
    @MSGDT1         	VARCHAR(255),
    @IDX_CODIGO1        CHAR(15),
    @IDX_CODIGO2        CHAR(15),
    @RFX_FCALC1     	CHAR(3),
    @RFX_FCALC2     	CHAR(3),
    @RFX_VOLTA      	DATETIME,
    @DT_VIRADA      	DATETIME,
    @VENCTO_ORI     	DATETIME,
    @DT_ANT         	DATETIME,
    @IS_IDXMISTO        INT,
    @RF_TRIB_IOF        CHAR(1),
    @FC_IOF         	CHAR(3),
    @IOF_R          	FLOAT,
    @COT_VALOR      	FLOAT,
    @CEN_VALOR      	FLOAT,
    @RF_BASE_CALC       DATETIME,
    @RF_COND_DTBASE     CHAR(01),
    @RF_DATA_VAR        DATETIME,
    @TIPO_ESTOQUE       CHAR(02),
    @CETIP_SELIC        CHAR(01),
    @RFM_DT_1MVTO       CHAR(01),
    @SGL_MEDA       	CHAR(15),
    @DT_CRT         	DATETIME,
    @DTERMO         	DATETIME,
    @DT_ACABOU_CENARIO  DATETIME,
    @ACHOU_CEN_DIA_ANT  CHAR(01),
    @LF_DIF         	FLOAT,
    @LF_PROV        	FLOAT,
    @LF_PROVANT     	FLOAT,
    @TIPO_CALC      	CHAR(1),
    @CEN_FCALC      	CHAR(03),
    @CD_LG          	INT,
    @TXT_OBS        	CHAR(255),
    @HOSTNAME       	CHAR(10),
    @DT_ULTRES      	DATETIME,
    @CD_CLFC_3068       INT,
    @CD_CATG        	INT,
    @CD_CATG_ORIG       INT,
    @ESTOQUE_ORI        CHAR(02),
    @RF_CARAC_ORI       CHAR(20),
    @CTB_DT_JUROS_CORRECAO  DATETIME,
    @PU_CTBL        	FLOAT,
    @VAR_CTBL       	FLOAT,
    @FIN_CTBL       	FLOAT,
    @REND_CTBL      	FLOAT,
    @V_JURS_CTBL        FLOAT,
    @V_CORC_CTBL        FLOAT,
    @V_JURS_CTBL_ANTR   FLOAT,
    @V_CORC_CTBL_ANTR   FLOAT,
    @FUNDO          	CHAR(01),
    @PU_U_ANT       	FLOAT,
    @PRINCIPAL_CTBL     FLOAT,
    @QTD_V_CTBL     	FLOAT,
    @PRINC_CTBL     	FLOAT,
    @LF_PROVTRF     	FLOAT,
    @DATA_AQUISICAO     DATETIME,
    @CALC_OVER_LNG      CHAR(01),
    @COT_PARTIDA        FLOAT,
    @COT_VOLTA      	FLOAT,
    @PU_FINAL       	FLOAT,
    @DIAS_C             FLOAT,
    @DIAS_TOT_C         FLOAT,
    @DIAS_U             FLOAT,
    @DIAS_TOT_U         FLOAT,
    @PU_UTEIS_ANT       FLOAT,
    @V_FATR_ACUD_UTES   FLOAT,
    @V_FATR_ACUD_MERC   FLOAT,
    @V_PU_CTBL      	FLOAT,
    @IC_FIDC        	CHAR(01),
    @ATV_PUBLICO        CHAR(01),
    -- 07.07.2004
    @ID_PDD_FX_SEQ      SMALLINT,
    @PC_PDD_FX      	FLOAT,
    @V_PDD          	FLOAT,
    @DT_SEM_PDD     	DATETIME,
    -- 27.01.2005
    @V_PZ_MDIO_CALD     FLOAT,
    @V_PZ_MDIO_AJTD     FLOAT,
    @V_DURT_CALD        FLOAT,
    @VFL_PERC_NEGOCIADO FLOAT, -- PERCENTUAL E PU DE ACORDO COM FLUXOS DE NEGOCIACAO
    @VFL_PU_NEGOCIADO   FLOAT,
    @NRO_OPERACAO       CHAR(09),
    @IC_OPRC_NAO_PADR   CHAR(01),
    @V_RFM_DATA         DATETIME,
    @V_RFM_DATA_REG     DATETIME,
    @V_RFM_HORA         DATETIME,

    -- DANILO 30/07/2007: UTILIZADO NO CALCULO DOS TRIBUTOS
    @LOC_NEGC		CHAR(15),
    @PU_APLICACAO	FLOAT,
    @PU_CURVA_ATUAL	FLOAT,
    @IDX_CODIGO_RFX	CHAR(15),
    
     @POS_DIA_CENARIO INT,
     @IC_CEN_VCMT CHAR(1),
     @RFM_PU_NEGOC_TERMO FLOAT,
     @RFX_DT_NEGOC_TERMO DATETIME,
     @DTAUX_OLD DATETIME,
     @DTAUXTERMO DATETIME	-- paçoca - 14/01/2010

-- if @@spid = 65 select 271 'gbvlfi03'

SELECT
    @LF_DIF         	= 0.0,
    @LF_PROV        	= 0.0,
    @LF_PROVANT     	= 0.0,
    @FUNDO          	= 'N',
    @PU_U_ANT       	= 0.0,
    @PRINCIPAL_CTBL     = 0.0,
    @QTD_V_CTBL     	= 0.0,
    @PRINC_CTBL     	= 0.0,
    @CALC_OVER_LNG      = 'N',
    @COT_PARTIDA        = 0.0,
    @COT_VOLTA      	= 0.0,
    @PU_FINAL       	= 0.0,
    -- 27.01.2005
    @V_PZ_MDIO_CALD     = 0,
    @V_PZ_MDIO_AJTD     = 0,
    @V_DURT_CALD        = 0,
    @IC_OPRC_NAO_PADR   = 'N'


SELECT
    @CD_LG = 0,
    @HOSTNAME = SUBSTRING(HOSTNAME,1,10),
    @TXT_OBS  = 'SIAN_RF_VAL_FIS_003 ' + @CARAC + ', ' + CONVERT(CHAR, @DTINI) + ', ' + CONVERT(CHAR, @DTFIM) + ', ' + @LOCAL_NEGOC
FROM
    MASTER..SYSPROCESSES
WHERE
    SPID = @@SPID

EXEC @CD_LG = SANPS_GE_LOG_PROCESSO @CD_LG, 'SQL', @HOSTNAME, 'X32EVA', @TXT_OBS, 1

SELECT  @ACHOU_CENARIO = 'S'

SELECT  @ATV_AP     =   A.ATV_AP,
    @ATV_CODIGO 	=   A.ATV_CODIGO,
    @IDX_CODIGO 	=   A.IDX_CODIGO,
    @RF_AGENTE  	=   A.RF_AGENTE,
    @ATV_RENDA  	=   B.ATV_RENDA,
    @ATV_LOTE   	=   E.ATV_LOTE,
    @ATV_COMPOSTO   =   C.ATV_COMPOSTO,
    @ATV_FCVALORIZ  =   E.ATV_FCVALORIZ,
    @FC_IR      	=   A.RF_FCALC_IR,
    @FC_IOF     	=   A.RF_FCALC_IOFR,
    @RF_MOEDA   	=   A.RF_MOEDA,
    @RF_EMISSAO 	=   A.RF_EMISSAO,
    @RF_EMISSAO_P1  =   A.RF_EMISSAO,
    @RF_VENCTO  	=   A.RF_VENCTO,
    @RF_VENCTO_IR   =   A.RF_VENCTO,
    @RF_CUPOM   	=   A.RF_CUPOM,
    @RF_TRIB_IR 	=   A.RF_TRIB_IR,
    @RF_TRIB_IOF    =   A.RF_TRIB_IOF,
    @EMPRESA    	=   A.PFPJ_APELIDO_EMPRESA,
    @RF_DTLIQ   	=   A.RF_DTLIQ,
    @EMP_ADM    	=   ISNULL(D.EMP_ADMINISTR,''),
    @CUSTODIANTE	=   A.PFPJ_CUSTODIANTE,
    @TITULO_ID  	=   A.TITULO_ID,
    @CORRENTISTA    =   A.PFPJ_CORRENTISTA,
    @ATV_FCVALGER   =   C.ATV_FCVALGER,
    @DTCAUTELA  	=   A.RF_DTCAUTELA,
    @ISENTO_IR  	=   C.ATV_ISENTO_IR,
    @DT_VIRADA  	=   ISNULL(RF_DTVIRADA,'01/01/1900'),
    @RF_BASE_CALC   =   A.RF_BASE_CALC,
    @TIPO_ESTOQUE   =   A.TIPO_ESTOQUE,
    @CETIP_SELIC    =   CASE WHEN A.PFPJ_LOCAL_NEGOC = 'SELIC' THEN 'S'
                     	ELSE 'C'
                	END,
    @CTB_CURVA  	=   ISNULL(RTRIM(A.CTB_CURVA), 'C'),
    @CD_CLFC_3068   =   ISNULL(A.CD_CLFC_3068, 0),
    @CD_CATG    	=   ISNULL(A.CD_CATG, 0),
    @ESTOQUE_ORI    =   A.TIPO_ESTOQUE,
    @IC_FIDC    	=   ISNULL(C.IC_FIDC, 'N'),
    @ATV_PUBLICO    =   C.ATV_PUBLICO
FROM
    RENDA_FIXA A  (nolock)  LEFT OUTER JOIN POSICAO D (nolock)
            	ON (A.PFPJ_APELIDO_EMPRESA = D.POS_APELIDO),
    ATIVO       	B (nolock),
    RF_MERCADORIA   C (nolock),
    RF_TITULO   	E (nolock)
WHERE
    A.RF_CARACTERISTICA =   @CARAC      	AND
    A.ATV_CODIGO        =   B.ATV_CODIGO    AND
    A.ATV_CODIGO        =   C.ATV_CODIGO    AND
    E.TITULO_ID     	=   A.TITULO_ID


if @estoque_ori = '08'
	select @DTCAUTELA = '1900-01-01'

SELECT  @FUNDO = 'S'
FROM
    FDO_POSICAO_FUNDO (nolock)
WHERE
    POS_APELIDO = @EMPRESA

SELECT  @MSG = 'SIAN' + RTRIM(@EMPRESA) + ';' + RTRIM(@ATV_CODIGO) + ';' + RTRIM(@IDX_CODIGO) + ';',
    @MSGDT = CONVERT(CHAR(10), @RF_VENCTO, 103) + '#'

SELECT
    @IC_OPRC_NAO_PADR = ISNULL(IC_OPRC_NAO_PADR,'N')
FROM
    RF_MERCADORIA (nolock)
WHERE
    ATV_CODIGO    = @ATV_CODIGO


IF @ATV_LOTE = 0 OR @ATV_LOTE IS NULL
BEGIN
    SELECT @MSGAUX = @MSG + @MSGDT + ' * LOTE INVALIDO PARA O TITULO * '
    IF @RETORNA_MSG <> 'S'
	RAISERROR 70000 @MSGAUX
    ELSE
	SELECT @MENSAGEM_ERRO = @MSGAUX
    RETURN -100
END

SELECT  @VENCTO_ORI = @RF_VENCTO,
    @FER_CHAVE = @EMPRESA

IF @CD_CLFC_3068 > 0
    SELECT  @RF_CARAC_ORI = RF_CARACTERISTICA,
        @ESTOQUE_ORI = TIPO_ESTOQUE
    FROM
        RENDA_FIXA (nolock)
    WHERE
        CD_CLFC_3068    =   @CD_CLFC_3068   AND
        TIPO_ESTOQUE    <>  '09'

SELECT  @FIN_TRUNCA = FIN_TRUNCA,
	@LOC_NEGC = CODIGO_TITULO.LOCAL_NEGOCIACAO
FROM
    CODIGO_TITULO (nolock),
    RF_TITULO (nolock)
WHERE
    CODIGO_TITULO.ATV_CODIGO    	=   RF_TITULO.ATV_CODIGO        AND
    CODIGO_TITULO.IDX_CODIGO    	=   RF_TITULO.IDX_CODIGO        AND
    CODIGO_TITULO.LOCAL_NEGOCIACAO  =   RF_TITULO.LOCAL_NEGOCIACAO  AND
    RF_TITULO.TITULO_ID     		=   @TITULO_ID

IF EXISTS ( SELECT CALC_TR_CORR FROM PARAMETRO_GLOBAL WHERE CALC_TR_CORR = 'S' )
    SELECT  @CALC_TR = 'S'
ELSE
    SELECT  @CALC_TR = 'N'


IF @CTB_CURVA = 'C' AND @CALC_TR = 'S'
    SELECT  @CURVA_CTB = 'X'
ELSE
    SELECT  @CURVA_CTB = 'C'

--if @@spid = 109 select '@CURVA_CTB', @CURVA_CTB

EXEC SIAN_EMPRESA_DO_GRUPO @FER_CHAVE OUTPUT, @CUSTODIANTE, @CORRENTISTA, 0

SELECT
    @MERCADO_1DIA = FDO_MERCADO_1DIA
FROM
    FDO_POSICAO_FUNDO (nolock)
WHERE
    POS_APELIDO = @EMPRESA

--if @@spid = 109 select '@MERCADO_1DIA', @MERCADO_1DIA

IF @MERCADO_1DIA IS NULL
    SELECT
        @MERCADO_1DIA = 'S'

SELECT  @CTB_DT_JUROS_CORRECAO = CTB_DT_JUROS_CORRECAO
FROM    PARAMETRO_GLOBAL (nolock)

SELECT  @IDX_CODIGO     =   IDX_CODIGO,
    @RFX_PARTIDA        =   RFX_PARTIDA,
    @RFX_VOLTA      	=   RFX_VOLTA,
    @RFX_PERC       	=   RFX_PERC,
    @TAXA_MEDIA     	=   RFX_TAXA,
    @TAXA_MEDIA_U       =   RFX_TAXA,
    @RFX_PRINCIPAL      =   RFX_PRINCIPAL,
    @RFX_COTACAO_PARTIDA    =   RFX_COTACAO_PARTIDA,
    @RFX_COTACAO_VOLTA  =   RFX_COTACAO_VOLTA,
    @RFX_FCALC      	=   RFX_FCALC,
    @RFX_RESULTADO      =   RFX_RESULTADO,
    @IDX_GERENCIAL      =   IDX_GERENCIAL,
    @RFX_QTDE       	=   RFX_QTDE,
    @RFX_DTANIV     	=   RFX_DTANIV,
    @RFX_DTERMO     	=   RFX_DTERMO,
    @RFX_PU_INICIAL     =   RFX_PU_INICIAL,
    @RFX_TAXA       	=   RFX_TAXA,
    @RFX_IR_INICIAL     =   RFX_IR_INICIAL,
    @TX_AGIO_DESAGIO    =   RFX_AGIO_DESAGIO,
    @SGL_MEDA       	=   SGL_MEDA,
    @IC_CEN_VCMT  = IC_CEN_VCMT --PAÇOCA
FROM
    RF_INDEXACAO (nolock)
WHERE
    RF_CARACTERISTICA   =   @CARAC      AND
    RFX_PARCELA     	=   0


--PAÇOCA 02/02/2010
--if @estoque_ori = '08'
--BEGIN
--IF @DTFIM > @RFX_DTERMO
--BEGIN
--	SET @DTFIM = @RFX_DTERMO
--END
--END

SET @RFX_DT_NEGOC_TERMO = @RFX_PARTIDA

IF @@ROWCOUNT = 0
BEGIN
    SELECT @MSGAUX = @MSG + @MSGDT + ' * PROBLEMAS COM A INDEXACAO * '
    IF @RETORNA_MSG <> 'S'
	RAISERROR 70000 @MSGAUX
    ELSE
	SELECT @MENSAGEM_ERRO = @MSGAUX
    RETURN -100
END

-- DANILO 24/07/2007: NAS COMPROMISSADAS CDI DE PAPEIS PUBLICOS QUE NAO CONTENHAM PERCENTUAL DO INDEXADOR, DEVE BUSCAR NA RF_BOLETA
IF @ESTOQUE_ORI IN ('01', '02', '03') AND @ATV_PUBLICO = 'S' AND @IDX_CODIGO = 'CDI' AND @RFX_PERC = 0.0
BEGIN
	SELECT	@RFX_PERC = MAX(RFB_PERC)
	FROM	RF_MOVIMENTACAO A (nolock)
	JOIN	RF_BOLETA B (nolock)
	ON	A.NRO_OPERACAO = B.NRO_OPERACAO
	WHERE	B.TIPO_ESTOQUE = '00'
	AND	B.RFB_DT = 'A'
	AND	A.RF_CARACTERISTICA = @CARAC
END

--if @@spid = 109 select '@RFX_PERC', @RFX_PERC

SELECT  @DT_CRT = '20020401'

SELECT  @DT_CRT     =   A.DT_CRT,
    	@CEN_FCALC  =   A.IDX_FCALC
FROM    SANT269_RF_CENARIO_MOS  A (nolock),
    	SANT447_GE_CEN_SAN_APC  B (nolock)
WHERE   A.SGL_MEDA  =   B.SGL_MEDA  AND
    B.IDX_CODIGO    =   @SGL_MEDA

IF @RFX_RESULTADO = '021' /* NORMAL-CUSTO */
    SELECT  @RF_CUPOM = 0.0

IF (@ESTOQUE_ORI = '01' OR @ESTOQUE_ORI = '02' OR @ESTOQUE_ORI = '03')
    SELECT  @RF_CUPOM = 0.0

SELECT  @MSGDT = CONVERT(CHAR(10), @RFX_DTERMO, 103) + CONVERT(CHAR(10), @RF_VENCTO, 103) + '#'

--if @@spid = 109 select '@MSGDT', @MSGDT

SELECT  @TAXA_ADIC = 0

IF @RFX_FCALC = '015'
    SELECT  @TAXA_ADIC = @RFX_TAXA

IF @RFX_QTDE <> 0.0
BEGIN
    IF @TIPO_ESTOQUE = '09' AND @ESTOQUE_ORI IN ('01', '02', '03')
        SELECT  @RFX_QTDE = ISNULL(( RFX_QTDE * @RFX_PRINCIPAL / RFX_PRINCIPAL), 0 )
        FROM    RF_INDEXACAO (nolock)
        WHERE   RF_CARACTERISTICA   =   @RF_CARAC_ORI   AND
	        RFX_PARCELA         =   0

    SELECT  @PU_PARTIDA = (@RFX_PRINCIPAL / @RFX_QTDE)

	--if @@spid = 109 select '@PU_PARTIDA', @PU_PARTIDA
END
ELSE
    IF @TIPO_ESTOQUE = '09' AND
       (    SELECT
            ISNULL( RFX_QTDE, 0 )
        FROM
            RF_INDEXACAO (nolock)
        WHERE
            RF_CARACTERISTICA   =   @RF_CARAC_ORI   AND
            RFX_PARCELA         =   0
       ) <> 0
    BEGIN
        SELECT  @PU_PARTIDA = (RFX_PRINCIPAL / RFX_QTDE), --RFX_PU_INICIAL
            @TAXA_MEDIA_U   = RFX_TAXA,
            @TAXA_MEDIA     = RFX_TAXA,
            @RFX_TAXA       = RFX_TAXA
        FROM    RF_INDEXACAO (nolock)
        WHERE   RF_CARACTERISTICA   =   @RF_CARAC_ORI   AND
                RFX_PARCELA         =   0

    END
    ELSE
    BEGIN
        SELECT @MSGAUX = @MSG + @MSGDT + ' * QTDE INVALIDA NA INDEXACAO '
	IF @RETORNA_MSG <> 'S'
	    RAISERROR 70000 @MSGAUX
	ELSE
	    SELECT @MENSAGEM_ERRO = @MSGAUX
        RETURN -100
    END


SELECT
    @COT_VALOR = @TAXA_ADIC , @DTAUX = @RFX_PARTIDA

	--if @@spid = 109 select '@COT_VALOR', @COT_VALOR

IF @ESTOQUE_ORI = '08'
BEGIN
    SELECT  @DTAUX  = MIN(RFM_DATA),
            @DTERMO = MIN(RFM_DTERMO)
    FROM    RF_MOVIMENTACAO (nolock)
    WHERE   RF_CARACTERISTICA   =   @CARAC      AND
            RFM_OK              =   'S'
            
    SELECT @RFM_PU_NEGOC_TERMO = RFM_PU
    FROM RF_MOVIMENTACAO WHERE RF_CARACTERISTICA = @CARAC
    AND RFM_OK   =   'S'
    AND RFM_DATA = @DTAUX            

    IF @DTERMO < @DTFIM
        SELECT  @DTFIM = @DTERMO
END

IF @DTAUX < @DTINI
    SELECT  @DTAUX = @DTINI

SELECT  @IS_IDXMISTO = 0

IF EXISTS(SELECT * FROM RF_IDX_MISTO (nolock) WHERE ATV_CODIGO = @ATV_CODIGO)
    SELECT  @IS_IDXMISTO = -1

IF (@ESTOQUE_ORI = '01' OR @ESTOQUE_ORI = '02' OR @ESTOQUE_ORI = '03')
    SELECT  @IS_IDXMISTO = 0

SELECT  @RFX_FCALC1  = ISNULL(RFIM_FCJUR1,''),
        @RFX_FCALC2  = ISNULL(RFIM_FCJUR2,''),
    	@IDX_CODIGO1 = ISNULL(IDX_CODIGO1,''),
    	@IDX_CODIGO2 = ISNULL(IDX_CODIGO2,''),
    	@DT_VIRADA   = ISNULL(RF_DTVIRADA,'01/01/1900')
FROM    RENDA_FIXA   A (nolock) LEFT OUTER JOIN RF_IDX_MISTO    B (nolock)
            ON (A.ATV_CODIGO = B.ATV_CODIGO AND
                A.IDX_CODIGO = B.IDX_CODIGO)
WHERE   RF_CARACTERISTICA   =   @CARAC

IF @IS_IDXMISTO = -1
BEGIN
    IF @DTAUX > @DT_VIRADA
        SELECT  @RFX_FCALC  =   @RFX_FCALC2,
	        @IDX_CODIGO =   @IDX_CODIGO2,
            	@RF_EMISSAO =   @DT_VIRADA,
            	@RF_CUPOM   =   0.0
    ELSE
        SELECT  @RFX_FCALC  =   @RFX_FCALC1,
            	@IDX_CODIGO =   @IDX_CODIGO1,
            	@RF_VENCTO  =   @DT_VIRADA,
            	@RF_VENCTO_IR   =   @DT_VIRADA
END


IF  @RFX_PARTIDA <> @RFX_VOLTA  AND
    EXISTS (
        SELECT  *
        FROM    RF_MOVIMENTACAO (nolock)
        WHERE   RF_CARACTERISTICA   =   @CARAC      AND
            ID_OVER_LNGA        =   'S'
        )
BEGIN
    SET ROWCOUNT 1
    SELECT  @RF_CUPOM   =   RFM_TAXA
    FROM    RF_MOVIMENTACAO (nolock)
    WHERE   RF_CARACTERISTICA   =   @CARAC
    ORDER BY
        RFM_DATA,
        RFM_HORA

    SET ROWCOUNT 0
END

SELECT  @RF_COND_DTBASE = 'N'

IF  EXISTS( SELECT *
        FROM RF_MERCADORIA (nolock)
        WHERE ATV_CODIGO = @ATV_CODIGO AND ATV_PUBLICO = 'S' )  AND
    EXISTS( SELECT *
        FROM INDEXADOR (nolock)
        WHERE IDX_CODIGO = @IDX_CODIGO AND IDX_PRE <> 'S' ) AND
    (@RF_BASE_CALC IS NOT NULL AND @RF_BASE_CALC <> '19000101') AND
    (@ESTOQUE_ORI <> '01' AND @ESTOQUE_ORI <> '02' AND @ESTOQUE_ORI <> '03')

    SELECT  @RF_COND_DTBASE = 'S'


EXEC @CDI_ANO = SIAN_SP_CDI_ANO 1

IF @CDI_ANO = 0
BEGIN
    SELECT  @IDX_PRE   = IDX_PRE,
	    @IDX_TIPO  = IDX_TIPO,
            @IDX_FCALC = IDX_FCALC
    FROM    INDEXADOR (nolock)
    WHERE   IDX_CODIGO = @IDX_CODIGO
END
ELSE
BEGIN
    SELECT  @IDX_PRE   = A.IDX_PRE,
            @IDX_TIPO  = A.IDX_TIPO,
            @IDX_FCALC = B.IDX_FCALC
    FROM    INDEXADOR       A (nolock),
        INDEXADOR_PERIODO   B (nolock)
    WHERE   A.IDX_CODIGO    =   @IDX_CODIGO AND
        A.IDX_CODIGO    =   B.IDX_CODIGO    AND
        @DTINI      BETWEEN B.IDX_INICIO    AND B.IDX_VALIDADE
END

IF @IDX_PRE = 'S'
    SELECT  @RF_EMISSAO = @RFX_PARTIDA

IF @ESTOQUE_ORI = '01' OR @ESTOQUE_ORI = '02' OR @ESTOQUE_ORI = '03'
BEGIN
    SELECT  @RF_EMISSAO = @RFX_PARTIDA,
            @RF_VENCTO = @RFX_VOLTA

    IF @TIPO_ESTOQUE = '09' AND @DTFIM > @RFX_VOLTA
        SELECT  @DTFIM = @RFX_VOLTA
END


IF @IDX_FCALC = '007' AND EXISTS ( SELECT POS_APELIDO FROM FDO_POSICAO_FUNDO WHERE POS_APELIDO = @EMPRESA )
   /*or @IC_FIDC = 'S'*/ -- 28.07.2004
BEGIN
    SELECT  @CURVA_CTB = 'U'

    IF @ATV_AP = 'A'
        SELECT  @PU_PARTIDA = RFM_PU
        FROM    RF_MOVIMENTACAO (nolock)
        WHERE   RF_CARACTERISTICA   =   @CARAC      AND
                RFM_OK          =   'S'     AND
                RFM_DT          =   'A'
        GROUP BY RFM_DATA,
                RFM_PU
        HAVING   RFM_DATA   =   MIN(RFM_DATA)

    ELSE
        SELECT  @PU_PARTIDA = RFM_PU
        FROM    RF_MOVIMENTACAO (nolock)
        WHERE   RF_CARACTERISTICA   =   @CARAC      AND
                RFM_OK	            =   'S'     AND
                RFM_DT              =   'C'
        GROUP BY RFM_DATA,
             RFM_PU
        HAVING   RFM_DATA   =   MIN(RFM_DATA)
    END

ELSE
BEGIN
    IF @RF_EMISSAO <> @RFX_PARTIDA AND @IDX_CODIGO = 'IGPM'
        SELECT  @RF_EMISSAO = @RFX_PARTIDA,
            @CETIP_SELIC = 'S'
END


IF @RFX_FCALC = '009' OR @RFX_FCALC = '022'
    BEGIN
        IF @IDX_FCALC = '006'
        BEGIN
            SELECT  @VAR_CUPOM_ANBID = @RF_CUPOM,
                    @DT_AUXILIAR = @DTAUX

            IF @DTAUX > @RF_VENCTO
                SELECT  @DT_AUXILIAR = @RF_VENCTO

            EXEC SIAN_CALC_JUROS    '001', @VAR_CUPOM_ANBID OUTPUT, @DT_AUXILIAR,
                        @RF_EMISSAO, @RF_VENCTO, -1, 'A',
                        @FER_CHAVE, 'C', 0, @IDX_FCALC, @IDX_CODIGO, @TAXA_MEDIA_U OUTPUT
        END
        ELSE
        BEGIN


            EXEC SIAN_CALC_JUROS    @RFX_FCALC, @TAXA_MEDIA_U OUTPUT, @RFX_DTERMO,
                        @RF_EMISSAO, @RF_VENCTO, 0, 'A', @FER_CHAVE, 'C', 0

            EXEC SIAN_CALC_TAXA '001', @TAXA_MEDIA_U OUTPUT, @RFX_DTERMO,
                        @RF_EMISSAO, @RF_VENCTO, 0, 'A', @FER_CHAVE, 'C', 0
        END
    END

SELECT
    @DTRESANT = DATEADD(day,-1,@DTAUX)

SELECT
    @QTD_ABE        	=   0.0,    @JUROS_ANT      	=   0.0,
    @CORRECAO_ANT       =   0.0,    @PRINCIPAL      	=   0.0,
    @PRINCIPAL_ABERTURA =   0.0,    @PU_MERC_ANT        =   0.0,
    @PU_CURVA_C     	=   0.0,    @PU_CURVA_U     	=   0.0,
    @VALOR_IR_ANT       =   0.0,    @CORRECAO_ACRU_T    =   0.0,
    @VAL_IOF        	=   0.0,    @PU_UTEIS_ANT       =   0.0,
    @V_FATR_ACUD_UTES   =   1.0,    @V_FATR_ACUD_MERC   =   1.0

SELECT  @QTD_ABE        =   ABS(RFS_QTDE+RFS_QTDE_C-RFS_QTDE_V),
    @TAXA_MEDIA     	=   RFS_TAXA_MEDIA,
    @TAXA_MEDIA_U       =   RFS_TAXA_MEDIA_U,
    @JUROS_ANT      	=   RFS_APROPRIAR_ANT,
    @CORRECAO_ANT       =   RFS_CORRECAO,
    @PRINCIPAL      	=   RFS_PRINCIPAL,
    @PRINCIPAL_ABERTURA =   RFS_PRINCIPAL,
    @PU_MERC_ANT        =   RFS_PU_MERCADO,
    @PU_CURVA_C     	=   RFS_PU_CORRIDOS,
    @PU_CURVA_U     	=   RFS_PU_UTEIS,
    @PU_CURVA_N     	=   V_PU_NEGC, -- LIGIA MUTO - 21/11/2008
    @VALOR_IR_ANT       =   RFS_IR,
    @CORRECAO_ACRU_T    =   RFS_CORRECAO_ACRU,
    @VAL_IOF        	=   RFS_IOF,
    @PU_UTEIS_ANT       =   RFS_PU_UTEIS,
    @V_FATR_ACUD_UTES   =   ISNULL(V_FATR_ACUD_UTES, 1.0),
    @V_FATR_ACUD_MERC   =   ISNULL(V_FATR_ACUD_MERC, 1.0)
FROM    RF_SALDOS (nolock)
WHERE   RF_CARACTERISTICA   =   @CARAC      AND
    RFS_DATA        =   @DTRESANT


-- if @@spid = 65 select 766 'gbvlfi03', * FROM RF_SALDOS WHERE RF_CARACTERISTICA = @CARAC AND RFS_DATA = @DTRESANT
-- if @@spid = 65 select 767 'gbvlfi03', @TAXA_MEDIA TAXA_MEDIA, @DTRESANT DTRESANT, @CARAC CARAC

IF @ATV_AP = 'A'
    SELECT  @AC = 'C'
ELSE
    SELECT  @AC = 'A'

SELECT  @CALC_OVER_LNG = 'S'
FROM    RF_MOVIMENTACAO (nolock)
WHERE   RF_CARACTERISTICA   =   @CARAC      AND
    	ID_OVER_LNGA        =   'S'

IF ( @ESTOQUE_ORI = '01' OR @ESTOQUE_ORI = '02' OR @ESTOQUE_ORI = '03' ) AND
   EXISTS ( SELECT * FROM RF_MOVIMENTACAO (nolock)
        WHERE RF_CARACTERISTICA = @CARAC AND RFM_DATA >= @DTAUX AND
        RFM_OK = 'S' AND CD_CATG_ORIG > 0 )
    SELECT
        @CALC_OVER_LNG = 'S'

IF @CALC_OVER_LNG = 'S' AND @FUNDO = 'N'
BEGIN
    SELECT  @PU_FINAL  = ISNULL(V_PU_VOLT,0)
    FROM    RF_MOVIMENTACAO (nolock)
    WHERE   RF_CARACTERISTICA   =   @CARAC      AND
        ID_OVER_LNGA        =   'S'     AND
        RFM_OK          =   'S'     AND
        RFM_DT          =   @AC     AND
        RFM_DATA        =   @RFX_PARTIDA

    IF @PU_FINAL = 0
        SELECT @PU_FINAL = @RFX_COTACAO_VOLTA
END

SELECT
    @CORRECAO_ANT = @CORRECAO_ANT - ISNULL(SUM(RFM_CORRECAO),0.0)
FROM    RF_MOVIMENTACAO (nolock)
WHERE   RF_CARACTERISTICA   =   @CARAC      AND
    	RFM_DT          =   @AC     AND
    	RFM_DTERMO      =   @DTRESANT   AND
    	RFM_OK          =   'S'


EXEC SANPP_RF_DEL_SALDOS @CARAC, @DTINI

IF @DTFIM > @RF_DTLIQ AND @IC_FIDC <> 'S'
    SELECT  @DTFIM = @RF_DTLIQ

SELECT  @QTD_SALDO = 0,
    	@ACHOU_COTACAO = 'S',
    	@VAR_CORRECAO_P1 = 1.0



IF @IS_IDXMISTO = -1 AND @DTAUX >= @DT_VIRADA AND @RFX_PU_INICIAL = 1.0
BEGIN
    IF EXISTS(  SELECT  *
            FROM    INDEXADOR       A (nolock), 
                INDEXADOR_PERIODO   B (nolock)
            WHERE   A.IDX_CODIGO    =   @IDX_CODIGO1    AND
                A.IDX_CODIGO    =   B.IDX_CODIGO    AND
                A.IDX_PRE   =   'N'     AND
                @DTAUX      BETWEEN B.IDX_INICIO    AND B.IDX_VALIDADE )
    BEGIN

        IF @RF_COND_DTBASE = 'N'
            SELECT @RF_DATA_VAR = @RF_EMISSAO_P1
        ELSE
            SELECT @RF_DATA_VAR = @RF_BASE_CALC

	SET @DTAuxTermo = NULL
	if @ESTOQUE_ORI = '08' AND EXISTS(SELECT IDX_FCALC FROM INDEXADOR(NOLOCK) WHERE IDX_FCALC = '000' AND IDX_CODIGO = @IDX_CODIGO) --PAÇOCA
	begin
		set @DTAuxTermo = @dt_auxiliar
		set @dt_auxiliar = @dtaux_old
		
	end

        EXEC SIAN_SP_VARIACAO   @IDX_CODIGO1, @DT_VIRADA, @RF_DATA_VAR,
                    @DT_VIRADA, @RFX_PERC, @RFX_COTACAO_PARTIDA ,
                    @RFX_COTACAO_VOLTA, @RFX_FCALC,
                    @FER_CHAVE, @RFX_DTANIV, @TAXA_ADIC,
                    @RFX_PU_INICIAL, @VAR_CORRECAO_P1 OUTPUT,
                    @VAR_ERRO OUTPUT, 'U', 0, @CETIP_SELIC,
		    @SGL_SISTEMA = 'RDF' -- Liana - 22/11/10

	--paçoca 
	if @DTAuxTermo IS NOT NULL
	begin
		set @dt_auxiliar = @dtAuxTermo
	end

        SELECT @RFX_PU_INICIAL = @VAR_CORRECAO_P1
    END
END

WHILE @DTAUX <= @DTFIM
BEGIN

   IF @ESTOQUE_ORI = '08'
   BEGIN
	   
	   SELECT @DTAUX_OLD = @DTAUX
	   select @DTAUX = @DTERMO
	   
   END

	--CRISTIANO-13/05/2015-INICIO ------------------------------------------------------------------------------------------------
	IF @FUNDO = 'S' AND @ATV_PUBLICO = 'S'
	BEGIN
		SELECT @PU_PARTIDA = A.RFM_PU, 
			   @QTD_COMPRA = CASE WHEN A.RFM_DT = 'A' THEN A.RFM_QTDE ELSE 0.0 END,
			   @QTD_VENDA  = CASE WHEN A.RFM_DT = 'C' THEN A.RFM_QTDE ELSE 0.0 END
		FROM   RF_MOVIMENTACAO A (NOLOCK)
		WHERE  A.RF_CARACTERISTICA = @CARAC
		AND    A.RFM_DATA  = @DTAUX
		AND    A.RFM_OK    = 'S'
	END
	--CRISTIANO-13/05/2015-FIM ------------------------------------------------------------------------------------------------
   
    /*DANILO 02/10/2006: Para operações de Debentures PRE-Fixadas, pegar o PU Partida calculado no Lastreamento */
    IF RTRIM(@ATV_CODIGO) = 'DEBENTURES' AND @IDX_PRE = 'S' AND ( @ESTOQUE_ORI = '01' OR @ESTOQUE_ORI = '02' OR @ESTOQUE_ORI = '03' )
        SELECT @PU_PARTIDA = @RFX_COTACAO_PARTIDA

    -- DANILO 24/07/2007: NAS COMPROMISSADAS DE PAPEIS PUBLICOS, UTILIZAR O PU PARTIDA DA INDEXACAO, FICANDO COERENTE AO MOS
    IF @ESTOQUE_ORI IN ('01', '02', '03') AND @ATV_PUBLICO = 'S'
        SELECT @PU_PARTIDA = @RFX_COTACAO_PARTIDA

    -- DANILO 05/09/2007: ATUALIZAR O CAMPO PU_INICIAL PARA EVITAR PROBLEMAS NA PRIMEIRA VEZ QUE FOR VALORIZADA A OPERACAO (ESSE CAMPO EH ATUALIZADO NO FIM DA ROTINA)
    IF @ESTOQUE_ORI IN ('01', '02', '03') AND @DTAUX = @RFX_PARTIDA
	SELECT @RFX_PU_INICIAL = @PU_PARTIDA

    SELECT @DT_ULTRES = @DTAUX
    EXEC  SIAN_SP_RESERVA_ANTERIOR @FER_CHAVE,'A',@DT_ULTRES OUTPUT, 0

    IF @ATV_AP = 'A'

        SELECT  @DATA_AQUISICAO = MAX(RFM_DATA)
        FROM    RF_MOVIMENTACAO (nolock)
        WHERE   RF_CARACTERISTICA   	=   @CARAC  AND
                RFM_OK          	=   'S' AND
            	RFM_DATA        	<   @DTAUX  AND
            	RFM_DT          	=   'A'

    ELSE

        SELECT  @DATA_AQUISICAO = MAX(RFM_DATA)
        FROM    RF_MOVIMENTACAO (nolock)
        WHERE   RF_CARACTERISTICA   	=   @CARAC  AND
            	RFM_OK          	=   'S' AND
            	RFM_DATA        	<   @DTAUX  AND
            	RFM_DT          	=   'C'


    -- NO MES DA AQUISICAO, RECALCULA A TAXA
    IF @IDX_FCALC = '007' AND
       DATEPART (yy, @DATA_AQUISICAO) = DATEPART (yy, @DTAUX) AND
       DATEPART (mm, @DATA_AQUISICAO) = DATEPART (mm, @DTAUX) AND
       EXISTS ( SELECT POS_APELIDO FROM FDO_POSICAO_FUNDO WHERE POS_APELIDO = @EMPRESA )

    BEGIN

-- if @@spid = 65 select 892 'gbvlfi03', @TAXA_MEDIA TAXA_MEDIA 

        IF (@RFX_FCALC = '009' OR @RFX_FCALC = '022')
            EXEC SANPP_RF_CALCULA_TAXA  @CARAC,
                            @DTAUX,
                            @TAXA_MEDIA_U   	OUTPUT, -- CORRIDOS
                            @TAXA_MEDIA 	OUTPUT, -- UTEIS
                            @VAR_ERRO   	OUTPUT
        ELSE

            EXEC SANPP_RF_CALCULA_TAXA  @CARAC,
                            @DTAUX,
                            @TAXA_MEDIA 	OUTPUT, -- CORRIDOS
                            @TAXA_MEDIA_U   	OUTPUT, -- UTEIS
                            @VAR_ERRO   	OUTPUT

-- if @@spid = 65 select 908 'gbvlfi03', @TAXA_MEDIA TAXA_MEDIA 

        IF @VAR_ERRO <> -1
        BEGIN
            SELECT @MSGAUX = @MSG + @MSGDT + ' * PROBLEMAS AO RECALCULAR A TAXA DA OPERACAO * '
	    IF @RETORNA_MSG <> 'S'
		RAISERROR 70000 @MSGAUX
	    ELSE
		SELECT @MENSAGEM_ERRO = @MSGAUX
            RETURN -100
        END

        UPDATE  RF_INDEXACAO
        SET RFX_TAXA = @TAXA_MEDIA
        WHERE   RF_CARACTERISTICA   = @CARAC    AND
            RFX_PARCELA     = 0
    END

--SELECT @TAXA_MEDIA TAXA_MEDIA, @DTAUX DTAUX, @DTFIM DTFIM


    SELECT  @QTD_V_CTBL  = ISNULL(SUM(RFM_QTDE),0),
        @PRINC_CTBL  = ISNULL(SUM(RFM_PRINCIPAL),0)
    FROM    RF_MOVIMENTACAO (nolock)
    WHERE   RF_CARACTERISTICA    = @CARAC   AND
        RFM_DATA         = @DTAUX   AND
        RFM_OK           = 'S'      AND
        RFM_DT           = @AC          AND
        ISNULL(IC_CLFC_ESTQ,'N') <> 'S'     --Patricia 07/07/2003


    EXEC @CORTE = SP_PEGA_FATOR_CORTE @RF_MOEDA , @DTRESANT , @DTAUX

    /*** BLOCO 1 ***/

    --IF @QTD_ABE > 0.0 AND NOT ( @ESTOQUE_ORI = '08' AND @RFX_FCALC = '000' )  AND @DTAUX <= @RF_DTLIQ
    --IF (@QTD_ABE > 0.0 OR @ESTOQUE_ORI = '08') AND NOT ( @ESTOQUE_ORI = '08' AND @RFX_FCALC = '000' )AND @DTAUX <= @RF_DTLIQ --CRISTIANO-13/05/2015
    IF (@QTD_ABE > 0.0 or @QTD_COMPRA > 0.0 OR @QTD_VENDA > 0.0 OR @ESTOQUE_ORI = '08') AND NOT ( @ESTOQUE_ORI = '08' AND @RFX_FCALC = '000' )AND @DTAUX <= @RF_DTLIQ --CRISTIANO-13/05/2015
    BEGIN  /* AA */

        SELECT  @PRINCIPAL = @PRINCIPAL / CONVERT(FLOAT,@CORTE),
            @CORRECAO_ACRU_T = @CORRECAO_ACRU_T / CONVERT(FLOAT,@CORTE)

        EXEC TRUNC @PRINCIPAL , 2, @PRINCIPAL OUTPUT
        EXEC TRUNC @CORRECAO_ACRU_T , 2, @CORRECAO_ACRU_T OUTPUT

        SELECT  @VAR_CORRECAO   = 1 ,
            @VAR_CORRECAO_C = 1 ,
            @VAR_CORRECAO_U = 1 ,
            @VAR_ERRO   = 0 ,
            @DT_AUXILIAR    = @DTAUX,
            @DT_FIM_VAR = @RF_DTLIQ,
            @ACHOU_COTACAO  = 'S'

        IF @IDX_FCALC = '001' /* TR */ 
		OR  (@IDX_FCALC = '007' AND @TIPO_ESTOQUE NOT IN ('01','02','03') AND @CETIP_SELIC = 'C') /* IGP-DI, IGP-M, IGPDI, IGPM, IPC-FIPE, IPCA*/ -- Liana - 16/05/2008
        BEGIN
            SELECT  @DT_FIM_VAR = @RF_VENCTO

            IF @DTAUX > @RF_VENCTO
                SELECT  @DT_AUXILIAR = @RF_VENCTO
        END

        SELECT @COT_VALOR = @TAXA_ADIC

        IF @IDX_PRE <> 'S'
        BEGIN
            IF @RF_COND_DTBASE = 'N'
                SELECT @RF_DATA_VAR = @RF_EMISSAO
            ELSE
                SELECT @RF_DATA_VAR = @RF_BASE_CALC

            SELECT @COT_PARTIDA  = @RFX_COTACAO_PARTIDA,
                   @COT_VOLTA    = @RFX_COTACAO_VOLTA

            IF @CALC_OVER_LNG = 'S' AND @FUNDO = 'N'
            BEGIN
                IF @IDX_FCALC = '002' /* DOLAR */
                   SELECT @COT_PARTIDA  = 0,
                      @COT_VOLTA    = 0

		-- DANILO 24/07/2007: NAS COMPROMISSADAS DE PAPEIS PUBLICOS, MANTER O CALCULO DA VARIACAO SEMELHANTE AOS PAPEIS PRIVADOS
                IF @ATV_PUBLICO = 'S' AND @ESTOQUE_ORI NOT IN ('01', '02', '03')
		BEGIN
			SET @DTAuxTermo = NULL
			if @ESTOQUE_ORI = '08' AND EXISTS(SELECT IDX_FCALC FROM INDEXADOR(NOLOCK) WHERE IDX_FCALC = '000' AND IDX_CODIGO = @IDX_CODIGO)--PAÇOCA
			begin
				set @DTAuxTermo = @dt_auxiliar
				set @dt_auxiliar = @dtaux_old
				
			end

			EXEC SIAN_SP_VARIACAO   @IDX_CODIGO, @DTAUX, @DT_ULTRES,
			            @DT_FIM_VAR, @RFX_PERC, @COT_PARTIDA,
			            @COT_VOLTA, @RFX_FCALC,
			            @FER_CHAVE, @RFX_DTANIV, @COT_VALOR OUTPUT,
			            @PU_CURVA_C, @VAR_CORRECAO_C OUTPUT,@VAR_ERRO OUTPUT,
			            @CURVA_CTB, 0, @CETIP_SELIC,'19000101',@CALC_OVER_LNG,
			      	    @SGL_SISTEMA = 'RDF' -- Liana - 22/11/10

			--paçoca 
			if @DTAuxTermo IS NOT NULL
			begin
				set @dt_auxiliar = @dtAuxTermo
			end

		END
                ELSE
		BEGIN
			SET @DTAuxTermo = NULL
			if @ESTOQUE_ORI = '08' AND EXISTS(SELECT IDX_FCALC FROM INDEXADOR(NOLOCK) WHERE IDX_FCALC = '000' AND IDX_CODIGO = @IDX_CODIGO)--PAÇOCA
			begin
				set @DTAuxTermo = @dt_auxiliar
				set @dt_auxiliar = @dtaux_old
				
			end

	                EXEC SIAN_SP_VARIACAO   @IDX_CODIGO, @DTAUX, @RFX_PARTIDA,
	                            @DT_FIM_VAR, @RFX_PERC, @COT_PARTIDA,
	                            @COT_VOLTA, @RFX_FCALC,
	                            @FER_CHAVE, @RFX_DTANIV, @COT_VALOR OUTPUT,
	                            @PU_CURVA_C, @VAR_CORRECAO_C OUTPUT,@VAR_ERRO OUTPUT,
	                            @CURVA_CTB, 0, @CETIP_SELIC,'19000101',@CALC_OVER_LNG,
		    		    @SGL_SISTEMA = 'RDF' -- Liana - 22/11/10

			--paçoca 
			if @DTAuxTermo IS NOT NULL
			begin
				set @dt_auxiliar = @dtAuxTermo
			end

		END

            END
            ELSE
            BEGIN
                IF @CALC_OVER_LNG = 'S' AND @FUNDO = 'S'
                BEGIN
                   IF @IDX_FCALC = '002'
                    SELECT  @COT_PARTIDA  = 0,
                        @COT_VOLTA    = 0
                END

		
		SET @DTAuxTermo = NULL
		if @ESTOQUE_ORI = '08' AND EXISTS(SELECT IDX_FCALC FROM INDEXADOR(NOLOCK) WHERE IDX_FCALC = '000' AND IDX_CODIGO = @IDX_CODIGO)--PAÇOCA
		begin
			set @DTAuxTermo = @dt_auxiliar
			set @dt_auxiliar = @dtaux_old
			
		end

                EXEC SIAN_SP_VARIACAO   @IDX_CODIGO, @DT_AUXILIAR, @RF_DATA_VAR,
                            @DT_FIM_VAR, @RFX_PERC, @COT_PARTIDA,
                            @COT_VOLTA, @RFX_FCALC,
                            @FER_CHAVE, @RFX_DTANIV, @COT_VALOR OUTPUT,
                            @RFX_PU_INICIAL, @VAR_CORRECAO_C OUTPUT,
                            @VAR_ERRO OUTPUT, @CURVA_CTB, 0, @CETIP_SELIC,
		    	    @SGL_SISTEMA = 'RDF' -- Liana - 22/11/10

		--paçoca 
		if @DTAuxTermo IS NOT NULL
		begin
			set @dt_auxiliar = @dtAuxTermo
		end

		-- LIGIA MUTO - 12/05/2008
		-- PADRONIZAÇÃO DOS CRITÉRIOS DE TÍTULOS PÚBLICOS
		-- FATOR ACUMULADO DA TAXA SELIC - TRUNCAR NA 16º CASA 
		IF @CETIP_SELIC = 'S'
			SELECT @VAR_CORRECAO_C = ROUND(@VAR_CORRECAO_C,16,1)

            END
        END
        ELSE

            SELECT  @VAR_CORRECAO_C = 1.0,
                @COT_VALOR = 0.0

        IF @VAR_ERRO <> -1 AND @IDX_PRE = 'N'
            SELECT @ACHOU_COTACAO = 'N'

        IF @VAR_CORRECAO_C = 0.0
            SELECT  @VAR_CORRECAO_C = 1.0


        IF @VAR_CORRECAO_C = 1.0 AND @DTAUX > @RF_VENCTO
            SELECT  @VAR_CORRECAO_C = @CORRECAO_C_ANT

        IF @IDX_PRE <> 'S'
        BEGIN
            IF @RF_COND_DTBASE = 'N'
                SELECT  @RF_DATA_VAR = @RF_EMISSAO
            ELSE
                SELECT  @RF_DATA_VAR = @RF_BASE_CALC

            SELECT  @COT_PARTIDA  = @RFX_COTACAO_PARTIDA,
                @COT_VOLTA    = @RFX_COTACAO_VOLTA

            IF @CALC_OVER_LNG = 'S' AND @FUNDO = 'N' /* Patricia 17/01/2003 */
            BEGIN
                IF @IDX_FCALC = '002' /* DOLAR */
                        SELECT  @COT_PARTIDA  = 0,
                            @COT_VOLTA    = 0

		    -- DANILO 24/07/2007: NAS COMPROMISSADAS DE PAPEIS PUBLICOS, MANTER O CALCULO DA VARIACAO SEMELHANTE AOS PAPEIS PRIVADOS
                    IF @ATV_PUBLICO = 'S' AND @ESTOQUE_ORI NOT IN ('01', '02', '03')
		    BEGIN
			SET @DTAuxTermo = NULL
			if @ESTOQUE_ORI = '08' AND EXISTS(SELECT IDX_FCALC FROM INDEXADOR(NOLOCK) WHERE IDX_FCALC = '000' AND IDX_CODIGO = @IDX_CODIGO)--PAÇOCA
			begin
				set @DTAuxTermo = @dt_auxiliar
				set @dt_auxiliar = @dtaux_old
				
			end

			EXEC SIAN_SP_VARIACAO   @IDX_CODIGO, @DTAUX, @DT_ULTRES,
			        @DT_FIM_VAR, @RFX_PERC, @COT_PARTIDA,
			        @COT_VOLTA, @RFX_FCALC, @FER_CHAVE,
			        @RFX_DTANIV, @TAXA_ADIC, @PU_CURVA_U,
			        @VAR_CORRECAO_U OUTPUT, @VAR_ERRO OUTPUT,
			        'U', 0, @CETIP_SELIC,'19000101', @CALC_OVER_LNG,
				@SGL_SISTEMA = 'RDF' -- Liana - 22/11/10

			--paçoca 
			if @DTAuxTermo IS NOT NULL
			begin
				set @dt_auxiliar = @dtAuxTermo
			end

		    END
                    ELSE
		    BEGIN
			SET @DTAuxTermo = NULL
			if @ESTOQUE_ORI = '08' AND EXISTS(SELECT IDX_FCALC FROM INDEXADOR(NOLOCK) WHERE IDX_FCALC = '000' AND IDX_CODIGO = @IDX_CODIGO)--PAÇOCA
			begin
				set @DTAuxTermo = @dt_auxiliar
				set @dt_auxiliar = @dtaux_old
				
			end

			EXEC SIAN_SP_VARIACAO   @IDX_CODIGO, @DTAUX, @RFX_PARTIDA,
			        @DT_FIM_VAR, @RFX_PERC, @COT_PARTIDA,
			        @COT_VOLTA, @RFX_FCALC, @FER_CHAVE,
			        @RFX_DTANIV, @TAXA_ADIC, @PU_CURVA_U,
			        @VAR_CORRECAO_U OUTPUT, @VAR_ERRO OUTPUT,
			        'U', 0, @CETIP_SELIC,'19000101', @CALC_OVER_LNG,
		    		@SGL_SISTEMA = 'RDF' -- Liana - 22/11/10

			--paçoca 
			if @DTAuxTermo IS NOT NULL
			begin
				set @dt_auxiliar = @dtAuxTermo
			end
		    END
            END
            ELSE
            BEGIN
                IF @CALC_OVER_LNG = 'S' AND @FUNDO = 'S'
                BEGIN
                    IF @IDX_FCALC = '002'
                        SELECT  @COT_PARTIDA  = 0,
                            @COT_VOLTA    = 0
                END


                SELECT @DT_AUXILIAR2 = @DT_AUXILIAR

                EXEC @E_RESERVA = SIAN_E_RESERVA @DT_AUXILIAR2, 'A', @FER_CHAVE, 0

                IF @E_RESERVA = 0
                    EXEC  SIAN_SP_RESERVA_ANTERIOR @FER_CHAVE,'A',@DT_AUXILIAR2 OUTPUT, 0

		SET @DTAuxTermo = NULL
		if @ESTOQUE_ORI = '08' AND EXISTS(SELECT IDX_FCALC FROM INDEXADOR(NOLOCK) WHERE IDX_FCALC = '000' AND IDX_CODIGO = @IDX_CODIGO)--PAÇOCA
		begin
			set @DTAuxTermo = @dt_auxiliar
			set @dt_auxiliar = @dtaux_old
			
		end

                EXEC  SIAN_SP_VARIACAO  @IDX_CODIGO, @DT_AUXILIAR2, @RF_DATA_VAR,
                            @DT_FIM_VAR, @RFX_PERC, @COT_PARTIDA,
                            @COT_VOLTA, @RFX_FCALC, @FER_CHAVE,
                            @RFX_DTANIV, @TAXA_ADIC, @RFX_PU_INICIAL,
                            @VAR_CORRECAO_U OUTPUT, @VAR_ERRO OUTPUT,
                            'U', 0, @CETIP_SELIC,
		    	    @SGL_SISTEMA = 'RDF' -- Liana - 22/11/10

		--paçoca 
		if @DTAuxTermo IS NOT NULL
		begin
			set @dt_auxiliar = @dtAuxTermo
		end

		-- LIGIA MUTO - 12/05/2008
		-- PADRONIZAÇÃO DOS CRITÉRIOS DE TÍTULOS PÚBLICOS
		-- FATOR ACUMULADO DA TAXA SELIC - TRUNCAR NA 16º CASA 
		IF @CETIP_SELIC = 'S'
			SELECT @VAR_CORRECAO_U = ROUND(@VAR_CORRECAO_U,16,1)

            END


        END
        ELSE
            SELECT @VAR_CORRECAO_U = 1.0

        IF @VAR_ERRO <> -1 AND @IDX_PRE = 'N'
            SELECT @ACHOU_COTACAO = 'N'

        IF @VAR_CORRECAO_U = 0.0
            SELECT @VAR_CORRECAO_U = 1.0

        IF @VAR_CORRECAO_U = 1.0 AND @DTAUX > @RF_VENCTO
            SELECT  @VAR_CORRECAO_U = @CORRECAO_U_ANT

        IF @RFX_FCALC = '009' OR @RFX_FCALC = '022'
            SELECT  @FCALC_AUX = '001',
                @VAR_JUROS_C = @TAXA_MEDIA_U

        ELSE

            SELECT  @FCALC_AUX = @RFX_FCALC,
                @VAR_JUROS_C = @TAXA_MEDIA
        IF @CALC_OVER_LNG = 'S' AND @FUNDO = 'N'
        BEGIN
            IF @ATV_PUBLICO = 'S'
                BEGIN
                    EXEC SIAN_CALC_JUROS    @FCALC_AUX, @VAR_JUROS_C OUTPUT, @DTAUX,
                                @DTRESANT, @RF_VENCTO, 1, 'A', @FER_CHAVE,
                                'C', 0

                    SELECT  @VAR_JUROS_U = @TAXA_MEDIA



                    EXEC SIAN_CALC_JUROS    @RFX_FCALC, @VAR_JUROS_U OUTPUT, @DTAUX,
                                @DTRESANT , @RF_VENCTO, 1, 'A', @FER_CHAVE,
                                'U', 0

                END
            ELSE

                BEGIN

                    EXEC SIAN_CALC_JUROS    @FCALC_AUX, @VAR_JUROS_C OUTPUT, @DTAUX,
                                @RFX_PARTIDA, @RF_VENCTO, 1, 'A', @FER_CHAVE,
                                'C', 0

                    SELECT  @VAR_JUROS_U = @TAXA_MEDIA

                    EXEC SIAN_CALC_JUROS    @RFX_FCALC, @VAR_JUROS_U OUTPUT, @DTAUX,
                                @RFX_PARTIDA , @RF_VENCTO, 1, 'A', @FER_CHAVE,
                                'U', 0
                END
        END
        ELSE
        BEGIN

    	    -- LIGIA MUTO - 12/05/2008
	    -- PADRONIZAÇÃO DOS CRITÉRIOS DE TÍTULOS PÚBLICOS
	    -- TAXA DE RETORNO - TRUNCAR NA 4º CASA
	    IF @CETIP_SELIC = 'S' 	    
		    SELECT @VAR_JUROS_C = ROUND(@VAR_JUROS_C,4,1)

	    -- PAÇOCA
	    IF (@ESTOQUE_ORI <>'08' OR @IDX_PRE = 'S')
            BEGIN

            	EXEC SIAN_CALC_JUROS    @FCALC_AUX, @VAR_JUROS_C OUTPUT, @DTAUX,
                        @RF_EMISSAO, @RF_VENCTO, 0, 'A', @FER_CHAVE,
                        'C', 0

	    END
            ELSE
            BEGIN
	       SELECT @VAR_JUROS_C = 1
            END

-- SELECT 'VAR_JUROS_C DEPOIS'
-- SELECT @VAR_JUROS_C

	    -- LIGIA MUTO - 12/05/2008
	    -- PADRONIZAÇÃO DOS CRITÉRIOS DE TÍTULOS PÚBLICOS
	    -- COTAÇÃO - Truncar na 4º casa (truncar na 6º casa)
	    IF @CETIP_SELIC = 'S' AND @ATV_CODIGO = 'LFT (REF)'
		    SELECT @VAR_JUROS_C =  ROUND(@VAR_JUROS_C,6,1)


            SELECT  @VAR_JUROS_U = @TAXA_MEDIA

    	    -- LIGIA MUTO - 12/05/2008
	    -- PADRONIZAÇÃO DOS CRITÉRIOS DE TÍTULOS PÚBLICOS
	    -- TAXA DE RETORNO - TRUNCAR NA 4º CASA
-- /*
-- charlie
	    IF @CETIP_SELIC = 'S' AND @ATV_CODIGO = 'LFT (REF)'
		    SELECT @VAR_JUROS_U = ROUND(@VAR_JUROS_U,4,1)
-- */

-- if @@spid = 65 select 1208 'gbvlfi03', @VAR_JUROS_U VAR_JUROS_U 
-- SELECT 'VAR_JUROS_U ANTES'
-- SELECT @VAR_JUROS_U 

	    EXEC SIAN_CALC_JUROS    @RFX_FCALC, @VAR_JUROS_U OUTPUT, @DTAUX,
                        @RFX_PARTIDA, @RF_VENCTO, 0, 'A', @FER_CHAVE,
                        'U', 0

	    -- LIGIA MUTO - 12/05/2008
	    -- PADRONIZAÇÃO DOS CRITÉRIOS DE TÍTULOS PÚBLICOS
	    -- COTAÇÃO - Truncar na 4º casa (truncar na 6º casa)
	    IF @CETIP_SELIC = 'S' AND @ATV_CODIGO = 'LFT (REF)'
		    SELECT @VAR_JUROS_U =  ROUND(@VAR_JUROS_U,6,1)

-- if @@spid = 65 select 1222 'gbvlfi03', @VAR_JUROS_U VAR_JUROS_U, @TAXA_MEDIA TAXA_MEDIA
-- SELECT 'VAR_JUROS_U DEPOIS'
-- SELECT @VAR_JUROS_U 


            -- 08.09.2004
            SELECT
                @VAR_CUPOM_EMISSAO = @RF_CUPOM

            EXEC SIAN_CALC_JUROS    @RFX_FCALC, @VAR_CUPOM_EMISSAO OUTPUT, @DTAUX,
                        @RFX_PARTIDA, @RF_VENCTO, 0, 'A', @FER_CHAVE,
                        'U', 0

        END

        IF @IDX_PRE = 'S' /* IF CC - INDEXADOR PRE */
            IF @ESTOQUE_ORI = '01' OR @ESTOQUE_ORI = '02' OR @ESTOQUE_ORI = '03'
            BEGIN
                IF @CALC_OVER_LNG = 'S' AND @FUNDO = 'N' --AND @ATV_PUBLICO = 'S'
                    BEGIN

			SELECT  @DIAS_C     	= 0,
			        @DIAS_TOT_C     = 0,
			        @DIAS_U         = 0,
			        @DIAS_TOT_U     = 0

			SELECT
				@DIAS_C       = CONVERT(FLOAT,DATEDIFF(DAY,@DTRESANT, @DTAUX)),
				@DIAS_TOT_C   = CONVERT(FLOAT,DATEDIFF(DAY,@DTRESANT, @RFX_VOLTA))

			SELECT  @DIAS_U       = CONVERT(FLOAT,DATEDIFF(DAY,@DT_ULTRES, @DTAUX)),
				@DIAS_TOT_U   = CONVERT(FLOAT,DATEDIFF(DAY,@DT_ULTRES, @RFX_VOLTA))

			SELECT  @VAR_JUROS_U = @RF_CUPOM

			IF @RFX_FCALC = '009' OR @RFX_FCALC = '022'
				SELECT  @FCALC_AUX = '001',
					@VAR_JUROS_C = @TAXA_MEDIA_U
			ELSE
				SELECT  @FCALC_AUX = @RFX_FCALC,
					@VAR_JUROS_C = @TAXA_MEDIA

			EXEC SIAN_CALC_JUROS    '001',@VAR_JUROS_C OUTPUT,
			            @DTAUX,@RF_EMISSAO,@RF_VENCTO,
			            1,'A',@FER_CHAVE,'C',0
			EXEC SIAN_CALC_JUROS    '022',@VAR_JUROS_U OUTPUT,
			            @DTAUX,@RF_EMISSAO,@RF_VENCTO,
			            1,'A',@FER_CHAVE,'C',0

			-- DANILO 24/07/2007: ARREDONDA SOMENTE PARA PAPEIS PRIVADOS
			IF @ATV_PUBLICO = 'N'
			SELECT
			    @VAR_JUROS_C    =   ROUND(@VAR_JUROS_C, 9),
			    @VAR_JUROS_U    =   ROUND(@VAR_JUROS_U, 9)

			-- DANILO 24/07/2007: NAO "EMPURRAR" MAIS O PU PARA PAPEIS PUBLICOS
			IF (@ESTOQUE_ORI = '01' OR @ESTOQUE_ORI = '02' OR @ESTOQUE_ORI = '03')  AND @CALC_OVER_LNG = 'S' AND @IDX_PRE = 'S' --AND @ATV_PUBLICO = 'N'
			SELECT  @PU_CURVA_C   = @PU_PARTIDA * @VAR_JUROS_C
			ELSE
			SELECT  @PU_CURVA_C   = @PU_CURVA_C * @VAR_JUROS_C
			EXEC @E_RESERVA = SIAN_E_RESERVA @DTAUX, 'A', @FER_CHAVE, 0
			IF @E_RESERVA <> 0
			BEGIN
			        -- DANILO 24/07/2007: NAO "EMPURRAR" MAIS O PU PARA PAPEIS PUBLICOS
	                        IF (@ESTOQUE_ORI = '01' OR @ESTOQUE_ORI = '02' OR @ESTOQUE_ORI = '03')  AND @CALC_OVER_LNG = 'S' AND @IDX_PRE = 'S' --AND @ATV_PUBLICO = 'N'
	                        BEGIN

	                            SELECT  @PU_CURVA_U = ROUND(@PU_PARTIDA * @VAR_JUROS_U, 9)

	                        END
	                        ELSE
	                        BEGIN
	                            SELECT  @PU_CURVA_U = @PU_CURVA_U * @VAR_JUROS_U
	                            SELECT @PU_CURVA_U = FLOOR(CONVERT(NUMERIC(22,11),@PU_CURVA_U) * CONVERT(FLOAT,10000000000))/CONVERT(FLOAT,10000000000)

	                        END
			END

			SELECT  @PU_CURVA_C = FLOOR(CONVERT(NUMERIC(22,11),@PU_CURVA_C) * CONVERT(FLOAT,10000000000))/CONVERT(FLOAT,10000000000)

                    END
                    ELSE
                    BEGIN

			SELECT  @VAR_JUROS_C = @TAXA_MEDIA_U

			EXEC SIAN_CALC_JUROS    '001',@VAR_JUROS_C OUTPUT,
			        @DTAUX,@RF_EMISSAO,@RF_VENCTO,
			        1,'A',@FER_CHAVE,'C',0

			SELECT  @VAR_JUROS_U = @TAXA_MEDIA

			EXEC  SIAN_CALC_JUROS   @RFX_FCALC,@VAR_JUROS_U OUTPUT,
			        @DTAUX, @RF_EMISSAO,@RF_VENCTO,
			        1,'A', @FER_CHAVE,'U',0
			
			SELECT  @PU_PARTIDA = RFX_PU_INICIAL
			FROM
			RF_INDEXACAO (nolock)
			WHERE
			RF_CARACTERISTICA   =   @CARAC  AND
			RFX_PARCELA     =   0

			SELECT  @PU_CURVA = @PU_PARTIDA * @VAR_JUROS_C,
			@PU_CURVA_C = @PU_CURVA

			SELECT  @PU_CURVA = @PU_PARTIDA * @VAR_JUROS_U,
			@PU_CURVA_U = @PU_CURVA

                    END

            END
            ELSE
            IF @ATV_COMPOSTO = 'N' /* JUROS SIMPLES AT */
            BEGIN   /* IF DD */

                SELECT @VAR_CUPOM = @TAXA_MEDIA

                EXEC SIAN_CALC_JUROS    @RFX_FCALC, @VAR_CUPOM OUTPUT, @DTAUX,
                            @RF_EMISSAO, @RF_VENCTO, -1, 'A',
                            @FER_CHAVE, 'C', 0

                SELECT  @PU_CURVA = (@VAR_CUPOM - (@VAR_JUROS_C - 1.)) / @VAR_CUPOM
                SELECT  @PU_CURVA_C = @PU_CURVA * @ATV_LOTE
                SELECT  @PU_CURVA = (@VAR_CUPOM - (@VAR_JUROS_U - 1.)) / @VAR_CUPOM
                SELECT  @PU_CURVA_U = @PU_CURVA * @ATV_LOTE

            END
            ELSE   /* ELSEIF DD */
            BEGIN

                IF @RFX_FCALC = '009' AND (@ESTOQUE_ORI = '01' OR @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
                    SELECT  @PU_CURVA = @RFX_COTACAO_VOLTA / @VAR_JUROS_C
                ELSE
                    SELECT  @PU_CURVA = @ATV_LOTE / @VAR_JUROS_C

                SELECT  @PU_CURVA_C = @PU_CURVA

		-- LIGIA MUTO - 12/05/2008
		-- PADRONIZAÇÃO DOS CRITÉRIOS DE CÁLCULO DOS TÍTULOS PÚBLICOS
		-- PREÇO UNITÁRIO - Truncar na 6º casa

		IF @CETIP_SELIC = 'S'
			SELECT @PU_CURVA_C = ROUND(@PU_CURVA_C,6,1)


                IF @RFX_FCALC = '009' AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
                    SELECT @PU_CURVA = @RFX_COTACAO_VOLTA / @VAR_JUROS_U
                ELSE
                    SELECT @PU_CURVA = @ATV_LOTE / @VAR_JUROS_U

                SELECT  @PU_CURVA_U = @PU_CURVA
		
		-- LIGIA MUTO - 12/05/2008
		-- PADRONIZAÇÃO DOS CRITÉRIOS DE CÁLCULO DOS TÍTULOS PÚBLICOS
		-- PREÇO UNITÁRIO - Truncar na 6º casa
-- charlie
-- 		IF @CETIP_SELIC = 'S'
-- 			SELECT @PU_CURVA_U = ROUND(@PU_CURVA_U,6,1)

-- if @@spid = 65 select 1381 'gbvlfi03', @PU_CURVA_U PU_CURVA_U, @CETIP_SELIC CETIP_SELIC, @PU_CURVA PU_CURVA, @RFX_FCALC RFX_FCALC, @ESTOQUE_ORI ESTOQUE_ORI, @ATV_LOTE ATV_LOTE, @VAR_JUROS_U VAR_JUROS_U

            END /* FIMIF DD */

        ELSE   /* ELSEIF CC - INDEXADOR POS */
        BEGIN   /* BEGIN CC-2 */
            SELECT
                @VAR_CUPOM = @RF_CUPOM,
                @VAR_CUPOM_ANBID = @RF_CUPOM,
                @DT_AUXILIAR = @DTAUX

            IF @DTAUX > @RF_VENCTO
                SELECT  @DT_AUXILIAR = @RF_VENCTO

            EXEC SIAN_CALC_JUROS    @RFX_FCALC, @VAR_CUPOM OUTPUT, @DT_AUXILIAR,
                        @RF_EMISSAO, @RF_VENCTO, -1, 'A',
                        @FER_CHAVE, 'C', 0

            IF @IDX_FCALC = '006'
                EXEC SIAN_CALC_JUROS    '001', @VAR_CUPOM_ANBID OUTPUT, @DT_AUXILIAR,
                            @RF_EMISSAO, @RF_VENCTO, -1, 'A',
                            @FER_CHAVE, 'C', 0, @IDX_FCALC, @IDX_CODIGO, @TAXA_MEDIA_U OUTPUT

            IF @CURVA_CTB = 'X' AND NOT(@CALC_OVER_LNG = 'S' AND @FUNDO = 'N')
            BEGIN
                SELECT  @VAR_JUROS_C = 1 / ROUND(@VAR_CUPOM/@VAR_JUROS_C,9)
                SELECT  @VAR_JUROS_U = 1 / ROUND(@VAR_CUPOM/@VAR_JUROS_U,9)
                SELECT  @VAR_CUPOM = 1.0
            END

            IF @ATV_COMPOSTO = 'N' /* JUROS SIMPLES - AT */
            BEGIN
                SELECT  @PU_VOLTA = @VAR_CUPOM * @ATV_LOTE
                SELECT  @PU_CURVA = (@PU_VOLTA - (@VAR_JUROS_C - 1.0)),
                    @PU_CURVA_C = (@VAR_CORRECAO_C + (@PU_CURVA - 1.0))
                SELECT  @PU_CURVA = (@PU_VOLTA - (@VAR_JUROS_U - 1.0)),
                    @PU_CURVA_U = (@VAR_CORRECAO_C + (@PU_CURVA - 1.0))

            END
            ELSE
            BEGIN

                -- 23.11.2004
                IF @CURVA_CTB = 'X' AND NOT(@CALC_OVER_LNG = 'S' AND @FUNDO = 'N')
                BEGIN

                    SELECT  @VAR_CORRECAO_C = ROUND(@VAR_CORRECAO_C, 8)
                    SELECT  @VAR_CORRECAO_U = ROUND(@VAR_CORRECAO_U, 8)

                END

                IF @CALC_OVER_LNG ='S' AND @FUNDO = 'N'
                BEGIN
                    EXEC TRUNC @VAR_CORRECAO_U,10,@VAR_CORRECAO_U OUTPUT
                    EXEC TRUNC @VAR_CORRECAO_C,10,@VAR_CORRECAO_C OUTPUT
                    EXEC TRUNC @VAR_JUROS_U,10,@VAR_JUROS_U OUTPUT
                    EXEC TRUNC @VAR_JUROS_C,10,@VAR_JUROS_C OUTPUT

		    -- DANILO 24/07/2007: NAS COMPROMISSADAS CDI DE PAPEIS PUBLICOS, ARREDONDAR NA OITAVA CASA, DE ACORDO COM REGRA DA CETIP
		    IF @ATV_PUBLICO = 'S' AND @IDX_CODIGO = 'CDI'
			SELECT	@VAR_CORRECAO_U = ROUND(@VAR_CORRECAO_U, 8),
				@VAR_CORRECAO_C = ROUND(@VAR_CORRECAO_C, 8)

                    EXEC @E_RESERVA = SIAN_E_RESERVA @DTAUX, 'A', @FER_CHAVE, 0

                    IF @E_RESERVA <> 0
                    BEGIN
		        -- DANILO 24/07/2007: NAO "EMPURRAR" MAIS O PU PARA PUBLICOS
                        IF @ATV_PUBLICO = 'N' OR (@ATV_PUBLICO = 'S' AND @IDX_CODIGO = 'CDI')
                            SELECT @PU_CURVA_U = @PU_PARTIDA, @PU_CURVA_C = @PU_PARTIDA

                        SELECT  @PU_CURVA_U = @PU_CURVA_U * @VAR_CORRECAO_U * @VAR_JUROS_U,
                            @PU_CURVA_C = @PU_CURVA_C * @VAR_CORRECAO_C * @VAR_JUROS_C

                        SELECT  @PU_CURVA_U = FLOOR(CONVERT(NUMERIC(22,11),@PU_CURVA_U) * CONVERT(FLOAT,10000000000))/CONVERT(FLOAT,10000000000)

                    END
                    ELSE
                    BEGIN

		        -- DANILO 24/07/2007: NAO "EMPURRAR" MAIS O PU PARA PUBLICOS
                        IF @ATV_PUBLICO = 'N' OR (@ATV_PUBLICO = 'S' AND @IDX_CODIGO = 'CDI')
                            SELECT  @PU_CURVA_C = @PU_PARTIDA * @VAR_CORRECAO_C * @VAR_JUROS_C
                        ELSE
                            SELECT  @PU_CURVA_C = @PU_CURVA_C * @VAR_CORRECAO_C * @VAR_JUROS_C
                    END

                END
                ELSE
                BEGIN
                    IF (@RFX_FCALC = '009' OR @RFX_FCALC = '022') AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
                        SELECT @PU_VOLTA = @VAR_CORRECAO_C * @VAR_CUPOM
                    ELSE
                        SELECT @PU_VOLTA = @VAR_CORRECAO_C * @VAR_CUPOM * @ATV_LOTE
		    
                    -- LIGIA MUTO - 12/05/2008
		    -- PADRONIZAÇÃO DOS CRITÉRIOS DE CÁLCULO DE TÍTULOS PÚBLICOS
		    -- VALOR NOMINAL - truncar na 6º casa

		    IF @CETIP_SELIC = 'S'
			    SELECT @PU_VOLTA = ROUND(@PU_VOLTA,6,1)	

	
                    SELECT  @PU_CURVA = @PU_VOLTA / @VAR_JUROS_C
	            SELECT  @PU_CURVA_C = @PU_CURVA

		    -- LIGIA MUTO - 12/05/2008
		    -- PADRONIZAÇÃO DOS CRITÉRIOS DE CÁLCULO DE TÍTULOS PÚBLICOS
		    -- PREÇO UNITÁRIO - TRUNCAR NA 6º CASA
		    IF @CETIP_SELIC = 'S' 
			    SELECT @PU_CURVA_C = ROUND(@PU_CURVA_C,6,1)

                    /* ANBID */
                    IF @IDX_FCALC = '006'
                        SELECT  @PU_CURVA_C = @VAR_CORRECAO_C * @VAR_CUPOM_ANBID * @ATV_LOTE

                    IF (@RFX_FCALC = '009' OR @RFX_FCALC = '022') AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
                        SELECT  @PU_VOLTA = @VAR_CORRECAO_U * @VAR_CUPOM /*  * @RFX_COTACAO_VOLTA*/
                    ELSE
                        SELECT  @PU_VOLTA = @VAR_CORRECAO_U * @VAR_CUPOM * @ATV_LOTE
		
	                    -- LIGIA MUTO - 12/05/2008
			    -- PADRONIZAÇÃO DOS CRITÉRIOS DE CÁLCULO DE TÍTULOS PÚBLICOS
			    -- VALOR NOMINAL - truncar na 6º casa
        		    IF @CETIP_SELIC = 'S'
        			    SELECT @PU_VOLTA = ROUND(@PU_VOLTA,6,1)

		    	    -- LIGIA MUTO - 12/05/2008
			    -- PADRONIZAÇÃO DOS CRITÉRIOS DE TÍTULOS PÚBLICOS
			    -- TAXA DE RETORNO - TRUNCAR NA 4º CASA
        		    IF @CETIP_SELIC = 'S'
        			    SELECT @VAR_JUROS_U = ROUND(1/@VAR_JUROS_U,6,1)
        

			    IF @CETIP_SELIC = 'S'
		                    SELECT  @PU_CURVA = @PU_VOLTA * @VAR_JUROS_U,
	     		                    @PU_CURVA_U = @PU_CURVA
			    ELSE
			            SELECT  @PU_CURVA = @PU_VOLTA / @VAR_JUROS_U,
	      		                    @PU_CURVA_U = @PU_CURVA
		        
        
  		    -- LIGIA MUTO - 12/05/2008
	 	    -- PADRONIZAÇÃO DOS CRITÉRIOS DE CÁLCULO DOS TÍTULOS PÚBLICOS
		    -- PREÇO UNITÁRIO - Truncar na 6º casa
                    IF @CETIP_SELIC = 'S'
        			    SELECT @PU_CURVA_U = ROUND(@PU_CURVA_U,6,1)

-- if @@spid = 65 select 1543 'gbvlfi03', @PU_CURVA_U PU_CURVA_U, @CETIP_SELIC CETIP_SELIC

                    -- 08.09.2004
                    SELECT  @PU_CURVA_EMISSAO = @PU_CURVA_U

                END
            END

-- ROBERTO CARLOS - 09/12/2005

            SELECT      @NRO_OPERACAO       =   ''  ,
                    @VFL_PERC_NEGOCIADO =   NULL    ,
                    @VFL_PU_NEGOCIADO   =   NULL    ,
                    @V_RFM_DATA             =       NULL    ,
                    @V_RFM_DATA_REG     =       NULL    ,
                    @V_RFM_HORA         =       NULL



            SELECT @V_RFM_DATA     =   MIN(RFM_DATA)
                           FROM RF_MOVIMENTACAO (nolock)

                           WHERE RF_CARACTERISTICA = @CARAC


            SELECT @V_RFM_DATA_REG = MIN(RFM_DATA_REGISTRO)
                         FROM  RF_MOVIMENTACAO (nolock)

                         WHERE RFM_DATA = @V_RFM_DATA
                         AND   RF_CARACTERISTICA = @CARAC


            SELECT @V_RFM_HORA     = MIN(RFM_HORA)
                         FROM RF_MOVIMENTACAO (nolock)

                         WHERE RFM_DATA             =   @V_RFM_DATA      AND
                               RFM_DATA_REGISTRO    =   @V_RFM_DATA_REG  AND
                               RF_CARACTERISTICA  =   @CARAC




            SELECT
                @NRO_OPERACAO       =   NRO_OPERACAO,
                @VFL_PU_NEGOCIADO   =   RFM_PU
            FROM
                RF_MOVIMENTACAO A (nolock)
            WHERE
                RF_CARACTERISTICA   =   @CARAC           AND
                RFM_DATA        =   @V_RFM_DATA      AND
                RFM_DATA_REGISTRO   =   @V_RFM_DATA_REG  AND
                RFM_HORA        =   @V_RFM_HORA



/*
--SELECT 'BICUDO8'
            SELECT
                @NRO_OPERACAO       =   '',
                @VFL_PERC_NEGOCIADO =   NULL,
                @VFL_PU_NEGOCIADO   =   NULL

            SELECT
                @NRO_OPERACAO       =   NRO_OPERACAO,
                @VFL_PU_NEGOCIADO   =   RFM_PU
            FROM
                RF_MOVIMENTACAO A
            WHERE
                RF_CARACTERISTICA   =   @CARAC  AND
                RFM_DATA        =   (SELECT MIN(RFM_DATA) FROM RF_MOVIMENTACAO WHERE RF_CARACTERISTICA = @CARAC)    AND
                RFM_DATA_REGISTRO   =   (SELECT MIN(RFM_DATA_REGISTRO) FROM RF_MOVIMENTACAO WHERE RF_CARACTERISTICA = @CARAC AND RFM_DATA = A.RFM_DATA) AND
                RFM_HORA        =   (SELECT MIN(RFM_HORA) FROM RF_MOVIMENTACAO WHERE RF_CARACTERISTICA = @CARAC AND RFM_DATA = A.RFM_DATA AND RFM_DATA_REGISTRO = A.RFM_DATA_REGISTRO)

*/
            IF @NRO_OPERACAO <> ''
                BEGIN

                SELECT
                    @VFL_PERC_NEGOCIADO =   PC_IDXR
                FROM
                    SANT1501_RF_PC_IDXR (nolock)
                WHERE
                    NRO_OPERACAO    =   @NRO_OPERACAO   AND
                    DT_INIC_PC  <=  @DTAUX      AND
                    DT_FIM_PC   >=  @DTAUX      AND
                    ID_T_PC     =   'P'

                   IF ISNULL(@VFL_PERC_NEGOCIADO, 0) = 0
		   BEGIN
	                SELECT
	                    @VFL_PERC_NEGOCIADO =   a.PC_IDXR
	                FROM
	                    SANT1501_RF_PC_IDXR a (nolock), SANT819_RF_OPERACAO_SIAN b (nolock)
	                WHERE
                            a.cd_oprc       =   b.cd_oprc     AND
	                    b.NRO_OPERACAO  =   @NRO_OPERACAO AND
	                    a.DT_INIC_PC    <=  @DTAUX        AND
	                    a.DT_FIM_PC     >=  @DTAUX        AND
	                    a.ID_T_PC       =   'P'
	 	   END

                END

--SELECT @NRO_OPERACAO NRO_OPERACAO, @DTAUX DTAUX, @VFL_PERC_NEGOCIADO VFL_PERC_NEGOCIADO, @VFL_PU_NEGOCIADO VFL_PU_NEGOCIADO

                IF ISNULL(@VFL_PERC_NEGOCIADO, 0) <> 0
                    BEGIN

		    SET @DTAuxTermo = NULL
		    if @ESTOQUE_ORI = '08' AND EXISTS(SELECT IDX_FCALC FROM INDEXADOR(NOLOCK) WHERE IDX_FCALC = '000' AND IDX_CODIGO = @IDX_CODIGO)--PAÇOCA
		    begin

			set @DTAuxTermo = @dt_auxiliar
			set @dt_auxiliar = @dtaux_old
			
		    end

                    EXEC SIAN_SP_VARIACAO   @IDX_CODIGO, @DTAUX, @RF_EMISSAO,
                                @DT_FIM_VAR, @VFL_PERC_NEGOCIADO, @COT_PARTIDA,
                                @COT_VOLTA, @RFX_FCALC,
                                @FER_CHAVE, @RFX_DTANIV, 0 ,
                                @VFL_PU_NEGOCIADO, @VFL_PU_NEGOCIADO OUTPUT,@VAR_ERRO OUTPUT,
                                'U', 0, @CETIP_SELIC,'19000101',
		    		@SGL_SISTEMA = 'RDF' -- Liana - 22/11/10

		   --paçoca 
		   if @DTAuxTermo IS NOT NULL
		   begin
			set @dt_auxiliar = @dtAuxTermo
		   end


                    END

                ELSE
                    SELECT @VFL_PU_NEGOCIADO = NULL
        END  /* END CC-2 */
    END   /* AA */
    ELSE   /* ELSEIF AA*/

    BEGIN   /* AA2 */
        IF @CALC_OVER_LNG ='S' AND @FUNDO = 'N' /* Patricia 17/01/03 */
           SELECT @PU_PARTIDA = FLOOR(CONVERT(NUMERIC(22,11),@PU_PARTIDA) * CONVERT(FLOAT,10000000000))/CONVERT(FLOAT,10000000000)

        SELECT  @PU_CURVA_C = @PU_PARTIDA,
            @PU_CURVA_U = @PU_PARTIDA,
            @PU_MERCADO = @PU_PARTIDA,
            @VAR_CORRECAO = 1.0 ,
            @VAR_CORRECAO_C = 1 ,
            @VAR_CORRECAO_U = 1 ,
            @DT_AUXILIAR    = @DTAUX,
            @DT_FIM_VAR = @RF_DTLIQ,
            @ACHOU_COTACAO  = 'S'

-- if @@spid = 65 select 1681 'gbvlfi03', @PU_MERCADO PU_MERCADO
    END   /* AA2 */

    --FIDC
    IF @IC_FIDC = 'S'
    BEGIN

        -- SE NAO FOR DIA UTIL, PU ANTERIOR

        EXEC @E_RESERVA = SIAN_E_RESERVA @DTAUX, 'A', @FER_CHAVE, 0

        IF @E_RESERVA = 0
        BEGIN

            SELECT
                @PU_CURVA_C = RFS_PU_CORRIDOS,
                @PU_CURVA_U = RFS_PU_UTEIS,
                @PU_MERCADO = RFS_PU_MERCADO,
                @RFX_RESULTADO = '007'
            FROM
                RF_SALDOS (nolock)
            WHERE
                RF_CARACTERISTICA   = @CARAC    AND
                RFS_DATA        = @DT_ULTRES

-- if @@spid = 65 select 1706 'gbvlfi03', @PU_MERCADO PU_MERCADO

        END
        ELSE
        BEGIN
            IF @DTAUX > @RF_DTLIQ
                SELECT
                    @PU_CURVA_C = @ATV_LOTE,
                    @PU_CURVA_U = @ATV_LOTE,
                    @PU_MERCADO = @ATV_LOTE,
                    @RFX_RESULTADO = '007'
-- if @@spid = 65 select 1717 'gbvlfi03', @PU_MERCADO PU_MERCADO, @DTAUX DTAUX, @RF_DTLIQ RF_DTLIQ
	    --DANILO ABR-2007: BUSCA NOVAMENTE O RFX_RESULTADO, POIS, CASO O PERIODO DE VALORIZACAO INCLUA DIAS NAO-UTEIS, O RESULTADO NAO FICARA INCORRETO
	    SELECT @RFX_RESULTADO = RFX_RESULTADO
	    FROM   RF_INDEXACAO (nolock)
	    WHERE  RF_CARACTERISTICA   = @CARAC

        END
    END

    /*** BLOCO 2 ***/

    IF @DTAUX = @DT_VIRADA AND @IS_IDXMISTO = -1
        SELECT  @TAXA_MEDIA   = 0.0,
            @TAXA_MEDIA_U = 0.0

    EXEC @E_RESERVA = SIAN_E_RESERVA @DTAUX, 'A', @FER_CHAVE, 0

    	IF @E_RESERVA = 0 AND @ESTOQUE_ORI <> '08' -- Liana - 14/10/2009
	begin
	        IF @ACHOU_CEN_DIA_ANT = 'S' OR @RFX_RESULTADO <> '001'
	            SELECT @PU_MERCADO = @PU_MERC_ANT
	        ELSE
	            IF @CTB_CURVA = 'C'
	                SELECT @PU_MERCADO = @PU_CURVA_C
	            ELSE
	                IF @CTB_CURVA = 'U'
	                    SELECT  @PU_MERCADO = @PU_CURVA_U
	                ELSE
	                    SELECT  @PU_MERCADO = @PU_MERC_ANT
	-- if @@spid = 65 select 1747 'gbvlfi03', @PU_MERCADO PU_MERCADO, @ACHOU_CEN_DIA_ANT ACHOU_CEN_DIA_ANT, @RFX_RESULTADO RFX_RESULTADO, @CTB_CURVA CTB_CURVA
	end
	ELSE   /* @DTAUX E RESERVA */
	IF @RFX_RESULTADO = '005' AND @ESTOQUE_ORI <> '08' -- Liana - 14/10/2009
	begin
            	SELECT  @PU_MERCADO = @PU_CURVA_C
		-- if @@spid = 65 select 1753 'gbvlfi03', @PU_MERCADO PU_MERCADO
	end
        ELSE
	IF @RFX_RESULTADO = '007' AND @ESTOQUE_ORI <> '08' -- Liana - 14/10/2009
	begin
	                SELECT  @PU_MERCADO = @PU_CURVA_U /* UTEIS */
	-- if @@spid = 65 select 1759 'gbvlfi03', @PU_MERCADO PU_MERCADO
	end
	ELSE
	BEGIN
	
		IF @RFX_FCALC = '009' OR @RFX_FCALC = '022'
		    SELECT  @PU_MERCADO = @PU_CURVA_U
		ELSE
		    SELECT  @PU_MERCADO = @PU_CURVA_C

-- if @@spid = 65 select 1768 'gbvlfi03', @PU_MERCADO PU_MERCADO, @RFX_FCALC RFX_FCALC

                /* TAXA LINEAR */
                IF @RFX_FCALC='003' OR @RFX_FCALC='004' OR
                   @RFX_FCALC='012' OR @RFX_FCALC='018'

                    SELECT  @PU_MERCADO     = @VAR_CORRECAO_C,
                            @TAXA_MEDIA_BBC = @RF_CUPOM

-- if @@spid = 65 select 1777 'gbvlfi03', @PU_MERCADO PU_MERCADO, @RFX_FCALC RFX_FCALC

                --P/ 09 SEM QTDE ABERTURA NAO CALCULA MERCADO
                --IF ((@QTD_ABE > 0 AND @TIPO_ESTOQUE = '09') OR @TIPO_ESTOQUE <> '09') AND
				--if (@MERCADO_1DIA = 'S' OR (@MERCADO_1DIA = 'N' AND @DTAUX <> @DTCAUTELA))

                if @MERCADO_1DIA = 'S' OR ((@MERCADO_1DIA = 'N' AND @DTAUX <> @DTCAUTELA) AND NOT (@ESTOQUE_ORI IN ('02', '03') AND @DTAUX = @RFX_PARTIDA) )
                BEGIN
                    SELECT  @VAR_ERRO = 0

                    /*PARA LFT UTILIZAR O PU PAR - SEM AGIO/DESAGIO DA TAXA*/
                    IF EXISTS (
                           SELECT
                            A.IDX_FCALC
                           FROM
                            SANT269_RF_CENARIO_MOS  A (nolock),
                            SANT447_GE_CEN_SAN_APC  B (nolock)
                           WHERE
                            A.SGL_MEDA  = B.SGL_MEDA    AND
                            B.IDX_CODIGO    = @SGL_MEDA AND
                            A.IDX_FCALC IN ('002','005') --Patricia 20/08/04 (Inclusao 005/Desagio Privados)
                          ) AND @IDX_PRE = 'N'
                    BEGIN
                        IF @RFX_FCALC = '009' OR @RFX_FCALC = '022'
                            SELECT  @TIPO_CALC = 'U'
                        ELSE
                            SELECT  @TIPO_CALC = 'C'

                        IF @RF_COND_DTBASE = 'N'
                            SELECT  @RF_DATA_VAR = @RF_EMISSAO
                        ELSE
                            SELECT  @RF_DATA_VAR = @RF_BASE_CALC

                        SELECT
                            @PU_PAR         = 1.0,
                            @TAXA_MEDIA_BBC = 0.0,
                            @VAR_ERRO       = 0,
                            @CEN_VALOR      = 0.0

                        IF @ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03'
                            SELECT
                                @PUINI = CONVERT (FLOAT, 1)
                        ELSE
                            SELECT
                                @PUINI = @RFX_PU_INICIAL


			SET @DTAuxTermo = NULL
			if @ESTOQUE_ORI = '08' AND EXISTS(SELECT IDX_FCALC FROM INDEXADOR(NOLOCK) WHERE IDX_FCALC = '000' AND IDX_CODIGO = @IDX_CODIGO) --PAÇOCA
			begin
				set @DTAuxTermo = @dt_auxiliar
				set @dt_auxiliar = @dtaux_old
				
			end
                        EXEC SIAN_SP_VARIACAO   @IDX_CODIGO,
                                    @DT_AUXILIAR,
                                    @RF_DATA_VAR,
                                    @DT_FIM_VAR,
                                    @RFX_PERC,
                                    @RFX_COTACAO_PARTIDA,
                                    @RFX_COTACAO_VOLTA,
                                    @RFX_FCALC,
                                    @FER_CHAVE,
                                    @RFX_DTANIV,
                                    @TAXA_ADIC,
                                    @PUINI,
                                    @PU_PAR     OUTPUT,
                                    @VAR_ERRO   OUTPUT,
                                    @TIPO_CALC,
                                    0,
                                    @CETIP_SELIC,
		    		    @SGL_SISTEMA = 'RDF' -- Liana - 22/11/10

			--paçoca
			if @DTAuxTermo IS NOT NULL
			begin
				set @dt_auxiliar = @dtAuxTermo
				
			end

                        SELECT  @PU_PAR = @PU_PAR * @ATV_LOTE

                        IF @VAR_ERRO = -1
                            SELECT  @PU_MERCADO = @PU_PAR

-- if @@spid = 65 select 1846 'gbvlfi03', @PU_MERCADO PU_MERCADO
-- SELECT 'PU_MERCADO_INICIAL_LFT'
-- SELECT @PU_MERCADO

                    END

                    IF @RFX_RESULTADO = '006' /* NORMAL PU */
                        SELECT  @IDX_MERCADO = @IDX_GERENCIAL
                    ELSE
                        SELECT  @IDX_MERCADO = @IDX_CODIGO

                    /* CDI E RESERVA */
                    IF @CEN_FCALC = '003' OR @CEN_FCALC = '006' --Patricia 20/08/04 (Inclusao 006/Perc. Privados)
			begin
			                        SELECT  @PU_MERCADO = @RFX_PERC
			-- if @@spid = 65 select 1863 'gbvlfi03', @PU_MERCADO PU_MERCADO
			end
			 --PAÇOCA

		    IF @ESTOQUE_ORI = '08'
		    BEGIN
				

			SELECT 
				@POS_DIA_CENARIO = POS_DIA_CENARIO 
			FROM
				POSICAO
			WHERE
				POS_APELIDO	= @EMPRESA


			EXEC SANPS_RF_MARCACAO_MERCADO  @IDX_MERCADO,
				@PU_MERCADO OUTPUT,
				@PU_CURVA_U,
				@PU_CURVA_C,
				@DTERMO, --PAÇOCA DATA DE VENCTO DO TERMO
				@RF_EMISSAO,
				@RF_VENCTO,
				@RFX_RESULTADO,
				@RFX_FCALC,
				@TAXA_MEDIA_U,
				@TAXA_MEDIA,
				@TAXA_MEDIA_BBC OUTPUT,
				'A',
				@FER_CHAVE,
				@SGL_MEDA,
				@ATV_CODIGO,
				@VAR_ERRO   OUTPUT,
				0,
         			@ESTOQUE_ORI,
				--@DTAUX,--' PARAMETRO NOVO COM A DATA DE VALORIZAÇÃO.
				@DTAUX_OLD, --PAÇOCA PARAMETRO NOVO COM A DATA DE VALORIZAÇÃO.
				@IC_CEN_VCMT -- paçoca parametro novo

			
			SELECT @PU_CURVA_U = @RFM_PU_NEGOC_TERMO,
				@PU_CURVA_C = @RFM_PU_NEGOC_TERMO
								
			EXEC SANPP_RF_CALC_PU_DIA_TERMO
							@PU_MERCADO OUTPUT,
							@PU_CURVA_U OUTPUT ,
							@PU_CURVA_C OUTPUT ,
							@DTAUX_OLD,
							@DTERMO,
							@RF_VENCTO	,		
							@IC_CEN_VCMT,	
							@FER_CHAVE,	
							@FUNDO,
							@IDX_CODIGO,
							@POS_DIA_CENARIO,
							@RFX_DT_NEGOC_TERMO,
							--@RF_EMISSAO,
							'A',
							@CTB_CURVA

								
                    END
                    ELSE
                    BEGIN
                
                    EXEC SANPS_RF_MARCACAO_MERCADO  @IDX_MERCADO,
                                    @PU_MERCADO OUTPUT,
                                    @PU_CURVA_U,
                                    @PU_CURVA_C,
                                    @DTAUX,
                                    @RF_EMISSAO,
                                    @RF_VENCTO,
                                    @RFX_RESULTADO,
                                    @RFX_FCALC,
                                    @TAXA_MEDIA_U,
                                    @TAXA_MEDIA,
                                    @TAXA_MEDIA_BBC OUTPUT,
                                    'A',
                                    @FER_CHAVE,
                                    @SGL_MEDA,
                                    @ATV_CODIGO,
                                    @VAR_ERRO   OUTPUT,
                                    0,
                				    @ESTOQUE_ORI
                 END

                    /* CDI E RESERVA */
                    IF (@CEN_FCALC = '003' OR @CEN_FCALC = '006') AND NOT (@PU_MERCADO  = @PU_CURVA_U)
                    BEGIN
                        SELECT  @PU_MERCADO = @PU_MERCADO * @PU_CURVA_U

-- if @@spid = 65 select 1919 'gbvlfi03', @PU_MERCADO PU_MERCADO

                        --08/09/2004 - MtM CDI/CALCULA TIR MTM
                        SELECT  @CEN_VALOR  =  @PU_CURVA_EMISSAO / @PU_MERCADO

                        EXEC    SIAN_CALC_TAXA '022', @CEN_VALOR OUTPUT, @DTAUX,
                            @DTAUX, @RF_VENCTO, 0, 'A', @FER_CHAVE,'U', 0
                    END

                    --09/09/2004 - MtM CDI
                    IF @CEN_FCALC = '005' AND NOT (@PU_MERCADO  = @PU_CURVA_U)
                        SELECT  @PU_MERCADO = @PU_VOLTA / @PU_MERCADO --VF/Taxa MtM

-- if @@spid = 65 select 1930 'gbvlfi03', @PU_MERCADO PU_MERCADO

                    IF @VAR_ERRO <> -1
                    BEGIN
                        SELECT  @ACHOU_CEN_DIA_ANT = 'N'

                        IF @DT_ACABOU_CENARIO IS NULL
                            SELECT  @DT_ACABOU_CENARIO = @DTAUX

                        IF @CTB_CURVA = 'M'
                            SELECT  @ACHOU_CENARIO = 'N'
                        ELSE
                            IF @CTB_CURVA = 'C'
                                SELECT  @PU_MERCADO = @PU_CURVA_C
                            ELSE
                                SELECT  @PU_MERCADO = @PU_CURVA_U
-- if @@spid = 65 select 1944 'gbvlfi03', @PU_MERCADO PU_MERCADO
                    END
                    ELSE
                    BEGIN
                        SELECT  @ACHOU_CEN_DIA_ANT = 'S'
                        IF @CEN_FCALC <> '003' AND @CEN_FCALC <> '006'
                           SELECT @CEN_VALOR = @TAXA_MEDIA_BBC
                    END
                END

                IF @PU_MERCADO = @VAR_CORRECAO_C /* SEM CENARIO PARA LINEAR*/
begin
                    SELECT @PU_MERCADO = @PU_CURVA_C /* Sempre Corridos */
-- if @@spid = 65 select 1956 'gbvlfi03', @PU_MERCADO PU_MERCADO
end
                ELSE
                IF (@RFX_RESULTADO = '023' OR @RFX_RESULTADO = '024') AND
                    @MERCADO_1DIA  = 'S' OR (@MERCADO_1DIA   = 'N' AND @DTAUX <> @DTCAUTELA)
                BEGIN /* se calcula agio/desagio */

                    IF @RFX_RESULTADO = '024'
                    BEGIN
                        IF @DTAUX = @RFX_PARTIDA
                        BEGIN
                            SELECT  @FIN_1COMPRA = ABS(SUM((66-ASCII(RFM_DT))*RFM_FINANCEIRO)),
                                @QTD_1COMPRA = ABS(SUM((66-ASCII(RFM_DT))*RFM_QTDE))
                            FROM
                                RF_MOVIMENTACAO A (nolock),
                                RENDA_FIXA  B (nolock)
                            WHERE
                                A.RF_CARACTERISTICA =   B.RF_CARACTERISTICA AND
                                A.RF_CARACTERISTICA =   @CARAC          AND
                                B.RF_OK             =   'S'         AND
                                A.RFM_OK            =   'S'         AND
                                A.RFM_DTERMO        =   @RFX_PARTIDA

                            SELECT  @FIN_MERCADO = @QTD_1COMPRA * @PU_MERCADO

                            IF @FIN_TRUNCA = 'S'
                                EXEC TRUNC @FIN_MERCADO, 2, @FIN_MERCADO OUTPUT
                            ELSE
                                SELECT  @FIN_MERCADO = ROUND(@FIN_MERCADO, 2)

                            SELECT
                                @TX_AGIO_DESAGIO = @FIN_MERCADO / @FIN_1COMPRA,
                                @DT_ANT = DATEADD(DAY, 1, @RFX_PARTIDA)

                            EXEC SIAN_CALC_TAXA '009', @TX_AGIO_DESAGIO OUTPUT,
                                        @DT_ANT, @RFX_PARTIDA, @DT_ANT, -1, 'A',
                                        @FER_CHAVE, 'C', 0

                            IF @FIN_MERCADO < @FIN_1COMPRA
                                SELECT  @TX_AGIO_DESAGIO = @TX_AGIO_DESAGIO * (-1)
                        END
                    END
                    ELSE /* se for 023 */
                        SELECT
                            @TX_AGIO_DESAGIO = 0.0

                    EXEC SIAN_SP_AGIO_DESAGIO   @ATV_AP, @RFX_RESULTADO,
                                    @EMPRESA, @IDX_GERENCIAL, @DTAUX, @RF_VENCTO, 'A',
                                    @FER_CHAVE, @TX_AGIO_DESAGIO, @PU_MERCADO OUTPUT, 0
-- if @@spid = 65 select 2002 'gbvlfi03', @PU_MERCADO PU_MERCADO
                END
            END

    IF @ESTOQUE_ORI = '08' AND @RFX_FCALC = '000'
        SELECT  @PU_MERCADO = @PU_PARTIDA

-- if @@spid = 138 select 2008 'gbvlfi03', @PU_MERCADO PU_MERCADO

    If @RFX_RESULTADO = '006'     --NORMAL_PU

    BEGIN
        SELECT  @PU_CURVA_U 		= @PU_MERCADO,
                @PU_CURVA_C 		= @PU_MERCADO,
		@V_JURS_CTBL         	= ISNULL(@JUROS,0.0),
               	@V_JURS_CTBL_ANTR    	= ISNULL(@JUROS_ANT,0.0),
               	@V_CORC_CTBL         	= ISNULL(@CORRECAO,0.0),
               	@V_CORC_CTBL_ANTR    	= ISNULL(@CORRECAO_ANT,0.0) 

	IF @FUNDO = 'N' 
		SELECT @V_JURS_CTBL = @PU_MERCADO 

    END 

--SELECT @V_JURS_CTBL V_JURS_CTBL, @JUROS JUROS
    /***** BLOCO3 *****/

    IF @CTB_CURVA = 'M'   /* IF EE */
    BEGIN
        SELECT  @PU_AUX         = @PU_MERCADO,
                @VAR_CORRECAO   = 1.0 ,
                @VAR_CORRECAO_C = 1 ,
                @VAR_CORRECAO_U = 1
    END
    ELSE   /* ELSE IF EE */
    BEGIN

        IF @CTB_CURVA = 'U'
        BEGIN
            SELECT  @PU_AUX   	  =  @PU_CURVA_U,
                    @VAR_CORRECAO = @VAR_CORRECAO_U
        END
        ELSE
        BEGIN
            SELECT  @PU_AUX       = @PU_CURVA_C,
                    @VAR_CORRECAO = @VAR_CORRECAO_C
        END
    END

    IF EXISTS ( SELECT POS_APELIDO FROM FDO_POSICAO_FUNDO(NOLOCK) WHERE POS_APELIDO = @EMPRESA )
        SELECT  @PU_CTBL    = @PU_CURVA_U,
            	@VAR_CTBL   = @VAR_CORRECAO_U,
            	@FIN_CTBL   = @PU_CURVA_U * @QTD_ABE
    ELSE
        SELECT  @PU_CTBL    = @PU_CURVA_C,
            	@VAR_CTBL   = @VAR_CORRECAO_C,
            	@FIN_CTBL   = @PU_CURVA_C * @QTD_ABE

    /****** BLOCO 4 ******/
    SELECT  @FIN_AUX = @PU_AUX * @QTD_ABE

    IF @FIN_TRUNCA = 'S'
    BEGIN
        EXEC TRUNC @FIN_AUX, 2, @FIN_AUX OUTPUT
        EXEC TRUNC @FIN_CTBL, 2, @FIN_CTBL OUTPUT
    END
    ELSE
    BEGIN
        SELECT  @FIN_AUX = ROUND(@FIN_AUX, 2)
        SELECT @FIN_CTBL = ROUND(@FIN_CTBL, 2)
    END

    SELECT  @RENDIMENTO = @FIN_AUX - @PRINCIPAL,
            @CURVA      = @FIN_AUX,
            @REND_CTBL  = @FIN_CTBL - @PRINCIPAL

    SELECT  @RENDIMENTO = CONVERT(MONEY, @RENDIMENTO),
            @REND_CTBL  = CONVERT(MONEY, @REND_CTBL)

    EXEC TRUNC @RENDIMENTO , 2, @RENDIMENTO OUTPUT
    EXEC TRUNC @REND_CTBL, 2, @REND_CTBL OUTPUT

    IF @IDX_PRE <> 'S'
    BEGIN   /* BEGIN IF FF */
        IF @VAR_CORRECAO = 1.0
            SELECT @CORRECAO = 0.0
        ELSE
            IF (@RFX_FCALC = '009' OR @RFX_FCALC = '022') AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
            BEGIN
                IF @PU_PARTIDA = 0.0
                    SELECT  @PU_PARTIDA = 1.0

                SELECT  @CORRECAO = (@QTD_ABE * ((@VAR_CORRECAO / @PU_PARTIDA) -1 ) * @RFX_COTACAO_VOLTA ) - @CORRECAO_ACRU_T
            END
            ELSE
                IF @RF_CUPOM = 0 AND @IDX_PRE = 'N' AND @IS_IDXMISTO <> -1
                    SELECT  @CORRECAO = @RENDIMENTO
                ELSE
                    SELECT  @CORRECAO = (@QTD_ABE * (@VAR_CORRECAO - 1.0) * @ATV_LOTE) - @CORRECAO_ACRU_T

        SELECT @CORRECAO = ISNULL(CONVERT(MONEY, @CORRECAO),0)

        EXEC TRUNC @CORRECAO , 2, @CORRECAO OUTPUT

    END   /* END IF FF */
    ELSE /* Sem Correcao */
        SELECT  @CORRECAO = 0.0

    /****** BLOCO 5 ******/

    IF @RF_CUPOM = 0 AND @IDX_PRE = 'N' AND @IS_IDXMISTO <> -1
    BEGIN
        SELECT  @JUROS = 0.0
    END
    ELSE
    BEGIN
        -- 25.05.2004
        IF @ATV_AP = 'A' AND @ATV_PUBLICO = 'S'
        BEGIN
            SELECT
                @JUROS = CONVERT(MONEY, ((@QTD_ABE - @QTD_V_CTBL) * @PU_AUX) - (@PRINCIPAL - @PRINC_CTBL))
        END
        ELSE
        BEGIN
            SELECT @JUROS = CONVERT(MONEY, @RENDIMENTO - @CORRECAO)
        END
    END


    IF @FUNDO = 'S' OR (@FUNDO = 'N' AND @ESTOQUE_ORI <> '00' AND @IC_OPRC_NAO_PADR = 'S') --FUNDO OU COMPROMISSADA NAO PADRONIZADA
    BEGIN


        SELECT  @JUROS    = @RENDIMENTO,
            	@CORRECAO = 0.0

	-- Danilo 13/11/2007: Para Fundos, o Juros contabil deve ser calculado na curva uteis tambem para Compromissos, pois Overs Longas Pre-fixadas podem ter variacao entre as Curvas Uteis e Mercado
        IF  @ESTOQUE_ORI = '00' OR (@FUNDO = 'S' AND @ESTOQUE_ORI IN ('01', '02', '03'))
        BEGIN
		SELECT  @V_CORC_CTBL 	  = 0.0,
		    	@V_CORC_CTBL_ANTR = 0.0
		
		EXEC @E_RESERVA = SIAN_E_RESERVA @DTAUX , 'A',@FER_CHAVE, 0
		
		IF @E_RESERVA  = 0
		BEGIN
			SELECT	@V_JURS_CTBL = 0.0,
				@V_JURS_CTBL_ANTR = 0.0
		END
		ELSE
		BEGIN
			-- SELECT  @V_JURS_CTBL  = ((@QTD_ABE - @QTD_V_CTBL ) * @PU_CURVA_U) - (@PRINCIPAL - @PRINC_CTBL)
			SELECT  @V_JURS_CTBL  = ((@QTD_ABE ) * @PU_CURVA_U) - @PRINCIPAL_ABERTURA

--			SELECT @QTD_ABE QTD_ABE, @PU_CURVA_U PU_CURVA_U, @PRINCIPAL_ABERTURA PRINCIPAL_ABERTURA
			
			SELECT  @PU_U_ANT  = RFS_PU_UTEIS
			FROM
				RF_SALDOS (nolock)
			WHERE
				RF_CARACTERISTICA 	= @CARAC         AND
				RFS_DATA		= DATEADD(DAY,-1,@DTAUX)
			
			SELECT  @V_JURS_CTBL_ANTR  = ((@QTD_ABE ) * @PU_U_ANT) - @PRINCIPAL_ABERTURA
		END
        END
    END

--  /***** BLOCO 6 ******/

    SELECT @IR_ABERTURA = 0.0

-- 12.11.2004 SOH EH UTILIZADO EM PROCEDURES FUNDOS E FUNDOS NAO TEM IR.
--
--  IF (@RF_TRIB_IR = 'S' OR (@RF_TRIB_IR = 'N' AND @DTAUX > '31-DEC-1997')) AND @QTD_ABE <> 0
--  BEGIN
--      SELECT @P1 = (@PU_PARTIDA * @QTD_ABE) + @VAL_IOF
--
--      IF @CTB_CURVA = 'M'
--          SELECT  @P2 = (@PU_MERCADO * @QTD_ABE)
--      ELSE
--          IF @CTB_CURVA = 'U'
--              SELECT  @P2 = (@PU_CURVA_U * @QTD_ABE)
--          ELSE
--              SELECT  @P2 = (@PU_CURVA_C * @QTD_ABE)
--
--      /* IOF DE RESGATE */
--      SELECT @IOF_R = 0.0
--
--      EXEC SIAN_VALOR_IOF @ATV_RENDA, @RFX_PARTIDA, @DTAUX,
--                  @FC_IOF, @P1  , @P2, '', '', @IOF_R OUTPUT, 1
--
--      SELECT  @P1 = @P1 + @IOF_R
--
--      IF (@ISENTO_IR = 'N' OR @DTAUX < @RF_VENCTO_IR)
--          EXEC SIAN_VALOR_IR_NOVO @ATV_RENDA, @RFX_PARTIDA, @DTAUX,
--                      @FC_IR, @P1, @P2, @EMPRESA, @CARAC, 0.0,
--                      @IR_ABERTURA OUTPUT, 1, @QTD_ABE
--      ELSE
--          SELECT  @IR_ABERTURA = 0.0
--  END
--  ELSE
--      SELECT  @IR_ABERTURA = 0.0


    /***** BLOCO 7 ******/

    IF @IS_IDXMISTO = -1
    BEGIN
        IF @DTAUX = @DT_VIRADA
        BEGIN
            SELECT  	@RFX_FCALC  = @RFX_FCALC2,
	                @IDX_CODIGO = @IDX_CODIGO2,
                	@RF_EMISSAO = @DT_VIRADA,
                	@RF_VENCTO  = @VENCTO_ORI,
                	@RF_CUPOM   = 0.0

            IF @CDI_ANO = 0
            BEGIN
                SELECT  @IDX_PRE   = IDX_PRE,
                    	@IDX_TIPO  = IDX_TIPO,
                    	@IDX_FCALC = IDX_FCALC
                FROM
                    INDEXADOR (nolock)
                WHERE
                    IDX_CODIGO = @IDX_CODIGO
            END
            ELSE
            BEGIN
                SELECT  @IDX_PRE   = A.IDX_PRE,
                    	@IDX_TIPO  = A.IDX_TIPO,
                    	@IDX_FCALC = B.IDX_FCALC
                FROM
                    INDEXADOR       A (nolock),
                    INDEXADOR_PERIODO   B (nolock)
                WHERE
                    A.IDX_CODIGO    =   @IDX_CODIGO     AND
                    A.IDX_CODIGO    =   B.IDX_CODIGO    AND
                    @DTINI      BETWEEN B.IDX_INICIO    AND B.IDX_VALIDADE
            END
        END
    END

    /****** BLOCO 8 ******/

    IF @RF_COND_DTBASE = 'N'
        SELECT  @RF_DATA_VAR = @RF_EMISSAO
    ELSE
        SELECT @RF_DATA_VAR  = @RF_BASE_CALC
        
    IF @ESTOQUE_ORI = '08'
    BEGIN
      select @DTAUX = @DTAUX_OLD
    END        

    EXEC SIAN_RF_VAL_FIS_003_2  @AC,    @ATV_AP,
                    @ATV_COMPOSTO,      @ATV_FCVALGER,
                    @ATV_LOTE,      	@IDX_PRE,
                    @IDX_TIPO,      	@CTB_CURVA,
                    @FIN_TRUNCA,        @RFX_FCALC,
                    @IDX_FCALC,     	@CARAC,
                    @CORRECAO,      	@IDX_CODIGO,
                    @CORRECAO_ACRU,     @CURVA_CTB,
                    @DT_FIM_VAR,        @DTAUX,
                    @EMPRESA,       	@FIN_AUX,
                    @FER_CHAVE,     	@RF_AGENTE,
                    @RF_DTLIQ,      	@RF_DATA_VAR,
                    @RFX_DTANIV,        @RFX_PU_INICIAL,
                    @TAXA_ADIC,     	@RF_CUPOM,
                    @RFX_PERC,      	@RFX_COTACAO_PARTIDA,
                    @RFX_COTACAO_VOLTA, @PU_VOLTA,
                    @PU_AUX,        	@QTD_ABE,
                    @TITULO_ID,     	@TX_AGIO_DESAGIO,
                    @VAR_JUROS_C,       @VAR_ERRO,
                    @ACHOU_COTACAO  OUTPUT, @CORRECAO_ACRU_T     OUTPUT,
                    @CORRECAO_BASE  OUTPUT, @FIN_COMPRA          OUTPUT,
                    @FIN_VENDA  OUTPUT, @JUROS_BASE         	 OUTPUT,
                    @MSG        OUTPUT, @MSGAUX 		 OUTPUT,
                    @MSGDT      OUTPUT, @PRINCIPAL      	 OUTPUT,
                    @RF_VENCTO  OUTPUT, @PU_PARTIDA     	 OUTPUT,
                    @PU_CURVA_C OUTPUT, @PU_CURVA_U     	 OUTPUT,
                    @PU_MERCADO OUTPUT, @RESULTADO      	 OUTPUT,
                    @TAXA_MEDIA OUTPUT, @TAXA_MEDIA_U       	 OUTPUT,
                    @QTD_COMPRA OUTPUT, @QTD_SALDO      	 OUTPUT,
                    @QTD_VENDA  OUTPUT, @VAL_IOF        	 OUTPUT,
                    @VAR_CORRECAO   OUTPUT, @CETIP_SELIC

   if @TIPO_ESTOQUE = '08'
   begin
     select @achou_cotacao='S'
   end

-- if @@spid = 65 select 2265 'gbvlfi03', @PU_MERCADO PU_MERCADO

    /**** BLOCO 20 ****/
    /* FORCAR PU_MERCADO NESTES CASOS */
    IF @RFX_RESULTADO = '005' OR (@TIPO_ESTOQUE = '09' AND @QTD_ABE = 0 AND @CTB_CURVA = 'C' AND
       (SELECT MIN(RFM_DATA) FROM RF_MOVIMENTACAO (nolock) WHERE RFM_OK = 'S' AND RFM_QTDE > 0 AND RF_CARACTERISTICA = @CARAC) = @DTAUX)
        SELECT
            @PU_MERCADO = @PU_CURVA_C

-- if @@spid = 65 select 2273 'gbvlfi03', @PU_MERCADO PU_MERCADO

    IF @RFX_RESULTADO = '007' OR (@TIPO_ESTOQUE = '09' AND @QTD_ABE = 0 AND @CTB_CURVA = 'U' AND
       (SELECT MIN(RFM_DATA) FROM RF_MOVIMENTACAO (nolock) WHERE RFM_OK = 'S' AND RFM_QTDE > 0 AND RF_CARACTERISTICA = @CARAC) = @DTAUX)
        SELECT
            @PU_MERCADO = @PU_CURVA_U

-- if @@spid = 65 select 2278 'gbvlfi03', @PU_MERCADO PU_MERCADO

    -- 30.06.2003 - SE FOR TRANSFERENCIA DE CATEGORIA DE FUNDOS
    IF @FUNDO = 'S' AND EXISTS (
       SELECT * FROM RF_MOVIMENTACAO (nolock) WHERE RF_CARACTERISTICA = @CARAC AND RFM_QTDE <> 0 AND
       RFM_DT = 'A' AND RFM_OK = 'S' AND RFM_DATA = @DTAUX AND ISNULL(CD_CATG_ORIG, 0) <> 0 )

        SELECT
            @PU_MERCADO = @PU_CURVA_U

-- if @@spid = 65 select 2287 'gbvlfi03', @PU_MERCADO PU_MERCADO

    IF @RF_TRIB_IR = 'S' OR @RF_TRIB_IOF = 'S' OR (@RF_TRIB_IR = 'N' AND @DTAUX > '31-DEC-1997')
    BEGIN
        SELECT  @VALOR_IR = 0.0

	IF @RFX_COTACAO_PARTIDA <> 0.0
        	SELECT  @PU_APLICACAO = @RFX_COTACAO_PARTIDA
	ELSE
        	SELECT  @PU_APLICACAO = @PU_PARTIDA

        IF @CTB_CURVA = 'M'
                SELECT  @PU_CURVA_ATUAL = @PU_MERCADO
        ELSE
            IF @CTB_CURVA = 'U'
                SELECT  @PU_CURVA_ATUAL = @PU_CURVA_U
            ELSE
                SELECT  @PU_CURVA_ATUAL = @PU_CURVA_C

	-- DANILO 18/06/2007: PARA CLIENTES, A CURVA UTILIZADA NO CALCULO DEVE SER SEMPRE A UTEIS
	IF NOT EXISTS(SELECT POS_APELIDO FROM POSICAO (nolock) WHERE POS_APELIDO = @EMPRESA)
		SELECT  @PU_CURVA_ATUAL = @PU_CURVA_U

	-- DANILO 18/06/2007: VERIFICAR O PARAMETRO DE CALCULO DO IOF NA PF_PJ
	SELECT	@FC_IOF	=	CASE WHEN ISNULL(IC_IOF, 'S') = 'N' THEN
					'000'
				ELSE
					@FC_IOF
				END
	FROM	PF_PJ (nolock)
	WHERE	PFPJ_APELIDO = @EMPRESA


	SELECT @IDX_CODIGO_RFX = @IDX_CODIGO

	-- NAS COMPROMISSADAS DE PAPEIS PUBLICOS, UTILIZA O INDEXADOR DA RENDA_FIXA PARA CALCULAR OS TRIBUTOS
	IF @LOC_NEGC = 'SELIC' AND @ESTOQUE_ORI IN ('01', '02', '03')
		SELECT	@IDX_CODIGO_RFX = IDX_CODIGO
		FROM	RENDA_FIXA (nolock)
		WHERE	RF_CARACTERISTICA = @CARAC


	EXEC SANPP_RF_CALCULA_TRIBUTOS          -- RF1941.PRC
	'X'                                     -- @PCH_ATV_RENDA
	, @EMPRESA                              -- @PCH_EMPRESA
	, @ATV_CODIGO                           -- @PCH_ATV_CODIGO
	, @IDX_CODIGO_RFX                       -- @PCH_IDX_CODIGO
	, @LOC_NEGC                             -- @PCH_LOC_NEGC
	, @RFX_PARTIDA                          -- @PDT_AQS
	, @DTAUX                                -- @PDT_NEGC
	, @PU_APLICACAO                         -- @PFL_PU_AQS
	, @PU_CURVA_ATUAL                       -- @PFL_PU_NEGC
	, @FC_IR                              	-- @PCH_FCALCIR
	, @FC_IOF                             	-- @PCH_FCALCIOF
	, @QTD_SALDO                          	-- @PFL_QUANTIDADE
	, 0                                   	-- @PFL_VL_LIQUIDO
	, 0                                   	-- @PFL_VL_APLICADO
	, 0                                   	-- @PFL_FINC_LQD
	, @IOF_R           OUTPUT             	-- @PFL_IOF
	, @VALOR_IR        OUTPUT             	-- @PFL_IR
	, 1                                   	-- @VBSQL

        /* Se mercadoria nao for isenta no vencto ou nao for a data
           de Vencto, calcular o IR */
        IF @RF_TRIB_IR = 'S' OR (@RF_TRIB_IR = 'N' AND @DTAUX > '31-DEC-1997')
        BEGIN
            IF NOT (@ISENTO_IR = 'N' OR @DTAUX < @RF_VENCTO_IR)
                SELECT  @VALOR_IR = 0.0, @IOF_R = 0.0
        END
        ELSE
            SELECT  @VALOR_IR = 0.0, @IOF_R = 0.0

    END
    ELSE
        SELECT @VALOR_IR = 0.0, @IOF_R = 0.0

    /***** BLOCO 21 *****/


    IF @IDX_PRE = 'S' AND @CALC_OVER_LNG = 'S' AND @FUNDO= 'N' /* Patricia 17/01/03 */
            SELECT @PU_PARTIDA = FLOOR(CONVERT(NUMERIC(22,11),@PU_PARTIDA) * CONVERT(FLOAT,10000000000))/CONVERT(FLOAT,10000000000)



    IF @DTAUX = @RFX_PARTIDA
    BEGIN
        IF @ESTOQUE_ORI <> '08'
	BEGIN
           SELECT  @PU_CURVA_C = @PU_PARTIDA,
               @PU_CURVA_U = @PU_PARTIDA
        END

        /* primeiro dia da repactuacao atualiza TAXA MEDIA */
        UPDATE  RF_INDEXACAO
        SET
            RFX_TAXA        	=   @TAXA_MEDIA,
            RFX_AGIO_DESAGIO    =   @TX_AGIO_DESAGIO
        WHERE
            RF_CARACTERISTICA   =   @CARAC  AND
            RFX_PARCELA     	=   0


        IF (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
            UPDATE  RF_INDEXACAO
            SET
                RFX_PU_INICIAL      =   @PU_PARTIDA
            WHERE
                RF_CARACTERISTICA   =   @CARAC  AND
                RFX_PARCELA         =   0
    END


    IF @ATV_AP = 'P'
        SELECT  @QTD_ABE = @QTD_ABE * -1.0

    IF @ESTOQUE_ORI = '08'
    BEGIN

        SELECT @RFM_DT_1MVTO = A.RFM_DT FROM RF_MOVIMENTACAO A (nolock)
            WHERE A.RF_CARACTERISTICA = @CARAC
            AND A.RFM_DATA = (SELECT MIN(B.RFM_DATA) FROM RF_MOVIMENTACAO B (nolock)
                      WHERE B.RF_CARACTERISTICA = A.RF_CARACTERISTICA)

        IF (@ATV_AP = 'A' AND @RFM_DT_1MVTO = 'C') OR (@ATV_AP = 'P' AND @RFM_DT_1MVTO = 'A')
        BEGIN
            SELECT  @QTD_ABE = @QTD_ABE * -1.0

            SELECT  @QTD_SALDO = ABS(@QTD_ABE + @QTD_COMPRA - @QTD_VENDA),
                    @PRINCIPAL = ABS(- @PRINCIPAL + @FIN_COMPRA - @FIN_VENDA)
        END
        ELSE
            SELECT  @QTD_SALDO = ABS(@QTD_ABE + @QTD_COMPRA - @QTD_VENDA),
                    @PRINCIPAL = ABS(@PRINCIPAL + @FIN_COMPRA - @FIN_VENDA)
    END

    IF @FUNDO = 'S' /* TRATAMENTO PARA FERIADOS E SEGUNDA-FEIRA */
    BEGIN
       EXEC @E_RESERVA = SIAN_E_RESERVA @DTAUX , 'A',@FER_CHAVE, 0

       /*DANILO 03/11/2006: Nao contemplar mais Segunda-Feira, pois gerava diferenças em relatórios */
       IF @E_RESERVA = 0 --OR DATEPART(dw,@DTAUX) = 2
       BEGIN
        SELECT  @JUROS_ANT = RFS_JUROS,
                @CORRECAO_ANT = RFS_CORRECAO
        FROM
            RF_SALDOS (nolock)
        WHERE
            RF_CARACTERISTICA = @CARAC  AND
            RFS_DATA      = @DT_ULTRES
       END

       IF @E_RESERVA <> 0
       BEGIN
        IF (@ESTOQUE_ORI = '00' AND @QTD_ABE  = 0.0)
          SELECT  @V_JURS_CTBL 	    = 0.0,
                  @V_CORC_CTBL      = 0.0,
                  @V_JURS_CTBL_ANTR = 0.0,
                  @V_CORC_CTBL_ANTR = 0.0
       END
    END

--   IF (@FUNDO = 'S' AND @ESTOQUE_ORI <> '00') OR @FUNDO = 'N'
    /*Igual a Fundo e Estoque Compromissada/Termo (01,02,03,08) ou Diferente de Fundo */

   -- Danilo 13/11/2007: Para Fundos, o Juros contabil deve sem sempre na curva (UTEIS), portanto, compromissos Longos podem ter Marcacao a Mercado e nao devem entrar neste IF
   IF (@FUNDO = 'S' AND @ESTOQUE_ORI NOT IN ('00', '01', '02', '03') ) OR (@FUNDO = 'N' AND @RFX_RESULTADO <> '006')
    BEGIN
        SELECT @V_JURS_CTBL         = ISNULL(@JUROS,0.0),
               @V_JURS_CTBL_ANTR    = ISNULL(@JUROS_ANT,0.0),
               @V_CORC_CTBL         = ISNULL(@CORRECAO,0.0),        
               @V_CORC_CTBL_ANTR    = ISNULL(@CORRECAO_ANT,0.0)
    END
 
    EXEC TRUNC @V_JURS_CTBL , 2, @V_JURS_CTBL OUTPUT
    EXEC TRUNC @V_CORC_CTBL , 2, @V_CORC_CTBL OUTPUT
    EXEC TRUNC @V_JURS_CTBL_ANTR, 2, @V_JURS_CTBL_ANTR OUTPUT
    EXEC TRUNC @V_CORC_CTBL_ANTR, 2, @V_CORC_CTBL_ANTR OUTPUT

    IF @PU_UTEIS_ANT <> 0
        SELECT  @V_FATR_ACUD_UTES = @V_FATR_ACUD_UTES * ( @PU_CURVA_U / @PU_UTEIS_ANT )

    IF @PU_MERC_ANT <> 0
        SELECT @V_FATR_ACUD_MERC = @V_FATR_ACUD_MERC * ( @PU_MERCADO / @PU_MERC_ANT )

    -- 27.01.2005
    SELECT
        @V_PZ_MDIO_CALD = DATEDIFF(Day, @DTAUX, @RF_VENCTO),
        @V_PZ_MDIO_AJTD = DATEDIFF(Day, @DTAUX, @RF_VENCTO)/*,
        @V_DURT_CALD    = DATEDIFF(Day, @DTAUX, @RF_VENCTO)*/

    -- DIAS UTEIS PARA O CALCULO DA DURATION

    EXEC    @V_DURT_CALD = SIAN_SP_QUANTAS_RESERVAS @DTAUX,
                            @RF_VENCTO,
                            '003',
                            'A',
                            @FER_CHAVE,
                            0



    -- O PRAZO MEDIO E A DURATION SAO TRUNCADAS NA 6ª CASA
    EXEC TRUNC @V_PZ_MDIO_CALD, 6, @V_PZ_MDIO_CALD OUTPUT
    EXEC TRUNC @V_DURT_CALD, 6, @V_DURT_CALD OUTPUT


    IF (
       (@QTD_ABE <> 0.0 OR @QTD_SALDO <> 0.0 OR @QTD_COMPRA <> 0.0 OR @QTD_VENDA <> 0.0)
       AND @ACHOU_COTACAO = 'S'
       AND NOT (@ACHOU_CENARIO = 'N' AND (@RFX_RESULTADO = '006' OR (@RFX_RESULTADO = '001' AND @CTB_CURVA ='M'))) --Patricia 20/08/04 -Nao inclui se curva = mercado e nao achou cenario
       )
       OR (@QTD_ABE = 0.0 AND @TIPO_ESTOQUE = '09')
    BEGIN

         /* VERIFICA SE EMPRESA POSSUI EVENTO CONTABIL DE PROVISAO - RF */

         IF @FUNDO = 'N' AND (@TIPO_ESTOQUE = '00' OR @ESTOQUE_ORI = '00')
         BEGIN

        IF (SELECT COUNT(REGR_CHAVE)
            FROM
                RENDA_FIXA     A (nolock),
                REGRA_CONTABIL B (nolock)
            WHERE
                A.RF_CARACTERISTICA 	= @CARAC                   AND
                A.PFPJ_APELIDO_EMPRESA 	= B.PFPJ_APELIDO_EMPRESA   AND
                A.ATV_CODIGO 		= B.ATV_CODIGO             AND
                A.IDX_CODIGO 		= B.IDX_CODIGO             AND
            	A.ATV_CODIGO 		= @ATV_CODIGO              AND
                A.IDX_CODIGO 		= @IDX_CODIGO              AND
                B.EVEN_CODIGO 		IN ('450','451')) > 0

            EXEC SIAN_RF_VAL_PROVISAO @CARAC,
                          @DTAUX,
                          @QTD_ABE,
                          @QTD_COMPRA,
                          @QTD_VENDA,
                          @PU_MERCADO,
                          @PU_CURVA_C,
                          @PU_CURVA_U,
                          @LF_DIF OUTPUT,
                          @LF_PROV OUTPUT,
                          @LF_PROVANT OUTPUT

        END

        --Para Fundos calcula sempre provisao

	-- Danilo 13/11/2007: Para Fundos, tambem deve ser calculado a Provisao, pois Overs Longas Pre-fixadas possuem Marcacao a Mercado
        IF @FUNDO = 'S' AND (@TIPO_ESTOQUE = '00' OR @ESTOQUE_ORI IN ('00', '01', '02', '03'))

	BEGIN
            EXEC SIAN_RF_VAL_PROVISAO @CARAC,
                          @DTAUX,
                          @QTD_ABE,
                          @QTD_COMPRA,
                          @QTD_VENDA,
                          @PU_MERCADO,
                          @PU_CURVA_C,
                          @PU_CURVA_U,
                          @LF_DIF OUTPUT,
                          @LF_PROV OUTPUT,
                          @LF_PROVANT OUTPUT


	--CALCULAR PROVISAO ANTERIOR DESPREZANDO OS MOVIMENTOS DO DIA

--             EXEC SIAN_RF_VAL_PROVISAO @CARAC,
--                           @DTAUX,
--                           @QTD_ABE,
--                           0,
--                           0,
--                           @PU_MERCADO,
--                           @PU_CURVA_C,
--                           @PU_CURVA_U,
--                           0 ,
--                           0 ,
--                           @LF_PROVANT OUTPUT


	END

        /*CALCULO DO CAMPO RFS_PROVISAO_TRANSF*/

        SELECT @LF_PROVTRF = 0.0

        IF @TIPO_ESTOQUE = '09'

            EXEC SANPS_RF_PROV_TRANSF   @CARAC,
                            @DTAUX,
                            @PU_CURVA_C,
                            @PU_CURVA_U,
                            @PU_MERCADO,
                            @LF_PROVTRF OUTPUT,
                            @LF_PROV    OUTPUT,
                            @LF_DIF OUTPUT

        -- GRAVA O PU DE APROPRIACAO

        IF @CTB_CURVA = 'M'
            SELECT  @V_PU_CTBL = @PU_MERCADO
        ELSE
            IF @CTB_CURVA = 'C'
                SELECT  @V_PU_CTBL = @PU_CURVA_C
            ELSE
		IF @CTB_CURVA = 'N'
		    SELECT @V_PU_CTBL = @PU_CURVA_N
		ELSE
	            SELECT  @V_PU_CTBL = @PU_CURVA_U



        -- 07.07.2004
        IF @IC_FIDC = 'S'
        BEGIN

            SELECT
                @ID_PDD_FX_SEQ  = 0,
                @PC_PDD_FX  	= 0,
                @V_PDD      	= 0


            -- SE NAO FOR DIA UTIL, NAO CALCULA A PDD

            EXEC @E_RESERVA = SIAN_E_RESERVA @DTAUX, 'A', @FER_CHAVE, 0

            IF @E_RESERVA = 0
            BEGIN

                SELECT
                    @ID_PDD_FX_SEQ  = PDD_FXA_SEQ,
                    @V_PDD          = V_PDD
                FROM
                    RF_SALDOS (nolock)
                WHERE
                    RF_CARACTERISTICA   = @CARAC    AND
                    RFS_DATA        	= @DT_ULTRES

            END
            ELSE
            BEGIN

                SELECT
                    @ID_PDD_FX_SEQ  = B.ID_PDD_FX_SEQ,
                    @PC_PDD_FX  = B.PC_PDD_FX
                FROM
                    SANT1108_FD_PDD_FIDC        A (nolock),
                    SANT1109_FD_PDD_FAIXAS_FIDC B (nolock)
                WHERE
                    A.CH_PDD        	=   B.CH_PDD                AND
                    A.IC_PDD_OK     	=   'S'	                    AND
                    B.IC_PDD_FX_OK      =   'S'                     AND
                    A.POS_APELIDO       =   @EMPRESA                AND
                    A.DT_VALD_INIC      <=  @DTAUX                  AND
                    A.DT_VALD_FIM       >=  @DTAUX                  AND
                    (
                     (
                      B.IC_POS_VCMT     =   'S'                     AND
                      @DTAUX        >   @RF_VENCTO                  AND
                      B.ID_PDD_FX_INIC  <=  DATEDIFF(day, @RF_VENCTO, @DTAUX)   AND
                      B.ID_PDD_FX_FIM   >=  DATEDIFF(day, @RF_VENCTO, @DTAUX)
                     )                                  OR
                     (
                      B.IC_POS_VCMT     =   'N'                 AND
                      @DTAUX            <=  @RF_VENCTO              AND
                      B.ID_PDD_FX_INIC  <=  DATEDIFF(day, @RF_EMISSAO, @RF_VENCTO)  AND
                      B.ID_PDD_FX_FIM   >=  DATEDIFF(day, @RF_EMISSAO, @RF_VENCTO)
                     )
                    )

                IF @@ROWCOUNT = 0 AND @DT_SEM_PDD IS NULL
                    SELECT
                        @DT_SEM_PDD = @DTAUX
                ELSE
                    SELECT
                        @V_PDD = (@V_PU_CTBL * @QTD_SALDO) * (@PC_PDD_FX / CONVERT(FLOAT, 100))

            END
        END

        --Patricia 06/09/04 Ocor. MtM CDI
        IF @CTB_CURVA = 'M' AND  (@CEN_VALOR = 0  OR @CEN_VALOR IS NULL)
            SELECT @CEN_VALOR = @TAXA_MEDIA

	-- LIGIA MUTO - 12/05/2008
	-- PADRONIZAÇÃO DOS CRITÉRIOS DE TÍTULOS PÚBLICOS
	-- PREÇO UNITÁRIO - TRUNCAR NA 6º CASA
	IF @CETIP_SELIC = 'S' AND @ESTOQUE_ORI = '00'
		BEGIN
			SELECT @PU_MERCADO = ROUND(@PU_MERCADO,6,1)
-- if @@spid = 65 select 2673 'gbvlfi03', @PU_MERCADO PU_MERCADO
		END

	IF @CETIP_SELIC = 'S' AND @ESTOQUE_ORI = '00' AND @ATV_CODIGO = 'LTN-1000'
		BEGIN
			SELECT @PU_CURVA_U = ROUND(@PU_CURVA_U,6,1)
-- if @@spid = 65 select 2673 'gbvlfi03', @PU_CURVA_U PU_CURVA_U
		END
--    SELECT @DTRESANT DTRESANT

    IF @FUNDO = 'N' AND @RFX_RESULTADO = '006'
	SELECT     @V_JURS_CTBL_ANTR   =   ISNULL(V_JURS_CTBL,0.0) FROM RF_SALDOS WHERE RF_CARACTERISTICA = @CARAC AND RFS_DATA = @DTRESANT

--	SELECT @V_JURS_CTBL_ANTR V_JURS_CTBL_ANTR, @V_JURS_CTBL V_JURS_CTBL

--PAÇOCA VALORIZAÇÃO DO TERMO
	IF @ESTOQUE_ORI = '08'
	BEGIN
		SELECT @JUROS_ANT = 0, @JUROS = 0
	END

        INSERT
            RF_SALDOS
        (
            RF_CARACTERISTICA,
            RFS_DATA,
            RFS_PRINCIPAL,
            RFS_TAXA_MEDIA,
            RFS_TAXA_MEDIA_U,   /* 05 */
            RFS_QTDE,
            RFS_QTDE_C,
            RFS_QTDE_V,
            RFS_FIN_C,
            RFS_FIN_V,      /* 10 */
            RFS_PU_UTEIS,
            RFS_PU_CORRIDOS,
            RFS_JUROS_ANT,
            RFS_JUROS,
            RFS_CORRECAO_ANT,   /* 15 */
            RFS_CORRECAO,
            LC_SITUACAO,
            RFS_RESULTADO,
            RFS_PU_MERCADO,
            RFS_PU_MERC_ANT,    /* 20 */
            RFS_IOF,
            RFS_IR,
            IR_ABERTURA,
            PRINCIPAL_ABERTURA,
            RFS_CORRECAO_ACRU,  /* 25 */
            RFS_APROPRIAR_ANT,
            RFS_IOFR,
            RFS_PROVISAO_ANT,
            RFS_PROVISAO_DIF,
            RFS_PROVISAO,       /* 30 */
            V_JURS_CTBL,
            V_CORC_CTBL,
            V_JURS_CTBL_ANTR,
            V_CORC_CTBL_ANTR,
            RFS_PROVISAO_TRANSF,    /* 35 */
            V_FATR_ACUD_UTES,
            V_FATR_ACUD_MERC,
            V_PU_CTBL,
            -- 07.07.2004
            PDD_FXA_SEQ,
            V_PDD,
            -- 27.01.2005
            V_PZ_MDIO_CALD,
            V_PZ_MDIO_AJTD,
            V_DURT_CALD,
            V_PU_NEGC
        )
        VALUES
        (
            @CARAC,
            @DTAUX,
            @PRINCIPAL,
            @TAXA_MEDIA,
            @TAXA_MEDIA_U,      /* 05 */
            @QTD_ABE,
            @QTD_COMPRA,
            @QTD_VENDA,
            @FIN_COMPRA,
            @FIN_VENDA,     /* 10 */
            @PU_CURVA_U,
            @PU_CURVA_C,
            @JUROS_ANT,
            @JUROS,
            @CORRECAO_ANT,      /* 15 */
            @CORRECAO,
            'A',
            @RESULTADO,
            @PU_MERCADO,
            @PU_MERC_ANT,       /* 20 */
            @VAL_IOF,
            @VALOR_IR,
            @IR_ABERTURA,
            @PRINCIPAL_ABERTURA,
            @CORRECAO_ACRU_T,   /* 25 */
            (@JUROS - @JUROS_BASE),
            @IOF_R,
            @LF_PROVANT,
            @LF_DIF,
            @LF_PROV,       /* 30 */
            @V_JURS_CTBL,
            @V_CORC_CTBL,
            ISNULL(@V_JURS_CTBL_ANTR,0.0),
            @V_CORC_CTBL_ANTR,
            @LF_PROVTRF,        /* 35 */
            @V_FATR_ACUD_UTES,
            @V_FATR_ACUD_MERC,
            @V_PU_CTBL,
            -- 07.07.2004
            @ID_PDD_FX_SEQ,
            @V_PDD,
            -- 27.01.2005
            @V_PZ_MDIO_CALD,
            @V_PZ_MDIO_AJTD,
            @V_DURT_CALD,
            @VFL_PU_NEGOCIADO
        )

        IF @@ERROR <> 0
        BEGIN
            SELECT @MSGAUX = @MSG + @MSGDT + ' * PROBLEMAS NO INSERT DE SALDOS * '
	    IF @RETORNA_MSG <> 'S'
		RAISERROR 70000 @MSGAUX
	    ELSE
		SELECT @MENSAGEM_ERRO = @MSGAUX
            RETURN -100
        END

        INSERT
            RF_SALDOS_COTACAO
        (
            RF_CARACTERISTICA,
            RFSC_DATA,
            RFSC_VALOR,
            RFSC_CENARIO, 
	    SGL_MEDA, 
	    RFSC_TAXA

        )
        VALUES
        (
            @CARAC,
            @DTAUX,
            ISNULL(@COT_VALOR,0.0),
            ISNULL(@CEN_VALOR,0.0), 
	    @SGL_MEDA, 
	    @TAXA_MEDIA
        )
    END


    IF (@ACHOU_COTACAO = 'N' OR @ACHOU_CENARIO = 'N') AND (@QTD_ABE <> 0.0 OR
        @QTD_SALDO <> 0.0 OR @QTD_COMPRA <> 0.0 OR @QTD_VENDA <> 0.0)
    BEGIN

        SELECT  @MSGDT1 = CONVERT(CHAR(10), @DTAUX, 103)

        IF @ACHOU_CENARIO   = 'N'
            SELECT  @MSGAUX = 'SIAN - ' + RTRIM(@SGL_MEDA) + ' - Nao Existe CENARIO para Valorizar a Data ' + @MSGDT1
        ELSE
            SELECT  @MSGAUX = 'SIAN - ' + RTRIM(@IDX_CODIGO) + ' - Nao Existe COTACAO para Valorizar a Data ' + @MSGDT1

	    IF @RETORNA_MSG <> 'S'
		RAISERROR 70000 @MSGAUX
	    ELSE
		SELECT @MENSAGEM_ERRO = @MSGAUX
        RETURN -100
    END

    SELECT  @QTD_ABE        = @QTD_SALDO,
        @JUROS_ANT          = CONVERT(MONEY,@JUROS - @JUROS_BASE),
        @CORRECAO_ANT       = CONVERT(MONEY,@CORRECAO - @CORRECAO_BASE),
        @VALOR_IR_ANT       = @VALOR_IR,
        @PU_MERC_ANT        = @PU_MERCADO,
        @DTRESANT           = @DTAUX,
        @PRINCIPAL_ABERTURA = @PRINCIPAL,
        @CORRECAO_C_ANT     = @VAR_CORRECAO_C,
        @CORRECAO_U_ANT     = @VAR_CORRECAO_U,
        @PU_UTEIS_ANT       = @PU_CURVA_U

    SELECT  @DTAUX = DATEADD(day,1,@DTAUX)
END

EXEC @CD_LG = SANPS_GE_LOG_PROCESSO @CD_LG, 'SQL', @HOSTNAME, 'X32EVA', @TXT_OBS, 1

IF  (@QTD_ABE <> 0.0 OR @QTD_SALDO <> 0.0 OR @QTD_COMPRA <> 0.0 OR @QTD_VENDA <> 0.0) --Patricia 20/08/04
BEGIN
    IF @DT_ACABOU_CENARIO IS NOT NULL AND @ERRO_CENARIO = 'S'
    BEGIN
        SELECT  @MSGAUX = ' SIAN - Caracteristica ' + @Carac + ' valorizada sem CENARIO na data ' + CONVERT(CHAR(10), @DT_ACABOU_CENARIO, 103) + '. Curva mercado = apropriação. '
	IF @RETORNA_MSG <> 'S'
	    RAISERROR 70000 @MSGAUX
	ELSE
	    SELECT @MENSAGEM_ERRO = @MSGAUX
        RETURN -100
    END

--  -- 07.07.2004
--  IF @DT_SEM_PDD IS NOT NULL
--  BEGIN
--      SELECT  @MSGAUX = ' SIAN - Caracteristica ' + @Carac + ' valorizada sem PDD na data ' + CONVERT(CHAR(10), @DT_SEM_PDD, 103)
--      RAISERROR 70000 @MSGAUX
--      RETURN -100
--  END
END
