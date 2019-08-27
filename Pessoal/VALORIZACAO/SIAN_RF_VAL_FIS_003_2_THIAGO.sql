CREATE PROCEDURE SIAN_RF_VAL_FIS_003_2_THIAGO(	
	@AC                    CHAR(1),
	@ATV_AP       	       CHAR(1),
	@ATV_COMPOSTO	       CHAR(1),
	@ATV_FCVALGER          CHAR(3),
	@ATV_LOTE              FLOAT,
	@IDX_PRE	       CHAR(1),
	@IDX_TIPO	       CHAR(1),
	@CTB_CURVA             CHAR(1),
	@FIN_TRUNCA            CHAR(1),
	@RFX_FCALC             CHAR(3),
	@IDX_FCALC             CHAR(3),
	@CARAC                 CHAR(20), 
	@CORRECAO              FLOAT,
	@IDX_CODIGO            CHAR(15),
	@CORRECAO_ACRU         FLOAT,
	@CURVA_CTB             CHAR(01),
	@DT_FIM_VAR            DATETIME,
	@DTAUX                 DATETIME,
	@EMPRESA               CHAR(15),
	@FIN_AUX               FLOAT,
	@FER_CHAVE             CHAR(15),
	@RF_AGENTE             CHAR(15),
	@RF_DTLIQ              DATETIME,
	@RF_EMISSAO            DATETIME,
	@RFX_DTANIV            DATETIME,
	@RFX_PU_INICIAL        FLOAT,
	@TAXA_ADIC             FLOAT,
	@RF_CUPOM              FLOAT,
	@RFX_PERC              FLOAT,
	@RFX_COTACAO_PARTIDA   FLOAT,
	@RFX_COTACAO_VOLTA     FLOAT,
	@PU_VOLTA              FLOAT,
	@PU_AUX                FLOAT,
	@QTD_ABE               MONEY,
	@TITULO_ID             INT,
	@TX_AGIO_DESAGIO       FLOAT,
	@VAR_JUROS_C           FLOAT,
	@VAR_ERRO              INT	,	
	@ACHOU_COTACAO         CHAR(1)      OUTPUT,
	@CORRECAO_ACRU_T       FLOAT        OUTPUT,
	@CORRECAO_BASE         FLOAT        OUTPUT,
	@FIN_COMPRA            FLOAT        OUTPUT,
	@FIN_VENDA             FLOAT        OUTPUT,
	@JUROS_BASE            FLOAT        OUTPUT,
	@MSG                   VARCHAR(255) OUTPUT,
	@MSGAUX                VARCHAR(255) OUTPUT,
	@MSGDT                 VARCHAR(255) OUTPUT,
	@PRINCIPAL             FLOAT        OUTPUT,
	@RF_VENCTO             DATETIME     OUTPUT,
	@PU_PARTIDA            FLOAT        OUTPUT,
	@PU_CURVA_C            FLOAT        OUTPUT,
	@PU_CURVA_U            FLOAT        OUTPUT,
	@PU_MERCADO            FLOAT        OUTPUT,
	@RESULTADO             FLOAT        OUTPUT,
	@TAXA_MEDIA            FLOAT        OUTPUT,
	@TAXA_MEDIA_U          FLOAT        OUTPUT,
	@QTD_COMPRA            MONEY        OUTPUT,
	@QTD_SALDO             MONEY        OUTPUT,
	@QTD_VENDA             MONEY        OUTPUT,
	@VAL_IOF               FLOAT        OUTPUT,
	@VAR_CORRECAO          FLOAT        OUTPUT,
	@CETIP_SELIC	       CHAR(01)
        )

AS

DECLARE
	@DATAHORA		CHAR(23),
	@CORRECAO_ACRU_S	FLOAT,
	@CORRECAO_MOV		FLOAT,
	@CORRECAO_BX		FLOAT,
	@JUROS_MOV		FLOAT,
	@JUROS_BX		FLOAT,
	@PRINCIPAL_MOV		FLOAT,
	@PRINCIPAL_BX		FLOAT,
	@VAR_AUX		FLOAT,
	@PU_MERC_P		FLOAT,
	@PU_CORR_P		FLOAT,
	@PU_UTEIS_P		FLOAT,
	@FIN_DAYTRADE		FLOAT,
	@FIN			FLOAT,
	@QTD			MONEY,
	@QTD_C			MONEY,
	@QTDE_DAYTRADE		MONEY,
	@IOF_C			FLOAT,
	@IOF_V			FLOAT,
	@IOF_MOV		FLOAT,
	@PRINCIPAL_BASE		FLOAT,
	@FIN_RECOMPRA		FLOAT,
	@TIPO_ESTOQUE		CHAR(02),
	@RFX_PARTIDA		DATETIME,
	@RFX_VOLTA		DATETIME,
	@RFX_QTDE		MONEY,
	@PU_CURVA		FLOAT,
	@VAR_JUROS_U		FLOAT,
	@CD_CLFC_3068		INT,
	@CD_CATG		INT,
	@ESTOQUE_ORI		CHAR(02),
	@RF_CARAC_ORI		CHAR(20),
	@RFX_PRINCIPAL		MONEY,
	@FUNDO			CHAR(01),
	@IC_FIDC		CHAR(01),
	@PUBLICO		CHAR(01),
	@PU_CURVA_U_ORI		FLOAT,
	@PU_CURVA_C_ORI		FLOAT


   SELECT  --Patricia 08/01/04
	@FUNDO = 'S'
   FROM 
	FDO_POSICAO_FUNDO 
   WHERE 
	POS_APELIDO = @EMPRESA     	

   SELECT
  	@RFX_PARTIDA	= RFX_PARTIDA,
  	@RFX_VOLTA	= RFX_VOLTA,
  	@RFX_QTDE	= RFX_QTDE,
	@RFX_PRINCIPAL	= RFX_PRINCIPAL
     FROM RF_INDEXACAO
    WHERE RF_CARACTERISTICA = @CARAC
      AND RFX_PARCELA       = 0

   SELECT
	@PUBLICO	=	B.ATV_PUBLICO,
	@IC_FIDC	=	ISNULL(B.IC_FIDC, 'N')
   FROM
	RENDA_FIXA	A,
	RF_MERCADORIA	B
   WHERE
	A.ATV_CODIGO		=	B.ATV_CODIGO	AND
	A.RF_CARACTERISTICA	=	@CARAC


   IF @@ROWCOUNT = 0
   BEGIN
      SELECT @MSGAUX = @MSG + @MSGDT + ' * Problemas com a Indexacao *' 
      RAISERROR 70000 @MSGAUX
      RETURN -100
   END
	
   SELECT @QTD_VENDA = 0.0,
  	@FIN_VENDA = 0.0, 
  	@QTD_COMPRA= 0.0,
  	@FIN_COMPRA= 0.0,
	  @IOF_C     = 0.0,
	  @IOF_V     = 0.0,
  	@FIN_RECOMPRA= 0.0,
	@PU_CURVA_U_ORI = @PU_CURVA_U,
	@PU_CURVA_C_ORI = @PU_CURVA_C


SELECT
	@TIPO_ESTOQUE = TIPO_ESTOQUE,
	@ESTOQUE_ORI = TIPO_ESTOQUE,
	@CD_CLFC_3068 = ISNULL(CD_CLFC_3068, 0),
	@CD_CATG = ISNULL(CD_CATG, 0),
	--ROBSON 16.10.2002
	@RF_CARAC_ORI = RF_CARACTERISTICA
FROM
	RENDA_FIXA
WHERE
	RF_CARACTERISTICA = @CARAC


IF @CD_CLFC_3068 > 0
	SELECT
		@ESTOQUE_ORI = TIPO_ESTOQUE,
		--ROBSON 16.10.2002
		@RF_CARAC_ORI = RF_CARACTERISTICA
	FROM
		RENDA_FIXA
	WHERE
		CD_CLFC_3068	=	@CD_CLFC_3068	AND
		TIPO_ESTOQUE	<>	'09'



--ROBSON 16.10.2002
IF @RFX_QTDE <> 0.0 AND @TIPO_ESTOQUE = '09' AND @ESTOQUE_ORI IN ('01', '02', '03')
BEGIN

	SELECT
		@RFX_QTDE = ISNULL(( RFX_QTDE * @RFX_PRINCIPAL / RFX_PRINCIPAL), 0 )
	FROM
		RF_INDEXACAO
	WHERE
		RF_CARACTERISTICA	=	@RF_CARAC_ORI	AND
		RFX_PARCELA		=	0

END


/*ROBSON 30.10.2001 - PARA ESTOQUE DE TERMO, DEVE COMECAR A VALORIZAR PELA DATA DE REFERENCIA*/
   SELECT @QTD_VENDA = SUM(RFM_QTDE),
  	@FIN_VENDA = SUM(RFM_FINANCEIRO),
	  @IOF_V     = SUM(RFM_IOF)
     FROM RF_MOVIMENTACAO
    WHERE RF_CARACTERISTICA=@CARAC
      AND RFM_DATA = @DTAUX
      AND RFM_DT = 'C'
      AND RFM_OK = 'S'
--      AND @TIPO_ESTOQUE = '08'
      AND @ESTOQUE_ORI = '08'

   IF @QTD_VENDA IS NULL
   SELECT @QTD_VENDA = SUM(RFM_QTDE),
  	@FIN_VENDA = SUM(RFM_FINANCEIRO),
	  @IOF_V     = SUM(RFM_IOF)
     FROM RF_MOVIMENTACAO
    WHERE RF_CARACTERISTICA=@CARAC
      AND RFM_DTERMO = @DTAUX
      AND RFM_DT = 'C'
      AND RFM_OK = 'S'
--      AND @TIPO_ESTOQUE <> '08'
      AND @ESTOQUE_ORI <> '08'

   IF @QTD_VENDA IS NULL
      SELECT @QTD_VENDA=0.0,
     	@FIN_VENDA=0.0,
	     @IOF_V    =0.0


/*ROBSON 30.10.2001 - PARA ESTOQUE DE TERMO, DEVE COMECAR A VALORIZAR PELA DATA DE REFERENCIA*/
   SELECT @QTD_COMPRA= SUM(RFM_QTDE), 
  	@FIN_COMPRA= SUM(RFM_FINANCEIRO),
	  @IOF_C     = SUM(RFM_IOF)
     FROM RF_MOVIMENTACAO
    WHERE RF_CARACTERISTICA=@CARAC
      AND RFM_DATA = @DTAUX
      AND RFM_DT='A'
      AND RFM_OK='S'
--      AND @TIPO_ESTOQUE = '08'
      AND @ESTOQUE_ORI = '08'

   IF @QTD_COMPRA IS NULL

   SELECT @QTD_COMPRA= SUM(RFM_QTDE), 
  	@FIN_COMPRA= SUM(RFM_FINANCEIRO),
	  @IOF_C     = SUM(RFM_IOF)
     FROM RF_MOVIMENTACAO
    WHERE RF_CARACTERISTICA=@CARAC
      AND RFM_DTERMO = @DTAUX
      AND RFM_DT='A'
      AND RFM_OK='S'
--      AND @TIPO_ESTOQUE <> '08'
      AND @ESTOQUE_ORI <> '08'

   IF @QTD_COMPRA IS NULL
      SELECT @QTD_COMPRA=0.0,
     	@FIN_COMPRA=0.0,
	     @IOF_C    =0.0

   SELECT @FIN_DAYTRADE = 0.0
   IF @ATV_FCVALGER = '019'
      BEGIN
      /* CALCULA A QUANTIDADE DE DAYTRADE */
        IF @QTD_COMPRA > 0 AND @QTD_VENDA > 0
           BEGIN
             IF @QTD_COMPRA = @QTD_VENDA
                SELECT @QTDE_DAYTRADE = ABS(@QTD_VENDA)
             ELSE
               IF @QTD_COMPRA > @QTD_VENDA
                  SELECT @QTDE_DAYTRADE = ABS(@QTD_VENDA)
               ELSE
                  SELECT @QTDE_DAYTRADE = ABS(@QTD_COMPRA)

             SELECT @FIN_DAYTRADE = (@FIN_COMPRA / @QTD_COMPRA) * @QTDE_DAYTRADE
           END
        ELSE
           SELECT @QTDE_DAYTRADE = 0
      END

   SELECT @QTDE_DAYTRADE = ISNULL(@QTDE_DAYTRADE,0.0),
  	@FIN_DAYTRADE  = ISNULL(@FIN_DAYTRADE,0.0)                 


   /*#******************BLOCO 9***********************/
--IF NOT @TIPO_ESTOQUE = '08'
IF NOT @ESTOQUE_ORI = '08'
   BEGIN
     IF @ATV_AP = 'A'
       BEGIN
         IF @QTD_ABE + @QTD_COMPRA - @QTD_VENDA < 0.0
            BEGIN
               SELECT @MSGAUX = @MSG + @MSGDT + ' * A Carteira esta Vendida *' 
               RAISERROR 70000 @MSGAUX
               RETURN -100
            END
       END
     ELSE   /* SE PASSIVO */
       IF (@QTD_ABE * -1) + @QTD_COMPRA - @QTD_VENDA > 0.0
         BEGIN
            SELECT @MSGAUX = @MSG + @MSGDT + ' * A Carteira esta Comprada *' 
            RAISERROR 70000 @MSGAUX
            RETURN -100
         END
   END
   ELSE
     BEGIN
       SELECT @RESULTADO = 0.0,
      	@PRINCIPAL_MOV = 0.0,
      	@CORRECAO_MOV = 0.0,
      	@JUROS_MOV = 0.0,
      	@CORRECAO_BASE = 0.0,
      	@JUROS_BASE = 0.0,
      	@PRINCIPAL_BASE = 0.0
       RETURN
     END


   /*#*****************BLOCO 10****************/

   IF (@ATV_AP = 'A' AND @QTD_COMPRA <> 0.0)
      SELECT @VAL_IOF = @VAL_IOF + @IOF_C


   /*#*****************BLOCO 11****************/	

   IF @DTAUX = @RF_DTLIQ
      IF (@ATV_AP = 'A' AND @QTD_VENDA <> 0.0) OR
		(@ATV_AP = 'P' AND @QTD_COMPRA <> 0.0)
         UPDATE RF_MOVIMENTACAO
	    SET RFM_IOF = @VAL_IOF
	  WHERE RFM_DTERMO = @DTAUX
            AND RFM_LIQ = 'S'
	    AND RFM_OK = 'S'
	    AND RF_CARACTERISTICA=@CARAC


   /*#*************BLOCO 12******************/

   IF (@ATV_AP = 'A' AND @QTD_VENDA <> 0.0)    
   BEGIN
      If @QTD_ABE = @QTD_VENDA
         SELECT @VAL_IOF = 0.0
      ELSE
      BEGIN
         SELECT @IOF_MOV = (@VAL_IOF * @QTD_VENDA / (@QTD_ABE+@QTD_COMPRA))
         EXEC TRUNC @IOF_MOV, 2, @IOF_MOV OUTPUT
         SELECT @VAL_IOF = @VAL_IOF - @IOF_MOV
      END
   END


   /*#*************BLOCO 13*******************/

   IF (@ATV_AP = 'P' AND @QTD_VENDA <> 0.0)
      SELECT @VAL_IOF = @VAL_IOF + @IOF_V


   /*#*************BLOCO 14********************/

   IF (@ATV_AP = 'P' AND @QTD_COMPRA <> 0.0)
   BEGIN
      IF @QTD_ABE = @QTD_COMPRA
         SELECT @VAL_IOF = 0.0
      ELSE
      BEGIN
         SELECT @IOF_MOV = (@VAL_IOF * @QTD_COMPRA / (@QTD_ABE+@QTD_VENDA))
         EXEC TRUNC @IOF_MOV, 2, @IOF_MOV OUTPUT
         SELECT @VAL_IOF = @VAL_IOF - @IOF_MOV
      END
   END


   /*#*************BLOCO 15*******************/

   IF @ATV_AP = 'A'
      SELECT @QTD_SALDO = ABS(@QTD_ABE + @QTD_COMPRA - @QTD_VENDA)
   ELSE 
      /* QTDE_ABE esta em ABSOLUTO */
      SELECT @QTD_SALDO  = ABS( (@QTD_ABE * -1) + @QTD_COMPRA - @QTD_VENDA)


   /*#**************BLOCO 16******************/

   SELECT @RESULTADO = 0.0,
  	@PRINCIPAL_MOV = 0.0,
  	@CORRECAO_MOV = 0.0,
  	@JUROS_MOV = 0.0

   SELECT @CORRECAO_BASE = 0.0,
  	@JUROS_BASE = 0.0,
  	@PRINCIPAL_BASE = 0.0

   /*#************BLOCO 17*******************/

   IF @QTD_COMPRA <> 0 OR  @QTD_VENDA <> 0
   BEGIN /* 01 - TEM MOVIMENTACAO NO DIA */

      IF @ATV_AP='A' /* ATIVO */              
      BEGIN /* 02 - SE ATIVO, PROCESSA PRIMEIRO COMPRAS, DEPOIS VENDAS */
         
         /* Qdo for uma compra de passivo sempre apurar resultado */
         /* P/ a primeira compra atualizar a indexacao c/ os dados do passivo*/
         IF @QTD_COMPRA <> 0 AND @EMPRESA = @RF_AGENTE
         BEGIN /* ATIVO / COMPRA DE EMISSAO */
            SELECT @PU_MERC_P  = RF_SALDOS.RFS_PU_MERCADO,
           	@PU_UTEIS_P = RF_SALDOS.RFS_PU_UTEIS,
           	@PU_CORR_P  = RF_SALDOS.RFS_PU_CORRIDOS
              FROM RF_SALDOS, RENDA_FIXA
             WHERE RF_SALDOS.RFS_DATA = @DTAUX
               AND RENDA_FIXA.TITULO_ID = @TITULO_ID
               AND RENDA_FIXA.RF_CARACTERISTICA = RF_SALDOS.RF_CARACTERISTICA
               AND RENDA_FIXA.ATV_AP = 'P'


           
            IF @CTB_CURVA = 'M'
               SELECT @PU_AUX = @PU_MERC_P
            ELSE
               IF @CTB_CURVA = 'U'
                  SELECT @PU_AUX = @PU_UTEIS_P
               ELSE
                  SELECT @PU_AUX = @PU_CORR_P
	   		
            
            IF @PU_AUX IS NULL
               SELECT @PU_AUX = @PU_PARTIDA

            SELECT @FIN_AUX = @PU_AUX * @QTD_COMPRA   /* FINANCEIRO NA CURVA DA EMISSAO*/
            IF @FIN_TRUNCA = 'S'
               EXEC TRUNC @FIN_AUX, 2, @FIN_AUX OUTPUT
            ELSE
               SELECT @FIN_AUX = ROUND(@FIN_AUX, 2)
            
            SELECT @FIN_RECOMPRA = @PU_AUX * @QTD_COMPRA

            SELECT @RESULTADO  = @FIN_AUX - @FIN_COMPRA
            SELECT @FIN_COMPRA = @FIN_AUX
            
            /* SE FOR A PRIMEIRA RECOMPRA DO PASSIVO ENTAO
               ATUALIZAR O PRINCIPAL C/ FINANCEIRO NA CURVA */
            IF @QTD_ABE = 0
               UPDATE RF_INDEXACAO SET
                      RFX_PRINCIPAL     = @FIN_COMPRA,
                      RFX_AGIO_DESAGIO  = @TX_AGIO_DESAGIO
                WHERE RF_CARACTERISTICA = @CARAC
             
         END   /* ATIVO / COMPRA DE EMISSAO */
         
         SELECT @PRINCIPAL = @PRINCIPAL + @FIN_COMPRA

         IF (@QTD_COMPRA - @QTDE_DAYTRADE) <> 0
         BEGIN /* 03 - APURA NOVA TAXA MEDIA */
            /*SELECT @PRINCIPAL = (@PRINCIPAL + (@FIN_COMPRA - @FIN_DAYTRADE))*/
            IF @IDX_PRE = 'S'   /* IF GG */
            BEGIN 
--		IF @TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03'
		IF @ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03'
		BEGIN
   		   SELECT @PU_VOLTA = @RFX_QTDE /* * @RFX_COTACAO_VOLTA*/ 
                   /*SOMENTE PARA OVER LONGA*/ 
		   IF EXISTS (SELECT RF_CARACTERISTICA FROM RF_BOLETA WHERE RF_CARACTERISTICA = @CARAC AND ID_OVER_LNGA = 'S')
		      SELECT @PU_VOLTA = @PU_VOLTA * @RFX_COTACAO_VOLTA 
                   SELECT @PU_CURVA_C = ((@PU_CURVA_C * @QTD_ABE) + @FIN_COMPRA) / (@QTD_ABE + @QTD_COMPRA)
                   SELECT @PU_CURVA_C = @PU_CURVA_C / @ATV_LOTE * @ATV_LOTE
                   SELECT @PU_CURVA_U = ((@PU_CURVA_U * @QTD_ABE) + @FIN_COMPRA) / (@QTD_ABE + @QTD_COMPRA)
                   SELECT @PU_CURVA_U = @PU_CURVA_U / @ATV_LOTE * @ATV_LOTE
		   
                   /* COMO OVER E COMPROMISSADAS SAO PRE, NAO USA O PU_INICIAL.         */
                   /* GRAVAMOS EM RFX_PU_INICIAL O PU_PARTIDA                           */
                   IF @DTAUX = @RF_EMISSAO
                      SELECT @PU_PARTIDA = @PU_CURVA_C


		END
		ELSE
                  SELECT @PU_VOLTA = 1.00 * @ATV_LOTE
            END
            ELSE

            BEGIN   /* BEGIN IF GG2 */
               /* POS-FIXADO */


		IF (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03') AND @PUBLICO = 'S'
		BEGIN
   		   SELECT @PU_VOLTA = @RFX_QTDE /* * @RFX_COTACAO_VOLTA*/ 
                   /*SOMENTE PARA OVER LONGA*/ 
		   IF EXISTS (SELECT RF_CARACTERISTICA FROM RF_BOLETA WHERE RF_CARACTERISTICA = @CARAC AND ID_OVER_LNGA = 'S')
		      SELECT @PU_VOLTA = @PU_VOLTA * @RFX_COTACAO_VOLTA 
                   SELECT @PU_CURVA_C = ((@PU_CURVA_C * @QTD_ABE) + @FIN_COMPRA) / (@QTD_ABE + @QTD_COMPRA)
                   SELECT @PU_CURVA_C = @PU_CURVA_C / @ATV_LOTE * @ATV_LOTE
                   SELECT @PU_CURVA_U = ((@PU_CURVA_U * @QTD_ABE) + @FIN_COMPRA) / (@QTD_ABE + @QTD_COMPRA)
                   SELECT @PU_CURVA_U = @PU_CURVA_U / @ATV_LOTE * @ATV_LOTE

                   /* GRAVAMOS EM RFX_PU_INICIAL O PU_PARTIDA                           */
                   IF @DTAUX = @RF_EMISSAO
                      SELECT @PU_PARTIDA = @PU_CURVA_C


		END

		SELECT @VAR_JUROS_C = @RF_CUPOM                                

		EXEC SIAN_CALC_JUROS @RFX_FCALC, @VAR_JUROS_C OUTPUT, 
			@DTAUX, @RF_EMISSAO, @RF_VENCTO, -1, 'A', @FER_CHAVE,
			'C', 0 


               SELECT @VAR_CORRECAO  = 1,
              	@VAR_ERRO      = 0,
              	@DT_FIM_VAR    = @RF_DTLIQ,
              	@ACHOU_COTACAO = 'S'

               IF @IDX_FCALC = '001' /* TR - ALT. 08/04/1997 - MAURICIO */
                  SELECT @DT_FIM_VAR = @RF_VENCTO


               EXEC SIAN_SP_VARIACAO @IDX_CODIGO,
                             	@DTAUX,
                             	@RF_EMISSAO,
                             	@DT_FIM_VAR,
                             	@RFX_PERC,
                             	@RFX_COTACAO_PARTIDA,
                             	@RFX_COTACAO_VOLTA,
                             	@RFX_FCALC,
                        	@FER_CHAVE,
                             	@RFX_DTANIV,
                             	@TAXA_ADIC,
                             	@RFX_PU_INICIAL,
                             	@VAR_CORRECAO OUTPUT,
                             	@VAR_ERRO OUTPUT,
                             	@CURVA_CTB,
				0, 
				@CETIP_SELIC,
				@SGL_SISTEMA = 'RDF'		-- Liana - 22/11/10



               IF @VAR_ERRO <> -1 AND @IDX_PRE = 'N'
                  SELECT @ACHOU_COTACAO = 'N'
                
               IF @ATV_COMPOSTO = 'N' /* JUROS SIMPLES - AT */
                  SELECT @PU_VOLTA = (@VAR_JUROS_C + @VAR_CORRECAO - 1.00) * @ATV_LOTE
               ELSE
		BEGIN
--		 if (@RFX_FCALC = "009" OR @RFX_FCALC = "022") AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
		 if (@RFX_FCALC = '009' OR @RFX_FCALC = '022') AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
                  SELECT @PU_VOLTA = @VAR_JUROS_C * @VAR_CORRECAO /*  * @RFX_COTACAO_VOLTA*/
		 ELSE
                  SELECT @PU_VOLTA = @VAR_JUROS_C * @VAR_CORRECAO * @ATV_LOTE
		END

            END /* END IF GG2 */

	    IF @IDX_PRE <> 'S' AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
		BEGIN

		SELECT @PU_VOLTA = @RFX_COTACAO_VOLTA

		END


--	    IF @IDX_PRE = "S" AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
	    IF @IDX_PRE = 'S' AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')

		BEGIN
		IF @PUBLICO = 'S'
			SELECT @TAXA_MEDIA = @PU_VOLTA / (@PU_CURVA_C * (@QTD_ABE + @QTD_COMPRA)) /* Sempre Corridos */
-- 		ELSE
-- 			SELECT @TAXA_MEDIA = @TAXA_MEDIA / 100 + 1

--SELECT @TAXA_MEDIA TAXA_MEDIA, @PU_CURVA_C PU_CURVA_C, @PU_CURVA_U PU_CURVA_U, @PU_VOLTA PU_VOLTA
		END
	    ELSE
	    BEGIN
            IF @FIN_RECOMPRA <> 0.0 
            BEGIN
               SELECT @PU_CURVA_C = ((@PU_CURVA_C * @QTD_ABE) + (@FIN_RECOMPRA - @FIN_DAYTRADE)) / (@QTD_ABE + (@QTD_COMPRA - @QTDE_DAYTRADE))
               SELECT @PU_CURVA_U = ((@PU_CURVA_U * @QTD_ABE) + (@FIN_RECOMPRA - @FIN_DAYTRADE)) / (@QTD_ABE + (@QTD_COMPRA - @QTDE_DAYTRADE))
            END
            ELSE
            BEGIN
               SELECT @PU_CURVA_C = ((@PU_CURVA_C * @QTD_ABE) + (@FIN_COMPRA - @FIN_DAYTRADE)) / (@QTD_ABE + (@QTD_COMPRA - @QTDE_DAYTRADE))
               SELECT @PU_CURVA_U = ((@PU_CURVA_U * @QTD_ABE) + (@FIN_COMPRA - @FIN_DAYTRADE)) / (@QTD_ABE + (@QTD_COMPRA - @QTDE_DAYTRADE))
            END
--	    if (@RFX_FCALC = "009" OR @RFX_FCALC = "022") AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
	    if (@RFX_FCALC = '009' OR @RFX_FCALC = '022') AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
              BEGIN
		IF @PUBLICO = 'S' 
		BEGIN
	                SELECT @PU_CURVA_C = @PU_CURVA_C / @RFX_COTACAO_VOLTA * @RFX_COTACAO_VOLTA
	                SELECT @PU_CURVA_U = @PU_CURVA_U / @RFX_COTACAO_VOLTA * @RFX_COTACAO_VOLTA
		END
 	      END
            ELSE
              BEGIN
	        SELECT @PU_CURVA_C = @PU_CURVA_C / @ATV_LOTE * @ATV_LOTE
                SELECT @PU_CURVA_U = @PU_CURVA_U / @ATV_LOTE * @ATV_LOTE
	      END
	    END

	    IF @IC_FIDC = 'S'
              BEGIN
	        SELECT @PU_CURVA_C = @PU_CURVA_C_ORI
                SELECT @PU_CURVA_U = @PU_CURVA_U_ORI
	      END

            IF @RFX_FCALC = '009' OR @RFX_FCALC = '022'
            BEGIN
--		IF @IDX_PRE = "S" AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
		IF @IDX_PRE = 'S' AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
		  BEGIN
			IF @PUBLICO = 'S'
				BEGIN
		                    SELECT @TAXA_MEDIA_U = @PU_VOLTA / (@PU_CURVA_C * (@QTD_ABE + @QTD_COMPRA)) /* Sempre Corridos */
		
		                    EXEC SIAN_CALC_TAXA '001',
		                                	@TAXA_MEDIA_U OUTPUT,
		               	@DTAUX,
		                                	@RFX_PARTIDA,
		                                	@RFX_VOLTA,
		                                        0,
		                                        'A',
		                                	@FER_CHAVE,
		                                        'C',
		                                        0 
				END
		  END
		ELSE
		  BEGIN
		    IF @IDX_FCALC <> '006'
		    BEGIN
			SELECT @TAXA_MEDIA_U = @PU_VOLTA / @PU_CURVA_C   /* Sempre Corridos */
			EXEC SIAN_CALC_TAXA '001', @TAXA_MEDIA_U OUTPUT, @DTAUX,
 				@RF_EMISSAO, @RF_VENCTO, 0, 'A', @FER_CHAVE,
				'C', 0 
		    END
                   SELECT @TAXA_MEDIA = @PU_VOLTA / @PU_CURVA_U /* Uteis */
		 END
            END
            ELSE
            BEGIN
               SELECT @TAXA_MEDIA_U = 0.0
 	       SELECT @TAXA_MEDIA = @PU_VOLTA / @PU_CURVA_C /* Sempre Corridos */
            END


--	    IF @IDX_PRE = "S" AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
	    IF @IDX_PRE = 'S' AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
		BEGIN
		IF @PUBLICO = 'S'
	               EXEC SIAN_CALC_TAXA @RFX_FCALC,
	                           	@TAXA_MEDIA OUTPUT,
	                           	@DTAUX,
	                           	@RFX_PARTIDA,
	                           	@RFX_VOLTA,
	                                   0,
	                                   'A',
	                           	@FER_CHAVE, 
	                                   'C',
	                                   0
		END
	    ELSE
               EXEC SIAN_CALC_TAXA @RFX_FCALC, @TAXA_MEDIA OUTPUT, @DTAUX,
            	@RF_EMISSAO, @RF_VENCTO, 0, 'A', @FER_CHAVE, 'C', 0 

         END   /* 03 - APURA NOVA TAXA MEDIA */


         IF (@QTD_VENDA - @QTDE_DAYTRADE) <> 0
         BEGIN /* 03 - APURA RESULTADO FISCAL*/

	   --Patricia 08/01/04
	   --Qdo venda c/ transf. de categoria e sem qtde de abertura - a proc. anterior
	   --nao calcula o PU MERCADO, entao assume o PU UTEIS (para fundos).		
	    IF @FUNDO = 'S' AND EXISTS (
		SELECT * FROM RF_MOVIMENTACAO WHERE RF_CARACTERISTICA = @CARAC AND RFM_QTDE <> 0 AND 
		RFM_DT = 'A' AND RFM_OK = 'S' AND RFM_DATA = @DTAUX AND ISNULL(CD_CATG_ORIG, 0) <> 0 )	     	
	
	 	SELECT @PU_MERCADO = @PU_CURVA_U		

            IF @CTB_CURVA = 'M'
               SELECT @PU_AUX = @PU_MERCADO
            ELSE
               IF @CTB_CURVA = 'U'
                  SELECT @PU_AUX = @PU_CURVA_U
               ELSE
                  SELECT @PU_AUX = @PU_CURVA_C


            SELECT @FIN_AUX = @PU_AUX * @QTD_VENDA  /* VENDA NA CURVA */
            IF @FIN_TRUNCA = 'S'
               EXEC TRUNC @FIN_AUX, 2, @FIN_AUX OUTPUT
            ELSE
               SELECT @FIN_AUX = ROUND(@FIN_AUX, 2)

            /* RESULTADO = VENDA_REAL - VENDA_CURVA */
            IF @ATV_FCVALGER = '019'
               BEGIN
                 SELECT @RESULTADO = @PU_AUX * (@QTD_VENDA - @QTDE_DAYTRADE) /* VENDA NA CURVA */
                 IF @FIN_TRUNCA = 'S'
                    EXEC TRUNC @RESULTADO, 2, @RESULTADO OUTPUT
                 ELSE
                    SELECT @RESULTADO = ROUND(@RESULTADO, 2)
                 SELECT @RESULTADO = (@FIN_VENDA / @QTD_VENDA * (@QTD_VENDA - @QTDE_DAYTRADE)) - @RESULTADO
               END
            ELSE
               SELECT @RESULTADO = @FIN_VENDA - @FIN_AUX
         END   /* 03 - APURA RESULTADO FISCAL*/
         IF @QTD_VENDA <> 0
            BEGIN
              SELECT @VAR_AUX = (CONVERT(FLOAT, @QTD_VENDA) / (@QTD_ABE + @QTD_COMPRA))
              SELECT @PRINCIPAL_MOV = @PRINCIPAL * @VAR_AUX,
             	@CORRECAO_MOV = @CORRECAO * @VAR_AUX
              
	      SELECT @CORRECAO_MOV = CONVERT(MONEY, @CORRECAO_MOV)
	      SELECT @PRINCIPAL_MOV = CONVERT(MONEY, @PRINCIPAL_MOV)
	      EXEC TRUNC @CORRECAO_MOV, 2, @CORRECAO_MOV OUTPUT
	      EXEC TRUNC @PRINCIPAL_MOV, 2, @PRINCIPAL_MOV OUTPUT

              SELECT @JUROS_MOV = @FIN_AUX - @PRINCIPAL_MOV - @CORRECAO_MOV
            END
      END   /* 02 - SE ATIVO, PROCESSA PRIMEIRO COMPRAS, DEPOIS VENDAS */

      ELSE   /* ELSE ATIVO, OU SEJA, PASSIVO */

      BEGIN /* 02 - SE PASSIVO, PROCESSA PRIMEIRO VENDAS, DEPOIS COMPRAS */
         IF @QTD_VENDA <> 0
         BEGIN /* 03 - APURA NOVA TAXA MEDIA */
            SELECT @PRINCIPAL= (@PRINCIPAL + @FIN_VENDA)
            IF @IDX_PRE = 'S'   /* IF GG */
            BEGIN
--		IF (@RFX_FCALC = "009" OR @RFX_FCALC = "022") AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
		IF (@RFX_FCALC = '009' OR @RFX_FCALC = '022') AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
		BEGIN
		   SELECT @PU_VOLTA = @RFX_QTDE /* * @RFX_COTACAO_VOLTA*/
                   /*SOMENTE PARA OVER LONGA*/ 
		   IF EXISTS (SELECT RF_CARACTERISTICA FROM RF_BOLETA WHERE RF_CARACTERISTICA = @CARAC AND ID_OVER_LNGA = 'S')
		      SELECT @PU_VOLTA = @PU_VOLTA * @RFX_COTACAO_VOLTA 
                   SELECT @PU_CURVA_C = ((@PU_CURVA_C * @QTD_ABE) + @FIN_VENDA) / (@QTD_ABE + @QTD_VENDA)
                   SELECT @PU_CURVA_C = @PU_CURVA_C / @ATV_LOTE * @ATV_LOTE
                   SELECT @PU_CURVA_U = ((@PU_CURVA_U * @QTD_ABE) + @FIN_VENDA) / (@QTD_ABE + @QTD_VENDA)
                   SELECT @PU_CURVA_U = @PU_CURVA_U / @ATV_LOTE * @ATV_LOTE

                   /* COMO OVER E COMPROMISSADAS SAO PRE, NAO USA O PU_INICIAL.         */
                   /* GRAVAMOS EM RFX_PU_INICIAL O PU_PARTIDA                           */
                   IF @DTAUX = @RF_EMISSAO
                      SELECT @PU_PARTIDA = @PU_CURVA_C

 
		END
		ELSE
                  SELECT @PU_VOLTA = 1.00 * @ATV_LOTE
            END
            ELSE

            BEGIN   /* BEGIN IF HH2 */
               /* POS-FIXADO */
               SELECT @VAR_JUROS_C = @RF_CUPOM                                

                 EXEC SIAN_CALC_JUROS @RFX_FCALC, @VAR_JUROS_C OUTPUT, @DTAUX,
              	@RF_EMISSAO, @RF_VENCTO, -1, 'A', @FER_CHAVE, 'C', 0 

               SELECT @VAR_CORRECAO  = 1,
              	@VAR_ERRO      = 0,
              	@DT_FIM_VAR    = @RF_DTLIQ,
              	@ACHOU_COTACAO = 'S'

               IF @IDX_FCALC = '001' /* TR - ALT. 08/04/1997 - MAURICIO */
                  SELECT @DT_FIM_VAR = @RF_VENCTO

               EXEC SIAN_SP_VARIACAO @IDX_CODIGO,
                             	@DTAUX,
                             	@RF_EMISSAO,
                             	@DT_FIM_VAR,
                             	@RFX_PERC,
                             	@RFX_COTACAO_PARTIDA,
                             	@RFX_COTACAO_VOLTA,
                             	@RFX_FCALC,
                             	@FER_CHAVE,
                             	@RFX_DTANIV,
                             	@TAXA_ADIC,
                             	@RFX_PU_INICIAL,
                             	@VAR_CORRECAO OUTPUT,
                             	@VAR_ERRO OUTPUT,
                             	@CURVA_CTB,
				0,
				@CETIP_SELIC,
				@SGL_SISTEMA = 'RDF'		-- Liana - 22/11/10


               IF @VAR_ERRO <> -1 AND @IDX_PRE = 'N'
                  SELECT @ACHOU_COTACAO = 'N'

               IF @ATV_COMPOSTO = 'N' /* JUROS SIMPLES - AT */
                  SELECT @PU_VOLTA = (@VAR_JUROS_C + @VAR_CORRECAO - 1.00) * @ATV_LOTE
               ELSE
		BEGIN
--		 if (@RFX_FCALC = "009" OR @RFX_FCALC = "022") AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
		 if (@RFX_FCALC = '009' OR @RFX_FCALC = '022') AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
                  SELECT @PU_VOLTA = @VAR_JUROS_C * @VAR_CORRECAO /*  * @RFX_COTACAO_VOLTA*/
		 ELSE
                  SELECT @PU_VOLTA = @VAR_JUROS_C * @VAR_CORRECAO * @ATV_LOTE
		END
            END   /* END IF HH2 */


--	    IF @IDX_PRE = "S" AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
	    IF @IDX_PRE = 'S' AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
		BEGIN
		IF @PUBLICO = 'S'
	               SELECT @TAXA_MEDIA = @PU_VOLTA / (@PU_CURVA_C * (@QTD_ABE + @QTD_VENDA)) /* Sempre Corridos */
		END
	    ELSE
	    BEGIN
            SELECT @PU_CURVA_C = ((@PU_CURVA_C * @QTD_ABE) + @FIN_VENDA) / (@QTD_ABE + @QTD_VENDA)
--	    if (@RFX_FCALC = "009" OR @RFX_FCALC = "022") AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
	    if (@RFX_FCALC = '009' OR @RFX_FCALC = '022') AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
		BEGIN
		IF @PUBLICO = 'S'
               		SELECT @PU_CURVA_C = @PU_CURVA_C / @RFX_COTACAO_VOLTA * @RFX_COTACAO_VOLTA
		END
	    ELSE
               SELECT @PU_CURVA_C = @PU_CURVA_C / @ATV_LOTE * @ATV_LOTE

            SELECT @PU_CURVA_U = ((@PU_CURVA_U * @QTD_ABE) + @FIN_VENDA) / (@QTD_ABE + @QTD_VENDA)

--	    if (@RFX_FCALC = "009" OR @RFX_FCALC = "022") AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
	    if (@RFX_FCALC = '009' OR @RFX_FCALC = '022') AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
		BEGIN
		IF @PUBLICO = 'S'
			SELECT @PU_CURVA_U = @PU_CURVA_U / @RFX_COTACAO_VOLTA * @RFX_COTACAO_VOLTA
		END
	    ELSE
               SELECT @PU_CURVA_U = @PU_CURVA_U / @ATV_LOTE * @ATV_LOTE
	    END

            IF @RFX_FCALC = '009' OR @RFX_FCALC = '022'
               BEGIN
--		IF @IDX_PRE = "S" AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
		IF @IDX_PRE = 'S' AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
		  BEGIN
			IF @PUBLICO = 'S'
				BEGIN
				SELECT @TAXA_MEDIA_U = @PU_VOLTA / (@PU_CURVA_C * (@QTD_ABE + @QTD_VENDA)) /* Sempre Corridos */
				EXEC SIAN_CALC_TAXA '001',
				        	@TAXA_MEDIA_U OUTPUT,
				        	@DTAUX,
				        	@RFX_PARTIDA,
				        	@RFX_VOLTA,
				                0,
				                'A',
				        	@FER_CHAVE,
				                'C',
				                0 
				END
		  END
		ELSE
		  BEGIN
		    IF @IDX_FCALC <> '006'
		    BEGIN
			SELECT @TAXA_MEDIA_U = @PU_VOLTA / @PU_CURVA_C   /* Sempre Corridos */
			EXEC SIAN_CALC_TAXA '001', @TAXA_MEDIA_U OUTPUT, @DTAUX,
				@RF_EMISSAO, @RF_VENCTO, 0, 'A', @FER_CHAVE,
				'C', 0 
		    END

		    SELECT @TAXA_MEDIA = @PU_VOLTA / @PU_CURVA_U /* Uteis */
                  END
               END
            ELSE
               BEGIN
	          SELECT @TAXA_MEDIA_U = 0.0
	          SELECT @TAXA_MEDIA = @PU_VOLTA / @PU_CURVA_C /* Sempre Corridos */
               END
            /* CALCULANDO A TAXA MEDIA */
--	    IF @IDX_PRE = "S" AND (@TIPO_ESTOQUE = '01' or @TIPO_ESTOQUE = '02' or @TIPO_ESTOQUE = '03')
	    IF @IDX_PRE = 'S' AND (@ESTOQUE_ORI = '01' or @ESTOQUE_ORI = '02' or @ESTOQUE_ORI = '03')
		BEGIN
		IF @PUBLICO = 'S'
			EXEC SIAN_CALC_TAXA @RFX_FCALC,
			           	@TAXA_MEDIA OUTPUT,
			           	@DTAUX,
			           	@RFX_PARTIDA,
			           	@RFX_VOLTA,
			                   0,
			                   'A',
			           	@FER_CHAVE, 
			                   'C',
			                   0
		END
	    ELSE
              EXEC SIAN_CALC_TAXA @RFX_FCALC, @TAXA_MEDIA OUTPUT, @DTAUX,
           	@RF_EMISSAO, @RF_VENCTO, 0, 'A', @FER_CHAVE, 'C', 0 
         END   /* 03 - APURA NOVA TAXA MEDIA */

         IF @QTD_COMPRA <> 0
         BEGIN /* 03 - APURA RESULTADO FISCAL*/
	
            IF @CTB_CURVA = 'M'
               SELECT @PU_AUX = @PU_MERCADO
            ELSE
               IF @CTB_CURVA = 'U'
                  SELECT @PU_AUX = @PU_CURVA_U
               ELSE
                  SELECT @PU_AUX = @PU_CURVA_C
			
	
            SELECT @FIN_AUX = @PU_AUX * @QTD_COMPRA   /* COMPRA NA CURVA */
            IF @FIN_TRUNCA = 'S'
               EXEC TRUNC @FIN_AUX, 2, @FIN_AUX OUTPUT
            ELSE
               SELECT @FIN_AUX = ROUND(@FIN_AUX, 2)

            /* RESULTADO = COMPRA_REAL - COMPRA_CURVA */
            SELECT @RESULTADO = @FIN_COMPRA - @FIN_AUX

            SELECT @VAR_AUX = (CONVERT(FLOAT, @QTD_COMPRA) / (@QTD_ABE + @QTD_VENDA))
            SELECT @PRINCIPAL_MOV = @PRINCIPAL * @VAR_AUX
            SELECT @CORRECAO_MOV = @CORRECAO * @VAR_AUX

	    SELECT @CORRECAO_MOV = CONVERT(MONEY, @CORRECAO_MOV)
	    SELECT @PRINCIPAL_MOV = CONVERT(MONEY, @PRINCIPAL_MOV)
	    EXEC TRUNC @CORRECAO_MOV, 2, @CORRECAO_MOV OUTPUT
	    EXEC TRUNC @PRINCIPAL_MOV, 2, @PRINCIPAL_MOV OUTPUT

            SELECT @JUROS_MOV = @FIN_AUX - @PRINCIPAL_MOV - @CORRECAO_MOV

         END   /* 03 - APURA RESULTADO FISCAL*/

      END   /* 02 - SE PASSIVO, PROCESSA PRIMEIRO VENDAS, DEPOIS COMPRAS */

      SELECT @RESULTADO = CONVERT(MONEY, @RESULTADO) 
      SELECT @JUROS_MOV = CONVERT(MONEY, @JUROS_MOV)
      EXEC TRUNC @RESULTADO, 2, @RESULTADO OUTPUT
      EXEC TRUNC @JUROS_MOV, 2, @JUROS_MOV OUTPUT
      /* Somente o Principal e atualizado antes do insert */
      SELECT @PRINCIPAL = CONVERT(MONEY, @PRINCIPAL - @PRINCIPAL_MOV)

      SELECT @CORRECAO_BASE = @CORRECAO_MOV,
      	     @JUROS_BASE = @JUROS_MOV,
     	@PRINCIPAL_BASE = @PRINCIPAL_MOV

   END   /* 01 - TEM MOVIMENTACAO NO DIA */


   /*#*****************BLOCO 18*************/	


   IF (@QTD_COMPRA <> 0 AND @ATV_AP = 'A') OR 
      (@QTD_VENDA  <> 0 AND @ATV_AP = 'P')
   BEGIN   /* BEGIN IF II */
      SELECT @DATAHORA = ''
      IF @IDX_PRE = 'N'   /* POS-FIXADO */
         BEGIN   /* BEGIN IF JJ */
            SELECT @VAR_CORRECAO  = 1,
           	@VAR_ERRO      = 0,
           	@DT_FIM_VAR    = @RF_DTLIQ,
           	@ACHOU_COTACAO = 'S'

            IF @IDX_FCALC = '001' /* TR - ALT. 08/04/1997 - MAURICIO */
               SELECT @DT_FIM_VAR = @RF_VENCTO

            EXEC SIAN_SP_VARIACAO @IDX_CODIGO,
                          	@DTAUX,
                          	@RF_EMISSAO,
                          	@DT_FIM_VAR,
                          	@RFX_PERC,
                          	@RFX_COTACAO_PARTIDA,
                          	@RFX_COTACAO_VOLTA,
                          	@RFX_FCALC,
                          	@FER_CHAVE,
                          	@RFX_DTANIV,
                          	@TAXA_ADIC,
                          	@RFX_PU_INICIAL,
                          	@VAR_CORRECAO OUTPUT,
                          	@VAR_ERRO OUTPUT,
                          	@CURVA_CTB,
                                  0, 
				  @CETIP_SELIC, /* ROBSON 21.06.2001 */
				@SGL_SISTEMA = 'RDF'		-- Liana - 22/11/10

           IF @VAR_ERRO <> -1 AND @IDX_PRE = 'N'
              SELECT @ACHOU_COTACAO = 'N'  
  
         END   /* END IF JJ */
      ELSE
         SELECT @VAR_CORRECAO = 1

      IF @ATV_AP = 'A'
         SELECT @QTD = @QTD_COMPRA,
        	@AC = 'A' 
      ELSE 
         SELECT @QTD = @QTD_VENDA,
        	@AC = 'C' 

      /* loop p/ calcular e atualizar na movimentacao Jur e Corr Acruados */
      WHILE @QTD > 0.0
      BEGIN   /* WHILE KK */
         SET ROWCOUNT 1

         SELECT @DATAHORA = CONVERT(CHAR(8), RFM_DATA,112) + RFM_HORA + RFM_USUARIO,
        	@QTD_C = RFM_QTDE,
        	@FIN = RFM_FINANCEIRO
           FROM RF_MOVIMENTACAO
          WHERE RF_CARACTERISTICA = @CARAC
            AND CONVERT(CHAR(8), RFM_DATA, 112) + RFM_HORA + RFM_USUARIO > @DATAHORA  
            AND RFM_DTERMO = @DTAUX
            AND RFM_DT = @AC
            AND RFM_OK = 'S'
          ORDER BY CONVERT(CHAR(8), RFM_DATA, 112) + RFM_HORA + RFM_USUARIO

         SET ROWCOUNT 0

         IF @IDX_PRE = 'N'   /* POS-FIXADO */
            IF @IDX_TIPO <> 'T'               
               SELECT @CORRECAO_ACRU = @QTD_C * (@VAR_CORRECAO - 1.0) * @ATV_LOTE
            ELSE
               SELECT @CORRECAO_ACRU = 0.0
         ELSE
            /* PRE-FIXADO */
            SELECT @CORRECAO_ACRU = 0.0

         SELECT @CORRECAO_ACRU = CONVERT(MONEY, @CORRECAO_ACRU)
         EXEC TRUNC @CORRECAO_ACRU, 2, @CORRECAO_ACRU OUTPUT

         SELECT @CORRECAO_ACRU_T = @CORRECAO_ACRU_T + @CORRECAO_ACRU
     
         SELECT @QTD = @QTD - @QTD_C 
      END   /* WHILE KK */
   END   /* IF II */


   /*#**************BLOCO 19****************/

   SELECT @DATAHORA = '',
  	@CORRECAO_ACRU_S = 0.0

   IF (@QTD_COMPRA <> 0 AND @ATV_AP = 'P') OR (@QTD_VENDA <> 0 AND @ATV_AP = 'A')
   BEGIN   /* IF LL */

      IF @ATV_AP = 'A'
         SELECT @QTD = @QTD_VENDA,
        	@AC = 'C'
      ELSE 
         SELECT @QTD = @QTD_COMPRA,
        	@AC = 'A' 

      /* @PRINCIPAL_BASE serve p/ manter a mesma proporcao qdo ha mais de
         uma venda na mesma data */
      SELECT @PRINCIPAL_BASE = @PRINCIPAL_MOV

      WHILE @QTD > 0.0
      BEGIN   /* WHILE MM */
         SET ROWCOUNT 1

         SELECT @DATAHORA = CONVERT(CHAR(8),RFM_DATA,112) + RFM_HORA + RFM_USUARIO,
        	@QTD_C = RFM_QTDE
           FROM RF_MOVIMENTACAO
          WHERE RF_CARACTERISTICA = @CARAC
            AND CONVERT(CHAR(8),RFM_DATA,112) + RFM_HORA + RFM_USUARIO > @DATAHORA  
            AND RFM_DT = @AC
            AND RFM_DTERMO = @DTAUX
            AND RFM_OK = 'S'
          ORDER BY CONVERT(CHAR(8),RFM_DATA,112) + RFM_HORA + RFM_USUARIO

         SET ROWCOUNT 0

         IF (@QTD - @QTD_C) = 0.0   /* IF NN */
            SELECT @CORRECAO_BX = @CORRECAO_MOV, 
           	@JUROS_BX = @JUROS_MOV,
           	@PRINCIPAL_BX = @PRINCIPAL_MOV
         ELSE 
            BEGIN   /* IF NN2 */
               IF @ATV_AP = 'P'
                  SELECT @CORRECAO_BX = (@CORRECAO_MOV / @QTD_COMPRA) * (@QTD_C), 
                 	@JUROS_BX = (@JUROS_MOV / @QTD_COMPRA) * (@QTD_C),
                 	@PRINCIPAL_BX = (@PRINCIPAL_BASE / @QTD_COMPRA) * (@QTD_C)
               ELSE
                  SELECT @CORRECAO_BX = (@CORRECAO_MOV / @QTD_VENDA) * (@QTD_C), 
                 	@JUROS_BX = (@JUROS_MOV / @QTD_VENDA) * (@QTD_C),
                 	@PRINCIPAL_BX = (@PRINCIPAL_BASE / @QTD_VENDA) * (@QTD_C)

               SELECT @CORRECAO_BX = CONVERT(MONEY, @CORRECAO_BX)
               SELECT @JUROS_BX = CONVERT(MONEY, @JUROS_BX)
               SELECT @PRINCIPAL_BX = CONVERT(MONEY, @PRINCIPAL_BX)
               EXEC TRUNC @CORRECAO_BX, 2, @CORRECAO_BX OUTPUT
               EXEC TRUNC @JUROS_BX, 2, @JUROS_BX OUTPUT
               EXEC TRUNC @PRINCIPAL_BX, 2, @PRINCIPAL_BX OUTPUT
               SELECT @CORRECAO_MOV = @CORRECAO_MOV - @CORRECAO_BX,
              	@JUROS_MOV = @JUROS_MOV - @JUROS_BX,
              	@PRINCIPAL_MOV = @PRINCIPAL_MOV - @PRINCIPAL_BX
            END   /* IF NN2 */

         IF (@QTD = @QTD_C AND @QTD_SALDO = 0.0)
            /* SE VENDEU TUDO E ULTIMO MOVTO DE VENDA */ 
            SELECT @CORRECAO_ACRU = @CORRECAO_ACRU_S - @CORRECAO_ACRU_T
         ELSE
            SELECT @CORRECAO_ACRU = ( -1.0 * (@CORRECAO_ACRU_T / (@QTD_ABE + @QTD_COMPRA)) * (@QTD_C))

         SELECT @CORRECAO_ACRU = CONVERT(MONEY, @CORRECAO_ACRU)
         EXEC TRUNC @CORRECAO_ACRU, 2, @CORRECAO_ACRU OUTPUT
         SELECT @CORRECAO_ACRU_S = @CORRECAO_ACRU_S + (@CORRECAO_ACRU * -1.0)

         UPDATE RF_MOVIMENTACAO SET
                RFM_CORRECAO = @CORRECAO_BX,
                RFM_JUROS = @JUROS_BX,
                RFM_PRINCIPAL = @PRINCIPAL_BX
          WHERE RFM_USUARIO = SUBSTRING(@DATAHORA, 21, 3)  
            AND RFM_HORA    = SUBSTRING(@DATAHORA, 9, 12)  
            AND RF_CARACTERISTICA = @CARAC
            AND RFM_DATA = SUBSTRING(@DATAHORA, 1, 8)
       
         IF @@ERROR <> 0
         BEGIN
            SELECT @MSGAUX = @MSG + @MSGDT + ' * Problemas no Update da Movimentacao *' 
            RAISERROR 70000 @MSGAUX
            RETURN -100
         END

         SELECT @QTD = @QTD - @QTD_C 
      END   /* WHILE MM */
   END  /* END IF LL */


   SELECT @CORRECAO_ACRU_T = @CORRECAO_ACRU_T - @CORRECAO_ACRU_S	


RETURN