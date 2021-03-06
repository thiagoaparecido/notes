USE [iTContabil]
GO
/****** Object:  Table [dbo].[agAgendaExtrato]    Script Date: 09/02/2020 23:17:00 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[agAgendaExtrato](
	[idagAgendaExtrato] [uniqueidentifier] NULL,
	[idAgendamento] [uniqueidentifier] NULL,
	[idLancamentos] [uniqueidentifier] NULL,
	[dt_pgto] [smalldatetime] NULL,
	[valor] [float] NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[agAgendamento]    Script Date: 09/02/2020 23:17:00 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[agAgendamento](
	[idAgendamento] [uniqueidentifier] NOT NULL,
	[IdEmpresa] [uniqueidentifier] NULL,
	[dt_pgto] [smalldatetime] NULL,
	[dt_doc] [smalldatetime] NULL,
	[num_doc] [varchar](30) NULL,
	[historico] [varchar](200) NULL,
	[valor] [float] NULL,
	[saldo] [float] NULL,
	[debito] [uniqueidentifier] NULL,
	[credito] [uniqueidentifier] NULL,
	[dt_vcto] [smalldatetime] NULL,
	[ativo] [int] NULL,
	[id_lote] [uniqueidentifier] NULL,
	[id_pgto] [uniqueidentifier] NULL,
	[num_parc] [varchar](6) NULL,
	[AnoMes] [varchar](6) NULL,
	[Tipo] [varchar](1) NULL,
 CONSTRAINT [PK_agAgendamento] PRIMARY KEY CLUSTERED 
(
	[idAgendamento] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[cdContasContabeis]    Script Date: 09/02/2020 23:17:00 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[cdContasContabeis](
	[IdContasContabeis] [uniqueidentifier] NOT NULL,
	[IdEmpresa] [uniqueidentifier] NULL,
	[Descricao] [varchar](50) NULL,
	[TipoContasContabeis] [varchar](2) NULL,
	[SubGrupo] [uniqueidentifier] NULL,
	[DC] [varchar](1) NULL,
	[Tipo] [varchar](1) NULL,
 CONSTRAINT [PK__cdContas__03F1533BAA9CA0E9] PRIMARY KEY CLUSTERED 
(
	[IdContasContabeis] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[cdEmpresa]    Script Date: 09/02/2020 23:17:00 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[cdEmpresa](
	[IdEmpresa] [uniqueidentifier] NOT NULL,
	[Reduzido] [varchar](15) NULL,
	[nome] [varchar](50) NULL,
PRIMARY KEY CLUSTERED 
(
	[IdEmpresa] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[cdSaldos]    Script Date: 09/02/2020 23:17:00 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[cdSaldos](
	[IdSaldo] [uniqueidentifier] NOT NULL,
	[IdContasContabeis] [uniqueidentifier] NULL,
	[AnoMes] [varchar](6) NULL,
	[Inicial] [float] NULL,
	[Debito] [float] NULL,
	[Credito] [float] NULL,
	[Final] [float] NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[cdTipoOperacao]    Script Date: 09/02/2020 23:17:00 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[cdTipoOperacao](
	[IdTipoOperacao] [uniqueidentifier] NULL,
	[TipoOperacaoDescricao] [varchar](50) NULL,
	[GrupoConta] [varchar](2) NULL,
	[DC] [varchar](1) NULL
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[lcLancamento]    Script Date: 09/02/2020 23:17:00 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[lcLancamento](
	[idLancamentos] [uniqueidentifier] NOT NULL,
	[IdEmpresa] [uniqueidentifier] NULL,
	[dt_pgto] [smalldatetime] NULL,
	[dt_doc] [smalldatetime] NULL,
	[num_doc] [varchar](30) NULL,
	[historico] [varchar](200) NULL,
	[valor] [float] NULL,
	[debito] [uniqueidentifier] NULL,
	[credito] [uniqueidentifier] NULL,
	[dt_vcto] [smalldatetime] NULL,
	[ativo] [int] NULL,
	[id_lote] [uniqueidentifier] NULL,
	[id_pgto] [uniqueidentifier] NULL,
	[num_parc] [varchar](6) NULL,
	[AnoMes] [varchar](6) NULL,
 CONSTRAINT [PK_lcLancamento] PRIMARY KEY CLUSTERED 
(
	[idLancamentos] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[paLcto]    Script Date: 09/02/2020 23:17:00 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[paLcto](
	[idLctos] [uniqueidentifier] NOT NULL,
	[IdEmpresa] [uniqueidentifier] NULL,
	[dt_lcto] [smalldatetime] NULL,
	[num_doc] [varchar](30) NULL,
	[historico] [varchar](200) NULL,
	[conta] [uniqueidentifier] NULL,
	[valor] [float] NULL,
	[DC] [varchar](1) NULL,
	[partida] [uniqueidentifier] NULL,
	[id_lote] [uniqueidentifier] NULL,
	[num_parc] [varchar](6) NULL,
	[AnoMes] [varchar](6) NULL,
	[ativo] [int] NULL,
 CONSTRAINT [PK_paLcto] PRIMARY KEY CLUSTERED 
(
	[idLctos] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
ALTER TABLE [dbo].[agAgendaExtrato] ADD  CONSTRAINT [DF_agAgendaExtrato_idAgendamento]  DEFAULT (newid()) FOR [idAgendamento]
GO
ALTER TABLE [dbo].[agAgendaExtrato] ADD  CONSTRAINT [DF_agAgendaExtrato_valor]  DEFAULT ((0)) FOR [valor]
GO
ALTER TABLE [dbo].[agAgendamento] ADD  CONSTRAINT [DF_agAgendamento_idAgendamento]  DEFAULT (newid()) FOR [idAgendamento]
GO
ALTER TABLE [dbo].[agAgendamento] ADD  CONSTRAINT [DF_agAgendamento_valor]  DEFAULT ((0)) FOR [valor]
GO
ALTER TABLE [dbo].[agAgendamento] ADD  CONSTRAINT [DF_agAgendamento_saldo]  DEFAULT ((0)) FOR [saldo]
GO
ALTER TABLE [dbo].[agAgendamento] ADD  CONSTRAINT [DF_agAgendamento_ativo]  DEFAULT ((0)) FOR [ativo]
GO
ALTER TABLE [dbo].[agAgendamento] ADD  CONSTRAINT [DF__agAgendam__id_lo__7849DB76]  DEFAULT ('00000000-0000-0000-0000-000000000000') FOR [id_lote]
GO
ALTER TABLE [dbo].[agAgendamento] ADD  CONSTRAINT [DF_agAgendamento_idpgto]  DEFAULT ('00000000-0000-0000-0000-000000000000') FOR [id_pgto]
GO
ALTER TABLE [dbo].[agAgendamento] ADD  CONSTRAINT [DF_agAgendamento_num_parc]  DEFAULT ('00/00') FOR [num_parc]
GO
ALTER TABLE [dbo].[cdContasContabeis] ADD  CONSTRAINT [DF__cdContasC__IdCon__44FF419A]  DEFAULT (newid()) FOR [IdContasContabeis]
GO
ALTER TABLE [dbo].[cdContasContabeis] ADD  CONSTRAINT [DF_cdContasContabeis_Tipo]  DEFAULT ('U') FOR [Tipo]
GO
ALTER TABLE [dbo].[cdEmpresa] ADD  DEFAULT (newid()) FOR [IdEmpresa]
GO
ALTER TABLE [dbo].[cdSaldos] ADD  CONSTRAINT [DF_cdSaldos_Inicial]  DEFAULT ((0)) FOR [Inicial]
GO
ALTER TABLE [dbo].[cdSaldos] ADD  CONSTRAINT [DF_cdSaldos_Debito]  DEFAULT ((0)) FOR [Debito]
GO
ALTER TABLE [dbo].[cdSaldos] ADD  CONSTRAINT [DF_cdSaldos_Credito]  DEFAULT ((0)) FOR [Credito]
GO
ALTER TABLE [dbo].[cdSaldos] ADD  CONSTRAINT [DF_cdSaldos_SaldoFinal]  DEFAULT ((0)) FOR [Final]
GO
ALTER TABLE [dbo].[cdTipoOperacao] ADD  CONSTRAINT [DF_cdTipoOperacao_IdTipoOperacao]  DEFAULT (newid()) FOR [IdTipoOperacao]
GO
ALTER TABLE [dbo].[lcLancamento] ADD  CONSTRAINT [DF_lcLancamento_idLancamentos]  DEFAULT (newid()) FOR [idLancamentos]
GO
ALTER TABLE [dbo].[lcLancamento] ADD  CONSTRAINT [DF_lcLancamento_ativo]  DEFAULT ((1)) FOR [ativo]
GO
ALTER TABLE [dbo].[lcLancamento] ADD  CONSTRAINT [DF__lcLancamen__lote__6E01572D]  DEFAULT ('00000000-0000-0000-0000-000000000000') FOR [id_lote]
GO
ALTER TABLE [dbo].[lcLancamento] ADD  CONSTRAINT [DF_lcLancamento_idpgto]  DEFAULT ('00000000-0000-0000-0000-000000000000') FOR [id_pgto]
GO
ALTER TABLE [dbo].[lcLancamento] ADD  CONSTRAINT [DF_lcLancamento_num_parc]  DEFAULT ('01/01') FOR [num_parc]
GO
ALTER TABLE [dbo].[paLcto] ADD  CONSTRAINT [DF_paLcto_idLctos]  DEFAULT (newid()) FOR [idLctos]
GO
ALTER TABLE [dbo].[paLcto] ADD  CONSTRAINT [DF__paLcto__lote__6E01572D]  DEFAULT ('00000000-0000-0000-0000-000000000000') FOR [id_lote]
GO
ALTER TABLE [dbo].[paLcto] ADD  CONSTRAINT [DF_paLcto_num_parc]  DEFAULT ('01/01') FOR [num_parc]
GO
ALTER TABLE [dbo].[paLcto] ADD  CONSTRAINT [DF_paLcto_ativo]  DEFAULT ((1)) FOR [ativo]
GO
/****** Object:  StoredProcedure [dbo].[Saldos_Apurar]    Script Date: 09/02/2020 23:17:00 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[Saldos_Apurar]
@RefAnoMes varchar(10),
@IdEmpresa uniqueidentifier
as
Declare @tB Table (
IdContasContabeis uniqueidentifier,
Inicial float,
Debito float,
Credito float)

	    INSERT INTO @tB(IdContasContabeis,Inicial,Debito,Credito)
		SELECT 
			sd.IdContasContabeis
			, sd.final 
			, 0
			, 0
			FROM dbo.cdSaldos sd 
				INNER JOIN [dbo].[cdContasContabeis] cc ON cc.IdEmpresa= @IdEmpresa AND cc.IdContasContabeis = sd.IdContasContabeis
			 WHERE 	AnoMes=CONVERT(varchar(6), DATEADD(MONTH, -1, @RefAnoMes + '01'),112)
		UNION
		SELECT
			debito      
			,0
			,sum(valor) DebitoValor
			,0
		FROM iTContabil.dbo.lcLancamento la
			WHERE AnoMes = @RefAnoMes AND la.IdEmpresa=@IdEmpresa
			GROUP BY debito
		UNION
		SELECT
			 credito
			,0
			,0
			,sum(valor) DebitoValor	  
		FROM iTContabil.dbo.lcLancamento la
			WHERE AnoMes = @RefAnoMes AND la.IdEmpresa=@IdEmpresa
			GROUP BY credito

DELETE sd 
		FROM dbo.cdSaldos sd 
			INNER JOIN [dbo].[cdContasContabeis] cc ON cc.IdEmpresa = @IdEmpresa AND cc.IdContasContabeis = sd.IdContasContabeis
		WHERE AnoMes = @RefAnoMes 

;WITH a  as (
	 SELECT NewId() IdSaldo
		,IdContasContabeis
		,@RefAnoMes AnoMes
		,SUM(Inicial) Inicial
		,SUM(Debito) Debito
		,SUM(Credito) Credito 
	FROM @tB 
	GROUP BY IdContasContabeis)

	 INSERT INTO cdSaldos
			SELECT 
				IdSaldo 
				,IdContasContabeis
				,AnoMes
				,Inicial
				,Debito
				,Credito 
				,Inicial + Debito -Credito as Final
			FROM a



--[Saldos_Apurar] '202003'
GO
