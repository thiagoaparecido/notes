-- LTN-1000 
exec sanpp_rf_valorizacao_lote_THIAGO '00000000000023021639', '2019-06-28', '2019-06-28'
go
exec sanpp_rf_valorizacao_lote_THIAGO '00000000000024199874', '2019-06-28', '2019-06-28'
go

exec SIAN_RF_VAL_FIS_003_THIAGO '00000000000024199874', '2019-06-28', '2019-06-28'
go


-- NTN-B
exec sanpp_rf_valorizacao_lote_THIAGO '00000000000013861200', '2019-08-19', '2019-08-19'
go

--LFT (REF)
exec sanpp_rf_valorizacao_lote_THIAGO '00000000000021525068', '2019-06-24', '2019-06-24'
go
exec sanpp_rf_valorizacao_lote_THIAGO '00000000000031281405', '2019-06-24', '2019-06-24'
go



--NTN-F
exec sanpp_rf_valorizacao_lote_THIAGO '00000000000024435716', '2019-06-24', '2019-06-24'
go

select top 15 rfs_pu_uteis, rfs_pu_corridos, * from rf_saldos where rf_caracteristica = '00000000000024435716' order by rfs_data desc

--SELECT TOP 10 * FROM RF_MERCADORIA WHERE ATV_CODIGO LIKE '%LTN%'

SELECT TOP 10 * FROM RENDA_FIXA WHERE ATV_CODIGO = '' AND 