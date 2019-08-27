select rfs_data, count(*)
from   rf_saldos (nolock)
where  rfs_data >= (select convert(varchar(10), getdate() -300, 120))
group by rfs_data
order by rfs_data



select * 
from renda_fixa rdf
join rf_saldos rfs
    on rfs.rf_caracteristica = rdf.rf_caracteristica
    and rfs.rfs_data = '2019-06-24'
where rdf.atv_codigo = 'LTN-1000'
    and rdf.pfpj_apelido_empresa not in (select pos_apelido from posicao)
    and rdf.tipo_estoque = '00'
order by rdf.rf_caracteristica



select * from renda_fixa where rf_caracteristica = '00000000000013861200'
select rfm_data, rfm_dt, rfm_qtde, rfm_pu, rfm_ok, * from rf_movimentacao where rf_caracteristica = '00000000000013861200'

