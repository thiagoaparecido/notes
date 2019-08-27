
-- mercadoria
select ATV_CODIGO from rf_mercadoria where atv_publico = 'S'

-- indexador
select distinct idx_codigo from codigo_titulo where atv_codigo in (
    select ATV_CODIGO from rf_mercadoria where atv_publico = 'S')



